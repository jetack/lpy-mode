;;; lpy-nrepl.el --- nREPL Client for LisPython -*- lexical-binding: t -*-

;; Copyright (C) 2025 Jetack <jetack23@gmail.com>
;;
;; lpy-mode is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; lpy-mode is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with lpy-mode.  If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;; Commentary:

;; nREPL client for LisPython.  Connects to a running `lpy --nrepl' server
;; over TCP using newline-delimited JSON messages.
;;
;; Provides eval, load-file, macroexpand, and completion operations that
;; integrate with `lpy-mode' keybindings and fall back to the comint shell
;; when no nREPL connection is active.

;;; Code:

(require 'lpy-base)
(require 'json)

;;; Configuration

(defvar lpy-nrepl--host "127.0.0.1"
  "Host for the nREPL server.")

(defvar lpy-nrepl--default-port nil
  "Default port for `lpy-nrepl-connect'.  When nil, prompt interactively.")

(defvar lpy-nrepl--overlay-duration 5
  "Seconds to display result overlays before they fade.")

(defvar lpy-nrepl--interpreter "lpy"
  "Executable used to start the nREPL server subprocess.")

;;; Internal State

(defvar lpy-nrepl--connection nil
  "The network process connected to the nREPL server.")

(defvar lpy-nrepl--server-process nil
  "The subprocess running `lpy --nrepl'.")

(defvar lpy-nrepl--response-buffer ""
  "Accumulates partial TCP data until a full newline-delimited message arrives.")

(defvar lpy-nrepl--pending-callbacks nil
  "Alist of (id . callback) for responses we are waiting on.
Each callback is a function taking one argument: the parsed JSON response.")

(defvar lpy-nrepl--request-counter 0
  "Monotonically increasing counter for request IDs.")

(defconst lpy-nrepl--repl-buffer-name "*lpy-nrepl*"
  "Buffer name for the nREPL interaction log.")

(defconst lpy-nrepl--macroexpand-buffer-name "*lpy-macroexpand*"
  "Buffer name for macroexpansion results.")

(defconst lpy-nrepl--server-buffer-name "*lpy-nrepl-server*"
  "Buffer name for the nREPL server subprocess output.")

;;; Helpers

(defun lpy-nrepl--next-id ()
  "Return a fresh string request ID."
  (setq lpy-nrepl--request-counter (1+ lpy-nrepl--request-counter))
  (number-to-string lpy-nrepl--request-counter))

(defun lpy-nrepl--repl-buffer ()
  "Return the nREPL interaction log buffer, creating it if needed."
  (let ((buf (get-buffer-create lpy-nrepl--repl-buffer-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'lpy-nrepl-repl-mode)
        (lpy-nrepl-repl-mode)))
    buf))

(defun lpy-nrepl--log (direction text)
  "Append TEXT to the nREPL REPL buffer in CIDER-like format.
DIRECTION is one of: \"input\" \"value\" \"stdout\" \"error\"."
  (with-current-buffer (lpy-nrepl--repl-buffer)
    (goto-char (point-max))
    (let ((inhibit-read-only t))
      (pcase direction
        ("input"
         (insert (propertize "repl> " 'face 'font-lock-keyword-face 'read-only t)
                 (propertize (concat text "\n") 'face 'font-lock-string-face 'read-only t)))
        ("value"
         (insert (propertize (concat text "\n") 'face 'font-lock-constant-face 'read-only t)))
        ("stdout"
         (insert (propertize (concat text "\n") 'read-only t)))
        ("error"
         (insert (propertize (concat text "\n") 'face 'error 'read-only t)))
        (_
         (insert (propertize (concat text "\n") 'read-only t)))))))

;;; TCP Communication

(defun lpy-nrepl--process-filter (_proc output)
  "Buffer OUTPUT from the nREPL TCP connection and dispatch complete messages."
  (setq lpy-nrepl--response-buffer (concat lpy-nrepl--response-buffer output))
  (while (string-match "\n" lpy-nrepl--response-buffer)
    (let* ((newline-pos (match-beginning 0))
           (line (substring lpy-nrepl--response-buffer 0 newline-pos))
           (rest (substring lpy-nrepl--response-buffer (1+ newline-pos))))
      (setq lpy-nrepl--response-buffer rest)
      (when (> (length line) 0)
        (condition-case err
            (let ((msg (json-parse-string line :object-type 'alist :null-object nil :false-object nil)))
              (lpy-nrepl--handle-response msg))
          (error
           (message "lpy-nrepl: failed to parse response: %s" (error-message-string err))))))))

(defun lpy-nrepl--process-sentinel (_proc event)
  "Handle connection state changes."
  (let ((event-trimmed (string-trim event)))
    (cond
     ((string-match-p "^open" event-trimmed)
      (message "lpy-nrepl: connected"))
     ((string-match-p "\\(deleted\\|connection broken\\|failed\\)" event-trimmed)
      (message "lpy-nrepl: disconnected (%s)" event-trimmed)
      (setq lpy-nrepl--connection nil)))))

(defun lpy-nrepl--handle-response (msg)
  "Dispatch a parsed JSON response MSG to the appropriate callback."
  (let* ((id (alist-get 'id msg))
         (entry (and id (assoc id lpy-nrepl--pending-callbacks))))
    (if entry
        (progn
          (setq lpy-nrepl--pending-callbacks
                (delq entry lpy-nrepl--pending-callbacks))
          (funcall (cdr entry) msg))
      ;; No pending callback -- log it for async/unsolicited messages
      (lpy-nrepl--log "<=" (json-serialize msg)))))

(defun lpy-nrepl--send (msg &optional callback)
  "Send MSG (an alist) as newline-delimited JSON to the nREPL server.
If CALLBACK is non-nil, register it to be called with the response."
  (unless (lpy-nrepl-connected-p)
    (user-error "Not connected to nREPL server"))
  (let ((id (lpy-nrepl--next-id)))
    (push (cons 'id id) msg)
    (when callback
      (push (cons id callback) lpy-nrepl--pending-callbacks))
    (let ((json-str (json-serialize msg))
          (code (alist-get 'code msg)))
      (when code
        (lpy-nrepl--log "input" code))
      (process-send-string lpy-nrepl--connection (concat json-str "\n")))))

;;; Connection Management

;;;###autoload
(defun lpy-nrepl-connected-p ()
  "Return non-nil when the nREPL connection is alive."
  (and lpy-nrepl--connection
       (process-live-p lpy-nrepl--connection)))

;;;###autoload
(defun lpy-nrepl-connect (port)
  "Connect to an nREPL server on 127.0.0.1:PORT."
  (interactive "nPort: ")
  (when (lpy-nrepl-connected-p)
    (lpy-nrepl-disconnect))
  (setq lpy-nrepl--response-buffer "")
  (setq lpy-nrepl--pending-callbacks nil)
  (setq lpy-nrepl--connection
        (make-network-process
         :name "lpy-nrepl"
         :host lpy-nrepl--host
         :service port
         :family 'ipv4
         :nowait nil
         :filter #'lpy-nrepl--process-filter
         :sentinel #'lpy-nrepl--process-sentinel
         :coding 'utf-8))
  (set-process-query-on-exit-flag lpy-nrepl--connection nil)
  (display-buffer (lpy-nrepl--repl-buffer)
                  '(display-buffer-at-bottom . ((window-height . 0.3))))
  (message "lpy-nrepl: connected to %s:%d" lpy-nrepl--host port))

;;;###autoload
(defun lpy-nrepl-disconnect ()
  "Close the nREPL connection."
  (interactive)
  (when lpy-nrepl--connection
    (delete-process lpy-nrepl--connection)
    (setq lpy-nrepl--connection nil)
    (setq lpy-nrepl--pending-callbacks nil)
    (message "lpy-nrepl: disconnected")))

;;;###autoload
(defun lpy-nrepl-start ()
  "Start `lpy --nrepl' as a subprocess, parse the port, and connect.
The server process output is captured in `*lpy-nrepl-server*'."
  (interactive)
  (when (and lpy-nrepl--server-process
             (process-live-p lpy-nrepl--server-process))
    (user-error "nREPL server already running (pid %d)"
                (process-id lpy-nrepl--server-process)))
  (let* ((buf (get-buffer-create lpy-nrepl--server-buffer-name))
         (port-found nil)
         (proc (make-process
                :name "lpy-nrepl-server"
                :buffer buf
                :command (list lpy-nrepl--interpreter "--nrepl")
                :noquery t
                :sentinel
                (lambda (_proc event)
                  (let ((event-trimmed (string-trim event)))
                    (unless (string-match-p "^open" event-trimmed)
                      (message "lpy-nrepl-server: %s" event-trimmed)
                      (when (string-match-p "\\(finished\\|exited\\|killed\\)" event-trimmed)
                        (setq lpy-nrepl--server-process nil))))))))
    (setq lpy-nrepl--server-process proc)
    (message "lpy-nrepl: starting server...")
    ;; Wait for the server to print its port line
    (with-timeout (10 (user-error "Timed out waiting for nREPL server to start"))
      (while (not port-found)
        (accept-process-output proc 0.1)
        (with-current-buffer buf
          (goto-char (point-min))
          (when (re-search-forward
                 "nREPL server started on [^:]+:\\([0-9]+\\)" nil t)
            (setq port-found (string-to-number (match-string 1)))))))
    (lpy-nrepl-connect port-found)
    (message "lpy-nrepl: server started on port %d" port-found)))

;;; Result Display

(defvar-local lpy-nrepl--eval-buffer nil
  "Buffer where the last eval was initiated.")
(defvar-local lpy-nrepl--eval-point nil
  "Point where the last eval was initiated.")

(defun lpy-nrepl--flash-overlay (text &optional face)
  "Display TEXT as a temporary overlay at the eval site.
Uses FACE for the overlay text (defaults to `font-lock-comment-face').
The overlay disappears after `lpy-nrepl--overlay-duration' seconds."
  (let ((buf lpy-nrepl--eval-buffer)
        (pt lpy-nrepl--eval-point))
    (when (and buf (buffer-live-p buf))
      (with-current-buffer buf
        ;; Remove all existing nREPL overlays first
        (dolist (ov (overlays-in (point-min) (point-max)))
          (when (overlay-get ov 'lpy-nrepl)
            (delete-overlay ov)))
        (save-excursion
          (goto-char (or pt (point)))
          (let* ((ov-face (or face 'font-lock-comment-face))
                 (ov (make-overlay (line-end-position) (line-end-position) nil t t))
                 (display-text (concat "  ; => " text)))
            (set-text-properties 0 (length display-text)
                                 (list 'face ov-face 'cursor 0) display-text)
            (overlay-put ov 'after-string display-text)
            (overlay-put ov 'lpy-nrepl t)
            (run-at-time lpy-nrepl--overlay-duration nil
                         (lambda () (when (overlay-buffer ov) (delete-overlay ov))))))))))

(defun lpy-nrepl--display-eval-result (response)
  "Display an eval RESPONSE in the minibuffer and as an overlay."
  (let ((value (alist-get 'value response))
        (stdout (alist-get 'stdout response))
        (stderr (alist-get 'stderr response))
        (error-msg (alist-get 'error response)))
    (when (and stdout (not (string-empty-p stdout)))
      (lpy-nrepl--log "stdout" (string-trim-right stdout)))
    (when (and stderr (not (string-empty-p stderr)))
      (lpy-nrepl--log "error" (string-trim-right stderr)))
    (cond
     (error-msg
      (lpy-nrepl--log "error" error-msg)
      (lpy-nrepl--flash-overlay (car (last (split-string error-msg "\n")))
                                'error)
      (message "%s" (propertize (car (last (split-string error-msg "\n")))
                                'face 'error)))
     (t
      (let ((display (or value "nil")))
        (when (and stdout (not (string-empty-p stdout)))
          (setq display (concat (string-trim-right stdout) "\n" display)))
        (lpy-nrepl--log "value" (or value "nil"))
        (lpy-nrepl--flash-overlay (or value "nil"))
        (message "%s" (or value "nil")))))))

;;; Eval Commands

(defun lpy-nrepl--eval-code (code)
  "Send CODE for evaluation, saving current buffer and point for overlay."
  (setq lpy-nrepl--eval-buffer (current-buffer))
  (setq lpy-nrepl--eval-point (point))
  (lpy-nrepl--send
   `((op . "eval") (code . ,(string-trim code)))
   #'lpy-nrepl--display-eval-result))

;;;###autoload
(defun lpy-nrepl-eval-last-sexp ()
  "Evaluate the last s-expression via nREPL.
The result is displayed as an overlay and in the minibuffer."
  (interactive)
  (-when-let (code (lpy--last-sexp-string))
    (lpy-nrepl--eval-code code)))

;;;###autoload
(defun lpy-nrepl-eval-current-form ()
  "Evaluate the current form via nREPL.
The result is displayed as an overlay and in the minibuffer."
  (interactive)
  (-when-let (code (lpy--current-form-string))
    (lpy-nrepl--eval-code code)))

;;;###autoload
(defun lpy-nrepl-eval-region (start end)
  "Evaluate the region between START and END via nREPL."
  (interactive "r")
  (let ((code (string-trim (buffer-substring-no-properties start end))))
    (when (string-empty-p code)
      (user-error "Region is empty"))
    (lpy-nrepl--eval-code code)))

;;;###autoload
(defun lpy-nrepl-eval-buffer ()
  "Load the current buffer's file via nREPL.
Sends a `load-file' operation with the buffer's file path."
  (interactive)
  (unless (buffer-file-name)
    (user-error "Buffer is not visiting a file"))
  (when (buffer-modified-p)
    (save-buffer))
  (lpy-nrepl--send
   `((op . "load-file") (path . ,(buffer-file-name)))
   #'lpy-nrepl--display-eval-result))

;;;###autoload
(defun lpy-nrepl-eval-buffer-contents ()
  "Evaluate the entire buffer contents via nREPL without saving.
Sends the buffer text directly as an eval operation."
  (interactive)
  (let ((code (buffer-substring-no-properties (point-min) (point-max))))
    (when (string-empty-p (string-trim code))
      (user-error "Buffer is empty"))
    (lpy-nrepl--eval-code code)))

;;;###autoload
(defun lpy-nrepl-macroexpand ()
  "Macroexpand the form at point via nREPL.
The expansion is displayed in a `*lpy-macroexpand*' buffer."
  (interactive)
  (-when-let (code (or (lpy--current-form-string)
                       (lpy--last-sexp-string)))
    (lpy-nrepl--send
     `((op . "macroexpand") (code . ,(string-trim code)))
     #'lpy-nrepl--display-macroexpand-result)))

(defun lpy-nrepl--display-macroexpand-result (response)
  "Display a macroexpand RESPONSE in a popup buffer."
  (let ((expansion (alist-get 'expansion response))
        (error-msg (alist-get 'error response)))
    (cond
     (error-msg
      (message "%s" (propertize error-msg 'face 'error)))
     (expansion
      (with-current-buffer (get-buffer-create lpy-nrepl--macroexpand-buffer-name)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert expansion)
          (goto-char (point-min))
          (when (fboundp 'lpy-mode)
            (lpy-mode)))
        (display-buffer (current-buffer)
                        '(display-buffer-pop-up-window . ((window-height . 0.4)))))
      (lpy-nrepl--log "<=" expansion)
      (message "Macroexpansion displayed in %s" lpy-nrepl--macroexpand-buffer-name)))))

;;; Completion

(unless (fboundp 'company-lpy)
  (defun company-lpy (&rest _args)
    "Stub for backward compatibility. Use `lpy-nrepl-completion-at-point' instead."
    nil))

(defun lpy-nrepl-completion-at-point ()
  "Completion-at-point function using nREPL.
Works with corfu, company-capf, and default completion."
  (when (lpy-nrepl-connected-p)
    (let* ((end (point))
           (start (save-excursion
                    (skip-chars-backward "a-zA-Z0-9_\\-.*")
                    (point)))
           (prefix (buffer-substring-no-properties start end)))
      (when (> (length prefix) 0)
        (list start end
              (completion-table-dynamic
               (lambda (_)
                 (let ((result nil)
                       (done nil))
                   (lpy-nrepl--send
                    `((op . "complete") (prefix . ,prefix))
                    (lambda (response)
                      (setq result (append (alist-get 'completions response) nil))
                      (setq done t)))
                   (with-timeout (1 nil)
                     (while (not done)
                       (accept-process-output lpy-nrepl--connection 0.05)))
                   result)))
              :exclusive 'no)))))

;;; Eldoc (nREPL-backed)

(defun lpy-nrepl-eldoc-function (callback &rest _ignored)
  "Eldoc function that queries nREPL for documentation.
Calls CALLBACK with the docs string when available."
  (when (lpy-nrepl-connected-p)
    (let ((symbol (or (thing-at-point 'symbol t)
                      (save-excursion
                        (skip-chars-backward " \t")
                        (thing-at-point 'symbol t)))))
      (when (and symbol (> (length symbol) 0))
        (lpy-nrepl--send
         `((op . "docs") (symbol . ,symbol))
         (lambda (response)
           (let ((docs (alist-get 'docs response)))
             (when docs
               (funcall callback docs)))))
        t))))

;;; Dispatch Commands (nREPL-or-Shell)

;;;###autoload
(defun lpy-nrepl-eval-last-sexp-or-shell ()
  "Eval last sexp via nREPL if connected, otherwise via comint shell."
  (interactive)
  (if (lpy-nrepl-connected-p)
      (lpy-nrepl-eval-last-sexp)
    (lpy-shell-eval-last-sexp)))

;;;###autoload
(defun lpy-nrepl-eval-current-form-or-shell ()
  "Eval current form via nREPL if connected, otherwise via comint shell."
  (interactive)
  (if (lpy-nrepl-connected-p)
      (lpy-nrepl-eval-current-form)
    (lpy-shell-eval-current-form)))

;;;###autoload
(defun lpy-nrepl-eval-region-or-shell (start end)
  "Eval region via nREPL if connected, otherwise via comint shell."
  (interactive "r")
  (if (lpy-nrepl-connected-p)
      (lpy-nrepl-eval-region start end)
    (lpy-shell-eval-region start end)))

;;;###autoload
(defun lpy-nrepl-eval-buffer-or-shell ()
  "Load file via nREPL if connected, otherwise eval buffer via comint shell."
  (interactive)
  (if (lpy-nrepl-connected-p)
      (lpy-nrepl-eval-buffer)
    (lpy-shell-eval-buffer)))

;;;###autoload
(defun lpy-nrepl-eval-buffer-contents-or-shell ()
  "Eval buffer contents via nREPL if connected, otherwise via comint shell."
  (interactive)
  (if (lpy-nrepl-connected-p)
      (lpy-nrepl-eval-buffer-contents)
    (lpy-shell-eval-buffer)))

;;;###autoload
(defun lpy-nrepl-switch-to-repl ()
  "Start nREPL (or connect), or switch to `*lpy-nrepl*' buffer if connected.
Falls back to `run-lpy' comint shell when not connected."
  (interactive)
  (cond
   ((lpy-nrepl-connected-p)
    (display-buffer (lpy-nrepl--repl-buffer)
                    '(display-buffer-at-bottom . ((window-height . 0.3)))))
   (t
    (if (y-or-n-p "No nREPL connection.  Start nREPL server?")
        (lpy-nrepl-start)
      (run-lpy)))))

;;; REPL Mode

(defvar lpy-nrepl-repl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `lpy-nrepl-repl-mode'.")

(define-derived-mode lpy-nrepl-repl-mode special-mode "Lpy-nREPL"
  "Major mode for the nREPL interaction log buffer.
Shows input sent to and output received from the nREPL server."
  (setq-local truncate-lines nil)
  (setq-local word-wrap t))

;;; Provide:

(provide 'lpy-nrepl)

;;; lpy-nrepl.el ends here
