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
;;
;; Buffer evals run in their file's scope.  `lpy-nrepl-set-file-scope' picks
;; a file whose scope `lpy-nrepl-eval-string' evals in (default: the
;; session's `__main__'); the choice is client state, shown in the REPL log.

;;; Code:

(require 'lpy-base)
(require 'json)
(require 'subr-x)

(declare-function run-lpy "lpy-shell" ())
(declare-function lpy-shell-eval-current-form "lpy-shell" ())
(declare-function lpy-shell-eval-last-sexp "lpy-shell" ())
(declare-function lpy-shell-eval-region "lpy-shell" (start end))
(declare-function lpy-shell-eval-buffer "lpy-shell" ())

;;; Configuration

(defvar lpy-nrepl--host "127.0.0.1"
  "Host for the nREPL server.")

(defvar lpy-nrepl--default-port nil
  "Default port for `lpy-nrepl-connect'.  When nil, prompt interactively.")

(defconst lpy-nrepl--port-file ".nrepl-port"
  "Name of the file `lpy --nrepl' writes its port to.
The file holds the decimal port and sits in the server's working directory.")

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

;; The file scope `lpy-nrepl-eval-string' evals in: an absolute file path
;; (exactly a `buffer-file-name', the string buffer evals send as `file'),
;; or nil for the session's `__main__'.  Global: there is one REPL session.
(defvar lpy-nrepl--scope nil
  "File path whose scope `lpy-nrepl-eval-string' uses, or nil for `__main__'.")

(defconst lpy-nrepl--main-scope-label "__main__"
  "Display name of the session's default scope (`lpy-nrepl--scope' nil).")

(defconst lpy-nrepl--ns-field 'ns
  "Name of the nREPL request field selecting a file's scope.
Unlike `file' it does not rename the eval; the one home of the key.")

(defvar lpy-nrepl--eval-string-history nil
  "Minibuffer history for `lpy-nrepl-eval-string'.")

;;; Helpers

(defun lpy-nrepl--ns-fields ()
  "Return the nREPL scope field for `lpy-nrepl--scope', or nil for `__main__'."
  (and lpy-nrepl--scope
       (list (cons lpy-nrepl--ns-field lpy-nrepl--scope))))

(defun lpy-nrepl--scope-label (&optional full)
  "Return a display string for `lpy-nrepl--scope'.
With FULL, abbreviate the whole path instead of keeping the file name."
  (cond ((null lpy-nrepl--scope) lpy-nrepl--main-scope-label)
        (full (abbreviate-file-name lpy-nrepl--scope))
        (t (file-name-nondirectory lpy-nrepl--scope))))

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
    ;; Window points are per-window markers and with-current-buffer moves
    ;; only buffer point.  Like comint, follow new output only in windows
    ;; already at the end: a user reading earlier output keeps their place,
    ;; and follows again once back at the end.  Buffer point matters when
    ;; no window shows the log (it is what C-c C-z will display), so a
    ;; buffer at its end keeps following too.
    (let* ((inhibit-read-only t)
           (old-end (point-max))
           (point-at-end (= (point) old-end))
           (following nil))
      (dolist (win (get-buffer-window-list (current-buffer) nil t))
        (when (= (window-point win) old-end)
          (push win following)))
      (save-excursion
        (goto-char (point-max))
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
           (insert (propertize (concat text "\n") 'read-only t)))))
      (when point-at-end
        (goto-char (point-max)))
      (dolist (win following)
        (set-window-point win (point-max))))))

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

(defun lpy-nrepl--process-sentinel (proc event)
  "Handle connection state changes of PROC.
Only the current connection may clear state: a replaced process still
reports its own deletion, which must not drop its successor."
  (let ((event-trimmed (string-trim event)))
    (cond
     ((string-match-p "^open" event-trimmed)
      (message "lpy-nrepl: connected"))
     ((and (eq proc lpy-nrepl--connection)
           (string-match-p "\\(deleted\\|connection broken\\|failed\\)" event-trimmed))
      (message "lpy-nrepl: disconnected (%s)" event-trimmed)
      ;; Swap rather than just clearing the variable: a remote-closed process
      ;; stays in `process-list' until deleted, and its callbacks never fire.
      (lpy-nrepl--swap-connection nil)))))

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

(defun lpy-nrepl--read-port-file (file)
  "Return the port number stored in port FILE, or nil when it is unusable."
  (when (file-readable-p file)
    (let ((text (string-trim
                 (with-temp-buffer
                   (insert-file-contents file nil 0 32)
                   (buffer-string)))))
      (when (string-match-p "\\`[0-9]+\\'" text)
        (let ((port (string-to-number text)))
          (and (<= 1 port 65535) port))))))

(defun lpy-nrepl--discover-port (&optional dir)
  "Return the port named by the nearest `.nrepl-port' above DIR, or nil.
DIR defaults to `default-directory'."
  (let ((root (locate-dominating-file (or dir default-directory)
                                      lpy-nrepl--port-file)))
    (and root
         (lpy-nrepl--read-port-file
          (expand-file-name lpy-nrepl--port-file root)))))

(defun lpy-nrepl--read-port ()
  "Read a port from the minibuffer, defaulting to the discovered one."
  (let ((found (or (lpy-nrepl--discover-port) lpy-nrepl--default-port)))
    (if found
        (read-number "Port: " found)
      (read-number "Port: "))))

(defun lpy-nrepl--swap-connection (new)
  "Make NEW (a process or nil) the connection and delete the previous one.
Pending callbacks belong to the old connection and are dropped.  NEW is
installed before the old process is deleted: `delete-process' runs its
sentinel synchronously, and the sentinel only clears state for the current
connection, so it must no longer see the old process as current."
  (let ((old lpy-nrepl--connection))
    (setq lpy-nrepl--connection new)
    (setq lpy-nrepl--pending-callbacks nil)
    ;; A replaced or dropped server's file scopes may be gone, and string
    ;; evals would silently run in an empty one; the log records the reset.
    ;; Quiet: the caller's own message (e.g. the sentinel's disconnect reason)
    ;; must stay in the echo area.
    ;; A scope chosen while disconnected (OLD nil) is kept for the first connect.
    (when (and old lpy-nrepl--scope)
      (lpy-nrepl--set-scope nil "connection changed" t))
    (when old
      (delete-process old))))

;;;###autoload
(defun lpy-nrepl-connect (port)
  "Connect to an nREPL server on 127.0.0.1:PORT.
Interactively, the port defaults to the one in the nearest `.nrepl-port'."
  (interactive (list (lpy-nrepl--read-port)))
  ;; Open first, swap after: a stale port (e.g. from `.nrepl-port') must not
  ;; drop a live connection.  The connect is synchronous (:nowait nil) and
  ;; signals on failure, leaving the old connection, callbacks and response
  ;; buffer untouched.  Being synchronous, two connects cannot overlap, so
  ;; there is no pending connection to cancel.  The old process is deleted
  ;; only after the new one is installed (see `lpy-nrepl--swap-connection').
  (let ((new (make-network-process
              :name "lpy-nrepl"
              :host lpy-nrepl--host
              :service port
              :family 'ipv4
              :nowait nil
              :filter #'lpy-nrepl--process-filter
              :sentinel #'lpy-nrepl--process-sentinel
              :coding 'utf-8)))
    (setq lpy-nrepl--response-buffer "")
    (set-process-query-on-exit-flag new nil)
    (lpy-nrepl--swap-connection new))
  (display-buffer (lpy-nrepl--repl-buffer)
                  '(display-buffer-at-bottom . ((window-height . 0.3))))
  (message "lpy-nrepl: connected to %s:%d" lpy-nrepl--host port))

;;;###autoload
(defun lpy-nrepl-connect-project ()
  "Connect using the `.nrepl-port' file in the project root."
  (interactive)
  (let ((port (lpy-nrepl--discover-port)))
    (unless port
      (user-error "No usable %s found above %s"
                  lpy-nrepl--port-file default-directory))
    (lpy-nrepl-connect port)))

;;;###autoload
(defun lpy-nrepl-disconnect ()
  "Close the nREPL connection."
  (interactive)
  (when lpy-nrepl--connection
    (lpy-nrepl--swap-connection nil)
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

(defun lpy-nrepl--set-scope (path &optional reason quiet)
  "Set the eval scope to file PATH (nil: `__main__'), log and announce it.
REASON, a string, is appended in parentheses to the log line.  QUIET skips
the echo-area message (the log still records it)."
  (setq lpy-nrepl--scope path)
  (let ((text (format "scope: %s%s" (lpy-nrepl--scope-label t)
                      (if reason (format " (%s)" reason) ""))))
    (lpy-nrepl--log "scope" (concat ";; " text))
    (force-mode-line-update t)
    (unless quiet
      (message "lpy-nrepl %s" text))))

(defun lpy-nrepl--scope-candidates ()
  "Return an alist (DISPLAY . PATH) of file-visiting `lpy-mode' buffers."
  (let (alist)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and (derived-mode-p 'lpy-mode) buffer-file-name)
          (push (cons (abbreviate-file-name buffer-file-name) buffer-file-name)
                alist))))
    (nreverse (cl-delete-duplicates alist :key #'car :test #'equal :from-end t))))

;;;###autoload
(defun lpy-nrepl-set-file-scope ()
  "Choose the file scope for `lpy-nrepl-eval-string'.
Pick among the open `lpy-mode' file buffers; empty input selects `__main__'."
  (interactive)
  ;; An explicit first candidate and DEF: with require-match and no default,
  ;; vertico preselects candidate 0, so RET would pick the first file.
  (let* ((main lpy-nrepl--main-scope-label)
         (alist (cons (cons main nil) (lpy-nrepl--scope-candidates)))
         (choice (completing-read (format "File scope (default %s): " main)
                           alist nil t nil nil main)))
    (lpy-nrepl--set-scope
     (and (not (string-empty-p choice)) (cdr (assoc choice alist))))))

;;;###autoload
(defun lpy-nrepl-eval-string (code)
  "Evaluate CODE, read from the minibuffer, in the chosen file scope.
The result goes to the echo area and REPL log, with no overlay."
  (interactive
   (list (read-string (format "Eval in %s: " (lpy-nrepl--scope-label))
                      nil 'lpy-nrepl--eval-string-history)))
  (let ((trimmed (string-trim code)))
    (when (string-empty-p trimmed)
      (user-error "Nothing to evaluate"))
    (lpy-nrepl--send
     `((op . "eval") (code . ,trimmed) ,@(lpy-nrepl--ns-fields))
     (lambda (response)
       ;; Not tied to a source buffer: keep an earlier eval's overlay target
       ;; (buffer-local) from receiving this result.
       (let ((lpy-nrepl--eval-buffer nil))
         (lpy-nrepl--display-eval-result response))))))

(defun lpy-nrepl--file-fields ()
  "Return the nREPL `file' field for the current buffer, or nil.
The server answers a request naming a file from that file's scope."
  (and (buffer-file-name) `((file . ,(buffer-file-name)))))

(defun lpy-nrepl--position-fields (pos)
  "Return the nREPL eval fields locating buffer position POS.
The fields are `file' (when the buffer visits one), 1-based `line' and
1-based `column'; the server reports errors at that position."
  ;; The column is a character offset, as Python counts code points; never
  ;; `current-column', which counts display width (tabs, wide Hangul).
  ;; `line-beginning-position' stops at `point-min', so widen first.
  (save-excursion
    (save-restriction
      (widen)
      (goto-char pos)
      (append (lpy-nrepl--file-fields)
              `((line . ,(line-number-at-pos pos t))
                (column . ,(1+ (- pos (line-beginning-position)))))))))

(defun lpy-nrepl--eval-code (code start)
  "Send CODE, which begins at buffer position START, for evaluation.
Saves current buffer and point for the overlay.  CODE is trimmed, and the
position sent is that of its first character after the trim."
  (setq lpy-nrepl--eval-buffer (current-buffer))
  (setq lpy-nrepl--eval-point (point))
  (let ((leading (- (length code) (length (string-trim-left code)))))
    (lpy-nrepl--send
     `((op . "eval") (code . ,(string-trim code))
       ,@(lpy-nrepl--position-fields (+ start leading)))
     #'lpy-nrepl--display-eval-result)))

(defun lpy-nrepl--eval-bounds (bounds)
  "Evaluate the text of BOUNDS, a (START . END) cons, via nREPL."
  (-when-let ((start . end) bounds)
    (lpy-nrepl--eval-code (buffer-substring-no-properties start end) start)))

;;;###autoload
(defun lpy-nrepl-eval-last-sexp ()
  "Evaluate the last s-expression via nREPL.
The result is displayed as an overlay and in the minibuffer."
  (interactive)
  (lpy-nrepl--eval-bounds (lpy--last-sexp-bounds)))

;;;###autoload
(defun lpy-nrepl-eval-current-form ()
  "Evaluate the current form via nREPL.
The result is displayed as an overlay and in the minibuffer."
  (interactive)
  (lpy-nrepl--eval-bounds (lpy--current-form-bounds)))

;;;###autoload
(defun lpy-nrepl-eval-region (start end)
  "Evaluate the region between START and END via nREPL."
  (interactive "r")
  (when (string-empty-p (string-trim (buffer-substring-no-properties start end)))
    (user-error "Region is empty"))
  (lpy-nrepl--eval-code (buffer-substring-no-properties start end) start))

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
    (lpy-nrepl--eval-code code (point-min))))

;;;###autoload
(defun lpy-nrepl-macroexpand ()
  "Macroexpand the form at point via nREPL.
The expansion is displayed in a `*lpy-macroexpand*' buffer and is
resolved in the buffer file's scope (the session scope without a file)."
  (interactive)
  (-when-let (code (or (lpy--current-form-string)
                       (lpy--last-sexp-string)))
    (lpy-nrepl--send
     `((op . "macroexpand") (code . ,(string-trim code))
       ,@(lpy-nrepl--file-fields))
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

(defconst lpy--completion-kinds
  '((text " text" text)
    (method " method" method)
    (function " function" function)
    (constructor " constructor" constructor)
    (field " field" field)
    (variable " variable" variable)
    (class " class" class)
    (interface " interface" interface)
    (module " module" module)
    (property " property" property)
    (unit " unit" unit)
    (value " value" value)
    (enum " enum" enum)
    (keyword " keyword" keyword)
    (snippet " snippet" snippet)
    (color " color" color)
    (file " file" file)
    (reference " reference" reference)
    (folder " folder" folder)
    (enum-member " enum member" enum-member)
    (constant " constant" constant)
    (struct " struct" struct)
    (event " event" event)
    (operator " operator" operator)
    (type-parameter " type parameter" type-parameter)
    (macro " macro" function))
  "Each completion kind as (KIND ANNOTATION COMPANY-KIND).
The one vocabulary of candidate annotations: nREPL candidates (whose
kind names are the KIND symbols) and LSP candidates (through
`lpy--lsp-kinds') both resolve through it.  Annotations are the full
lowercase kind word, as JupyterLab, elpy and nvim-cmp show it.")

(defconst lpy--lsp-kinds
  (let ((kinds (make-hash-table)))
    (cl-loop for kind in '(text method function constructor field variable
                           class interface module property unit value enum
                           keyword snippet color file reference folder
                           enum-member constant struct event operator
                           type-parameter)
             for number from 1
             do (puthash number kind kinds))
    kinds)
  "The `lpy--completion-kinds' kind of each LSP CompletionItemKind number.")

(defun lpy--kind-annotation (kind)
  "Return the annotation for KIND, a `lpy--completion-kinds' symbol."
  (nth 1 (assq kind lpy--completion-kinds)))

(defun lpy-nrepl--kind (name)
  "Return the kind symbol of NAME, a kind the nREPL `complete' op returns.
Nil when NAME is no known kind."
  (and (stringp name)
       (car (assq (intern-soft name) lpy--completion-kinds))))

(defun lpy-nrepl--kind-annotation (name)
  "Return the annotation for nREPL kind NAME, or nil when unknown."
  (lpy--kind-annotation (lpy-nrepl--kind name)))

(defun lpy-nrepl--kinds-lookup (kinds candidate)
  "Return the kind of CANDIDATE in KINDS, the alist from a complete response."
  (alist-get (intern candidate) kinds))

(defun lpy-nrepl-completion-at-point ()
  "Completion-at-point function using nREPL.
Works with corfu, company-capf, and default completion.  Candidates are
annotated with the kind the server reports for them.  Candidates come
from the buffer file's scope (the session scope without a file)."
  (when (lpy-nrepl-connected-p)
    (let* ((end (point))
           (start (save-excursion
                    (skip-chars-backward "a-zA-Z0-9_\\-.*")
                    (point)))
           (prefix (buffer-substring-no-properties start end))
           ;; Computed here: the table may run in another buffer.
           (file-fields (lpy-nrepl--file-fields))
           (kinds nil)
           (result nil)
           (fetched nil))
      (when (> (length prefix) 0)
        (list start end
              (completion-table-dynamic
               (lambda (_)
                 ;; The fetch depends only on the prefix captured above, so
                 ;; one request serves every table call; a timeout stays retryable.
                 (unless fetched
                   (lpy-nrepl--send
                    `((op . "complete") (prefix . ,prefix) ,@file-fields)
                    (lambda (response)
                      (setq result (append (alist-get 'completions response) nil))
                      (setq kinds (alist-get 'kinds response))
                      (setq fetched t)))
                   (with-timeout (1 nil)
                     (while (not fetched)
                       (accept-process-output lpy-nrepl--connection 0.05))))
                 result))
              :annotation-function
              (lambda (candidate)
                (lpy-nrepl--kind-annotation
                 (lpy-nrepl--kinds-lookup kinds candidate)))
              :company-kind
              (lambda (candidate)
                (nth 2 (assq (lpy-nrepl--kind
                              (lpy-nrepl--kinds-lookup kinds candidate))
                             lpy--completion-kinds)))
              :exclusive 'no)))))

;;;; Merged completion (LSP + nREPL)

(declare-function eglot-completion-at-point "eglot" ())
(declare-function eglot-managed-p "eglot" ())

(defconst lpy--capf-candidate-properties
  '(:annotation-function :exit-function :company-doc-buffer :company-docsig
    :company-location :company-kind :company-deprecated)
  "Capf properties that take a candidate.
A merged completion dispatches each to the source that owns the candidate.")

(defun lpy--capf-table-metadata (table pred)
  "Return the metadata alist (without the `metadata' head) of TABLE.
PRED is the predicate TABLE was given."
  (cdr (completion-metadata "" table pred)))

(defun lpy--merge-capf-results (main extra)
  "Merge capf results MAIN and EXTRA, each (BEG END TABLE . PLIST) or nil.
A candidate name both offer is kept once, as MAIN's string, so MAIN's
annotation, docs and exit function apply to it.  Names only EXTRA offers
follow MAIN's candidates and keep EXTRA's per-candidate properties."
  (cond
   ((null extra) main)
   ((null main) extra)
   ;; Candidates completing different prefixes cannot be compared or
   ;; shown in one list (the same rule as `cape-capf-super').
   ((/= (car main) (car extra)) main)
   (t
    (pcase-let* ((`(,beg ,main-end ,main-table . ,main-plist) main)
                 (`(,_ ,extra-end ,extra-table . ,extra-plist) extra)
                 (main-pred (plist-get main-plist :predicate))
                 (extra-pred (plist-get extra-plist :predicate))
                 (extra-only (make-hash-table :test #'equal)))
      (cl-labels
          ((and-pred (own pred)
             (cond ((and own pred) (lambda (&rest args)
                                     (and (apply own args) (apply pred args))))
                   (t (or own pred))))
           (candidates (str pred)
             (clrhash extra-only)
             (let ((main-cands (all-completions str main-table
                                                (and-pred main-pred pred)))
                   (seen (make-hash-table :test #'equal))
                   (kept nil))
               (dolist (cand main-cands)
                 (puthash (substring-no-properties cand) t seen))
               (dolist (cand (all-completions str extra-table
                                              (and-pred extra-pred pred)))
                 (let ((name (substring-no-properties cand)))
                   (unless (gethash name seen)
                     (puthash name t seen)
                     (puthash name t extra-only)
                     (push cand kept))))
               ;; MAIN's strings are returned as they are: eglot keeps
               ;; its LSP item as a text property on the candidate.
               (append main-cands (nreverse kept))))
           (owner-plist (cand)
             (if (gethash (substring-no-properties cand) extra-only)
                 extra-plist
               main-plist)))
        (let* ((main-meta (lpy--capf-table-metadata main-table main-pred))
               (extra-meta (lpy--capf-table-metadata extra-table extra-pred))
               (sorter
                (lambda (key)
                  (let ((main-sort (or (alist-get key main-meta) #'identity))
                        (extra-sort (or (alist-get key extra-meta) #'identity)))
                    (lambda (cands)
                      (let (main-part extra-part)
                        (dolist (cand cands)
                          (if (gethash (substring-no-properties cand) extra-only)
                              (push cand extra-part)
                            (push cand main-part)))
                        (append (funcall main-sort (nreverse main-part))
                                (funcall extra-sort (nreverse extra-part))))))))
               (metadata
                ;; MAIN's `category' is kept, so a style tied to it
                ;; (eglot's `eglot-capf') still applies.
                `(metadata
                  (display-sort-function . ,(funcall sorter 'display-sort-function))
                  (cycle-sort-function . ,(funcall sorter 'cycle-sort-function))
                  ,@(cl-remove-if (lambda (entry)
                                  (memq (car-safe entry)
                                        '(display-sort-function
                                          cycle-sort-function)))
                                main-meta)))
               (table
                (lambda (str pred action)
                  (pcase action
                    ('metadata metadata)
                    (`(boundaries . ,_) nil)
                    ;; Not re-filtered by prefix: eglot's table returns
                    ;; flex matches that a prefix filter would drop.
                    ('t (candidates str pred))
                    (_ (complete-with-action action (candidates str pred)
                                             str nil)))))
               (plist (cl-loop for (key value) on main-plist by #'cddr
                               unless (eq key :predicate)
                               nconc (list key value))))
          (dolist (prop lpy--capf-candidate-properties)
            (when (or (plist-member main-plist prop)
                      (plist-member extra-plist prop))
              (setq plist
                    (plist-put plist prop
                               (lambda (cand &rest args)
                                 (let ((fn (plist-get (owner-plist cand) prop)))
                                   (when fn (apply fn cand args))))))))
          `(,beg ,(max main-end extra-end) ,table ,@plist)))))))

(defun lpy--eglot-item (cand)
  "Return the LSP CompletionItem plist eglot attached to CAND, or nil.
`eglot--lsp-item' is eglot's internal text property; the tests pin it."
  (and (stringp cand) (> (length cand) 0)
       (get-text-property 0 'eglot--lsp-item cand)))

(defun lpy--lsp-signature (item)
  "Return the trimmed `detail' of LSP completion ITEM, or nil when empty."
  (let ((detail (plist-get item :detail)))
    (when (stringp detail)
      (let ((sig (string-trim detail)))
        (unless (string-empty-p sig) sig)))))

(defun lpy--lsp-annotation (item)
  "Return the list annotation of LSP completion ITEM.
`labelDetails.description' when the server sends one, else the kind's
tag.  Never `detail': backends such as pyrefly put a whole multi-line
overload set there, which LSP means for the focused item only."
  (let ((desc (plist-get (plist-get item :labelDetails) :description)))
    (if (and (stringp desc) (not (string-empty-p desc)))
        (concat " " desc)
      (lpy--kind-annotation (gethash (plist-get item :kind) lpy--lsp-kinds)))))

(defun lpy--signature-doc-buffer (sig docs)
  "Return a doc buffer showing SIG above DOCS, a buffer, (BUFFER . POS) or nil."
  (let* ((docs-buffer (if (consp docs) (car docs) docs))
         (text (and (buffer-live-p docs-buffer)
                    (with-current-buffer docs-buffer (buffer-string)))))
    (with-current-buffer (get-buffer-create " *lpy doc*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize sig 'face 'font-lock-function-name-face))
        (when (and text (not (string-empty-p (string-trim text))))
          (insert "\n\n" text)))
      (current-buffer))))

(defun lpy--shorten-eglot-capf (result)
  "Return eglot capf RESULT with short annotations and the signature in the docs.
Each candidate is annotated by `lpy--lsp-annotation'; its signature (the
item's `detail') heads the `:company-doc-buffer' popup and is the
`:company-docsig' line, as CIDER shows the arglist outside the list."
  (pcase-let ((`(,beg ,end ,table . ,plist) result))
    (when result
      (let ((annotate (plist-get plist :annotation-function))
            (doc-buffer (plist-get plist :company-doc-buffer))
            (docsig (plist-get plist :company-docsig)))
        `(,beg ,end ,table
          :annotation-function
          ,(lambda (cand)
             (let ((item (lpy--eglot-item cand)))
               (if item
                   (lpy--lsp-annotation item)
                 (and annotate (funcall annotate cand)))))
          :company-doc-buffer
          ,(lambda (cand)
             (let ((sig (lpy--lsp-signature (lpy--eglot-item cand)))
                   (docs (and doc-buffer (funcall doc-buffer cand))))
               (if sig (lpy--signature-doc-buffer sig docs) docs)))
          :company-docsig
          ,(lambda (cand)
             (let ((sig (lpy--lsp-signature (lpy--eglot-item cand))))
               (if sig
                   (replace-regexp-in-string "[ \t\n]+" " " sig)
                 (and docsig (funcall docsig cand)))))
          ,@(cl-loop for (key value) on plist by #'cddr
                     unless (memq key '(:annotation-function
                                        :company-doc-buffer
                                        :company-docsig))
                     nconc (list key value)))))))

(defun lpy-completion-at-point ()
  "Completion-at-point function for `lpy-mode' buffers.
Merges the candidates of `eglot-completion-at-point', when eglot manages
the buffer, with those of `lpy-nrepl-completion-at-point', when an nREPL
is connected.  A name both offer appears once, with eglot's docs and exit
function; names only the nREPL knows keep its annotation and kind.  Both
sources are annotated with one short kind tag (`lpy--completion-kinds');
an LSP candidate's signature goes to the doc popup and the docsig line
\(`lpy--shorten-eglot-capf').  Works with neither, either or both
sources available."
  (lpy--merge-capf-results
   (lpy--shorten-eglot-capf
    (and (fboundp 'eglot-managed-p) (eglot-managed-p)
         (eglot-completion-at-point)))
   (lpy-nrepl-completion-at-point)))

;;; Eldoc (nREPL-backed)

(defun lpy-nrepl-eldoc-function (callback &rest _ignored)
  "Eldoc function that queries nREPL for documentation.
Calls CALLBACK with the docs string when available.  The symbol is
looked up in the buffer file's scope (the session scope without a file)."
  (when (lpy-nrepl-connected-p)
    (let ((symbol (or (thing-at-point 'symbol t)
                      (save-excursion
                        (skip-chars-backward " \t")
                        (thing-at-point 'symbol t)))))
      (when (and symbol (> (length symbol) 0))
        (lpy-nrepl--send
         `((op . "docs") (symbol . ,symbol) ,@(lpy-nrepl--file-fields))
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
    (define-key map (kbd "C-c M-n") #'lpy-nrepl-set-file-scope)
    (define-key map (kbd "C-c M-:") #'lpy-nrepl-eval-string)
    map)
  "Keymap for `lpy-nrepl-repl-mode'.")

(define-derived-mode lpy-nrepl-repl-mode special-mode "Lpy-nREPL"
  "Major mode for the nREPL interaction log buffer.
Shows input sent to and output received from the nREPL server."
  (setq-local truncate-lines nil)
  (setq-local word-wrap t)
  (setq mode-line-process
        '(:eval (concat " ["
                  (replace-regexp-in-string "%" "%%" (lpy-nrepl--scope-label)
                                            t t)
                  "]"))))

;;; Provide:

(provide 'lpy-nrepl)

;;; lpy-nrepl.el ends here
