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
(require 'thingatpt) ; `bounds-of-thing-at-point-provider-alist' is why Emacs 30 is required

(declare-function run-lpy "lpy-shell" ())
(declare-function lpy-shell-eval-current-form "lpy-shell" ())
(declare-function lpy-shell-eval-last-sexp "lpy-shell" ())
(declare-function lpy-shell-eval-region "lpy-shell" (start end))
(declare-function lpy-shell-eval-buffer "lpy-shell" ())

;;; Configuration

(defgroup lpy-nrepl nil
  "nREPL client for LisPython."
  :group 'languages
  :prefix "lpy-nrepl-")

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

;;; Wire protocol names
;; Each op and field name the client reads or writes beyond the basic
;; request/reply has one home here: a misspelt boundary string compiles and
;; the feature just stops firing.

(defconst lpy-nrepl--op-eval "eval" "Op evaluating a code string.")
(defconst lpy-nrepl--op-load-file "load-file" "Op loading a file by path.")
(defconst lpy-nrepl--op-complete "complete" "Op listing completions of a prefix.")
(defconst lpy-nrepl--op-docs "docs" "Op looking up a symbol's docs.")
(defconst lpy-nrepl--op-macroexpand "macroexpand" "Op macroexpanding a form.")

(defconst lpy-nrepl--eval-ops (list lpy-nrepl--op-eval lpy-nrepl--op-load-file)
  "Request ops that run user code.
They are sent with `lpy-nrepl--stream-field' and tracked in
`lpy-nrepl--in-flight-evals' until their final reply.")

(defconst lpy-nrepl--stream-field 'stream
  "Request field opting an eval into interim output messages.")

(defconst lpy-nrepl--out-op "out"
  "Op of an interim message carrying an eval's output.
Every other message with an id is that request's final reply.")

(defconst lpy-nrepl--stdout-field 'stdout
  "Message field holding standard output text.")

(defconst lpy-nrepl--stderr-field 'stderr
  "Message field holding standard error text.")

(defconst lpy-nrepl--interrupt-op "interrupt"
  "Op asking the server to raise KeyboardInterrupt in a running request.")

(defconst lpy-nrepl--interrupt-id-field 'interrupt-id
  "Field of an interrupt request naming the request to interrupt.")

(defconst lpy-nrepl--unknown-op-error "unknown op"
  "Text of the server's error reply to an op it does not implement.")

(defconst lpy-nrepl--interrupt-result-op "interrupt-result"
  "Op of the server's reply accepting an interrupt request.")

(defconst lpy-nrepl--superseding-ops (list lpy-nrepl--op-complete lpy-nrepl--op-docs)
  "Request ops whose newer request makes an older unanswered one moot.
Sending one drops the pending callback of the previous request of its op,
so a server that answers nothing (busy on an eval) cannot pile them up.")

;;; Request state

(defvar lpy-nrepl--in-flight-evals nil
  "Ids of `lpy-nrepl--eval-ops' requests without a final reply, newest first.
The server runs a connection's evals one at a time in request order, so
the last element is the running one.")

(defvar lpy-nrepl--ignored-ids nil
  "Ids of superseded requests whose late final reply is dropped silently.")

(defvar lpy-nrepl--latest-by-op nil
  "Alist (OP . ID) of the newest request of each `lpy-nrepl--superseding-ops'.")

(defvar lpy-nrepl--answers-during-eval nil
  "Non-nil once the connection answered a request sent behind an eval.
That proves the server serves other requests while an eval runs, so
completion may wait for its reply (`lpy-nrepl--may-wait-p').")

(defcustom lpy-nrepl-request-timeout 1.0
  "Seconds a synchronous request (completion) waits for its reply, at most.
Waited once per completion, and not at all while an eval runs on a server
that has not shown it answers during one (`lpy-nrepl--may-wait-p')."
  :type 'number
  :group 'lpy-nrepl)

(defcustom lpy-nrepl-log-max-size 1000000
  "Most characters the REPL log `*lpy-nrepl*' keeps, or nil for no limit.
Once over it, the top is cut down to 90% of it, at the next line start
when one lies within 10% of it and mid-line otherwise, so an endless
printing loop cannot grow the buffer without bound."
  :type '(choice (const :tag "Unlimited" nil) integer)
  :group 'lpy-nrepl)

(defcustom lpy-nrepl-server-log-max-size 1000000
  "Most characters the server buffer `*lpy-nrepl-server*' keeps, or nil.
Output of threads an eval started goes to the server's own stdout and
stderr, so a chatty one would otherwise grow the buffer without bound.
Trimmed like `lpy-nrepl-log-max-size'."
  :type '(choice (const :tag "Unlimited" nil) integer)
  :group 'lpy-nrepl)

(defvar lpy-nrepl-start-timeout 10
  "Seconds `lpy-nrepl-start' waits for the server's port line.")

(defvar lpy-nrepl-interrupt-timeout 2
  "Seconds after which an unanswered interrupt is reported.")

(defconst lpy-nrepl--server-started-regexp
  "nREPL server started on [^\n]+:\\([0-9]+\\)\r?\n"
  "Regexp matching the server's startup line; group 1 is the port.
The host may itself contain colons (`::1', `[::1]'), so the port is the
digits after the last colon of the line.
The line must be complete: a port line split across output chunks would
otherwise record the digits seen so far.")

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

(defun lpy-nrepl--trim-buffer (max-size)
  "Drop text from the top of the current buffer to fit MAX-SIZE characters.
MAX-SIZE nil means no limit.  Only once over it, and then down to 90% of
it, so the deletion's cost is spread over many appended chunks.  Window
points and point are positions the deletion shifts, so they keep their text."
  (when (and max-size (> (buffer-size) max-size))
    (let* ((inhibit-read-only t)
           (keep (floor (* 0.9 max-size)))
           (cut (max (point-min) (- (point-max) keep)))
           ;; How far past the cut a line start may move it: unbounded,
           ;; a cut inside one long line snapped to the newline before
           ;; the newest entry and dropped almost the whole log.
           (slack (floor (* 0.1 max-size))))
      (save-excursion
        (goto-char cut)
        (unless (or (bolp)
                    (search-forward "\n" (min (point-max) (+ cut slack)) t))
          (goto-char cut))
        (delete-region (point-min) (point))))))

(defun lpy-nrepl--log (direction text)
  "Append TEXT to the nREPL REPL buffer in CIDER-like format.
DIRECTION is one of: \"input\" \"value\" \"stdout\" \"error\", or
\"stdout-chunk\" / \"stderr-chunk\" for streamed output, inserted as is:
a chunk need not end a line.  Any other entry starts on a fresh line."
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
        (unless (or (bolp) (member direction '("stdout-chunk" "stderr-chunk")))
          (insert (propertize "\n" 'read-only t)))
        (pcase direction
          ("stdout-chunk"
           (insert (propertize text 'read-only t)))
          ("stderr-chunk"
           (insert (propertize text 'face 'error 'read-only t)))
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
           (insert (propertize (concat text "\n") 'read-only t))))
        (lpy-nrepl--trim-buffer lpy-nrepl-log-max-size))
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

(defconst lpy-nrepl--reconnect-hint
  "C-c C-z to connect to our server once it is up, or start one"
  "Log hint for a lost connection, true whether or not our server runs.
\\[lpy-nrepl-switch-to-repl] connects to a live server of ours once it
has reported its port (until then it says the server is still starting),
and offers to start one when none runs.")

(defconst lpy-nrepl--restart-hint
  "C-c C-z or M-x lpy-nrepl-restart to start a new one"
  "Log hint for the death of our own server.")

(defun lpy-nrepl--process-sentinel (proc event)
  "Handle connection state changes of PROC.
Only the current connection may clear state: a replaced process still
reports its own deletion, which must not drop its successor."
  ;; Liveness, not the event text: the texts vary by platform and cause
  ;; ("connection broken by remote peer", "deleted", "failed with code
  ;; ..."), and a missed spelling would leave a dead connection current.
  (cond
   ((process-live-p proc)
    (when (string-prefix-p "open" event)
      (message "lpy-nrepl: connected")))
   ((eq proc lpy-nrepl--connection)
    (lpy-nrepl--connection-lost
     (format "nREPL connection lost (%s)" (string-trim event))
     nil lpy-nrepl--reconnect-hint))))

(defun lpy-nrepl--connection-lost (what &optional port hint)
  "Drop the current connection, and log WHAT.
WHAT describes the loss (the server went away, or a send failed or
was quit).  With PORT, only a connection to that port is
dropped (the Emacs-owned server's death must not drop a connection to
another server); WHAT is logged either way.  Pending callbacks and
in-flight evals are cleared with the connection, and the log line says
how many requests will never be answered.  HINT, from the caller (who
knows what is still running), ends the line when the current connection
was dropped or, with PORT, there was none; a current connection to another
server gets no hint."
  (let* ((conn lpy-nrepl--connection)
         (ours (and conn
                    (or (null port)
                        (equal (process-contact conn :service) port))))
         (pending (if ours (length lpy-nrepl--pending-callbacks) 0))
         ;; The connection's sentinel can run before the server's, leaving no
         ;; connection when the server-died line is logged; it still needs
         ;; its hint.
         (hint (and hint (or ours (and port (null conn)))
                    (concat "; " hint))))
    (lpy-nrepl--log
     "error"
     (if ours
         (format ";; %s — %d pending request(s) dropped%s" what pending
                 (or hint ""))
       (format ";; %s%s" what (or hint ""))))
    (when ours
      ;; Swap rather than just clearing the variable: a remote-closed process
      ;; stays in `process-list' until deleted, and its callbacks never fire.
      (lpy-nrepl--swap-connection nil))
    (message "lpy-nrepl: %s" what)))

(defun lpy-nrepl--request-number (id)
  "Return request ID (a string) as a number, for ordering."
  (if (stringp id) (string-to-number id) 0))

(defun lpy-nrepl--log-output (msg)
  "Append the output an interim message MSG carries to the REPL log."
  (let ((out (alist-get lpy-nrepl--stdout-field msg))
        (err (alist-get lpy-nrepl--stderr-field msg)))
    (when (and (stringp out) (not (string-empty-p out)))
      (lpy-nrepl--log "stdout-chunk" out))
    (when (and (stringp err) (not (string-empty-p err)))
      (lpy-nrepl--log "stderr-chunk" err))))

(defun lpy-nrepl--finish-request (id)
  "Forget request ID as in flight; its final reply has arrived."
  (when (member id lpy-nrepl--in-flight-evals)
    (setq lpy-nrepl--in-flight-evals (delete id lpy-nrepl--in-flight-evals))
    (force-mode-line-update t)))

(defun lpy-nrepl--handle-response (msg)
  "Dispatch a parsed JSON response MSG.
An interim `lpy-nrepl--out-op' message is logged as output and keeps its
request open, whatever the request; any other message is the request's
final reply and is passed to its callback, which is then forgotten."
  (let* ((id (alist-get 'id msg))
         (entry (and id (assoc id lpy-nrepl--pending-callbacks))))
    (if (equal (alist-get 'op msg) lpy-nrepl--out-op)
        (lpy-nrepl--log-output msg)
      (let ((running (car (last lpy-nrepl--in-flight-evals))))
        ;; A reply to a request sent after the running eval: the server
        ;; answers while evaluating (an old one answers in order).
        (when (and running (not (equal id running))
                   (> (lpy-nrepl--request-number id)
                      (lpy-nrepl--request-number running)))
          (setq lpy-nrepl--answers-during-eval t)))
      (lpy-nrepl--finish-request id)
      (cond
       (entry
        (setq lpy-nrepl--pending-callbacks
              (delq entry lpy-nrepl--pending-callbacks))
        (funcall (cdr entry) msg))
       ((member id lpy-nrepl--ignored-ids)
        (setq lpy-nrepl--ignored-ids (delete id lpy-nrepl--ignored-ids)))
       ;; No pending callback -- log it for async/unsolicited messages
       (t (lpy-nrepl--log "<=" (json-serialize msg)))))))

(defun lpy-nrepl--supersede (op id)
  "Record ID as the newest request of OP, dropping the previous one's callback.
Only `lpy-nrepl--superseding-ops' are affected.  Return what
`lpy-nrepl--unsupersede' needs to undo this, or nil if nothing changed."
  (when (member op lpy-nrepl--superseding-ops)
    (let* ((cell (assoc op lpy-nrepl--latest-by-op))
           (old (cdr cell))
           (entry (and old (assoc old lpy-nrepl--pending-callbacks))))
      (when entry
        (setq lpy-nrepl--pending-callbacks
              (delq entry lpy-nrepl--pending-callbacks))
        (push old lpy-nrepl--ignored-ids))
      (if cell
          (setcdr cell id)
        (push (cons op id) lpy-nrepl--latest-by-op))
      (list op id old entry))))

(defun lpy-nrepl--unsupersede (undo)
  "Undo the `lpy-nrepl--supersede' that returned UNDO, for an unsent request.
Only while the request is still OP's newest: if a newer one superseded
it meanwhile, the previous request stays superseded by that one."
  (pcase-let ((`(,op ,id ,old ,entry) undo))
    ;; A newer request may have superseded this unsent one: its id would
    ;; otherwise wait in the ignored list for a reply that never comes.
    (setq lpy-nrepl--ignored-ids (delete id lpy-nrepl--ignored-ids))
    (let ((cell (assoc op lpy-nrepl--latest-by-op)))
      (when (and cell (equal (cdr cell) id))
        ;; OLD is the newest request of OP sent, answered or not; one
        ;; already answered has no callback for a later supersede to drop.
        (if old
            (setcdr cell old)
          (setq lpy-nrepl--latest-by-op (delq cell lpy-nrepl--latest-by-op)))
        ;; OLD's callback comes back only while OLD is still ignored: its
        ;; final reply may have been read, and dropped, during the send,
        ;; and a restored callback would then wait for a reply that is gone.
        (when (and entry (member old lpy-nrepl--ignored-ids))
          (push entry lpy-nrepl--pending-callbacks)
          (setq lpy-nrepl--ignored-ids (delete old lpy-nrepl--ignored-ids)))))))

(defun lpy-nrepl--send (msg &optional callback)
  "Send MSG (an alist) as newline-delimited JSON to the nREPL server.
If CALLBACK is non-nil, register it to be called with the final reply.
Eval requests (`lpy-nrepl--eval-ops') ask for streamed output and are
tracked in flight until their final reply.  Return the request id."
  (unless (lpy-nrepl-connected-p)
    (user-error "Not connected to nREPL server"))
  (let* ((id (lpy-nrepl--next-id))
         (op (alist-get 'op msg))
         (eval-p (member op lpy-nrepl--eval-ops))
         (json-str (json-serialize
                    `((id . ,id) ,@msg
                      ,@(and eval-p (list (cons lpy-nrepl--stream-field t))))))
         (code (alist-get 'code msg))
         undo)
    ;; Registered only once serialized and logged: a message that cannot
    ;; be encoded or logged must not leave an eval in flight, a callback
    ;; waiting forever or its op superseded.  The input log, registration
    ;; and superseding all happen before the send, because a blocking send
    ;; runs timers and process filters: a nested newer request must find
    ;; this one already superseded (not be superseded by it afterwards),
    ;; and a reply read meanwhile must be logged after its input.  A send
    ;; that fails undoes the registration, logs it and re-signals.
    (when code
      (lpy-nrepl--log "input" code))
    (when eval-p
      (push id lpy-nrepl--in-flight-evals)
      (force-mode-line-update t))
    (when callback
      (push (cons id callback) lpy-nrepl--pending-callbacks))
    (setq undo (lpy-nrepl--supersede op id))
    (condition-case err
        (process-send-string lpy-nrepl--connection (concat json-str "\n"))
      (error
       (setq lpy-nrepl--pending-callbacks
             (assoc-delete-all id lpy-nrepl--pending-callbacks))
       (lpy-nrepl--finish-request id)
       (when undo
         (lpy-nrepl--unsupersede undo))
       (lpy-nrepl--log "error"
                       (format ";; send failed: %s" (error-message-string err)))
       (signal (car err) (cdr err)))
      ;; A quit lands while Emacs waits with the unwritten rest of the line
      ;; queued, which the next send writes: the request may still run, and
      ;; the stream may hold half a line, so the connection is dropped.
      (quit
       ;; A second C-g during the log or delete must not leave a
       ;; half-dropped connection current.
       (let ((inhibit-quit t))
         (lpy-nrepl--connection-lost
          "send interrupted; the request may be half-sent"
          nil lpy-nrepl--reconnect-hint))
       (signal (car err) (cdr err))))
    id))

(defun lpy-nrepl--may-wait-p ()
  "Return non-nil when a synchronous request may wait for its reply.
Not while an eval is in flight on a server that has not shown it answers
during one: an old server answers a connection's requests in order, so
nothing comes back until the eval ends, and the wait would only stall
the keystroke (from corfu's timer, where \\[keyboard-quit] cannot break it)."
  (or (null lpy-nrepl--in-flight-evals)
      lpy-nrepl--answers-during-eval))

(defun lpy-nrepl--await (done)
  "Wait, for a bounded time, until calling DONE gives non-nil.
Waits at most `lpy-nrepl-request-timeout' seconds, and not at all when
`lpy-nrepl--may-wait-p' says no reply can come.  \\[keyboard-quit] ends
the wait even under `inhibit-quit' (a timer), and is then re-signalled
outside it."
  (when (lpy-nrepl--may-wait-p)
    (let ((deadline (+ (float-time) lpy-nrepl-request-timeout)))
      (with-local-quit
        (while (and (not (funcall done))
                    (lpy-nrepl-connected-p)
                    (< (float-time) deadline))
          (accept-process-output lpy-nrepl--connection 0.02))))))

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
Pending callbacks, in-flight evals and the rest of the request state
belong to the old connection and are dropped.  NEW is
installed before the old process is deleted: `delete-process' runs its
sentinel synchronously, and the sentinel only clears state for the current
connection, so it must no longer see the old process as current."
  (let ((old lpy-nrepl--connection))
    (setq lpy-nrepl--connection new)
    (setq lpy-nrepl--pending-callbacks nil
          lpy-nrepl--in-flight-evals nil
          lpy-nrepl--ignored-ids nil
          lpy-nrepl--latest-by-op nil
          lpy-nrepl--answers-during-eval nil)
    (force-mode-line-update t)
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

(defun lpy-nrepl--server-filter (proc output)
  "Insert OUTPUT of server PROC in its buffer; connect once it names its port.
Only PROC's own output is searched for the port line, accumulated in its
`lpy-nrepl-output' property until the line appears."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char (point-max))
          (insert output))
        ;; Trimmed here, never from `seen' below: the port line is searched
        ;; in the process property, not in the buffer.
        (lpy-nrepl--trim-buffer lpy-nrepl-server-log-max-size))))
  (unless (process-get proc 'lpy-nrepl-port)
    (let ((seen (concat (process-get proc 'lpy-nrepl-output) output)))
      (process-put proc 'lpy-nrepl-output seen)
      (when (string-match lpy-nrepl--server-started-regexp seen)
        (let ((port (string-to-number (match-string 1 seen))))
          (process-put proc 'lpy-nrepl-port port)
          (process-put proc 'lpy-nrepl-output nil)
          (lpy-nrepl--cancel-start-timer proc)
          (when (eq proc lpy-nrepl--server-process)
            (condition-case err
                (progn
                  (lpy-nrepl-connect port)
                  (message "lpy-nrepl: server started on port %d" port))
              (error
               (message "lpy-nrepl: server started on port %d but connecting failed: %s"
                        port (error-message-string err))))))))))

(defun lpy-nrepl--cancel-start-timer (proc)
  "Cancel the startup timeout timer of server PROC, if any."
  (let ((timer (process-get proc 'lpy-nrepl-timer)))
    (when (timerp timer)
      (cancel-timer timer))
    (process-put proc 'lpy-nrepl-timer nil)))

(defun lpy-nrepl--start-timed-out (proc)
  "Give up on server PROC, which never printed its port line."
  (process-put proc 'lpy-nrepl-timer nil)
  (unless (process-get proc 'lpy-nrepl-port)
    ;; Not current first: its sentinel must not report a server death.
    (when (eq proc lpy-nrepl--server-process)
      (setq lpy-nrepl--server-process nil))
    (delete-process proc)
    (message "lpy-nrepl: timed out waiting for the nREPL server to start; see %s"
             lpy-nrepl--server-buffer-name)))

(defun lpy-nrepl--server-sentinel (proc event)
  "Handle the exit of server PROC, described by EVENT.
Only the current server (`lpy-nrepl--server-process') clears state: a
replaced one still reports its own exit."
  ;; Liveness, not the event text: the texts vary ("killed: 9",
  ;; "terminated", "exited abnormally with code 1", "finished"), and a
  ;; missed one left `lpy-nrepl--server-process' naming a dead server.
  (unless (process-live-p proc)
    (lpy-nrepl--cancel-start-timer proc)
    (when (eq proc lpy-nrepl--server-process)
      (setq lpy-nrepl--server-process nil)
      (let ((port (process-get proc 'lpy-nrepl-port))
            (what (string-trim event)))
        (if port
            (lpy-nrepl--connection-lost (format "nREPL server died (%s)" what)
                                        port lpy-nrepl--restart-hint)
          (message "lpy-nrepl: nREPL server exited before it started (%s); see %s"
                   what lpy-nrepl--server-buffer-name))))))

;;;###autoload
(defun lpy-nrepl-start ()
  "Start `lpy --nrepl' as a subprocess and connect once it names its port.
Return at once; the connection is made when the server prints its port
line, or the start is abandoned after `lpy-nrepl-start-timeout' seconds.
The server's output is captured in `*lpy-nrepl-server*'.  Return the
server process."
  (interactive)
  (when (process-live-p lpy-nrepl--server-process)
    (user-error "nREPL server already running (pid %d)"
                (process-id lpy-nrepl--server-process)))
  ;; The buffer is emptied and the port read only from the new process's
  ;; own output: a dead server's "started on ...:PORT" line left in the
  ;; buffer would otherwise be found first, and the client would connect
  ;; to that dead port.
  (let ((buf (get-buffer-create lpy-nrepl--server-buffer-name)))
    (with-current-buffer buf
      ;; Undo would keep every line the cap trims (see `lpy-nrepl-repl-mode').
      (buffer-disable-undo)
      (let ((inhibit-read-only t))
        (erase-buffer)))
    (let ((proc (make-process
                 :name "lpy-nrepl-server"
                 :buffer buf
                 :command (list lpy-nrepl--interpreter "--nrepl")
                 :noquery t
                 :connection-type 'pipe
                 :filter #'lpy-nrepl--server-filter
                 :sentinel #'lpy-nrepl--server-sentinel)))
      (process-put proc 'lpy-nrepl-output "")
      ;; A timer, not a blocking wait: Emacs stays usable while the server
      ;; imports, and a server that never starts is reported and killed.
      (process-put proc 'lpy-nrepl-timer
                   (run-at-time lpy-nrepl-start-timeout nil
                                #'lpy-nrepl--start-timed-out proc))
      (setq lpy-nrepl--server-process proc)
      (message "lpy-nrepl: starting server...")
      proc)))

;;;###autoload
(defun lpy-nrepl-restart ()
  "Kill the Emacs-started nREPL server, drop the connection, start afresh.
Recovers from a hung server, or one too old to interrupt an eval."
  (interactive)
  (let ((old lpy-nrepl--server-process))
    ;; Not current first, so its sentinel reports no death.
    (setq lpy-nrepl--server-process nil)
    (when old
      (lpy-nrepl--cancel-start-timer old)
      (delete-process old)))
  (when lpy-nrepl--connection
    (let ((pending (length lpy-nrepl--pending-callbacks)))
      (lpy-nrepl--swap-connection nil)
      (lpy-nrepl--log "error"
                      (format ";; nREPL restarted — %d pending request(s) dropped"
                              pending))))
  (lpy-nrepl-start))

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
        ;; A streaming server sent the output already (`lpy-nrepl--out-op')
        ;; and sends "" here; an old one sends all of it here.
        (stdout (alist-get lpy-nrepl--stdout-field response))
        (stderr (alist-get lpy-nrepl--stderr-field response))
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
     `((op . ,lpy-nrepl--op-eval) (code . ,trimmed) ,@(lpy-nrepl--ns-fields))
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
     `((op . ,lpy-nrepl--op-eval) (code . ,(string-trim code))
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
   `((op . ,lpy-nrepl--op-load-file) (path . ,(buffer-file-name)))
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

(defun lpy-nrepl--interrupt-reply (target response)
  "Report RESPONSE, the server's reply to an interrupt of eval TARGET."
  (let ((err (alist-get 'error response)))
    (cond
     ((equal (alist-get 'op response) lpy-nrepl--interrupt-result-op)
      (message "lpy-nrepl: interrupted eval %s" target))
     ((and (stringp err) (string-match-p lpy-nrepl--unknown-op-error err))
      (lpy-nrepl--log "error" ";; this nREPL server cannot interrupt an eval")
      (message "lpy-nrepl: the server is too old to interrupt; upgrade lispython (nREPL streaming and interrupt support) or M-x lpy-nrepl-restart"))
     (t
      (message "lpy-nrepl: interrupt of eval %s failed: %s"
               target (or err (json-serialize response)))))))

(defun lpy-nrepl--interrupt-unanswered (request target)
  "Report that interrupt REQUEST of eval TARGET is still unanswered.
A server that answers requests only in order (one without interrupt
support) cannot reply while the eval it should interrupt runs."
  (when (assoc request lpy-nrepl--pending-callbacks)
    (message "lpy-nrepl: the server did not answer the interrupt of eval %s; it may be too old to interrupt (upgrade lispython), or M-x lpy-nrepl-restart"
             target)))

;;;###autoload
(defun lpy-nrepl-interrupt (&optional all)
  "Interrupt the running nREPL evaluation.
The server runs a connection's evals one at a time in request order, so
the oldest unfinished eval is the running one.  With prefix argument
ALL, interrupt every unfinished eval, the queued ones too.  The eval
then ends with a KeyboardInterrupt error.  Needs lispython with nREPL
streaming and interrupt support; otherwise use `lpy-nrepl-restart'."
  (interactive "P")
  (if (or (null lpy-nrepl--in-flight-evals) (not (lpy-nrepl-connected-p)))
      (message "No evaluation running")
    (dolist (target (if all
                        (reverse lpy-nrepl--in-flight-evals)
                      (last lpy-nrepl--in-flight-evals)))
      (lpy-nrepl--log "error" (format ";; interrupting eval %s" target))
      (let ((request
             (lpy-nrepl--send
              `((op . ,lpy-nrepl--interrupt-op)
                (,lpy-nrepl--interrupt-id-field . ,target))
              (lambda (response)
                (lpy-nrepl--interrupt-reply target response)))))
        (run-at-time lpy-nrepl-interrupt-timeout nil
                     #'lpy-nrepl--interrupt-unanswered request target)))))

(defun lpy-nrepl--eval-status ()
  "Return the mode-line text showing that an eval is running, or \"\"."
  (if lpy-nrepl--in-flight-evals " eval…" ""))

;;;###autoload
(defun lpy-nrepl-macroexpand ()
  "Macroexpand the form at point via nREPL.
The expansion is displayed in a `*lpy-macroexpand*' buffer and is
resolved in the buffer file's scope (the session scope without a file)."
  (interactive)
  (-when-let (code (or (lpy--current-form-string)
                       (lpy--last-sexp-string)))
    (lpy-nrepl--send
     `((op . ,lpy-nrepl--op-macroexpand) (code . ,(string-trim code))
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
    (type-parameter " type parameter" type-parameter))
  "Each completion kind as (KIND ANNOTATION COMPANY-KIND).
The one vocabulary of candidate annotations: nREPL and LSP candidates
resolve their kind number through `lpy--lsp-kinds' and then through
this table.
Annotations are the full lowercase kind word, as JupyterLab, elpy and
nvim-cmp show it.")

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

(defun lpy--item-annotation (description kind-number)
  "Return the list annotation of a completion item.
DESCRIPTION (`labelDetails.description') when a non-empty string, else
the word of KIND-NUMBER, an LSP CompletionItemKind.  The one annotation
rule of eglot and nREPL candidates, so one kind displays identically."
  (if (and (stringp description) (not (string-empty-p description)))
      (concat " " description)
    (lpy--kind-annotation (gethash kind-number lpy--lsp-kinds))))

(defun lpy--item-company-kind (kind-number)
  "Return the `:company-kind' of KIND-NUMBER, an LSP CompletionItemKind."
  (nth 2 (assq (gethash kind-number lpy--lsp-kinds) lpy--completion-kinds)))

;;; Completion word bounds

;; The word completed at point is lispython's: its word-boundary spec
;; (`python -m lispy.word_boundary', copied to test/word-boundary.json)
;; publishes the rule as data and the cases a client must reproduce.  This
;; client holds the rule's fields verbatim and builds every regexp from
;; them, so a spec change is a change of data here, never a hand-edited
;; charset.  `lpy--word-span' follows the spec's steps, as its reference
;; interpreter (`spec_word_span' in lispython's tests/test_word_boundary.py)
;; does: the input and context steps are `lpy--word-context' (the text is
;; read from its start, so a string or comment opened lines above holds the
;; caret), the body step `lpy--word-body', each mode's token patterns
;; `lpy--word-token-kinds', and the run, word token, debug = and peel steps
;; `lpy--word-span'.  In a string's or comment's text there is no word.
;; Nothing here uses Emacs's `\w', syntax classes or a syntax table: none is
;; Python's definition, and the buffer may be in another mode.  The `\w'/Nd
;; classes come from the running Emacs's Unicode tables, as the spec's own
;; come from the running Python's (the spec names no Unicode version), so
;; the two agree where their Unicode versions agree.
;; Cost: every call copies the text from the buffer's start (widened)
;; through the caret's line end into one scan buffer and normalizes its
;; digits, linear in that text.  The lex then
;; resumes from `lpy--word-scan-cache', the last top-level state an earlier
;; scan recorded before the caret's line, while the text through that
;; state's line end is unchanged; an edit above it re-lexes from the start.

(defconst lpy--word-boundary-rule
  '((stopChars . ("\u0009" "\u000a" "\u000b" "\u000c" "\u000d" "\u001c"
                  "\u001d" "\u001e" "\u001f" " " "\"" ")" "," ";" "[" "]"
                  "{" "}" "\u0085" "\u00a0" "\u1680" "\u2000" "\u2001"
                  "\u2002" "\u2003" "\u2004" "\u2005" "\u2006" "\u2007"
                  "\u2008" "\u2009" "\u200a" "\u2028" "\u2029" "\u202f"
                  "\u205f" "\u3000"))
    (formEndChars . ("\u0009" "\u000a" "\u000b" "\u000c" "\u000d" "\u001c"
                     "\u001d" "\u001e" "\u001f" " " ")" "," ";" "]" "}"
                     "\u0085" "\u00a0" "\u1680" "\u2000" "\u2001" "\u2002"
                     "\u2003" "\u2004" "\u2005" "\u2006" "\u2007" "\u2008"
                     "\u2009" "\u200a" "\u2028" "\u2029" "\u202f" "\u205f"
                     "\u3000"))
    (numberEndChars . ("\u0009" "\u000a" "\u000b" "\u000c" "\u000d" "\u001c"
                       "\u001d" "\u001e" "\u001f" " " ")" "," ";" "]" "{"
                       "}" "\u0085" "\u00a0" "\u1680" "\u2000" "\u2001"
                       "\u2002" "\u2003" "\u2004" "\u2005" "\u2006" "\u2007"
                       "\u2008" "\u2009" "\u200a" "\u2028" "\u2029" "\u202f"
                       "\u205f" "\u3000"))
    (wrappers . ("" "*" "^" "**"))
    (openings . ("(" "[" "{"))
    (closings . (")" "]" "}"))
    (metaChars . ("'" "`" "~" "~@"))
    (tokenPrefixes . ("**" "*" "^" ":"))
    (signs . ("+" "-"))
    (signPeelFollowers . ("_" "."))
    (neverPeeled . ("%=" "&=" "**=" "*=" "+=" "-=" "//=" "/=" "<<=" ">>="
                    "@=" "^=" "|="))
    (stringPrefix . "\\w*")
    (stringOpenings . ("'''" "\"\"\"" "\""))
    (escapedStringOpenings . ("\\\"\\\"\\\"" "\\\""))
    (rawLetter . "r")
    (kindLetters . (("t" "t-string") ("f" "f-string")))
    (plainBodyStops . ("\"" "'" "\\"))
    (interpolatedBodyStops . ("\"" "'" "\\" "{" "}"))
    (fieldBraces . ("{" "}"))
    (backslash . "\\")
    (namedEscape . ("N{" "}"))
    (quoteChars . ("\"" "'"))
    (escapedQuote . "\\\"")
    (bareQuote . "\"")
    (lineBreakChars . ("\u000a" "\u000d"))
    (whitespace . ("\u0009" "\u000a" "\u000b" "\u000c" "\u000d" "\u001c"
                   "\u001d" "\u001e" "\u001f" " " "\u0085" "\u00a0" "\u1680"
                   "\u2000" "\u2001" "\u2002" "\u2003" "\u2004" "\u2005"
                   "\u2006" "\u2007" "\u2008" "\u2009" "\u200a" "\u2028"
                   "\u2029" "\u202f" "\u205f" "\u3000"))
    (fieldDelimiter . ":|!(?!=)")
    (conversionEnds . (":" "}"))
    (fieldContinuation . "\\\\(?:\\r\\n|\\r|\\n|\\Z)")
    (fieldSymbolBackslash . "\\\\(?![\"\\r\\n]|\\Z)")
    (debugBlockers . ("=" "!" "<" ">" ":" "+" "-" "*" "/" "%" "&" "|" "^"
                      "@" "~"))
    (danglingQuotes . ("'" "`" "~@"))
    (numberToken . "[+-]*(?:(?:(?:\\d+(?:_\\d+)*\\.\\d+(?:_\\d+)*[eE][\\-\\+]\
?\\d+(?:_\\d+)*|\\d+(?:_\\d+)*\\.\\d*[eE][\\-\\+]?\\d+(?:_\\d+)*|\\d*\\.\\d+(\
?:_\\d+)*[eE][\\-\\+]?\\d+(?:_\\d+)*|\\d[\\dA-Za-z]*(?:_[\\dA-Za-z]+)*[eE][\\\
-\\+]?\\d+(?:_\\d+)*|\\d+(?:_\\d+)*\\.\\d+(?:_\\d+)*|\\d+(?:_\\d+)*\\.\\d*|\\\
d*\\.\\d+(?:_\\d+)*|\\d[\\dA-Za-z]*(?:_[\\dA-Za-z]+)*)[+-])?(?:\\d+(?:_\\d+)*\
\\.\\d+(?:_\\d+)*[eE][\\-\\+]?\\d+(?:_\\d+)*|\\d+(?:_\\d+)*\\.\\d*[eE][\\-\\+\
]?\\d+(?:_\\d+)*|\\d*\\.\\d+(?:_\\d+)*[eE][\\-\\+]?\\d+(?:_\\d+)*|\\d[\\dA-Za\
-z]*(?:_[\\dA-Za-z]+)*[eE][\\-\\+]?\\d+(?:_\\d+)*|\\d+(?:_\\d+)*\\.\\d+(?:_\\\
d+)*|\\d+(?:_\\d+)*\\.\\d*|\\d*\\.\\d+(?:_\\d+)*|\\d[\\dA-Za-z]*(?:_[\\dA-Za-\
z]+)*)[jJ]|\\d+(?:_\\d+)*\\.\\d+(?:_\\d+)*[eE][\\-\\+]?\\d+(?:_\\d+)*|\\d+(?:\
_\\d+)*\\.\\d*[eE][\\-\\+]?\\d+(?:_\\d+)*|\\d*\\.\\d+(?:_\\d+)*[eE][\\-\\+]?\
\\d+(?:_\\d+)*|\\d[\\dA-Za-z]*(?:_[\\dA-Za-z]+)*[eE][\\-\\+]?\\d+(?:_\\d+)*|\
\\d+(?:_\\d+)*\\.\\d+(?:_\\d+)*|\\d+(?:_\\d+)*\\.\\d*|\\d*\\.\\d+(?:_\\d+)*|\
\\d[\\dA-Za-z]*(?:_[\\dA-Za-z]+)*)")
    (signedComplex . "[+-]*(?:([0-9](?:_?[0-9])*[jJ]|(([0-9](?:_?[0-9])*\\.(?\
:[0-9](?:_?[0-9])*)?|\\.[0-9](?:_?[0-9])*)([eE][-+]?[0-9](?:_?[0-9])*)?|[0-9]\
(?:_?[0-9])*[eE][-+]?[0-9](?:_?[0-9])*)[jJ])|(([0-9](?:_?[0-9])*\\.(?:[0-9](?\
:_?[0-9])*)?|\\.[0-9](?:_?[0-9])*)([eE][-+]?[0-9](?:_?[0-9])*)?|[0-9](?:_?[0-\
9])*[eE][-+]?[0-9](?:_?[0-9])*)|0[xX](?:_?[0-9a-fA-F])+|0[bB](?:_?[01])+|0[oO\
](?:_?[0-7])+|[0-9](?:_?[0-9])*)[+-](?:([0-9](?:_?[0-9])*[jJ]|(([0-9](?:_?[0-\
9])*\\.(?:[0-9](?:_?[0-9])*)?|\\.[0-9](?:_?[0-9])*)([eE][-+]?[0-9](?:_?[0-9])\
*)?|[0-9](?:_?[0-9])*[eE][-+]?[0-9](?:_?[0-9])*)[jJ])|(([0-9](?:_?[0-9])*\\.(\
?:[0-9](?:_?[0-9])*)?|\\.[0-9](?:_?[0-9])*)([eE][-+]?[0-9](?:_?[0-9])*)?|[0-9\
](?:_?[0-9])*[eE][-+]?[0-9](?:_?[0-9])*)|0[xX](?:_?[0-9a-fA-F])+|0[bB](?:_?[0\
1])+|0[oO](?:_?[0-7])+|[0-9](?:_?[0-9])*)"))
  "The `rule' of lispython's word-boundary spec, field for field.
Copied verbatim from `python -m lispy.word_boundary' (the prose `steps'
excepted); the tests pin it to test/word-boundary.json.  Character fields
are lists of strings; `numberToken', `signedComplex', `stringPrefix',
`fieldDelimiter', `fieldContinuation' and `fieldSymbolBackslash' are
Python `re' patterns, translated by `lpy--word-python-regexp'.")

(defun lpy--word-rule (field)
  "Return FIELD of `lpy--word-boundary-rule'."
  (or (alist-get field lpy--word-boundary-rule)
      (error "No word-boundary rule field `%s'" field)))

(defun lpy--word-rule-char (field &optional value)
  "Return FIELD of the rule, a one-character string, as a character.
VALUE, when given, is the string to read instead of the field's."
  (let ((entry (or value (lpy--word-rule field))))
    (if (= (length entry) 1)
        (aref entry 0)
      (error "Rule field `%s' holds %S, not one character" field entry))))

(defun lpy--word-rule-chars (field)
  "Return FIELD of the rule, a list of one-character strings, as characters."
  (mapcar (lambda (entry) (lpy--word-rule-char field entry))
          (lpy--word-rule field)))

(defun lpy--word-char-class (chars &optional negated)
  "Return a regexp matching one of CHARS, or one not in them when NEGATED.
Built by rx, so `]', `^' and `-' among CHARS need no care.  CHARS may hold
\(FROM . TO) ranges."
  (rx-to-string (if negated `(not (any ,@chars)) `(any ,@chars)) t))

(defun lpy--word-longest-first (entries)
  "Return ENTRIES longest first, equal lengths in their order.
The spec's `one_of' order: the first alternative that matches wins, so
the longest entry is tried first (`~@' before `~')."
  (sort (copy-sequence entries) (lambda (a b) (> (length a) (length b)))))

(defun lpy--word-one-of (entries)
  "Return a regexp group matching one of the literal ENTRIES, longest first."
  (concat "\\(?:"
          (mapconcat #'regexp-quote (lpy--word-longest-first entries) "\\|")
          "\\)"))

(defconst lpy--word-digit ?٠
  "The one non-ASCII decimal digit the text's digits are normalized to.
Python's `\\d' is every Unicode Nd character, but a class of all Nd ranges
at each of `numberToken's hundred or so `\\d's makes the regexp too big
for Emacs.  So `lpy--word-normalize-digits' maps each non-ASCII Nd
character to this one (ARABIC-INDIC DIGIT ZERO) and `\\d' becomes [0-9]
plus it.  Every predicate the rule applies survives: the ASCII-literal
classes (`[0-7]', `[0-9a-fA-F]'...) still reject it, no stop, form-end
or body-stop character is a digit, `\\w' and `str.isalnum' hold every Nd
character, and positions are kept, the mapping being character for
character.")

(defun lpy--word-category-ranges (pred)
  "Return the (FROM . TO) ranges of characters whose category satisfies PRED.
PRED is called with a general category symbol."
  (let (ranges)
    ;; Ranges of `unicode-category-table' share one category, so the first
    ;; character's names the range.  The cons key is reused: copy it.
    (map-char-table
     (lambda (key _value)
       (let ((from (if (consp key) (car key) key))
             (to (if (consp key) (cdr key) key)))
         (when (funcall pred (get-char-code-property from 'general-category))
           (if (and ranges (= (cdar ranges) (1- from)))
               (setcdr (car ranges) to)
             (push (cons from to) ranges)))))
     unicode-category-table)
    (nreverse ranges)))

(defun lpy--word-alnum-category-p (category)
  "Return non-nil when CATEGORY is a letter's or number's (L* or N*)."
  (and category (memq (aref (symbol-name category) 0) '(?L ?N))))

(defun lpy--word-alnum-p (char)
  "Return non-nil when CHAR is alphanumeric as Python's `str.isalnum'."
  (lpy--word-alnum-category-p (get-char-code-property char 'general-category)))

(defconst lpy--word-alnum-ranges
  (lpy--word-category-ranges #'lpy--word-alnum-category-p)
  "The (FROM . TO) ranges of `lpy--word-alnum-p' characters.")

(defconst lpy--word-other-digits-regexp
  (lpy--word-char-class
   (cl-remove-if (lambda (range) (< (car range) 128))
                 (lpy--word-category-ranges (lambda (c) (eq c 'Nd)))))
  "Matches a non-ASCII decimal digit (`lpy--word-digit').")

(defconst lpy--word-python-classes
  `((?d . ((?0 . ?9) ,lpy--word-digit))
    (?w . (?_ ,@lpy--word-alnum-ranges)))
  "The characters of Python's `\\d' and `\\w' over normalized text.
`\\d' relies on `lpy--word-normalize-digits'; `\\w' is `str.isalnum' or
`_'.")

(defconst lpy--word-python-control-escapes '((?r . ?\r) (?n . ?\n))
  "The control-character escapes a Python pattern may use.")

(defconst lpy--word-python-literals ".-+*?()[]{}|^$\\/"
  "The characters a Python pattern may escape to mean themselves.")

(defun lpy--word-python-regexp (pattern &optional mark)
  "Translate PATTERN, a Python `re' pattern, to an Emacs regexp.
Only the subset the spec publishes is understood: groups (all made shy),
`|', the greedy `*', `+' and `?', classes with ranges, `\\d', `\\w',
`\\r', `\\n', `\\Z', escaped punctuation, literal characters, and a
lookahead ending a top-level alternative whose body is single characters,
classes and `\\Z' joined by `|'.  Anything else signals an error, so a
spec that starts using more syntax fails at load instead of being
silently mistranslated.

An Emacs regexp has no lookahead, so one is matched as the text it tests:
`(?=X)' as X, `(?!X)' as a character X does not hold, or the end unless X
holds `\\Z'.  The match may then run past Python's, which is right for a
test at a position or at the very end of a pattern.  With MARK an empty
group 1 precedes each lookahead: where it matched, its start is the end
of Python's match (`lpy--word-at')."
  (let ((i 0) (n (length pattern)) (out nil) (quantifiable nil) (depth 0)
        ;; Right after a lookahead, which must end its alternative.
        (ended nil))
    (cl-labels
        ((fail (what) (error "Unsupported Python regexp %s at %d in %S"
                             what i pattern))
         (escaped (char)
           ;; (class . CHARS), a character, `end' for \Z, or nil.
           (cond ((assq char lpy--word-python-classes)
                  (cons 'class (cdr (assq char lpy--word-python-classes))))
                 ((assq char lpy--word-python-control-escapes)
                  (cdr (assq char lpy--word-python-control-escapes)))
                 ((eq char ?Z) 'end)
                 ((cl-position char lpy--word-python-literals) char)))
         (parse-class (j)
           ;; J follows a `['.  Return (CHARS NEGATED . NEXT).
           (let ((negated nil) (chars nil))
             (when (and (< j n) (eq (aref pattern j) ?^))
               (setq negated t j (1+ j)))
             (when (and (< j n) (eq (aref pattern j) ?\]))
               (fail "class starting with ]"))
             (while (and (< j n) (not (eq (aref pattern j) ?\])))
               (let* ((d (aref pattern j))
                      (item (cond ((eq d ?\\)
                                   (prog1 (and (< (1+ j) n)
                                               (escaped (aref pattern (1+ j))))
                                     (setq j (1+ j))))
                                  ((eq d ?\[) nil)
                                  (t d))))
                 (setq j (1+ j))
                 (cond
                  ((or (null item) (eq item 'end)) (fail "class item"))
                  ((consp item)
                   (when (and (< j n) (eq (aref pattern j) ?-)
                              (not (and (< (1+ j) n)
                                        (eq (aref pattern (1+ j)) ?\]))))
                     (fail "range from a class escape"))
                   (setq chars (append (cdr item) chars)))
                  ;; A range: `-' between two characters.
                  ((and (< (1+ j) n) (eq (aref pattern j) ?-)
                        (not (eq (aref pattern (1+ j)) ?\])))
                   (let ((to (aref pattern (1+ j))))
                     (when (memq to '(?\\ ?\[)) (fail "range end"))
                     (push (cons item to) chars)
                     (setq j (+ j 2))))
                  (t (push item chars)))))
             (unless (< j n) (fail "unterminated class"))
             (cons chars (cons negated (1+ j)))))
         (parse-lookahead (j)
           ;; J follows `(?=' or `(?!'.  Return (CHARS END-P . NEXT).
           (let ((chars nil) (end-p nil) (expect-item t))
             (while (not (and (< j n) (eq (aref pattern j) ?\))
                              (not expect-item)))
               (unless (< j n) (fail "unterminated lookahead"))
               (let ((d (aref pattern j)))
                 (cond
                  ((not expect-item)
                   (unless (eq d ?|) (fail "lookahead item"))
                   (setq j (1+ j) expect-item t))
                  ((eq d ?\\)
                   (let ((e (and (< (1+ j) n) (escaped (aref pattern (1+ j))))))
                     (cond ((null e) (fail "lookahead escape"))
                           ((eq e 'end) (setq end-p t))
                           ((consp e) (setq chars (append (cdr e) chars)))
                           (t (push e chars)))
                     (setq j (+ j 2) expect-item nil)))
                  ((eq d ?\[)
                   (pcase-let ((`(,class-chars ,negated . ,next)
                                (parse-class (1+ j))))
                     (when negated (fail "negated class in a lookahead"))
                     (setq chars (append class-chars chars)
                           j next expect-item nil)))
                  ((memq d '(?. ?^ ?$ ?* ?+ ?? ?{ ?} ?\( ?\) ?|))
                   (fail "lookahead item"))
                  (t (push d chars)
                     (setq j (1+ j) expect-item nil)))))
             (cons chars (cons end-p (1+ j))))))
      (while (< i n)
        (let ((c (aref pattern i)))
          (when (and ended (not (eq c ?|)))
            (fail "lookahead not ending its alternative"))
          (cond
           ((eq c ?\\)
            (let ((e (and (< (1+ i) n) (escaped (aref pattern (1+ i))))))
              (cond ((null e) (fail "escape"))
                    ((eq e 'end) (push "\\'" out))
                    ((consp e) (push (lpy--word-char-class (cdr e)) out))
                    (t (push (regexp-quote (string e)) out)))
              (setq quantifiable (not (eq e 'end)) i (+ i 2))))
           ((eq c ?\[)
            (pcase-let ((`(,chars ,negated . ,next) (parse-class (1+ i))))
              (push (lpy--word-char-class chars negated) out)
              (setq i next quantifiable t)))
           ((and (eq c ?\() (< (+ i 2) n) (eq (aref pattern (1+ i)) ??)
                 (memq (aref pattern (+ i 2)) '(?= ?!)))
            (unless (zerop depth) (fail "lookahead inside a group"))
            (pcase-let ((`(,chars ,end-p . ,next) (parse-lookahead (+ i 3)))
                        (negative (eq (aref pattern (+ i 2)) ?!)))
              (when mark (push "\\(?1:\\)" out))
              (push (concat "\\(?:"
                            (cond (negative
                                   (concat (lpy--word-char-class chars t)
                                           (if end-p "" "\\|\\'")))
                                  ((null chars) "\\'")
                                  (t (concat (lpy--word-char-class chars)
                                             (if end-p "\\|\\'" ""))))
                            "\\)")
                    out)
              (setq i next quantifiable nil ended t)))
           ((eq c ?\()
            (cond ((not (and (< (1+ i) n) (eq (aref pattern (1+ i)) ??)))
                   (setq i (1+ i)))
                  ((and (< (+ i 2) n) (eq (aref pattern (+ i 2)) ?:))
                   (setq i (+ i 3)))
                  (t (fail "group extension")))
            (push "\\(?:" out)
            (setq depth (1+ depth) quantifiable nil))
           ((eq c ?\))
            (when (zerop depth) (fail "unbalanced `)'"))
            (setq depth (1- depth))
            (push "\\)" out)
            (setq i (1+ i) quantifiable t))
           ((eq c ?|)
            (push "\\|" out)
            (setq i (1+ i) quantifiable nil ended nil))
           ((memq c '(?* ?+ ??))
            ;; Not after a quantifier: `*?' is lazy and `*+' possessive.
            (unless quantifiable (fail "quantifier"))
            (push (string c) out)
            (setq i (1+ i) quantifiable nil))
           ((memq c '(?. ?^ ?$ ?{ ?}))
            (fail (format "`%c'" c)))
           (t
            (push (regexp-quote (string c)) out)
            (setq i (1+ i) quantifiable t)))))
      (unless (zerop depth) (fail "unbalanced `('"))
      (apply #'concat (nreverse out)))))

(defconst lpy--word-stop-chars (lpy--word-rule-chars 'stopChars)
  "The characters no word runs over.")

(defconst lpy--word-backslash (lpy--word-rule-char 'backslash)
  "The backslash of the body step and the field patterns.")

(defconst lpy--word-braces (lpy--word-rule-chars 'fieldBraces)
  "A field's (OPEN CLOSE) braces.")

(defconst lpy--word-delimiter-regexp
  (lpy--word-python-regexp (lpy--word-rule 'fieldDelimiter))
  "Matches where a fieldDelimiter match starts (a test, not its extent).")

(defun lpy--word-at (regexp pos)
  "Return where REGEXP, matched at POS in the current buffer, ends, or nil.
The end of Python's match: group 1's start where it matched (a
lookahead's, `lpy--word-python-regexp'), else the match's end."
  (save-excursion
    (goto-char pos)
    (and (looking-at regexp) (or (match-beginning 1) (match-end 0)))))

(defun lpy--word-starts-with-p (string pos)
  "Return non-nil when the current buffer holds STRING at POS."
  (let ((end (+ pos (length string))))
    (and (<= end (point-max))
         (string= string (buffer-substring-no-properties pos end)))))

(defun lpy--word-meta-matcher (form-end)
  "Return a matcher of a meta-indicator not followed by FORM-END.
FORM-END is a regexp tested at the indicator's end.  An Emacs regexp has
no negative lookahead, so each wrappers entry and metaChars entry pair
is tried in the order Python's backtracking tries them."
  (let ((candidates
         (cl-loop for wrapper in (lpy--word-longest-first
                                  (lpy--word-rule 'wrappers))
                  nconc (cl-loop for meta in (lpy--word-longest-first
                                              (lpy--word-rule 'metaChars))
                                 collect (concat wrapper meta)))))
    (lambda (pos)
      (cl-loop for candidate in candidates
               for end = (+ pos (length candidate))
               when (and (lpy--word-starts-with-p candidate pos)
                         (not (lpy--word-at form-end end)))
               return end))))

(defun lpy--word-field-symbol-matcher (field-level)
  "Return a matcher of a symbol in a field, at its own level when FIELD-LEVEL.
A run of characters, each no stopChars character and no backslash
\(never where fieldDelimiter matches at the field's level), or a
backslash matching fieldSymbolBackslash; at the field's level a
fieldDelimiter match alone is a symbol too."
  (let ((backslash (lpy--word-python-regexp
                    (lpy--word-rule 'fieldSymbolBackslash) t))
        (delimiter (lpy--word-python-regexp (lpy--word-rule 'fieldDelimiter) t)))
    (lambda (pos)
      (let ((at pos) next)
        (while (and (< at (point-max))
                    (setq next
                          (let ((c (char-after at)))
                            (if (and (not (and field-level
                                               (lpy--word-at
                                                lpy--word-delimiter-regexp at)))
                                     (not (memq c lpy--word-stop-chars))
                                     (not (eq c lpy--word-backslash)))
                                (1+ at)
                              (lpy--word-at backslash at)))))
          (setq at next))
        (cond ((> at pos) at)
              (field-level (lpy--word-at delimiter pos)))))))

(defun lpy--word-field-comment-matcher ()
  "Return a matcher of a comment at a field's own level.
It stops before a `}', a lineBreakChars character or a fieldDelimiter
match."
  (let ((line-breaks (lpy--word-rule-chars 'lineBreakChars))
        (close (nth 1 lpy--word-braces)))
    (lambda (pos)
      (when (eq (char-after pos) ?\;)
        (let ((at (1+ pos)))
          (while (and (< at (point-max))
                      (let ((c (char-after at)))
                        (not (or (memq c line-breaks) (eq c close)
                                 (lpy--word-at lpy--word-delimiter-regexp
                                               at)))))
            (setq at (1+ at)))
          at)))))

(defun lpy--word-token-kinds (mode)
  "Return the token patterns of MODE, `code', `field' or `bracket'.
A list of (KIND . MATCHER) in the order the spec tries them.  A MATCHER
is a regexp matched at the token's start (`lpy--word-at') or, for a
pattern whose lookahead an Emacs regexp cannot express, a function of
the start returning the token's end or nil."
  (let* ((in-field (not (eq mode 'code)))
         (stop (lpy--word-char-class lpy--word-stop-chars))
         (continuation (lpy--word-python-regexp
                        (lpy--word-rule 'fieldContinuation)))
         ;; The line's end of every lookahead: Python's `$' is the text's
         ;; end (or before a last newline, which every set below holds).
         (end (concat "\\(?:"
                      (if in-field (concat continuation "\\|") "")
                      "\\'"
                      (if (eq mode 'field)
                          (concat "\\|" lpy--word-delimiter-regexp)
                        "")
                      "\\)"))
         (form-end-chars (lpy--word-rule-chars 'formEndChars))
         (form-end (concat "\\(?:" (lpy--word-char-class form-end-chars)
                           "\\|" end "\\)"))
         ;; A number ends at its own set, which also holds `{'.
         (number-end (concat "\\(?:"
                             (lpy--word-char-class
                              (lpy--word-rule-chars 'numberEndChars))
                             "\\|" end "\\)"))
         (wrapper (lpy--word-one-of (lpy--word-rule 'wrappers)))
         (opening (lpy--word-one-of (lpy--word-rule 'openings)))
         (meta (lpy--word-one-of (lpy--word-rule 'metaChars)))
         (escaped (lpy--word-one-of (lpy--word-rule 'escapedStringOpenings)))
         ;; Emacs has no lookahead: what a lookahead tests is matched
         ;; after an empty group 1, whose start is then the token's end.
         (ahead "\\(?1:\\)"))
    `((opening . ,(concat wrapper opening))
      (closing . ,(lpy--word-char-class (lpy--word-rule-chars 'closings)))
      (string . ,(concat "\\(?:" (lpy--word-python-regexp
                                  (lpy--word-rule 'stringPrefix))
                         "\\)\\(?:" (if in-field (concat escaped "\\|") "")
                         (lpy--word-one-of (lpy--word-rule 'stringOpenings))
                         "\\)"))
      (dangling . ,(concat wrapper
                           (lpy--word-one-of (lpy--word-rule 'danglingQuotes))
                           ahead form-end))
      ;; Not followed by a form end.  In code mode that is a character
      ;; outside them, and backtracking to a shorter indicator stays the
      ;; regexp's; a field's form end is no character class.
      (meta . ,(if in-field
                   (lpy--word-meta-matcher form-end)
                 (concat wrapper meta ahead
                         (lpy--word-char-class form-end-chars t))))
      (bad . ,(concat "\\^\\*+\\(?:" opening "\\|" meta "\\|" ahead
                      "\\(?:" stop (if in-field (concat "\\|" escaped) "")
                      "\\|" end "\\)\\)"))
      ;; Python's `.' is any character but a newline.
      (comment . ,(if (eq mode 'field)
                      (lpy--word-field-comment-matcher)
                    (concat ";" (lpy--word-char-class
                                 (cons ?\n (lpy--word-rule-chars
                                            'lineBreakChars))
                                 t)
                            "*")))
      (number . ,(concat "\\(?:" (lpy--word-python-regexp
                                  (lpy--word-rule 'numberToken))
                         "\\)" ahead number-end))
      (comma . ",")
      (symbol . ,(if in-field
                     (lpy--word-field-symbol-matcher (eq mode 'field))
                   (concat (lpy--word-char-class lpy--word-stop-chars t) "+")))
      (spaces . ,(if in-field (concat "\\(?: \\|" continuation "\\)+") " +")))))

(defconst lpy--word-modes
  (mapcar (lambda (mode)
            (let ((kinds (lpy--word-token-kinds mode)))
              (list mode kinds
                    ;; One regexp of every kind, each its own group from
                    ;; 2 (group 1 is the lookahead mark), when all are
                    ;; regexps: searched in one call, as Python's `search'.
                    (and (cl-every (lambda (kind) (stringp (cdr kind))) kinds)
                         (mapconcat
                          (lambda (index)
                            (format "\\(?%d:%s\\)" (+ index 2)
                                    (cdr (nth index kinds))))
                          (number-sequence 0 (1- (length kinds)))
                          "\\|")))))
          '(code field bracket))
  "Each mode as (MODE KINDS COMBINED): `lpy--word-token-kinds' and the
regexp of them all, or nil when a kind is a function.")

(defun lpy--word-combined-token (kinds)
  "Return (KIND . END) of the last match of KINDS' combined regexp."
  (cl-loop for (kind . _) in kinds
           for group from 2
           when (match-beginning group)
           return (cons kind (or (match-beginning 1) (match-end group)))))

(defun lpy--word-token-at (mode pos)
  "Return (KIND . END), the token of MODE's patterns at POS, or nil."
  (pcase-let ((`(,kinds ,combined) (alist-get mode lpy--word-modes)))
    (if combined
        (save-excursion
          (goto-char pos)
          (and (looking-at combined) (lpy--word-combined-token kinds)))
      (cl-loop for (kind . matcher) in kinds
               for end = (if (stringp matcher)
                             (lpy--word-at matcher pos)
                           (funcall matcher pos))
               when end return (cons kind end)))))

(defun lpy--word-next-token (mode pos limit)
  "Return (START KIND . END), MODE's next token from POS starting before LIMIT.
Nil when there is none.  As Python's `search': a character no pattern
matches is skipped."
  (pcase-let ((`(,kinds ,combined) (alist-get mode lpy--word-modes)))
    (if combined
        (save-excursion
          (goto-char pos)
          (and (re-search-forward combined nil t)
               (< (match-beginning 0) limit)
               (cons (match-beginning 0) (lpy--word-combined-token kinds))))
      (cl-loop for at from pos below limit
               for token = (lpy--word-token-at mode at)
               when token return (cons at token)))))

(defconst lpy--word-plain-stops
  (lpy--word-char-class (lpy--word-rule-chars 'plainBodyStops))
  "Matches where a plain string's body read stops.")

(defconst lpy--word-interpolated-stops
  (lpy--word-char-class (lpy--word-rule-chars 'interpolatedBodyStops))
  "Matches where an f/t-string's body read stops.")

(defun lpy--word-body (pos quote raw interpolated doubled)
  "The body step: read a string's body from POS; return (STOP . AT).
QUOTE is the string's quote as written, RAW non-nil for a raw string,
INTERPOLATED for an f/t-string, DOUBLED the brace characters a doubled
one of which is text.  STOP is `end' (AT the text's end), `quote' (AT the
closing quote), `refused' (a bareQuote in an escaped string), `field' (a
single opening brace) or `close' (a single closing brace)."
  (let ((escaped-quote (member quote (lpy--word-rule 'escapedStringOpenings)))
        (escaped (lpy--word-rule 'escapedQuote))
        (named (lpy--word-rule 'namedEscape))
        (quote-chars (lpy--word-rule-chars 'quoteChars))
        (bare (lpy--word-rule-char 'bareQuote))
        (stops (if interpolated lpy--word-interpolated-stops
                 lpy--word-plain-stops))
        (n (point-max))
        (i pos)
        (result nil))
    (save-excursion
      (while (not result)
        (goto-char i)
        (if (not (re-search-forward stops nil t))
            (setq result (cons 'end n))
          (let* ((j (match-beginning 0))
                 (c (char-after j)))
            (cond
             ((eq c lpy--word-backslash)
              (cond
               ((and escaped-quote (lpy--word-starts-with-p quote j))
                (setq result (cons 'quote j)))
               ((>= (1+ j) n) (setq i (1+ j)))
               ((and escaped-quote (lpy--word-starts-with-p escaped (1+ j)))
                (setq i (+ j 1 (length escaped))))
               ((and (not raw) (lpy--word-starts-with-p (car named) (1+ j)))
                (goto-char (+ j 1 (length (car named))))
                (setq i (if (search-forward (nth 1 named) nil t) (point) n)))
               ((and interpolated (memq (char-after (1+ j)) lpy--word-braces))
                (setq i (1+ j)))
               (t (setq i (+ j 2)))))
             ((memq c quote-chars)
              (cond ((and escaped-quote (eq c bare))
                     (setq result (cons 'refused j)))
                    ((lpy--word-starts-with-p quote j)
                     (setq result (cons 'quote j)))
                    (t (setq i (1+ j)))))
             ((and (memq c doubled) (eq (char-after (1+ j)) c))
              (setq i (+ j 2)))
             (t (setq result (cons (if (eq c (car lpy--word-braces)) 'field 'close)
                                   j))))))))
    result))

(cl-defstruct (lpy--word-literal
               (:constructor lpy--word-literal (quote-text raw kind))
               (:copier nil))
  "An open f/t-string literal of the context scan."
  quote-text raw kind)

(cl-defstruct (lpy--word-field
               (:constructor lpy--word-field (literal))
               (:copier nil))
  "An open field of the context scan: its LITERAL, open BRACKETS, whether
its format SPEC is being read and whether that spec holds a NESTED field."
  literal (brackets 0) spec nested)

(defun lpy--word-string-kind (prefix)
  "Return (RAW . KIND) of a string opening's PREFIX.
KIND is the kindLetters kind the prefix names, nil for a plain string."
  (let ((lowered (downcase prefix)))
    (cons (and (string-search (lpy--word-rule 'rawLetter) lowered) t)
          (cl-loop for (letter kind) in (lpy--word-rule 'kindLetters)
                   when (string-search letter lowered) return kind))))

(defconst lpy--word-prefix-regexp
  (lpy--word-python-regexp (lpy--word-rule 'stringPrefix))
  "Matches a string opening's prefix.")

(defconst lpy--word-conversion-ends
  (lpy--word-char-class (lpy--word-rule-chars 'conversionEnds))
  "Matches the character a conversion ends at.")

(defun lpy--word-context (stop &optional from snapshot)
  "Return (MODE . RESUME), the context and resume steps at STOP, or nil.
Lexes the current buffer's accessible text from its start, over a stack
of the f/t literals and fields open.  MODE is `code', `field', `bracket'
or `text'; RESUME is where the last string or literal the scan closed
ends (the text's start when none).  Nil where the reader refuses the text
before STOP.
FROM, a (POS . RESUME) state at the top level (no literal open) that a
scan of the same text up to POS's line end recorded, starts the scan
there.  SNAPSHOT, a cons (LIMIT . nil), records in its cdr the scan's
last such state before LIMIT (`lpy--word-scan-cache')."
  (catch 'context
    (let ((stack nil)
          (pos (if from (car from) (point-min)))
          (resume (if from (cdr from) (point-min)))
          (open (car lpy--word-braces)) (close (nth 1 lpy--word-braces)))
      (cl-flet ((done (mode) (throw 'context (cons mode resume)))
                (refused () (throw 'context nil)))
        (while t
          (when (and snapshot (null stack) (< pos (car snapshot)))
            (setcdr snapshot (cons pos resume)))
          (let ((top (car stack)))
            (if (and top (or (lpy--word-literal-p top)
                             (lpy--word-field-spec top)))
                ;; The text of a literal or a format spec.
                (let* ((spec (and (lpy--word-field-p top) top))
                       (literal (if spec (lpy--word-field-literal spec) top))
                       (quote-text (lpy--word-literal-quote-text literal)))
                  (when (>= pos stop) (done 'text))
                  (pcase-let ((`(,kind . ,at)
                               (lpy--word-body
                                pos quote-text (lpy--word-literal-raw literal)
                                t (cond ((null spec) lpy--word-braces)
                                        ((lpy--word-field-nested spec)
                                         (list open))))))
                    (cond
                     ((eq kind 'quote)
                      (when (> (+ at (length quote-text)) stop) (done 'text))
                      (when spec (refused))
                      (setq pos (+ at (length quote-text)) resume pos)
                      (pop stack))
                     ((and (eq kind 'refused) (< at stop)) (refused))
                     ((or (memq kind '(end refused)) (>= at stop)) (done 'text))
                     ((eq kind 'field)
                      (when spec (setf (lpy--word-field-nested spec) t))
                      (push (lpy--word-field literal) stack)
                      (setq pos (1+ at)))
                     (spec (pop stack) (setq pos (1+ at)))
                     (t (refused)))))
              (let* ((field top)
                     (mode (cond ((null field) 'code)
                                 ((> (lpy--word-field-brackets field) 0) 'bracket)
                                 (t 'field)))
                     (token (or (lpy--word-next-token mode pos stop)
                                (done mode))))
                (pcase-let ((`(,start ,kind . ,end) token))
                  (cond
                   ((and (eq mode 'field) (eq (char-after start) close))
                    (pop stack)
                    (setq pos (1+ start)))
                   ((and (eq mode 'field)
                         (lpy--word-at lpy--word-delimiter-regexp start))
                    ;; The steps name the two delimiters: `:' starts the
                    ;; format spec, `!' the conversion.
                    (if (eq (char-after start) ?:)
                        (setf (lpy--word-field-spec field) t
                              pos (1+ start))
                      (save-excursion
                        (goto-char (1+ start))
                        (unless (re-search-forward lpy--word-conversion-ends
                                                   stop t)
                          (done 'text))
                        (setq pos (match-beginning 0)))))
                   ((and (eq kind 'comment) (>= end stop)) (done 'text))
                   ((> end stop) (done mode))
                   ((eq kind 'string)
                    (let* ((prefix-end (lpy--word-at lpy--word-prefix-regexp
                                                     start))
                           (quote-text (buffer-substring-no-properties
                                        prefix-end end))
                           (string-kind (lpy--word-string-kind
                                         (buffer-substring-no-properties
                                          start prefix-end))))
                      (if (cdr string-kind)
                          (progn (push (lpy--word-literal
                                        quote-text (car string-kind)
                                        (cdr string-kind))
                                       stack)
                                 (setq pos end))
                        (pcase-let ((`(,body-stop . ,at)
                                     (lpy--word-body end quote-text
                                                     (car string-kind) nil nil)))
                          (when (and (eq body-stop 'refused) (< at stop))
                            (refused))
                          (when (or (not (eq body-stop 'quote))
                                    (> (+ at (length quote-text)) stop))
                            (done 'text))
                          (setq pos (+ at (length quote-text)) resume pos)))))
                   (t
                    (setq pos end)
                    (when field
                      (cond ((or (eq kind 'opening)
                                 (and (eq kind 'bad)
                                      (memq (char-before end)
                                            (lpy--word-rule-chars 'openings))))
                             (cl-incf (lpy--word-field-brackets field)))
                            ((and (eq kind 'closing)
                                  (> (lpy--word-field-brackets field) 0))
                             (cl-decf (lpy--word-field-brackets field))))))))))))))))

(defun lpy--word-line-token (bol caret mode run-start endpos)
  "The word token step: return (START KIND . END), or nil.
Columns of the line at BOL in the current buffer, which ends the line.
Lexes from RUN-START with MODE's patterns, the line ending at ENDPOS for
the patterns, until a token reaches the run's end."
  (let ((run-end run-start))
    (while (and (< run-end endpos)
                (not (memq (char-after (+ bol run-end)) lpy--word-stop-chars)))
      (setq run-end (1+ run-end)))
    (catch 'token
      (let ((pos run-start))
        (while (< pos run-end)
          (let ((token (save-restriction
                         (narrow-to-region bol (+ bol endpos))
                         (lpy--word-token-at mode (+ bol pos)))))
            (when (or (null token)
                      (and (eq mode 'field)
                           (lpy--word-at lpy--word-delimiter-regexp (+ bol pos))))
              (throw 'token nil))
            (let ((kind (car token)) (end (- (cdr token) bol)))
              (when (or (> end caret)
                        (and (= end caret) (memq kind '(symbol number))))
                (throw 'token (cons pos (cons kind end))))
              (setq pos end)))))
      nil)))

(defun lpy--word-debug-equals-p (line bol end)
  "Return non-nil when the symbol ending at column END of LINE loses its `='.
The debug = step: LINE starts at BOL in the current buffer and ends it."
  (let ((n (length line))
        (whitespace (lpy--word-rule-chars 'whitespace)))
    (and (>= end 2)
         ;; The steps name the debug `='.
         (eq (aref line (1- end)) ?=)
         (not (memq (aref line (- end 2)) (lpy--word-rule-chars 'debugBlockers)))
         (let ((at end))
           (while (and (< at n)
                       (let ((c (aref line at)))
                         (or (memq c whitespace)
                             (and (eq c lpy--word-backslash) (= (1+ at) n)))))
             (setq at (1+ at)))
           (or (= at n)
               (eq (aref line at) (nth 1 lpy--word-braces))
               (lpy--word-at lpy--word-delimiter-regexp (+ bol at)))))))

(defun lpy--word-full-regexp (field)
  "Return a regexp matching a whole string as the rule's pattern FIELD."
  (concat "\\`\\(?:" (lpy--word-python-regexp (lpy--word-rule field)) "\\)\\'"))

(defconst lpy--word-number-regexp (lpy--word-full-regexp 'numberToken)
  "Matches a whole number token.")

(defconst lpy--word-signed-complex-regexp (lpy--word-full-regexp 'signedComplex)
  "Matches a whole signed complex literal, which keeps its sign.")

(defun lpy--word-normalize-digits ()
  "Make each non-ASCII decimal digit of the buffer `lpy--word-digit'.
See `lpy--word-digit' for why."
  (save-excursion
    (goto-char (point-min))
    (let ((digit (string lpy--word-digit)))
      (while (re-search-forward lpy--word-other-digits-regexp nil t)
        (replace-match digit t t)))))

(defun lpy--word-peel (line start end kind caret)
  "The peel, start, replaceStart and end steps over LINE's token START..END.
KIND is the token's, `symbol' or `number'.  Return the span's columns."
  (let ((signs (lpy--word-rule-chars 'signs))
        (followers (lpy--word-rule-chars 'signPeelFollowers))
        (prefixes (lpy--word-rule 'tokenPrefixes))
        (never (lpy--word-rule 'neverPeeled))
        (leaf start))
    ;; Peel the token's front; LEAF is where the rest begins.
    (while (let ((rest (substring line leaf end)))
             (and (not (member rest never))
                  (>= (length rest) 2)
                  (not (and (eq kind 'number)
                            (or (not (memq (aref rest 0) signs))
                                (string-match-p
                                 lpy--word-signed-complex-regexp rest))))
                  (let ((prefix (cl-find-if
                                 (lambda (p) (string-prefix-p p rest))
                                 prefixes))
                        (unsigned (cl-position-if-not
                                   (lambda (c) (memq c signs)) rest)))
                    (cond
                     (prefix
                      (setq leaf (+ leaf (length prefix)))
                      (when (string-match-p lpy--word-number-regexp
                                            (substring line leaf end))
                        (setq kind 'number))
                      t)
                     ((and unsigned (> unsigned 0)
                           (let ((c (aref rest unsigned)))
                             (or (lpy--word-alnum-p c) (memq c followers))))
                      (setq leaf (+ leaf unsigned))
                      t))))))
    (when (>= caret leaf)
      (setq start leaf))
    (let ((dot (cl-position ?. line :start start :end caret :from-end t)))
      (list start
            (if dot (1+ dot) start)
            (or (cl-position ?. line :start caret :end end) end)))))

(defvar lpy--word-scan-cache nil
  "(BEFORE HORIZON POS . RESUME): a top-level state of the last scan.
BEFORE is the text before that scan's line, POS and RESUME a state
`lpy--word-context' recorded before the line, and HORIZON the length of
BEFORE's prefix the state depends on, through POS's line end.  A scan
whose text agrees with BEFORE that far starts there, so typing on a line
does not re-lex the text above it.  Sound because at the top level no
token and no pattern reads past a line break (each a stop character,
`lpy--word-line-breaks-end-top-level-tokens'): a state is decided by the
text through its line's end.")

(defconst lpy--word-line-breaks (lpy--word-rule-chars 'lineBreakChars)
  "The characters that end a line, as the reader splits lines.
A CR LF pair is one break, but either character ends the line before it
and starts the one after it, so no position needs the pair.")

(defconst lpy--word-line-break-regexp
  (lpy--word-char-class lpy--word-line-breaks)
  "Matches a line-break character.")

(defconst lpy--word-line-breaks-end-top-level-tokens
  (or (and lpy--word-line-breaks
           (cl-every (lambda (c) (memq c lpy--word-stop-chars))
                     lpy--word-line-breaks))
      (error "A line break is no word-boundary stop: `lpy--word-scan-cache' is unsound"))
  "Non-nil: the spec keeps what `lpy--word-scan-cache' relies on.")

(defun lpy--word-line-context (before bol caret)
  "Return (MODE . RESUME) of the caret at column CARET of the line at BOL.
The scan buffer holds BEFORE, the text before the line, and the line.
RESUME is a column of the line.  Starts from `lpy--word-scan-cache' when
it holds, and updates it."
  (let* ((cache lpy--word-scan-cache)
         (horizon (nth 1 cache))
         (from (and cache (<= horizon (length before))
                    (eq t (compare-strings (car cache) 0 horizon
                                           before 0 horizon))
                    (nthcdr 2 cache)))
         (snapshot (list bol))
         (found (lpy--word-context (+ bol caret) from snapshot)))
    (when-let* ((state (cdr snapshot)))
      (setq lpy--word-scan-cache
            (cons before
                  (cons (save-excursion
                          (goto-char (car state))
                          (1- (if (re-search-forward
                                   lpy--word-line-break-regexp bol t)
                                  (point)
                                bol)))
                        state))))
    (if found
        (cons (car found) (max (- (cdr found) bol) 0))
      ;; Refused: the caret's line alone, else code.
      (save-restriction
        (narrow-to-region bol (point-max))
        (let ((alone (lpy--word-context (+ bol caret))))
          (if alone
              (cons (car alone) (- (cdr alone) bol))
            (cons 'code 0)))))))

(defun lpy--word-span (line caret &optional before)
  "Return (START REPLACE-START END), the completion word at CARET in LINE.
Columns, as lispython's word-boundary spec defines them: START begins
the dotted name the server completes (sent up to the caret),
REPLACE-START is after its last dot before the caret, where a candidate
replaces, and END is the word's end, before the next dot.  All three are
CARET when the word is empty, and the result is nil in a string's or
comment's text, where no completion is offered.  BEFORE is the text
before LINE (its line breaks included), whose strings, comments and
fields decide the caret's context; LINE excludes its newline."
  (with-current-buffer (get-buffer-create " *lpy-word-scan*")
    (let ((case-fold-search nil)
          (inhibit-modification-hooks t))
      (widen)
      (erase-buffer)
      (insert (or before "") line)
      (lpy--word-normalize-digits)
      (let* ((bol (- (point-max) (length line)))
             (line (buffer-substring-no-properties bol (point-max)))
             (context (lpy--word-line-context (or before "") bol caret))
             (mode (car context))
             (run-start caret))
        (unless (eq mode 'text)
          (while (and (> run-start 0)
                      (not (memq (aref line (1- run-start))
                                 lpy--word-stop-chars)))
            (setq run-start (1- run-start)))
          (setq run-start (max run-start (cdr context)))
          (let ((token (lpy--word-line-token bol caret mode run-start
                                             (length line))))
            (when (and token (eq mode 'field) (eq (nth 1 token) 'symbol)
                       (lpy--word-debug-equals-p line bol (nthcdr 2 token)))
              ;; Lex again with the line ending at the `='.
              (setq token (lpy--word-line-token bol caret mode (car token)
                                                (1- (nthcdr 2 token))))
              (when (and token (< (nthcdr 2 token) caret))
                (setq token nil)))
            (if (not (and token (memq (nth 1 token) '(symbol number))))
                (list caret caret caret)
              (lpy--word-peel line (car token) (nthcdr 2 token) (nth 1 token)
                              caret))))))))

(defun lpy--completion-span ()
  "Return (WORD-START START END), the completion word at point, or nil.
Buffer positions of `lpy--word-span' over the reader's line at point
\(ended by each lineBreakChars character), the whole buffer's text
before it deciding the context, narrowing aside, each position then
clamped to the accessible region; nil in a string's or comment's text,
where no completion is offered.  The nREPL source sends
WORD-START..point, the full dotted prefix; both the eglot and nREPL
sources complete START..END, which they must share:
`lpy--merge-capf-results' drops a source with another start and
`lpy--rerunning-capf' keeps a re-run only when its bounds are unchanged."
  (let ((low (point-min)) (high (point-max)))
    (save-excursion
      (save-restriction
        ;; The spec's text is the whole document, and its lines end at each
        ;; lineBreakChars character (a lone CR too), never at Emacs's lines.
        (widen)
        (let* ((point (point))
               (not-break (concat "^" (mapconcat (lambda (c) (string ?\\ c))
                                                 lpy--word-line-breaks "")))
               (bol (progn (skip-chars-backward not-break) (point)))
               (eol (progn (goto-char point) (skip-chars-forward not-break)
                           (point)))
               (span (lpy--word-span (buffer-substring-no-properties bol eol)
                                     (- point bol)
                                     (buffer-substring-no-properties
                                      (point-min) bol))))
          ;; Clamped: callers read the bounds in the narrowed buffer.
          (and span
               (mapcar (lambda (column) (min high (max low (+ bol column))))
                       span)))))))

(defun lpy--completion-symbol-bounds ()
  "Return the (START . END) eglot completes, for `bounds-of-thing-at-point'.
The nREPL source's bounds (`lpy--completion-span').  Never nil, so no
word, or a string's or comment's text, yields (point . point), eglot's
own fallback, and not the default symbol bounds."
  (let ((span (lpy--completion-span)))
    (if span
        (cons (nth 1 span) (nth 2 span))
      (cons (point) (point)))))

(defconst lpy-nrepl--completion-category 'lpy-nrepl
  "Completion category of the nREPL source's table.
Its style is `lpy-nrepl-passthrough' (`completion-category-defaults').")

(defun lpy-nrepl--passthrough-all (string table pred point)
  "Return TABLE's own candidates for STRING up to POINT, filtered by PRED.
The all-completions function of the `lpy-nrepl-passthrough' style."
  (all-completions (substring string 0 point) table pred))

(defun lpy-nrepl--passthrough-try (string table pred point)
  "Complete STRING up to POINT as TABLE's own try-completion does.
PRED filters the candidates.  The try-completion function of the
`lpy-nrepl-passthrough' style: the rule is the table's
\(`lpy-nrepl--matched-completion'), only reshaped to a style's answer.
STRING is the whole word (up to the END of `lpy--completion-span') and
a completion replaces all of it, so the text after POINT is not kept; a
try that makes no progress leaves STRING as it is."
  (let* ((before (substring string 0 point))
         (try (try-completion before table pred)))
    (cond ((null try) nil) ; No match: later capfs and "No match" apply.
          ;; BEFORE is the sole label; t only when nothing follows it.
          ((eq try t) (if (equal before string) t (cons before point)))
          ((equal try before) (cons string point))
          (t (cons try (length try))))))

(defun lpy-nrepl--matched-completion (str matched action)
  "Answer completion ACTION for STR from MATCHED, the labels listed for STR.
Try- and test-completion follow the listing, not a label prefix match:
`completion--capf-wrapper' (default completion, company) drops a
non-exclusive source whose `try-completion' is nil, which a label match
would be at `my_v' for `my-var'.  Try-completion gives the sole match,
the shared start when every match extends STR literally, and otherwise
STR as typed (`m' for `MyClass' and `my-var')."
  (pcase action
    ('t matched)
    ('lambda (and (member str matched) t))
    (_ (cond ((null matched) nil)
             ((equal matched (list str)) t)
             ((null (cdr matched)) (car matched))
             ((cl-every (lambda (label) (string-prefix-p str label)) matched)
              (try-completion str matched))
             (t str)))))

;; The server matches the word (case and `-'/`_' insensitively), so the
;; table already holds exactly the candidates; the user's style (basic,
;; orderless) would re-filter the labels and drop `my-var' for `my_v'.
;; The same arrangement as eglot's `eglot-capf' and `eglot--dumb-flex'.
(add-to-list 'completion-styles-alist
             '(lpy-nrepl-passthrough
               lpy-nrepl--passthrough-try lpy-nrepl--passthrough-all
               "List the candidates the completion table answers, unfiltered."))
(add-to-list 'completion-category-defaults
             `(,lpy-nrepl--completion-category (styles lpy-nrepl-passthrough)))

(defun lpy-nrepl-completion-at-point ()
  "Completion-at-point function using nREPL.
Works with corfu, company-capf, and default completion.  Candidates are
the server's `items' for the word, annotated from their kind and
`labelDetails', as eglot candidates are.  The server matches the word
and gives each item a `filterText' that starts with the word as typed;
the table lists the labels whose filterText does, so `my_v' keeps
`my-var', and inserts the label.  The word is re-asked when it changes
\(`lpy--rerunning-capf').  Candidates come from the buffer file's scope
\(the session scope without a file).  Nothing is offered in a string's
or comment's text, where `lpy--completion-span' has no word."
  (when-let* (((lpy-nrepl-connected-p))
              (span (lpy--completion-span)))
    (let* ((prefix (buffer-substring-no-properties (nth 0 span) (point)))
           ;; The server gets the full dotted PREFIX and replies with
           ;; labels that are already the word after the last dot, which
           ;; is the text from START to point.
           (start (nth 1 span))
           (end (nth 2 span))
           ;; Computed here: the table may run in another buffer.
           (file-fields (lpy-nrepl--file-fields))
           (items (make-hash-table :test #'equal)) ; label -> (KIND-NUMBER DESCRIPTION FILTER-TEXT)
           (labels nil)
           (requested nil)
           (fetched nil)
           (fetch
            (lambda ()
              ;; The fetch depends only on the prefix captured above, so
              ;; one request serves every table call.  Sent and waited for
              ;; once, even unanswered: a busy server answers no re-send
              ;; either, and each table call (corfu makes several per
              ;; keystroke) would stall again.  A late reply still fills
              ;; the table; `lpy--rerunning-capf' re-asks on the next key.
              (unless requested
                (setq requested t)
                (lpy-nrepl--send
                 `((op . ,lpy-nrepl--op-complete) (prefix . ,prefix) ,@file-fields)
                 (lambda (response)
                   (setq items (make-hash-table :test #'equal)
                         labels nil)
                   (mapc (lambda (item)
                           (let ((label (alist-get 'label item)))
                             (push label labels)
                             (puthash label
                                      (list (alist-get 'kind item)
                                            (alist-get 'description
                                                       (alist-get 'labelDetails item))
                                            (or (alist-get 'filterText item) label))
                                      items)))
                         (alist-get 'items response))
                   (setq labels (nreverse labels)
                         fetched t)))
                (lpy-nrepl--await (lambda () fetched))))))
      (when (> (length prefix) 0)
        (list start end
              (lambda (str pred action)
                (pcase action
                  ('metadata
                   `(metadata (category . ,lpy-nrepl--completion-category)))
                  (`(boundaries . ,_) nil)
                  (_
                   (funcall fetch)
                   (lpy-nrepl--matched-completion
                    str
                    ;; Matched on filterText, never the label: the server
                    ;; guarantees filterText starts with the typed word.
                    ;; A label also matches itself, so an inserted
                    ;; candidate passes `test-completion'.  A fresh list,
                    ;; as callers may sort it in place.
                    (cl-loop for label in labels
                             when (and (or (equal str label)
                                           (string-prefix-p
                                            str (nth 2 (gethash label items))))
                                       (or (null pred) (funcall pred label)))
                             collect label)
                    action))))
              :annotation-function
              (lambda (candidate)
                (let ((item (gethash candidate items)))
                  (lpy--item-annotation (nth 1 item) (nth 0 item))))
              :company-kind
              (lambda (candidate)
                (lpy--item-company-kind
                 (nth 0 (gethash candidate items))))
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
follow MAIN's candidates and keep EXTRA's per-candidate properties.
Each table lists its own candidates for the string: EXTRA's are not
re-filtered by label, since the nREPL's match on filterText.
Each EXTRA-only candidate is a copy carrying the `lpy--extra-only' text
property, which the sorter and the per-candidate dispatchers read.  The
owner travels on the string, not in a per-table record of EXTRA-only
names, because one merged table is queried with several strings (corfu
lists the candidates, then calls `test-completion' on the inserted
text): a record rebuilt by the later query would no longer hold the
names the earlier listing showed."
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
                 (extra-pred (plist-get extra-plist :predicate)))
      (cl-labels
          ((and-pred (own pred)
             (cond ((and own pred) (lambda (&rest args)
                                     (and (apply own args) (apply pred args))))
                   (t (or own pred))))
           (extra-only-p (cand)
             (and (stringp cand) (> (length cand) 0)
                  (get-text-property 0 'lpy--extra-only cand)))
           (candidates (str pred)
             (let ((main-cands (all-completions str main-table
                                                (and-pred main-pred pred)))
                   (seen (make-hash-table :test #'equal))
                   (kept nil))
               (dolist (cand main-cands)
                 (puthash (substring-no-properties cand) t seen))
               ;; A function table answers `all-completions' itself, so
               ;; the nREPL's filterText match decides, not the label.
               (dolist (cand (all-completions str extra-table
                                              (and-pred extra-pred pred)))
                 (let ((name (substring-no-properties cand)))
                   (unless (gethash name seen)
                     (puthash name t seen)
                     (let ((copy (copy-sequence cand)))
                       (put-text-property 0 (length copy)
                                          'lpy--extra-only t copy)
                       (push copy kept)))))
               ;; MAIN's strings are returned as they are: eglot keeps
               ;; its LSP item as a text property on the candidate.
               (append main-cands (nreverse kept))))
           (owner-plist (cand)
             (if (extra-only-p cand)
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
                          (if (extra-only-p cand)
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
Never `detail': backends such as pyrefly put a whole multi-line
overload set there, which LSP means for the focused item only."
  (lpy--item-annotation
   (plist-get (plist-get item :labelDetails) :description)
   (plist-get item :kind)))

(defun lpy--docsig-line (sig)
  "Return signature SIG as one echo-area line.
Decorator lines (pyrefly heads an overload set with `@overload') are
dropped, so the line starts at the first `def'."
  (string-join (split-string
                (mapconcat #'identity
                           (cl-remove-if (lambda (line)
                                           (string-prefix-p "@" (string-trim-left line)))
                                         (split-string sig "\n"))
                           " "))
               " "))

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
  "Return eglot capf RESULT with kind-word annotations, signature in docs.
Each candidate is annotated by `lpy--lsp-annotation'; its signature (the
item's `detail') heads the `:company-doc-buffer' popup and is the
`:company-docsig' line (`lpy--docsig-line'), as CIDER shows the arglist
outside the list.  The signature is read from the item as first sent: a
server that sends `detail' only on completionItem/resolve (pyright) gets
eglot's docs and docsig unchanged, since eglot keeps the resolved item
private.  pyrefly sends it up front."
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
                   (lpy--docsig-line sig)
                 (and docsig (funcall docsig cand)))))
          ,@(cl-loop for (key value) on plist by #'cddr
                     unless (memq key '(:annotation-function
                                        :company-doc-buffer
                                        :company-docsig))
                     nconc (list key value)))))))

(defun lpy--merged-capf ()
  "Return the merged capf result of the eglot and nREPL sources.
Merges the candidates of `eglot-completion-at-point', when eglot manages
the buffer, with those of `lpy-nrepl-completion-at-point', when an nREPL
is connected.  A name both offer appears once, with eglot's docs and exit
function; names only the nREPL knows keep its annotation and kind.  Both
sources are annotated with the kind's word (`lpy--completion-kinds');
an LSP candidate's signature goes to the doc popup and the docsig line
\(`lpy--shorten-eglot-capf').  Works with neither, either or both
sources available; a lone source is returned unmerged."
  (lpy--merge-capf-results
   (lpy--shorten-eglot-capf
    (and (fboundp 'eglot-managed-p) (eglot-managed-p)
         ;; eglot's bounds are `bounds-of-thing-at-point' on a symbol, its
         ;; only syntax-dependent step.  The provider is consulted first
         ;; and never returns nil, so it alone decides the bounds (the
         ;; nREPL source's start) and the syntax table is never read.
         ;; This alist is why the package needs Emacs 30.
         (let ((bounds-of-thing-at-point-provider-alist
                (cons (cons 'symbol #'lpy--completion-symbol-bounds)
                      bounds-of-thing-at-point-provider-alist)))
           (eglot-completion-at-point))))
   (lpy-nrepl-completion-at-point)))

(defun lpy--candidate-generation (cand)
  "Return the capf generation plist tagged on CAND, or nil."
  (and (stringp cand) (> (length cand) 0)
       (get-text-property 0 'lpy--capf-generation cand)))

(defun lpy--generation-dispatcher (key latest)
  "Return a KEY property applying KEY of the candidate's generation.
An untagged candidate (a `try-completion' result) uses the plist the
function LATEST returns.  A separate function, so each KEY is its own
binding (`cl-loop' reuses one variable for every iteration)."
  (lambda (cand &rest args)
    (let ((fn (plist-get (or (lpy--candidate-generation cand)
                             (funcall latest))
                         key)))
      (when fn (apply fn cand args)))))

(defun lpy--rerunning-capf (capf)
  "Call CAPF and return its result with a table that re-runs CAPF.
eglot caches the server's answer inside each table it returns, and
corfu keeps one table for its whole session, so an answer the server
marked isIncomplete would never be re-asked while typing.  The returned
table re-calls CAPF when its candidates are asked for and the text
between the bounds has changed, and lists the new result's candidates
when its bounds are the same.  A re-run with no result or other
bounds means the session's word (the text from the session's start)
is no longer the word at point, so the table answers nothing until a
re-run has the session's bounds again.  Each listed candidate is
tagged with the plist of the CAPF call that produced it, and the returned
per-candidate properties apply that plist's functions, so a candidate
keeps its exit function and docs after later re-runs.  Modelled on
cape's `cape-wrap-buster'."
  (pcase (funcall capf)
    (`(,beg ,end ,table . ,plist)
     (let* ((buffer (current-buffer))
            (beg (copy-marker beg))
            (end (copy-marker end t))
            (input (buffer-substring-no-properties beg end))
            (latest plist))
       `(,(marker-position beg) ,(marker-position end)
         ,(lambda (str pred action)
            ;; Only the candidates call re-runs, never metadata or
            ;; `test-completion': accepting a candidate inserts it and
            ;; then tests it, and a re-run there would build a table
            ;; from the inserted text.  Reading the previous table's
            ;; metadata first is harmless: the merged sorter is
            ;; stateless (`lpy--merge-capf-results').
            (when (and (eq action t) (buffer-live-p buffer))
              (with-current-buffer buffer
                (let ((new-input (buffer-substring-no-properties beg end)))
                  (unless (equal input new-input)
                    (let ((result (let (completion-ignore-case
                                        completion-regexp-list)
                                    (funcall capf))))
                      (let (throw-on-input) ; No interrupt mid-update.
                        (pcase result
                          ((and `(,new-beg ,new-end ,new-table . ,new-plist)
                                (guard (and (= beg new-beg) (= end new-end))))
                           (setq table new-table
                                 latest new-plist))
                          ;; No result or moved bounds: the session's
                          ;; word (the text from its start) is no longer
                          ;; the word at point.  The old table is
                          ;; server-filtered and answers only for its own
                          ;; word; kept, it would feed the user's fallback
                          ;; style (orderless) every old candidate, so the
                          ;; session lists nothing.
                          (_ (setq table nil)))
                        ;; Recorded either way, so CAPF is not re-run on
                        ;; every call.
                        (setq input new-input)))))))
            (if (eq action t)
                (let ((generation latest))
                  (mapcar (lambda (cand)
                            (let ((copy (copy-sequence cand)))
                              (put-text-property 0 (length copy)
                                                 'lpy--capf-generation
                                                 generation copy)
                              copy))
                          (complete-with-action t table str pred)))
              (complete-with-action action table str pred)))
         ;; A dispatcher for every per-candidate key, not only the first
         ;; result's: a source joining mid-session (eglot answering after
         ;; the nREPL did) brings its exit function and docs then.
         ,@(cl-loop for key in lpy--capf-candidate-properties
                    nconc (list key (lpy--generation-dispatcher
                                     key (lambda () latest))))
         ,@(cl-loop for (key value) on plist by #'cddr
                    unless (memq key lpy--capf-candidate-properties)
                    nconc (list key value)))))))

(defun lpy-completion-at-point ()
  "Completion-at-point function for `lpy-mode' buffers.
Merges eglot's LSP candidates with the nREPL's (`lpy--merged-capf'),
re-running the merge as the typed word changes (`lpy--rerunning-capf'),
so candidates follow the server on every keystroke."
  (lpy--rerunning-capf #'lpy--merged-capf))

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
         `((op . ,lpy-nrepl--op-docs) (symbol . ,symbol) ,@(lpy-nrepl--file-fields))
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
   ((process-live-p lpy-nrepl--server-process)
    (let ((port (process-get lpy-nrepl--server-process 'lpy-nrepl-port)))
      (if port
          (lpy-nrepl-connect port)
        (message "lpy-nrepl: the nREPL server is still starting"))))
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
    (define-key map (kbd "C-c C-c") #'lpy-nrepl-interrupt)
    (define-key map (kbd "C-c C-z") #'lpy-nrepl-switch-to-repl)
    map)
  "Keymap for `lpy-nrepl-repl-mode'.")

(define-derived-mode lpy-nrepl-repl-mode special-mode "Lpy-nREPL"
  "Major mode for the nREPL interaction log buffer.
Shows input sent to and output received from the nREPL server."
  (setq-local truncate-lines nil)
  (setq-local word-wrap t)
  ;; A log, like *Messages*: with undo on, every line the cap trims
  ;; (`lpy-nrepl--trim-buffer') is kept in `buffer-undo-list', so the growth
  ;; an endless printing loop causes just moves there.
  (buffer-disable-undo)
  (setq mode-line-process
        '(:eval (concat " ["
                  (replace-regexp-in-string "%" "%%" (lpy-nrepl--scope-label)
                                            t t)
                  "]" (lpy-nrepl--eval-status)))))

;;; Provide:

(provide 'lpy-nrepl)

;;; lpy-nrepl.el ends here
