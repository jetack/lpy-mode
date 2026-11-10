;;; lpy-nrepl-test.el --- nREPL client tests -*- lexical-binding: t -*-

;; ERT tests for `lpy-nrepl': completion kind annotations, `.nrepl-port'
;; discovery, connection swapping and REPL log scrolling.  Run with:
;;   B=~/.emacs.d/straight/build
;;   emacs -Q --batch -L . -L $B/dash -L $B/s -L $B/f \
;;     --eval '(setq load-prefer-newer t)' \
;;     -l test/lpy-nrepl-test.el -f ert-run-tests-batch-and-exit
;; load-prefer-newer matters: a stale (gitignored) lpy-nrepl.elc beside the
;; source is otherwise loaded instead, and the tests silently run old code.
;; LPY_TEST_LSP_PYTHON=<a python with lispython> also runs the live tests,
;; among them the check that test/word-boundary.json is lispython's spec.
;; LPY_TEST_NREPL_LPY=<an lpy executable> runs the live nREPL tests (start,
;; kill, restart; the streaming/interrupt one needs lispython with nREPL
;; streaming and interrupt support and fails, bounded, on an older one).
;; Refresh that file with `python -m lispy.word_boundary > test/word-boundary.json'
;; and copy its `rule' into `lpy--word-boundary-rule'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(add-to-list 'load-path
             (file-name-directory
              (directory-file-name
               (file-name-directory (or load-file-name buffer-file-name)))))
(require 'lpy-nrepl)

(ert-deftest lpy-nrepl-test-every-lsp-kind-is-annotated ()
  ;; Defect: an LSP kind missing from the table (Snippet, Operator, ...)
  ;; is listed with no annotation, where eglot showed its kind name.
  (cl-loop for number from 1 to 25
           do (should (lpy--kind-annotation (gethash number lpy--lsp-kinds)))))

(defmacro lpy-nrepl-test--with-project (port-text &rest body)
  "Run BODY in a temp project/sub dir; write PORT-TEXT to root's port file."
  (declare (indent 1))
  `(let* ((root (make-temp-file "lpy-nrepl-" t))
          (sub (expand-file-name "src/pkg/" root)))
     (unwind-protect
         (progn
           (make-directory sub t)
           (when ,port-text
             (with-temp-file (expand-file-name ".nrepl-port" root)
               (insert ,port-text)))
           ,@body)
       (delete-directory root t))))

(ert-deftest lpy-nrepl-test-discover-port-from-subdir ()
  (lpy-nrepl-test--with-project "54321"
    (should (equal (lpy-nrepl--discover-port sub) 54321))))

(ert-deftest lpy-nrepl-test-discover-port-trims-whitespace ()
  (lpy-nrepl-test--with-project "7777\n"
    (should (equal (lpy-nrepl--discover-port sub) 7777))))

(ert-deftest lpy-nrepl-test-discover-port-rejects-bad-content ()
  (dolist (text '("" "abc" "12 34" "0" "70000" "-5"))
    (lpy-nrepl-test--with-project text
      (should-not (lpy-nrepl--discover-port sub)))))

(ert-deftest lpy-nrepl-test-discover-port-missing-file ()
  (lpy-nrepl-test--with-project nil
    (should-not (lpy-nrepl--read-port-file
                 (expand-file-name ".nrepl-port" root)))))

;;; Connection swapping

(defun lpy-nrepl-test--server ()
  "Start a local TCP server on a free port and return it.
The server records its accepted clients (`lpy-nrepl-test--client') and
the text they send (`lpy-nrepl-test--requests')."
  (make-network-process
   :name "lpy-nrepl-test-server" :server t
   :service t :host "127.0.0.1" :family 'ipv4
   :noquery t
   :log (lambda (server client _message)
          (process-put client 'lpy-test-server server)
          (process-put server 'lpy-test-clients
                       (cons client (process-get server 'lpy-test-clients))))
   :filter (lambda (client text)
             (let ((server (process-get client 'lpy-test-server)))
               (process-put server 'lpy-test-received
                            (concat (process-get server 'lpy-test-received)
                                    text))))))

(defun lpy-nrepl-test--client (server)
  "Return SERVER's newest accepted client connection."
  (car (process-get server 'lpy-test-clients)))

(defun lpy-nrepl-test--requests (server &optional op)
  "Return the requests SERVER received, oldest first, as alists.
With OP, only the requests of that op."
  (cl-loop for line in (split-string (or (process-get server 'lpy-test-received) "")
                                     "\n" t)
           for msg = (json-parse-string line :object-type 'alist
                                        :false-object nil)
           when (or (null op) (equal (alist-get 'op msg) op))
           collect msg))

(defun lpy-nrepl-test--wait (pred &optional seconds)
  "Process input until PRED returns non-nil or SECONDS (default 3) pass.
Return PRED's last value."
  (let ((deadline (+ (float-time) (or seconds 3)))
        value)
    (while (and (not (setq value (funcall pred)))
                (< (float-time) deadline))
      (accept-process-output nil 0.02))
    value))

(defun lpy-nrepl-test--answer (server reply)
  "Send REPLY, an alist, from SERVER's client connection to the client."
  (process-send-string (lpy-nrepl-test--client server)
                       (concat (json-serialize reply) "\n")))

(defun lpy-nrepl-test--log-text ()
  "Return the REPL log's text."
  (with-current-buffer (lpy-nrepl--repl-buffer)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun lpy-nrepl-test--port (server)
  "Return the port SERVER listens on."
  (process-contact server :service))

(defvar lpy-nrepl-test--messages nil
  "Messages emitted while `lpy-nrepl-test--with-servers' runs, newest first.")

(defconst lpy-nrepl-test--servers-buffer-name " *lpy-nrepl-test-servers*"
  "REPL log buffer name used instead of the live \"*lpy-nrepl*\".
Connecting writes to the log buffer and cleanup kills it; under M-x ert in
a live Emacs the real buffer holds the user's REPL history.")

(defmacro lpy-nrepl-test--with-servers (vars &rest body)
  "Run BODY with VARS bound to local servers and `message' captured.
The globals and the REPL log buffer name are let-bound; processes and the
test log buffer are cleaned up afterwards."
  (declare (indent 1))
  `(let ((lpy-nrepl--repl-buffer-name lpy-nrepl-test--servers-buffer-name)
         (lpy-nrepl--server-buffer-name " *lpy-nrepl-test-server-output*")
         (lpy-nrepl--connection nil)
         (lpy-nrepl--server-process nil)
         (lpy-nrepl--pending-callbacks nil)
         (lpy-nrepl--in-flight-evals nil)
         (lpy-nrepl--ignored-ids nil)
         (lpy-nrepl--latest-by-op nil)
         (lpy-nrepl--answers-during-eval nil)
         (lpy-nrepl--response-buffer "")
         (lpy-nrepl-test--messages nil)
         ,@(mapcar (lambda (v) `(,v (lpy-nrepl-test--server))) vars))
     (cl-letf (((symbol-function 'message)
                (lambda (fmt &rest args)
                  (when fmt
                    (push (apply #'format fmt args) lpy-nrepl-test--messages)))))
       (unwind-protect (progn ,@body)
         (when lpy-nrepl--server-process
           (let ((proc lpy-nrepl--server-process))
             (setq lpy-nrepl--server-process nil)
             (delete-process proc)))
         (when lpy-nrepl--connection (delete-process lpy-nrepl--connection))
         ,@(mapcar (lambda (v)
                     `(progn
                        (mapc #'delete-process (process-get ,v 'lpy-test-clients))
                        (delete-process ,v)))
                   vars)
         (accept-process-output nil 0.05)
         (dolist (name (list lpy-nrepl-test--servers-buffer-name
                             lpy-nrepl--server-buffer-name))
           (when (get-buffer name)
             (kill-buffer name)))))))

(ert-deftest lpy-nrepl-test-with-servers-spares-live-repl-buffer ()
  ;; Never writes to a pre-existing real buffer: only compares it before and
  ;; after.  Batch has none, so the test creates (and removes) a stand-in.
  (let* ((created (not (get-buffer "*lpy-nrepl*")))
         (live (get-buffer-create "*lpy-nrepl*"))
         (before (progn
                   (when created
                     (with-current-buffer live (insert "user history")))
                   (with-current-buffer live (buffer-string)))))
    (unwind-protect
        (progn
          (lpy-nrepl-test--with-servers (srv)
            (lpy-nrepl-connect (lpy-nrepl-test--port srv))
            (should (get-buffer lpy-nrepl-test--servers-buffer-name)))
          (should-not (get-buffer lpy-nrepl-test--servers-buffer-name))
          (should (buffer-live-p live))
          (should (equal (with-current-buffer live (buffer-string)) before)))
      (when (and created (buffer-live-p live)) (kill-buffer live)))))

(ert-deftest lpy-nrepl-test-failed-connect-keeps-connection ()
  (lpy-nrepl-test--with-servers (srv)
    (lpy-nrepl-connect (lpy-nrepl-test--port srv))
    (let* ((orig lpy-nrepl--connection)
           (callbacks '((1 . ignore)))
           (dead (let ((tmp (lpy-nrepl-test--server)))
                   (prog1 (lpy-nrepl-test--port tmp)
                     (delete-process tmp)))))
      (setq lpy-nrepl--pending-callbacks callbacks
            lpy-nrepl--response-buffer "{\"id\"")
      (should-error (lpy-nrepl-connect dead))
      (accept-process-output nil 0.1)
      (should (eq lpy-nrepl--connection orig))
      (should (lpy-nrepl-connected-p))
      (should (eq lpy-nrepl--pending-callbacks callbacks))
      (should (equal lpy-nrepl--response-buffer "{\"id\"")))))

(ert-deftest lpy-nrepl-test-reconnect-swaps-connection ()
  (lpy-nrepl-test--with-servers (srv-a srv-b)
    (lpy-nrepl-connect (lpy-nrepl-test--port srv-a))
    (let ((a lpy-nrepl--connection))
      (setq lpy-nrepl-test--messages nil)
      (lpy-nrepl-connect (lpy-nrepl-test--port srv-b))
      (let ((b lpy-nrepl--connection))
        (should-not (eq a b))
        (should (process-live-p b))
        (should-not (process-live-p a))
        (dotimes (_ 3) (accept-process-output nil 0.1))
        (should (eq lpy-nrepl--connection b))
        (should (lpy-nrepl-connected-p))
        (should-not (cl-some (lambda (m) (string-match-p "disconnected" m))
                             lpy-nrepl-test--messages))))))

(ert-deftest lpy-nrepl-test-sentinel-drops-broken-connection ()
  (lpy-nrepl-test--with-servers (srv)
    (lpy-nrepl-connect (lpy-nrepl-test--port srv))
    (let ((proc lpy-nrepl--connection))
      (setq lpy-nrepl--pending-callbacks '((1 . ignore)))
      (should (lpy-nrepl-test--wait (lambda () (lpy-nrepl-test--client srv))))
      (delete-process (lpy-nrepl-test--client srv))
      (should (lpy-nrepl-test--wait (lambda () (null lpy-nrepl--connection))))
      (should-not lpy-nrepl--pending-callbacks)
      (should-not (process-live-p proc))
      (should-not (memq proc (process-list))))))

;; Shared setup for the REPL-log scroll tests.  Binds REPL to an isolated
;; log buffer prefilled with 100 lines, SRC to a source buffer shown in the
;; selected window, and WIN to a non-selected window showing REPL.
;; EVAL-INPUT runs the real eval path for "(+ 1 2)"; EVAL-RESPONSE feeds the
;; matching "3" response through the process filter.
;; Callers assert window-point, not window-end: batch Emacs never
;; redisplays, so window-start stays put and (window-end W t) reports
;; point-max either way.
(defmacro lpy-nrepl-test--with-repl-window (&rest body)
  (declare (indent 0) (debug t))
  `(let* ((lpy-nrepl--repl-buffer-name " *lpy-nrepl-test-scroll*")
          (lpy-nrepl--connection nil)
          (lpy-nrepl--pending-callbacks nil)
          (lpy-nrepl--response-buffer "")
          (lpy-nrepl--request-counter 0)
          (sent nil)
          (src (generate-new-buffer " *lpy-nrepl-test-src*"))
          (repl (get-buffer-create lpy-nrepl--repl-buffer-name)))
     (unwind-protect
         (cl-letf (((symbol-function 'lpy-nrepl-connected-p) (lambda () t))
                   ((symbol-function 'process-send-string)
                    (lambda (_proc str) (push str sent)))
                   ((symbol-function 'message) #'ignore))
           (with-current-buffer repl
             (lpy-nrepl-repl-mode)
             (let ((inhibit-read-only t))
               (dotimes (i 100) (insert (format "line %d\n" i)))))
           (save-window-excursion
             (let* ((sel (selected-window))
                    (win (split-window sel))
                    (repl-end (lambda () (with-current-buffer repl (point-max))))
                    (eval-input
                     (lambda ()
                       (with-current-buffer src
                         (lpy-nrepl--eval-code "(+ 1 2)" (point-min)))))
                    (eval-response
                     (lambda ()
                       (let ((id (alist-get 'id (json-parse-string
                                                 (car sent) :object-type 'alist))))
                         (lpy-nrepl--process-filter
                          nil (format "{\"id\":\"%s\",\"value\":\"3\"}\n" id))))))
               (ignore repl-end eval-input eval-response)
               (set-window-buffer sel src)
               (set-window-buffer win repl)
               (with-current-buffer src (insert "(+ 1 2)"))
               ,@body)))
       (kill-buffer src)
       (kill-buffer repl))))

(ert-deftest lpy-nrepl-test-log-follows-at-end ()
  (lpy-nrepl-test--with-repl-window
    (set-window-point win (funcall repl-end))
    (funcall eval-input)
    ;; "input" path.
    (should (= (window-point win) (funcall repl-end)))
    (funcall eval-response)
    ;; "value" path.
    (should (= (window-point win) (funcall repl-end)))
    (should-not (eq win (selected-window)))
    (with-current-buffer repl
      (should (string-suffix-p "3\n" (buffer-string))))))

(ert-deftest lpy-nrepl-test-log-scrolled-up-stays ()
  (lpy-nrepl-test--with-repl-window
    (set-window-point win 50)
    (funcall eval-input)
    (should (= (window-point win) 50))
    (funcall eval-response)
    (should (= (window-point win) 50))
    (should-not (eq win (selected-window)))
    (with-current-buffer repl
      (should (string-suffix-p "3\n" (buffer-string))))))

(ert-deftest lpy-nrepl-test-log-follows-again-at-end ()
  (lpy-nrepl-test--with-repl-window
    (set-window-point win 50)
    (funcall eval-input)
    (funcall eval-response)
    (should (= (window-point win) 50))
    (set-window-point win (funcall repl-end))
    (funcall eval-input)
    (should (= (window-point win) (funcall repl-end)))
    (funcall eval-response)
    (should (= (window-point win) (funcall repl-end)))))

;; In the selected window, window-point is the buffer's own point.
(ert-deftest lpy-nrepl-test-log-selected-window-scrolled-up-stays ()
  (lpy-nrepl-test--with-repl-window
    (select-window win)
    (with-current-buffer repl (goto-char 50))
    (funcall eval-input)
    (funcall eval-response)
    (should (eq win (selected-window)))
    (should (= (window-point win) 50))
    (with-current-buffer repl
      (should (= (point) 50))
      (should (string-suffix-p "3\n" (buffer-string))))))

(ert-deftest lpy-nrepl-test-log-selected-window-follows-at-end ()
  (lpy-nrepl-test--with-repl-window
    (select-window win)
    (with-current-buffer repl (goto-char (point-max)))
    (funcall eval-input)
    (should (eq win (selected-window)))
    (should (= (window-point win) (funcall repl-end)))
    (with-current-buffer repl
      (should (= (point) (point-max))))
    (funcall eval-response)
    (should (= (window-point win) (funcall repl-end)))
    (with-current-buffer repl
      (should (= (point) (point-max)))
      (should (string-suffix-p "3\n" (buffer-string))))))

;; With no window showing the log, buffer point is what a later C-c C-z
;; (display-buffer) shows, so a buffer at its end must keep following.
(ert-deftest lpy-nrepl-test-log-undisplayed-follows-at-end ()
  (lpy-nrepl-test--with-repl-window
    (set-window-buffer win src)
    (should-not (get-buffer-window-list repl nil t))
    (with-current-buffer repl (goto-char (point-max)))
    (funcall eval-input)
    (with-current-buffer repl
      (should (= (point) (point-max))))
    (funcall eval-response)
    (should-not (get-buffer-window-list repl nil t))
    (with-current-buffer repl
      (should (= (point) (point-max)))
      (should (string-suffix-p "3\n" (buffer-string))))))

;;; Request file field

(defmacro lpy-nrepl-test--with-sent (file &rest body)
  "Run BODY in a temp buffer visiting FILE (nil: none); return the sent messages.
The stubbed send records each message and answers it with a canned response."
  (declare (indent 1))
  `(let ((sent nil))
     (cl-letf (((symbol-function 'lpy-nrepl-connected-p) (lambda () t))
               ((symbol-function 'lpy-nrepl--send)
                (lambda (msg &optional callback)
                  (push msg sent)
                  (when callback
                    (funcall callback '((items . []) (docs . "d")
                                        (expansion . "x"))))))
               ((symbol-function 'lpy-nrepl--display-macroexpand-result)
                #'ignore)
               ((symbol-function 'lpy--current-form-string)
                (lambda () "(foo bar)")))
       (with-temp-buffer
         (insert "(foo bar)")
         (goto-char (+ (point-min) 2))
         (when ,file (setq buffer-file-name ,file))
         ,@body))
     (nreverse sent)))

(defun lpy-nrepl-test--file-of (msgs op)
  "Return the `file' entry of the MSGS message for OP."
  (let ((msg (cl-find op msgs :key (lambda (m) (alist-get 'op m)) :test #'equal)))
    (should msg)
    (assq 'file msg)))

(ert-deftest lpy-nrepl-test-complete-sends-file ()
  (let ((msgs (lpy-nrepl-test--with-sent "/tmp/a.lpy"
                ;; The table runs in a buffer with no file, like a minibuffer.
                (let ((table (nth 2 (lpy-nrepl-completion-at-point))))
                  (with-temp-buffer (all-completions "fo" table))))))
    (should (equal (lpy-nrepl-test--file-of msgs "complete")
                   '(file . "/tmp/a.lpy")))))

(ert-deftest lpy-nrepl-test-docs-sends-file ()
  (let ((msgs (lpy-nrepl-test--with-sent "/tmp/a.lpy"
                (lpy-nrepl-eldoc-function #'ignore))))
    (should (equal (lpy-nrepl-test--file-of msgs "docs")
                   '(file . "/tmp/a.lpy")))))

(ert-deftest lpy-nrepl-test-macroexpand-sends-file ()
  (let ((msgs (lpy-nrepl-test--with-sent "/tmp/a.lpy"
                (lpy-nrepl-macroexpand))))
    (should (equal (lpy-nrepl-test--file-of msgs "macroexpand")
                   '(file . "/tmp/a.lpy")))))

(ert-deftest lpy-nrepl-test-no-file-buffer-sends-no-file ()
  (let ((msgs (lpy-nrepl-test--with-sent nil
                (all-completions "fo" (nth 2 (lpy-nrepl-completion-at-point)))
                (lpy-nrepl-eldoc-function #'ignore)
                (lpy-nrepl-macroexpand))))
    (should (= (length msgs) 3))
    (dolist (m msgs) (should-not (assq 'file m)))))

(ert-deftest lpy-nrepl-test-complete-fetches-once-per-capf-call ()
  (let ((sends 0))
    (cl-letf (((symbol-function 'lpy-nrepl-connected-p) (lambda () t))
              ((symbol-function 'lpy-nrepl--file-fields) (lambda () nil))
              ((symbol-function 'lpy-nrepl--send)
               (lambda (_msg &optional callback)
                 (cl-incf sends)
                 (funcall callback
                          `((items . [,(lpy-nrepl-test--item "heapify" 3)
                                      ,(lpy-nrepl-test--item "heappush" 3)]))))))
      (with-temp-buffer
        (insert "hea")
        (let ((table (nth 2 (lpy-nrepl-completion-at-point))))
          (should (equal (all-completions "hea" table) '("heapify" "heappush")))
          (should (equal (try-completion "hea" table) "heap"))
          (should (test-completion "heapify" table))
          (should (= sends 1)))))))

;;; File scope

(require 'lpy-mode)

(ert-deftest lpy-nrepl-test-eval-string-sends-scope-as-ns ()
  (let ((lpy-nrepl--scope "/tmp/a.lpy")
        (lpy-nrepl--repl-buffer-name " *lpy-nrepl-test-evalstr*"))
    (unwind-protect
        (let ((msg (car (lpy-nrepl-test--with-sent nil
                          (lpy-nrepl-eval-string "  (+ 1 2) ")))))
          (should (equal (alist-get 'code msg) "(+ 1 2)"))
          ;; Literal key: the server contract.
          (should (equal (alist-get 'ns msg) "/tmp/a.lpy"))
          (dolist (k '(file line column)) (should-not (assq k msg))))
      (when (get-buffer lpy-nrepl--repl-buffer-name)
        (kill-buffer lpy-nrepl--repl-buffer-name)))))

(ert-deftest lpy-nrepl-test-eval-string-main-scope-sends-no-scope ()
  ;; Even from a file buffer: the string eval is not the buffer's.
  (let ((lpy-nrepl--scope nil)
        (lpy-nrepl--repl-buffer-name " *lpy-nrepl-test-evalstr*"))
    (unwind-protect
        (let ((msg (car (lpy-nrepl-test--with-sent "/tmp/a.lpy"
                          (lpy-nrepl-eval-string "x")))))
          (dolist (k '(ns file line column))
            (should-not (assq k msg))))
      (when (get-buffer lpy-nrepl--repl-buffer-name)
        (kill-buffer lpy-nrepl--repl-buffer-name)))))

(ert-deftest lpy-nrepl-test-eval-string-blank-errors ()
  (lpy-nrepl-test--with-sent nil
    (should-error (lpy-nrepl-eval-string "  ") :type 'user-error)))

(defmacro lpy-nrepl-test--with-scope-buffers (&rest body)
  "Run BODY with an lpy file buffer, a non-lpy file buffer and an lpy
buffer without a file; bind `lpy-file' to the first one's exact path."
  `(let ((lpy-file (expand-file-name "scope-a.lpy" temporary-file-directory))
         (lpy-lib-buf (generate-new-buffer " *lpy-scope-a*"))
         (txt-buf (generate-new-buffer " *lpy-scope-txt*"))
         (anon-buf (generate-new-buffer " *lpy-scope-anon*"))
         (lpy-nrepl--scope nil)
         (lpy-nrepl--repl-buffer-name " *lpy-nrepl-test-scope*"))
     (unwind-protect
         (progn
           (with-current-buffer lpy-lib-buf
             (lpy-mode) (setq buffer-file-name lpy-file))
           (with-current-buffer txt-buf
             (fundamental-mode)
             (setq buffer-file-name (expand-file-name "scope-b.txt"
                                                      temporary-file-directory)))
           (with-current-buffer anon-buf (lpy-mode))
           ,@body)
       (dolist (b (list lpy-lib-buf txt-buf anon-buf))
         (with-current-buffer b (set-buffer-modified-p nil))
         (kill-buffer b))
       (when (get-buffer lpy-nrepl--repl-buffer-name)
         (kill-buffer lpy-nrepl--repl-buffer-name)))))

(ert-deftest lpy-nrepl-test-set-file-scope-picks-exact-path ()
  (lpy-nrepl-test--with-scope-buffers
    (let (offered)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt coll &rest _)
                   (setq offered (mapcar #'car coll))
                   (car (cadr coll)))))
        (lpy-nrepl-set-file-scope))
      (should (equal offered (list "__main__" (abbreviate-file-name lpy-file))))
      (should (equal lpy-nrepl--scope lpy-file)))))

(ert-deftest lpy-nrepl-test-set-file-scope-empty-resets-to-main ()
  (lpy-nrepl-test--with-scope-buffers
    (setq lpy-nrepl--scope lpy-file)
    (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "")))
      (lpy-nrepl-set-file-scope))
    (should-not lpy-nrepl--scope)))

(ert-deftest lpy-nrepl-test-set-file-scope-defaults-to-main ()
  ;; Vertico preselects candidate 0 without a DEF; RET must mean __main__.
  (lpy-nrepl-test--with-scope-buffers
    (let (def)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_p _c _pred _rm _init _hist d) (setq def d) "")))
        (lpy-nrepl-set-file-scope))
      (should (equal def "__main__")))))

(ert-deftest lpy-nrepl-test-reconnect-resets-scope-and-logs ()
  (lpy-nrepl-test--with-servers (srv-a srv-b)
    (let ((lpy-nrepl--scope nil))
      (lpy-nrepl-connect (lpy-nrepl-test--port srv-a))
      (setq lpy-nrepl--scope "/tmp/a.lpy")
      (lpy-nrepl-connect (lpy-nrepl-test--port srv-b))
      (should-not lpy-nrepl--scope)
      (with-current-buffer (lpy-nrepl--repl-buffer)
        (should (string-suffix-p ";; scope: __main__ (connection changed)\n"
                                 (buffer-string)))))))

(ert-deftest lpy-nrepl-test-sentinel-drop-resets-scope-keeps-reason ()
  ;; The scope reset is pinned on the drop path (not only on connect), and its
  ;; announcement must not replace the disconnect reason in the echo area.
  (lpy-nrepl-test--with-servers (srv)
    (let ((lpy-nrepl--scope nil))
      (lpy-nrepl-connect (lpy-nrepl-test--port srv))
      (setq lpy-nrepl--scope "/tmp/a.lpy")
      (setq lpy-nrepl-test--messages nil)
      (should (lpy-nrepl-test--wait (lambda () (lpy-nrepl-test--client srv))))
      (delete-process (lpy-nrepl-test--client srv))
      (should (lpy-nrepl-test--wait (lambda () (null lpy-nrepl--connection))))
      (should-not lpy-nrepl--scope)
      (should (string-match-p "connection lost (connection broken"
                              (car lpy-nrepl-test--messages)))
      (with-current-buffer (lpy-nrepl--repl-buffer)
        (should (string-suffix-p ";; scope: __main__ (connection changed)\n"
                                 (buffer-string)))))))

(ert-deftest lpy-nrepl-test-disconnect-resets-scope ()
  (lpy-nrepl-test--with-servers (srv)
    (let ((lpy-nrepl--scope nil))
      (lpy-nrepl-connect (lpy-nrepl-test--port srv))
      (setq lpy-nrepl--scope "/tmp/a.lpy")
      (setq lpy-nrepl-test--messages nil)
      (lpy-nrepl-disconnect)
      (should-not lpy-nrepl--scope)
      (should (equal (car lpy-nrepl-test--messages)
                     "lpy-nrepl: disconnected")))))

(ert-deftest lpy-nrepl-test-first-connect-keeps-scope ()
  (lpy-nrepl-test--with-servers (srv)
    (let ((lpy-nrepl--scope "/tmp/a.lpy"))
      (lpy-nrepl-connect (lpy-nrepl-test--port srv))
      (should (equal lpy-nrepl--scope "/tmp/a.lpy")))))

(ert-deftest lpy-nrepl-test-mode-line-label-escapes-percent ()
  (let ((lpy-nrepl--scope "/tmp/50%.lpy")
        (lpy-nrepl--repl-buffer-name " *lpy-nrepl-test-pct*"))
    (unwind-protect
        (with-current-buffer (lpy-nrepl--repl-buffer)
          (should (equal (eval (cadr mode-line-process) t) " [50%%.lpy]")))
      (kill-buffer lpy-nrepl--repl-buffer-name))))

(ert-deftest lpy-nrepl-test-scope-shown-in-log-and-mode-line ()
  (lpy-nrepl-test--with-scope-buffers
    (lpy-nrepl--set-scope lpy-file)
    (with-current-buffer (lpy-nrepl--repl-buffer)
      (should (string-match-p (regexp-quote (concat ";; scope: "
                                                    (abbreviate-file-name lpy-file)))
                              (buffer-string)))
      ;; `format-mode-line' returns "" in batch Emacs, so evaluate the
      ;; (:eval FORM) construct directly.
      (should (equal (eval (cadr mode-line-process) t) " [scope-a.lpy]")))
    (lpy-nrepl--set-scope nil)
    (with-current-buffer (lpy-nrepl--repl-buffer)
      (should (equal (eval (cadr mode-line-process) t) " [__main__]")))))

(ert-deftest lpy-nrepl-test-eval-string-result-makes-no-overlay ()
  (lpy-nrepl-test--with-scope-buffers
    (let ((lpy-nrepl--scope lpy-file)
          (callback nil))
      (with-temp-buffer
        (insert "(foo)")
        ;; State left behind by an earlier buffer eval.
        (setq lpy-nrepl--eval-buffer (current-buffer)
              lpy-nrepl--eval-point (point-min))
        (cl-letf (((symbol-function 'lpy-nrepl-connected-p) (lambda () t))
                  ((symbol-function 'lpy-nrepl--send)
                   (lambda (_msg &optional cb) (setq callback cb))))
          (lpy-nrepl-eval-string "(foo)"))
        (let ((inhibit-message t))
          (funcall callback '((value . "3"))))
        (should-not (cl-some (lambda (ov) (overlay-get ov 'lpy-nrepl))
                             (overlays-in (point-min) (point-max))))))))

;;; Merged completion (LSP + nREPL)

(defvar lpy-nrepl-test--calls nil
  "Calls recorded by the stub capfs, newest first.")

(defun lpy-nrepl-test--eglot-capf ()
  "Stub of `eglot-completion-at-point' over the word before point.
Its table matches flexibly, like eglot's: \"theap\" matches \"hea\"."
  (let* ((end (point))
         (beg (save-excursion (skip-chars-backward "a-z") (point)))
         (cands (list (propertize "heapify" 'eglot--lsp-item
                                  '(:label "heapify" :kind 3))
                      (propertize "heap" 'eglot--lsp-item '(:label "heap"))
                      (propertize "theap" 'eglot--lsp-item '(:label "theap")))))
    (list beg end
          (lambda (str _pred action)
            (pcase action
              ('metadata
               `(metadata (category . eglot-capf)
                          (display-sort-function
                           . ,(lambda (cs) (sort (copy-sequence cs) #'string<)))))
              ('t (cl-remove-if-not
                   (lambda (c) (string-match-p (regexp-quote str) c)) cands))
              (_ nil)))
          :annotation-function (lambda (_c) " [sig]")
          :exit-function (lambda (c status)
                           (push (list 'eglot-exit c status) lpy-nrepl-test--calls))
          :company-doc-buffer (lambda (c)
                                (push (list 'eglot-doc c) lpy-nrepl-test--calls)
                                nil)
          :company-prefix-length t)))

(defun lpy-nrepl-test--nrepl-capf ()
  "Stub of `lpy-nrepl-completion-at-point' over the word before point."
  (let* ((end (point))
         (beg (save-excursion (skip-chars-backward "a-z") (point))))
    (list beg end
          (completion-table-dynamic
           (lambda (_) (list "heapify" "heappush" "heappush" "head")))
          :annotation-function (lambda (_c) " fn")
          :company-kind (lambda (c)
                          (push (list 'nrepl-kind c) lpy-nrepl-test--calls)
                          'function)
          :exclusive 'no)))

(defmacro lpy-nrepl-test--with-capfs (eglot nrepl &rest body)
  "Run BODY after \"hea\" with the eglot and nREPL capfs stubbed.
EGLOT and NREPL are forms giving each stub's result."
  (declare (indent 2))
  `(with-temp-buffer
     (insert "x hea")
     (setq lpy-nrepl-test--calls nil)
     (cl-letf (((symbol-function 'eglot-managed-p) (lambda () t))
               ((symbol-function 'eglot-completion-at-point)
                (lambda () ,eglot))
               ((symbol-function 'lpy-nrepl-completion-at-point)
                (lambda () ,nrepl)))
       ,@body)))

(defun lpy-nrepl-test--find (name cands)
  "Return the string in CANDS equal to NAME."
  (cl-find name cands :test #'equal))

(ert-deftest lpy-nrepl-test-merged-capf-dedupes-to-eglot-candidate ()
  ;; Defect: a name both sources offer is listed twice, or the merged list
  ;; keeps nREPL's copy so eglot's docs and exit function never run.
  (lpy-nrepl-test--with-capfs (lpy-nrepl-test--eglot-capf)
      (lpy-nrepl-test--nrepl-capf)
    (pcase-let* ((`(,_ ,_ ,table . ,plist) (lpy-completion-at-point))
                 (cands (funcall table "hea" nil t)))
      (should (= 1 (cl-count "heapify" cands :test #'equal)))
      (should (= 1 (cl-count "heapify" (all-completions "hea" table nil)
                             :test #'equal)))
      (let ((heapify (lpy-nrepl-test--find "heapify" cands)))
        (should (get-text-property 0 'eglot--lsp-item heapify))
        (should (equal (funcall (plist-get plist :annotation-function) heapify)
                       " function"))
        (funcall (plist-get plist :exit-function) heapify 'finished)
        (funcall (plist-get plist :company-doc-buffer) heapify)
        (should (equal lpy-nrepl-test--calls
                       `((eglot-doc ,heapify) (eglot-exit ,heapify finished)))))
      ;; eglot's own keys carry over.
      (should (eq (plist-get plist :company-prefix-length) t)))))

(ert-deftest lpy-nrepl-test-merged-capf-keeps-nrepl-only-names ()
  ;; Defect: names only the nREPL knows are dropped, listed twice, or
  ;; annotated by eglot's function (which knows nothing of them).
  (lpy-nrepl-test--with-capfs (lpy-nrepl-test--eglot-capf)
      (lpy-nrepl-test--nrepl-capf)
    (pcase-let* ((`(,_ ,_ ,table . ,plist) (lpy-completion-at-point))
                 (cands (funcall table "hea" nil t)))
      (should (= 1 (cl-count "heappush" cands :test #'equal)))
      (let ((head (lpy-nrepl-test--find "head" cands)))
        (should head)
        (should (equal (funcall (plist-get plist :annotation-function) head)
                       " fn"))
        (should (eq (funcall (plist-get plist :company-kind) head) 'function)))
      (should (equal lpy-nrepl-test--calls '((nrepl-kind "head"))))
      ;; A property only nREPL provides yields nil for eglot's candidates.
      (should-not (funcall (plist-get plist :company-kind)
                           (lpy-nrepl-test--find "heap" cands))))))

(ert-deftest lpy-nrepl-test-merged-capf-sorts-eglot-first ()
  ;; Defect: the merged sort interleaves the sources, or eglot's own
  ;; ranking is discarded.
  (lpy-nrepl-test--with-capfs (lpy-nrepl-test--eglot-capf)
      (lpy-nrepl-test--nrepl-capf)
    (pcase-let* ((`(,beg ,end ,table . ,_) (lpy-completion-at-point))
                 (meta (completion-metadata
                        (buffer-substring beg end) table nil))
                 (sort-fn (completion-metadata-get meta 'display-sort-function))
                 (cands (funcall table "hea" nil t)))
      (should (eq (completion-metadata-get meta 'category) 'eglot-capf))
      (should (equal (funcall sort-fn (reverse cands))
                     '("heap" "heapify" "theap" "head" "heappush"))))))

(ert-deftest lpy-nrepl-test-merged-capf-keeps-eglot-flex-matches ()
  ;; Defect: the merged table re-filters by prefix, dropping eglot's flex
  ;; (non-prefix) matches.
  (lpy-nrepl-test--with-capfs (lpy-nrepl-test--eglot-capf)
      (lpy-nrepl-test--nrepl-capf)
    (let ((table (nth 2 (lpy-completion-at-point))))
      (should (member "theap" (funcall table "hea" nil t))))))

(defvar lpy-nrepl-test--requests nil
  "Words the fake LSP server was asked to complete, newest first.")

(defvar lpy-nrepl-test--lsp-label "my-var"
  "The one label `lpy-nrepl-test--incomplete-server' answers.")

(defun lpy-nrepl-test--incomplete-server (word)
  "Fake LSP answer for WORD, as lispython marks it isIncomplete.
The label is `lpy-nrepl-test--lsp-label'.  filterText is WORD plus the
rest of the label, spelled with `_' until WORD holds a `-'."
  (push word lpy-nrepl-test--requests)
  (let* ((label lpy-nrepl-test--lsp-label)
         (spelling (if (string-search "-" word)
                       label
                     (string-replace "-" "_" label))))
    (list (propertize label 'eglot--lsp-item
                      `(:label ,label :kind 6
                        :filterText ,(concat word (substring spelling
                                                             (length word))))))))

(defun lpy-nrepl-test--caching-eglot-capf ()
  "Stub of `eglot-completion-at-point' caching its answer per table.
As eglot's `local-cache', the table asks the server once, lazily, for
the word at capf time and matches every later input against that answer."
  (let* ((end (point))
         (beg (save-excursion (skip-chars-backward "A-Za-z_-") (point)))
         (word (buffer-substring-no-properties beg end))
         (cache :none))
    (list beg end
          (lambda (str _pred action)
            (pcase action
              ('metadata '(metadata (category . eglot-capf)))
              ('t (when (eq cache :none)
                    (setq cache (lpy-nrepl-test--incomplete-server word)))
                  (cl-remove-if-not
                   (lambda (c)
                     (string-prefix-p
                      str (plist-get (lpy--eglot-item c) :filterText)))
                   cache))
              (_ nil)))
          :exit-function (lambda (_c _status)
                           (push word lpy-nrepl-test--calls)))))

(ert-deftest lpy-nrepl-test-capf-reasks-incomplete-server-per-input ()
  ;; Defect: the table from the first capf call is kept for the whole
  ;; corfu session, so `my-' is matched against the `my_var' filterText
  ;; answered for `m', no LSP candidate is left and the popup blinks.
  (with-temp-buffer
    (insert "x m")
    (setq lpy-nrepl-test--calls nil
          lpy-nrepl-test--requests nil)
    (cl-letf (((symbol-function 'eglot-managed-p) (lambda () t))
              ((symbol-function 'eglot-completion-at-point)
               #'lpy-nrepl-test--caching-eglot-capf)
              ((symbol-function 'lpy-nrepl-completion-at-point)
               (lambda ()
                 (list (save-excursion (skip-chars-backward "a-z_-") (point))
                       (point)
                       (completion-table-dynamic (lambda (_) '("my-only")))
                       :exclusive 'no))))
      (pcase-let* ((`(,beg ,_ ,table . ,plist) (lpy-completion-at-point)))
        (cl-flet ((corfu-update ()
                    (let ((input (buffer-substring-no-properties beg (point))))
                      (completion-metadata input table nil)
                      (all-completions input table nil))))
          (should (member "my-var" (corfu-update)))
          (dolist (char '("y" "-" "v"))
            (insert char)
            (should (member "my-var" (corfu-update))))
          ;; One request per distinct input; an unchanged input sends none.
          (corfu-update)
          (should (equal lpy-nrepl-test--requests '("my-v" "my-" "my" "m")))
          ;; A candidate listed at `my-v' runs that capf call's exit function.
          (funcall (plist-get plist :exit-function)
                   (lpy-nrepl-test--find "my-var" (corfu-update)) 'finished)
          (should (equal lpy-nrepl-test--calls '("my-v"))))))))

(defun lpy-nrepl-test--text-edit-eglot-capf ()
  "Stub of `eglot-completion-at-point' whose exit applies a textEdit.
Each candidate's edit covers the word as it was at the request.  As
eglot's, the exit function first restores the buffer to its own capf
call's state (ORIG-POS, BOUNDS-STRING), then applies the edit."
  (let* ((orig-pos (point))
         (beg (save-excursion (skip-chars-backward "a-z_-") (point)))
         (bounds-string (buffer-substring-no-properties beg orig-pos))
         (cands (list (propertize "my-var" 'eglot--lsp-item
                                  `(:label "my-var" :kind 6
                                    :textEdit (,beg . ,orig-pos))))))
    (list beg orig-pos
          (lambda (str pred action)
            (if (eq action 'metadata)
                '(metadata (category . eglot-capf))
              (complete-with-action action cands str pred)))
          :exit-function
          (lambda (proxy _status)
            (let* ((item (lpy--eglot-item proxy))
                   (edit (plist-get item :textEdit)))
              (delete-region orig-pos (point))
              (insert (substring bounds-string (- orig-pos beg)))
              (delete-region (car edit) (cdr edit))
              (goto-char (car edit))
              (insert (plist-get item :label)))))))

(ert-deftest lpy-nrepl-test-capf-accept-keeps-listing-generation ()
  ;; Defect: a re-run after the candidate is inserted replaces the exit
  ;; function, which then restores the inserted text and applies the
  ;; listed candidate's edit after it: "x my-varvar".  Its merge also
  ;; forgets which listed names were the nREPL's.
  (with-temp-buffer
    (insert "x m")
    (cl-letf (((symbol-function 'eglot-managed-p) (lambda () t))
              ((symbol-function 'eglot-completion-at-point)
               #'lpy-nrepl-test--text-edit-eglot-capf)
              ((symbol-function 'lpy-nrepl-completion-at-point)
               (lambda ()
                 (list (save-excursion (skip-chars-backward "a-z_-") (point))
                       (point)
                       (completion-table-dynamic (lambda (_) '("my-only")))
                       :annotation-function (lambda (_c) " nrepl")
                       :exclusive 'no))))
      (pcase-let* ((`(,beg ,_ ,table . ,plist) (lpy-completion-at-point)))
        (insert "y-")
        (let* ((input (buffer-substring-no-properties beg (point)))
               (cands (progn (completion-metadata input table nil)
                             (all-completions input table nil)))
               (my-var (lpy-nrepl-test--find "my-var" cands))
               (my-only (lpy-nrepl-test--find "my-only" cands)))
          ;; Accept as corfu does: insert, then query the table again.
          (delete-region beg (point))
          (insert "my-var")
          (all-completions "my-var" table nil)
          (test-completion "my-var" table nil)
          (funcall (plist-get plist :exit-function) my-var 'finished)
          (should (equal (buffer-string) "x my-var"))
          (should (equal (funcall (plist-get plist :annotation-function)
                                  my-only)
                         " nrepl")))))))

(ert-deftest lpy-nrepl-test-capf-late-source-keeps-its-exit-function ()
  ;; Defect: the session starts with the nREPL alone (no exit function)
  ;; and eglot answers on a later re-run, but the returned plist only
  ;; dispatches the first result's keys, so eglot's exit function (its
  ;; textEdit and snippet path) never runs.
  (with-temp-buffer
    (insert "x m")
    (setq lpy-nrepl-test--calls nil)
    (let ((eglot-ready nil))
      (cl-letf (((symbol-function 'eglot-managed-p) (lambda () t))
                ((symbol-function 'eglot-completion-at-point)
                 (lambda ()
                   (when eglot-ready
                     (lpy-nrepl-test--caching-eglot-capf))))
                ((symbol-function 'lpy-nrepl-completion-at-point)
                 (lambda ()
                   (list (save-excursion (skip-chars-backward "a-z_-") (point))
                         (point)
                         (completion-table-dynamic (lambda (_) '("my-only")))
                         :exclusive 'no))))
        (pcase-let* ((`(,beg ,_ ,table . ,plist) (lpy-completion-at-point)))
          (setq eglot-ready t)
          (insert "y")
          (let* ((my-var (lpy-nrepl-test--find
                          "my-var"
                          (all-completions (buffer-substring beg (point))
                                           table nil)))
                 (exit (plist-get plist :exit-function)))
            (should my-var)
            (should exit)
            (funcall exit my-var 'finished)
            (should (equal lpy-nrepl-test--calls '("my")))))))))

(ert-deftest lpy-nrepl-test-merged-capf-single-source-is-unchanged ()
  ;; Defect: with one source missing the merge still wraps the other,
  ;; changing behaviour for users without eglot or without an nREPL.
  (let ((eglot (with-temp-buffer (insert "hea") (lpy-nrepl-test--eglot-capf)))
        (nrepl (with-temp-buffer (insert "hea") (lpy-nrepl-test--nrepl-capf))))
    (lpy-nrepl-test--with-capfs eglot nrepl
      (cl-letf (((symbol-function 'eglot-managed-p) #'ignore))
        (should (eq (lpy--merged-capf) nrepl))))
    (lpy-nrepl-test--with-capfs eglot nil
      ;; eglot alone keeps its table; only the annotations are shortened.
      (let ((result (lpy--merged-capf)))
        (should (eq (nth 2 result) (nth 2 eglot)))
        (should (eq (plist-get (nthcdr 3 result) :exit-function)
                    (plist-get (nthcdr 3 eglot) :exit-function)))))
    (lpy-nrepl-test--with-capfs nil nil
      (should-not (lpy--merged-capf)))))

(defconst lpy-nrepl-test--pyrefly-nlargest
  '(:label "nlargest" :kind 3 :labelDetails nil
    :detail "\n@overload\ndef nlargest[_S](\n    n: int,\n    iterable: Iterable[_S],\n    key: (_S) -> SupportsRichComparison\n) -> list[_S]: ...\ndef nlargest[SupportsRichComparisonT: SupportsRichComparison](\n    n       : int,\n    iterable: Iterable[SupportsRichComparisonT],\n    key     : None = None\n) -> list[SupportsRichComparisonT]: ...")
  "pyrefly 1.4.1's completion item for `heapq.nl', as captured.")

(defun lpy-nrepl-test--eglot-item-capf (item)
  "Stub of `eglot-completion-at-point' offering one candidate carrying ITEM.
eglot's own annotation returns `detail', its doc buffer holds \"DOCS\"."
  (let ((cand (propertize (plist-get item :label) 'eglot--lsp-item item)))
    (list (- (point) 2) (point) (list cand)
          :annotation-function (lambda (c)
                                 (concat " " (plist-get (lpy--eglot-item c)
                                                        :detail)))
          :company-doc-buffer (lambda (_c)
                                (with-current-buffer
                                    (get-buffer-create " *lpy-test eglot doc*")
                                  (erase-buffer)
                                  (insert "Return the n largest elements. DOCS")
                                  (current-buffer)))
          :company-docsig (lambda (_c) "eglot docsig"))))

(defmacro lpy-nrepl-test--with-item (item &rest body)
  "Run BODY with CAND, ANNOTATE, DOC-BUFFER and DOCSIG bound for ITEM.
The capf result is `lpy-completion-at-point''s over an eglot stub."
  (declare (indent 1))
  `(lpy-nrepl-test--with-capfs (lpy-nrepl-test--eglot-item-capf ,item) nil
     (pcase-let* ((`(,_ ,_ ,table . ,plist) (lpy-completion-at-point))
                  (cand (car (all-completions "" table)))
                  (annotate (plist-get plist :annotation-function))
                  (doc-buffer (plist-get plist :company-doc-buffer))
                  (docsig (plist-get plist :company-docsig)))
       (ignore cand annotate doc-buffer docsig)
       ,@body)))

(ert-deftest lpy-nrepl-test-lsp-annotation-is-kind-not-detail ()
  ;; Defect: the item's multi-line `detail' (the whole overload set) is
  ;; shown beside the candidate, as eglot's own annotation does.
  (lpy-nrepl-test--with-item lpy-nrepl-test--pyrefly-nlargest
    (should (equal (funcall annotate cand) " function"))))

(ert-deftest lpy-nrepl-test-lsp-annotation-prefers-label-description ()
  ;; Defect: a server's own short tag (labelDetails.description) is
  ;; replaced by the generic kind tag.
  (lpy-nrepl-test--with-item '(:label "defn" :kind 14 :detail "(defn name args)"
                               :labelDetails (:description "special form"))
    (should (equal (funcall annotate cand) " special form"))))

(ert-deftest lpy-nrepl-test-lsp-signature-goes-to-docs ()
  ;; Defect: once `detail' leaves the list, the signature is shown nowhere:
  ;; the doc popup must carry it above eglot's docs, and the docsig line
  ;; must be one line.
  (lpy-nrepl-test--with-item lpy-nrepl-test--pyrefly-nlargest
    (let ((text (with-current-buffer (funcall doc-buffer cand)
                  (buffer-string)))
          (sig (funcall docsig cand)))
      (should (string-match-p (regexp-quote "def nlargest[_S](") text))
      (should (string-match-p "DOCS" text))
      ;; The decorator line is no part of the one-line signature.
      (should (string-prefix-p "def nlargest[_S](" sig))
      (should-not (string-match-p "\n" sig)))))

(ert-deftest lpy-nrepl-test-lsp-without-detail-keeps-eglot-docs ()
  ;; Defect: an item without a signature loses eglot's own docs or docsig.
  (lpy-nrepl-test--with-item '(:label "nl" :kind 6)
    (should (equal (funcall annotate cand) " variable"))
    (should (equal (with-current-buffer (funcall doc-buffer cand)
                     (buffer-string))
                   "Return the n largest elements. DOCS"))
    (should (equal (funcall docsig cand) "eglot docsig"))))

(ert-deftest lpy-nrepl-test-merged-capf-different-starts-keep-main ()
  ;; Defect: candidates completing different prefixes are mixed.
  (let ((main (list 3 5 '("heap")))
        (extra (list 1 5 '("x.heap"))))
    (should (eq (lpy--merge-capf-results main extra) main))))

(ert-deftest lpy-nrepl-test-lpy-mode-capf-runs-before-eglot ()
  ;; Defect: eglot's capf, added at depth 0 when it manages the buffer,
  ;; runs first and shadows the merged one, so nREPL names never appear.
  (with-temp-buffer
    (lpy-mode)
    (add-hook 'completion-at-point-functions #'eglot-completion-at-point nil t)
    (should (eq (car completion-at-point-functions) #'lpy-completion-at-point))
    (should (memq #'eglot-completion-at-point completion-at-point-functions))))

(defun lpy-nrepl-test--assert-dotted-bounds (result)
  "Assert RESULT's bounds cover only the `nl' after the dot in the buffer."
  (let* ((start (nth 0 result))
         (end (nth 1 result))
         (text (buffer-substring-no-properties start end)))
    (should (= start (1+ (save-excursion (goto-char (point-max))
                                         (search-backward ".")))))
    (should (equal text "nl"))
    (should (member "nlargest" (all-completions text (nth 2 result))))))

(ert-deftest lpy-nrepl-test-capf-dotted-bounds-stub ()
  ;; Defect: eglot takes symbol bounds, and `.' is a symbol character in
  ;; `lpy-mode-syntax-table', so `heapq.nl' becomes the prefix and the
  ;; server's candidates (edit range `nl') never match.
  (require 'lpy-mode)
  (with-temp-buffer
    (lpy-mode)
    (insert "(import heapq)\nheapq.nl")
    (cl-letf (((symbol-function 'eglot-managed-p) (lambda () t))
              ((symbol-function 'eglot-completion-at-point)
               (lambda ()
                 (let ((b (or (bounds-of-thing-at-point 'symbol)
                              (cons (point) (point)))))
                   (list (car b) (cdr b) (list "nlargest" "nsmallest")))))
              ((symbol-function 'lpy-nrepl-completion-at-point) (lambda () nil)))
      (lpy-nrepl-test--assert-dotted-bounds (lpy-completion-at-point))
      ;; The mode table is untouched: sexp motion needs dotted symbols.
      (should (eq (char-syntax ?.) ?_)))))

(ert-deftest lpy-nrepl-test-capf-dotted-bounds-live ()
  ;; Same defect against real eglot and the real lispython server.
  (let ((python (getenv "LPY_TEST_LSP_PYTHON")))
    (skip-unless (and python (file-executable-p python)
                      (zerop (call-process python nil nil nil
                                           "-c" "import lispy.lsp"))))
    (require 'eglot)
    (require 'lpy-mode)
    (let* ((dir (file-name-as-directory (make-temp-file "lpy-lsp-" t)))
           (file (expand-file-name "t.lpy" dir))
           (eglot-server-programs
            `((lpy-mode . (,python "-m" "lispy.lsp"))))
           (project-find-functions
            (list (lambda (_) (cons 'transient dir))))
           (eglot-sync-connect 60)
           (buf nil))
      (with-temp-file file (insert "(import heapq)\nheapq.nl"))
      (unwind-protect
          (progn
            (setq buf (find-file-noselect file))
            (with-current-buffer buf
              (apply #'eglot (eglot--guess-contact))
              (let ((deadline (+ (float-time) 30)))
                (while (and (not (eglot-managed-p))
                            (< (float-time) deadline))
                  (accept-process-output nil 0.1)))
              (should (eglot-managed-p))
              (goto-char (point-max))
              (lpy-nrepl-test--assert-dotted-bounds (lpy-completion-at-point))))
        (when (buffer-live-p buf)
          (with-current-buffer buf
            (ignore-errors (when (eglot-managed-p)
                             (eglot-shutdown (eglot-current-server))))
            (set-buffer-modified-p nil))
          (kill-buffer buf))
        (delete-directory dir t)))))

(defmacro lpy-nrepl-test--with-reply (sent reply &rest body)
  "Run BODY with an nREPL whose `complete' reply is the alist REPLY.
SENT is set to the prefix the client asked for."
  (declare (indent 2))
  `(cl-letf (((symbol-function 'lpy-nrepl-connected-p) (lambda () t))
             ((symbol-function 'lpy-nrepl--file-fields) (lambda () nil))
             ((symbol-function 'lpy-nrepl--send)
              (lambda (msg &optional callback)
                (setq ,sent (alist-get 'prefix msg))
                (funcall callback ,reply))))
     ,@body))

(defun lpy-nrepl-test--captured (json)
  "Parse JSON, a reply captured from the nREPL, as the client does."
  (json-parse-string json :object-type 'alist :null-object nil
                     :false-object nil))

(defconst lpy-nrepl-test--reply-heapq-nl
  "{\"op\": \"complete-result\", \"word\": \"nl\", \"completions\": [\"heapq.nlargest\"], \"items\": [{\"label\": \"nlargest\", \"kind\": 3, \"filterText\": \"nlargest\"}], \"id\": \"6\"}"
  "Captured reply to prefix `heapq.nl'.")

(defconst lpy-nrepl-test--reply-heapq-heap
  "{\"op\": \"complete-result\", \"word\": \"heap\", \"completions\": [\"heapq.heapify\", \"heapq.heapify-max\", \"heapq.heappop\", \"heapq.heappop-max\", \"heapq.heappush\", \"heapq.heappush-max\", \"heapq.heappushpop\", \"heapq.heappushpop-max\", \"heapq.heapreplace\", \"heapq.heapreplace-max\"], \"items\": [{\"label\": \"heapify\", \"kind\": 3, \"filterText\": \"heapify\"}, {\"label\": \"heapify-max\", \"kind\": 3, \"filterText\": \"heapify-max\"}, {\"label\": \"heappop\", \"kind\": 3, \"filterText\": \"heappop\"}, {\"label\": \"heappop-max\", \"kind\": 3, \"filterText\": \"heappop-max\"}, {\"label\": \"heappush\", \"kind\": 3, \"filterText\": \"heappush\"}, {\"label\": \"heappush-max\", \"kind\": 3, \"filterText\": \"heappush-max\"}, {\"label\": \"heappushpop\", \"kind\": 3, \"filterText\": \"heappushpop\"}, {\"label\": \"heappushpop-max\", \"kind\": 3, \"filterText\": \"heappushpop-max\"}, {\"label\": \"heapreplace\", \"kind\": 3, \"filterText\": \"heapreplace\"}, {\"label\": \"heapreplace-max\", \"kind\": 3, \"filterText\": \"heapreplace-max\"}], \"id\": \"0\"}"
  "Captured reply to prefix `heapq.heap'.")

(ert-deftest lpy-nrepl-test-dotted-capf-completes-word-after-dot ()
  ;; Defect: labels are the word after the last dot; filtering them by the
  ;; dotted head drops every one.
  (let (sent)
    (lpy-nrepl-test--with-reply sent
        (lpy-nrepl-test--captured lpy-nrepl-test--reply-heapq-nl)
      (with-temp-buffer
        (insert "(import heapq)\nheapq.nl")
        (let* ((res (lpy-nrepl-completion-at-point))
               (table (nth 2 res))
               (annotate (plist-get (nthcdr 3 res) :annotation-function)))
          (should (= (nth 0 res) (- (point) 2)))
          (should (equal (all-completions "nl" table) '("nlargest")))
          (should (equal (funcall annotate "nlargest") " function"))
          (should (eq (funcall (plist-get (nthcdr 3 res) :company-kind)
                               "nlargest")
                      'function))
          (should (equal sent "heapq.nl")))))))

(ert-deftest lpy-nrepl-test-dotted-capf-empty-word-after-dot ()
  ;; Defect: `heapq.' starts at the whole symbol instead of at point.
  ;; Asserts only START and the sent prefix, which do not depend on the
  ;; reply (the `heapq.nl' capture is a stand-in).
  (let (sent)
    (lpy-nrepl-test--with-reply sent
        (lpy-nrepl-test--captured lpy-nrepl-test--reply-heapq-nl)
      (with-temp-buffer
        (insert "heapq.")
        (let ((res (lpy-nrepl-completion-at-point)))
          (should (= (nth 0 res) (point)))
          (all-completions "" (nth 2 res)) ; triggers the fetch
          (should (equal sent "heapq.")))))))

(ert-deftest lpy-nrepl-test-dotted-merge-keeps-nrepl-names ()
  ;; Defect: stripping the head from bare labels drops them all, so the
  ;; nREPL-only name `heapify-max' is lost in the merge.
  (require 'lpy-mode)
  (let (sent)
    (lpy-nrepl-test--with-reply sent
        (lpy-nrepl-test--captured lpy-nrepl-test--reply-heapq-heap)
      (with-temp-buffer
        (lpy-mode)
        (insert "(import heapq)\nheapq.heap")
        (cl-letf (((symbol-function 'eglot-managed-p) (lambda () t))
                  ((symbol-function 'eglot-completion-at-point)
                   (lambda ()
                     (let ((b (or (bounds-of-thing-at-point 'symbol)
                                  (cons (point) (point)))))
                       (list (car b) (cdr b) (list "heapify"))))))
          (let* ((res (lpy-completion-at-point))
                 (names (all-completions "heap" (nth 2 res))))
            (should (member "heapify" names))
            (should (member "heapify-max" names))))))))

;;; Completion word bounds: lispython's word-boundary spec

(defconst lpy-nrepl-test--word-spec-file
  (expand-file-name "word-boundary.json"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "The spec as `python -m lispy.word_boundary' prints it, committed.")

(defun lpy-nrepl-test--json (text)
  "Parse TEXT, the word-boundary spec's JSON."
  (json-parse-string text :object-type 'alist :array-type 'list))

(defun lpy-nrepl-test--word-spec ()
  "Return the committed word-boundary spec, parsed."
  (lpy-nrepl-test--json
   (with-temp-buffer
     (insert-file-contents lpy-nrepl-test--word-spec-file)
     (buffer-string))))

(ert-deftest lpy-nrepl-test-word-span-reproduces-every-spec-case ()
  ;; Defect: the completion word disagrees with the reader's (a charset or
  ;; peel of the client's own, a Python class read as Emacs's, a lookahead
  ;; mistranslated), so a name gets the wrong prefix or none.  The line
  ;; sits between two others, so taking the buffer, not the line, fails.
  (let ((cases (alist-get 'cases (lpy-nrepl-test--word-spec)))
        (mismatches nil))
    (dolist (case cases)
      (let-alist case
        (with-temp-buffer
          (insert "(prev line)\n")
          (let ((bol (point)))
            (insert .text "\n(next line)")
            (goto-char (+ bol .caret))
            (let ((got (mapcar (lambda (pos) (- pos bol))
                               (lpy--completion-span)))
                  (want (list .start .replaceStart .end)))
              (unless (equal got want)
                (push (format "%S at %d: got %S, want %S" .text .caret got want)
                      mismatches)))))))
    ;; One message, so ERT's print length elides no mismatch.
    (when mismatches
      (ert-fail (format "%d of %d spec cases differ:\n%s"
                        (length mismatches) (length cases)
                        (string-join (nreverse mismatches) "\n"))))))

(ert-deftest lpy-nrepl-test-word-rule-is-the-committed-spec ()
  ;; Defect: `lpy--word-boundary-rule' drifts from the spec file (a field
  ;; edited by hand or a regenerated spec not copied in), so the cases
  ;; pinned and the rule the client runs are different specs.
  (let ((rule (alist-get 'rule (lpy-nrepl-test--word-spec))))
    (should (equal (cl-loop for (field . value) in lpy--word-boundary-rule
                            unless (equal value (alist-get field rule))
                            collect field)
                   nil))
    ;; A spec field the const lacks fails here (`steps' is the span
    ;; algorithm, implemented by `lpy--word-span', not data).
    (should (equal (sort (cl-loop for (field . _) in rule
                                  unless (eq field 'steps) collect field)
                         #'string<)
                   (sort (mapcar #'car lpy--word-boundary-rule) #'string<)))))

;; First Emacs shipping each Unicode version (from Emacs's NEWS files).
(defconst lpy-nrepl-test--unicode-emacs
  '(("13.0" . "27.1") ("14.0" . "28.1") ("15.0" . "29.1") ("15.1" . "30.1")))

(ert-deftest lpy-nrepl-test-declared-emacs-meets-completion-needs ()
  ;; Defect: the header lets Emacs < 30 install the package, where eglot
  ;; ignores the bounds provider and the merge drops a source, and older
  ;; Unicode tables mis-classify the spec's `\\w'/Nd.
  (require 'lisp-mnt)
  (let* ((root (file-name-directory
                (directory-file-name
                 (file-name-directory lpy-nrepl-test--word-spec-file))))
         (requires (with-temp-buffer
                     (insert-file-contents (expand-file-name "lpy-mode.el" root))
                     (car (read-from-string (lm-header "package-requires")))))
         (declared (cadr (assq 'emacs requires)))
         (unicode (alist-get 'unicode
                             (alist-get 'rule (lpy-nrepl-test--word-spec))))
         (unicode-mm (mapconcat #'identity
                                (seq-take (split-string unicode "\\.") 2) "."))
         (unicode-emacs (cdr (assoc unicode-mm lpy-nrepl-test--unicode-emacs))))
    (should declared)
    ;; `bounds-of-thing-at-point-provider-alist' is new in Emacs 30.1.
    (should (version<= "30.1" declared))
    (unless unicode-emacs
      (ert-fail (format "Unicode %s is not in `lpy-nrepl-test--unicode-emacs'; add the Emacs that ships it"
                        unicode)))
    (should (version<= unicode-emacs declared))))

(ert-deftest lpy-nrepl-test-word-spec-file-is-lispythons ()
  ;; Defect: lispython publishes a new word-boundary spec and the committed
  ;; copy (with the rule built from it) is left stale.
  (let ((python (getenv "LPY_TEST_LSP_PYTHON")))
    (skip-unless (and python (file-executable-p python)
                      (zerop (call-process python nil nil nil
                                           "-c" "import lispy.word_boundary"))))
    (let* ((published (lpy-nrepl-test--json
                       (with-temp-buffer
                         (let ((coding-system-for-read 'utf-8))
                           (should (zerop (call-process
                                           python nil '(t nil) nil
                                           "-m" "lispy.word_boundary"))))
                         (buffer-string))))
           (committed (lpy-nrepl-test--word-spec)))
      (should (equal (cl-loop for (key . value) in published
                              unless (equal value (alist-get key committed))
                              collect key)
                     nil))
      (should (equal (cl-set-difference
                      (alist-get 'cases published) (alist-get 'cases committed)
                      :test #'equal)
                     nil))
      (should (equal published committed)))))

(defun lpy-nrepl-test--item (label kind &optional description filter-text)
  "Return a parsed `items' entry, as `json-parse-string' yields it.
FILTER-TEXT defaults to LABEL, the server's filterText for a word typed
in the label's own spelling."
  `((label . ,label) (kind . ,kind) (filterText . ,(or filter-text label))
    ,@(when description `((labelDetails . ((description . ,description)))))))

(ert-deftest lpy-nrepl-test-capf-arrow-name-start ()
  ;; Defect: a hand-written word charset stops at `>', so `->f' sends `f'
  ;; and starts at `f', where eglot's symbol bounds start at `-' and the
  ;; merge drops the nREPL source.
  (let (sent)
    (lpy-nrepl-test--with-reply sent
        `((items . [,(lpy-nrepl-test--item "->foo" 3)]))
      (with-temp-buffer
        (insert "(->f")
        (let ((res (lpy-nrepl-completion-at-point)))
          (should (= (nth 0 res) (- (point) 3)))
          (should (equal (all-completions "->f" (nth 2 res)) '("->foo")))
          (should (equal sent "->f")))))))

(ert-deftest lpy-nrepl-test-arrow-merge-keeps-nrepl-names ()
  ;; Defect: the nREPL source starts at `f' while eglot starts at `-', so
  ;; the merge drops the nREPL-only name `->foo'.
  (require 'lpy-mode)
  (let (sent)
    (lpy-nrepl-test--with-reply sent
        `((items . [,(lpy-nrepl-test--item "->foo" 3)]))
      (with-temp-buffer
        (lpy-mode)
        (insert "(->f")
        (cl-letf (((symbol-function 'eglot-managed-p) (lambda () t))
                  ((symbol-function 'eglot-completion-at-point)
                   (lambda ()
                     (let ((b (or (bounds-of-thing-at-point 'symbol)
                                  (cons (point) (point)))))
                       (list (car b) (cdr b) (list "->fmt"))))))
          (let* ((res (lpy-completion-at-point))
                 (names (all-completions "->f" (nth 2 res))))
            (should (member "->fmt" names))
            (should (member "->foo" names))))))))

(ert-deftest lpy-nrepl-test-capf-question-mark-name ()
  ;; Defect: `?' is outside the word charset (and punctuation in the
  ;; temp buffer's own table), so after `ok?' the prefix is empty and the
  ;; capf offers nothing.
  (let (sent)
    (lpy-nrepl-test--with-reply sent
        `((items . [,(lpy-nrepl-test--item "ok?" 3)]))
      (with-temp-buffer
        (insert "(if ok?")
        (let ((res (lpy-nrepl-completion-at-point)))
          (should res)
          (should (= (nth 0 res) (- (point) 3)))
          (all-completions "" (nth 2 res)) ; triggers the fetch
          (should (equal sent "ok?")))))))

(defun lpy-nrepl-test--sent-and-offset (text)
  "Complete after TEXT in a temp buffer; return (SENT . POINT-MINUS-START)."
  (let (sent)
    (lpy-nrepl-test--with-reply sent
        `((items . [,(lpy-nrepl-test--item "bar" 3)]))
      (with-temp-buffer
        (insert text)
        (let ((res (lpy-nrepl-completion-at-point)))
          (all-completions "" (nth 2 res)) ; triggers the fetch
          (cons sent (- (point) (nth 0 res))))))))

(ert-deftest lpy-nrepl-test-capf-comma-is-whitespace ()
  ;; Defect: `,' is a symbol character in the mode table but whitespace to
  ;; the reader, so `,ba' sent `,ba' and matched no name.
  (should (equal (lpy-nrepl-test--sent-and-offset "(f ,ba") '("ba" . 2)))
  (should (equal (lpy-nrepl-test--sent-and-offset "(, a,ba") '("ba" . 2))))

(ert-deftest lpy-nrepl-test-capf-reader-prefixes ()
  ;; Defect: the reader peels `^' (annotation) and a sign before a name,
  ;; so sending them finds nothing; `#' and `->' are part of the name.
  (should (equal (lpy-nrepl-test--sent-and-offset "(f ^in") '("in" . 2)))
  (should (equal (lpy-nrepl-test--sent-and-offset "(f -ba") '("ba" . 2)))
  (should (equal (lpy-nrepl-test--sent-and-offset "(f #ba") '("#ba" . 3)))
  (should (equal (lpy-nrepl-test--sent-and-offset "(f ->ba") '("->ba" . 4)))
  (should (equal (lpy-nrepl-test--sent-and-offset "(f :ke") '("ke" . 2)))
  (should (equal (lpy-nrepl-test--sent-and-offset "(f *ar") '("ar" . 2)))
  (should (equal (lpy-nrepl-test--sent-and-offset "(f **kw") '("kw" . 2)))
  ;; An augmented-assignment operator is read whole, never peeled.
  (should (equal (lpy-nrepl-test--sent-and-offset "(**=") '("**=" . 3)))
  ;; `**' peels as one prefix, leaving the operator `*='.
  (should (equal (lpy-nrepl-test--sent-and-offset "(f ***=") '("*=" . 2))))

(ert-deftest lpy-nrepl-test-comma-merge-keeps-nrepl-names ()
  ;; Defect: eglot's symbol bounds start at `,' while the nREPL source
  ;; starts after it, so the merge drops the nREPL-only name.
  (require 'thingatpt)
  (require 'lpy-mode)
  (let (sent)
    (lpy-nrepl-test--with-reply sent
        `((items . [,(lpy-nrepl-test--item "bar" 3)]))
      (with-temp-buffer
        (lpy-mode)
        (insert "(f ,ba")
        (cl-letf (((symbol-function 'eglot-managed-p) (lambda () t))
                  ((symbol-function 'eglot-completion-at-point)
                   (lambda ()
                     (let ((b (or (bounds-of-thing-at-point 'symbol)
                                  (cons (point) (point)))))
                       (list (car b) (cdr b) (list "baz"))))))
          (let* ((res (lpy-completion-at-point))
                 (names (all-completions "ba" (nth 2 res))))
            (should (= (nth 0 res) (- (point) 2)))
            (should (member "baz" names))
            (should (member "bar" names))))))))

(defun lpy-nrepl-test--annotations (text &rest names)
  "Complete TEXT in a temp buffer; return (ANNOTATION . COMPANY-KIND) per NAMES."
  (with-temp-buffer
    (insert text)
    (let* ((res (lpy-nrepl-completion-at-point))
           (props (nthcdr 3 res)))
      (all-completions "" (nth 2 res)) ; the fetch happens on the first table call
      (mapcar (lambda (n)
                (cons (funcall (plist-get props :annotation-function) n)
                      (funcall (plist-get props :company-kind) n)))
              names))))

(ert-deftest lpy-nrepl-test-items-annotate-and-kind ()
  (let (sent)
    (lpy-nrepl-test--with-reply sent
        `((items . [,(lpy-nrepl-test--item "defmacro" 14 "special form")
                    ,(lpy-nrepl-test--item "defn" 3 "macro")
                    ,(lpy-nrepl-test--item "deque" 7)]))
      (should (equal (lpy-nrepl-test--annotations "de" "defmacro" "defn" "deque")
                     '((" special form" . keyword)
                       (" macro" . function)
                       (" class" . class)))))))

(ert-deftest lpy-nrepl-test-items-dotted-lookup-by-label ()
  ;; Pins that every captured label, hyphen spellings like `heapify-max'
  ;; included, comes through unfiltered, with its kind.
  (let (sent)
    (lpy-nrepl-test--with-reply sent
        (lpy-nrepl-test--captured lpy-nrepl-test--reply-heapq-heap)
      (with-temp-buffer
        (insert "heapq.heap")
        (let* ((res (lpy-nrepl-completion-at-point))
               (props (nthcdr 3 res)))
          (should (= (length (all-completions "heap" (nth 2 res))) 10))
          (should (equal (funcall (plist-get props :annotation-function)
                                  "heapify-max")
                         " function"))
          (should (eq (funcall (plist-get props :company-kind) "heapify-max")
                      'function))
          (should (equal sent "heapq.heap")))))))

(ert-deftest lpy-nrepl-test-items-empty-vector-is-present ()
  ;; Defect: an empty `items' reply lists candidates from another field.
  (let (sent)
    (lpy-nrepl-test--with-reply sent
        '((items . []) (completions . ["foo"]))
      (with-temp-buffer
        (insert "fo")
        (should-not (all-completions "fo" (nth 2 (lpy-nrepl-completion-at-point))))))))

(ert-deftest lpy-nrepl-test-items-empty-description-uses-kind-word ()
  ;; Defect: a description of "" shown as a blank annotation instead of
  ;; the kind word.
  (let (sent)
    (lpy-nrepl-test--with-reply sent
        `((items . [,(lpy-nrepl-test--item "foo" 7 "")]))
      (should (equal (lpy-nrepl-test--annotations "fo" "foo")
                     '((" class" . class)))))))

;;; Server-matched (filterText) completion

(defconst lpy-nrepl-test--word-replies
  '(
    ("m"
     . "{\"op\": \"complete-result\", \"word\": \"m\", \"completions\": [\"MemoryError\", \"ModuleNotFoundError\", \"MyClass\", \"map\", \"match\", \"max\", \"memoryview\", \"min\", \"my-fn\", \"my-var\"], \"items\": [{\"label\": \"MemoryError\", \"kind\": 7, \"filterText\": \"memoryError\"}, {\"label\": \"ModuleNotFoundError\", \"kind\": 7, \"filterText\": \"moduleNotFoundError\"}, {\"label\": \"MyClass\", \"kind\": 7, \"filterText\": \"myClass\"}, {\"label\": \"map\", \"kind\": 7, \"filterText\": \"map\"}, {\"label\": \"match\", \"kind\": 14, \"filterText\": \"match\", \"labelDetails\": {\"description\": \"special form\"}}, {\"label\": \"max\", \"kind\": 3, \"filterText\": \"max\"}, {\"label\": \"memoryview\", \"kind\": 7, \"filterText\": \"memoryview\"}, {\"label\": \"min\", \"kind\": 3, \"filterText\": \"min\"}, {\"label\": \"my-fn\", \"kind\": 3, \"filterText\": \"my-fn\"}, {\"label\": \"my-var\", \"kind\": 6, \"filterText\": \"my-var\"}], \"id\": \"0\"}")
    ("my"
     . "{\"op\": \"complete-result\", \"word\": \"my\", \"completions\": [\"MyClass\", \"my-fn\", \"my-var\"], \"items\": [{\"label\": \"MyClass\", \"kind\": 7, \"filterText\": \"myClass\"}, {\"label\": \"my-fn\", \"kind\": 3, \"filterText\": \"my-fn\"}, {\"label\": \"my-var\", \"kind\": 6, \"filterText\": \"my-var\"}], \"id\": \"1\"}")
    ("my-"
     . "{\"op\": \"complete-result\", \"word\": \"my-\", \"completions\": [\"my-fn\", \"my-var\"], \"items\": [{\"label\": \"my-fn\", \"kind\": 3, \"filterText\": \"my-fn\"}, {\"label\": \"my-var\", \"kind\": 6, \"filterText\": \"my-var\"}], \"id\": \"2\"}")
    ("my-v"
     . "{\"op\": \"complete-result\", \"word\": \"my-v\", \"completions\": [\"my-var\"], \"items\": [{\"label\": \"my-var\", \"kind\": 6, \"filterText\": \"my-var\"}], \"id\": \"3\"}")
    ("my_v"
     . "{\"op\": \"complete-result\", \"word\": \"my_v\", \"completions\": [\"my-var\"], \"items\": [{\"label\": \"my-var\", \"kind\": 6, \"filterText\": \"my_var\"}], \"id\": \"4\"}")
    ("MY_v"
     . "{\"op\": \"complete-result\", \"word\": \"MY_v\", \"completions\": [\"my-var\"], \"items\": [{\"label\": \"my-var\", \"kind\": 6, \"filterText\": \"MY_var\"}], \"id\": \"5\"}")
    ("heapq.heappu"
     . "{\"op\": \"complete-result\", \"word\": \"heappu\", \"completions\": [\"heapq.heappush\", \"heapq.heappush-max\", \"heapq.heappushpop\", \"heapq.heappushpop-max\"], \"items\": [{\"label\": \"heappush\", \"kind\": 3, \"filterText\": \"heappush\"}, {\"label\": \"heappush-max\", \"kind\": 3, \"filterText\": \"heappush-max\"}, {\"label\": \"heappushpop\", \"kind\": 3, \"filterText\": \"heappushpop\"}, {\"label\": \"heappushpop-max\", \"kind\": 3, \"filterText\": \"heappushpop-max\"}], \"id\": \"0\"}")
    ("my-var"
     . "{\"op\": \"complete-result\", \"word\": \"my-var\", \"completions\": [\"my-var\"], \"items\": [{\"label\": \"my-var\", \"kind\": 6, \"filterText\": \"my-var\"}], \"id\": \"0\"}"))
  "Replies captured from lispython's nREPL, keyed by the prefix asked.")

(defmacro lpy-nrepl-test--with-captured-nrepl (sent &rest body)
  "Run BODY with an nREPL answering each prefix with its captured reply.
SENT collects the prefixes asked, newest first.  A prefix without a
captured reply is an error."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'lpy-nrepl-connected-p) (lambda () t))
             ((symbol-function 'lpy-nrepl--file-fields) (lambda () nil))
             ((symbol-function 'lpy-nrepl--send)
              (lambda (msg &optional callback)
                (let* ((prefix (alist-get 'prefix msg))
                       (json (or (cdr (assoc prefix lpy-nrepl-test--word-replies))
                                 (error "No captured reply to %S" prefix))))
                  (push prefix ,sent)
                  (funcall callback (lpy-nrepl-test--captured json))))))
     ,@body))

(defun lpy-nrepl-test--type-word (beg word)
  "Turn the text from BEG to point into WORD, as typing it would."
  (let ((typed (buffer-substring-no-properties beg (point))))
    (if (string-prefix-p typed word)
        (insert (substring word (length typed)))
      (delete-region beg (point))
      (insert word))))

(defmacro lpy-nrepl-test--with-user-style (&rest body)
  "Run BODY with `substring' as the user's style and no category override.
`substring' re-filters the labels by the input (`my_v' is no substring
of `my-var'), so a server match in another spelling survives only
through the style the table's category brings."
  `(let ((completion-styles '(substring))
         (completion-category-overrides nil))
     ,@body))

(defun lpy-nrepl-test--corfu-candidates (beg table)
  "List TABLE's candidates for the text from BEG to point, as corfu does.
The user's style is `lpy-nrepl-test--with-user-style''s."
  (lpy-nrepl-test--with-user-style
   (let* ((input (buffer-substring-no-properties beg (point)))
          (metadata (completion-metadata input table nil))
          (cands (completion-all-completions input table nil (length input)
                                             metadata)))
     ;; The last cdr is the base size.
     (when cands (setcdr (last cands) nil))
     cands)))

(defun lpy-nrepl-test--style-try (beg table)
  "Return `completion-try-completion' of the text from BEG to point in TABLE.
The user's style is `lpy-nrepl-test--with-user-style''s."
  (lpy-nrepl-test--with-user-style
   (let ((input (buffer-substring-no-properties beg (point))))
     (completion-try-completion input table nil (length input)
                                (completion-metadata input table nil)))))

(defun lpy-nrepl-test--capf-wrapper-result ()
  "Return what `completion-at-point' gets from `lpy-completion-at-point'.
`completion--capf-wrapper' drops a non-exclusive result whose
`try-completion' of the text before point is nil."
  (let ((completion-at-point-functions '(lpy-completion-at-point)))
    (cdr (run-hook-wrapped 'completion-at-point-functions
                           #'completion--capf-wrapper 'all))))

(defconst lpy-nrepl-test--typed-words '("m" "my" "my-" "my-v" "my_v" "MY_v")
  "Keystrokes of a corfu session, ending in mixed spellings of `my-var'.")

(ert-deftest lpy-nrepl-test-capf-never-blinks-nrepl-alone ()
  ;; Defect: the nREPL table (or the user's style) filters the labels by
  ;; the input, so the server's matches for another spelling (`my_v' for
  ;; `my-var', `m' for `MyClass') are dropped and the list empties.
  (let ((sent nil))
    (lpy-nrepl-test--with-captured-nrepl sent
      (with-temp-buffer
        (lpy-mode)
        (insert "(f " (car lpy-nrepl-test--typed-words))
        (pcase-let ((`(,beg ,_ ,table . ,_) (lpy-completion-at-point)))
          (should (member "MyClass" (lpy-nrepl-test--corfu-candidates beg table)))
          (dolist (word lpy-nrepl-test--typed-words)
            (lpy-nrepl-test--type-word beg word)
            (should (member "my-var" (lpy-nrepl-test--corfu-candidates beg table)))
            (lpy-nrepl-test--corfu-candidates beg table))
          ;; One request per word; an unchanged word asks nothing.
          (should (equal (reverse sent) lpy-nrepl-test--typed-words)))))))

(ert-deftest lpy-nrepl-test-capf-mixed-spelling-survives-capf-wrapper ()
  ;; Defect: try-completion matches labels by prefix, so at `my_v' it is
  ;; nil and `completion--capf-wrapper' (default completion, company)
  ;; drops the non-exclusive nREPL source; or the user's style re-filters
  ;; the labels and TAB finds nothing.
  (let ((sent nil))
    (lpy-nrepl-test--with-captured-nrepl sent
      (with-temp-buffer
        (lpy-mode)
        (insert "(f my_v")
        (let ((result (lpy-nrepl-test--capf-wrapper-result)))
          (should result)
          (should (member "my-var" (lpy-nrepl-test--corfu-candidates
                                    (nth 0 result) (nth 2 result))))
          (should (equal (lpy-nrepl-test--style-try (nth 0 result) (nth 2 result))
                         '("my-var" . 6)))
          ;; Accepting inserts the label, which then tests as complete.
          (should (test-completion "my-var" (nth 2 result))))))))

(ert-deftest lpy-nrepl-test-capf-try-inserts-shared-start ()
  ;; Defect: the style's try leaves the input as typed whenever several
  ;; candidates match, so TAB at `heappu' never inserts the `heappush'
  ;; every candidate shares; a mixed spelling (`m' for `MyClass' and
  ;; `my-var') must still stay as typed.
  (let ((sent nil))
    (lpy-nrepl-test--with-captured-nrepl sent
      (dolist (case '(("heapq.heappu" "heappush" . 8) ("m" "m" . 1)))
        (with-temp-buffer
          (lpy-mode)
          (insert "(f " (car case))
          (pcase-let ((`(,beg ,_ ,table . ,_) (lpy-completion-at-point)))
            (should (equal (lpy-nrepl-test--style-try beg table)
                           (cdr case)))))))))

(ert-deftest lpy-nrepl-test-capf-try-mid-word-replaces-word ()
  ;; Defect: mid-word the field already holds the rest of the word, and
  ;; the style's try appends it again (`(f my-v|ar)' TAB gives
  ;; `my-varar'), or calls `my-var|x' complete as if `x' were not there.
  (let ((sent nil))
    (lpy-nrepl-test--with-captured-nrepl sent
      ;; Text before point, text after it, the style's try of the field.
      (dolist (case '(("my-v" "ar" "my-var" . 6)
                      ("my-var" "x" "my-var" . 6)
                      ;; No progress: the field stays as it is.
                      ("m" "x" "mx" . 1)))
        (with-temp-buffer
          (lpy-mode)
          (insert "(f " (nth 0 case) (nth 1 case) ")")
          (goto-char (- (point-max) (length (nth 1 case)) 1))
          (pcase-let* ((`(,beg ,end ,table . ,_) (lpy-completion-at-point))
                       (field (buffer-substring-no-properties beg end)))
            (should (equal (lpy-nrepl-test--with-user-style
                            (completion-try-completion
                             field table nil (- (point) beg)
                             (completion-metadata field table nil)))
                           (nthcdr 2 case)))))))
    ;; Default TAB replaces the whole word with the label.
    (lpy-nrepl-test--with-captured-nrepl sent
      (with-temp-buffer
        (lpy-mode)
        (insert "(f my-var)")
        (goto-char (- (point-max) 3))
        (let ((completion-at-point-functions '(lpy-completion-at-point))
              (inhibit-message t)
              (minibuffer-message-timeout 0))
          (lpy-nrepl-test--with-user-style (completion-at-point)))
        (should (equal (buffer-string) "(f my-var)"))
        (should (= (point) (1- (point-max))))))))

(ert-deftest lpy-nrepl-test-capf-empty-reply-tries-nil ()
  ;; Defect: an empty answer still "completes" (the pass-through style
  ;; returns the input), so the non-exclusive nREPL source shadows later
  ;; capfs (cape-dabbrev) and TAB never says "No match".
  (let (sent)
    (lpy-nrepl-test--with-reply sent '((items . []))
      (with-temp-buffer
        (lpy-mode)
        (insert "(f zq")
        (pcase-let ((`(,beg ,_ ,table . ,_) (lpy-nrepl-completion-at-point)))
          (should-not (try-completion "zq" table))
          (should-not (lpy-nrepl-test--style-try beg table)))
        (should-not (lpy-nrepl-test--capf-wrapper-result))))))

(ert-deftest lpy-nrepl-test-capf-never-blinks-merged ()
  ;; Defect: the merge lists the nREPL's candidates by label prefix, so
  ;; at `my_v' the nREPL-only `my-var' leaves the list.
  (skip-unless (require 'eglot nil t)) ; its `eglot-capf' style
  (let ((sent nil)
        (lpy-nrepl-test--lsp-label "my-value")
        (lpy-nrepl-test--requests nil))
    (lpy-nrepl-test--with-captured-nrepl sent
      (with-temp-buffer
        (lpy-mode)
        (insert "(f " (car lpy-nrepl-test--typed-words))
        (cl-letf (((symbol-function 'eglot-managed-p) (lambda () t))
                  ((symbol-function 'eglot-completion-at-point)
                   #'lpy-nrepl-test--caching-eglot-capf))
          (pcase-let ((`(,beg ,_ ,table . ,_) (lpy-completion-at-point)))
            (dolist (word lpy-nrepl-test--typed-words)
              (lpy-nrepl-test--type-word beg word)
              (let ((cands (lpy-nrepl-test--corfu-candidates beg table)))
                (should (member "my-var" cands))
                (should (member "my-value" cands)))
              (lpy-nrepl-test--corfu-candidates beg table))
            (should (equal (reverse sent) lpy-nrepl-test--typed-words))
            (should (equal (reverse lpy-nrepl-test--requests)
                           lpy-nrepl-test--typed-words))))))))

(ert-deftest lpy-nrepl-test-merged-capf-owner-survives-later-query ()
  ;; Defect: a merge that records the nREPL-only names per table (the
  ;; tempting replacement for the `lpy--extra-only' property) rebuilds the
  ;; record on each query, so after corfu's `test-completion' of the
  ;; inserted text a candidate from the earlier listing is dispatched to
  ;; eglot: eglot's annotation and no kind.
  (lpy-nrepl-test--with-capfs (lpy-nrepl-test--eglot-capf)
      (lpy-nrepl-test--nrepl-capf)
    (pcase-let* ((`(,_ ,_ ,table . ,plist) (lpy-completion-at-point))
                 (head (lpy-nrepl-test--find "head" (all-completions "hea" table))))
      (should head)
      (should (test-completion "heapify" table))
      (should (equal (funcall (plist-get plist :annotation-function) head)
                     " fn"))
      (should (eq (funcall (plist-get plist :company-kind) head) 'function)))))

(ert-deftest lpy-nrepl-test-capf-late-eglot-joins-mid-word ()
  ;; Defect: mid-word the nREPL source ends at point and eglot at the
  ;; symbol end, so in a session the nREPL started alone every merged
  ;; re-run has other bounds and is dropped: eglot never joins.
  (require 'thingatpt)
  (let ((sent nil)
        (eglot-ready nil))
    (lpy-nrepl-test--with-captured-nrepl sent
      (with-temp-buffer
        (lpy-mode)
        (insert "(f mx)")
        (goto-char (- (point-max) 2))
        (cl-letf (((symbol-function 'eglot-managed-p) (lambda () t))
                  ((symbol-function 'eglot-completion-at-point)
                   (lambda ()
                     (when eglot-ready
                       (let ((b (or (bounds-of-thing-at-point 'symbol)
                                    (cons (point) (point)))))
                         (list (car b) (cdr b) (list "my-lsp-name")))))))
          (pcase-let ((`(,beg ,_ ,table . ,_) (lpy-completion-at-point)))
            (should (member "my-var" (all-completions "m" table nil)))
            (setq eglot-ready t)
            (insert "y")
            (let ((cands (all-completions
                          (buffer-substring-no-properties beg (point)) table nil)))
              (should (member "my-lsp-name" cands))
              (should (member "my-var" cands)))
            ;; The prefix asked stays the text before point.
            (should (equal sent '("my" "m")))))))))

;;; Busy server: completion, streaming, interrupt, death and restart

(defun lpy-nrepl-test--table-calls (table)
  "Make the three table calls corfu makes for \"hea\" in TABLE.
Return the seconds they took."
  (let ((start (float-time)))
    (all-completions "hea" table)
    (try-completion "hea" table)
    (test-completion "hea" table)
    (- (float-time) start)))

(ert-deftest lpy-nrepl-test-capf-unanswered-asks-once ()
  ;; Defect: an unanswered `complete' is re-sent and re-waited for on every
  ;; table call, so one keystroke against a busy server stalls Emacs for a
  ;; second per call, from corfu's timer where C-g cannot break it.
  (lpy-nrepl-test--with-servers (srv)
    (lpy-nrepl-connect (lpy-nrepl-test--port srv))
    (with-temp-buffer
      (insert "hea")
      (let* ((lpy-nrepl-request-timeout 0.3)
             (elapsed (lpy-nrepl-test--table-calls
                       (nth 2 (lpy-nrepl-completion-at-point)))))
        (should (< elapsed 1.5))
        (lpy-nrepl-test--wait #'ignore 0.2)
        (should (= (length (lpy-nrepl-test--requests srv "complete")) 1))))))

(ert-deftest lpy-nrepl-test-capf-during-eval-does-not-wait ()
  ;; Defect: completion waits for a server that answers in order while an
  ;; eval runs, so every keystroke during the eval stalls.
  (lpy-nrepl-test--with-servers (srv)
    (lpy-nrepl-connect (lpy-nrepl-test--port srv))
    (lpy-nrepl-eval-string "(while True (pass))")
    (with-temp-buffer
      (insert "hea")
      (let ((elapsed (lpy-nrepl-test--table-calls
                      (nth 2 (lpy-nrepl-completion-at-point)))))
        (should (< elapsed 0.3))
        ;; The eval was still unanswered throughout.
        (should lpy-nrepl--in-flight-evals)
        (lpy-nrepl-test--wait #'ignore 0.2)
        (should (<= (length (lpy-nrepl-test--requests srv "complete")) 1))))))

(ert-deftest lpy-nrepl-test-capf-during-eval-waits-on-concurrent-server ()
  ;; Defect: a server that answers completion while an eval runs (a
  ;; streaming lispython) still gets no wait, so no candidate ever shows
  ;; during a long eval.
  (lpy-nrepl-test--with-servers (srv)
    (lpy-nrepl-connect (lpy-nrepl-test--port srv))
    (lpy-nrepl-eval-string "(while True (pass))")
    (let ((answer-next
           (lambda ()
             (let ((req (car (last (lpy-nrepl-test--requests srv "complete")))))
               (lpy-nrepl-test--answer
                srv `((id . ,(alist-get 'id req)) (op . "complete-result")
                      (items . [((label . "heapify") (kind . 3))])))))))
      (with-temp-buffer
        (insert "hea")
        ;; The first ask, unanswered in time, teaches nothing yet.
        (all-completions "hea" (nth 2 (lpy-nrepl-completion-at-point)))
        (should (lpy-nrepl-test--wait
                 (lambda () (lpy-nrepl-test--requests srv "complete"))))
        (funcall answer-next)
        (should (lpy-nrepl-test--wait (lambda () lpy-nrepl--answers-during-eval)))
        ;; A wide bound: the answer comes from a timer, late on a loaded box.
        (let ((table (nth 2 (lpy-nrepl-completion-at-point)))
              (lpy-nrepl-request-timeout 2))
          (run-at-time 0.1 nil answer-next)
          (should (equal (all-completions "hea" table) '("heapify"))))))))

(ert-deftest lpy-nrepl-test-evals-request-streaming ()
  ;; Defect: an eval or load-file sent without the stream flag gets no
  ;; interim output, so a long eval shows nothing until it ends.
  (lpy-nrepl-test--with-servers (srv)
    (lpy-nrepl-connect (lpy-nrepl-test--port srv))
    (let ((file (make-temp-file "lpy-nrepl-stream-" nil ".lpy")))
      (unwind-protect
          (with-current-buffer (find-file-noselect file)
            (unwind-protect
                (progn
                  (lpy-nrepl-eval-string "(+ 1 2)")
                  (lpy-nrepl-eval-buffer))
              (set-buffer-modified-p nil)
              (kill-buffer)))
        (delete-file file)))
    (should (lpy-nrepl-test--wait
             (lambda () (lpy-nrepl-test--requests srv "load-file"))))
    (dolist (op '("eval" "load-file"))
      (let ((reqs (lpy-nrepl-test--requests srv op)))
        (should (= (length reqs) 1))
        (should (eq (alist-get 'stream (car reqs)) t))))))

(ert-deftest lpy-nrepl-test-out-messages-stream-before-final-reply ()
  ;; Defect: the first interim `out' message is taken for the final reply:
  ;; it consumes the callback, the output is logged as raw JSON, and the
  ;; real result is then dropped as unsolicited.
  (lpy-nrepl-test--with-servers (srv)
    (lpy-nrepl-connect (lpy-nrepl-test--port srv))
    (let* ((finals 0)
           (id (lpy-nrepl--send
                '((op . "eval") (code . "(loop)"))
                (lambda (response)
                  (cl-incf finals)
                  (let ((lpy-nrepl--eval-buffer nil))
                    (lpy-nrepl--display-eval-result response))))))
      (should (lpy-nrepl-test--wait (lambda () (lpy-nrepl-test--requests srv))))
      (lpy-nrepl-test--answer srv `((id . ,id) (op . "out") (stdout . "tick\n")))
      (lpy-nrepl-test--answer srv `((id . ,id) (op . "out") (stdout . "tock")))
      (should (lpy-nrepl-test--wait
               (lambda () (string-match-p "tick\ntock\\'" (lpy-nrepl-test--log-text)))))
      (should (= finals 0))
      (should (equal lpy-nrepl--in-flight-evals (list id)))
      (should-not (string-match-p "\"out\"" (lpy-nrepl-test--log-text)))
      (lpy-nrepl-test--answer srv `((id . ,id) (op . "eval-result") (value . "None")
                                    (stdout . "")))
      (should (lpy-nrepl-test--wait (lambda () (= finals 1))))
      (should-not lpy-nrepl--in-flight-evals)
      ;; The value starts its own line after an unterminated chunk.
      (should (string-suffix-p "tick\ntock\nNone\n" (lpy-nrepl-test--log-text))))))

(ert-deftest lpy-nrepl-test-final-reply-output-still-shown ()
  ;; Defect: with output expected as interim messages, a server without
  ;; streaming (all output in the final reply) shows none of it.
  (lpy-nrepl-test--with-servers (srv)
    (lpy-nrepl-connect (lpy-nrepl-test--port srv))
    (lpy-nrepl-eval-string "(do (print \"a\") (print \"b\") 3)")
    (should (lpy-nrepl-test--wait (lambda () (lpy-nrepl-test--requests srv "eval"))))
    (lpy-nrepl-test--answer
     srv `((id . ,(alist-get 'id (car (lpy-nrepl-test--requests srv "eval"))))
           (op . "eval-result") (value . "3") (stdout . "a\nb\n")))
    (should (lpy-nrepl-test--wait
             (lambda () (string-suffix-p "a\nb\n3\n" (lpy-nrepl-test--log-text)))))))

(ert-deftest lpy-nrepl-test-interrupt-names-running-eval ()
  ;; Defect: the interrupt names the wrong request (the newest, queued one
  ;; instead of the running, oldest one), names none, or is sent with no
  ;; eval running; an old server's refusal is not explained.
  (lpy-nrepl-test--with-servers (srv)
    (lpy-nrepl-connect (lpy-nrepl-test--port srv))
    (lpy-nrepl-interrupt)
    (should (equal (car lpy-nrepl-test--messages) "No evaluation running"))
    (let* ((first (lpy-nrepl--send '((op . "eval") (code . "(a)")) #'ignore))
           (second (lpy-nrepl--send '((op . "eval") (code . "(b)")) #'ignore)))
      (lpy-nrepl-interrupt)
      (should (lpy-nrepl-test--wait
               (lambda () (lpy-nrepl-test--requests srv "interrupt"))))
      (let ((reqs (lpy-nrepl-test--requests srv "interrupt")))
        (should (= (length reqs) 1))
        (should (equal (alist-get 'interrupt-id (car reqs)) first))
        ;; Only the eval requests went out before: none was an interrupt.
        (should (= (length (lpy-nrepl-test--requests srv)) 3))
        (lpy-nrepl-test--answer srv `((id . ,(alist-get 'id (car reqs)))
                                      (op . "error")
                                      (error . "unknown op: interrupt"))))
      (should (lpy-nrepl-test--wait
               (lambda () (string-match-p "too old" (car lpy-nrepl-test--messages)))))
      (lpy-nrepl-interrupt t)
      (should (lpy-nrepl-test--wait
               (lambda () (= (length (lpy-nrepl-test--requests srv "interrupt")) 3))))
      (let ((reqs (cdr (lpy-nrepl-test--requests srv "interrupt"))))
        (should (equal (mapcar (lambda (r) (alist-get 'interrupt-id r)) reqs)
                       (list first second)))
        ;; Success is the server's accepting reply, not a missing error.
        (setq lpy-nrepl-test--messages nil)
        (lpy-nrepl-test--answer srv `((id . ,(alist-get 'id (car reqs)))
                                      (op . "pong")))
        (should (lpy-nrepl-test--wait
                 (lambda () (string-match-p "failed" (or (car lpy-nrepl-test--messages) "")))))
        (lpy-nrepl-test--answer srv `((id . ,(alist-get 'id (cadr reqs)))
                                      (op . "interrupt-result")
                                      (interrupt-id . ,second)))
        (should (lpy-nrepl-test--wait
                 (lambda () (string-match-p "interrupted eval"
                                            (or (car lpy-nrepl-test--messages) "")))))))))

(ert-deftest lpy-nrepl-test-unencodable-request-leaves-nothing-pending ()
  ;; Defect: a request registered before it is encoded stays in flight
  ;; (mode line " eval…" forever) when encoding signals.
  (lpy-nrepl-test--with-servers (srv)
    (lpy-nrepl-connect (lpy-nrepl-test--port srv))
    (should-error (lpy-nrepl--send `((op . "eval") (code . ,(make-marker)))
                                   #'ignore))
    (should-not lpy-nrepl--in-flight-evals)
    (should-not lpy-nrepl--pending-callbacks)))

(ert-deftest lpy-nrepl-test-op-names-have-one-home ()
  ;; Policy: every op name lives in its defconst, so a rename is one edit;
  ;; a request site spelling the op inline drifts silently.
  (let ((source (locate-library "lpy-nrepl.el" t)))
    (should source)
    (with-temp-buffer
      (insert-file-contents source)
      ;; Positive control: the request sites exist, spelled via a constant.
      (should (re-search-forward "(op \\. ,lpy-nrepl--op-" nil t))
      (goto-char (point-min))
      (should-not (re-search-forward "(op \\. \"\\|(cons 'op \"" nil t)))))

(ert-deftest lpy-nrepl-test-log-is-capped ()
  ;; Defect: an endless printing loop grows the REPL log without bound
  ;; (tens of MB in seconds) on a machine shared with other sessions.
  (lpy-nrepl-test--with-servers (srv)
    (lpy-nrepl-connect (lpy-nrepl-test--port srv))
    (let* ((lpy-nrepl-log-max-size 2000)
           (id (lpy-nrepl--send '((op . "eval") (code . "(loop)")) #'ignore)))
      (should (lpy-nrepl-test--wait (lambda () (lpy-nrepl-test--requests srv))))
      (process-send-string
       (lpy-nrepl-test--client srv)
       (mapconcat (lambda (n)
                    (concat (json-serialize `((id . ,id) (op . "out")
                                              (stdout . ,(format "tick %d\n" n))))
                            "\n"))
                  (number-sequence 1 500) ""))
      (should (lpy-nrepl-test--wait
               (lambda () (string-match-p "tick 500\n" (lpy-nrepl-test--log-text)))))
      (with-current-buffer (lpy-nrepl--repl-buffer)
        (should (<= (buffer-size) (+ 2000 (length "tick 500\n"))))
        ;; The lines are short, so the cut snaps to a line start.
        (should (string-prefix-p "tick " (lpy-nrepl-test--log-text)))))))

(defmacro lpy-nrepl-test--with-named-log (&rest body)
  "Run BODY with a REPL log buffer named like the real one, then kill it.
No leading space: Emacs disables undo in such buffers by itself, which
would hide what `lpy-nrepl-repl-mode' does."
  (declare (indent 0))
  `(let ((lpy-nrepl--repl-buffer-name "*lpy-nrepl-test-log*"))
     (unwind-protect (progn ,@body)
       (when (get-buffer lpy-nrepl--repl-buffer-name)
         (kill-buffer lpy-nrepl--repl-buffer-name)))))

(defun lpy-nrepl-test--undo-deleted-chars ()
  "Return how many characters of deleted text `buffer-undo-list' holds."
  (if (eq buffer-undo-list t)
      0
    (cl-loop for e in buffer-undo-list
             when (and (consp e) (stringp (car e)))
             sum (length (car e)))))

(ert-deftest lpy-nrepl-test-trimmed-log-keeps-no-undo ()
  ;; Defect: the lines the cap trims are kept in `buffer-undo-list', so
  ;; an endless loop's output grows memory there instead (57 MB in 4 s).
  (lpy-nrepl-test--with-named-log
    (let ((lpy-nrepl-log-max-size 2000))
      (dotimes (n 2000)
        (lpy-nrepl--log "stdout-chunk" (format "tick %d\n" n)))
      (with-current-buffer (lpy-nrepl--repl-buffer)
        (should (<= (buffer-size) 2000))
        ;; Over 17000 characters were trimmed; what undo holds of them
        ;; must stay bounded by the cap, not grow with the output.
        (should (< (lpy-nrepl-test--undo-deleted-chars) 2000))))))

(ert-deftest lpy-nrepl-test-trim-inside-long-line-keeps-the-margin ()
  ;; Defect: a cut inside a long line snaps to the next newline wherever it
  ;; is, here the one before the newest entry, and drops almost the whole
  ;; log instead of trimming it to 90% of the cap.
  (lpy-nrepl-test--with-named-log
    (let ((lpy-nrepl-log-max-size 1000))
      (lpy-nrepl--log "stdout-chunk" (make-string 990 ?x))
      (lpy-nrepl--log "error" ";; interrupting eval 1")
      (lpy-nrepl--log "stdout-chunk" "yy")
      (with-current-buffer (lpy-nrepl--repl-buffer)
        (should (<= 850 (buffer-size) 1000))
        (should (string-suffix-p "\n;; interrupting eval 1\nyy"
                                 (lpy-nrepl-test--log-text)))))))

(ert-deftest lpy-nrepl-test-connection-death-is-logged-and-cleared ()
  ;; Defect: a server dying mid-eval leaves its callbacks and the eval in
  ;; flight forever, with nothing in the REPL log saying so.
  (lpy-nrepl-test--with-servers (srv)
    (lpy-nrepl-connect (lpy-nrepl-test--port srv))
    (lpy-nrepl-eval-string "(while True (pass))")
    (should lpy-nrepl--pending-callbacks)
    (should (lpy-nrepl-test--wait (lambda () (lpy-nrepl-test--client srv))))
    (delete-process (lpy-nrepl-test--client srv))
    (should (lpy-nrepl-test--wait (lambda () (not (lpy-nrepl-connected-p)))))
    (should (string-match-p "connection lost .* 1 pending request(s) dropped"
                            (lpy-nrepl-test--log-text)))
    (should-not lpy-nrepl--pending-callbacks)
    (should-not lpy-nrepl--in-flight-evals)
    (should-not lpy-nrepl--connection)))

(defun lpy-nrepl-test--fake-interpreter (dir port)
  "Write into DIR a fake `lpy' announcing PORT and then sleeping; return it."
  (let ((script (expand-file-name "fake-lpy" dir)))
    (with-temp-file script
      (insert "#!/bin/sh\n"
              (format "echo 'nREPL server started on 127.0.0.1:%d'\n" port)
              "exec sleep 30\n"))
    (set-file-modes script #o755)
    script))

(ert-deftest lpy-nrepl-test-start-ignores-dead-servers-port-line ()
  ;; Defect: start reads the port from the server buffer from its top, so
  ;; after a server death it finds the dead server's line and connects to
  ;; that port; and the death leaves `lpy-nrepl--server-process' set, so no
  ;; new server can start without restarting Emacs.
  (lpy-nrepl-test--with-servers (srv)
    (let* ((dir (make-temp-file "lpy-nrepl-fake-" t))
           (port (lpy-nrepl-test--port srv))
           (lpy-nrepl--interpreter (lpy-nrepl-test--fake-interpreter dir port)))
      (unwind-protect
          (progn
            (with-current-buffer (get-buffer-create lpy-nrepl--server-buffer-name)
              (insert "nREPL server started on 127.0.0.1:1\n"))
            (lpy-nrepl-start)
            (should (lpy-nrepl-test--wait #'lpy-nrepl-connected-p))
            (should (equal (process-contact lpy-nrepl--connection :service) port))
            (signal-process lpy-nrepl--server-process 'KILL)
            (should (lpy-nrepl-test--wait (lambda () (null lpy-nrepl--server-process))))
            (should (lpy-nrepl-test--wait (lambda () (not (lpy-nrepl-connected-p)))))
            (should (string-match-p "nREPL server died (killed"
                                    (lpy-nrepl-test--log-text)))
            (lpy-nrepl-start)
            (should (lpy-nrepl-test--wait #'lpy-nrepl-connected-p))
            (should (equal (process-contact lpy-nrepl--connection :service) port)))
        (delete-directory dir t)))))

;;; Send: superseding and its undo

(defmacro lpy-nrepl-test--with-send-stub (send-fn &rest body)
  "Run BODY with request state isolated and `process-send-string' as SEND-FN.
SEND-FN is called with the sent string."
  (declare (indent 1))
  `(let ((lpy-nrepl--repl-buffer-name " *lpy-nrepl-test-send*")
         (lpy-nrepl--connection nil)
         (lpy-nrepl--pending-callbacks nil)
         (lpy-nrepl--in-flight-evals nil)
         (lpy-nrepl--ignored-ids nil)
         (lpy-nrepl--latest-by-op nil)
         (lpy-nrepl--request-counter 0))
     (unwind-protect
         (cl-letf (((symbol-function 'lpy-nrepl-connected-p) (lambda () t))
                   ((symbol-function 'process-send-string)
                    (lambda (_proc str) (funcall ,send-fn str))))
           ,@body)
       (when (get-buffer lpy-nrepl--repl-buffer-name)
         (kill-buffer lpy-nrepl--repl-buffer-name)))))

(ert-deftest lpy-nrepl-test-request-sent-during-send-stays-latest ()
  ;; Defect: a blocking send runs timers (corfu's), and a newer complete
  ;; request sent from one is superseded by the older request once its send
  ;; returns, so the newer one's reply is dropped.
  (let ((sends 0) (nested nil))
    (lpy-nrepl-test--with-send-stub
        (lambda (_str)
          (when (= (cl-incf sends) 1)
            (setq nested (lpy-nrepl--send '((op . "complete")) #'ignore))))
      (let ((outer (lpy-nrepl--send '((op . "complete")) #'ignore)))
        (should nested)
        (should (assoc nested lpy-nrepl--pending-callbacks))
        (should-not (member nested lpy-nrepl--ignored-ids))
        (should (equal (alist-get "complete" lpy-nrepl--latest-by-op
                                  nil nil #'equal)
                       nested))
        (should-not (assoc outer lpy-nrepl--pending-callbacks))))))

(ert-deftest lpy-nrepl-test-failed-send-restores-superseded-request ()
  ;; Defect: a send that fails after superseding leaves the previous request
  ;; without its callback and the failed one as the op's latest, so the
  ;; previous request's reply, the only one that can come, is dropped.
  (let ((fail nil))
    (lpy-nrepl-test--with-send-stub
        (lambda (_str) (when fail (error "Broken pipe")))
      (let* ((a (lpy-nrepl--send '((op . "complete")) #'ignore))
             (b (progn
                  (setq fail t)
                  (should-error (lpy-nrepl--send '((op . "complete")) #'ignore))
                  (number-to-string lpy-nrepl--request-counter))))
        (should (assoc a lpy-nrepl--pending-callbacks))
        (should-not (member a lpy-nrepl--ignored-ids))
        (should (equal (alist-get "complete" lpy-nrepl--latest-by-op
                                  nil nil #'equal)
                       a))
        (should-not (assoc b lpy-nrepl--pending-callbacks))
        (should-not (member b lpy-nrepl--in-flight-evals))
        (should (string-match-p ";; send failed: Broken pipe"
                                (with-current-buffer (lpy-nrepl--repl-buffer)
                                  (buffer-string))))))))

(ert-deftest lpy-nrepl-test-failed-send-keeps-newer-nested-request ()
  ;; Defect: undoing a failed send's supersede after a newer request
  ;; superseded it meanwhile puts the older request back as the latest.
  (let ((fail nil) (nested nil))
    (lpy-nrepl-test--with-send-stub
        (lambda (_str)
          (when fail
            (setq fail nil
                  nested (lpy-nrepl--send '((op . "complete")) #'ignore))
            (error "Broken pipe")))
      (let ((a (lpy-nrepl--send '((op . "complete")) #'ignore)))
        (setq fail t)
        (should-error (lpy-nrepl--send '((op . "complete")) #'ignore))
        (should (equal (alist-get "complete" lpy-nrepl--latest-by-op
                                  nil nil #'equal)
                       nested))
        (should (assoc nested lpy-nrepl--pending-callbacks))
        (should-not (assoc a lpy-nrepl--pending-callbacks))
        (should (equal lpy-nrepl--ignored-ids (list a)))))))

(ert-deftest lpy-nrepl-test-failed-send-keeps-answered-request-done ()
  ;; Defect: the superseded request's final reply is read, and dropped,
  ;; during the failing send, and undoing the supersede then restores its
  ;; callback, which waits forever for a reply that is gone.
  (let ((fail nil) (fired nil))
    (lpy-nrepl-test--with-send-stub
        (lambda (_str)
          (when fail
            (setq fail nil)
            (lpy-nrepl--handle-response '((id . "1") (items . [])))
            (error "Broken pipe")))
      (let ((a (lpy-nrepl--send '((op . "complete"))
                                (lambda (_) (push 'a fired)))))
        (setq fail t)
        (should-error (lpy-nrepl--send '((op . "complete")) #'ignore))
        (should-not (assoc a lpy-nrepl--pending-callbacks))
        (let ((c (lpy-nrepl--send '((op . "complete"))
                                  (lambda (_) (push 'c fired)))))
          (lpy-nrepl--handle-response `((id . ,c) (items . [])))
          (should (equal fired '(c)))
          (should-not lpy-nrepl--pending-callbacks)
          (should-not lpy-nrepl--ignored-ids))))))

(ert-deftest lpy-nrepl-test-unloggable-request-is-not-registered ()
  ;; Defect: an input log that signals leaves the never-sent request
  ;; registered: its callback waits forever, its eval stays in flight, and
  ;; it supersedes the previous request of its op.
  (lpy-nrepl-test--with-send-stub #'ignore
    (let ((a (lpy-nrepl--send '((op . "complete")) #'ignore)))
      (cl-letf (((symbol-function 'lpy-nrepl--log)
                 (lambda (direction _text)
                   (when (equal direction "input") (error "Log broken")))))
        (should-error (lpy-nrepl--send '((op . "complete") (code . "x"))
                                       #'ignore))
        (should-error (lpy-nrepl--send '((op . "eval") (code . "x"))
                                       #'ignore)))
      (should (equal (mapcar #'car lpy-nrepl--pending-callbacks) (list a)))
      (should-not lpy-nrepl--in-flight-evals)
      (should (equal (alist-get "complete" lpy-nrepl--latest-by-op
                                nil nil #'equal)
                     a)))))

(ert-deftest lpy-nrepl-test-quit-send-drops-connection ()
  ;; Defect: C-g during a blocking send undoes the request as if unsent,
  ;; but Emacs still writes the queued rest of its line on the next send,
  ;; so the server runs an eval the client no longer tracks (or the stream
  ;; holds half a line).
  (let ((quit-p nil)
        (conn (make-pipe-process :name "lpy-nrepl-test-conn" :noquery t))
        (server (make-pipe-process :name "lpy-nrepl-test-srv" :noquery t)))
    (unwind-protect
        (lpy-nrepl-test--with-send-stub
            (lambda (_str) (when quit-p (signal 'quit nil)))
          (let ((lpy-nrepl--server-process server)
                (lpy-nrepl--scope nil))
            (setq lpy-nrepl--connection conn)
            (lpy-nrepl--send '((op . "complete")) #'ignore)
            (setq quit-p t)
            (should (eq (condition-case nil
                            (lpy-nrepl--send '((op . "eval") (code . "x"))
                                             #'ignore)
                          (quit 'quit))
                        'quit))
            (should-not lpy-nrepl--connection)
            (should-not (process-live-p conn))
            (should (process-live-p server))
            (should-not lpy-nrepl--pending-callbacks)
            (should-not lpy-nrepl--in-flight-evals)
            (should-not lpy-nrepl--latest-by-op)
            (should (string-match-p
                     "2 pending"
                     (with-current-buffer (lpy-nrepl--repl-buffer)
                       (buffer-string))))))
      (delete-process conn)
      (delete-process server))))

;;; Live tests against `lpy --nrepl' (LPY_TEST_NREPL_LPY)

(defun lpy-nrepl-test--live-lpy ()
  "Return the `lpy' named by LPY_TEST_NREPL_LPY, or skip the test."
  (let ((lpy (getenv "LPY_TEST_NREPL_LPY")))
    (unless (and lpy (file-executable-p lpy))
      (ert-skip "LPY_TEST_NREPL_LPY names no executable lpy"))
    lpy))

(defmacro lpy-nrepl-test--with-live-server (&rest body)
  "Run BODY connected to a fresh `lpy --nrepl' started in a temp directory.
The server is killed afterwards."
  (declare (indent 0))
  `(let ((lpy (lpy-nrepl-test--live-lpy)))
     (lpy-nrepl-test--with-servers ()
       (let* ((dir (file-name-as-directory (make-temp-file "lpy-nrepl-live-" t)))
              (default-directory dir)
              (lpy-nrepl--interpreter lpy))
         (unwind-protect
             (progn
               (lpy-nrepl-start)
               (should (lpy-nrepl-test--wait #'lpy-nrepl-connected-p 20))
               ,@body)
           (delete-directory dir t))))))

(defun lpy-nrepl-test--live-sum ()
  "Eval (+ 1 2) on the live server and assert it yields 3."
  (lpy-nrepl-eval-string "(+ 1 2)")
  (should (lpy-nrepl-test--wait
           (lambda () (string-match-p "^3$" (lpy-nrepl-test--log-text))))))

(ert-deftest lpy-nrepl-test-live-endless-eval-streams-and-interrupts ()
  ;; Defect: C-c C-b on an endless printing loop shows no output and can
  ;; only be stopped by killing the server.
  (lpy-nrepl-test--with-live-server
    (with-temp-buffer
      (insert "(import time)\n(while True (print \"tick\") (time.sleep 0.05))\n")
      (let ((start (float-time)))
        (lpy-nrepl-eval-buffer-contents)
        (should (< (- (float-time) start) 0.5))))
    (should (lpy-nrepl-test--wait
             (lambda () (string-match-p "^tick$" (lpy-nrepl-test--log-text)))))
    (should lpy-nrepl--in-flight-evals)
    (lpy-nrepl-interrupt)
    (should (lpy-nrepl-test--wait (lambda () (null lpy-nrepl--in-flight-evals))))
    (should (string-match-p "KeyboardInterrupt" (lpy-nrepl-test--log-text)))
    (should-not (cl-some (lambda (m) (string-match-p "too old\\|failed" m))
                         lpy-nrepl-test--messages))
    (lpy-nrepl-test--live-sum)))

(ert-deftest lpy-nrepl-test-live-killed-server-restarts ()
  ;; Defect: after the server is killed mid-eval, nREPL stays dead until
  ;; Emacs restarts (a stale server process, or a stale port).
  (lpy-nrepl-test--with-live-server
    (lpy-nrepl-eval-string "(do (import time) (while True (time.sleep 0.05)))")
    (signal-process lpy-nrepl--server-process 'KILL)
    (should (lpy-nrepl-test--wait (lambda () (null lpy-nrepl--server-process))))
    (should (lpy-nrepl-test--wait (lambda () (not (lpy-nrepl-connected-p)))))
    (should-not lpy-nrepl--in-flight-evals)
    (lpy-nrepl-start)
    (should (lpy-nrepl-test--wait #'lpy-nrepl-connected-p 20))
    (lpy-nrepl-test--live-sum)))

(provide 'lpy-nrepl-test)

;;; lpy-nrepl-test.el ends here
