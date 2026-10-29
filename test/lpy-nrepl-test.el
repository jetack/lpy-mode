;;; lpy-nrepl-test.el --- nREPL client tests -*- lexical-binding: t -*-

;; ERT tests for `lpy-nrepl': completion kind annotations, `.nrepl-port'
;; discovery, connection swapping and REPL log scrolling.  Run with:
;;   B=~/.emacs.d/straight/build
;;   emacs -Q --batch -L . -L $B/dash -L $B/s -L $B/f \
;;     --eval '(setq load-prefer-newer t)' \
;;     -l test/lpy-nrepl-test.el -f ert-run-tests-batch-and-exit
;; load-prefer-newer matters: a stale (gitignored) lpy-nrepl.elc beside the
;; source is otherwise loaded instead, and the tests silently run old code.

;;; Code:

(require 'ert)
(require 'cl-lib)
(add-to-list 'load-path
             (file-name-directory
              (directory-file-name
               (file-name-directory (or load-file-name buffer-file-name)))))
(require 'lpy-nrepl)

(ert-deftest lpy-nrepl-test-kind-annotation ()
  (dolist (case '(("class" . " class") ("function" . " fn")
                  ("variable" . " var") ("macro" . " macro")
                  ("keyword" . " kw")))
    (should (equal (lpy-nrepl--kind-annotation (car case)) (cdr case))))
  (should-not (lpy-nrepl--kind-annotation "mystery"))
  (should-not (lpy-nrepl--kind-annotation nil)))

(ert-deftest lpy-nrepl-test-kinds-lookup ()
  ;; json-parse-string :object-type 'alist interns keys as symbols.
  (let ((kinds '((foo . "function") (os.path . "variable"))))
    (should (equal (lpy-nrepl--kinds-lookup kinds "foo") "function"))
    (should (equal (lpy-nrepl--kinds-lookup kinds "os.path") "variable"))
    (should-not (lpy-nrepl--kinds-lookup kinds "absent"))
    (should-not (lpy-nrepl--kinds-lookup nil "foo"))))

(ert-deftest lpy-nrepl-test-kinds-from-parsed-response ()
  (let* ((line "{\"op\":\"complete-result\",\"completions\":[\"os.path\"],\"kinds\":{\"os.path\":\"variable\"}}")
         (msg (json-parse-string line :object-type 'alist))
         (kinds (alist-get 'kinds msg)))
    (should (equal (lpy-nrepl--kind-annotation
                    (lpy-nrepl--kinds-lookup kinds "os.path"))
                   " var"))))

(ert-deftest lpy-nrepl-test-company-kinds-cover-annotations ()
  (dolist (entry lpy-nrepl--kind-annotations)
    (should (assoc (car entry) lpy-nrepl--company-kinds))))

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
  "Start a local TCP server on a free port and return it."
  (make-network-process :name "lpy-nrepl-test-server" :server t
                        :service t :host "127.0.0.1" :family 'ipv4
                        :noquery t))

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
         (lpy-nrepl--connection nil)
         (lpy-nrepl--pending-callbacks nil)
         (lpy-nrepl--response-buffer "")
         (lpy-nrepl-test--messages nil)
         ,@(mapcar (lambda (v) `(,v (lpy-nrepl-test--server))) vars))
     (cl-letf (((symbol-function 'message)
                (lambda (fmt &rest args)
                  (push (apply #'format fmt args) lpy-nrepl-test--messages))))
       (unwind-protect (progn ,@body)
         (when lpy-nrepl--connection (delete-process lpy-nrepl--connection))
         ,@(mapcar (lambda (v) `(delete-process ,v)) vars)
         (accept-process-output nil 0.05)
         (when (get-buffer lpy-nrepl-test--servers-buffer-name)
           (kill-buffer lpy-nrepl-test--servers-buffer-name))))))

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
      (lpy-nrepl--process-sentinel proc "connection broken by remote peer\n")
      (should-not lpy-nrepl--connection)
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
                         (lpy-nrepl--eval-code "(+ 1 2)"))))
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

(provide 'lpy-nrepl-test)

;;; lpy-nrepl-test.el ends here
