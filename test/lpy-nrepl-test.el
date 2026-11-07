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

(ert-deftest lpy-nrepl-test-lsp-and-nrepl-kinds-share-tags ()
  ;; Defect: the LSP and nREPL kind vocabularies drift, so one list shows
  ;; the same kind of name under two tags.
  (dolist (case '((3 . "function") (7 . "class") (6 . "variable")
                  (14 . "keyword")))
    (should (equal (lpy--kind-annotation (alist-get (car case) lpy--lsp-kinds))
                   (lpy-nrepl--kind-annotation (cdr case))))))

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
                    (funcall callback '((completions . []) (docs . "d")
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
                          '((completions . ["heapify" "heappush"])
                            (kinds . ((heapify . "function"))))))))
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
      (lpy-nrepl--process-sentinel lpy-nrepl--connection
                                   "connection broken by remote peer\n")
      (should-not lpy-nrepl--scope)
      (should (string-match-p "disconnected (connection broken"
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
                       " fn"))
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
      (should (member "head" cands))
      (should (equal (funcall (plist-get plist :annotation-function) "head")
                     " fn"))
      (should (eq (funcall (plist-get plist :company-kind) "head") 'function))
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

(ert-deftest lpy-nrepl-test-merged-capf-single-source-is-unchanged ()
  ;; Defect: with one source missing the merge still wraps the other,
  ;; changing behaviour for users without eglot or without an nREPL.
  (let ((eglot (with-temp-buffer (insert "hea") (lpy-nrepl-test--eglot-capf)))
        (nrepl (with-temp-buffer (insert "hea") (lpy-nrepl-test--nrepl-capf))))
    (lpy-nrepl-test--with-capfs eglot nrepl
      (cl-letf (((symbol-function 'eglot-managed-p) #'ignore))
        (should (eq (lpy-completion-at-point) nrepl))))
    (lpy-nrepl-test--with-capfs eglot nil
      ;; eglot alone keeps its table; only the annotations are shortened.
      (let ((result (lpy-completion-at-point)))
        (should (eq (nth 2 result) (nth 2 eglot)))
        (should (eq (plist-get (nthcdr 3 result) :exit-function)
                    (plist-get (nthcdr 3 eglot) :exit-function)))))
    (lpy-nrepl-test--with-capfs nil nil
      (should-not (lpy-completion-at-point)))))

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
    (should (equal (funcall annotate cand) " fn"))))

(ert-deftest lpy-nrepl-test-lsp-annotation-prefers-label-description ()
  ;; Defect: a server's own short tag (labelDetails.description) is
  ;; replaced by the generic kind tag.
  (lpy-nrepl-test--with-item '(:label "defn" :kind 14 :detail "special form"
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
      (should (string-match-p "def nlargest" sig))
      (should-not (string-match-p "\n" sig)))))

(ert-deftest lpy-nrepl-test-lsp-without-detail-keeps-eglot-docs ()
  ;; Defect: an item without a signature loses eglot's own docs or docsig.
  (lpy-nrepl-test--with-item '(:label "nl" :kind 6)
    (should (equal (funcall annotate cand) " var"))
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

(provide 'lpy-nrepl-test)

;;; lpy-nrepl-test.el ends here
