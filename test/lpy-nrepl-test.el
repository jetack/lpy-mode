;;; lpy-nrepl-test.el --- nREPL client tests -*- lexical-binding: t -*-

;; ERT tests for the pure parts of `lpy-nrepl': completion kind annotations
;; and `.nrepl-port' discovery.  Run with:
;;   B=~/.emacs.d/straight/build
;;   emacs -Q --batch -L . -L $B/dash -L $B/s -L $B/f \
;;     -l test/lpy-nrepl-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
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

(provide 'lpy-nrepl-test)

;;; lpy-nrepl-test.el ends here
