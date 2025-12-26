;;; lpy-mode.el --- Major mode for Lpy -*- lexical-binding: t -*-

;; Copyright © 2013 Julien Danjou <julien@danjou.info>
;;           © 2017-2019 Eric Kaschalk <ekaschalk@gmail.com>
;;           © 2025 Jetack <jetack23@gmail.com>
;;
;; Author: Jetack <jetack23@gmail.com>
;; URL: https://github.com/jetack/lpy-mode
;; Version: 1.0
;; Keywords: languages, lisp, python
;; Package-Requires: ((dash "2.18.0") (s "1.11.0") (emacs "24"))

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

;; Provides syntax highlighting, indentation, autocompletion, documentation
;; lookup, REPL support, and more features for working productively in Lpy,
;; the lisp embedded in Python.

;; Syntax highlighting and related can be found in `lpy-font-lock.el'
;; REPL support can be found in `lpy-shell.el'
;; IDE components support can be found in `lpy-autocomplete.el'
;; Common utilities and requires can be found in `lpy-base.el'
;; Testing utilities for the test/ folder can be found in `lpy-test.el'

;; This file implements the syntax table, indentation, keybindings, and
;; `lpy-mode' setup code.

;;; Code:

(require 'lpy-base)

(require 'lpy-font-lock)
(require 'lpy-shell)
(require 'lpy-autocomplete)

;;; Configuration
;;;; Indentation

;; See other files for configuring specific aspects of Lpy, like the shell.

(defvar lpy-indent--exactly
  '("when" "unless"
    "for" "for*" "for/a" "for/a*"
    "while"
    "except" "catch")
  "Symbols that will have following lines indented +1 when matched.

Examples:

(when foo
  body)
(when-xx foo
         body)
")


(defvar lpy-indent--fuzzily
  '("def"
    "let"
    "with" "with/a"
    "fn" "fn/a"
    "class"
    "deco")
  "Symbols that will have following lines indented +1 when matched at start.

Examples:

(with foo
  body)
(with-xx foo
  body)
")

;;; Syntax
;;;; Syntax Table

(defconst lpy-mode-syntax-table
  (let ((table (copy-syntax-table lisp-mode-syntax-table)))
    ;; List-likes
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)

    ;; Quote Characters
    (modify-syntax-entry ?\~ "'" table)

    ;; Symbol Constituents
    (modify-syntax-entry ?\, "_" table)
    (modify-syntax-entry ?\| "_" table)
    (modify-syntax-entry ?\# "_" table)

    ;; Note that @ is a valid symbol token but in almost all usages we would
    ;; rather the symbol for ~@foo to be recognized as foo and not @foo.
    (modify-syntax-entry ?\@ "'" table)

    table)
  "The `lpy-mode' syntax table.")

(defconst inferior-lpy-mode-syntax-table (copy-syntax-table lpy-mode-syntax-table)
  "`inferior-lpy-mode' inherits `lpy-mode-syntax-table'.")

;;;; Context Sensitive

(defconst lpy--bracket-string-rx
  (rx "#["
      (0+ not-newline)
      "["
      (group (1+ (not (any "]"))))
      "]"
      (0+ not-newline)
      "]")
  "Regex identifying Lpy's bracket string literals.")

(defun lpy-syntax-propertize-function (start end)
  "Implements context sensitive syntax highlighting beyond `font-lock-keywords'.

In particular this implements bracket string literals.
START and END are the limits with which to search for bracket strings passed
and determined by `font-lock-mode' internals when making an edit to a buffer."
  (save-excursion
    (goto-char start)

    ;; Go to the start of the #[[ block
    (when (lpy--goto-inner-char (syntax-ppss))
      (ignore-errors (backward-char 2)))

    (while (re-search-forward lpy--bracket-string-rx end 'noerror)
      (let ((a (match-beginning 1))
            (b (match-end 1))
            (string-fence (string-to-syntax "|")))
        (put-text-property (1- a) a 'syntax-table string-fence)
        (put-text-property b (1+ b) 'syntax-table string-fence)))))

;;; Indentation
;;;; Normal Indent

(defun lpy-indent--normal (calculated-last-sexp-indent)
  "Get indent of the priorly let-bound value `calculate-lisp-indent-last-sexp'

Example:
 (a (b c d
       e
       f))

1. Indent e => start at d (the last sexp) -> c -> b -> err.
=> backwards-sexp will throw error trying to jump to a
=> `lpy-indent-function' returns nil
=> black magic then yields the correct indent

2. Indent f => start at e (the last sexp) -> loop done
=> backward-sexp loop terminates because the indentation caught up to the sexp
=> return indent of e

Users interested in the arcane (the nil case) can step through the part of
`calculate-lisp-indent' occurring right after `lisp-indent-function' is called.
Stepping through the trace is particularly useful in understanding indentation
commands."
  (goto-char calculated-last-sexp-indent)

  (let ((last-sexp-start))
    (cond ((ignore-errors
             (while (<= (current-indentation) (current-column))
               (setq last-sexp-start (prog1 (point) (backward-sexp))))
             t)
           (current-column))

          ((null last-sexp-start)
           (progn
             (when (-contains? '(?\' ?\` ?\~ ?\# ?\@) (char-before))
               (backward-char))
             (when (eq ?\~ (char-before))
               (backward-char))

             (1+ (current-column)))))))

;;;; Spec Finding

(defun lpy-indent--syntax->indent-spec (syntax)
  "Get int for special indentation for SYNTAX state or nil for normal indent."
  (-when-let (sym (and (lpy--prior-sexp? syntax)
                       (thing-at-point 'symbol)))
    (or (-contains? lpy-indent--exactly sym)
        (-some (-cut s-matches? <> sym) lpy-indent--fuzzily))))

;;;; Indent Function

(defun lpy-indent-function (_indent-point syntax)
  "Given SYNTAX, the `parse-partial-sexp' corr. to _INDENT-POINT, get indent."
  (lpy--goto-inner-sexp syntax)

  (cond ((-contains? '(?\[ ?\{) (char-before))
         (current-column))

        ((lpy-indent--syntax->indent-spec syntax)
         (1+ (current-column)))

        (t (lpy-indent--normal calculate-lisp-indent-last-sexp))))

;;; Setup
;;;; Core

(defun lpy-mode--setup-font-lock ()
  "Setup `font-lock-defaults' and others for `lpy-mode.'"
  (setq-local font-lock-multiline t)
  (setq font-lock-defaults
        '(lpy-font-lock-kwds
          nil nil
          (("+-*/.<>=!?$%_&~^:@" . "w"))  ; syntax alist
          nil
          (font-lock-mark-block-function . mark-defun)
          (font-lock-syntactic-face-function  ; Differentiates (doc)strings
           . lpy-font-lock-syntactic-face-function))))

(defun lpy-mode--setup-syntax ()
  "Setup syntax, indentation, and other core components of major modes."
  ;; We explictly set it for tests that only call this setup-fn
  (set-syntax-table lpy-mode-syntax-table)

  ;; Bracket string literals require context sensitive highlighting
  (setq-local syntax-propertize-function 'lpy-syntax-propertize-function)

  ;; AutoHighlightSymbol needs adjustment for symbol recognition
  (setq-local ahs-include "^[0-9A-Za-z/_.,:;*+=&%|$#@!^?-~\-]+$")

  ;; Lispy comment syntax
  (setq-local comment-start ";")
  (setq-local comment-start-skip
              "\\(\\(^\\|[^\\\\\n]\\)\\(\\\\\\\\\\)*\\)\\(;+\\|#|\\) *")
  (setq-local comment-add 1)

  ;; Lispy indent with lpy-specialized indentation
  (setq-local indent-tabs-mode nil)
  (setq-local indent-line-function #'lisp-indent-line)
  (setq-local lisp-indent-function #'lpy-indent-function))

;;;; Support

(defun lpy-mode--support-smartparens ()
  "Setup `smartparens-mode' pairs for Lpy, if applicable."
  (when (fboundp #'sp-local-pair)
    (sp-local-pair '(lpy-mode inferior-lpy-mode) "`" "`" :actions nil)))

;;;; Lpy-autocomplete

(defun lpy-mode--setup-lpy-autocomplete ()
  "Auto-start lpy-autocomplete for company, eldoc, and other `lpy-mode' IDE features."
  (let ((lpy-shell--notify?))
    (run-lpy-autocomplete))  ; Unlikely that lpy-autocomplete installed globally so dont warn

  (when (fboundp 'pyvenv-mode)
    (add-hook 'pyvenv-post-activate-hooks #'run-lpy-autocomplete)
    (add-hook 'pyvenv-post-deactivate-hooks
              #'run-lpy-autocomplete--pyvenv-post-deactive-hook)))

(defun lpy-mode--support-company ()
  "Support `company-mode' autocompletion."
  (add-to-list 'company-backends #'company-lpy))

(defun lpy-mode--support-eldoc ()
  "Support `eldoc-mode' with lispy docstring leaders."
  (setq-local eldoc-documentation-function #'lpy-eldoc-documentation-function)
  (eldoc-mode +1))

;;; lpy-mode

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.lpy\\'" . lpy-mode))
;;;###autoload
(add-to-list 'interpreter-mode-alist '("lpy" . lpy-mode))

;;;###autoload
(define-derived-mode lpy-mode prog-mode "Lpy"
  "Major mode for editing Lpy files."
  (lpy-mode--setup-font-lock)
  (lpy-mode--setup-syntax)

  (lpy-mode--support-smartparens)

  (when lpy-autocomplete--enable?
    (lpy-mode--setup-lpy-autocomplete)

    (lpy-mode--support-eldoc)

    (when (featurep 'company)
      (lpy-mode--support-company)
      (add-hook 'inferior-lpy-mode-hook #'lpy-mode--support-company))))

;;; Bindings

(set-keymap-parent lpy-mode-map lisp-mode-shared-map)

;;;; Shell

(define-key lpy-mode-map (kbd "C-c C-z") #'run-lpy)

(define-key lpy-mode-map (kbd "C-c C-b") #'lpy-shell-eval-buffer)
(define-key lpy-mode-map (kbd "C-c C-r") #'lpy-shell-eval-region)
(define-key lpy-mode-map (kbd "C-c C-e") #'lpy-shell-eval-last-sexp)
(define-key lpy-mode-map (kbd "C-M-x") #'lpy-shell-eval-current-form)

(define-key lpy-mode-map (kbd "C-c C-d d") #'lpy-describe-thing-at-point)
(define-key lpy-mode-map (kbd "C-c C-d C-d") #'lpy-describe-thing-at-point)

;;;; Misc

;;;###autoload
(defun lpy-insert-pdb ()
  "Import and set pdb trace at point."
  (interactive)
  (insert "(do (import pdb) (pdb.set-trace))"))

(define-key lpy-mode-map (kbd "C-c C-t") #'lpy-insert-pdb)

;;; Provide:

(provide 'lpy-mode)

;;; lpy-mode.el ends here
