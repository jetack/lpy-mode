;;; lpy-font-lock.el --- Font Locks -*- lexical-binding: t -*-

;; Copyright © 2013 Julien Danjou <julien@danjou.info>
;;           © 2017-2019 Eric Kaschalk <ekaschalk@gmail.com>
;;           © 2025 Jetack <jetack23@gmail.com>
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

;; Font lock definitions and setup for `lpy-mode'.

;; Font locks are organized and exposed at the end: `lpy-font-lock-kwds'
;; Comint Font locks are exposed in: `inferior-lpy-font-lock-kwds'
;; Also implements docstring detection: `lpy-font-lock-syntactic-face-function'

;;; Code:

(require 'lpy-base)

(defvar lpy-font-lock-highlight-percent-args? t
  "Whether to highlight '%i' symbols in Lpy's clojure-like syntax for lambdas.")

;;; Names Lists
;;;; Lpy Builtins

(defconst lpy-font-lock--lpy-builtins
  '()
  "Lpy-only builtin names.")

;;;; Python Builtins

(defconst lpy-font-lock--python-builtins
  '("abs"
    "all"
    "any"
    "ascii"
    "bytes"
    "bin"
    "bool"
    "bytearray"
    "callable"
    "chr"
    "compile"
    "complex"
    "delattr"
    "dict"
    "dir"
    "divmod"
    "enumerate"
    "eval"
    "exec"
    "float"
    "format"
    "frozenset"
    "getattr"
    "globals"
    "hasattr"
    "hash"
    "help"
    "hex"
    "id"
    "input"
    "int"
    "isinstance"
    "issubclass"
    "iter"
    "len"
    "list"
    "locals"
    "map"
    "max"
    "memoryview"
    "min"
    "next"
    "object"
    "oct"
    "open"
    "ord"
    "pow"
    "range"
    "repr"
    "reversed"
    "round"
    "set"
    "setattr"
    "slice"
    "sorted"
    "str"
    "sum"
    "super"
    "tuple"
    "type"
    "vars"
    "--package--" "__package__"
    "--import--" "__import__"
    "--all--" "__all__"
    "--doc--" "__doc__"
    "--name--" "__name__")
  "Builtin names available in Python normally.")

;;;; Constants

(defconst lpy-font-lock--constants
  '("True"
    "False"
    "None"
    "Ellipsis"
    "NotImplemented"
    "nil"  ; Provided for those that alias None as nil, not a part of Lpy
    )
  "Constant names in Lpy.")

;;;; Exceptions

(defconst lpy-font-lock--exceptions
  '("ArithmeticError" "AssertionError" "AttributeError" "BaseException"
    "DeprecationWarning" "EOFError" "EnvironmentError" "Exception"
    "FloatingPointError" "FutureWarning" "GeneratorExit" "IOError" "ImportError"
    "ImportWarning" "IndexError" "KeyError" "KeyboardInterrupt" "LookupError"
    "MemoryError" "NameError" "NotImplementedError" "OSError" "OverflowError"
    "PendingDeprecationWarning" "ReferenceError" "RuntimeError" "RuntimeWarning"
    "StopIteration" "SyntaxError" "SyntaxWarning" "SystemError" "SystemExit"
    "TypeError" "UnboundLocalError" "UnicodeDecodeError" "UnicodeEncodeError"
    "UnicodeError" "UnicodeTranslateError" "UnicodeWarning" "UserWarning"
    "VMSError" "ValueError" "Warning" "WindowsError" "ZeroDivisionError"
    "BufferError" "BytesWarning" "IndentationError" "ResourceWarning"
    "TabError")
  "Exception and error names.")

;;;; Definitions

(defconst lpy-font-lock--definitions
  '("def" "async-def" "defmacro")
  "Names in Lpy that define functions, macros, etc.")

;;;; Operators

(defconst lpy-font-lock--operators
  '("!=" "%" "%=" "&" "&=" "*" "**" "**=" "*=" "+" "+=" "," "-"
    "-=" "/" "//" "//=" "/=" "<" "<<" "<<=" "<=" "=" ">" ">=" ">>" ">>="
    "^" "^=" "|" "|=" "~")
  "Operators in Lpy.")

;;;; Special Names

(defconst lpy-font-lock--special-names
  '(;; Looping
    "for" "async-for"

    ;; Flow control
    "return"
    "if" "ife"
    "when"
    "cond" "conde"
    "else"
    "break" "continue" "while"
    "match" "case"
    "do"
    "comment"
    "->" "->>"
    "pass"

    ;; Functional
    "fn"
    "lambda"
    "await"
    "yield" "yield-from"
    "with" "async-with"

    ;; Decorators / deletion
    "deco"
    "del"

    ;; Error Handling
    "except" "try" "raise" "finally" "assert"

    ;; Python builtins (and shadowed calls to builtins)
    "print"
    "not" "and" "or"
    "in"

    ;; Namespaces
    "global" "nonlocal"

    ;; Evaluation
    "eval")
  "Special names like compiler stuff to highlight as keywords.")

;;;; Anaphorics

(defconst lpy-font-lock--anaphorics
  '()
  "Lpy anaphoric contrib keywords.")

;;; Keywords
;;;; Based on Names Lists

(defconst lpy-font-lock--kwds-builtins
  (list
   (rx-to-string
    `(: symbol-start
        (or ,@lpy-font-lock--lpy-builtins
            ,@lpy-font-lock--python-builtins
            ,@lpy-font-lock--operators
            ,@lpy-font-lock--anaphorics)
        symbol-end))

   '(0 font-lock-builtin-face))
  "Lpy builtin keywords.")

(defconst lpy-font-lock--kwds-constants
  (list
   (rx-to-string
    `(: symbol-start
        (or ,@lpy-font-lock--constants)
        symbol-end))

   '(0 font-lock-constant-face))
  "Lpy constant keywords.")

(defconst lpy-font-lock--kwds-definitions
  (list
   (rx-to-string
    `(: "("
        symbol-start
        (group-n 1 (or ,@lpy-font-lock--definitions))
        (1+ space)
        (group-n 2 (1+ word))))

   '(1 font-lock-keyword-face)
   '(2 font-lock-function-name-face nil t))
  "Lpy definition keywords.")

(defconst lpy-font-lock--kwds-exceptions
  (list
   (rx-to-string
    `(: symbol-start
        (or ,@lpy-font-lock--exceptions)
        symbol-end))

   '(0 font-lock-type-face))
  "Lpy exception keywords.")

(defconst lpy-font-lock--kwds-special-names
  (list
   (rx-to-string
    `(: symbol-start
        (or ,@lpy-font-lock--special-names)
        symbol-end))

   '(0 font-lock-keyword-face))
  "Lpy special names keywords.")

;;;; Static

(defconst lpy-font-lock--kwds-class
  (list
   (rx "("
       (group-n 1 "class")
       (1+ space)
       (group-n 2 (1+ word)))

   '(1 font-lock-keyword-face)
   '(2 font-lock-type-face))
  "Lpy class keywords.")

(defconst lpy-font-lock--kwds-decorators
  (list
   (rx symbol-start
       "deco"
       symbol-end
       (1+ space)
       (1+ word))

   '(0 font-lock-type-face))
  "Lpylight the symbol after `deco' macros keywords.")

(defconst lpy-font-lock--kwds-imports
  (list
   (rx symbol-start
       (or "import"
           "from"
           "require"
           ":as")
       symbol-end)

   '(0 font-lock-keyword-face))
  "Lpy import keywords.")

(defconst lpy-font-lock--kwds-self
  (list
   (rx symbol-start
       (group "self")
       (or "." symbol-end))

   '(1 font-lock-keyword-face))
  "Lpy self keyword.")

(defconst lpy-font-lock--kwds-tag-macros
  (list
   (rx "#"
       ;; #* is unpacking, #@ decorator, #[ bracket str
       (not (any "*"
                 "@"
                 "["
                 ")"
                 space))
       (0+ (syntax word)))

   '(0 font-lock-function-name-face))
  "Lpylight tag macros, ie. `#tag-macro', so they stand out.")

;;;; Misc

(defconst lpy-font-lock--kwds-anonymous-funcs
  (list
   (rx symbol-start
       (group "%" (1+ digit))
       (or "." symbol-end))

   '(1 font-lock-variable-name-face))
  "Lpy '#%(print %1 %2)' styling anonymous variables.")

(defconst lpy-font-lock--kwds-func-modifiers
  (list
   (rx symbol-start
       "&"
       (1+ word))

   '(0 font-lock-type-face))
  "Lpy '&rest/&kwonly/...' styling.")

(defconst lpy-font-lock--kwds-kwargs
  (list
   (rx symbol-start
       ":"
       (1+ word))

   '(0 font-lock-constant-face))
  "Lpy ':kwarg' styling.")

(defconst lpy-font-lock--kwds-shebang
  (list
   (rx buffer-start
       "#!"
       (0+ not-newline)
       eol)

   '(0 font-lock-comment-face))
  "Lpy shebang line.")

(defconst lpy-font-lock--kwds-unpacking
  (list
   (rx (or "#*"
           "#**")
       symbol-end)

   '(0 font-lock-keyword-face))
  "Lpy #* arg and #** kwarg unpacking keywords.")

(defconst lpy-font-lock--kwds-variables
  (list
   (rx symbol-start
       "="
       symbol-end
       (1+ space)
       (group (1+ word)))

   '(1 font-lock-variable-name-face))
  "Lpylight variable names in setv/def, only first name.")

;;;; Advanced

(defconst lpy-font-lock--tag-comment-prefix-rx
  (rx "#_"
      (* " ")
      (group-n 1 (not (any " "))))
  "The regex to match #_ tag comment prefixes.")

(defun lpy-font-lock--search-comment-macro (limit)
  "Search for a comment forward stopping at LIMIT."
  (-when-let* ((_ (re-search-forward lpy-font-lock--tag-comment-prefix-rx limit t))
               (md (match-data))
               (start (match-beginning 1))
               (state (syntax-ppss start)))
    (if (lpy--in-string-or-comment? state)
        (lpy-font-lock--search-comment-macro limit)
      (goto-char start)
      (forward-sexp)
      (setf (elt md 3) (point))
      (set-match-data md)
      t)))

(defconst lpy-font-lock--kwds-tag-comment-prefix
  (list #'lpy-font-lock--search-comment-macro

        '(1 font-lock-comment-face t))
  "Support for higlighting #_(form) the form as a comment.")

;;; Syntactic Face Function
;;;; Utilities

(defun lpy-font-lock--string-is-module-docstring? (syntax)
  "Is string SYNTAX specifically a module docstring?"
  (= 1 (lpy--syntax->string-start syntax)))

(defun lpy-font-lock--string-is-function-docstring? (syntax)
  "Is string SYNTAX specifically a function docstring?"
  (-when-let (inner-symbol (lpy--syntax->inner-symbol syntax))
    (when (and (not (s-equals? "defmethod" inner-symbol))
               (s-matches? (rx "def" (not blank)) inner-symbol))
      (let ((start-point (point)))
        (save-excursion
          (lpy--goto-inner-sexp syntax)

          (-when-let* ((start (ignore-errors (scan-sexps (point) 3)))
                       (end (ignore-errors (scan-sexps (point) 4))))
            (<= start start-point end)))))))

;;;; Exposes

(defun lpy-font-lock-syntactic-face-function (syntax)
  "Return syntactic face function for synatax STATE."
  (if (lpy--in-string? syntax)
      (if (or (lpy-font-lock--string-is-module-docstring? syntax)
              (lpy-font-lock--string-is-function-docstring? syntax))
          font-lock-doc-face
        font-lock-string-face)
    font-lock-comment-face))

;;; Comint Support

(defun lpy-font-lock--kwd->comint-kwd (kwd)
  "Converts a `font-lock-keywords' KWD for `comint-mode' input fontification.

This is a rather clever solution to fontifying repl input. I wrote a post
about this idea here: http://www.modernemacs.com/post/comint-highlighting/.

The `comint-snapshot-last-prompt' call within `comint-send' is what makes
this solution tick as future attempts to font-lock prior to the current
prompt will be frozen by comint.

It actually implements comint fontification for arbitrary major modes and have
applied with success to `ielm'."
  (-let (((matcher . match-highlights) kwd))
    `((lambda (limit)
        ;; Matcher can be a function or a regex
        (when ,(if (symbolp matcher)
                   `(,matcher limit)
                 `(re-search-forward ,matcher limit t))

          ;; While the SUBEXP can be anything, this search always can use zero
          (-let ((start (match-beginning 0))
                 ((comint-last-start . comint-last-end) comint-last-prompt)
                 (state (syntax-ppss)))
            (and (> start comint-last-start)
                 ;; Make sure not in comment or string
                 ;; have to manually do this in custom MATCHERs
                 (not (or (nth 3 state) (nth 4 state)))))))

      ,@match-highlights)))

;;; Font Lock Keywords

(defconst lpy-font-lock-kwds
  (list lpy-font-lock--kwds-builtins
        lpy-font-lock--kwds-class
        lpy-font-lock--kwds-constants
        lpy-font-lock--kwds-definitions
        lpy-font-lock--kwds-decorators
        lpy-font-lock--kwds-exceptions
        lpy-font-lock--kwds-func-modifiers
        lpy-font-lock--kwds-imports
        lpy-font-lock--kwds-kwargs
        lpy-font-lock--kwds-self
        lpy-font-lock--kwds-shebang
        lpy-font-lock--kwds-special-names
        lpy-font-lock--kwds-tag-macros
        lpy-font-lock--kwds-unpacking
        lpy-font-lock--kwds-variables

        ;; Advanced kwds
        lpy-font-lock--kwds-tag-comment-prefix

        ;; Optional kwds
        (when lpy-font-lock-highlight-percent-args?
          lpy-font-lock--kwds-anonymous-funcs))
  "All Lpy font lock keywords.")

(defconst inferior-lpy-font-lock-kwds
  (-map #'lpy-font-lock--kwd->comint-kwd (-non-nil lpy-font-lock-kwds))
  "Comint-compatible version of `lpy-font-lock-kwds'.

See `lpy-font-lock--kwd->comint-kwd' for details.")

;;; Provide:

(provide 'lpy-font-lock)

;;; lpy-font-lock.el ends here
