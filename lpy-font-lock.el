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

(defconst lpy-font-lock--docstring-forms
  (append lpy-font-lock--definitions '("class"))
  "Forms whose first string after the signature is a docstring.")

;;;; Operators

(defconst lpy-font-lock--operators
  '("!=" "%" "%=" "&" "&=" "*" "**" "**=" "*=" "+" "+=" "-"
    "-=" "/" "//" "//=" "/=" "<" "<<" "<<=" "<=" "=" "==" ">" ">=" ">>" ">>="
    "^" "^=" "|" "|=" ":=" ".")
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

    ;; Subscript
    "sub"

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

(defconst lpy-font-lock--symbol-token-rx
  (rx (1+ (not (any " \t\n\r\f" "()[]{}\",;"))))
  "A Lispython symbol token, dotted names like `functools.cache' included.

Mirrors the symbol pattern of the tokenizer (`_TOKEN_PATTERNS' in
lispy/core/parser.py) rather than Emacs word or symbol syntax, which
counts `,' as a symbol constituent here.")

(defun lpy-font-lock--decorator-forms-end ()
  "Return the end of the decorator form at point, or nil when unbalanced."
  (save-excursion
    (ignore-errors (forward-sexp) (point))))

(defun lpy-font-lock--match-decorator-name (limit)
  "Match the next decorator name before LIMIT in a `deco' form.

Point is inside the `[...]' of decorators or at the `(...)' of a single
call decorator.  A decorator is a name or a call; only the name, or the
call's head, is matched - never the call's arguments.  Point ends past
the whole decorator so the next call resumes at its sibling."
  ;; Every call either moves point forward or returns nil: font-lock
  ;; calls an anchored matcher again for as long as it returns non-nil
  ;; with point before LIMIT, so a match that leaves point in place
  ;; (an unbalanced call whose `forward-sexp' fails) hangs Emacs.
  (let (found)
    (while (and (not found)
                (progn (forward-comment (buffer-size)) (< (point) limit))
                (not (eq (char-syntax (char-after)) ?\))))
      (cond
       ((eq (char-after) ?\()
        (save-excursion
          (forward-char)
          (forward-comment (buffer-size))
          (when (looking-at lpy-font-lock--symbol-token-rx)
            (setq found (match-data)))))
       ((looking-at lpy-font-lock--symbol-token-rx)
        (setq found (match-data))))
      (unless (ignore-errors (forward-sexp) t)
        (goto-char limit)))
    (if found
        (progn (set-match-data found) t)
      (goto-char limit)
      nil)))

(defconst lpy-font-lock--kwds-decorators
  (list
   (rx-to-string
    `(: symbol-start
        "deco"
        symbol-end
        (1+ space)
        (regexp ,lpy-font-lock--symbol-token-rx)))

   '(0 font-lock-type-face))
  "Lpylight the symbol after `deco' macros keywords.")

(defconst lpy-font-lock--kwds-decorator-groups
  (list
   (rx symbol-start
       (group-n 1 "deco")
       symbol-end
       (1+ space)
       (group-n 2 (any "([")))

   '(1 font-lock-type-face)
   '(lpy-font-lock--match-decorator-name
     (progn
       (goto-char (match-beginning 2))
       (prog1 (lpy-font-lock--decorator-forms-end)
         (when (eq (char-after) ?\[)
           (forward-char))))
     nil
     (0 font-lock-type-face)))
  "Lpylight `deco' with a decorator list or call, and each decorator name.")

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
  (-when-let* ((md (and (re-search-forward lpy-font-lock--tag-comment-prefix-rx limit t)
                         (match-data)))
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

(defun lpy-font-lock--skip-glued-bracket ()
  "Skip a `[' glued to the symbol before point, as in `f[T]' or `^list[int]'.

Lispython's parser glues a bracket that starts exactly where a symbol
ends into one form with it, so `[T]' is not a separate sexp there.  A
bracket after a closing delimiter or whitespace is a form of its own."
  (when (and (eq (char-after) ?\[)
             (memq (char-syntax (char-before)) '(?w ?_)))
    (forward-sexp)))

(defun lpy-font-lock--annotation-at-point? ()
  "Does a return annotation start at point?

Mirror Lispython's tokenizer: `^' makes an annotation only when the
annotated form follows it directly - `^int', `^(sub list int)',
`^[...]', `^{...}', `^\\='x'.  A `^' followed by whitespace, a comma
\(whitespace to Lispython), a string or a closing delimiter is the
plain symbol `^', so it is itself the first body form."
  (and (eq (char-after) ?^)
       (let ((next (char-after (1+ (point)))))
         (and next
              (not (memq next '(?\" ?, ?\) ?\] ?\})))
              (not (memq (char-syntax next) '(?\s ?>)))))))

(defun lpy-font-lock--skip-token-filler ()
  "Skip what Lispython's tokenizer drops between tokens.

That is whitespace, newlines, comments and commas; a comma is
whitespace to Lispython except right after `(' or `{'."
  (while (progn (forward-comment (buffer-size))
                (eq (char-after) ?,))
    (forward-char)))

(defun lpy-font-lock--skip-annotation ()
  "Move past the return annotation that starts at point.

Point is where `lpy-font-lock--annotation-at-point?' holds.  The
annotation's extent mirrors Lispython's tokenizer (`_TOKEN_PATTERNS'
in lispy/core/parser.py) instead of trusting `forward-sexp': Emacs
continues a symbol over expression-prefix characters, so a scan from
`^' in front of a quoted, quasiquoted, unquoted or starred list stops
just before that list.  The tokenizer reads `^', up to two `*', then
a chain of meta indicators (quote, backquote, `~@', `~') each followed
by a non-space, then a bracketed form, a string or a symbol token.
The form a meta indicator wraps is the next token, past any comma or
comment, so `^\\=',x' wraps `x' and a string on the line after
`^\\=';c' is the annotation, not the docstring.
A prefix that ends in symbol characters runs on into the symbol
token, which stops at whitespace, a closing delimiter, a comma, a
string or a `[' - so `^\\=' x' and `^* \"x\"' are the bare symbols
`^\\='' and `^*'."
  (forward-char)                        ; `^'
  (let ((in-symbol t))
    (skip-chars-forward "*" (+ (point) 2))
    (while (and (looking-at "~@\\|['`~]")
                (let ((next (char-after (match-end 0))))
                  (and next (not (memq (char-syntax next) '(?\s ?>))))))
      (goto-char (match-end 0))
      (lpy-font-lock--skip-token-filler)
      (setq in-symbol nil)
      (when (looking-at "\\*\\{1,2\\}")
        (goto-char (match-end 0))
        (setq in-symbol t)))
    (cond
     ((looking-at "[({[]")                ; opening token
      (forward-sexp))
     ((and (not in-symbol) (looking-at "\\w*\""))  ; string token
      (goto-char (1- (match-end 0)))
      (forward-sexp))
     (t                                   ; rest of a symbol token
      (skip-chars-forward "^ \t\n\r\f)}]\"[,")))))

(defun lpy-font-lock--first-body-form-start (head)
  "Return the start of the first body form of the HEAD form after point.

Point is just inside the form's opening paren.  Return nil when the
form ends before a body form.

The body does not sit at a fixed sexp index, so the signature is
walked piece by piece:
- type parameters glued to the name, `f[T]', scan as a second sexp;
- `def', `async-def' and `defmacro' (which expands to `def') take an
  optional return annotation before the body, skipped by
  `lpy-font-lock--skip-annotation'; `^list[int]' leaves a glued
  bracket after it.  `class' takes none: the compiler only strips a
  leading annotation from function bodies."
  (ignore-errors
    (forward-sexp 2)                    ; head and name
    (lpy-font-lock--skip-glued-bracket) ; type parameters
    (forward-sexp)                      ; arguments or bases
    (forward-comment (buffer-size))
    (when (and (member head lpy-font-lock--definitions)
               (lpy-font-lock--annotation-at-point?))
      (lpy-font-lock--skip-annotation)
      (lpy-font-lock--skip-glued-bracket)
      (forward-comment (buffer-size)))
    (unless (eq (char-syntax (char-after)) ?\))
      (point))))

(defun lpy-font-lock--string-is-function-docstring? (syntax)
  "Is string SYNTAX specifically a function docstring?"
  (-when-let (inner-symbol (lpy--syntax->inner-symbol syntax))
    (when (member inner-symbol lpy-font-lock--docstring-forms)
      (save-excursion
        (lpy--goto-inner-sexp syntax)
        (eql (lpy-font-lock--first-body-form-start inner-symbol)
             (lpy--syntax->string-start syntax))))))

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
        lpy-font-lock--kwds-decorator-groups
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
