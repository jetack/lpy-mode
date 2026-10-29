;;; lpy-mode-test.el --- Lpy Mode Tests -*- lexical-binding: t -*-

;; `lpy-mode' is well-tested.
;; More-or-less all components are tested, including process-based components.

;; See `lpy-test.el' for extensions made to Buttercup and other utilities.

;; Covered:
;; - Indentation
;; - Font Locks
;; - Syntax
;;   - Syntax Table Components
;;   - Docstring Detection
;;   - Context Sensitive (Bracket Strings)
;; - Shell support
;;   - Startup/Teardown
;;   - Font Locks
;;   - Sending and getting from running processes
;; - Lpy-autocomplete (within Lpy-autocomplete repo, not here)
;; - Misc
;;   - Form extraction

;;; Load lpy-test and lpy-mode

(progn (require 'f)
       (add-to-list 'load-path (f-parent (f-parent (f-this-file))))
       (require 'lpy-test)

       ;; Sets up process-based tests if we have a Lpy interpreter to use
       (lpy-test--setup-env))

;;; Indentation

(describe "Indentation"
  (before-all (lpy-mode--setup-syntax))

  ;; ~~
  ;; NORMAL INDENT
  ;; ~~

  (describe "normal indent"

    (it "opening line has one sexp - indent doesnt carry"
      (expect "
(a
  b
  c)
" :indented))

    (it "opening line has one sexp - indent doesnt carry - with empty lines"
      (expect "
(a

  b

  c)
" :indented))

    (it "opening line has two sexps - indent carries"
      (expect "
(a b
   c)
" :indented))

    (it "opening line has many sexps - indent carries"
      (expect "
(a b c d
   e)
" :indented))

    (it "opening line has many sexps - indent carries - with empty lines"
      (expect "
(a b c d

   e

   f g
   h)
" :indented))

    (it "with nested forms"
      (expect "
(a b (c
       d)
   e (f g h
        i)

   j)
" :indented)))

  ;; ~~
  ;; SYNTAX TABLE
  ;; ~~

  (describe "of syntax table"

    (describe "quote chars"
      (it "backtick"
        (expect "
(a `b
   c)" :indented))

      (it "tick"
        (expect "
(a 'b
   c)" :indented))

      (it "tilde"
        (expect "
(a 'b
   c)" :indented))

      (it "tilde+@ ie. unquote-splice"
        (expect "
(a ~@b
   c)" :indented)))

    (describe "prefix chars"
      (it "dot"
        (expect "
(a .b
   c)" :indented))

      (it "hashtag"
        (expect "
(a #b
   c)" :indented))

      (it "minus sign"
        (expect "
(a -b
   c)" :indented))

      (it "colon ie. keyword"
        (expect "
(a :b
   c)" :indented)))

    (describe "lists"
      (it "list - only sexp on line"
        (expect "
[a
 b]" :indented))

      (it "list - many sexp on line"
        (expect "
[a b
 c d

 e]" :indented))

      (it "list - opening sexp is kwd"
        (expect "
[:a
 b]" :indented)))

    (it "tuple constructor"
      (expect "
(,
  a)
(, a
   b)
(, a b
   c)
" :indented))

    ;; FIXME Not bothering fixing atm because when will anyone ever do this?
    ;; It would make `lpy-indent--normal' slightly more obfuscated, a bad thing.
    (xit "matmul operator @"
      (expect "
(@
  a)
(@ a
   b)
" :indented))

    (it "or operator"
      (expect "
(|
  a)
(| a
   b)
(| a b
   c)
" :indented)))

  ;; ~~
  ;; OTHER CASES
  ;; ~~

  (describe "nonstandard cases"
    (it "form opens a form - only sexp on line"
      (expect "
((a b)
  c)
" :indented))

    (it "form opens a form - two+ sexps on line"
      (expect "
((a b) c
 d)
" :indented))

    (it "tag macro opens a form - only sexp on line"
      (expect "
(#a
  c)" :indented))

    (it "tag macro opens a form - two+ sexps on line"
      (expect "
(#a b
    c)" :indented))

    (it "quoted symbol opens a form - only sexp on line"
      (expect "
('a
  c)" :indented))

    (it "quoted symbol opens a form - two+ sexps on line"
      (expect "
('a b
    c)" :indented))

    (it "tilde symbol opens a form - two+ sexps on line"
      (expect "
(~a b
    c)" :indented))

    (it "tilde symbol opens a form - only sexp on line"
      (expect "
(~a
  c)" :indented))

    (it "unquote-spliced symbol opens a form - two+ sexps on line"
      (expect "
(~@a b
     c)" :indented))

    (it "unquote-spliced symbol opens a form - only sexps on line"
      (expect "
(~@a
  c)" :indented))
    )

  ;; ~~
  ;; SPECIAL INDENT RULES
  ;; ~~

  (describe "special indent rules"
    (it "handles exact matches"
      (let ((lpy-indent--exactly '("foo")))
        (expect "
(foo
  a)
(foo a
  b)
(fooo a
      b)
" :indented)))

    (it "handles fuzzy matches"
      (let ((lpy-indent--fuzzily '("foo")))
        (expect "
(foo
  a)
(foo a
  b)
(fooo a
  b)
" :indented))))

  ;; ~~
  ;; BRACKET STRINGS
  ;; ~~

  (describe "bracket strings never indent"
    (it "all lines have chars"
      (expect "
(#[[a
b]])" :indented))

    (it "not all lines have chars"
      (expect "
(#[[a

b

c]])" :indented))

    (it "has matching delims"
      (expect "
(#[-[a

b

c]-])" :indented))

    (it "has not matching delims"
      (expect "
(#[-+[a

b

c]+-])" :indented))

    (it "only applies when two brackets, not just 1"
      (expect "
(#[no second bracket
   => not a string])
") :indented)))

;;; Font Lock

(describe "Font Lock face application"
  (it "to builtins"
    (expect "«b:+» «b:map» map-foo map-do map.foo" :faces))

  (it "to constants"
    (expect "«c:True» Truex xTrue" :faces))

  (it "to function defs"
    (expect "(«k:def» «f:func» []) (defx func) (xdef func)" :faces))

  (it "to exceptions"
    (expect "«t:ValueError» ValueErrorx xValueError" :faces))

  (it "to special forms"
    (expect "«k:->» «k:for» forx xfor for.x x.for" :faces))

  (it "to class definitions"
    (expect "(«k:class» «t:Klass» [parent])" :faces))

  (it "to deco decorator"
    (expect "(«t:deco a-dec» («k:def» «f:func-def» []))" :faces))

  (it "to imports"
    (expect "(«k:import» x [y «k::as» z])" :faces))

  (it "to requires"
    (expect "(«k:require» x [y «k::as» z])" :faces))

  (it "to self kwd"
    (expect "«k:self» x «k:self».x self-x x-self" :faces))

  (it "to tag macros"
    (expect "«f:#tag-x» y" :faces))

  (it "to variables"
    (expect "(«b:=» «v:foo» 1)" :faces))

  (it "to #% anon funcs"
    (let ((lpy-font-lock-highlight-percent-args? t))
      (expect "«v:%1» «v:%12» «v:%1».foo %foo %1foo «b:%»" :faces)))

  (it "to function &rest and &kwargs"
    (expect "«t:&rest» args «t:&kwargs» kwargs" :faces))

  (it "to kwargs"
    (expect "«c::kwarg-1» foo «c::kwarg-2»" :faces))

  (it "to shebang"
    (expect "«x:#!shebang»\ncode" :faces))

  (it "to #* and #** unpacking sugar"
    (expect "«k:#*» args «k:#**» kwargs" :faces)))

;;; Syntax

(describe "Syntax Table"
  (describe "comments"
    (it "single char"
      (expect "foo «x:; bar»" :faces))

    (it "two+ chars"
      (expect "foo «m:;; »«x:bar»" :faces)))

  (describe "context sensitive"
    (describe "bracket string literals"
      (it "fence set one line"
        (expect "#[«s:[foo]»]" :faces))

      (it "fence set many lines"
        (expect "#[«s:[foo\n\nbar]»]" :faces))

      (it "fence set with quote chars"
        (expect "#[«s:[\"foo\"]»]" :faces))

      (it "fence set with matching delims"
        (expect "#[delim«s:[foo]»delim]" :faces))

      (it "fence set with different delims"
        (expect "#[delim-1«s:[foo]»delim-2]" :faces))

      (it "many bracket strings"
        (expect "#[«s:[foo]»] \nfoo \n #[«s:[foo]»]" :faces))))

  (describe "characters"
    (before-all (set-syntax-table lpy-mode-syntax-table))
    (after-each (delete-region (point-min) (point-max)))

    (describe "symbols"
      (it "dots"
        (insert "foo.bar")
        (expect (thing-at-point 'symbol) :to-equal "foo.bar"))

      (it "dashes"
        (insert "foo-bar")
        (expect (thing-at-point 'symbol) :to-equal "foo-bar"))

      (it "hashtag"
        (insert "#foo")
        (expect (thing-at-point 'symbol) :to-equal "#foo")))

    (describe "quotes"
      (it "tick"
        (insert "'foo")
        (expect (thing-at-point 'symbol) :to-equal "foo"))

      (it "backtick"
        (insert "`foo")
        (expect (thing-at-point 'symbol) :to-equal "foo"))

      (it "tilde"
        (insert "~foo")
        (expect (thing-at-point 'symbol) :to-equal "foo"))

      (it "unquote-splice"
        (insert "~@foo")
        (expect (thing-at-point 'symbol) :to-equal "foo")))))

;;; Docstrings

(describe "Docstring Detection"
  (it "distinguishes module docstrings"
    (expect "«d:\"foo\"»" :faces)
    (expect " «s:\"foo\"»" :faces))

  (it "strings identify parent form"
    (expect "(«k:def» «f:foo» [] «d:\"bar\"»)" :faces)
    (expect "(«k:def» «f:foo» [] [«s:\"bar\"»])" :faces))

  (it "detects async-def docstrings"
    (expect "(«k:async-def» «f:foo» [] «d:\"bar\"»)" :faces))

  (it "detects class docstrings"
    (expect "(«k:class» «t:Klass» [] «d:\"doc\"»)" :faces))

  (it "ignores calls to symbols merely containing def"
    (expect "(undefined x y «s:\"bar\"»)" :faces))

  (it "detects a docstring after a return annotation"
    (expect (lpy-test--last-string-face "(def f [x] ^float \"doc\")")
            :to-equal 'font-lock-doc-face))

  (it "detects a docstring after a compound return annotation"
    (expect (lpy-test--last-string-face "(def f [x] ^(sub list int) \"doc\")")
            :to-equal 'font-lock-doc-face))

  (it "detects a docstring after a glued return annotation"
    (expect (lpy-test--last-string-face "(def f [x] ^list[int] \"doc\")")
            :to-equal 'font-lock-doc-face)
    (expect (lpy-test--last-string-face "(def f [x] ^Callable[[int] str] \"doc\")")
            :to-equal 'font-lock-doc-face))

  (it "treats a lone ^ as a body form, not an annotation"
    (expect (lpy-test--last-string-face "(def f [x] ^\"Foo\" \"doc\")")
            :to-equal 'font-lock-string-face)
    (expect (lpy-test--last-string-face "(def f [x] ^ int \"doc\")")
            :to-equal 'font-lock-string-face))

  (it "detects a docstring after glued type parameters on a def"
    (expect "(«k:def» «f:f»[T] [x] «d:\"doc\"»)" :faces))

  (it "detects a docstring after glued type parameters on a class"
    (expect "(«k:class» «t:K»[T] [Base] «d:\"doc\"»)" :faces))

  (it "has only first string of a def as the docstring"
    (expect "(«k:def» «f:foo» [] «d:\"bar\"» «s:\"baz\"»)" :faces)))

;;; Form Captures

(describe "Form Capturing"
  (before-all (lpy-mode--setup-syntax))
  (after-each (delete-region (point-min) (point-max)))

  (describe "current form"
    (it "captures a simple parenthetical form"
      (setq text "(foo bar)")
      (insert text)
      (backward-char)

      (expect (lpy--current-form-string) :to-equal (s-concat text "\n")))

    (it "captures a simple list form"
      (setq text "[foo bar]")
      (insert text)
      (backward-char)

      (expect (lpy--current-form-string) :to-equal (s-concat text "\n")))

    (it "captures nested forms"
      (setq text "[foo (foo bar) bar]")
      (setq paren-start 6)
      (insert text)
      (backward-char)

      (expect (lpy--current-form-string) :to-equal (s-concat text "\n"))
      (goto-char (1+ paren-start))
      (expect (lpy--current-form-string) :to-equal (s-concat "(foo bar)" "\n"))))

  (describe "last sexp"
    (it "captures a simple parenthetical form"
      (setq text "(foo bar)")
      (insert text)

      (expect (lpy--last-sexp-string) :to-equal (s-concat "(foo bar)" "\n")))

    (it "captures nested forms"
      (setq text "[foo (foo bar) bar]")
      (setq paren-end 14)
      (insert text)

      (expect (lpy--last-sexp-string) :to-equal (s-concat text "\n"))
      (goto-char (1+ paren-end))
      (expect (lpy--last-sexp-string) :to-equal (s-concat "(foo bar)" "\n")))))

;;; nREPL Eval Position

(describe "nREPL eval position"
  :var (sent)
  (before-all (lpy-mode--setup-syntax))
  (before-each
    (setq sent nil)
    (spy-on 'lpy-nrepl--send :and-call-fake (lambda (msg &rest _) (setq sent msg)))
    (spy-on 'buffer-file-name :and-return-value "/tmp/proj/app.lpy"))
  (after-each (delete-region (point-min) (point-max)))

  (cl-flet ((fields () (list (alist-get 'file sent) (alist-get 'line sent)
                             (alist-get 'column sent))))
    (it "sends the form start for current-form"
      (insert "x = 1\n  (foo bar)")
      (backward-char)
      (lpy-nrepl-eval-current-form)
      (expect (alist-get 'code sent) :to-equal "(foo bar)")
      (expect (fields) :to-equal '("/tmp/proj/app.lpy" 2 3)))

    (it "sends the sexp start for last-sexp"
      (insert "\n\n(a)  (foo bar)")
      (lpy-nrepl-eval-last-sexp)
      (expect (alist-get 'code sent) :to-equal "(foo bar)")
      (expect (fields) :to-equal '("/tmp/proj/app.lpy" 3 6)))

    (it "counts from the first character after the trim for a region"
      (insert "x = 1\n   \n  (foo bar)\n")
      (lpy-nrepl-eval-region 7 (point-max))
      (expect (alist-get 'code sent) :to-equal "(foo bar)")
      (expect (fields) :to-equal '("/tmp/proj/app.lpy" 3 3)))

    (it "handles a multi-line region starting mid-line"
      (insert "x = 1; (foo\n  bar)")
      (lpy-nrepl-eval-region 8 (point-max))
      (expect (alist-get 'code sent) :to-equal "(foo\n  bar)")
      (expect (fields) :to-equal '("/tmp/proj/app.lpy" 1 8)))

    (it "sends line 1 column 1 for the buffer contents"
      (insert "(foo)\n(bar)\n")
      (lpy-nrepl-eval-buffer-contents)
      (expect (fields) :to-equal '("/tmp/proj/app.lpy" 1 1)))

    (it "omits file for a buffer without one"
      (spy-on 'buffer-file-name :and-return-value nil)
      (insert "(foo)")
      (lpy-nrepl-eval-buffer-contents)
      (expect (assq 'file sent) :to-be nil)
      (expect (alist-get 'line sent) :to-equal 1))))

;;; Shell

(nt-shell-describe "Shell"
  (describe "startup"
    (before-all (lpy-test--run-lpy))
    (after-all (lpy-shell--kill))

    (it "sets the name and switches"
      (expect (buffer-name) :to-equal lpy-shell--buffer-name))

    (it "sets inferior-lpy-mode"
      (expect major-mode :to-be 'inferior-lpy-mode))

    (it "starts the process and associates with the buffer"
      (expect (process-live-p (lpy-shell--current-process)))))

  (describe "teardown"
    (before-each (lpy-test--run-lpy))
    (after-each (lpy-shell--kill))

    (it "kills the buffer and process"
      (expect (lpy-shell--live?))
      (expect (lpy-shell--kill))
      (expect (lpy-shell--live?) :nil)
      (expect (lpy-shell--kill) :nil)))

  ;; No need to copy over all the different face tests, as these are built as
  ;; transforms of those keywords, rather than being new keywords entirely.
  (describe "font locking"
    (it "fontifies shell input"
      (expect "(«k:defn» «f:foo» [] «d:\"bar\"»)" :shell-faces)))

  (describe "sending redirected"
    (before-all (lpy-test--run-lpy))
    (after-all (lpy-shell--kill))

    (it "extracts output properly"
      ;; Depends on the interpreter args, improving this
      (expect (lpy-shell--redirect-send "(+ 1 2)") :to-equal
              "1 + 2

3"))))
