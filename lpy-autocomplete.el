;;; lpy-autocomplete.el --- Lpy-autocomplete integration -*- lexical-binding: t -*-

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

;; Interface with the Lpy pkg `lpy-autocomplete' for IDE components.

;; Implements the following functions (follows standard naming conventions):
;; - `company-lpy'
;; - `lpy-eldoc-documentation-function'
;; - `lpy-describe-thing-at-point'
;; - `lpy-autocomplete-update-imports'

;;; Code:

(require 'lpy-base)

(require 'lpy-shell)

;;; Configuration
;;;; Configured

(defvar lpy-autocomplete--enable? t
  "Should an internal process startup for use by ide components?")

;;;; Managed

(defvar-local lpy-autocomplete--running? nil
  "Was `lpy-autocomplete' successfully started up in the current buffer?")

(defvar lpy-autocomplete--doc-lookup-buffer " *Lpy Doc Lookup Buffer"
  "The buffer name to use for documentation lookups.")

;;;; Lpy Code

;; TODO Code is one-line until I figure out how to concatenate redirected output
;; TODO Would like to auto-update namespace, but not sure how
;; TODO Would prefer to use the .set-namespace instead of remaking lpy-autocomplete instance

(defconst lpy-autocomplete--setup-code
  "(require lispy.macros *)
(try (do
       (import lpy-autocomplete)
       (= __JEDHY (lpy-autocomplete.API :locals- (locals) :globals- (globals)))
       \"Started Lpy-AC\")
     (except [Exception as E] \"Failed to start Lpy-AC\"))"
  "Text to send to internal Lpy process to setup `lpy-autocomplete'.")

(defconst lpy-autocomplete--startup-success-text "'Started lpy-autocomplete'"
  "Text identifying successful startup of lpy-autocomplete.")

(defconst lpy-autocomplete--reset-namespace-code
  "(= __JEDHY (lpy-autocomplete.API :locals- (locals) :globals- (globals)))"
  "Text to send to make Lpy-autocomplete's namespace current.")

;;; Startup

(defun lpy-autocomplete--startup ()
  "Startup lpy-autocomplete and notify its status, returning non-nil if successful."
  (lpy-shell--with-internal
    (if lpy-autocomplete--running?
        (lpy-shell--notify "Lpy-autocomplete should already be running")
      (if (s-equals? lpy-autocomplete--startup-success-text
                     (lpy-shell--redirect-send-internal lpy-autocomplete--setup-code))
          (prog1 t
            (setq-local lpy-autocomplete--running? t)
            (lpy-shell--notify "Lpy-autocomplete successfully started"))
        (lpy-shell--notify "Lpy-autocomplete failed to start")))))

;;; Namespace Management

(defconst lpy-shell--import-rgx
  (rx "(" (0+ space) (or "import" "require" "sys.path.extend"))
  "A regex used to extract importing-related forms for updating IDE features.")

(defun lpy-autocomplete-update-imports ()
  "Send imports/requires to the current internal process and updating namespace.

This is currently done manually as I'm not sure of the consequences of doing
so automatically through eg. regular intervals. Sending the imports allows
Eldoc/Company to function on packages like numpy/pandas, even if done via an
alias like (import [numpy :as np]).

Not bound atm as this is temporary, run via M-x or bind yourself."
  (interactive)
  (save-excursion
    (goto-char (point-min))

    (while (re-search-forward lpy-shell--import-rgx nil t)
      (-when-let (lpy-form (lpy--current-form-string))
        (let ((text (s-join " " (s-lines lpy-form))))
          (lpy-shell--redirect-send-internal text))))

    (lpy-shell--redirect-send-internal lpy-autocomplete--reset-namespace-code)))

;;; Dot DSL Completion

(defun lpy-autocomplete--method-call? (symbol)
  "Is SYMBOL a method call in Lpy?"
  (s-starts-with? "." symbol))

(defun lpy-autocomplete--quickfix-eldoc-dot-dsl-syntax-errors (text)
  "Quick fix to address parsing an incomplete dot-dsl."
  (if (< 1 (-> text s-lines length))
      ""
    text))

(defun lpy-autocomplete--get-inner-symbol ()
  "Get inner symbol for point, completing Lpy's method-dot DSL if applicable."
  (save-excursion
    (-when-let (inner-symbol (and (lpy--goto-inner-sexp (syntax-ppss))
                                  (not (-contains? '(?\[ ?\{) (char-before)))
                                  (thing-at-point 'symbol)))
      (if (lpy-autocomplete--method-call? inner-symbol)
          (when (ignore-errors (forward-sexp) (forward-whitespace 1) t)
            (pcase (char-after)
              ;; Can't send just .method to eldoc
              ((or ?\) ?\s ?\C-j) nil)

              ;; Dot dsl doesn't work on literals
              (?\[ (s-concat "list" inner-symbol))
              (?\{ (s-concat "dict" inner-symbol))
              (?\" (s-concat "str" inner-symbol))

              ;; Otherwise complete the dot dsl
              (_ (s-concat (thing-at-point 'symbol) inner-symbol))))
        inner-symbol))))

;;; Output Handling
;;;; Formatting

(defun lpy-autocomplete--format-output-str (output)
  "Format OUTPUT given as a string."
  (->> output
       (s-chop-prefixes '("'" "\""))
       (s-chop-suffixes '("'" "\""))))

(defun lpy-autocomplete--format-output-tuple (output)
  "Format OUTPUT given as a tuple."
  (unless (s-equals? "()" output)
    (->> output
         (s-replace-all '(("'" . "")
                          (",)" . "")  ; one element list case
                          ("(" . "")
                          (")" . "")))
         (s-split ", ")
         )))  ; comma is a valid token so can't replace it

(defun lpy-autocomplete--format-describe-output (output)
  "Converts escaped newlines to true newlines."
  (let ((kwarg-newline-regexp (rx ","
                                  (1+ (not (any "," ")")))
                                  (group-n 1 "\\\n")
                                  (1+ (not (any "," ")"))))))
    (-some-->
        output
      (s-replace "\\n" "\n" it)
      (replace-regexp-in-string kwarg-newline-regexp "newline" it nil t 1))))

;;;; Fontifying

(defun lpy-autocomplete--fontify-text (text regexp &rest faces)
  "Fontify portions of TEXT matching REGEXP with FACES."
  (when text
    (-each (s-matched-positions-all regexp text)
      (-lambda ((start . end))
        (-each faces
          (lambda (face)
            (add-face-text-property start end face nil text)))))))

(defun lpy-autocomplete--fontify-eldoc (text)
  "Fontify eldoc TEXT."
  (let ((kwd-rx
         (rx string-start (1+ (not (any space ":"))) ":"))
        (unpack-rx
         (rx (or "#*" "#**")))
        (kwargs-rx
         (rx symbol-start "&" (1+ word)))
        (quoted-rx
         (rx "`" (1+ (not space)) "`")))
    (lpy-autocomplete--fontify-text text kwd-rx 'font-lock-keyword-face)
    (lpy-autocomplete--fontify-text text unpack-rx 'font-lock-keyword-face)
    (lpy-autocomplete--fontify-text text kwargs-rx 'font-lock-type-face)
    (lpy-autocomplete--fontify-text text quoted-rx 'font-lock-constant-face 'bold-italic))
  text)

(defun lpy-autocomplete--fontify-first-docs-line (output)
  "Fontify only the first line of lpy-autocomplete OUTPUT accordding to eldoc."
  (when output
    (-let (((leader . rest) (s-lines output)))
      (s-join "\n"
              (cons (lpy-autocomplete--fontify-eldoc leader)
                    rest)))))

;;; Lpy-autocomplete Interface

(defun lpy-autocomplete--prefix-str->candidates (prefix-str)
  "Get company candidates for a PREFIX-STR."
  (unless (lpy-autocomplete--method-call? prefix-str)
    (-some->>
        prefix-str
      (format "(__JEDHY.complete \"%s\")")
      lpy-shell--redirect-send-internal
      lpy-autocomplete--format-output-tuple)))

(defun lpy-autocomplete--candidate-str->annotation (candidate-str)
  "Get company annotation for a CANDIDATE-STR."
  (-some->>
      candidate-str
    (format "(__JEDHY.annotate \"%s\")")
    lpy-shell--redirect-send-internal
    lpy-autocomplete--format-output-str))

(defun lpy-autocomplete--candidate-str->eldoc (candidate-str)
  "Get eldoc docstring for a CANDIDATE-STR."
  (-some->>
      candidate-str
    (format "(__JEDHY.docs \"%s\")")
    lpy-shell--redirect-send-internal
    lpy-autocomplete--format-output-str
    lpy-autocomplete--quickfix-eldoc-dot-dsl-syntax-errors
    lpy-autocomplete--fontify-eldoc))

(defun lpy-autocomplete--candidate-str->full-docs (candidate-str)
  "Get full, multi-line docs for a CANDIDATE-STR."
  (-some->>
      candidate-str
    (format "(__JEDHY.full-docs \"%s\")")
    lpy-shell--redirect-send-internal
    lpy-autocomplete--format-output-str
    s-chomp
    lpy-autocomplete--fontify-first-docs-line
    lpy-autocomplete--format-describe-output))

;;; Describe thing at point

(defun lpy-describe-thing-at-point ()
  "Describe symbol at point with help popup buffer.

Retrieves full documentation, with firstline formatted same as eldoc, in a
popup buffer.

.Does not (yet) complete the dot-dsl like Eldoc does currently.

Spacemacs users maybe be familiar with this functionality via
shift-K keybinding that executes `spacemacs/evil-smart-doc-lookup'."
  (interactive)
  (-when-let (text (lpy-autocomplete--candidate-str->full-docs (thing-at-point 'symbol)))
    (unless (s-blank-str? text)
      (with-current-buffer (get-buffer-create lpy-autocomplete--doc-lookup-buffer)
        (erase-buffer)
        (switch-to-buffer-other-window lpy-autocomplete--doc-lookup-buffer)

        (insert text)

        (when (< 1 (length (s-lines text)))
          (goto-char (point-min))
          (forward-line)
          (newline)
          (insert "------")
          (fill-region (point) (point-max)))

        (goto-char (point-min))

        ;; TODO This can be in a better way I'm assuming
        (local-set-key "q" #'quit-window)
        (when (fboundp #'evil-local-set-key)
          (evil-local-set-key 'normal "q" #'quit-window))))))

;;; Eldoc

(defun lpy-eldoc-documentation-function ()
  "Drives `eldoc-mode', retrieves eldoc msg string for inner-most symbol."
  (lpy-autocomplete--candidate-str->eldoc (lpy-autocomplete--get-inner-symbol)))

;;; Company

(defun company-lpy (command &optional prefix-or-candidate-str &rest ignored)
  "Implements autocompletion for `lpy-mode'."
  (interactive (list 'interactive))

  (when (and (memq major-mode '(lpy-mode inferior-lpy-mode))
             (lpy-shell--live-internal?))
    (cl-case command
      (prefix (unless (company-in-string-or-comment)
                (company-grab-symbol)))
      (candidates (lpy-autocomplete--prefix-str->candidates prefix-or-candidate-str))
      (annotation (lpy-autocomplete--candidate-str->annotation prefix-or-candidate-str))
      (meta (lpy-autocomplete--candidate-str->eldoc prefix-or-candidate-str)))))

;;; Run Lpy-autocomplete

(defun run-lpy-autocomplete--pyvenv-post-deactive-hook ()
  "Kill lpy-autocomplete without notifying and possibly rerun for global context."
  (let ((lpy-shell--notify?))
    (lpy-shell--kill-internal)

    ;; The activation hook handles switching the environment rerunning lpy-autocomplete
    (unless pyvenv-virtual-env-name
      (run-lpy-autocomplete))))

;;;###autoload
(defun run-lpy-autocomplete ()
  "Startup internal Lpy interpreter process, enabling lpy-autocomplete for `company-mode'."
  (interactive)

  (lpy-shell--with-internal
    (lpy-autocomplete--startup)))

;;; Provide:

(provide 'lpy-autocomplete)

;;; lpy-autocomplete.el ends here
