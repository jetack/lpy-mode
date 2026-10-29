;;; lpy-base.el --- Common Utils -*- lexical-binding: t -*-

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

;; Common requires and utilities for `lpy-mode'.

;; Mostly just methods for syntax states and sexp traversals.

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 's)

;;; Syntax Methods
;;;; `syntax-ppss' and `parse-partial-sexp' aliases
;;;;; Positions

(defun lpy--syntax->inner-char (syntax)
  "Get innermost char of SYNTAX."
  (nth 1 syntax))

(defun lpy--syntax->last-sexp-start (state)
  "Return start of last sexp of syntax STATE."
  (nth 2 state))

(defun lpy--syntax->string-start (syntax)
  "Return start of STATE that is in a string."
  (nth 8 syntax))

(defun lpy--syntax->inner-symbol (syntax)
  "Get innermost sexp of SYNTAX."
  (save-excursion
    (when (lpy--goto-inner-sexp syntax)
      (thing-at-point 'symbol))))

;;;;; Predicates

(defun lpy--in-string? (state)
  "Is syntax STATE in a string?"
  (nth 3 state))

(defun lpy--in-string-or-comment? (state)
  "Is syntax STATE in a string or comment?"
  (or (nth 3 state) (nth 4 state)))

(defun lpy--prior-sexp? (state)
  "Is there a prior sexp from syntax STATE?"
  (number-or-marker-p (lpy--syntax->last-sexp-start state)))

;;;; Gotos

(defun lpy--goto-inner-char (syntax)
  "Goto innermost char of SYNTAX."
  (-some-> syntax lpy--syntax->inner-char goto-char))

(defun lpy--goto-inner-sexp (syntax)
  "Goto innermost sexp of SYNTAX."
  (-some-> syntax lpy--syntax->inner-char 1+ goto-char))

(defun lpy--goto-last-sexp-start (syntax)
  "Goto start of last sexp of SYNTAX."
  (-some-> syntax lpy--syntax->last-sexp-start goto-char))

;;; Form Captures

(defun lpy--current-form-bounds ()
  "Get (START . END) of the form containing current point."
  (save-excursion
    (-when-let (start (lpy--goto-inner-char (syntax-ppss)))
      (while (ignore-errors (forward-sexp)))
      (cons start (point)))))

(defun lpy--last-sexp-bounds ()
  "Get (START . END) of the form containing last s-exp point."
  (save-excursion
    (-when-let (start (lpy--goto-last-sexp-start (syntax-ppss)))
      (while (ignore-errors (forward-sexp)))
      (cons start (point)))))

(defun lpy--bounds-string (bounds)
  "Get the text of BOUNDS, a (START . END) cons, plus a trailing newline."
  (-when-let ((start . end) bounds)
    (s-concat (buffer-substring-no-properties start end) "\n")))

(defun lpy--current-form-string ()
  "Get form containing current point as string plus a trailing newline."
  (lpy--bounds-string (lpy--current-form-bounds)))

(defun lpy--last-sexp-string ()
  "Get form containing last s-exp point as string plus a trailing newline."
  (lpy--bounds-string (lpy--last-sexp-bounds)))

;;; Provide:

(provide 'lpy-base)

;;; lpy-base.el ends here
