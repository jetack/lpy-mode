;;; lpy-shell.el --- Shell and Process Support -*- lexical-binding: t -*-

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

;; Shell and process functionality for Lpy.

;; This file implements `inferior-lpy-mode', commands to send and get text
;; from Lpy interpreters, and other repl components.

;; See `lpy-autocomplete.el' which builds on these commands to support IDE components.

;;; Code:

(require 'lpy-base)

(require 'lpy-font-lock)

;;; Configuration
;;;; Configured

(defvar lpy-shell--interpreter "lpy"
  "Default Lpy interpreter name.")

(defvar lpy-shell--interpreter-args '("-t")
  "Default argument list to pass to the Lpy interpreter.")

(defvar lpy-shell--enable-font-lock? t
  "Whether the shell should font-lock repl prompt input.")

(defvar lpy-shell--notify? t
  "Allow Lpy to message on failure to find Lpy, instantiation, shutdown, etc?")

(defvar lpy-shell--redirect-timeout 0.5
  "Seconds (float) to allow redirection commands to complete before quitting.")

;;;; Managed

(defconst lpy-shell--name "Lpy"
  "The name to use for the Lpy interpreter process.")

(defconst lpy-shell--name-internal (format "%s Internal" lpy-shell--name)
  "The name to use for the internal Lpy interpreter process.")

(defconst lpy-shell--buffer-name (s-concat "*" lpy-shell--name "*")
  "The buffer name to use for the Lpy interpreter process.")

(defconst lpy-shell--buffer-name-internal (s-concat "*" lpy-shell--name-internal "*")
  "The buffer name to use for the internal Lpy interpreter process.")

(defvar lpy-shell--redirect-output-buffer " *Lpy Comint Redirect Buffer"
  "The buffer name to use for comint redirection of text sending commands.")

;;; Macros

(defmacro lpy-shell--with (&rest body)
  "Run BODY for Lpy process, starting up if needed."
  (declare (indent 0))
  `(when (lpy-shell--check-installed?)
     (with-current-buffer (get-buffer-create lpy-shell--buffer-name)
       (lpy-shell--make-comint)
       ,@body)))

(defmacro lpy-shell--with-internal (&rest body)
  "Run BODY for internal Lpy process, starting up if needed."
  (declare (indent 0))
  `(when (lpy-shell--check-installed?)
     (with-current-buffer (get-buffer-create lpy-shell--buffer-name-internal)
       (lpy-shell--make-comint-internal)
       ,@body)))

(defmacro lpy-shell--with-live (&rest body)
  "Run BODY for Lpy process, when it's alive."
  (declare (indent 0))
  `(when (lpy-shell--live?)
     (lpy-shell--with ,@body)))

(defmacro lpy-shell--with-internal-live (&rest body)
  "Run BODY for internal Lpy process, when it's alive."
  (declare (indent 0))
  `(when (lpy-shell--live-internal?)
     (lpy-shell--with-internal ,@body)))

;;; Process Management
;;;; Utilities

(defun lpy-shell--live? ()
  "Is the Lpy intereprter process alive?"
  (get-buffer-process lpy-shell--buffer-name))

(defun lpy-shell--live-internal? ()
  "Is the internal Lpy intereprter process alive?"
  (get-buffer-process lpy-shell--buffer-name-internal))

(defun lpy-shell--current-process ()
  "Run `get-buffer-process' on the `current-buffer'."
  (get-buffer-process (current-buffer)))

(defun lpy-shell--internal? ()
  "Is current buffer for an internal Lpy interpreter process?"
  (s-equals? (buffer-name) lpy-shell--buffer-name-internal))

(defun lpy-shell--format-startup-command ()
  "Format Lpy shell startup command."
  (let ((prog (shell-quote-argument lpy-shell--interpreter))
        (switches (->> lpy-shell--interpreter-args
                     (-map #'shell-quote-argument)
                     (s-join " "))))
    (if (lpy-shell--internal?)
        prog
      (format "%s %s" prog switches))))

;;;; Creation

(defun lpy-shell--make-comint ()
  "Create Lpy shell comint process in current-buffer."
  (unless (process-live-p (lpy-shell--current-process))
    (-let (((program . switches)
            (split-string-and-unquote (lpy-shell--format-startup-command)))
           (name
            (if (lpy-shell--internal?) lpy-shell--name-internal lpy-shell--name)))
      (apply #'make-comint-in-buffer name nil program nil switches)

      (unless (derived-mode-p 'inferior-lpy-mode)
        (inferior-lpy-mode))

      ;; Get shell's initial output/prompt
      (accept-process-output (lpy-shell--current-process) 0.5)

      (lpy-shell--current-process))))

(defun lpy-shell--make-comint-internal ()
  "Run `lpy-shell--make-comint' with additional setup for internal processes."
  (let ((lpy-shell--enable-font-lock?))
    (-when-let (proc (lpy-shell--make-comint))
      (set-process-query-on-exit-flag proc nil)
      proc)))

;;; Redirected Sending
;;;; Commentary

;; Maybe in the future I build an nrepl or lsp implementation. Until that day,
;; interacting with Lpy's running processes programatically is done through the
;; `lpy-shell--redirect-send' and friends commands.

;; They are rewrites of some components of comint's redirection commands.
;; The redirection add-on for comint was developed to run SQL on a process
;; with state. Similarly, we maintain state in lpy-autocomplete's namespace. There
;; are better, but more advanced, solutions. The one chosen should allow a
;; pretty quick, and !easily testable!, integration of lpy-autocomplete. It also allows
;; some fancier things w.r.t shell output transformations and fontifying.

;; The commands are rewritten because 1. we don't need all the options 2. we
;; require a timeout during the accept process output 3. we have some macros
;; that simplify things 4. easy to test this way.

;;;; Implementation

(defun lpy-shell--redirect-check-prompt-regexp ()
  "Avoid infinite loop in redirect if `comint-prompt-regexp' badly defined."
  (when comint-redirect-perform-sanity-check
    (save-excursion
	    (goto-char (point-max))
	    (or (re-search-backward comint-prompt-regexp nil t)
		      (error "No prompt found or `comint-prompt-regexp' not set properly")))))

(defun lpy-shell--redirect-send-1 (text)
  "Internal implementation of `comint-redirect-send-command-to-process'.

Expected to be called within a Lpy interpreter process buffer."
  (lpy-shell--redirect-check-prompt-regexp)

  (let ((buffer (current-buffer))
        (output-buffer lpy-shell--redirect-output-buffer)
        (process (lpy-shell--current-process))
        (timeout lpy-shell--redirect-timeout))
    ;; Setup local vars for the filter, temporarily overwrite comint filters
    (comint-redirect-setup output-buffer buffer comint-prompt-regexp)
    (add-function :around (process-filter process) #'comint-redirect-filter)

    (process-send-string buffer (s-concat text "\n"))
    (while (and (null comint-redirect-completed)
		            (accept-process-output process timeout)))))

(defun lpy-shell--redirect-send (text)
  "Send TEXT to Lpy interpreter, capturing and removing the output."
  (with-current-buffer (get-buffer-create lpy-shell--redirect-output-buffer)
    (erase-buffer)
    (lpy-shell--with
      (lpy-shell--redirect-send-1 text))
    (s-chomp (buffer-substring-no-properties (point-min) (point-max)))))

(defun lpy-shell--redirect-send-internal (text)
  "Send TEXT to internal Lpy interpreter, capturing and removing the output."
  (with-current-buffer (get-buffer-create lpy-shell--redirect-output-buffer)
    (erase-buffer)
    (lpy-shell--with-internal
      (lpy-shell--redirect-send-1 text))
    (s-chomp (buffer-substring-no-properties (point-min) (point-max)))))

;;; Sending Text
;;;; Interface

(defun lpy-shell--send (text)
  "Send TEXT to Lpy interpreter, starting up if needed."
  (lpy-shell--with
    (let ((lpy-shell--output-in-progress t)
          (proc (lpy-shell--current-process)))
      (comint-send-string proc text))))

(defun lpy-shell--send-internal (text)
  "Send TEXT to Lpy interpreter, starting up if needed."
  (lpy-shell--with-internal
    (let ((lpy-shell--output-in-progress t)
          (proc (lpy-shell--current-process)))
      (comint-send-string proc text))))

;;;; Macros

(defmacro lpy-shell--eval-1 (text)
  "Internal implementation of interactive eval commands."
  (declare (indent 0))
  (let ((text-sym (gensym)))
    `(-when-let (,text-sym ,text)
       ;; (run-lpy)
       (lpy-shell--with-live
         ;; TODO Force the initial/end cases in a nicer way if possible
         (lpy-shell--send "\n")
         (lpy-shell--send ,text-sym)
         (lpy-shell--send "\n")))))

;;;; Commands

(defun lpy-shell-eval-current-form ()
  "Send form containing point to the Lpy interpreter, starting up if needed."
  (interactive)
  (lpy-shell--eval-1
    (lpy--current-form-string)))

(defun lpy-shell-eval-last-sexp ()
  "Send the last sexp to the Lpy interpreter, starting up if needed."
  (interactive)
  (lpy-shell--eval-1
    (lpy--last-sexp-string)))

(defun lpy-shell-eval-region ()
  "Send region to the Lpy interpreter, starting up if needed."
  (interactive)
  (when (and (region-active-p) (not (region-noncontiguous-p)))
    (lpy-shell--eval-1
      (buffer-substring (region-beginning) (region-end)))))

(defun lpy-shell-eval-buffer ()
  "Send the current buffer to the Lpy interpreter, starting up if needed."
  (interactive)
  (lpy-shell--eval-1
    (buffer-string)))

;;; Notifications

(defun lpy-shell--notify (msg)
  "`message' MSG if `lpy-shell--notify?' is non-nil."
  (when lpy-shell--notify?
    (message msg)))

(defun lpy-shell--check-installed? ()
  "Warn if `lpy-shell--interpreter' is not found, returning non-nil otherwise."
  (cond
   ((executable-find lpy-shell--interpreter))
   ((prog1 nil
      (lpy-shell--notify "Lpy cmd not found. Install or activate a env with Lpy.")))))

;;; inferior-lpy-mode
;;;; Colorings

(defun lpy-inferior--support-font-locking-input ()
  "Fontify the current line being entered in the Lpy shell.

The solution implemented is my own and was interesting enough to warrant
a blog post: http://www.modernemacs.com/post/comint-highlighting/."
  (unless (lpy-shell--internal?)
    (setq font-lock-defaults
          '(inferior-lpy-font-lock-kwds
            nil nil
            (("+-*/.<>=!?$%_&~^:@" . "w"))  ; syntax alist
            nil
            (font-lock-mark-block-function . mark-defun)
            (font-lock-syntactic-face-function  ; Differentiates (doc)strings
             . lpy-font-lock-syntactic-face-function)))
    (setq-local syntax-propertize-function #'lpy-syntax-propertize-function)
    (font-lock-mode 1)))

(defun lpy-inferior--support-colorama-output ()
  "Support colorama'd shell output (like errors/traces) with `ansi-color'."
  (ansi-color-for-comint-mode-on)
  (add-hook 'comint-output-filter-functions #'ansi-color-process-output))

(defun lpy-inferior--support-xterm-color ()
  "Support `xterm-color' in shell output."
  (when (fboundp #'xterm-color-filter)  ; not installed by default
    (add-hook 'comint-preoutput-filter-functions #'xterm-color-filter)))

;;;; Comint Configurations

;; (defun lpy-inferior--fix-comint-input-history-breaking ()
;;   "Temp resolves comint's history sometimes failing, no side effects I think."
;;   (advice-add #'comint-previous-input :before
;;               (lambda (&rest args) (setq-local comint-stored-incomplete-input ""))))

;;;; Mode Declaration

(defvar sy-inferior-lpy-prompt "repl> ")

(defun sy-inferior-preoutput-filter (output)
  (let ((stripped (replace-regexp-in-string
                   (concat "^" (regexp-quote sy-inferior-lpy-prompt)) "" output)))
    ;; strip continuation prompts (e.g. "... " or "... ... ") emitted per newline
    (concat (replace-regexp-in-string "^\\(\\.\\.\\.[ ]?\\)+" "" stripped)
            sy-inferior-lpy-prompt)))

;;;###autoload
(define-derived-mode inferior-lpy-mode comint-mode "Inferior Lpy"
  "Major mode for Lpy inferior process."
  (setenv "PYTHONIOENCODING" "UTF-8")

  (setq-local indent-tabs-mode nil)
  (setq-local comint-prompt-read-only t)
  (setq-local comint-prompt-regexp (rx bol "repl> "))
  ;; (lpy-inferior--fix-comint-input-history-breaking)

  (setq-local comint-preoutput-filter-functions '(sy-inferior-preoutput-filter))
  (setq-local comint-output-filter-functions nil)

  (lpy-inferior--support-colorama-output)
  (lpy-inferior--support-xterm-color)

  (when lpy-shell--enable-font-lock?
    (lpy-inferior--support-font-locking-input)))

(define-key inferior-lpy-mode-map (kbd "C-c C-z")
  (lambda () (interactive) (other-window -1)))

;;; Commands
;;;; Killing

(defun lpy-shell--kill ()
  "Kill the Lpy interpreter process."
  (interactive)

  (-when-let (buff (get-buffer lpy-shell--buffer-name))
    (kill-buffer buff)))

(defun lpy-shell--kill-internal ()
  "Kill the internal Lpy interpreter process."
  (interactive)

  (-when-let (buff (get-buffer lpy-shell--buffer-name-internal))
    (kill-buffer buff)))

(defun lpy-shell--kill-all ()
  "Kill all Lpy interpreter processes."
  (interactive)

  (lpy-shell--kill)
  (lpy-shell--kill-internal))

;;;; Running

;;;###autoload
(defun run-lpy ()
  "Startup and/or switch to a Lpy interpreter process."
  (interactive)

  (lpy-shell--with
    (switch-to-buffer-other-window (current-buffer))))

;;; Provide:

(provide 'lpy-shell)

;;; lpy-shell.el ends here
