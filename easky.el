;;; easky.el --- Control Eask in Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Shen, Jen-Chieh

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Maintainer: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/emacs-eask/easky
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (eask-api "0.1.0") (ansi "0.4.1"))
;; Keywords: lisp easky

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Control Eask in Emacs.
;;

;;; Code:

(require 'cl-lib)

(require 'eask-api-core)
(require 'ansi)  ; we need `ansi' to run through Eask API

(defun easky--message-concat (&rest messages)
  "Concatenate MESSAGES with space."
  (mapconcat #'identity (cl-remove-if #'null messages) " "))

(defun easky--valid-project-p (&optional path)
  "Return t if PATH is a valid Eask project."
  (let ((eask-api-strict-p)
        (default-directory (or path default-directory)))
    (eask-api-setup)))

(defvar easky--error-message nil
  "Set to non-nil when error occurs while loading Eask-file.")

(defconst easky-ignore-functions
  '( eask-debug eask-log eask-info eask-warn eask-error)
  "List of functions that we wish to enabled since")

(defun easky-load-eask (&optional path)
  "Load Eask-file from PATH."
  (eask--silent (eask-file-try-load (or path default-directory)))
  eask-file)

(defun eask--ignore-error (&optional arg0 &rest args)
  "Record error."
  (setq easky--error-message
        (or (ignore-errors (apply #'format arg0 args))  ; Record message when valid
            t)))                                   ; fallback to t

(defmacro easky--ignore-env (&rest body)
  "Execute BODY with valid Eask environment."
  (declare (indent 0) (debug t))
  ;; This will maintain your Eask-file information!
  `(eask--save-eask-file-state
     (dolist (func easky-ignore-functions) (advice-add func :override #'eask--ignore-error))
     ,@body
     (dolist (func easky-ignore-functions) (advice-remove func #'eask--ignore-error))))

(defmacro easky--setup (&rest body)
  "Execute BODY without touching the Eask-file global variables."
  (declare (indent 0) (debug t))
  `(if (easky--valid-project-p)
       (let* (eask--initialized-p
              easky--error-message
              (user-emacs-directory (expand-file-name (concat ".eask/" emacs-version "/")))
              (package-user-dir (expand-file-name "elpa" user-emacs-directory))
              (user-init-file (locate-user-emacs-file "init.el"))
              (custom-file (locate-user-emacs-file "custom.el"))
              (package-activated-list))
         (easky--ignore-env
           (if (and (ignore-errors (easky-load-eask))
                    (not easky--error-message))
               (progn ,@body)
             (user-error
              (easky--message-concat
               (when (stringp easky--error-message)
                 (format "[ERROR] %s\n\n" easky--error-message))
               "Error loading Eask-file, few suggestions: \n\n"
               "  [1] Lint your Eask-file with command `eask check-eask [EASK-FILE]`\n"
               "  [2] Make sure your Eask-file doesn't contain any invalid syntax\n\n"
               "Here are useful tools to help you edit Eask-file:\n\n"
               "  | company-eask | Company backend for Eask-file | https://github.com/emacs-eask/company-eask |\n"
               "  | eldoc-eask   | Eldoc support for Eask-file   | https://github.com/emacs-eask/eldoc-eask   |\n")))))
     (user-error (easky--message-concat
                  "Error execute Easky command, invalid Eask project.\n\n"
                  "  [1] Make sure you have a valid proejct-root\n"
                  "  [2] Make sure you have Eask-file inside your project\n"))))

(defun easky-eask-command-async (&rest strings)
  ""
  ;; TODO: ..
  )

(defun easky-eask-command (&rest strings)
  "Execute Eask command.

The rest argument STRINGS are concatenate with space between, then send it to
`shell-command'."
  (let* ((commad (apply #'easky--message-concat strings))
         (result (shell-command-to-string commad)))
    (string-trim result)))

;;;###autoload
(defun easky-eask-help ()
  "Print Eask help manual."
  (interactive)
  (message (easky-eask-command "eask" "--help")))

;;;###autoload
(defun easky-eask-version ()
  "Print Eask version."
  (interactive)
  (message "Eask CLI (%s)" (easky-eask-command "eask" "--version")))

;;;###autoload
(defun easky-info ()
  "Print Eask-file information."
  (interactive)
  ;; TODO: ..
  )

(provide 'easky)
;;; easky.el ends here
