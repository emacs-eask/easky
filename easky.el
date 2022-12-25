;;; easky.el --- Control Eask in Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Shen, Jen-Chieh

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Maintainer: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/emacs-eask/easky
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (eask-api "0.1.0") (ansi "0.4.1") (lv "0.0"))
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

(require 'eask-api)
(require 'eask-api-core)
(require 'ansi)  ; we need `ansi' to run through Eask API
(require 'lv)

(require 'easky-package)

(defgroup easky nil
  "Control Eask in Emacs."
  :prefix "easky-"
  :group 'tool
  :link '(url-link :tag "Repository" "https://github.com/emacs-eask/easky"))

(defcustom easky-executable nil
  "Executable to eask-cli."
  :type 'string
  :group 'easky)

;;
;; (@* "Externals" )
;;

(defvar eask-api-strict-p)
(declare-function easky-package--setup "easky-package.el")

;;
;; (@* "Core" )
;;

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
  "List of functions that we wish to enabled since.")

(defun easky-load-eask (&optional path)
  "Load Eask-file from PATH."
  (eask--silent (eask-file-try-load (or path default-directory)))
  eask-file)

(defun easky--ignore-error (&optional arg0 &rest args)
  "Record error.

We use number to name our arguments, ARG0 and ARGS."
  (setq easky--error-message
        (or (ignore-errors (apply #'format arg0 args))  ; Record message when valid
            t)))                                        ; fallback to t

(defmacro easky--ignore-env (&rest body)
  "Execute BODY with valid Eask environment."
  (declare (indent 0) (debug t))
  ;; This will maintain your Eask-file information!
  `(eask--save-eask-file-state
     (dolist (func easky-ignore-functions) (advice-add func :override #'easky--ignore-error))
     ,@body
     (dolist (func easky-ignore-functions) (advice-remove func #'easky--ignore-error))))

(defmacro easky--setup (&rest body)
  "Execute BODY without touching the Eask-file global variables."
  (declare (indent 0) (debug t))
  `(cond
    ;; Executable not found!
    ((and (not (executable-find "eask")) (not easky-executable))
     (user-error
      (easky--message-concat
       "No executable named `eask` in the PATH environment, make sure:\n\n"
       "  [1] You have installed eask-cli and added to your PATH\n"
       "  [2] You can manually set variable `easky-executable' to point to eask executable\n\n"
       "For more information, find the manual at https://emacs-eask.github.io/")))
    ;; Invalid Eask Project!
    ((not (easky--valid-project-p))
     (user-error (easky--message-concat
                  "Error execute Easky command, invalid Eask project.\n\n"
                  "  [1] Make sure you have a valid proejct-root\n"
                  "  [2] Make sure you have Eask-file inside your project\n")))
    ;; Okay! Good to go!
    (t (let* (eask--initialized-p
              easky--error-message  ; init error message
              (user-emacs-directory (expand-file-name (concat ".eask/" emacs-version "/")))
              (package-user-dir (expand-file-name "elpa" user-emacs-directory))
              (user-init-file (locate-user-emacs-file "init.el"))
              (custom-file (locate-user-emacs-file "custom.el"))
              (package-activated-list))
         (easky--ignore-env
           (if (and (ignore-errors (easky-load-eask))  ; Error loading Eask file!
                    (not easky--error-message))        ; The message is stored here!
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
               "  | eldoc-eask   | Eldoc support for Eask-file   | https://github.com/emacs-eask/eldoc-eask   |\n"))))))))

(defun easky-command-async (&rest strings)
  "Execute Eask command asynchronously.

The rest argument STRINGS are concatenate with space between, then send it to
`shell-command'."
  ;; TODO: ..
  )

(defun easky-command (&rest strings)
  "Execute Eask command.

The rest argument STRINGS are concatenate with space between, then send it to
`shell-command'."
  (push (or easky-executable "eask") strings)
  (let* ((command (apply #'easky--message-concat strings))
         (result (shell-command-to-string command)))
    (string-trim result)))

(defun easky--strip-headers (str)
  "Strip command headers from STR, and leave only the execution result."
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    (search-forward "Loading Eask file ")
    (forward-line 1)
    (string-trim (buffer-substring (point) (point-max)))))

;;;###autoload
(defun easky-help ()
  "Print Eask help manual."
  (interactive)
  (message (easky-command "--help")))

;;;###autoload
(defun easky-version ()
  "Print Eask version."
  (interactive)
  (message "Eask CLI (%s)" (easky-command "--version")))

;;;###autoload
(defun easky-info ()
  "Print Eask information."
  (interactive)
  (message (easky--strip-headers (easky-command "info"))))

;;;###autoload
(defun easky-locate ()
  "Print Eask installed location."
  (interactive)
  (message (easky-command "locate")))

;;;###autoload
(defun easky-compile ()
  "Clean up .eask directory."
  (interactive)
  (message (easky--strip-headers (easky-command "compile"))))

;;;###autoload
(defun easky-files ()
  "Print the list of all package files."
  (interactive)
  (message (easky--strip-headers (easky-command "files"))))

;;;###autoload
(defun easky-path ()
  "Print the PATH (exec-path) from Eask sandbox."
  (interactive)
  (message (easky--strip-headers (easky-command "path"))))

;;;###autoload
(defalias 'easky-exec-path 'easky-path)

;;;###autoload
(defun easky-load-path ()
  "Print the `load-path' from Eask sandbox."
  (interactive)
  (message (easky--strip-headers (easky-command "load-path"))))

;;;###autoload
(defun easky-init (dir)
  "Initialize Eask-file in DIR."
  (interactive
   (list (read-directory-name "Where you want to place your Eask-file: ")))
  (let* ((eask-api-strict-p)  ; disable strict
         (files (eask-api-files dir))
         (continue
          (or (not files)
              (y-or-n-p
               (easky--message-concat
                "Eask-file already exist,\n\n  "
                (mapconcat #'identity files "\n   ")
                "\n\nContinue the initialization? "))))
         (new-name "Eask"))
    (when continue
      (when files
        (setq new-name (read-file-name "New Eask-file name: " dir nil nil "Eask")))
      (when (file-exists-p new-name)
        (user-error "File already exists, operation aborted"))
      (let* ((project-name (file-name-nondirectory (directory-file-name default-directory)))
             (package-name (read-string (format "package name: (%s) " project-name) nil nil project-name))
             (version (read-string "version: (1.0.0) " nil nil "1.0.0"))
             (description (read-string "description: "))
             (guess-entry-point (format "%s.el" project-name))
             (entry-point (read-string (format "entry point: (%s) " guess-entry-point)
                                       nil nil guess-entry-point))
             (emacs-version (read-string "emacs version: (26.1) " nil nil "26.1"))
             (website (read-string "website: "))
             (keywords (read-string "keywords: "))
             (keywords (split-string keywords "[, ]"))
             (keywords (string-join keywords "\" \""))
             (content (format
                       "(package \"%s\"
         \"%s\"
         \"%s\")

(website-url \"%s\")
(keywords \"%s\")

(package-file \"%s\")

(script \"test\" \"echo \\\"Error: no test specified\\\" && exit 1\")

(source \"gnu\")

(depends-on \"emacs\" \"%s\")
"
                       package-name version description website keywords
                       entry-point emacs-version)))
        (lv-message content)
        (when (yes-or-no-p (format "About to write to %s:\n\nIs this Okay? " new-name))
          (write-region content nil new-name))
        (lv-delete-window)))))

;;
;;; Install

;;;###autoload
(defun easky-install-deps ()
  "Update all packages from Eask sandbox."
  (interactive)
  (easky-package--setup
      (progn
        (message "Installing %s package dependenc%s..."
                 (length eask-depends-on)
                 (eask--sinr eask-depends-on "y" "ies"))
        (dolist (package eask-depends-on)
          (package-install (intern (car package)))))
    (easky-package--revert-info)))

;;;###autoload
(defun easky-install-deps-dev ()
  "Update all packages from Eask sandbox."
  (interactive)
  (easky-package--setup
      (progn
        (message "Installing %s package dependenc%s..."
                 (length eask-depends-on)
                 (eask--sinr eask-depends-on "y" "ies"))
        (dolist (package eask-depends-on)
          (package-install (intern (car package))))
        (message "Installing %s development dependenc%s..."
                 (length eask-depends-on-dev)
                 (eask--sinr eask-depends-on-dev "y" "ies"))
        (dolist (package eask-depends-on-dev)
          (package-install (intern (car package)))))
    (easky-package--revert-info)))

;;
;;; Cleaning

;;;###autoload
(defun easky-clean-workspace ()
  "Clean up .eask directory."
  (interactive)
  (message (easky--strip-headers (easky-command "clean" "workspace"))))

;;;###autoload
(defalias 'easky-clean-.eask 'easky-clean-workspace)

;;;###autoload
(defun easky-clean-dist (dest)
  "Delete dist subdirectory.

Argument DEST is the destination folder, default is set to `dist'."
  (interactive
   (list (read-directory-name "Destination: " nil nil nil "dist")))
  (message (easky--strip-headers (easky-command "clean" "dist" dest))))

;;;###autoload
(defun easky-clean-elc ()
  "Remove byte compiled files generated by eask compile."
  (interactive)
  (message (easky--strip-headers (easky-command "clean" "elc"))))

;;;###autoload
(defun easky-clean-all ()
  "Remove byte compiled files generated by eask compile."
  (interactive)
  (message (easky--strip-headers (easky-command "clean" "all"))))

(provide 'easky)
;;; easky.el ends here
