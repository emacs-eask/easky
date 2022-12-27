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

(require 'ansi-color)
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

(defcustom easky-display-function #'lv-message
  "Function to display Easky's result."
  :type 'function
  :group 'easky)

(defcustom easky-focus-p nil
  "Select window after command execution."
  :type 'boolean
  :group 'easky)

(defcustom easky-move-point-for-output nil
  "Controls whether interpreter output moves point to the end of the output."
  :type 'boolean
  :group 'easky)

(defcustom easky-timeout-seconds 30
  "Timeout seconds for running too long process."
  :type 'number
  :group 'easky)

(defconst easky-buffer-name "*easky*"
  "Buffer name for process file.")

(defvar easky--timeout-timer nil)

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

(defun easky--setup-eask-env ()
  "Set up for eask environment."
  (setenv "EASK_HASCOLORS" (if (or (display-graphic-p) (display-color-cells))
                               "true"
                             nil)))

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
    (t (easky--setup-eask-env)
       (let* (eask--initialized-p
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
               "  | eask-mode    | major mode for editing Eask files | https://github.com/emacs-eask/eask-mode    |\n"
               "  | company-eask | Company backend for Eask-file     | https://github.com/emacs-eask/company-eask |\n"
               "  | eldoc-eask   | Eldoc support for Eask-file       | https://github.com/emacs-eask/eldoc-eask   |\n"))))))))

;;
;; (@* "Display" )
;;

(defvar easky-process nil
  "Singleton process.")

(defun easky--strip-headers (str)
  "Strip command headers from STR, and leave only the execution result."
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    (when (search-forward "Loading Eask file" nil t)
      (forward-line 1))
    (let ((content (string-trim (buffer-substring (point) (point-max)))))
      (if (string-empty-p content)
          (buffer-string)  ; try to print something, don't let the user left unknown
        content))))

(defun easky--default-filter (proc output)
  "Default filter for PROC's OUTPUT."
  (with-current-buffer (process-buffer proc)
    (goto-char (point-max))
    (let ((inhibit-read-only t)
          (start (point)))
      (insert output)
      (ansi-color-apply-on-region start (point) t)  ; apply in buffer
      (funcall easky-display-function (easky--strip-headers (buffer-string)))
      (when (easky-lv-message-p)
        ;; Apply color in lv buffer!
        (with-current-buffer (window-buffer lv-wnd)
          (ansi-color-apply-on-region (point-min) (point-max)))
        ;; Move to end of buffer!
        (when easky-move-point-for-output
          (with-selected-window lv-wnd
            ;; XXX: Don't go above max lin, it will shift!
            (goto-char (1- (point-max)))))))))

(defun easky--default-sentinel (process &optional _event)
  "Default sentinel for PROCESS."
  (when (memq (process-status process) '(exit signal))
    (delete-process process)
    (setq easky-process nil)
    ;; XXX: This is only for lv-message!
    (when (easky-lv-message-p)
      (add-hook 'pre-command-hook #'easky--pre-command-once)
      (when easky-focus-p
        (select-window lv-wnd)))))

(defun easky--output-buffer (cmd)
  "Output CMD to buffer."
  (when (and easky-process
             (yes-or-no-p "Easky is still busy, kill it anyway? "))
    (delete-process easky-process)
    (setq easky-process nil))
  ;; XXX: Make sure we only have one process running!
  (unless easky-process
    (let ((prev-dir default-directory))
      (with-current-buffer (get-buffer-create easky-buffer-name)
        (setq default-directory prev-dir)  ; hold `default-directory'
        (read-only-mode 1)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (goto-char (point-min)))
        (let* ((program (car (split-string cmd)))
               (proc-name (format "easky-process-%s" program))
               (process (start-file-process-shell-command proc-name (current-buffer) cmd)))
          (set-process-filter process #'easky--default-filter)
          (set-process-sentinel process #'easky--default-sentinel)
          (setq easky-process process)
          ;; Set timeout!
          (when (timerp easky--timeout-timer)
            (cancel-timer easky--timeout-timer))
          (setq easky--timeout-timer (run-with-timer easky-timeout-seconds
                                                     nil #'easky--kill-process)))))))

(defun easky--kill-process ()
  "Kill process."
  (when (and easky-process (eq (process-status easky-process) 'run))
    (message "Easky process timed out, %s (running over %s seconds)"
             (process-name easky-process)
             easky-timeout-seconds)
    (kill-process easky-process)
    (setq easky-process nil)))

(defun easky-lv-message-p ()
  "Return t if using lv to display message."
  (equal easky-display-function #'lv-message))

(defmacro easky--display (cmd)
  "Display CMD output."
  (declare (indent 0) (debug t))
  `(easky--setup (easky--output-buffer ,cmd)))

;;
;; (@* "Pre-command / Post-command" )
;;

(defun easky--pre-command-once (&rest _)
  "One time pre-command after Easky command."
  ;; XXX: We pass on to next post-command!
  (remove-hook 'pre-command-hook #'easky--pre-command-once)
  (add-hook 'post-command-hook #'easky--post-command-once))

(defun easky--post-command-once (&rest _)
  "One time post-command after Easky command."
  ;; XXX: This will allow us to scroll in the lv's window!
  (unless (equal lv-wnd (ignore-errors (selected-window)))
    ;; Once we select window other than lv's window, then we kill it!
    (remove-hook 'post-command-hook #'easky--post-command-once)
    (lv-delete-window)))

;;
;; (@* "Commands" )
;;

(defmacro easky--exec-with-files (prompt form-1 form-2 form-3)
  "Execut command with file selected.

Argument PROMPT is a string to ask the user regarding the file action.

Arguments FORM-1, FORM-2 and FORM-3 are execution by each file action."
  (declare (indent 1) (debug t))
  `(let* ((options '(("All (Default)"   . "Select all files defined in your Eask-file")
                     ("Select file"     . "Select a file through minibuffer")
                     ("Enter wildcards" . "Enter wildcards pattern")))
          (max-len (max (eask-seq-str-max (mapcar #'cdr options))
                        (/ (frame-width) 2.5)))
          (option
           (completing-read
            ,prompt
            (lambda (string predicate action)
              (if (eq action 'metadata)
                  `(metadata
                    (annotation-function
                     . ,(lambda (cand)
                          (concat (propertize " " 'display `((space :align-to (- right ,max-len))))
                                  (cdr (assoc cand options))))))
                (complete-with-action action options string predicate)))
            nil t nil nil "All (Default)")))
     (pcase option
       ("All (Default)"   ,form-1)
       ("Select file"     ,form-2)
       ("Enter wildcards" ,form-3))))

(defun easky--select-el-files (candidate)
  "Return t if CANDIDATE is either directory or an elisp file."
  (or (string-suffix-p ".el" candidate)
      (file-directory-p candidate)))

(defun easky-command (&rest args)
  "Form command string.

Rest argument ARGS is the Eask's CLI arguments."
  (concat (or easky-executable "eask") " "
          (mapconcat #'shell-quote-argument args " ")))

;;;###autoload
(defun easky-help ()
  "Print Eask help manual."
  (interactive)
  (easky--display (easky-command "--help")))

;;;###autoload
(defun easky-version ()
  "Print Eask version."
  (interactive)
  (easky--display (easky-command "--version")))

;;;###autoload
(defun easky-info ()
  "Print Eask information."
  (interactive)
  (easky--display (easky-command "info")))

;;;###autoload
(defun easky-locate ()
  "Print Eask installed location."
  (interactive)
  (easky--display (easky-command "locate")))

;;;###autoload
(defun easky-compile ()
  "Clean up .eask directory."
  (interactive)
  (easky--exec-with-files "Select `compile' action: "
    (easky--display (easky-command "compile"))
    (let ((file (read-file-name "Select file for `compile': "
                                nil nil t nil #'easky--select-el-files)))
      (easky--display (easky-command "compile" file)))
    (let ((wildcards (read-string "Wildcards: ")))
      (easky--display (easky-command "compile" wildcards)))))

;;;###autoload
(defun easky-files ()
  "Print the list of all package files."
  (interactive)
  (easky--display (easky-command "files")))

;;;###autoload
(defun easky-archives ()
  "Print used archives."
  (interactive)
  (easky--display (easky-command "archives")))

;;;###autoload
(defun easky-archives-all ()
  "Print available archives."
  (interactive)
  (easky--display (easky-command "archives" "--all")))

;;;###autoload
(defun easky-keywords ()
  "List available keywords that can be used in the header section."
  (interactive)
  (easky--display (easky-command "keywords")))

;;;###autoload
(defun easky-path ()
  "Print the PATH (exec-path) from Eask sandbox."
  (interactive)
  (easky--display (easky-command "path")))

;;;###autoload
(defalias 'easky-exec-path 'easky-path)

;;;###autoload
(defun easky-load-path ()
  "Print the `load-path' from Eask sandbox."
  (interactive)
  (easky--display (easky-command "load-path")))

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

;;;###autoload
(defun easky-run ()
  "Execute Eask's script."
  (interactive)
  (easky--setup
    (if eask-scripts
        (let* ((max-len (max (eask-seq-str-max (mapcar #'cdr eask-scripts))
                             (/ (frame-width) 2.5)))
               (selected-script
                (completing-read
                 "Run Eask's script: "
                 (lambda (string predicate action)
                   (if (eq action 'metadata)
                       `(metadata
                         (annotation-function
                          . ,(lambda (cand)
                               (concat (propertize " " 'display `((space :align-to (- right ,max-len))))
                                       (cdr (assoc cand eask-scripts))))))
                     (complete-with-action action eask-scripts string predicate)))
                 nil t)))
          (easky--display (easky-command "run" selected-script)))
      (message (easky--message-concat
                "Not finding any script to run, you can add one by adding the line below to your Eask-file:\n\n"
                "  (script \"test\" \"echo Hi!~\")"
                "\n\nThen re-run this command once again!")))))

;;;###autoload
(defalias 'easky-run-script 'easky-run)

;;;###autoload
(defun easky-package ()
  "Package your package to dist folder."
  (interactive)
  (easky--display (easky-command "package")))

;;
;;; Install

;;;###autoload
(defun easky-install-deps ()
  "Update all packages from Eask sandbox."
  (interactive)
  (easky--display (easky-command "install-deps")))

;;;###autoload
(defun easky-install-deps-dev ()
  "Update all packages from Eask sandbox."
  (interactive)
  (easky--display (easky-command "install-deps" "--dev")))

;;
;;; Cleaning

;;;###autoload
(defun easky-clean-workspace ()
  "Clean up .eask directory."
  (interactive)
  (easky--display (easky-command "clean" "workspace")))

;;;###autoload
(defalias 'easky-clean-.eask 'easky-clean-workspace)

;;;###autoload
(defun easky-clean-dist (dest)
  "Delete dist subdirectory.

Argument DEST is the destination folder, default is set to `dist'."
  (interactive
   (list (read-directory-name "Destination: " nil nil nil "dist")))
  (easky--display (easky-command "clean" "dist" dest)))

;;;###autoload
(defun easky-clean-elc ()
  "Remove byte compiled files generated by eask compile."
  (interactive)
  (easky--display (easky-command "clean" "elc")))

;;;###autoload
(defun easky-clean-all ()
  "Remove byte compiled files generated by eask compile."
  (interactive)
  (easky--display (easky-command "clean" "all")))

;;
;;; Linting

;;;###autoload
(defun easky-lint-checkdoc ()
  "Run checkdoc."
  (interactive)
  (easky--exec-with-files "Select `checkdoc' action: "
    (easky--display (easky-command "lint" "checkdoc"))
    (let ((file (read-file-name "Select file for `checkdoc': "
                                nil nil t nil #'easky--select-el-files)))
      (easky--display (easky-command "lint" "checkdoc" file)))
    (let ((wildcards (read-string "Wildcards: ")))
      (easky--display (easky-command "lint" "checkdoc" wildcards)))))

;;;###autoload
(defun easky-lint-check-declare ()
  "Run check-declare."
  (interactive)
  (easky--exec-with-files "Select `check-declare' action: "
    (easky--display (easky-command "lint" "check-declare"))
    (let ((file (read-file-name "Select file for `check-declare': "
                                nil nil t nil #'easky--select-el-files)))
      (easky--display (easky-command "lint" "check-declare" file)))
    (let ((wildcards (read-string "Wildcards: ")))
      (easky--display (easky-command "lint" "check-declare" wildcards)))))

;;;###autoload
(defun easky-lint-elint ()
  "Run elint."
  (interactive)
  (easky--exec-with-files "Select `elint' action: "
    (easky--display (easky-command "lint" "elint"))
    (let ((file (read-file-name "Select file for `elint': "
                                nil nil t nil #'easky--select-el-files)))
      (easky--display (easky-command "lint" "elint" file)))
    (let ((wildcards (read-string "Wildcards: ")))
      (easky--display (easky-command "lint" "elint" wildcards)))))

;;;###autoload
(defun easky-lint-elsa ()
  "Run elsa."
  (interactive)
  (easky--exec-with-files "Select `elsa' action: "
    (easky--display (easky-command "lint" "elsa"))
    (let ((file (read-file-name "Select file for `elsa': "
                                nil nil t nil #'easky--select-el-files)))
      (easky--display (easky-command "lint" "elsa" file)))
    (let ((wildcards (read-string "Wildcards: ")))
      (easky--display (easky-command "lint" "elsa" wildcards)))))

;;;###autoload
(defun easky-lint-indent ()
  "Run indent-linet."
  (interactive)
  (easky--exec-with-files "Select `indent' action: "
    (easky--display (easky-command "lint" "indent"))
    (let ((file (read-file-name "Select file for `indent': "
                                nil nil t nil #'easky--select-el-files)))
      (easky--display (easky-command "lint" "indent" file)))
    (let ((wildcards (read-string "Wildcards: ")))
      (easky--display (easky-command "lint" "indent" wildcards)))))

;;;###autoload
(defun easky-lint-keywords ()
  "Run keywords linter."
  (interactive)
  (easky--display (easky-command "lint" "keywords")))

;;;###autoload
(defun easky-lint-package ()
  "Run package-lint."
  (interactive)
  (easky--exec-with-files "Select `package' action: "
    (easky--display (easky-command "lint" "package"))
    (let ((file (read-file-name "Select file for `package': "
                                nil nil t nil #'easky--select-el-files)))
      (easky--display (easky-command "lint" "package" file)))
    (let ((wildcards (read-string "Wildcards: ")))
      (easky--display (easky-command "lint" "package" wildcards)))))

;;;###autoload
(defun easky-lint-regexps ()
  "Run relint."
  (interactive)
  (easky--exec-with-files "Select `regexps' action: "
    (easky--display (easky-command "lint" "regexps"))
    (let ((file (read-file-name "Select file for `regexps': "
                                nil nil t nil #'easky--select-el-files)))
      (easky--display (easky-command "lint" "regexps" file)))
    (let ((wildcards (read-string "Wildcards: ")))
      (easky--display (easky-command "lint" "regexps" wildcards)))))

;;;###autoload
(defalias 'easky-lint-relint 'easky-lint-regexps)

;;
;;; Testing

;; TODO: ..

(provide 'easky)
;;; easky.el ends here
