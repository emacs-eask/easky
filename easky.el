;;; easky.el --- Control Eask in Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Shen, Jen-Chieh

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Maintainer: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/emacs-eask/easky
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (eask-mode "0.1.0") (eask-api "0.1.0") (ansi "0.4.1") (lv "0.0") (marquee-header "0.1.0"))
;; Keywords: maint easky

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
(require 'frame)
(require 'files)

(require 'eask-mode)
(require 'eask-api)
(require 'eask-api-core)
(require 'ansi)  ; we need `ansi' to run through Eask API
(require 'lv)
(require 'marquee-header)

(defgroup easky nil
  "Control Eask in Emacs."
  :prefix "easky-"
  :group 'tool
  :link '(url-link :tag "Repository" "https://github.com/emacs-eask/easky"))

(defcustom easky-strip-header t
  "Remove output header while displaying."
  :type 'boolean
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

(defcustom easky-show-tip t
  "If non-nil, show the tip in the lv window."
  :type 'boolean
  :group 'easky)

(defcustom easky-annotation-ratio 2.5
  "Ratio align from the right to display `completin-read' annotation."
  :type 'float
  :group 'easky)

(defconst easky-buffer-name "*easky*"
  "Buffer name for process file.")

(defvar easky--timeout-timer nil)

;;
;; (@* "Externals" )
;;

(defvar eask-api-strict-p)

;;
;; (@* "Util" )
;;

(defmacro easky--inhibit-log (&rest body)
  "Execute BODY without write it to message buffer."
  (declare (indent 0) (debug t))
  `(let (message-log-max) ,@body))

(defun easky--completing-frame-offset (options)
  "Return frame offset while `completing-read'.

Argument OPTIONS ia an alist use to calculate the frame offset."
  (max (eask-seq-str-max (mapcar #'cdr options))
       (/ (frame-width) easky-annotation-ratio)))

;;
;; (@* "Compat" )
;;

;; TODO: Remove this after we dropped version 27.x!
(defun easky--ansi-color-apply-on-region (start end &optional preserve-sequences)
  "Compatible version of function `ansi-color-apply-on-region'.

Arguments START, END and PRESERVE-SEQUENCES is the same to original function."
  (if (version< emacs-version "28.1")
      (ansi-color-apply-on-region start end)
    (ansi-color-apply-on-region start end preserve-sequences)))

;;
;; (@* "Core" )
;;

(defun easky--valid-source-p (&optional path)
  "Return t if PATH has a valid Eask-file"
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
    ((not (eask-api-executable))
     (user-error
      (concat
       "No executable named `eask` in the PATH environment, make sure:\n\n"
       "  [1] You have installed eask-cli and added to your PATH\n"
       "  [2] You can manually set variable `eask-api-executable' to point to eask executable"
       "\n\nFor more information, find the manual at https://emacs-eask.github.io/")))
    ;; Invalid Eask Project!
    ((not (easky--valid-source-p))
     (user-error
      (concat
       "Error execute Easky command, invalid Eask project.\n\n"
       "  [1] Make sure you have a valid proejct-root\n"
       "  [2] Make sure you have Eask-file inside your project"
       "\n\nYou can creat Eask-file by doing 'M-x eask-init'")))
    ;; Okay! Good to go!
    (t (easky--setup-eask-env)
       (let* (eask--initialized-p
              easky--error-message  ; init error message
              (eask-lisp-root (eask-api-lisp-root))
              (default-directory (plist-get (easky--valid-source-p) :root))
              (user-emacs-directory (expand-file-name (concat ".eask/" emacs-version "/")))
              (package-user-dir (expand-file-name "elpa" user-emacs-directory))
              (user-init-file (locate-user-emacs-file "init.el"))
              (custom-file (locate-user-emacs-file "custom.el"))
              (package-activated-list)  ; make sure package.el does not change
              eask-depends-on-recipe-p  ; make sure github-elpa creates directory
              (github-elpa-working-dir (expand-file-name "./temp-elpa/.working/" user-emacs-directory))
              (github-elpa-archive-dir (expand-file-name "./temp-elpa/packages/" user-emacs-directory))
              (github-elpa-recipes-dir (expand-file-name "./temp-elpa/recipes/" user-emacs-directory)))
         (easky--ignore-env
           (if (and (ignore-errors (easky-load-eask))  ; Error loading Eask file!
                    (not easky--error-message))        ; The message is stored here!
               (progn ,@body)
             (user-error
              (concat
               (when (stringp easky--error-message)
                 (format "[ERROR] %s\n\n" easky--error-message))
               "Error loading Eask-file, few suggestions: \n\n"
               "  [1] Lint your Eask-file with command `eask check-eask [EASK-FILE]`\n"
               "  [2] Make sure your Eask-file doesn't contain any invalid syntax"
               "\n\nHere are useful tools to help you edit Eask-file:\n\n"
               "  | Package       | Description                       | Repository URL                              |\n"
               "  | ------------- | --------------------------------- | ------------------------------------------- |\n"
               "  | company-eask  | Company backend for Eask-file     | https://github.com/emacs-eask/company-eask  |\n"
               "  | eldoc-eask    | Eldoc support for Eask-file       | https://github.com/emacs-eask/eldoc-eask    |\n"
               "  | flycheck-eask | Eask support in Flycheck          | https://github.com/emacs-eask/flycheck-eask |\n"
               "  | flymake-eask  | Eask support in Flymake           | https://github.com/flymake/flymake-eask     |"))))))))

;;
;; (@* "Tip" )
;;

(defconst easky-tips
  '("💡 Some commands may take longer time to complete..."
    "💡 Try 'M-x easky' to see all available commands!"
    "💡 Easky uses `marquee-header' to display tip and `lv' to display message"
    "💡 The full output can be seen in the `*easky*' buffer; use `M-x easky-to-buffer` to see the result!"
    "💡 You can use `eask create' to create an Elisp project"
    "💡 Make sure you have all dependencies installed before you compile it!"
    "💡 `eask info` command prints out the package information!")
  "List of tips.")

;; XXX: Some commands can wait amount of time, display tip can help a little.
(defun easky--pick-tips ()
  "Return a tip."
  (let ((index (random (length easky-tips))))
    (nth index easky-tips)))

;;
;; (@* "Display" )
;;

(defun easky-buffer ()
  "Return easky buffer."
  (get-buffer-create easky-buffer-name))

(defun easky-to-buffer ()
  "Display easky buffer."
  (interactive)
  (pop-to-buffer (easky-buffer) `((display-buffer-in-direction) (dedicated . t))))

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
          (start (point))
          (lv-first (not (window-live-p lv-wnd))))
      (insert output)
      (easky--ansi-color-apply-on-region start (point) t)  ; apply in buffer
      (funcall easky-display-function (if easky-strip-header
                                          (easky--strip-headers (buffer-string))
                                        (buffer-string)))
      (when (easky-lv-message-p)
        ;; Apply color in lv buffer!
        (with-current-buffer (window-buffer lv-wnd)
          (ansi-color-apply-on-region (point-min) (point-max)))
        (when (and easky-show-tip lv-first)
          (with-selected-window lv-wnd
            (marquee-header-notify (easky--pick-tips) :loop t)))
        ;; Move to end of buffer!
        (when easky-move-point-for-output
          (with-selected-window lv-wnd
            ;; XXX: Don't go above max lin, it will shift!
            (goto-char (1- (point-max)))))))))

(defun easky--default-sentinel (process &optional _event)
  "Default sentinel for PROCESS."
  (when (memq (process-status process) '(exit signal))
    (easky--inhibit-log
      (cl-case (process-status process)
        (`signal (message "Easky task exit with error code"))  ; TODO: print with error code!
        (`exit (message "Easky task completed"))))
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
      (with-current-buffer (easky-buffer)
        (setq default-directory prev-dir)  ; hold `default-directory'
        (buffer-disable-undo)
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
;; (@* "All in one commands" )
;;

(defun easky-parse-help-manual (help-cmd subcmd-index)
  "Return an alist regarding help manual from HELP-CMD.

Argument HELP-CMD is a string contain option `--help'.  SUBCMD-INDEX is the
index to target subcommand.

The format is in (command . description)."
  (let ((manual (shell-command-to-string help-cmd))
        (data))
    (with-temp-buffer
      (insert manual)
      (goto-char (point-min))
      (search-forward "Commands:")
      (forward-line 1)
      (while (not (string-empty-p (string-trim (thing-at-point 'line))))
        (beginning-of-line)
        (let ((command)
              (description))
          (forward-symbol (or subcmd-index 1))
          (setq command (symbol-at-point))
          (search-forward "  " (line-end-position))
          (search-forward-regexp "[^ \t]" (line-end-position))
          (let ((start (1- (point))))
            (setq description (buffer-substring start (if (search-forward "  " (line-end-position) t)
                                                          (point)
                                                        (line-end-position)))))
          (push (cons (eask-2str command) (string-trim description)) data))
        (forward-line 1)))
    (reverse data)))

(defmacro easky--exec-with-help (help-cmd subcmd-index prompt &rest body)
  "Execut command with help manual parsed.

For arguments HELP-CMD and SUBCMD-INDEX, see function `easky-parse-help-manual'
for more information.

Argument PROMPT is the first prompt to show for the current help command.  BODY
is the implementation."
  (declare (indent 3) (debug t))
  `(let* ((options (easky-parse-help-manual ,help-cmd ,subcmd-index))
          (offset (easky--completing-frame-offset options))
          (command (completing-read
                    ,prompt
                    (lambda (string predicate action)
                      (if (eq action 'metadata)
                          `(metadata
                            (display-sort-function . ,#'identity)
                            (annotation-function
                             . ,(lambda (cand)
                                  (concat (propertize " " 'display `((space :align-to (- right ,offset))))
                                          (cdr (assoc cand options))))))
                        (complete-with-action action options string predicate)))
                    nil t)))
     ,@body))

;;;###autoload
(defun easky ()
  "Start Eask."
  (interactive)
  (easky--exec-with-help
      "eask --help" 1 "Select `eask' command: "
    (let ((command (intern (format "easky-%s" command))))
      (if (fboundp command)
          (call-interactively command)
        (user-error "Command %s not implemented yet, please consider report it to us!" command)))))

;;;###autoload
(defun easky-clean ()
  "Start Eask clean."
  (interactive)
  (easky--exec-with-help
      "eask clean --help" 2 "Select `eask clean' command: "
    (let ((command (intern (format "easky-clean-%s" command))))
      (if (fboundp command)
          (call-interactively command)
        (user-error "Command %s not implemented yet, please consider report it to us!" command)))))

;;;###autoload
(defun easky-lint ()
  "Start Eask lint."
  (interactive)
  (easky--exec-with-help
      "eask lint --help" 2 "Select `eask lint' command: "
    (let ((command (intern (format "easky-lint-%s" command))))
      (if (fboundp command)
          (call-interactively command)
        (user-error "Command %s not implemented yet, please consider report it to us!" command)))))

;;;###autoload
(defun easky-test ()
  "Start Eask test."
  (interactive)
  (easky--exec-with-help
      "eask test --help" 2 "Select `eask test' command: "
    (let ((command (intern (format "easky-test-%s" command))))
      (if (fboundp command)
          (call-interactively command)
        (user-error "Command %s not implemented yet, please consider report it to us!" command)))))

;;
;; (@* "Commands" )
;;

(defconst easky-exec-files-options
  '(("All files (Default)" . "Select all files defined in your Eask-file")
    ("Select file"         . "Select a file through completing-read")
    ("Enter wildcards"     . "Enter wildcards pattern"))
  "Options for command `check-eask'.")

(defmacro easky--exec-with-files (prompt form-1 form-2 form-3)
  "Execut command with file selected.

Argument PROMPT is a string to ask the user regarding the file action.

Arguments FORM-1, FORM-2 and FORM-3 are execution by each file action."
  (declare (indent 1) (debug t))
  `(let* ((offset (easky--completing-frame-offset easky-exec-files-options))
          (option
           (completing-read
            ,prompt
            (lambda (string predicate action)
              (if (eq action 'metadata)
                  `(metadata
                    (display-sort-function . ,#'identity)
                    (annotation-function
                     . ,(lambda (cand)
                          (concat (propertize " " 'display `((space :align-to (- right ,offset))))
                                  (cdr (assoc cand easky-exec-files-options))))))
                (complete-with-action action easky-exec-files-options string predicate)))
            nil t nil nil (nth 0 easky-exec-files-options)))
          (index (cl-position option (mapcar #'car easky-exec-files-options) :test 'string=)))
     (pcase index
       (0 ,form-1)
       (1 ,form-2)
       (2 ,form-3))))

(defun easky--select-el-files (candidate)
  "Return t if CANDIDATE is either directory or an elisp file."
  (or (and (string-suffix-p ".el" candidate)
           (not (string= dir-locals-file candidate)))
      (file-directory-p candidate)))

(defun easky-command (&rest args)
  "Form command string.

Rest argument ARGS is the Eask's CLI arguments."
  (concat (or eask-api-executable "eask") " "
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
  "Print Eask-file information."
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
(defun easky-search (query)
  "Search available packages with QUERY.

This can be replaced with `easky-package-install' command."
  (interactive
   (list (read-string "Query: ")))
  (easky--display (easky-command "search" query)))

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
         (new-name (expand-file-name "Eask"))
         (base-name)
         (invalid-name))
    (when (and files
               (yes-or-no-p (concat "Eask-file already exist,\n\n  "
                                    (mapconcat #'identity files "\n  ")
                                    "\n\nContinue the creation? ")))
      (while (or (file-exists-p new-name) invalid-name)
        (setq new-name (read-file-name
                        (format
                         (concat (if invalid-name
                                     "[?] Invalid filename `%s', "
                                   "[?] Filename `%s' already taken, ")
                                 "try another one: ")
                         (file-name-nondirectory (directory-file-name new-name)))
                        dir nil nil nil
                        #'eask-api-check-filename)
              base-name (file-name-nondirectory (directory-file-name new-name))
              invalid-name (not (eask-api-check-filename base-name)))
        (easky--inhibit-log (message "Checking filename..."))
        (sleep-for 0.2)))
    ;; Starting Eask-file creation!
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
      (lv-message (with-temp-buffer  ; colorized
                    (insert content)
                    (delay-mode-hooks (funcall #'eask-mode))
                    (ignore-errors (font-lock-ensure))
                    (buffer-string)))
      (unwind-protect
          (when (yes-or-no-p (format "About to write to %s:\n\nIs this Okay? " new-name))
            (write-region content nil new-name)
            (find-file new-name))
        (lv-delete-window)))))

;;;###autoload
(defun easky-run ()
  "Execute Eask's script."
  (interactive)
  (easky--setup
    (if eask-scripts
        (let* ((offset (easky--completing-frame-offset eask-scripts))
               (selected-script
                (completing-read
                 "Run Eask's script: "
                 (lambda (string predicate action)
                   (if (eq action 'metadata)
                       `(metadata
                         (display-sort-function . ,#'identity)
                         (annotation-function
                          . ,(lambda (cand)
                               (concat (propertize " " 'display `((space :align-to (- right ,offset))))
                                       (cdr (assoc cand eask-scripts))))))
                     (complete-with-action action eask-scripts string predicate)))
                 nil t)))
          (easky--display (easky-command "run" selected-script)))
      (message (concat
                "Not finding any script to run, you can add one by adding the line below to your Eask-file:\n\n"
                "  (script \"test\" \"echo Hi!~\")"
                "\n\nThen re-run this command once again!")))))

;;;###autoload
(defalias 'easky-run-script 'easky-run)

;;;###autoload
(defun easky-package (dir)
  "Package your package to DIR."
  (interactive
   (list (read-directory-name "Destination: " nil nil nil "dist")))  ; default to dist
  (easky--display (easky-command "package" "--dest" dir)))

;;;###autoload
(defun easky-refresh ()
  "Package your package to DIR."
  (interactive)
  (easky--display (easky-command "refresh")))

;;;###autoload
(defun easky-pkg-file ()
  "Generate pkg-file and printed it out!"
  (interactive)
  (easky--display (easky-command "pkg-file")))

;;;###autoload
(defun easky-recipe ()
  "Recommend me a recipe format."
  (interactive)
  (easky--display (easky-command "recipe")))

;;;###autoload
(defun easky-outdated ()
  "List outdated packages."
  (interactive)
  (easky--display (easky-command "outdated")))

;;;###autoload
(defun easky-upgrade-eask ()
  "Upgrade Eask CLI."
  (interactive)
  (easky--display (easky-command "upgrade-eask")))

;;
;;; Eask-file Checker

(defconst easky-check-eask-options
  '(("All Eask-files (Default)" . "Check all eask files")
    ("Pick a Eask-file"         . "Select an Eask-file through completing-read"))
  "Options for command `check-eask'.")

(defun easky-check-eask-collection (string predicate action)
  "Collection arguments for function `easky-check-eask'.

Arguments STRING, PREDICATE and ACTION are default value for collection
argument."
  (if (eq action 'metadata)
      (let ((offset (easky--completing-frame-offset easky-check-eask-options)))
        `(metadata
          (display-sort-function . ,#'identity)
          (annotation-function
           . ,(lambda (cand)
                (concat (propertize " " 'display `((space :align-to (- right ,offset))))
                        (cdr (assoc cand easky-check-eask-options)))))))
    (complete-with-action action easky-check-eask-options string predicate)))

;;;###autoload
(defun easky-check-eask (action)
  "Run Eask-file checker.

Argument ACTION is used to select checker's action."
  (interactive
   (list (completing-read "Select `check-eask' action: "
                          #'easky-check-eask-collection nil t nil nil
                          (car (nth 0 easky-check-eask-options)))))
  (let* ((options (mapcar #'car easky-check-eask-options))
         (index (cl-position action options :test 'string=)))
    (pcase index
      (0 (easky--display (easky-command "check-eask")))
      (1 (let ((file (read-file-name "Select file for `check-eask': "
                                     nil nil t nil
                                     (lambda (cand)
                                       (or (eask-api-check-filename cand)
                                           (file-directory-p cand))))))
           (easky--display (easky-command "check-eask" file)))))))

;;
;;; Execution

;;;###autoload
(defun easky-eask (args)
  "Run Eask CLI directly with ARGS."
  (interactive
   (list (read-string "eask ")))
  (easky--display (easky-command args)))

;;;###autoload
(defun easky-exec (args)
  "Run eask exec with ARGS."
  (interactive
   (list (read-string "eask exec ")))
  (easky--display (easky-command "exec" args)))

;;;###autoload
(defun easky-emacs (args)
  "Run eask emacs with ARGS."
  (interactive
   (list (read-string "eask emacs ")))
  (easky--display (easky-command "emacs" args)))

;;
;;; Install

(defconst easky-exec-packages-options
  '(("Current Package (Default)" . "Operate with current package defined in Eask-file")
    ("Specified"                 . "Specify packages through read-string"))
  "Options for command `check-eask'.")

(defmacro easky--exec-with-packages (prompt form-1 form-2)
  "Execut command with packages selected.

Argument PROMPT is a string to ask the user regarding the file action.

Arguments FORM-1 and FORM-2 are execution by each file action."
  (declare (indent 1) (debug t))
  `(let* ((offset (easky--completing-frame-offset easky-exec-packages-options))
          (option
           (completing-read
            ,prompt
            (lambda (string predicate action)
              (if (eq action 'metadata)
                  `(metadata
                    (display-sort-function . ,#'identity)
                    (annotation-function
                     . ,(lambda (cand)
                          (concat (propertize " " 'display `((space :align-to (- right ,offset))))
                                  (cdr (assoc cand easky-exec-packages-options))))))
                (complete-with-action action easky-exec-packages-options string predicate)))
            nil t nil nil (nth 0 easky-exec-packages-options)))
          (index (cl-position option (mapcar #'car easky-exec-packages-options) :test 'string=)))
     (pcase index
       (0 ,form-1)
       (1 ,form-2))))

;;;###autoload
(defun easky-install ()
  "Install packages."
  (interactive)
  (easky--exec-with-packages "Select `install' action: "
    (easky--display (easky-command "install"))
    (let ((pattern (read-string "Specify packages: ")))
      (easky--display (easky-command "install" pattern)))))

;;;###autoload
(defun easky-uninstall ()
  "Uninstall packages."
  (interactive)
  (easky--exec-with-packages "Select `uninstall' action: "
    (easky--display (easky-command "uninstall"))
    (let ((pattern (read-string "Specify packages: ")))
      (easky--display (easky-command "uninstall" pattern)))))

;;;###autoload
(defun easky-reinstall ()
  "Reinstall packages."
  (interactive)
  (easky--exec-with-packages "Select `reinstall' action: "
    (easky--display (easky-command "reinstall"))
    (let ((pattern (read-string "Specify packages: ")))
      (easky--display (easky-command "reinstall" pattern)))))

;;;###autoload
(defun easky-upgrade ()
  "Upgrade packages."
  (interactive)
  (easky--exec-with-packages "Select `upgrade' action: "
    (easky--display (easky-command "upgrade"))
    (let ((pattern (read-string "Specify packages: ")))
      (easky--display (easky-command "upgrade" pattern)))))

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
  "Do all cleaning tasks."
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

;;;###autoload
(defun easky-test-ert ()
  "Run ert test."
  (interactive)
  (easky--exec-with-files "Select `ert' action: "
    (easky--display (easky-command "test" "ert"))
    (let ((file (read-file-name "Select file for `ert': "
                                nil nil t nil #'easky--select-el-files)))
      (easky--display (easky-command "test" "ert" file)))
    (let ((wildcards (read-string "Wildcards: ")))
      (easky--display (easky-command "test" "ert" wildcards)))))

;;;###autoload
(defun easky-test-ert-runner ()
  "Run ert test through `ert-runner'."
  (interactive)
  (easky--exec-with-files "Select `ert-runner' action: "
    (easky--display (easky-command "test" "ert-runner"))
    (let ((file (read-file-name "Select file for `ert-runner': "
                                nil nil t nil #'easky--select-el-files)))
      (easky--display (easky-command "test" "ert-runner" file)))
    (let ((wildcards (read-string "Wildcards: ")))
      (easky--display (easky-command "test" "ert-runner" wildcards)))))

;;;###autoload
(defun easky-test-buttercup ()
  "Run buttercup test."
  (interactive)
  (easky--exec-with-files "Select `buttercup' action: "
    (easky--display (easky-command "test" "buttercup"))
    (let ((file (read-file-name "Select file for `regexps': "
                                nil nil t nil #'easky--select-el-files)))
      (easky--display (easky-command "test" "buttercup" file)))
    (let ((wildcards (read-string "Wildcards: ")))
      (easky--display (easky-command "test" "buttercup" wildcards)))))

(provide 'easky)
;;; easky.el ends here
