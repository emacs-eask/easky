;;; easky-package.el --- Control Eask's package module  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Shen, Jen-Chieh

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
;; This module simulate most important functionality from `package.el'
;;

;;; Code:

(require 'package)

(require 'easky)

(defmacro easky-package--setup (body &rest unwind)
  "Execute BODY without touching the Eask-file global variables.

The form UNWIND is use to revert package information."
  (declare (indent 1) (debug t))
  `(unwind-protect (easky--setup (package-initialize t) ,body) ,@unwind))

(defun easky-package--revert-info (&rest _)
  "Revert package inforamtion after we have displayed in Package Menu."
  (package-initialize t))

;;;###autoload
(defun easky-package-refresh-contents ()
  "Like command `package-refresh-contents' but in Eask sandbox."
  (interactive)
  (easky-package--setup
      (call-interactively #'package-refresh-contents)
    (easky-package--revert-info)))

;;;###autoload
(defun easky-list-packages ()
  "List packages."
  (interactive)
  (easky-package--setup
      (package-list-packages t)
    ;; XXX: We revert information after it's done displaying!
    (add-hook 'package-menu-mode-hook #'easky-package--revert-info)))

;;;###autoload
(defalias 'easky-package-list-packages 'easky-list-packages)

;;;###autoload
(defun easky-package-install ()
  "Install a package to Eask sandbox."
  (interactive)
  (easky-package--setup
      (call-interactively #'package-install)
    (easky-package--revert-info)))

;;;###autoload
(defun easky-package-delete ()
  "Delete a package from Eask sandbox."
  (interactive)
  (easky-package--setup
      (call-interactively #'package-delete)
    (easky-package--revert-info)))

;;;###autoload
(defun easky-package-reinstall ()
  "Reinstall a package in Eask sandbox."
  (interactive)
  (easky-package--setup
      (call-interactively #'package-reinstall)
    (easky-package--revert-info)))

;;;###autoload
(defun easky-package-recompile ()
  "Recompile a package in Eask sandbox."
  (interactive)
  (easky-package--setup
      (call-interactively #'package-recompile)
    (easky-package--revert-info)))

;;;###autoload
(defun easky-package-recompile-all ()
  "Recompile all packages in Eask sandbox."
  (interactive)
  (easky-package--setup
      (call-interactively #'package-recompile-all)
    (easky-package--revert-info)))

;;;###autoload
(defun easky-describe-package ()
  "Describe a package from Eask source."
  (interactive)
  (easky-package--setup
      (call-interactively #'describe-package)
    (easky-package--revert-info)))

;;;###autoload
(defun easky-package-update ()
  "Update a package from Eask sandbox."
  (interactive)
  (easky-package--setup
      (call-interactively #'package-update)
    (easky-package--revert-info)))

;;;###autoload
(defun easky-package-update-all ()
  "Update all packages from Eask sandbox."
  (interactive)
  (easky-package--setup
      (call-interactively #'package-update-all)
    (easky-package--revert-info)))

(provide 'easky-package)
;;; easky-package.el ends here
