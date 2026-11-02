;;; packages.el --- Package management -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Package startup is deliberately offline. Package changes happen only through
;; the explicit commands in this file.

;;; Code:

(require 'package)

(setq
 package-enable-at-startup nil
 package-quickstart-file (expand-file-name "package-quickstart.el"
                                           user-emacs-directory)
 package-archives '(("melpa"        . "https://melpa.org/packages/")
                    ("gnu"          . "https://elpa.gnu.org/packages/")
                    ("nongnu"       . "https://elpa.nongnu.org/nongnu/")
                    ("melpa-stable" . "https://stable.melpa.org/packages/"))
 package-archive-priorities '(("gnu"          . 99)
                              ("nongnu"       . 80)
                              ("melpa"        . 70)
                              ("melpa-stable" . 50)))

(defconst mx-packages-selected
  '(use-package vertico marginalia orderless consult embark
    embark-consult wgrep)
  "Packages required by this configuration.")

(setq package-selected-packages mx-packages-selected)

(defun mx-packages-missing ()
  "Return required packages whose libraries are not available locally."
  (seq-filter
   (lambda (package)
     (not (locate-library (symbol-name package))))
   mx-packages-selected))

(defun mx-packages-initialize ()
  "Initialize local packages without network access or package changes.
Return the list of missing packages."
  (interactive)
  (package-initialize)
  (let ((missing (mx-packages-missing)))
    (if missing
        (display-warning
         'mx-emacs
         (format "Some configured packages are unavailable: %s. Their configuration will be skipped. Run ./mx-emacs.sh init to install them."
                 (mapconcat #'symbol-name missing ", "))
         :warning)
      (message "mx-emacs: all configured packages are available"))
    (when (locate-library "use-package")
      (require 'use-package nil t))
    missing))

(defun mx-packages-init ()
  "Refresh archives and install packages required by this configuration."
  (interactive)
  (package-refresh-contents)
  (package-install-selected-packages)
  ;; Re-evaluate declarations now that any missing packages are available.
  (let ((post-init (expand-file-name "post-init.el" user-emacs-directory)))
    (when (file-readable-p post-init)
      (load post-init nil t)))
  (message "mx-emacs: required packages are installed"))

(defun mx-packages-sync ()
  "Refresh archives and upgrade installed packages."
  (interactive)
  (package-refresh-contents)
  (if (fboundp 'package-upgrade-all)
      (package-upgrade-all)
    (user-error "This Emacs version does not provide package-upgrade-all"))
  (message "mx-emacs: package synchronization complete"))

(defun mx-packages-clean ()
  "Remove packages not required by this configuration."
  (interactive)
  (setq package-selected-packages mx-packages-selected)
  (package-autoremove)
  (message "mx-emacs: package cleanup complete"))

(provide 'mx-packages)

;;; packages.el ends here
