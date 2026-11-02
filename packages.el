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

(defun mx-packages-initialize ()
  "Initialize the local package database without network access."
  (interactive)
  (package-initialize)
  ;; Emacs 29 bundles use-package. Do not install it during startup.
  (unless (require 'use-package nil t)
    (display-warning 'mx-emacs
                     "use-package is unavailable; run M-x mx-packages-init"
                     :warning))
  t)

(defun mx-packages-init ()
  "Refresh archives and install packages required by this configuration."
  (interactive)
  (package-refresh-contents)
  (package-install-selected-packages)
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
