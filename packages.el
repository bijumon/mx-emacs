;;; packages.el --- Package management -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Centralize package archive configuration and package lifecycle commands.
;; This file is loaded from early-init.el so archive settings are in place
;; before package.el is initialized.

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

(defun mx-packages-initialize (&optional refresh)
  "Initialize package.el and ensure use-package is available.
When REFRESH is non-nil, refresh package archive contents unconditionally."
  (interactive "P")
  (package-initialize)
  (when (or refresh (null package-archive-contents))
    (message "mx-emacs: refreshing package archive contents...")
    (package-refresh-contents))
  ;; Emacs 29 includes use-package; older supported installations need it
  ;; installed from the configured archives.
  (unless (package-installed-p 'use-package)
    (message "mx-emacs: installing use-package...")
    (package-install 'use-package))
  (require 'use-package)
  (message "mx-emacs: package system initialized"))

(defun mx-packages-init ()
  "Initialize packages declared by the loaded configuration.
The configuration's `:ensure' declarations install missing packages as
they are evaluated during startup."
  (interactive)
  (mx-packages-initialize)
  (unless (featurep 'use-package)
    (user-error "use-package is not available"))
  (message "mx-emacs: package declarations have been processed"))

(defun mx-packages-sync ()
  "Refresh package archives and upgrade installed packages."
  (interactive)
  (mx-packages-initialize t)
  (message "mx-emacs: upgrading installed packages...")
  (if (fboundp 'package-upgrade-all)
      (package-upgrade-all)
    (user-error "This Emacs version does not provide package-upgrade-all"))
  (message "mx-emacs: package synchronization complete"))

(defun mx-packages-clean ()
  "Remove packages that are no longer required by the configuration.
The configuration must be loaded first so package-selected-packages
reflects its current use-package declarations."
  (interactive)
  (mx-packages-initialize)
  (message "mx-emacs: removing unused packages...")
  (package-autoremove)
  (message "mx-emacs: package cleanup complete"))

(provide 'mx-packages)

;;; packages.el ends here
