;;; packages.el --- Explicit package management -*- lexical-binding: t; -*-

;;; Commentary:

;; Emacs startup only loads installed packages. These commands are the sole
;; entry points for changing the installed package set.

;;; Code:

(require 'package)
(require 'subr-x)
(require 'seq)

(defconst mx-packages-selected
  '(use-package ag dash s projectile vertico marginalia orderless
    embark-consult wgrep)
  "Packages used by this Emacs configuration.")

(setq package-selected-packages mx-packages-selected)

(defun mx-packages-missing ()
  "Return configured packages whose libraries are not available locally."
  (seq-filter
   (lambda (package)
     (not (locate-library (symbol-name package))))
   mx-packages-selected))

(defun mx-packages-initialize ()
  "Initialize locally installed packages without network or package changes.
Return the list of configured packages that are missing."
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

(defun mx-packages--run-step (description function)
  "Run FUNCTION while reporting DESCRIPTION and its result."
  (message "[INIT] %s..." description)
  (condition-case error-data
      (prog1 (funcall function)
        (message "[OK] %s." description))
    (error
     (message "[ERROR] %s: %s"
              description (error-message-string error-data))
     (signal (car error-data) (cdr error-data)))))

(defun mx-packages--missing ()
  "Return selected packages that are not currently installed."
  (let (missing)
    (dolist (package mx-packages-selected)
      (unless (package-installed-p package)
        (push package missing)))
    (nreverse missing)))

(defun mx-packages--report-installation-summary (installed failed refresh-error)
  "Report INSTALLED and FAILED packages and REFRESH-ERROR, if any."
  (message "")
  (message "Package installation summary")
  (message "----------------------------")
  (message "Installed (%d): %s"
           (length installed)
           (if installed
               (mapconcat #'symbol-name (nreverse installed) ", ")
             "none"))
  (message "Failed (%d): %s"
           (length failed)
           (if failed
               (mapconcat (lambda (failure)
                            (format "%s: %s" (car failure) (cdr failure)))
                          (nreverse failed) "; ")
             "none"))
  (when refresh-error
    (message "Archive refresh error: %s" refresh-error))
  (message ""))

(defun mx-packages-init ()
  "Refresh package archives and install missing configured packages.
Attempt every missing package even if an earlier installation fails.
Signal an error after reporting all failures so batch callers exit non-zero."
  (interactive)
  (message "")
  (message "Emacs package manager")
  (message "------------------------")
  (mx-packages--run-step
   "Initializing package archives"
   #'package-initialize)
  (let ((refresh-error nil)
        (installed nil)
        (failed nil))
    ;; Keep going after a refresh error: cached archive metadata may still
    ;; allow some packages to install, and every missing package is attempted.
    (message "[INIT] Refreshing package contents...")
    (condition-case error-data
        (progn
          (package-refresh-contents)
          (message "[OK] Refreshing package contents."))
      (error
       (setq refresh-error (error-message-string error-data))
       (message "[ERROR] Refreshing package contents: %s" refresh-error)))
    (let ((missing (mx-packages--missing)))
      (if missing
          (progn
            (message "")
            (message "[INIT] Installing %d missing package(s)..." (length missing))
            (dolist (package missing)
              (message "[INSTALL] %s" package)
              (condition-case error-data
                  (progn
                    (package-install package)
                    (push package installed))
                (error
                 (let ((error-message (error-message-string error-data)))
                   (push (cons package error-message) failed)
                   (message "[ERROR] Failed to install %s: %s"
                            package error-message)))))
        (message "[OK] All selected packages are already installed.")))
    (mx-packages--report-installation-summary installed failed refresh-error)
    (when (or failed refresh-error)
      (error "mx-emacs: package initialization failed (%d installation failure(s)%s)"
             (length failed)
             (if refresh-error ", archive refresh failed" ""))))
  (mx-packages--run-step
   "Initializing installed packages"
   #'package-initialize)
  (mx-packages--run-step
   "Loading package configuration"
   (lambda ()
     (load (expand-file-name "post-init.el" user-emacs-directory)
           nil 'nomessage)))
  (message "[OK] MX-Emacs initialization completed."))

(defun mx-packages-sync ()
  "Refresh package archives and upgrade installed packages."
  (interactive)
  (unless (fboundp 'package-upgrade-all)
    (user-error "mx-emacs: package-upgrade-all is unavailable in this Emacs version"))
  (message "")
  (message "Emacs package manager")
  (message "------------------------")
  (mx-packages--run-step
   "Refreshing package contents"
   #'package-refresh-contents)
  (mx-packages--run-step
   "Upgrading installed packages"
   #'package-upgrade-all)
  (message "[OK] Package synchronization completed."))

(defun mx-packages-clean ()
  "Remove installed packages not required by this configuration."
  (interactive)
  (setq package-selected-packages mx-packages-selected)
  (message "")
  (message "Emacs package manager")
  (message "------------------------")
  (mx-packages--run-step
   "Removing unselected packages"
   #'package-autoremove)
  (message "[OK] Package cleanup completed."))

(provide 'packages)

;;; packages.el ends here
