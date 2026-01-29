;;; manage-packages.el --- robust package manager for script use -*- lexical-binding: t; -*-

(require 'package)

;; Archives MUST be set before initialize
(setq package-archives
      '(("gnu"    . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa"  . "https://melpa.org/packages/")))

(defun packages--init ()
  "Initialize package.el reliably for batch/script use."
  (setq package-enable-at-startup nil)
  (package-initialize)
  (unless package-archive-contents
    (message "Refreshing package archives...")
    (package-refresh-contents)))

(defun packages--load-list (file)
  "Load FILE and return `my/package-list`."
  (unless (file-exists-p file)
    (error "packages.el not found: %s" file))
  (let ((load-path (cons (file-name-directory file) load-path)))
    (load (file-name-sans-extension file) nil t)
    (unless (boundp 'my/package-list)
      (error "my/package-list not defined in %s" file))
    my/package-list))

(defun packages--installed-p (pkg)
  "Return non-nil if PKG is installed."
  (package-installed-p pkg))

(defun packages--latest-desc (pkg)
  "Return latest available package-desc for PKG."
  (let ((entry (assq pkg package-archive-contents)))
    (when entry
      (car (sort (cdr entry)
                 (lambda (a b)
                   (version-list-< (package-desc-version b)
                                   (package-desc-version a))))))))

(defun packages--install-one (pkg)
  "Install or upgrade PKG with retry on MELPA failures."
  (let ((desc (packages--latest-desc pkg)))
    (unless desc
      (error "Package not found in archives: %s" pkg))
    (message "Installing/upgrading %s..." pkg)
    (condition-case err
        (package-install desc)
      (file-error
       (message "Install failed for %s (%S), refreshing archives and retrying..."
                pkg err)
       ;; MELPA metadata may be stale
       (package-refresh-contents)
       (let ((retry-desc (packages--latest-desc pkg)))
         (unless retry-desc
           (error "Package %s disappeared after refresh" pkg))
         (package-install retry-desc))))))


;;;###autoload
(defun packages-install (&optional packages-file)
  "Install packages listed in PACKAGES-FILE."
  (packages--init)
  (let ((pkgs (packages--load-list packages-file)))
    ;; bootstrap use-package
    (unless (packages--installed-p 'use-package)
      (packages--install-one 'use-package))
    (dolist (pkg pkgs)
      (packages--install-one pkg)))
  (message "packages-install complete."))

;;;###autoload
(defun packages-remove (&optional packages-file)
  "Remove installed packages not listed in PACKAGES-FILE."
  (packages--init)
  (let* ((keep (packages--load-list packages-file))
         (installed (mapcar #'car package-alist)))
    (dolist (pkg installed)
      (unless (memq pkg keep)
        (let ((desc (cadr (assq pkg package-alist))))
          (when desc
            (message "Removing %s..." pkg)
            (package-delete desc t))))))
  (message "packages-remove complete."))

;;;###autoload
(defun packages-clean ()
  "Remove orphaned dependencies."
  (packages--init)
  (when (fboundp 'package-autoremove)
    (package-autoremove))
  (message "packages-clean complete."))

;;;###autoload
(defun packages-update (&optional packages-file)
  "Update installed packages (restricted to PACKAGES-FILE if provided)."
  (packages--init)
  (let* ((limit (and packages-file
                     (packages--load-list packages-file)))
         (installed (mapcar #'car package-alist)))
    (dolist (pkg installed)
      (when (or (null limit) (memq pkg limit))
        (let* ((installed-desc (cadr (assq pkg package-alist)))
               (latest-desc (packages--latest-desc pkg)))
          (when (and latest-desc
                     (version-list-< (package-desc-version installed-desc)
                                     (package-desc-version latest-desc)))
            (packages--install-one pkg))))))
  (message "packages-update complete."))

;;;###autoload
(defun packages-list (&optional packages-file)
  "Print desired and installed packages."
  (packages--init)
  (when packages-file
    (message "Desired packages: %S"
             (packages--load-list packages-file)))
  (message "Installed packages: %S"
           (mapcar #'car package-alist)))

(provide 'manage-packages)
;;; manage-packages.el ends here

