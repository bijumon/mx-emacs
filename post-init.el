;;; post-init.el --- User package configuration -*- lexical-binding: t; -*-

;; Optional package configuration. Startup remains usable when one or more
;; packages have not yet been installed; run ./mx-emacs.sh init to install them.
(when (and (locate-library "use-package")
           (require 'use-package nil t))
  ;; Vertico provides a vertical minibuffer completion interface.
  (when (locate-library "vertico")
    (use-package vertico
      :config
      (setq vertico-cycle t
            vertico-resize nil)
      (vertico-mode 1)))

  ;; Marginalia adds annotations to completion candidates.
  (when (locate-library "marginalia")
    (use-package marginalia
      :config
      (marginalia-mode 1)))

  ;; Orderless enables out-of-order pattern matching.
  (when (locate-library "orderless")
    (use-package orderless
      :config
      (setq completion-styles '(orderless basic))))

  ;; Consult provides enhanced versions of built-in commands.
  (when (locate-library "consult")
    (use-package consult
      :bind (("M-s M-g" . consult-grep)
             ("M-s M-f" . consult-find)
             ("M-s M-o" . consult-outline)
             ("M-s M-l" . consult-line)
             ("M-s M-b" . consult-buffer))))

  ;; Embark provides context-sensitive actions.
  (when (locate-library "embark")
    (use-package embark
      :bind (("C-." . embark-act)
             :map minibuffer-local-map
             ("C-c C-c" . embark-collect)
             ("C-c C-e" . embark-export))))

  ;; Glue code between Embark and Consult.
  (when (locate-library "embark-consult")
    (use-package embark-consult))

  ;; Edit grep results in a grep-mode buffer.
  (when (locate-library "wgrep")
    (use-package wgrep
      :bind (:map grep-mode-map
                  ("e" . wgrep-change-to-wgrep-mode)
                  ("C-x C-q" . wgrep-change-to-wgrep-mode)
                  ("C-c C-c" . wgrep-finish-edit)))))

;; Built-in features remain enabled even when optional packages are missing.
(savehist-mode 1)
(recentf-mode 1)

;;; Package selection
(setq package-selected-packages
      '(use-package vertico marginalia orderless consult embark
                    embark-consult wgrep))

;;; post-init.el ends here
