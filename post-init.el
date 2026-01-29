;;; post-init.el --- Clojure development setup -*- lexical-binding: t; -*-

;; package initialization (keep this minimal; do NOT auto-install packages here)
(require 'package)

(setq package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("nongnu". "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")))

(unless package--initialized
  (package-initialize))

;; Do NOT force installation during runtime. Package management is handled outside
;; of runtime via manage-packages.sh / manage-packages.el.
;; Ensure use-package is available in your managed list; if you prefer, the
;; package manager can install it for you.

(require 'use-package nil t) ;; optional: if installed, fine; if not, we won't fail at startup
(setq use-package-always-ensure nil) ;; do not auto-install at runtime

;; --- runtime configuration (assumes packages are already installed) ---

(use-package paredit
  :ensure nil
  :hook ((clojure-mode clojurec-mode clojurescript-mode emacs-lisp-mode)
         . paredit-mode))

(use-package rainbow-delimiters
  :ensure nil
  :hook ((clojure-mode clojurec-mode clojurescript-mode)
         . rainbow-delimiters-mode))

(use-package aggressive-indent
  :ensure nil
  :hook ((clojure-mode clojurec-mode clojurescript-mode)
         . aggressive-indent-mode))

;; treesit grammar location
(setq treesit-language-source-alist
      '((clojure "https://github.com/sogaiu/tree-sitter-clojure")))

(when (fboundp 'treesit-install-language-grammar)
  (unless (treesit-language-available-p 'clojure)
    (ignore-errors (treesit-install-language-grammar 'clojure))))

(use-package clojure-ts-mode
  :mode ("\\.clj\\'" "\\.cljc\\'" "\\.cljs\\'")
  :config
  (setq clojure-ts-indent-style 'semantic))
;; (add-hook 'clojure-ts-mode-hook #'electric-pair-mode)
;; (add-hook 'clojure-ts-mode-hook #'subword-mode)


(use-package cider
  :ensure nil
  :after clojure-ts-mode
  :hook ((clojure-ts-mode . cider-mode))
  :config
  (setq cider-repl-display-help-banner nil
        cider-repl-pop-to-buffer-on-connect 'display-only
        cider-repl-use-pretty-printing t
        cider-show-error-buffer 'except-in-repl
        cider-font-lock-dynamically '(macro core function var)
        cider-save-file-on-load t
        cider-use-overlays t))

(use-package corfu
  :ensure nil
  :init
  (when (fboundp 'global-corfu-mode) (global-corfu-mode))
  :config
  (setq corfu-auto t corfu-cycle t))

(use-package cape
  :ensure nil
  :init
  (when (boundp 'completion-at-point-functions)
    (add-to-list 'completion-at-point-functions #'cape-dabbrev)
    (add-to-list 'completion-at-point-functions #'cape-file)))

(use-package flycheck
  :ensure nil
  :init (when (fboundp 'global-flycheck-mode) (global-flycheck-mode)))

(use-package flycheck-clj-kondo
  :ensure nil
  :after flycheck
  :config (ignore-errors (require 'flycheck-clj-kondo)))

(use-package projectile
  :ensure nil
  :init
  (when (require 'projectile nil t)
    (projectile-mode))
  :config (setq projectile-project-search-path '("~/code" "~/src")))

(use-package ripgrep :ensure nil)
(use-package babashka :ensure nil :defer t)
(use-package zprint-mode :ensure nil :hook (clojure-ts-mode . zprint-mode))

(setq clojure-indent-style 'align-arguments)
(setq clojure-align-forms-automatically t)

(setq print-length 100
      print-level 20)

(provide 'post-init-clj)
;;; post-init.el ends here
