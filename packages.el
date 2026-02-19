;;; packages.el --- list of packages to manage -*- lexical-binding: t; -*-

;; Keep this file purely as a data file: a symbol list of packages to manage.
;; It is NOT loaded by post-init.el; it's only used by manage-packages.el.

(defvar my/package-list
  '(
    ;; Core development & editing
    paredit
    rainbow-delimiters
    aggressive-indent

    ;; Clojure / Tree-sitter / REPL
    clojure-ts-mode
    cider

    ;; Completion
    corfu
    cape

    ;; Linting/formatting
    flycheck
    flycheck-clj-kondo
    zprint-mode

    ;; Project tools
    projectile
    ripgrep

    ;; Tools
    babashka
    
    vterm
    )
  "List of packages to be installed/managed by manage-packages.el.")

(provide 'my-packages)
;;; packages.el ends here
