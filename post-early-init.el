;; Set the default font to DejaVu Sans Mono with specific size and weight
(set-face-attribute 'default nil :height 160 :weight 'normal :family "Iosevka Term SS17")

;; Set theme
(mapc #'disable-theme custom-enabled-themes)  ; Disable all active themes
(load-theme 'modus-operandi t)  ; Load the built-in theme

(setq initial-scratch-message
";; minimal-emacs.d with Clojure — use manage-packages.sh for packages
;; 
;; This buffer is for text that is not saved, and for Lisp evaluation
;; To create a file, visit it with ‘C-x C-f’ and enter text in its buffer\n\n")
