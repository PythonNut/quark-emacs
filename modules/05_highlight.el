;;; ======================
;;; Highlight current line
;;; ======================
(require 'highlight-current-line)
(highlight-current-line-on "y")

(highlight-current-line-set-bg-color "grey20")
(when (display-graphic-p)
  (highlight-current-line-set-bg-color "#073642"))

(column-number-mode +1)
;;(size-indication-mode +1)

;; highlight-current-symbol
(require 'auto-highlight-symbol)
(define-key auto-highlight-symbol-mode-map (kbd "M-<left>") nil)
(define-key auto-highlight-symbol-mode-map (kbd "M-<right>") nil)
(define-key auto-highlight-symbol-mode-map (kbd "C-c <left>") 'ahs-backward)
(define-key auto-highlight-symbol-mode-map (kbd "C-c <right>") 'ahs-forward)
(global-auto-highlight-symbol-mode +1)

(set-face-background 'ahs-definition-face "grey10")
(set-face-foreground 'ahs-definition-face 'nil)

(set-face-background 'ahs-plugin-defalt-face "grey20")
(set-face-foreground 'ahs-plugin-defalt-face 'nil)
(set-face-background 'ahs-face 'nil)
(set-face-foreground 'ahs-face 'nil)
(set-face-attribute 'ahs-face nil :weight 'semi-bold)

(global-set-key (kbd "C-c <left>") 'auto-ahs-backward)
(global-set-key (kbd "C-c <right>") 'auto-ahs-forward)
(define-key auto-highlight-symbol-mode-map (kbd "M-S-<left>") nil)
(define-key auto-highlight-symbol-mode-map (kbd "M-S-<right>") nil)

