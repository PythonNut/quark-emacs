;; Evil exchange, easily swap two things
(autoload 'evil-exchange        "evil-exchange")
(autoload 'evil-exchange-cancel "evil-exchange")

(define-key evil-normal-state-map "gx" 'evil-exchange)
(define-key evil-visual-state-map "gx" 'evil-exchange)
(define-key evil-normal-state-map "gX" 'evil-exchange-cancel)
(define-key evil-visual-state-map "gX" 'evil-exchange-cancel)

;; Evil surround, easily change surrounding chars
(require 'evil-surround)
(global-evil-surround-mode +1)

;; Also change cursor colors in a terminal
(unless (display-graphic-p)
  (require 'evil-terminal-cursor-changer))
