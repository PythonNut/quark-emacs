(eval-when-compile
  (progn
    (require 'cl)
    (require 'evil)
    (require 'evil-nerd-commenter)))

(evil-set-initial-state #'diff-mode 'motion)
(evil-set-initial-state #'backups-mode 'insert)
(evil-set-initial-state #'erc-mode 'emacs)
(evil-set-initial-state #'git-commit-mode 'insert)
(evil-set-initial-state #'backup-walker-mode 'motion)
(evil-set-initial-state #'package-menu-mode 'motion)
(evil-set-initial-state #'undo-tree-visualizer-mode 'motion)

;; Evil exchange, easily swap two things
(autoload #'evil-exchange        "evil-exchange")
(autoload #'evil-exchange-cancel "evil-exchange")

(define-key evil-normal-state-map "gx" #'evil-exchange)
(define-key evil-visual-state-map "gx" #'evil-exchange)
(define-key evil-normal-state-map "gX" #'evil-exchange-cancel)
(define-key evil-visual-state-map "gX" #'evil-exchange-cancel)

;; Evil surround, easily change surrounding chars
(global-evil-surround-mode +1)

;; Also change cursor colors in a terminal
(unless (display-graphic-p)
  (require 'evil-terminal-cursor-changer))

;; evil NERD commenter, commenting awesomeness!
(global-set-key (kbd "M-;") #'evilnc-comment-or-uncomment-lines)

(evil-leader/set-key
  "cl" #'evilnc-comment-or-uncomment-to-the-line
  "cc" #'evilnc-copy-and-comment-lines)

(provide 'config-evil-modules)
