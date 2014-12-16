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

;; evil matchit, jump between matching tags and keywords
(define-key evil-normal-state-map "%" #'evilmi-jump-items)
(define-key evil-inner-text-objects-map "%" #'evilmi-text-object)
(define-key evil-outer-text-objects-map "%" #'evilmi-text-object)

(with-eval-after-load 'evil-matchit
  (evilmi-init-plugins))

(autoload 'evil-inner-arg "evil-args")
(autoload 'evil-outer-arg "evil-args")
(autoload 'evil-forward-arg "evil-args")
(autoload 'evil-backward-arg "evil-args")

(define-key evil-inner-text-objects-map "a" #'evil-inner-arg)
(define-key evil-outer-text-objects-map "a" #'evil-outer-arg)

;; (define-key evil-normal-state-map "L" #'evil-forward-arg)
;; (define-key evil-normal-state-map "H" #'evil-backward-arg)
;; (define-key evil-motion-state-map "L" #'evil-forward-arg)
;; (define-key evil-motion-state-map "H" #'evil-backward-arg)

(define-key evil-normal-state-map "K" #'evil-jump-out-args)

(provide 'config-evil-modules)
