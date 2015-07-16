(eval-when-compile
  (with-demoted-errors
    (require 'evil)
    (require 'evil-snipe)
    (require 'evil-nerd-commenter)))

(evil-set-initial-state #'diff-mode 'motion)
(evil-set-initial-state #'backups-mode 'insert)
(evil-set-initial-state #'erc-mode 'emacs)
(evil-set-initial-state #'package-menu-mode 'motion)
(evil-set-initial-state #'undo-tree-visualizer-mode 'motion)
(evil-set-initial-state #'profiler-report-mode 'motion)

;; Evil exchange, easily swap two things
(autoload #'evil-exchange        "evil-exchange")
(autoload #'evil-exchange-cancel "evil-exchange")

(define-key evil-normal-state-map "gx" #'evil-exchange)
(define-key evil-visual-state-map "gx" #'evil-exchange)
(define-key evil-normal-state-map "gX" #'evil-exchange-cancel)
(define-key evil-visual-state-map "gX" #'evil-exchange-cancel)

(autoload #'evil-surround-edit "evil-surround")
(autoload #'evil-Surround-edit "evil-surround")
(autoload #'evil-surround-region "evil-surround")
(autoload #'evil-Surround-region "evil-surround")

;; Evil surround, easily change surrounding chars
(define-key evil-operator-state-map "s" 'evil-surround-edit)
(define-key evil-operator-state-map "S" 'evil-Surround-edit)

(define-key evil-visual-state-map (kbd "C-s") 'evil-surround-region)
(define-key evil-visual-state-map (kbd "C-S-s") 'evil-Surround-region)

;; evil NERD commenter, commenting awesomeness!
(global-set-key (kbd "M-;") #'evilnc-comment-or-uncomment-lines)

(autoload #'evilmi-inner-text-object "evil-matchit")
(autoload #'evilmi-outer-text-object "evil-matchit")

;; evil matchit, jump between matching tags and keywords
(define-key evil-normal-state-map "%" #'evilmi-jump-items)
(define-key evil-visual-state-map "%" #'evilmi-jump-items)

(define-key evil-inner-text-objects-map "%" #'evilmi-inner-text-object)
(define-key evil-outer-text-objects-map "%" #'evilmi-outer-text-object)

(with-eval-after-load 'evil-matchit
  (evilmi-init-plugins))

(autoload #'evil-inner-arg "evil-args")
(autoload #'evil-outer-arg "evil-args")
(autoload #'evil-forward-arg "evil-args")
(autoload #'evil-backward-arg "evil-args")

(define-key evil-inner-text-objects-map "a" #'evil-inner-arg)
(define-key evil-outer-text-objects-map "a" #'evil-outer-arg)

(define-key evil-normal-state-map "K" #'evil-jump-out-args)

(with-eval-after-load 'evil-snipe
  (diminish 'evil-snipe-mode)

  (setq
    evil-snipe-scope 'visible
    evil-snipe-repeat-scope 'visible
    evil-snipe-override-evil t
    evil-snipe-smart-case t)

  (evil-snipe-mode +1)
  (evil-snipe-override-mode +1))

(autoload #'evil-snipe-f "evil-snipe")
(autoload #'evil-snipe-F "evil-snipe")
(autoload #'evil-snipe-t "evil-snipe")
(autoload #'evil-snipe-T "evil-snipe")
(autoload #'evil-snipe-s "evil-snipe")
(autoload #'evil-snipe-S "evil-snipe")
(autoload #'evil-snipe-x "evil-snipe")
(autoload #'evil-snipe-X "evil-snipe")
(autoload #'evil-snipe-s "evil-snipe")
(autoload #'evil-snipe-S "evil-snipe")

(define-key evil-motion-state-map "f" #'evil-snipe-f)
(define-key evil-motion-state-map "F" #'evil-snipe-F)
(define-key evil-motion-state-map "t" #'evil-snipe-t)
(define-key evil-motion-state-map "T" #'evil-snipe-T)
(define-key evil-motion-state-map "s" #'evil-snipe-s)
(define-key evil-motion-state-map "S" #'evil-snipe-S)

(define-key evil-normal-state-map "f" #'evil-snipe-f)
(define-key evil-normal-state-map "F" #'evil-snipe-F)
(define-key evil-normal-state-map "t" #'evil-snipe-t)
(define-key evil-normal-state-map "T" #'evil-snipe-T)
(define-key evil-normal-state-map "s" #'evil-snipe-s)
(define-key evil-normal-state-map "S" #'evil-snipe-S)

(define-key evil-visual-state-map "f" #'evil-snipe-f)
(define-key evil-visual-state-map "F" #'evil-snipe-F)
(define-key evil-visual-state-map "t" #'evil-snipe-t)
(define-key evil-visual-state-map "T" #'evil-snipe-T)
(define-key evil-visual-state-map "s" #'evil-snipe-s)
(define-key evil-visual-state-map "S" #'evil-snipe-S)

(provide 'config-evil-modules)
