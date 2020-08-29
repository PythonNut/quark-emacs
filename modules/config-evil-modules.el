;; -*- lexical-binding: t -*-
(eval-when-compile
  (require 'config-macros)
  (use-package evil))

(require 'evil)

(evil-set-initial-state 'special-mode 'motion)
(evil-set-initial-state 'backups-mode 'insert)
(evil-set-initial-state 'erc-mode 'emacs)
(evil-set-initial-state 'package-menu-mode 'motion)
(evil-set-initial-state 'profiler-report-mode 'motion)

(add-to-list 'evil-overriding-maps '(backup-walker-mode-map))

;; Evil exchange, easily swap two things
(use-package evil-exchange
  :commands (evil-exchange
             evil-exchange-cancel)
  :init
  (define-key evil-normal-state-map "gx" #'evil-exchange)
  (define-key evil-visual-state-map "gx" #'evil-exchange)
  (define-key evil-normal-state-map "gX" #'evil-exchange-cancel)
  (define-key evil-visual-state-map "gX" #'evil-exchange-cancel))


;; Evil surround + embrace, easily change surrounding chars
(use-package embrace
  :commands (embrace-LaTeX-mode-hook
             embrace-org-mode-hook
             embrace-ruby-mode-hook)
  :init
  (add-hook 'org-mode-hook 'embrace-org-mode-hook)
  (add-hook 'ruby-mode-hook 'embrace-ruby-mode-hook))

(use-package evil-embrace
  :commands (evil-embrace-enable-evil-surround-integration
             evil-embrace-disable-evil-surround-integration))

(use-package evil-surround
  :commands (evil-surround-edit
             evil-Surround-edit
             evil-surround-region
             evil-Surround-region)

  :init
  (define-key evil-operator-state-map "s" 'evil-surround-edit)
  (define-key evil-operator-state-map "S" 'evil-Surround-edit)
  (define-key evil-visual-state-map (kbd "S") 'evil-surround-region)
  (define-key evil-visual-state-map (kbd "gS") 'evil-Surround-region)

  :config
  (evil-embrace-enable-evil-surround-integration))


;; evil NERD commenter, commenting awesomeness!
(use-package evil-nerd-commenter
  :init (global-set-key (kbd "M-;") #'evilnc-comment-or-uncomment-lines))

(use-package evil-matchit
  :commands (evilmi-inner-text-object
             evilmi-outer-text-object)

  :init
  ;; evil matchit, jump between matching tags and keywords
  (define-key evil-normal-state-map "%" #'evilmi-jump-items)
  (define-key evil-visual-state-map "%" #'evilmi-jump-items)

  (define-key evil-inner-text-objects-map "%" #'evilmi-inner-text-object)
  (define-key evil-outer-text-objects-map "%" #'evilmi-outer-text-object)

  :config
  (evilmi-init-plugins))

(use-package evil-args
  :init
  (define-key evil-inner-text-objects-map "a" #'evil-inner-arg)
  (define-key evil-outer-text-objects-map "a" #'evil-outer-arg)
  (define-key evil-normal-state-map "K" #'evil-jump-out-args)
  :commands (evil-inner-arg
             evil-outer-arg
             evil-forward-arg
             evil-backward-arg
             evil-jump-out-args))

(use-package evil-snipe
  :diminish evil-snipe-local-mode
  :commands (evil-snipe-f
             evil-snipe-F
             evil-snipe-t
             evil-snipe-T
             evil-snipe-s
             evil-snipe-S
             evil-snipe-x
             evil-snipe-X
             evil-snipe-s
             evil-snipe-S)

  :init
  (define-key evil-motion-state-map "f" #'evil-snipe-f)
  (define-key evil-motion-state-map "F" #'evil-snipe-F)
  (define-key evil-motion-state-map "t" #'evil-snipe-t)
  (define-key evil-motion-state-map "T" #'evil-snipe-T)
  (define-key evil-motion-state-map "z" #'evil-snipe-s)
  (define-key evil-motion-state-map "Z" #'evil-snipe-S)

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
  (define-key evil-visual-state-map "z" #'evil-snipe-s)
  (define-key evil-visual-state-map "Z" #'evil-snipe-S)

  :config
  (setq evil-snipe-scope 'visible
        evil-snipe-repeat-scope 'visible
        evil-snipe-smart-case t
        evil-snipe-tab-increment t)

  (set-face-attribute 'evil-snipe-matches-face nil
                      :background "#586e75")

  (evil-snipe-mode +1)
  (evil-snipe-override-mode +1))

(use-package evil-quickscope
  :init (unless (or (bound-and-true-p my/slow-device)
                    (< (display-color-cells) 256))
          (global-evil-quickscope-always-mode +1))
  :config
  (setq evil-quickscope-word-separator " -./")
  (set-face-attribute 'evil-quickscope-first-face nil
                      :inherit nil)

  (if (display-graphic-p)
      (set-face-attribute 'evil-quickscope-second-face nil
                          :underline '(:style wave)
                          :inherit nil)
    (set-face-attribute 'evil-quickscope-second-face nil
                        :inherit nil))

  (advice-add
   'evil-quickscope-update-overlays-bidirectional :override
   (my/defun-as-value nadvice/evil-quickscope-update-overlays-bidirectional ()
     "Update overlays in both directions from point."
     (evil-quickscope-remove-overlays)
     (when (memq evil-state '(normal motion))
       (evil-quickscope-apply-overlays-forward)
       (evil-quickscope-apply-overlays-backward)))))

(use-package evil-lion
  :commands (evil-lion-left evil-lion-right)
  :init
  (define-key evil-normal-state-map (kbd "gl") 'evil-lion-left)
  (define-key evil-visual-state-map (kbd "gl") 'evil-lion-left)
  (define-key evil-normal-state-map (kbd "gL") 'evil-lion-right)
  (define-key evil-visual-state-map (kbd "gL") 'evil-lion-right))

(provide 'config-evil-modules)
