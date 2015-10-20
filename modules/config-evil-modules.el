;; -*- lexical-binding: t -*-

(eval-when-compile
  (with-demoted-errors "Load error: %s"
    (require 'evil)))

(evil-set-initial-state 'diff-mode 'motion)
(evil-set-initial-state 'backups-mode 'insert)
(evil-set-initial-state 'erc-mode 'emacs)
(evil-set-initial-state 'package-menu-mode 'motion)
(evil-set-initial-state 'undo-tree-visualizer-mode 'motion)
(evil-set-initial-state 'profiler-report-mode 'motion)
(evil-set-initial-state 'backup-walker-mode 'motion)


;; Evil exchange, easily swap two things
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

(define-key evil-visual-state-map (kbd "gw") 'evil-surround-region)
(define-key evil-visual-state-map (kbd "gW") 'evil-Surround-region)

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
  (eval-when-compile
    (with-demoted-errors "Load error: %s"
      (require 'evil-snipe)))

  (diminish 'evil-snipe-mode)

  (setq evil-snipe-scope 'visible
        evil-snipe-repeat-scope 'visible
        evil-snipe-smart-case t)

  (set-face-attribute 'evil-snipe-matches-face nil
                      :background "#586e75")

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

(with-eval-after-load 'evil-quickscope
  (eval-when-compile
    (with-demoted-errors "Load error: %s"
      (require 'evil-quickscope)))

  (setq evil-quickscope-word-separator " -./")
  (set-face-attribute 'evil-quickscope-first-face nil
                      :inherit nil)

  (if (display-graphic-p)
      (set-face-attribute 'evil-quickscope-second-face nil
                          :underline '(:style wave)
                          :inherit nil)
    (set-face-attribute 'evil-quickscope-second-face nil
                        :inherit nil))

  (defun nadvice/evil-quickscope-update-overlays-bidirectional ()
    "Update overlays in both directions from point."
    (evil-quickscope-remove-overlays)
    (when (memq evil-state '(normal motion))
      (evil-quickscope-apply-overlays-forward)
      (evil-quickscope-apply-overlays-backward)))

  (advice-add 'evil-quickscope-update-overlays-bidirectional
              :override
              #'nadvice/evil-quickscope-update-overlays-bidirectional))

(unless (or (bound-and-true-p my/slow-device)
            (< (display-color-cells) 256))
  (global-evil-quickscope-always-mode +1))

(with-eval-after-load 'evil-jumper
  (eval-when-compile
    (with-demoted-errors "Load error: %s"
      (require 'evil-jumper)))

  (setq evil-jumper-file (expand-file-name "jumps" user-emacs-directory))
  (evil-jumper--init-file))

(define-key evil-normal-state-map (kbd "C-o") #'evil-jumper/backward)
(define-key evil-normal-state-map (kbd "C-i") #'evil-jumper/forward)
(define-key evil-motion-state-map (kbd "C-o") #'evil-jumper/backward)
(define-key evil-motion-state-map (kbd "C-i") #'evil-jumper/forward)

(autoload #'evil-jumper--set-jump "evil-jumper")
(autoload #'evil-jumper--window-configuration-hook "evil-jumper")

(defun nadvice/autoload-evil-jumper (&rest _args)
  (require 'evil-jumper)
  (evil-jumper--set-jump))

(add-hook 'next-error-hook #'evil-jumper--set-jump)
(add-hook 'window-configuration-change-hook
          #'evil-jumper--window-configuration-hook)

(advice-add 'evil-set-jump :after #'nadvice/autoload-evil-jumper)
(advice-add 'switch-to-buffer :before #'nadvice/autoload-evil-jumper)
(advice-add 'find-tag-noselect :after #'nadvice/autoload-evil-jumper)

(provide 'config-evil-modules)
