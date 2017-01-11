;; -*- lexical-binding: t -*-

(require 'evil)
(require 'hydra)

(evil-set-initial-state 'diff-mode 'motion)
(evil-set-initial-state 'backups-mode 'insert)
(evil-set-initial-state 'erc-mode 'emacs)
(evil-set-initial-state 'package-menu-mode 'motion)
(evil-set-initial-state 'profiler-report-mode 'motion)

(add-to-list 'evil-overriding-maps '(backup-walker-mode-map))

;; Evil exchange, easily swap two things
(define-key evil-normal-state-map "gx" #'evil-exchange)
(define-key evil-visual-state-map "gx" #'evil-exchange)
(define-key evil-normal-state-map "gX" #'evil-exchange-cancel)
(define-key evil-visual-state-map "gX" #'evil-exchange-cancel)

;; Evil surround + embrace, easily change surrounding chars
(add-hook 'LaTeX-mode-hook 'embrace-LaTeX-mode-hook)
(add-hook 'org-mode-hook 'embrace-org-mode-hook)
(add-hook 'ruby-mode-hook 'embrace-ruby-mode-hook)

(with-eval-after-load 'evil-surround
  (evil-embrace-enable-evil-surround-integration))

(autoload #'evil-surround-edit "evil-surround")
(autoload #'evil-Surround-edit "evil-surround")
(autoload #'evil-surround-region "evil-surround")
(autoload #'evil-Surround-region "evil-surround")

(define-key evil-operator-state-map "s" 'evil-surround-edit)
(define-key evil-operator-state-map "S" 'evil-Surround-edit)

(define-key evil-visual-state-map (kbd "S") 'evil-surround-region)
(define-key evil-visual-state-map (kbd "gS") 'evil-Surround-region)

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

  (diminish 'evil-snipe-local-mode)

  (setq evil-snipe-scope 'visible
        evil-snipe-repeat-scope 'visible
        evil-snipe-smart-case t
        evil-snipe-tab-increment t)

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

(with-eval-after-load 'session
  (eval-when-compile
    (with-demoted-errors "Load error: %s"
      (require 'session)))
  (defun my/evil--jumps-savehist-load ()
    (let ((ring (make-ring evil-jumps-max-length)))
      (cl-loop for jump in (reverse evil-jumps-history)
               do (ring-insert ring jump))
      (setf (evil-jumps-struct-ring (evil--jumps-get-current)) ring)))
  (add-hook 'session-after-load-save-file-hook #'my/evil--jumps-savehist-load)
  (defun nadvice/session-save-session/evil--jumps (&rest _args)
    (evil--jumps-savehist-sync))
  (advice-add 'session-save-session :before
              #'nadvice/session-save-session/evil--jumps)
  (add-to-list 'session-globals-include 'evil-jumps-history))

(provide 'config-evil-modules)
