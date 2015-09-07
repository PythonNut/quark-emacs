;; -*- lexical-binding: t -*-

(eval-when-compile
  (with-demoted-errors "Load error: %s"
    (require 'cl-lib)
    (require 'hydra)
    (require 'key-chord)
    (require 'evil)
    (require 'diminish)
    (require 'evil-easymotion)
    (require 'config-modes)))

(setq mouse-wheel-scroll-amount '(3 ((shift) . 1))
      mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse t
      smooth-scroll-margin 5
      scroll-step 1
      auto-window-vscroll nil
      scroll-conservatively 1000)

(require 'config-line-numbers)
(require 'config-avy-easymotion)
(require 'config-undo)

(require 'config-smartparens)
(require 'config-rainbow-delimiters)

(require 'config-hydras)

(with-eval-after-load 'multiple-cursors
  (define-key mc/keymap (kbd "<return>") nil)
  (define-key mc/keymap (kbd "C-c <return>") 'multiple-cursors-mode))

(cl-macrolet
    ((autoload-multiple-cursors (func)
                                `(autoload ,func "multiple-cursors")))

  (with-no-warnings
    (my/generate-calls-single
     autoload-multiple-cursors
     (#'mc/mark-lines
      #'mc/mark-next-lines
      #'mc/mark-previous-lines
      #'mc/unmark-next-like-this
      #'mc/unmark-previous-like-this
      #'mc/skip-to-previous-like-this
      #'mc/mark-all-like-this
      #'mc/mark-all-words-like-this
      #'mc/mark-all-symbols-like-this
      #'mc/mark-all-in-region
      #'mc/mark-all-in-region-regexp
      #'mc/mark-more-like-this-extended
      #'mc/mmlte--up
      #'mc/mmlte--down
      #'mc/mmlte--left
      #'mc/mmlte--right
      #'mc/mark-all-like-this-dwim
      #'mc/mark-all-dwim
      #'mc/mark-all-like-this-in-defun
      #'mc/mark-all-words-like-this-in-defun
      #'mc/mark-all-symbols-like-this-in-defun
      #'mc/add-cursor-on-click
      #'mc/mark-sgml-tag-pair
      #'mc/mark-pop
      #'set-rectangular-region-anchor
      #'rrm/switch-to-multiple-cursors
      #'mc/insert-numbers
      #'mc/reverse-regions
      #'mc/sort-regions
      #'hum/keyboard-quit
      #'mc-hide-unmatched-lines-mode
      ))))

(global-set-key (kbd "C-c l") #'mc/edit-lines)
(global-set-key (kbd "C-c a") #'mc/mark-all-like-this-dwim)
(global-set-key (kbd "C-c s") #'mc/mark-all-in-region)
(global-set-key (kbd "C-c M") #'mc/mark-pop)
(global-set-key (kbd "C->") #'mc/mark-next-like-this)
(global-set-key (kbd "C-<") #'mc/mark-previous-like-this)

;; directional window movement
(global-set-key (kbd "<M-left>")  #'windmove-left)
(global-set-key (kbd "<M-right>") #'windmove-right)
(global-set-key (kbd "<M-up>")    #'windmove-up)
(global-set-key (kbd "<M-down>")  #'windmove-down)

(defun my/framemove-onetime-setup ()
  (windmove-default-keybindings 'meta)
  (require 'framemove)
  (remove-hook 'before-make-frame-hook #'my/framemove-onetime-setup))

;; directional frame movement too
(add-hook 'emacs-startup-hook
          (lambda ()
            (add-hook 'before-make-frame-hook #'my/framemove-onetime-setup)))

(with-eval-after-load 'framemove
  (setq framemove-hook-into-windmove t))

(global-set-key (kbd "C-.") #'er/expand-region)

(defun pop-window-into-frame ()
  (interactive)
  (let ((buffer (current-buffer)))
    (unless (one-window-p)
      (delete-window))
    (display-buffer-pop-up-frame buffer nil)))

;;; ====================================
;;; iflib - switch buffers alt-tab style
;;; ====================================
(let ((my/iflipb-running-p))
  (with-eval-after-load 'iflipb
    (setq iflipb-ignore-buffers '("^ " "^*helm" "^*Compile" "^*Quail")
          iflipb-wrap-around 't)

    (defun my/iflipb-smart-buffer ()
      (unless (fboundp 'iflipb-hydra/body)
        (require 'hydra)
        (defhydra iflipb-hydra
          (:pre (setq hydra-is-helpful nil)
                :post (setq hydra-is-helpful t))
          ("<C-tab>"
           (call-interactively #'iflipb-next-buffer))
          ("TAB"
           (call-interactively #'iflipb-next-buffer))
          ("<C-S-iso-lefttab>"
           (call-interactively #'iflipb-previous-buffer))
          ("<backtab>"
           (call-interactively #'iflipb-previous-buffer))))

      (iflipb-hydra/body))

    (defun nadvice/iflipb-first-iflipb-buffer-switch-command ()
      "Determines whether this is the first invocation of
  iflipb-next-buffer or iflipb-previous-buffer this round."
      my/iflipb-running-p)

    (advice-add 'iflipb-first-iflipb-buffer-switch-command :override
                #'nadvice/iflipb-first-iflipb-buffer-switch-command))

  (defun iflipb-next-buffer-smart ()
    "A `hydra' enabled next-buffer"
    (interactive)
    (let ((my/iflipb-running-p t))
      (call-interactively #'iflipb-next-buffer))
    (my/iflipb-smart-buffer))

  (defun iflipb-previous-buffer-smart ()
    "A `hydra' enabled previous-buffer"
    (interactive)
    (let ((my/iflipb-running-p t))
      (call-interactively #'iflipb-previous-buffer))
    (my/iflipb-smart-buffer)))

(global-set-key (kbd "<C-tab>") #'iflipb-next-buffer-smart)
(global-set-key (kbd "C-S-<iso-lefttab>") #'iflipb-previous-buffer-smart)

(global-set-key (kbd "C-c TAB") #'iflipb-next-buffer-smart)
(global-set-key (kbd "C-c <backtab>") #'iflipb-previous-buffer-smart)

;; also allow undo/redo on window configs
(add-hook 'window-configuration-change-hook #'winner-mode)

(defun my/vhl-onetime-setup ()
  (require 'volatile-highlights)
  (remove-hook 'first-change-hook #'my/vhl-onetime-setup))

(add-hook 'emacs-startup-hook
          (lambda ()
            (add-hook 'first-change-hook #'my/vhl-onetime-setup)))

(with-eval-after-load 'volatile-highlights
  (diminish #'volatile-highlights-mode)
  (with-demoted-errors "Load error: %s"
    (vhl/define-extension 'my-evil-highlights
                          'evil-yank
                          'evil-paste-pop-proxy
                          'evil-paste-pop-next
                          'evil-paste-after
                          'evil-paste-before)

    (vhl/install-extension 'my-evil-highlights)

    (vhl/define-extension 'my-undo-tree-highlights
                          'undo-tree-undo
                          'undo-tree-redo)

    (vhl/install-extension 'my-undo-tree-highlights)

    (volatile-highlights-mode +1)))

(with-eval-after-load 'auto-highlight-symbol
  (setq ahs-idle-interval 0.3)

  (defun my/ahs-setup-faces ()
    (if (display-graphic-p)
        (progn
          (set-face-attribute 'ahs-plugin-defalt-face nil
                              :foreground nil
                              :background nil)
          (set-face-attribute 'ahs-face nil
                              :foreground nil
                              :background "#073642")
          (set-face-attribute 'ahs-definition-face nil
                              :foreground nil
                              :background "#073642"
                              :underline '(:color "#268bd2")))

      (set-face-attribute 'ahs-plugin-defalt-face nil
                          :foreground nil
                          :background "grey20")
      (set-face-attribute 'ahs-face nil
                          :foreground nil
                          :background "grey20")
      (set-face-attribute 'ahs-definition-face nil
                          :foreground nil
                          :background "grey20"
                          :underline '(:color "#268bd2")))

    (set-face-attribute 'ahs-warning-face nil
                        :foreground nil
                        :underline '(:color "yellow"))
    (set-face-attribute 'ahs-plugin-bod-face nil
                        :foreground nil
                        :underline '(:color "purple"))
    (set-face-attribute 'ahs-plugin-whole-buffer-face nil
                        :foreground nil
                        :underline '(:color "black")))

  (add-hook 'load-theme-hook #'my/ahs-setup-faces))

(add-hook 'auto-highlight-symbol-mode-hook
          (lambda ()
            (diminish 'auto-highlight-symbol-mode)))

(unless (or (bound-and-true-p my/slow-device)
            (< (display-color-cells) 256))
  (require 'auto-highlight-symbol)
  (global-auto-highlight-symbol-mode +1))

(global-set-key (kbd "<remap> <just-one-space>") #'cycle-spacing)

(provide 'config-ui)
