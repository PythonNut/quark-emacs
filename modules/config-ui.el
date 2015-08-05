(eval-when-compile
  (with-demoted-errors
    (require 'cl-lib)
    (require 'key-chord)
    (require 'evil)
    (require 'diminish)
    (require 'evil-easymotion)
    (require 'config-modes)))

(require 'config-scroll)
(require 'config-line-numbers)
(require 'config-avy-easymotion)
(require 'config-undo)

(require 'config-smartparens)
(require 'config-rainbow-delimiters)

(show-paren-mode +1)

(defadvice show-paren-function
  (after show-matching-paren-offscreen activate preactivate compile)
  "If the matching paren is offscreen, show the matching line in the
        echo area. Has no effect if the character before point is not of
        the syntax class ')'."
  (interactive)
  (let* ((cb (char-before (point)))
          (matching-text (and cb
                           (char-equal (char-syntax cb) ?\) )
                           (blink-matching-open))))
    (when matching-text (message matching-text))))

(with-eval-after-load 'multiple-cursors
  (define-key mc/keymap (kbd "<return>") nil)
  (define-key mc/keymap (kbd "C-c <return>") 'multiple-cursors-mode))

(cl-macrolet
  ((autoload-multiple-cursors (func)
     `(autoload ,func "multiple-cursors")))

  (with-no-warnings
    (generate-calls-single autoload-multiple-cursors
      (
        #'mc/mark-lines
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
(global-set-key (kbd "<M-left>")  'windmove-left)
(global-set-key (kbd "<M-right>") 'windmove-right)
(global-set-key (kbd "<M-up>")    'windmove-up)
(global-set-key (kbd "<M-down>")  'windmove-down)

(defun framemove-onetime-setup ()
  (windmove-default-keybindings 'meta)
  (require 'framemove)
  (remove-hook 'before-make-frame-hook
    #'framemove-onetime-setup))

;; directional frame movement too
(add-hook 'emacs-startup-hook
  (lambda ()
    (add-hook 'before-make-frame-hook
      #'framemove-onetime-setup)))

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
(lexical-let ((iflipb-running-p))
  (with-eval-after-load 'iflipb
    (setq
      iflipb-ignore-buffers '("^ " "^*helm" "^*Compile" "^*Quail")
      iflipb-wrap-around 't)

    (defun iflipb-first-iflipb-buffer-switch-command ()
      "Determines whether this is the first invocation of
  iflipb-next-buffer or iflipb-previous-buffer this round."
      iflipb-running-p))

  (defun iflipb-smart-buffer ()
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

  (defun iflipb-next-buffer-smart ()
    "A `hydra' enabled next-buffer"
    (interactive)
    (let ((iflipb-running-p t))
      (call-interactively #'iflipb-next-buffer))
    (iflipb-smart-buffer))

  (defun iflipb-previous-buffer-smart ()
    "A `hydra' enabled previous-buffer"
    (interactive)
    (let ((iflipb-running-p t))
      (call-interactively #'iflipb-previous-buffer))
    (iflipb-smart-buffer)))

(global-set-key (kbd "<C-tab>") 'iflipb-next-buffer-smart)
(global-set-key (kbd "C-S-<iso-lefttab>") 'iflipb-previous-buffer-smart)

(global-set-key (kbd "C-c TAB") 'iflipb-next-buffer-smart)
(global-set-key (kbd "C-c <backtab>") 'iflipb-previous-buffer-smart)

;; also allow undo/redo on window configs
(add-hook 'window-configuration-change-hook #'winner-mode)

(defun vhl-onetime-setup ()
  (require 'volatile-highlights)
  (remove-hook 'first-change-hook #'vhl-onetime-setup))

(add-hook 'emacs-startup-hook
  (lambda ()
    (add-hook 'first-change-hook #'vhl-onetime-setup)))

(with-eval-after-load 'volatile-highlights
  (diminish #'volatile-highlights-mode)

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

  (volatile-highlights-mode +1))

(defun restart-emacs ()
  (interactive)
  ;; We need the new emacs to be spawned after all kill-emacs-hooks
  ;; have been processed and there is nothing interesting left
  (add-hook 'kill-emacs-hook
    (lambda ()
      (if (display-graphic-p)
        (call-process "sh" nil nil nil "-c" "emacs &")
        (suspend-emacs "(sleep 1; emacs -nw < `tty`) & fg; fg")))
    t)
  (kill-emacs))

(provide 'config-ui)
