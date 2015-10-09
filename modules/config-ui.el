;; -*- lexical-binding: t -*-

(eval-when-compile
  (with-demoted-errors "Load error: %s"
    (require 'cl-lib)
    (require 'hydra)
    (require 'key-chord)
    (require 'evil)
    (require 'diminish)
    (require 'evil-easymotion)
    (require 'volatile-highlights)
    (require 'linum-relative)
    (require 'config-setq)
    (require 'config-package)
    (require 'which-key)
    (require 'framemove)))

(setq mouse-wheel-scroll-amount '(3 ((shift) . 1))
      mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse t
      smooth-scroll-margin 5
      scroll-step 1
      auto-window-vscroll nil
      scroll-conservatively 1000)

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
     'autoload-multiple-cursors
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

(defun my/framemove-onetime-setup (&optional frame)
  (with-selected-frame (or frame (selected-frame))
    (windmove-default-keybindings 'meta)
    (require 'framemove)
    (remove-hook 'before-make-frame-hook #'my/framemove-onetime-setup)))

;; directional frame movement too
(add-hook 'emacs-startup-hook
          (lambda ()
            (add-hook 'before-make-frame-hook #'my/framemove-onetime-setup)))

(with-eval-after-load 'framemove
  (setq framemove-hook-into-windmove t))

(defun pop-window-into-frame ()
  (interactive)
  (let ((buffer (current-buffer)))
    (unless (one-window-p)
      (delete-window))
    (display-buffer-pop-up-frame buffer nil)))

;;; ====================================
;;; iflib - switch buffers alt-tab style
;;; ====================================
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

    (iflipb-hydra/body)

    (defun nadvice/iflipb-first-iflipb-buffer-switch-command ()
      nil)

    (advice-add #'iflipb-first-iflipb-buffer-switch-command
                :override
                #'nadvice/iflipb-first-iflipb-buffer-switch-command)))

(defun iflipb-next-buffer-smart ()
  "A `hydra' enabled next-buffer"
  (interactive)
  (require 'iflipb)
  (cl-letf (((symbol-function 'iflipb-first-iflipb-buffer-switch-command)
             (lambda () t)))
    (call-interactively #'iflipb-next-buffer))
  (my/iflipb-smart-buffer))

(defun iflipb-previous-buffer-smart ()
  "A `hydra' enabled previous-buffer"
  (interactive)
  (require 'iflipb)
  (cl-letf (((symbol-function 'iflipb-first-iflipb-buffer-switch-command)
             (lambda () t)))
    (call-interactively #'iflipb-previous-buffer))
  (my/iflipb-smart-buffer))

(global-set-key (kbd "<C-tab>") #'iflipb-next-buffer-smart)
(global-set-key (kbd "C-S-<iso-lefttab>") #'iflipb-previous-buffer-smart)

(global-set-key (kbd "C-c TAB") #'iflipb-next-buffer-smart)
(global-set-key (kbd "C-c <backtab>") #'iflipb-previous-buffer-smart)

;; also allow undo/redo on window configs
(add-hook 'window-configuration-change-hook #'winner-mode)

(with-eval-after-load 'volatile-highlights
  (diminish #'volatile-highlights-mode)
  (vhl/define-extension 'my-evil-highlights
                        'evil-yank
                        'evil-paste-after
                        'evil-paste-before)

  (vhl/install-extension 'my-evil-highlights)

  (vhl/define-extension 'my-undo-tree-highlights
                        'undo-tree-undo
                        'undo-tree-redo)

  (vhl/install-extension 'my-undo-tree-highlights)

  (volatile-highlights-mode +1))

(defun my/vhl-onetime-setup ()
  (require 'volatile-highlights)
  (remove-hook 'first-change-hook #'my/vhl-onetime-setup))

(add-hook 'emacs-startup-hook
          (lambda ()
            (add-hook 'first-change-hook #'my/vhl-onetime-setup)))

(with-eval-after-load 'auto-highlight-symbol
  (setq ahs-idle-interval 0.3)
  (add-to-list 'ahs-inhibit-face-list 'font-lock-keyword-face)
  (add-to-list 'ahs-inhibit-face-list 'region)
  (add-to-list 'ahs-inhibit-face-list 'isearch)

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

(evil-define-command evil-cycle-spacing (&optional count)
  (cycle-spacing (or count 1)))

(global-set-key (kbd "<remap> <just-one-space>") #'evil-cycle-spacing)
(global-set-key (kbd "<remap> <delete-horizontal-space>") #'evil-cycle-spacing)

;; ============
;; Line numbers
;; ============

(with-eval-after-load 'linum
  (unless (package-installed-p 'linum-relative)
    (save-window-excursion
      (package-install 'linum-relative)))

  (require 'linum-relative)

  (setq linum-relative-current-symbol ""
        linum-relative-format "%3s "
        linum-delay t)

  (set-face-background 'linum nil)
  (set-face-attribute 'linum-relative-current-face nil
                      :weight 'extra-bold
                      :foreground nil
                      :background nil
                      :inherit '(hl-line default))

  ;; truncate current line to three digits
  (defun nadvice/linum-relative (line-number)
    (let* ((diff1 (abs (- line-number linum-relative-last-pos)))
           (diff (if (minusp diff1)
                     diff1
                   (+ diff1 linum-relative-plusp-offset)))
           (current-p (= diff linum-relative-plusp-offset))
           (current-symbol (if (and linum-relative-current-symbol current-p)
                               (if (string= "" linum-relative-current-symbol)
                                   (number-to-string (% line-number 1000))
                                 linum-relative-current-symbol)
                             (number-to-string diff)))
           (face (if current-p 'linum-relative-current-face 'linum)))
      (propertize (format linum-relative-format current-symbol) 'face face)))

  (advice-add 'linum-relative :override #'nadvice/linum-relative))

(defun linum-cycle ()
  (interactive)
  (if (bound-and-true-p linum-mode)
      (if (eq linum-format 'dynamic)
          (linum-mode -1)
        (setq linum-format 'dynamic))
    (progn
      (linum-mode +1)
      (setq linum-format 'linum-relative))))

(global-set-key (kbd "C-c L") #'linum-cycle)
(global-set-key (kbd "C-c C-l") #'linum-cycle)

(with-eval-after-load 'which-key
  (diminish 'which-key-mode)
  (which-key-mode +1)
  (setq which-key-sort-order nil
        which-key-prevent-C-h-from-cycling nil
        which-key-side-window-max-height 0.33)

  (add-to-list 'which-key-description-replacement-alist
               `(,(eval-when-compile
                    (concat
                     "evil-"
                     (regexp-opt (list "a"
                                       "an"
                                       "inner"))
                     "-\\(.*\\)")) . "\\1")))

(global-set-key (kbd "C-0") #'delete-window)
(global-set-key (kbd "C-1") #'delete-other-windows)
(global-set-key (kbd "C-2") #'split-window-below)
(global-set-key (kbd "C-3") #'split-window-right)
(global-set-key (kbd "C-4") #'find-file-other-window)
(global-set-key (kbd "C-5") #'make-frame-command)
(global-set-key (kbd "M-j") #'evil-join)
(global-set-key (kbd "C-.") #'er/expand-region)
(global-set-key (kbd "<C-mouse-5>") #'evil-scroll-page-down)
(global-set-key (kbd "<C-mouse-4>") #'evil-scroll-page-up)

(defun isearch-delete-something ()
  "Delete non-matching text or the last character."
  ;; An attempt at this Emacs SX question:
  ;; https://emacs.stackexchange.com/questions/10359/delete-portion-of-isearch-string-that-does-not-match-or-last-char-if-complete-m
  ;; Mostly copied from `isearch-del-char' and Drew's answer on the page above
  (interactive)
  (if (= 0 (length isearch-string))
      (ding)
    (setq isearch-string
          (substring isearch-string
                     0
                     (or (isearch-fail-pos) (1- (length isearch-string)))))
    (setq isearch-message
          (mapconcat #'isearch-text-char-description isearch-string "")))
  (if isearch-other-end (goto-char isearch-other-end))
  (isearch-search)
  (isearch-push-state)
  (isearch-update))

(define-key isearch-mode-map (kbd "<backspace>") #'isearch-delete-something)

;; add intelligent buffer renaming
(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(global-set-key (kbd "C-x C-r") #'rename-current-buffer-file)

(provide 'config-ui)
