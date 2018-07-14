;; -*- lexical-binding: t -*-
(eval-when-compile (require 'config-macros))

(require 'cl-lib)

(require 'config-avy-easymotion)
(require 'config-undo)

(require 'config-smartparens)
(require 'config-rainbow-delimiters)

(setq mouse-wheel-scroll-amount '(3 ((shift) . 1))
      mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse t
      smooth-scroll-margin 5
      scroll-step 1
      auto-window-vscroll nil
      scroll-conservatively 1000)

(defun isearch-exit-chord-worker ()
  "Exit out of isearch after a chord"
  (interactive)
  (isearch-delete-char)
  (isearch-exit))

(eval-when-compile
  (with-demoted-errors "Load error: %s"
    (require 'hydra)))

(defhydra my/smart-isearch-chord-hydra (:timeout
                                        key-chord-one-key-delay
                                        :pre
                                        (setq hydra-is-helpful nil)
                                        :post
                                        (setq hydra-is-helpful t))
  ("j" isearch-exit-chord-worker)
  ("k" isearch-exit-chord-worker))

(defun isearch-exit-chord ()
  (interactive)
  (isearch-printing-char)
  (my/smart-isearch-chord-hydra/body))

(define-key isearch-mode-map "j" #'isearch-exit-chord)
(define-key isearch-mode-map "k" #'isearch-exit-chord)

(use-package smooth-scrolling
  :config
  (smooth-scrolling-mode +1))

(use-package multiple-cursors
  :commands (mc/mark-lines
             mc/mark-next-lines
             mc/mark-previous-lines
             mc/unmark-next-like-this
             mc/unmark-previous-like-this
             mc/skip-to-previous-like-this
             mc/mark-all-like-this
             mc/mark-all-words-like-this
             mc/mark-all-symbols-like-this
             mc/mark-all-in-region
             mc/mark-all-in-region-regexp
             mc/mark-more-like-this-extended
             mc/mmlte--up
             mc/mmlte--down
             mc/mmlte--left
             mc/mmlte--right
             mc/mark-all-like-this-dwim
             mc/mark-all-dwim
             mc/mark-all-like-this-in-defun
             mc/mark-all-words-like-this-in-defun
             mc/mark-all-symbols-like-this-in-defun
             mc/add-cursor-on-click
             mc/mark-sgml-tag-pair
             mc/mark-pop
             set-rectangular-region-anchor
             rrm/switch-to-multiple-cursors
             mc/insert-numbers
             mc/reverse-regions
             mc/sort-regions
             hum/keyboard-quit
             mc-hide-unmatched-lines-mode)
  :config
  (setq mc/list-file (locate-user-emacs-file "data/.mc-lists.el"))

  ;; This is required to load the save file, due to a poor design
  ;; decision in multiple-cursors.el
  (load mc/list-file t)

  (define-key mc/keymap (kbd "<return>") nil)
  (define-key mc/keymap (kbd "C-c <return>") 'multiple-cursors-mode))

(global-set-key (kbd "C-c l") #'mc/edit-lines)
(global-set-key (kbd "C-c a") #'mc/mark-all-dwim)
(global-set-key (kbd "C->") #'mc/mark-next-like-this)
(global-set-key (kbd "C-<") #'mc/mark-previous-like-this)
(global-set-key (kbd "C-?") #'mc/mark-all-dwim)
(global-set-key (kbd "C-c >") #'mc/mark-next-like-this)
(global-set-key (kbd "C-c <") #'mc/mark-previous-like-this)
(global-set-key (kbd "C-c ?") #'mc/mark-all-dwim)

;; directional window movement
(if (display-graphic-p)
    (progn
      (global-set-key (kbd "<M-left>")  #'windmove-left)
      (global-set-key (kbd "<M-right>") #'windmove-right)
      (global-set-key (kbd "<M-up>")    #'windmove-up)
      (global-set-key (kbd "<M-down>")  #'windmove-down))

  (global-set-key (kbd "<C-left>")  #'windmove-left)
  (global-set-key (kbd "<C-right>") #'windmove-right)
  (global-set-key (kbd "<C-up>")    #'windmove-up)
  (global-set-key (kbd "<C-down>")  #'windmove-down))

(use-package framemove
  :init
  (my/onetime-setup framemove
    :hook 'before-make-frame-hook
    :after-hook 'emacs-startup-hook
    (require 'framemove))

  :config
  (setq framemove-hook-into-windmove t))

(defun pop-window-into-frame ()
  (interactive)
  (let ((buffer (current-buffer)))
    (unless (one-window-p)
      (delete-window))
    (display-buffer-pop-up-frame buffer nil)))

(defhydra my/window-resize-hydra ()
  ("{" (call-interactively #'shrink-window-horizontally) "shrink ↔")
  ("}" (call-interactively #'enlarge-window-horizontally) "enlarge ↔")
  ("^" (call-interactively #'enlarge-window) "enlarge ↕")
  ("-" (call-interactively #'shrink-window-if-larger-than-buffer) "shrink ↕"))

(global-set-key (kbd "C-x {") #'my/window-resize-hydra/lambda-{)
(global-set-key (kbd "C-x }") #'my/window-resize-hydra/lambda-})
(global-set-key (kbd "C-x ^") #'my/window-resize-hydra/lambda-^)
(global-set-key (kbd "C-x -") #'my/window-resize-hydra/lambda--)

;;; ====================================
;;; iflib - switch buffers alt-tab style
;;; ====================================
(use-package iflipb
  :init
  (defhydra iflipb-hydra (:pre
                          (setq hydra-is-helpful nil)
                          :post
                          (setq hydra-is-helpful t))
    ("<C-tab>"
     (call-interactively #'iflipb-next-buffer))
    ("TAB"
     (call-interactively #'iflipb-next-buffer))
    ("<C-S-iso-lefttab>"
     (call-interactively #'iflipb-previous-buffer))
    ("<backtab>"
     (call-interactively #'iflipb-previous-buffer))
    ("<C-S-tab>"
     (call-interactively #'iflipb-previous-buffer)))

  (defun iflipb-next-buffer-smart ()
    "A `hydra' enabled next-buffer"
    (interactive)
    (require 'iflipb)
    (cl-letf (((symbol-function #'iflipb-first-iflipb-buffer-switch-command)
               (lambda () t)))
      (call-interactively #'iflipb-next-buffer))
    (iflipb-hydra/body))

  (defun iflipb-previous-buffer-smart ()
    "A `hydra' enabled previous-buffer"
    (interactive)
    (require 'iflipb)
    (cl-letf (((symbol-function #'iflipb-first-iflipb-buffer-switch-command)
               (lambda () t)))
      (call-interactively #'iflipb-previous-buffer))
    (iflipb-hydra/body))

  (global-set-key (kbd "<C-tab>") #'iflipb-next-buffer-smart)
  (global-set-key (kbd "C-S-<iso-lefttab>") #'iflipb-previous-buffer-smart)
  (global-set-key (kbd "<C-S-tab>") #'iflipb-previous-buffer-smart)

  (global-set-key (kbd "C-c TAB") #'iflipb-next-buffer-smart)
  (global-set-key (kbd "C-c <backtab>") #'iflipb-previous-buffer-smart)
  (global-set-key (kbd "C-c <C-S-tab>") #'iflipb-previous-buffer-smart)

  :config
  (setq iflipb-ignore-buffers (list (rx line-start " ")
                                    (rx line-start "*helm")
                                    (rx line-start "*Compile")
                                    (rx line-start "*Quail")
                                    (rx line-start "magit-process")
                                    (rx line-start "magit-diff")
                                    (rx line-start "*tramp")
                                    (rx line-start "*anaconda")
                                    (rx line-start "*" (zero-or-more anything) "output*")
                                    (rx line-start "*straight-process*"))
        iflipb-wrap-around t)

  (advice-add
   'iflipb-first-iflipb-buffer-switch-command :override
   (my/defun-as-value nadvice/iflipb-first-iflipb-buffer-switch-command (&rest _args) nil)))

;; also allow undo/redo on window configs
(add-hook 'window-configuration-change-hook #'winner-mode)

(use-package volatile-highlights
  :ensure t
  :init
  (my/onetime-setup vhl
    :hook 'first-change-hook
    :after-hook 'emacs-startup-hook
    (volatile-highlights-mode +1))

  :config
  (eval-when-compile
    (with-demoted-errors "Load error: %s"
      (require 'volatile-highlights)))

  (diminish #'volatile-highlights-mode)
  (vhl/define-extension 'my-evil-highlights
                        'evil-yank
                        'evil-paste-after
                        'evil-paste-before
                        'evil-move)

  (vhl/install-extension 'my-evil-highlights)

  (vhl/define-extension 'my-undo-tree-highlights
                        'undo-tree-undo
                        'undo-tree-redo)

  (vhl/install-extension 'my-undo-tree-highlights)

  (volatile-highlights-mode +1))

;; ==================
;; Visible whitespace
;; ==================

(with-eval-after-load 'whitespace
  (setq whitespace-display-mappings '((space-mark 32 [?·]))
        whitespace-style '(face trailing spaces space-mark))

  (set-face-attribute 'whitespace-space nil
                      :inherit nil
                      :foreground (face-background 'default)
                      :background nil)

  (set-face-attribute 'whitespace-trailing nil
                      :inherit 'avy-background-face
                      :background nil
                      :inverse-video nil
                      :foreground nil)

  (advice-add
   'whitespace-trailing-regexp :override
   (my/defun-as-value nadvice/whitespace-trailing-regexp (limit)
     "Match all trailing spaces. This overloads the definition in whitespace.el."
     (let ((status t))
       (while (unless (re-search-forward whitespace-trailing-regexp limit t)
                (setq status nil)))          ;; end of buffer
       status))))

;; ============
;; Line numbers
;; ============

;; column numbers too
(column-number-mode +1)

(defun linum-cycle ()
  (interactive)
  (cond ((not display-line-numbers)
         (setq display-line-numbers 'relative))
        ((equal display-line-numbers 'relative)
         (setq display-line-numbers t))
        ((equal display-line-numbers t)
         (setq display-line-numbers nil))))

(global-set-key (kbd "C-c L") #'linum-cycle)
(global-set-key (kbd "C-c C-l") #'linum-cycle)

(use-package which-key
  :init (which-key-mode +1)
  :diminish which-key-mode
  :config
  (setq which-key-sort-order nil
        which-key-side-window-max-height 0.33)

  (add-to-list 'which-key-description-replacement-alist
               `(,(rx "evil-"
                      (or "a" "an" "inner")
                      "-"
                      (group (zero-or-more not-newline)))
                 . "\\1")))

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

(defun split-window-below-cycle ()
  (interactive)
  (with-selected-window (split-window-below)
    (cl-letf (((symbol-function #'iflipb-first-iflipb-buffer-switch-command)
               (lambda () t)))
      (call-interactively #'iflipb-next-buffer))))

(defun split-window-right-cycle ()
  (interactive)
  (with-selected-window (split-window-right)
    (cl-letf (((symbol-function #'iflipb-first-iflipb-buffer-switch-command)
               (lambda () t)))
      (call-interactively #'iflipb-next-buffer))))

(global-set-key (kbd "C-0") #'delete-window)
(global-set-key (kbd "C-1") #'delete-other-windows)
(global-set-key (kbd "C-2") #'split-window-below-cycle)
(global-set-key (kbd "C-3") #'split-window-right-cycle)
(global-set-key (kbd "C-4") #'find-file-other-window)
(global-set-key (kbd "C-5") #'make-frame-command)
(global-set-key (kbd "M-j") #'evil-join)
(global-set-key (kbd "C-.") #'er/expand-region)

(define-key key-translation-map (kbd "<mouse-21>") (kbd "<C-mouse-5>"))
(define-key key-translation-map (kbd "<mouse-20>") (kbd "<C-mouse-4>"))
(global-set-key (kbd "<C-mouse-5>") #'evil-scroll-page-down)
(global-set-key (kbd "<C-mouse-4>") #'evil-scroll-page-up)

(use-package subword
  :ensure t
  :diminish subword-mode
  :init (global-subword-mode +1))

;; ==========================================
;; Window splitting that is actually sensible
;; ==========================================
(eval-when-compile (require 'el-patch))
(el-patch-defun split-window-sensibly (&optional window)
  "Split WINDOW in a way suitable for `display-buffer'.
WINDOW defaults to the currently selected window.
If `split-height-threshold' specifies an integer, WINDOW is at
least `split-height-threshold' lines tall and can be split
vertically, split WINDOW into two windows one above the other and
return the lower window.  Otherwise, if `split-width-threshold'
specifies an integer, WINDOW is at least `split-width-threshold'
columns wide and can be split horizontally, split WINDOW into two
windows side by side and return the window on the right.  If this
can't be done either and WINDOW is the only window on its frame,
try to split WINDOW vertically disregarding any value specified
by `split-height-threshold'.  If that succeeds, return the lower
window.  Return nil otherwise.

By default `display-buffer' routines call this function to split
the largest or least recently used window.  To change the default
customize the option `split-window-preferred-function'.

You can enforce this function to not split WINDOW horizontally,
by setting (or binding) the variable `split-width-threshold' to
nil.  If, in addition, you set `split-height-threshold' to zero,
chances increase that this function does split WINDOW vertically.

In order to not split WINDOW vertically, set (or bind) the
variable `split-height-threshold' to nil.  Additionally, you can
set `split-width-threshold' to zero to make a horizontal split
more likely to occur.

Have a look at the function `window-splittable-p' if you want to
know how `split-window-sensibly' determines whether WINDOW can be
split."
  (let ((window (or window (selected-window))))
    (el-patch-let (($vertical
                    (and (window-splittable-p window)
                         ;; Split window vertically.
                         (with-selected-window window
                           (split-window-below))))
                   ($horizontal
                    (and (window-splittable-p window t)
                         ;; Split window horizontally.
                         (with-selected-window window
                           (split-window-right)))))
      (or (el-patch-swap $vertical $horizontal)
          (el-patch-swap $horizontal $vertical)
          (and
           ;; If WINDOW is the only usable window on its frame (it is
           ;; the only one or, not being the only one, all the other
           ;; ones are dedicated) and is not the minibuffer window, try
           ;; to split it vertically disregarding the value of
           ;; `split-height-threshold'.
           (let ((frame (window-frame window)))
             (or
              (eq window (frame-root-window frame))
              (catch 'done
                (walk-window-tree (lambda (w)
                                    (unless (or (eq w window)
                                                (window-dedicated-p w))
                                      (throw 'done nil)))
                                  frame)
                t)))
	   (not (window-minibuffer-p window))
	   (let ((split-height-threshold 0))
	     (when (window-splittable-p window)
	       (with-selected-window window
	         (split-window-below)))))))))

;; =========================================================
;; digit-groups - make large unbroken numbers easier to read
;; =========================================================

(use-package digit-groups
  :defer-install t
  :commands (digit-groups-global-mode
             digit-groups-mode))

(provide 'config-ui)
