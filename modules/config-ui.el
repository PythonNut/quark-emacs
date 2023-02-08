;; -*- lexical-binding: t -*-
(eval-when-compile
  (require 'config-macros)
  (require 'config-package))

(require 'cl-lib)

(my/require-config-module 'config-avy-easymotion)
(my/require-config-module 'config-undo)

(my/require-config-module 'config-smartparens)

(setq mouse-wheel-scroll-amount '(3 ((shift) . 1))
      mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse t
      scroll-step 1
      auto-window-vscroll nil
      scroll-conservatively 1000
      fast-but-imprecise-scrolling t
      frame-inhibit-implied-resize t
      inhibit-compacting-font-caches t)

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

(use-package evil-mc
  :commands (evil-mc-make-all-cursors
             evil-mc-undo-last-added-cursor
             evil-mc-undo-all-cursors
             evil-mc-pause-cursors
             evil-mc-resume-cursors
             evil-mc-make-and-goto-first-cursor
             evil-mc-make-and-goto-last-cursor
             evil-mc-make-cursor-here
             evil-mc-make-cursor-move-next-line
             evil-mc-make-cursor-move-prev-line
             evil-mc-skip-and-goto-next-cursor
             evil-mc-skip-and-goto-prev-cursor
             evil-mc-skip-and-goto-next-match
             evil-mc-skip-and-goto-prev-match
             evil-mc-make-cursor-in-visual-selection-beg
             evil-mc-make-cursor-in-visual-selection-end)
  :init
  (el-patch-feature evil-mc)

  (el-patch-defvar evil-mc-cursors-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "m") #'evil-mc-make-all-cursors)
      (define-key map (kbd "u") #'evil-mc-undo-last-added-cursor)
      (define-key map (kbd "q") #'evil-mc-undo-all-cursors)
      (define-key map (kbd "s") #'evil-mc-pause-cursors)
      (define-key map (kbd "r") #'evil-mc-resume-cursors)
      (define-key map (kbd "f") #'evil-mc-make-and-goto-first-cursor)
      (define-key map (kbd "l") #'evil-mc-make-and-goto-last-cursor)
      (define-key map (kbd "h") #'evil-mc-make-cursor-here)
      (define-key map (kbd "j") #'evil-mc-make-cursor-move-next-line)
      (define-key map (kbd "k") #'evil-mc-make-cursor-move-prev-line)
      (define-key map (kbd "N") #'evil-mc-skip-and-goto-next-cursor)
      (define-key map (kbd "P") #'evil-mc-skip-and-goto-prev-cursor)
      (define-key map (kbd "n") #'evil-mc-skip-and-goto-next-match)
      (define-key map (kbd "p") #'evil-mc-skip-and-goto-prev-match)
      (define-key map (kbd "I") #'evil-mc-make-cursor-in-visual-selection-beg)
      (define-key map (kbd "A") #'evil-mc-make-cursor-in-visual-selection-end)
      map))

  (el-patch-defvar evil-mc-key-map
    (let ((map (make-sparse-keymap)))
      (evil-define-key* '(normal visual) map
        (kbd "gr") evil-mc-cursors-map
        (el-patch-remove
          (kbd "M-n") 'evil-mc-make-and-goto-next-cursor
          (kbd "M-p") 'evil-mc-make-and-goto-prev-cursor
          (kbd "C-n") 'evil-mc-make-and-goto-next-match
          (kbd "C-t") 'evil-mc-skip-and-goto-next-match
          (kbd "C-p") 'evil-mc-make-and-goto-prev-match))
      map))

  (global-set-key (kbd "C->") #'evil-mc-make-cursor-move-next-line)
  (global-set-key (kbd "C-<") #'evil-mc-make-cursor-move-prev-line)
  (define-key evil-normal-state-map (kbd "gr") evil-mc-cursors-map)

  :config
  (global-evil-mc-mode +1)

  (setq evil-mc-custom-known-commands
        '((evil-delete-backward-word-smart
           .
           ((:default . evil-mc-execute-default-call)))
          (end-of-visual-line-or-end
           .
           ((:default . evil-mc-execute-default-call)))
          (back-to-indentation-visual-or-beginning
           .
           ((:default . evil-mc-execute-default-call))))
        evil-mc-mode-line
        `(:eval
          (if (> (length evil-mc-cursor-list) 0)
              (evil-mc-active-mode-line (concat " " evil-mc-mode-line-prefix))
            "")))

  (el-patch-defun evil-mc-make-cursor-move-by-line (dir count)
    "Create COUNT cursors one for each line moving in the direction DIR.
DIR should be 1 or -1 and COUNT should be a positive integer or nil."
    (evil-force-normal-state)
    (setq count (max 0 (or count 1)))
    (dotimes (i count)
      (evil-mc-run-cursors-before)
      (evil-mc-make-cursor-at-pos (point))
      (let ((line-move-visual (el-patch-swap t nil)))
        (evil-line-move dir)))))

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

;;; ===================================================
;;; Rainbow-delimiters - Easily see paren nesting level
;;; ===================================================g

(use-package rainbow-delimiters
  :init
  ;; the equivalent of a global mode, but does not
  ;; turn on for odd non-programming modes
  (defun my/maybe-enable-rainbow-delimiters ()
    (when (display-graphic-p)
      (rainbow-delimiters-mode +1)))

  (unless (bound-and-true-p my/slow-device)
    (add-hook 'prog-mode-hook #'my/maybe-enable-rainbow-delimiters)
    (add-hook 'text-mode-hook #'my/maybe-enable-rainbow-delimiters))

  :config
  (defvar-local my/rainbow-delimiters-switch nil
    "t if rainbow-delimiters are currently punched")
  (defvar-local my/rainbow-delimiters-face-cookies nil
    "a list of face-remap-add-relative cookies to reset")

  (defun my/rainbow-delimiters-setup-faces ()
    (set-face-foreground 'rainbow-delimiters-depth-1-face "#889899")
    (set-face-foreground 'rainbow-delimiters-depth-2-face "#9b7b6b")
    (set-face-foreground 'rainbow-delimiters-depth-3-face "#7b88a5")
    (set-face-foreground 'rainbow-delimiters-depth-4-face "#889899")
    (set-face-foreground 'rainbow-delimiters-depth-5-face "#839564")
    (set-face-foreground 'rainbow-delimiters-depth-6-face "#6391aa")
    (set-face-foreground 'rainbow-delimiters-depth-7-face "#9d748f")
    (set-face-foreground 'rainbow-delimiters-depth-8-face "#7b88a5")
    (set-face-foreground 'rainbow-delimiters-depth-9-face "#659896"))

  (my/rainbow-delimiters-setup-faces)
  (add-hook 'load-theme-hook #'my/rainbow-delimiters-setup-faces)

  (defun my/rainbow-delimiters-focus-on ()
    "Punch the rainbow-delimiters"
    (setq my/rainbow-delimiters-face-cookies
          (list
           (face-remap-add-relative 'rainbow-delimiters-depth-1-face
                                    '((:foreground "#3B9399") rainbow-delimiters-depth-1-face))
           (face-remap-add-relative 'rainbow-delimiters-depth-2-face
                                    '((:foreground "#9B471D") rainbow-delimiters-depth-2-face))
           (face-remap-add-relative 'rainbow-delimiters-depth-3-face
                                    '((:foreground "#284FA5") rainbow-delimiters-depth-3-face))
           (face-remap-add-relative 'rainbow-delimiters-depth-4-face
                                    '((:foreground "#3B9399") rainbow-delimiters-depth-4-face))
           (face-remap-add-relative 'rainbow-delimiters-depth-5-face
                                    '((:foreground "#679519") rainbow-delimiters-depth-5-face))
           (face-remap-add-relative 'rainbow-delimiters-depth-6-face
                                    '((:foreground "#0E73AA") rainbow-delimiters-depth-6-face))
           (face-remap-add-relative 'rainbow-delimiters-depth-7-face
                                    '((:foreground "#9D2574") rainbow-delimiters-depth-7-face))
           (face-remap-add-relative 'rainbow-delimiters-depth-8-face
                                    '((:foreground "#284FA5") rainbow-delimiters-depth-8-face))
           (face-remap-add-relative 'rainbow-delimiters-depth-9-face
                                    '((:foreground "#199893") rainbow-delimiters-depth-9-face)))
          my/rainbow-delimiters-switch t))

  (defun my/rainbow-delimiters-focus-off ()
    "Reset the rainbow-delimiters faces"
    (mapc #'face-remap-remove-relative my/rainbow-delimiters-face-cookies)
    (setq my/rainbow-delimiters-switch nil))

  (defun my/rainbow-delimiters-focus-toggle-maybe ()
    "Punch the rainbow-delimiters if the point is on a paren"
    (if (or (looking-at (rx (any "[](){}")))
            (and
             (evil-insert-state-p)
             (looking-back (rx (any "[](){}")) (1- (point)))))
        (unless (or my/rainbow-delimiters-switch (minibufferp))
          (my/rainbow-delimiters-focus-on))
      (when my/rainbow-delimiters-switch
        (my/rainbow-delimiters-focus-off))))

  (run-with-idle-timer 0.1 t 'my/rainbow-delimiters-focus-toggle-maybe))

;;; ====================================
;;; iflib - switch buffers alt-tab style
;;; ====================================
(use-package iflipb
  :init
  (global-set-key (kbd "<C-tab>") #'iflipb-next-buffer)
  (global-set-key (kbd "C-S-<iso-lefttab>") #'iflipb-previous-buffer)
  (global-set-key (kbd "<C-S-tab>") #'iflipb-previous-buffer)

  (global-set-key (kbd "C-c TAB") #'iflipb-next-buffer)
  (global-set-key (kbd "C-c <backtab>") #'iflipb-previous-buffer)
  (global-set-key (kbd "C-c <C-S-tab>") #'iflipb-previous-buffer)

  (global-set-key (kbd "<mouse-8>") #'iflipb-previous-buffer)
  (global-set-key (kbd "<mouse-9>") #'iflipb-next-buffer)

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
        iflipb-wrap-around t))

;; also allow undo/redo on window configs
(add-hook 'window-configuration-change-hook #'winner-mode)

(use-package volatile-highlights
  :ensure t
  :init
  (my/onetime-setup vhl
    :hook 'first-change-hook
    :after-hook 'emacs-startup-hook
    :condition (get-buffer-window)
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

  (vhl/install-extension 'my-undo-tree-highlights))

;; ==================
;; Visible whitespace
;; ==================

(with-eval-after-load 'whitespace
  (setf (alist-get 'space-mark whitespace-display-mappings) '(32 [?·]))
  (setq whitespace-style '(face trailing tab tab-mark spaces space-mark))

  (define-advice whitespace-trailing-regexp
      (:override (limit) match-all)
    "Match all trailing spaces. This overloads the definition in whitespace.el."
    (let ((status t))
      (while (unless (re-search-forward whitespace-trailing-regexp limit t)
               (setq status nil))) ;; end of buffer
      status)))

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

  (add-to-list 'which-key-replacement-alist
               `((nil . ,(rx "evil-"
                             (or "a" "an" "inner")
                             "-"
                             (group (zero-or-more not-newline))))
                 . (nil . "\\1"))))

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
                                  frame nil 'nomini)
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
