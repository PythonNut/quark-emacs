(eval-and-compile
  (add-to-list 'load-path (locate-user-emacs-file "modules/")))

(require 'config-setq)

;; first subtractive
(menu-bar-mode -1)

(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(blink-cursor-mode -1)

;; then additive
(column-number-mode +1)
(xterm-mouse-mode +1)
(icomplete-mode +1)
(global-visual-line-mode +1)

;; enable semantic code LALR(1) parser
(add-hook 'prog-mode-hook #'semantic-mode)
(with-eval-after-load 'semantic
  (global-semanticdb-minor-mode +1)
  (global-semantic-idle-scheduler-mode +1)
  (global-semantic-idle-summary-mode +1))

(add-hook 'find-file-hook #'auto-save-mode)
(add-hook 'find-file-hook #'global-auto-revert-mode)
(add-hook 'find-file-hook #'flymake-find-file-hook)
(add-hook 'find-file-hook (lambda () (prefer-coding-system 'utf-8)))
(add-hook 'before-save-hook (lambda () (setq buffer-backed-up nil)))
(add-hook 'emacs-startup-hook (lambda () (message "")))

(defun generic-term-init ()
  (face-remap-add-relative 'default :background "#111")
  (visual-line-mode -1)
  (setq-local global-hl-line-mode nil)
  (setq-local scroll-margin 0))

(add-hook 'term-mode-hook #'generic-term-init)
(add-hook 'shell-mode-hook #'generic-term-init)
(add-hook 'eshell-mode-hook #'generic-term-init)

(global-hl-line-mode +1)

;; interpreted as C-<Arrow> in a terminal
(global-set-key (kbd "M-[ d") (kbd "<C-left>"))
(global-set-key (kbd "M-[ c") (kbd "<C-right>"))
(global-set-key (kbd "M-[ a") (kbd "<C-up>"))
(global-set-key (kbd "M-[ b") (kbd "<C-down"))

;; and parsed normally in GUI
(global-set-key (kbd "C-<left>") #'windmove-left)
(global-set-key (kbd "C-<right>") #'windmove-right)
(global-set-key (kbd "C-<up>") #'windmove-up)
(global-set-key (kbd "C-<down>") #'windmove-down)

(setq term-default-bg-color "#111"
      
      ;; make scrolling sane
      scroll-margin 5
      scroll-conservatively 1000
      scroll-step 1
      mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse t
      mouse-wheel-scroll-amount '(3 ((shift) . 1))
      
      ;; backup locations
      backup-directory-alist
      `((".*" . ,(locate-user-emacs-file "data/backups")))
      auto-save-file-name-transforms
      `((".*" ,(locate-user-emacs-file "data/autosave") t))

      ;; auto-save parameters
      version-control t 
      kept-new-versions 30 
      kept-old-versions 0
      delete-old-versions t 
      backup-by-copying t
      backup-by-copying-when-linked t
      auto-save-default t   
      auto-save-timeout 10  
      auto-save-interval 200
      vc-make-backup-files t

      ;; hippie expand settings
      hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-dabbrev-from-kill
                                         try-expand-dabbrev-all-buffers
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-list
                                         try-expand-line
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))

(global-set-key (kbd "M-/") #'hippie-expand)

(setq-default buffer-file-coding-system 'utf-8
              indent-tabs-mode nil)

(defalias 'yes-or-no-p #'y-or-n-p)

;; make vertical window borders prettier
(let ((display-table (or standard-display-table (make-display-table))))
  (set-display-table-slot display-table 'vertical-border (make-glyph-code ?│))
  (setq standard-display-table display-table))

(set-face-attribute 'vertical-border nil
                    :inherit 'default)

(load-theme 'tango-dark)

(set-face-background 'highlight "grey20")

(set-face-attribute 'hl-line nil
                    :foreground nil
                    :background nil
                    :inherit 'fringe)

(set-face-attribute 'mode-line nil :box nil)
(set-face-attribute 'mode-line-inactive nil :box nil)
(set-face-foreground 'highlight nil)

(add-hook 'window-configuration-change-hook #'winner-mode)

(global-set-key (kbd "<remap> <just-one-space>") #'cycle-spacing)
(global-set-key (kbd "<remap> <delete-horizontal-space>") #'cycle-spacing)
(global-set-key (kbd "C-0") #'delete-window)
(global-set-key (kbd "C-1") #'delete-other-windows)
(global-set-key (kbd "C-2") #'split-window-below)
(global-set-key (kbd "C-3") #'split-window-right)
(global-set-key (kbd "C-4") #'find-file-other-window)
(global-set-key (kbd "C-5") #'make-frame-command)


