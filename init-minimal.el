;; first subtractive
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)

;; then additive
(column-number-mode +1)
(delete-selection-mode +1)
(icomplete-mode +1)
(show-paren-mode +1)
(xterm-mouse-mode +1)
(visual-line-mode +1)
(auto-save-mode +1)
(global-auto-revert-mode +1)

(set-face-attribute 'show-paren-match nil
  :background nil
  :weight 'extra-bold)

(setf
  delete-by-moving-to-trash t
  echo-keystrokes 0.4
  
  ;; switch windows without clicking
  focus-follows-mouse 1
  mouse-autoselect-window 0.3
  
  ;; indentation settings
  indent-tabs-mode nil
  tab-width 4
  lisp-indent-offset 2
  lisp-body-indent 2
  use-dialog-box nil
  
  ;; supress useless stuff
  inhibit-default-init 1
  inhibit-startup-screen t

  ;; syntax highlighting settings
  jit-lock-defer-time 0.04
  jit-lock-stealth-nice 0.1
  jit-lock-stealth-time 0.2
  jit-lock-stealth-verbose nil

  ;; cursor and selection settings
  mouse-drag-copy-region nil
  select-active-regions t
  ring-bell-function 'ignore
  x-stretch-cursor t  
  cursor-type 'box
  
  ;; clipboard stuff
  x-select-enable-clipboard t
  x-select-enable-primary t
  x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)
  interprogram-cut-function #'x-select-text
  interprogram-paste-function #'x-selection-value
  
  ;; backup locations
  backup-directory-alist
  `((".*" . ,(concat user-emacs-directory "data/backups")))
  auto-save-file-name-transforms
  `((".*" ,(concat user-emacs-directory "data/autosave") t))

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
  vc-make-backup-files t)

(setq-default
  buffer-file-coding-system 'utf-8
  indent-tabs-mode nil)

(defalias 'yes-or-no-p #'y-or-n-p)

(add-hook 'find-file-hook (lambda () (prefer-coding-system 'utf-8)))
(add-hook 'before-save-hook (lambda () (setq buffer-backed-up nil)))
(add-hook 'emacs-startup-hook (lambda () (message "")))

(load-theme 'tango-dark)

