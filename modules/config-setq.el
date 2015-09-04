(setq
  ad-redefinition-action 'accept
  cursor-type 'box
  delete-by-moving-to-trash t
  echo-keystrokes 0.4
  focus-follows-mouse t
  gc-cons-threshold 20000000
  indent-tabs-mode nil
  inhibit-default-init t
  inhibit-startup-echo-area-message t
  inhibit-startup-screen t
  inhibit-x-resources t
  interprogram-cut-function #'x-select-text
  interprogram-paste-function #'x-selection-value
  jit-lock-defer-time 0.04
  jit-lock-stealth-nice 0.1
  jit-lock-stealth-time 0.2
  jit-lock-stealth-verbose nil
  left-margin-width 0
  max-lisp-eval-depth 5000
  max-specpdl-size 10000
  mode-line-end-spaces nil
  mouse-autoselect-window 0.3
  mouse-drag-copy-region nil
  right-margin-width 0
  ring-bell-function 'ignore
  split-height-threshold 0
  split-width-threshold 0
  tab-width 4
  use-dialog-box nil
  x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)
  x-stretch-cursor t)

(setq-default
  buffer-file-coding-system 'utf-8
  indent-tabs-mode nil)

(put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'erase-buffer 'disabled nil)

(prefer-coding-system 'utf-8)

(defalias 'yes-or-no-p #'y-or-n-p)

(defun restart-emacs (&optional arg)
  (interactive "p")
  ;; We need the new emacs to be spawned after all kill-emacs-hooks
  ;; have been processed and there is nothing interesting left
  (if (/= arg 4)
    (add-hook 'kill-emacs-hook
      (lambda ()
        (if (display-graphic-p)
          (call-process "sh" nil nil nil "-c" "emacs &")
          (suspend-emacs "(sleep 1; emacs -nw < `tty`) & fg; fg")))
      t)

    (desktop-save-in-desktop-dir)
    (add-hook 'kill-emacs-hook
      (lambda ()
        (if (display-graphic-p)
          (call-process "sh" nil nil nil "-c" "emacs --eval '(desktop-read)'&")
          (suspend-emacs
            "(sleep 1; emacs --eval '(desktop-read)' -nw < `tty`) & fg; fg")))
      t))
  (save-buffers-kill-emacs))

(defun byte-recompile-config ()
  (interactive)
  (byte-compile-file (concat
                       user-emacs-directory
                       "init.el"))
  (byte-recompile-directory
    (concat
      user-emacs-directory
      "modules/")
    0))

(defun emergency-fix-config ()
  (interactive)
  (let ((default-directory user-emacs-directory))
    (shell-command "git stash")
    (shell-command "git pull --rebase"))
  (with-demoted-errors
    (delete-directory (concat user-emacs-directory "elpa") t t))
  (my/ensure-packages-are-installed my/required-packages)
  (byte-recompile-config)
  (restart-emacs))

(provide 'config-setq)
