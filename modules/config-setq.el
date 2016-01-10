;; -*- lexical-binding: t -*-

(setq ad-redefinition-action 'accept
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
      set-mark-command-repeat-pop t
      split-height-threshold 0
      split-width-threshold 0
      use-dialog-box nil
      user-full-name "PythonNut"
      user-mail-address "PythonNut@PythonNut.com"
      visible-cursor nil
      x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)
      x-stretch-cursor t)

(setq-default buffer-file-coding-system 'utf-8
              indent-tabs-mode nil
              indicate-buffer-boundaries '((bottom . left)))

(put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'erase-buffer 'disabled nil)

(prefer-coding-system 'utf-8)

(defalias 'yes-or-no-p #'y-or-n-p)

(defun my/restart-emacs-engine (&rest args)
  (let ((command-args (mapconcat (lambda (x) (format "%s" x))
                                 args
                                 " ")))
    (add-hook 'kill-emacs-hook
              (lambda ()
                (if (display-graphic-p)
                    (call-process "sh" nil nil nil "-c"
                                  (format "emacs %s &"
                                          command-args))
                  (suspend-emacs (format "(emacs %s -nw < `tty`) & fg; fg"
                                         command-args))))
              t)
    (save-buffers-kill-emacs)))

(defun restart-emacs (&optional arg)
  (interactive "P")
  (cond
   ((stringp arg)
    (my/restart-emacs-engine arg))
   ((or (consp arg) (and (numberp arg) (> arg 0)))
    (desktop-save-in-desktop-dir)
    (my/restart-emacs-engine "--eval '(desktop-read)'"))
   ((eq '- arg)
    (my/restart-emacs-engine "--debug-init"))
   ((and (numberp arg) (< arg 0))
    (my/restart-emacs-engine "--eval '(desktop-read)' --debug-init"))
   (t
    (my/restart-emacs-engine))))

(defun byte-recompile-config (&optional arg)
  (interactive "p")
  (when (fboundp 'my/concat-autoloads)
    (my/concat-autoloads))
  (let ((force (if (called-interactively-p 'any)
                   (and (integerp arg) (= arg 4))
                 arg)))
    (when (progn
            (when force
              (delete-file (expand-file-name "init.elc" user-emacs-directory)))
            (byte-compile-file (expand-file-name
                                "init.el"
                                user-emacs-directory)))
      (not (string-match-p "failed"
                           (byte-recompile-directory
                            (expand-file-name
                             "modules/"
                             user-emacs-directory)
                            0
                            force))))))

(defun emergency-fix-config ()
  (interactive)
  (when (fboundp 'my/package-rebuild-autoloads)
    (my/package-rebuild-autoloads))
  (let ((default-directory user-emacs-directory)
        (module-dir (expand-file-name "modules" user-emacs-directory)))
    (shell-command "git stash")
    (shell-command "git clean -ffXd :/")
    (shell-command "git pull --rebase -X histogram")
    (with-demoted-errors "Emergency fix delete error: %s"
      (mapc (lambda (file) (delete-file file t))
            (append
             (list (expand-file-name "init.elc" user-emacs-directory))
             (file-expand-wildcards (concat module-dir "/*.elc"))))
      (delete-directory (expand-file-name "elpa" user-emacs-directory) t t))
    (byte-recompile-config)
    (package-initialize)
    (my/ensure-packages-are-installed (bound-and-true-p my/required-packages))
    (restart-emacs)))

(blink-cursor-mode -1)
(delete-selection-mode +1)
(global-hl-line-mode +1)
(subword-mode +1)

(defun my/auto-compression-onetime-setup ()
  (auto-compression-mode +1)
  (remove-hook 'find-file-hook #'my/auto-compression-onetime-setup))

(add-hook 'find-file-hook #'my/auto-compression-onetime-setup)

;; encryption mode
(setq epa-file-name-regexp (eval-when-compile
                             (concat (regexp-opt (list ".gpg"
                                                       ".asc"))
                                     "$")))
(epa-file-name-regexp-update)
(setenv "GPG_AGENT_INFO" nil)

;; set major mode for new buffers based on file rules
(setq-default major-mode (lambda ()
                           (if buffer-file-name
                              (fundamental-mode)
                             (let ((buffer-file-name (buffer-name)))
                               (set-auto-mode)))))

;; basically, a mapcar for macros
(defmacro my/generate-calls (operator arglists)
  (declare (indent 1))
  `(progn
     ,@(mapcar (lambda (arglist) `(,(cadr operator) ,@arglist)) (cadr arglists))))

(defmacro my/generate-calls-single (operator arglist)
  (declare (indent 1))
  `(progn
     ,@(mapcar (lambda (arg) `(,(cadr operator) (,@arg))) arglist)))

(defun really-kill-emacs ()
  "Like `kill-emacs', but ignores `kill-emacs-hook'."
  (interactive)
  (let (kill-emacs-hook)
    (kill-emacs)))

(defun brutally-kill-emacs ()
  "Use `call-process' to send ourselves a KILL signal."
  (interactive)
  (call-process "kill" nil nil nil "-9" (number-to-string (emacs-pid))))

;; let emacs blink when something interesting happens.
;; in KDE this marks the active Emacs icon in the tray.
(defun my/x-urgency-hint (frame arg &optional source)
  "Set the x-urgency hint for the frame to arg:

- If arg is nil, unset the urgency.
- If arg is any other value, set the urgency.

If you unset the urgency, you still have to visit the frame to make the urgency setting disappear (at least in KDE)."
  (when (and (display-graphic-p)
             (eq window-system 'x))
    (let* ((wm-hints (append (x-window-property
                              "WM_HINTS" frame "WM_HINTS"
                              source nil t) nil))
           (flags (car wm-hints)))
                                        ; (message flags)
      (setcar wm-hints
              (if arg
                  (logior flags #x00000100)
                (logand flags #x1ffffeff)))
      (x-change-window-property "WM_HINTS" wm-hints frame "WM_HINTS" 32 t))))

(defun my/x-urgent (&optional arg)
  "Mark the current emacs frame as requiring urgent attention.

With a prefix argument which does not equal a boolean value of nil, remove the urgency flag (which might or might not change display, depending on the window manager)."
  (interactive "P")
  (let (frame (car (car (cdr (current-frame-configuration)))))
    (my/x-urgency-hint frame (not arg)))
  (unless arg
    (run-with-timer 10 nil
                    (lambda ()
                      (my/x-urgent t)))))

(provide 'config-setq)
