;; -*- lexical-binding: t -*-

(setq ad-redefinition-action 'accept
      cursor-type 'box
      custom-safe-themes
      '("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879"
        "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4"
        default)
      delete-by-moving-to-trash t
      disabled-command-function nil
      echo-keystrokes 0.4
      focus-follows-mouse t
      frame-title-format "ε %b [%m]"
      indent-tabs-mode nil
      inhibit-default-init t
      inhibit-startup-echo-area-message t
      inhibit-startup-screen t
      inhibit-x-resources t
      interprogram-cut-function (and (fboundp #'x-select-text)
                                     #'x-select-text)
      interprogram-paste-function (and (fboundp #'x-selection-value)
                                       #'x-selection-value)
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
      save-interprogram-paste-before-kill t
      sentence-end-double-space nil
      set-mark-command-repeat-pop t
      split-height-threshold 48
      split-width-threshold 160
      use-dialog-box nil
      user-full-name "PythonNut"
      user-mail-address "PythonNut@PythonNut.com"
      visible-cursor nil
      x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)
      x-stretch-cursor t)

(setq-default buffer-file-coding-system 'utf-8
              indent-tabs-mode nil
              indicate-buffer-boundaries '((bottom . left)))

(prefer-coding-system 'utf-8)
(define-coding-system-alias 'UTF-8 'utf-8)

(when (memq window-system '(mac ns))
  (setq mac-command-modifier 'meta
        mac-option-modifier 'super)
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t)))

(defalias 'yes-or-no-p #'y-or-n-p)

(defun byte-recompile-config (&optional arg)
  (interactive "p")
  "Recompile this Emacs configuration.
If passed a non-nil or called interactively with a C-u, also recompile
files with (apparently) up to date bytecodes."
  (let* ((force (if (called-interactively-p 'any)
                    (and (integerp arg) (= arg 4))
                  arg))
         (init-el-error
          (progn
            (when force
              (delete-file (locate-user-emacs-file "init.elc")))
            (not (byte-compile-file (locate-user-emacs-file "init.el")))))
         (modules-error (string-match-p
                         (rx "failed")
                         (byte-recompile-directory
                          (locate-user-emacs-file "modules/")
                          0
                          force))))
    (or init-el-error modules-error)))

(defun emergency-fix-config ()
  "Non-destructively reset the config to whatever git is tracking."
  (interactive)
  (when (fboundp 'my/package-rebuild-autoloads)
    (my/package-rebuild-autoloads))
  (let ((default-directory user-emacs-directory)
        (module-dir (locate-user-emacs-file "modules")))
    (shell-command "git stash")
    (shell-command "git clean -ffXd :/")
    (shell-command "git pull --rebase -X histogram")
    (with-demoted-errors "Emergency fix delete error: %s"
      (mapc (lambda (file) (delete-file file t))
            (append
             (list (locate-user-emacs-file "init.elc"))
             (file-expand-wildcards (concat module-dir "/*.elc"))))
      (delete-directory (locate-user-emacs-file "elpa") t t))
    (byte-recompile-config)
    (package-initialize)
    (my/ensure-packages-are-installed (bound-and-true-p my/required-packages))
    (restart-emacs)))

(blink-cursor-mode -1)
(delete-selection-mode +1)
(global-hl-line-mode +1)

;; Lazily load auto-compression-mode
(defun my/auto-compression-onetime-setup ()
  (auto-compression-mode +1)
  (remove-hook 'find-file-hook #'my/auto-compression-onetime-setup))

(add-hook 'find-file-hook #'my/auto-compression-onetime-setup)

;; encryption mode
(setq epa-file-name-regexp (rx (or ".gpg" ".asc") line-end))

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
     ,@(mapcar (lambda (arg) `(,(cadr operator) (,@arg))) (cadr arglist))))

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

If arg is nil, unset the urgency.
If arg is any other value, set the urgency.
If you unset the urgency, you still have to visit the frame to make the
urgency setting disappear (at least in KDE)."
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

With a prefix argument which does not equal a boolean value of nil,
remove the urgency flag (which might or might not change display,
depending on the window manager)."
  (interactive "P")
  (let (frame (car (car (cdr (current-frame-configuration)))))
    (my/x-urgency-hint frame (not arg)))
  (unless arg
    (run-with-timer 10 nil
                    (lambda ()
                      (my/x-urgent t)))))

(defun my/y-or-n-p-optional (prompt)
  "Prompt the user for a yes or no response, but accept any non-y
response as a no."
  (let ((query-replace-map (copy-keymap query-replace-map)))
    (define-key query-replace-map [t] 'skip)
    (y-or-n-p prompt)))

(provide 'config-setq)
