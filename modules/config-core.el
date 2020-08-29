;; -*- lexical-binding: t -*-
(eval-when-compile (require 'config-macros))

(setq ad-redefinition-action 'accept
      cursor-type 'box
      create-lockfiles nil
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
      jka-compr-use-shell nil
      left-margin-width 0
      max-lisp-eval-depth 5000
      max-specpdl-size 10000
      mode-line-end-spaces nil
      mouse-autoselect-window 0.3
      mouse-drag-copy-region nil
      my/flag-debug-init (eq debug-on-error 'startup)
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
        mac-option-modifier 'super
        mac-right-option-modifier nil)
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
          (let ((init-elc (locate-user-emacs-file "init.elc"))
                (init-el (locate-user-emacs-file "init.el")))
            (when force (delete-file init-elc))
            (when (or (not (file-exists-p init-elc))
                      (time-less-p (file-attribute-modification-time
                                    (file-attributes init-elc))
                                   (file-attribute-modification-time
                                    (file-attributes init-el))))
              (not (byte-compile-file (locate-user-emacs-file "init.el"))))))
         (modules-result (byte-recompile-directory
                          (locate-user-emacs-file "modules/")
                          0
                          force))
         (modules-error (when (string-match-p
                               (rx "failed")
                               modules-result)
                          modules-result)))
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

(defun my/file-name-first-existing-parent (file-path &optional include-self)
  (catch 'found-existing-directory
    (let ((temp-path (if include-self
                         file-path
                       (file-name-directory file-path))))
      (while t
        (if (file-exists-p temp-path)
            (throw 'found-existing-directory
                   temp-path)
          ;; strip one directory off the path
          (setq temp-path
                (directory-file-name
                 (file-name-directory (if (file-name-absolute-p temp-path)
                                          temp-path
                                        (expand-file-name temp-path))))))))))

(defun my/detect-fs-stat (fname)
  (string-trim-right
   (with-output-to-string
     (with-current-buffer
         standard-output
       (call-process "stat" nil '(t nil) nil "-f" "-c" "%T" fname)))))

(defun my/msdos-fs (fname)
  (cond ((eq system-type 'gnu/linux)
         (let* ((dir (my/file-name-first-existing-parent
                      (expand-file-name fname)
                      t))
                (fs (my/detect-fs-stat dir)))
           (member fs '("msdos"))))))

(defun my/slow-fs (dir &optional exclude-remote)
  (cond ((and (file-remote-p dir) (not exclude-remote))
         t)
        ((eq system-type 'gnu/linux)
         (let* ((dir (my/file-name-first-existing-parent
                      (expand-file-name dir)
                      t))
                (fs (my/detect-fs-stat dir)))
           (member fs '("fuse.sshfs"))))
        ((eq system-type 'darwin)
         (let* ((dir (my/file-name-first-existing-parent
                      (expand-file-name dir)
                      t))
                (args (append
                       (cl-loop for fs in '("osxfuse")
                                collect "-T"
                                collect fs)
                       (list dir))))
           (with-temp-buffer
             (apply #'call-process "df" nil '(t nil) nil args)
             (/= (point) (point-min)))))))


(defun nadvice/read-passwd/isolate-kill-ring (old-fun &rest args)
  (let ((kill-ring kill-ring))
    (apply old-fun args)))

(advice-add 'read-passwd :around #'nadvice/read-passwd/isolate-kill-ring)

(defun my/detect-shell (&optional dir)
  (require 'tramp)
  (let* ((dir (or dir default-directory))
         (full-shells
          (cl-remove-duplicates
           (cl-remove-if #'not
                         (list (bound-and-true-p explicit-shell-file-name)
                               (bound-and-true-p shell-file-name)
                               (getenv "SHELL")))
           :test #'string=))
         (bare-shells
          (cl-remove-duplicates
           (cl-remove-if #'not
                         (append (cl-mapcar #'file-name-base full-shells)
                                 (list "bash"
                                       "ksh"
                                       "sh")))
           :test #'string=)))
    (if (tramp-tramp-file-p dir)
        (with-parsed-tramp-file-name default-directory vec
          (cl-some (lambda (shell)
                     (substring-no-properties
                      (tramp-find-executable
                       vec shell (tramp-get-remote-path vec) t t)))
                   bare-shells))
      (or (cl-some (lambda (shell)
                     (when (file-exists-p shell)
                       shell))
                   full-shells)
          (cl-some #'executable-find bare-shells)))))

(advice-add
 'shell-command-to-string :around
 (my/defun-as-value nadvice/shell-command-to-string (old-fun &rest args)
   (let ((shell-file-name (my/detect-shell)))
     (apply old-fun args))))

(defun my/process-file-to-string (&rest args)
  (let* ((return-code 0)
         (output
          (with-output-to-string
            (with-current-buffer
                standard-output
              (setq return-code
                    (apply #'process-file args))))))
    (cons return-code (substring-no-properties output))))

(defun my/local-executable-find (name)
  (if (tramp-tramp-file-p default-directory)
      (let ((response (with-parsed-tramp-file-name default-directory vec
                        (tramp-find-executable
                         vec name (tramp-get-remote-path vec) t t))))
        (if (stringp response)
            (substring-no-properties response)
          response))
    (executable-find name)))

(defun my/tramp-build-name-from-localname (localname)
  (with-parsed-tramp-file-name default-directory parsed
    (tramp-make-tramp-file-name
     parsed-method
     parsed-user
     parsed-domain
     parsed-host
     parsed-port
     localname
     parsed-hop)))

(defun my/tramp-localname (path)
  (if (tramp-tramp-file-p path)
      (with-parsed-tramp-file-name path parsed
        parsed-localname)
    path))

(defun hash-buffer (alg)
  (interactive
   (list
    (intern (completing-read "Choose algorithm: "
                             '(md5 sha1 sha224 sha256 sha384 sha512)))))
  (let ((result (secure-hash alg (current-buffer))))
    (message "%s" result)
    (kill-new result)))

(provide 'config-core)
