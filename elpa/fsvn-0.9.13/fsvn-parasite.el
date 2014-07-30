;;; fsvn-parasite.el --- Parasite to any major-mode

;;; History:
;; 

;;; Commentary:
;; 

;;; Code:



(require 'fsvn-msgedit)
(require 'fsvn-select)



(defun fsvn-parasite-check-mode (&rest modes)
  (unless (memq major-mode modes)
    (error "Not arrowed operation in this major-mode `%s'" major-mode)))

(defmacro fsvn-parasite-in-select-file (&rest form)
  `(let ((BUFFER (if (eq major-mode 'fsvn-select-file-mode) 
                     (current-buffer)
                   fsvn-message-edit-file-select-buffer)))
     (unless BUFFER
       (error "File Select buffer is not found"))
     (with-current-buffer BUFFER
       ,@form)))

(defmacro fsvn-parasite-in-message-edit (&rest form)
  `(let ((BUFFER (if (eq major-mode 'fsvn-message-edit-mode)
                     (current-buffer)
                   fsvn-select-file-msgedit-buffer)))
     (unless BUFFER
       (error "Message Edit buffer is not found"))
     (with-current-buffer BUFFER
       ,@form)))

(defun fsvn-parasite-make-buffer-variables (variables)
  (fsvn-make-buffer-variables-internal variables))

(defun fsvn-parasite-when-kill-buffer ()
  (fsvn-parasite-cleanup-buffers 
   (fsvn-parasite-related-buffers)))

(defun fsvn-parasite-cleanup-buffers (buffers)
  (mapc
   (lambda (b)
     (when (and b (buffer-live-p b))
       (kill-buffer b)))
   buffers))

(defun fsvn-parasite-quit-message-edit ()
  (interactive)
  (fsvn-restore-window-buffer 
   (fsvn-parasite-cleanup-message-edit)))

(defun fsvn-parasite-cleanup-message-edit ()
  (fsvn-parasite-when-kill-buffer))

(defun fsvn-parasite-setup-message-edit-window (msgedit-buffer)
  (delete-other-windows)
  (let ((win (split-window)))
    (set-window-buffer win msgedit-buffer)
    (set-frame-selected-window (selected-frame) win))
  (fsvn-parasite-show-brief-help))

(defvar fsvn-parasite-brief-help-function nil)

(defun fsvn-parasite-show-brief-help ()
  (message (substitute-command-keys (funcall fsvn-parasite-brief-help-function))))

(defun fsvn-parasite-related-buffers ()
  (remove nil (list 
               fsvn-select-file-msgedit-buffer
               fsvn-message-edit-file-select-buffer
               (current-buffer))))

(defun fsvn-parasite-quit ()
  (interactive)
  (fsvn-restore-window-buffer 
   (fsvn-parasite-cleanup-buffers
    (fsvn-parasite-related-buffers))))



;;; commit

(defvar fsvn-parasite-commit-mode-map nil)

(unless fsvn-parasite-commit-mode-map
  (setq fsvn-parasite-commit-mode-map
        (let ((map (make-sparse-keymap)))

          (define-key map "\C-c\C-c" 'fsvn-parasite-commit-execute)
          (define-key map "\C-c\C-o" 'fsvn-parasite-commit-cycle-window)
          (define-key map "\C-c\C-q" 'fsvn-parasite-quit)
          (define-key map "\C-c\C-k" 'fsvn-parasite-quit)
          (define-key map "\C-c\en" 'fsvn-parasite-commit-toggle-no-unlock)
          (define-key map "\C-c\ek" 'fsvn-parasite-commit-toggle-keep-changelist)

          map)))

(defconst fsvn-parasite-commit-buffer-local-variables
  '(
    (fsvn-parasite-commit-no-unlock)
    (fsvn-parasite-commit-keep-changelist)
    (fsvn-parasite-commit-target-files)
    (fsvn-parasite-commit-subcommand-args)
    (fsvn-parasite-brief-help-function . 'fsvn-parasite-commit-brief-help)
    ))

(defvar fsvn-parasite-commit-no-unlock nil)
(defvar fsvn-parasite-commit-keep-changelist nil)
(defvar fsvn-parasite-commit-subcommand-args nil)
(defvar fsvn-parasite-commit-target-files nil)

(defcustom fsvn-parasite-commit-before-commit-hook nil
  ""
  :group 'fsvn
  :type 'hook)

(defcustom fsvn-parasite-commit-after-commit-hook nil
  ""
  :group 'fsvn
  :type 'hook)

(define-minor-mode fsvn-parasite-commit-mode
  "
Keybindings:
\\{fsvn-parasite-commit-mode-map}"
  nil (" (Commit)" 
       (:eval fsvn-parasite-commit-no-unlock)
       (:eval fsvn-parasite-commit-keep-changelist))
  fsvn-parasite-commit-mode-map
  (fsvn-parasite-check-mode 'fsvn-select-file-mode 'fsvn-message-edit-mode)
  (fsvn-parasite-make-buffer-variables fsvn-parasite-commit-buffer-local-variables))

(defun fsvn-parasite-commit-draw-list ()
  (fsvn-parasite-in-select-file
   (fsvn-parasite-commit-draw-applicant fsvn-parasite-commit-target-files)))

(defun fsvn-parasite-commit-draw-applicant (files)
  (fsvn-select-file-draw-applicant files "Commit marked files below." "No changed/added files.")
  (setq fsvn-parasite-commit-target-files files))

(defun fsvn-parasite-commit-substitute-subcommand-arg (arg value)
  (setq fsvn-parasite-commit-subcommand-args
        (cond
         ((and (null value) (member arg fsvn-parasite-commit-subcommand-args))
          (fsvn-delete arg fsvn-parasite-commit-subcommand-args))
         ((and value (not (member arg fsvn-parasite-commit-subcommand-args)))
          (cons arg fsvn-parasite-commit-subcommand-args))
         (t
          fsvn-parasite-commit-subcommand-args))))

(defun fsvn-parasite-commit-brief-help ()
  (concat "Type \\[fsvn-parasite-commit-execute] to commit edit, "
          "\\[fsvn-parasite-quit] to quit edit, "
          "\\[fsvn-parasite-commit-cycle-window] to cycle window."))

(defun fsvn-parasite-commit-internal (arg-files arg-buffer)
  (fsvn-async-let ((targets (fsvn-make-targets-file arg-files))
                   (message (fsvn-message-edit-create-message-file))
                   (no-unlock fsvn-parasite-commit-no-unlock)
                   (args fsvn-parasite-commit-subcommand-args)
                   (output-size (buffer-size arg-buffer))
                   (files arg-files)
                   (buffer arg-buffer)
                   (buffers (fsvn-parasite-related-buffers))
                   proc locked unversioned)
    (setq locked (fsvn-parasite-commit-choice-just-locked files))
    (setq unversioned (fsvn-parasite-commit-choice-unversioned files))
    (when (> (length unversioned) 0)
      (let ((unversiond-targets (fsvn-make-targets-file unversioned)))
        (prog1
            (setq proc (fsvn-start-command-display "add" buffer "--targets" unversiond-targets "--non-recursive"))
          (set-process-sentinel proc (lambda (proc event)
                                       (fsvn-process-exit-handler proc event
                                         (fsvn-parse-result-cmd-add buffer output-size)
                                         (goto-char (point-max))
                                         (insert "\n")
                                         (setq output-size (buffer-size (current-buffer)))))))))
    (unless no-unlock
      (when locked
        (let ((locked-targets (fsvn-make-targets-file locked)))
          (prog1
              (setq proc (fsvn-start-command-display "unlock" buffer "--targets" locked-targets))
            (set-process-sentinel proc (lambda (proc event)
                                         (fsvn-process-exit-handler proc event
                                           (fsvn-parse-result-cmd-unlock buffer output-size)
                                           (goto-char (point-max))
                                           (insert "\n")
                                           (setq output-size (buffer-size (current-buffer))))))))))
    (prog1
        (setq proc (fsvn-start-command-display "commit" buffer
                                       "--targets" targets
                                       (if message
                                           (list "--file" message)
                                         (list "--message" ""))
                                       "--encoding" (fsvn-coding-system-name fsvn-message-edit-file-encoding)
                                       args))
      (process-put proc 'fsvn-parasite-cleanup-buffers buffers)
      (set-process-sentinel proc 'fsvn-parasite-commit-sentinel)
      (set-process-filter proc 'fsvn-popup-process-filter-in-buffer))
    (process-put proc 'fsvn-process-start-point output-size)))

(defun fsvn-parasite-commit-sentinel (proc event)
  (fsvn-process-exit-handler proc event
    (when (= (process-exit-status proc) 0)
      (let ((output-size (process-get proc 'fsvn-process-start-point)))
        (fsvn-parse-result-cmd-commit (current-buffer) output-size)
        (fsvn-parasite-cleanup-buffers (process-get proc 'fsvn-parasite-cleanup-buffers)))
      (run-hooks 'fsvn-parasite-commit-after-commit-hook)
      (fsvn-run-recursive-status (fsvn-find-most-top-buffer-directory default-directory)))))

(defun fsvn-parasite-commit-check (files)
  (let ((dir default-directory))
    (when (fsvn-config-tortoise-property-use (fsvn-buffer-repos-root))
      (fsvn-tortoise-commit-check files dir))))

(defun fsvn-parasite-commit-gather-marked-files ()
  (fsvn-parasite-in-select-file 
   (fsvn-select-file-gather-marked-files)))

(defun fsvn-parasite-commit-choice-unversioned (files)
  (fsvn-parasite-in-select-file 
   (fsvn-select-file-choice-unversioned files)))

(defun fsvn-parasite-commit-choice-just-locked (files)
  (fsvn-parasite-in-select-file 
   (fsvn-select-file-choice-just-locked files)))

(defun fsvn-parasite-commit-set-subcommand-args (args)
  (when (member "--no-unlock" args)
    (fsvn-parasite-commit-toggle-no-unlock t))
  (when (member "--keep-changelist" args)
    (fsvn-parasite-commit-toggle-keep-changelist t))
  (setq fsvn-parasite-commit-subcommand-args args))

(defun fsvn-parasite-commit-setup-window (log fselect &optional no-msg)
  (delete-other-windows)
  (let* ((log-win (split-window))
         (fselect-win (selected-window))
         (root (fsvn-buffer-repos-root))
         sel-buffer sel-window)
    (set-window-buffer log-win log)
    (set-window-buffer fselect-win fselect)
    ;; move focus to log edit buffer.
    (if (fsvn-config-commit-default-file-select-p root)
        (setq sel-window fselect-win
              sel-buffer fselect)
      (setq sel-window log-win
            sel-buffer log))
    (set-frame-selected-window (selected-frame) sel-window)
    (let ((f (lambda (buffer)
               (fsvn-set-buffer-local-variable
                buffer
                'fsvn-default-window-configuration (current-window-configuration)))))
      (funcall f log)
      (funcall f fselect))
    (switch-to-buffer sel-buffer)
    (unless no-msg
      (fsvn-parasite-show-brief-help))))

(defun fsvn-parasite-commit-get-buffers (files)
  (catch 'found
    (fsvn-select-file-each-buffers 'fsvn-parasite-commit-mode
      (when (and (equal fsvn-parasite-commit-target-files files)
                 (buffer-live-p fsvn-select-file-msgedit-buffer))
        (throw 'found (cons (current-buffer) fsvn-select-file-msgedit-buffer))))
    (cons (fsvn-select-file-generate-buffer) (fsvn-message-edit-generate-buffer))))

(defun fsvn-parasite-commit-execute ()
  (interactive)
  (fsvn-parasite-in-message-edit
   (run-hooks 'fsvn-parasite-commit-before-commit-hook)
   (let ((files (fsvn-parasite-commit-gather-marked-files))
         (buffer (fsvn-popup-result-create-buffer))
         msg)
     (if (= (length files) 0)
         (message "No file to be commited.")
       (fsvn-parasite-commit-check files)
       (prog1
           (fsvn-restore-window-buffer
            (fsvn-parasite-commit-internal files buffer))
         (fsvn-buffer-popup-as-information buffer))))))

(defun fsvn-parasite-commit-cycle-window ()
  (interactive)
  (let ((mode major-mode)
        (buffers (remove (current-buffer) (fsvn-parasite-related-buffers))))
    (when buffers
      (fsvn-restore-default-window-setting)
      (fsvn-switch-buffer-window (car buffers)))))

(defun fsvn-parasite-commit-toggle-no-unlock (&optional arg)
  (interactive "P")
  (fsvn-parasite-in-message-edit
   (let ((value (fsvn-toggle-mode-line-variable
                 arg 'fsvn-parasite-commit-no-unlock
                 " (No Unlock)" "no unlock")))
     (fsvn-parasite-commit-substitute-subcommand-arg "--no-unlock" value))))

(defun fsvn-parasite-commit-toggle-keep-changelist (&optional arg)
  (interactive "P")
  (fsvn-parasite-in-message-edit
   (let ((value (fsvn-toggle-mode-line-variable
                 arg 'fsvn-parasite-commit-keep-changelist
                 " (Keep Changelist)" "keep changelist")))
     (fsvn-parasite-commit-substitute-subcommand-arg "--keep-changelist" value))))



;;; delete

(defvar fsvn-parasite-delete-mode-map nil)

(unless fsvn-parasite-delete-mode-map
  (setq fsvn-parasite-delete-mode-map
        (let ((map (make-sparse-keymap)))

          (define-key map "\C-c\C-c" 'fsvn-parasite-delete-execute)
          (define-key map "\C-c\C-q" 'fsvn-parasite-delete-quit)
          (define-key map "\C-c\C-k" 'fsvn-parasite-delete-quit)

          map)))

(defconst fsvn-parasite-delete-buffer-local-variables
  '(
    (fsvn-parasite-delete-target-files)
    (fsvn-parasite-delete-subcommand-args)
    (fsvn-parasite-brief-help-function . 'fsvn-parasite-delete-brief-help)
    ))

(defvar fsvn-parasite-delete-subcommand-args nil)
(defvar fsvn-parasite-delete-target-files nil)

(define-minor-mode fsvn-parasite-delete-mode
  "
Keybindings:
\\{fsvn-parasite-delete-mode-map}"
  nil " (Delete)" fsvn-parasite-delete-mode-map
  (fsvn-parasite-check-mode 'fsvn-message-edit-mode)
  (fsvn-parasite-make-buffer-variables fsvn-parasite-delete-buffer-local-variables))

(defun fsvn-parasite-delete-internal (targets buffer message)
  (let (proc)
    (setq proc (fsvn-start-command-display 
                "delete" buffer
                "--targets" targets
                (if message
                    (list "--file" message)
                  (list "--message" ""))
                "--encoding" (fsvn-coding-system-name fsvn-message-edit-file-encoding)
                fsvn-parasite-delete-subcommand-args
                fsvn-parasite-delete-target-files))
    (process-put proc 'fsvn-parasite-cleanup-buffers (fsvn-parasite-related-buffers))
    (set-process-sentinel proc 'fsvn-parasite-delete-sentinel)
    (set-process-filter proc 'fsvn-popup-process-filter-in-buffer)
    proc))

(defun fsvn-parasite-delete-sentinel (proc event)
  (fsvn-process-exit-handler proc event
    (when (= (process-exit-status proc) 0)
      (fsvn-parse-result-cmd-delete (current-buffer))
      (fsvn-parasite-cleanup-buffers 
       (process-get proc 'fsvn-parasite-cleanup-buffers)))))

(defun fsvn-parasite-delete-brief-help ()
  (concat "Type \\[fsvn-parasite-delete-execute] to delete files and commit, "
          "\\[fsvn-parasite-delete-execute] to quit edit."))

(defun fsvn-parasite-delete-execute ()
  (interactive)
  (run-hooks 'fsvn-parasite-commit-before-commit-hook)
  (let ((buffer (fsvn-popup-result-create-buffer))
        (targets (fsvn-make-targets-file fsvn-parasite-delete-target-files))
        (message (fsvn-message-edit-create-message-file)))
    (prog1
        (fsvn-restore-window-buffer
         (fsvn-parasite-delete-internal targets buffer message))
      (fsvn-buffer-popup-as-information buffer))))

(defalias 'fsvn-parasite-delete-quit 'fsvn-parasite-quit-message-edit)
(defalias 'fsvn-parasite-delete-cleanup-buffer 'fsvn-parasite-cleanup-message-edit)



;;; import

(defvar fsvn-parasite-import-mode-map nil)

(unless fsvn-parasite-import-mode-map
  (setq fsvn-parasite-import-mode-map
        (let ((map (make-sparse-keymap)))

          (define-key map "\C-c\C-c" 'fsvn-parasite-import-execute)
          (define-key map "\C-c\C-q" 'fsvn-parasite-import-quit)
          (define-key map "\C-c\C-k" 'fsvn-parasite-import-quit)

          map)))

(defconst fsvn-parasite-import-buffer-local-variables
  '(
    (fsvn-parasite-import-target-path)
    (fsvn-parasite-import-target-url)
    (fsvn-parasite-import-subcommand-args)
    (fsvn-parasite-brief-help-function . 'fsvn-parasite-import-brief-help)
    ))

(defvar fsvn-parasite-import-subcommand-args nil)
(defvar fsvn-parasite-import-target-path nil)
(defvar fsvn-parasite-import-target-url nil)

(define-minor-mode fsvn-parasite-import-mode
  "
Keybindings:
\\{fsvn-parasite-import-mode-map}"
  nil " (Import)" fsvn-parasite-import-mode-map
  (fsvn-parasite-check-mode 'fsvn-message-edit-mode)
  (fsvn-parasite-make-buffer-variables fsvn-parasite-import-buffer-local-variables))

(defun fsvn-parasite-import-internal (buffer)
  (let ((message (fsvn-message-edit-create-message-file))
        proc)
    (setq proc (fsvn-start-command-display
                "import" buffer
                (if message
                    (list "--file" message)
                  (list "--message" ""))
                "--encoding" (fsvn-coding-system-name fsvn-message-edit-file-encoding)
                fsvn-parasite-import-subcommand-args
                fsvn-parasite-import-target-path
                fsvn-parasite-import-target-url))
    (process-put proc 'fsvn-parasite-cleanup-buffers (fsvn-parasite-related-buffers))
    (set-process-sentinel proc 'fsvn-parasite-import-sentinel)
    (set-process-filter proc 'fsvn-popup-process-filter-in-buffer)
    proc))

(defun fsvn-parasite-import-sentinel (proc event)
  (fsvn-process-exit-handler proc event
    (when (= (process-exit-status proc) 0)
      (fsvn-parasite-cleanup-buffers (process-get proc 'fsvn-parasite-cleanup-buffers))
      (run-hooks 'fsvn-parasite-commit-after-commit-hook))))

(defun fsvn-parasite-import-brief-help ()
  (concat "Type \\[fsvn-parasite-import-execute] to import path, "
          "\\[fsvn-parasite-import-quit] to quit edit."))

(defun fsvn-parasite-import-execute ()
  (interactive)
  (run-hooks 'fsvn-parasite-commit-before-commit-hook)
  (let ((buffer (fsvn-popup-result-create-buffer)))
    (prog1
        (fsvn-restore-window-buffer
         (fsvn-parasite-import-internal buffer))
      (fsvn-buffer-popup-as-information buffer))))

(defalias 'fsvn-parasite-import-quit 'fsvn-parasite-quit-message-edit)
(defalias 'fsvn-parasite-import-cleanup-buffer 'fsvn-parasite-cleanup-message-edit)



;;; mkdir

(defvar fsvn-parasite-mkdir-mode-map nil)

(unless fsvn-parasite-mkdir-mode-map
  (setq fsvn-parasite-mkdir-mode-map
        (let ((map (make-sparse-keymap)))

          (define-key map "\C-c\C-c" 'fsvn-parasite-mkdir-execute)
          (define-key map "\C-c\C-q" 'fsvn-parasite-mkdir-quit)
          (define-key map "\C-c\C-k" 'fsvn-parasite-mkdir-quit)

          map)))

(defconst fsvn-parasite-mkdir-buffer-local-variables
  '(
    (fsvn-parasite-mkdir-target-directory)
    (fsvn-parasite-mkdir-subcommand-args)
    (fsvn-parasite-brief-help-function . 'fsvn-parasite-mkdir-brief-help)
    ))

(defvar fsvn-parasite-mkdir-subcommand-args nil)
(defvar fsvn-parasite-mkdir-target-directory nil)

(define-minor-mode fsvn-parasite-mkdir-mode
  "
Keybindings:
\\{fsvn-parasite-mkdir-mode-map}"
  nil " (Make directory)" fsvn-parasite-mkdir-mode-map
  (fsvn-parasite-check-mode 'fsvn-message-edit-mode)
  (fsvn-parasite-make-buffer-variables fsvn-parasite-mkdir-buffer-local-variables))

(defun fsvn-parasite-mkdir-internal (buffer message)
  (let (proc)
    (setq proc (fsvn-start-command-display
                "mkdir" buffer
                (if message
                    (list "--file" message)
                  (list "--message" ""))
                "--encoding" (fsvn-coding-system-name fsvn-message-edit-file-encoding)
                fsvn-parasite-mkdir-target-directory))
    (process-put proc 'fsvn-parasite-cleanup-buffers (fsvn-parasite-related-buffers))
    (set-process-sentinel proc 'fsvn-parasite-mkdir-sentinel)
    (set-process-filter proc 'fsvn-popup-process-filter-in-buffer)
    proc))

(defun fsvn-parasite-mkdir-sentinel (proc event)
  (fsvn-process-exit-handler proc event
    (when (= (process-exit-status proc) 0)
      (fsvn-parasite-cleanup-buffers (process-get proc 'fsvn-parasite-cleanup-buffers))
      (run-hooks 'fsvn-parasite-commit-after-commit-hook))))

(defun fsvn-parasite-mkdir-brief-help ()
  (concat "Type \\[fsvn-parasite-mkdir-execute] to make directory and commit, "
          "\\[fsvn-parasite-mkdir-quit] to quit edit."))

(defun fsvn-parasite-mkdir-execute ()
  (interactive)
  (run-hooks 'fsvn-parasite-commit-before-commit-hook)
  (let ((buffer (fsvn-popup-result-create-buffer))
        (message (fsvn-message-edit-create-message-file))
        proc)
    (prog1
        (fsvn-restore-window-buffer
         (fsvn-parasite-mkdir-internal buffer message))
      (fsvn-buffer-popup-as-information buffer))))

(defalias 'fsvn-parasite-mkdir-quit 'fsvn-parasite-quit-message-edit)
(defalias 'fsvn-parasite-mkdir-cleanup-buffer 'fsvn-parasite-cleanup-message-edit)



;;; lock

(defvar fsvn-parasite-lock-mode-map nil)

(unless fsvn-parasite-lock-mode-map
  (setq fsvn-parasite-lock-mode-map
        (let ((map (make-sparse-keymap)))

          (define-key map "\C-c\C-c" 'fsvn-parasite-lock-execute)
          (define-key map "\C-c\C-q" 'fsvn-parasite-lock-quit)
          (define-key map "\C-c\C-k" 'fsvn-parasite-lock-quit)

          map)))

(defconst fsvn-parasite-lock-buffer-local-variables
  '(
    (fsvn-parasite-lock-target-files)
    (fsvn-parasite-lock-subcommand-args)
    (fsvn-parasite-brief-help-function . 'fsvn-parasite-lock-brief-help)
    ))

(defvar fsvn-parasite-lock-subcommand-args nil)
(defvar fsvn-parasite-lock-target-files nil)

(define-minor-mode fsvn-parasite-lock-mode
  "
Keybindings:
\\{fsvn-parasite-lock-mode-map}"
  nil " (Lock)" fsvn-parasite-lock-mode-map
  (fsvn-parasite-check-mode 'fsvn-message-edit-mode)
  (fsvn-parasite-make-buffer-variables fsvn-parasite-lock-buffer-local-variables))

(defun fsvn-parasite-lock-brief-help ()
  (concat "Type \\[fsvn-parasite-lock-execute] to lock files, "
          "\\[fsvn-parasite-lock-quit] to quit edit."))

(defun fsvn-parasite-lock-internal (buffer)
  (let ((message (fsvn-message-edit-create-message-file))
        proc)
    (setq proc (fsvn-start-command-display
                "lock" buffer
                (if message
                    (list "--file" message)
                  (list "--message" ""))
                "--encoding" (fsvn-coding-system-name fsvn-message-edit-file-encoding)
                fsvn-parasite-lock-subcommand-args fsvn-parasite-lock-target-files))
    (process-put proc 'fsvn-parasite-cleanup-buffers (fsvn-parasite-related-buffers))
    (set-process-sentinel proc 'fsvn-parasite-lock-sentinel)
    (set-process-filter proc 'fsvn-popup-process-filter-in-buffer)
    proc))

(defun fsvn-parasite-lock-sentinel (proc event)
  (fsvn-process-exit-handler proc event
    (when (= (process-exit-status proc) 0)
      (fsvn-parse-result-cmd-lock (current-buffer))
      (fsvn-parasite-cleanup-buffers (process-get proc 'fsvn-parasite-cleanup-buffers)))))

(defun fsvn-parasite-lock-set-subcommand-args (args)
  (when (member "--message" args)
    ;; remove message arguments
    (let ((split (fsvn-split-list "--message" args)))
      (when (car (cdr (cdr split)))
        (insert (car (cdr (cdr split)))))
      (setq args (append (car split) (cdr (cdr (cdr split)))))))
  (setq fsvn-parasite-lock-subcommand-args args))

(defun fsvn-parasite-lock-execute ()
  (interactive)
  (let ((buffer (fsvn-popup-result-create-buffer)))
    (prog1
        (fsvn-restore-window-buffer
         (fsvn-parasite-lock-internal buffer))
      (fsvn-buffer-popup-as-information buffer))))

(defalias 'fsvn-parasite-lock-quit 'fsvn-parasite-quit-message-edit)
(defalias 'fsvn-parasite-lock-cleanup-buffer 'fsvn-parasite-cleanup-message-edit)



;;; copy

(defvar fsvn-parasite-copy-mode-map nil)

(unless fsvn-parasite-copy-mode-map
  (setq fsvn-parasite-copy-mode-map
        (let ((map (make-sparse-keymap)))

          (define-key map "\C-c\C-c" 'fsvn-parasite-copy-execute)
          (define-key map "\C-c\C-q" 'fsvn-parasite-copy-quit)
          (define-key map "\C-c\C-k" 'fsvn-parasite-copy-quit)

          map)))

(defconst fsvn-parasite-copy-buffer-local-variables
  '(
    (fsvn-parasite-copy-from-files)
    (fsvn-parasite-copy-destination)
    (fsvn-parasite-copy-subcommand-args)
    (fsvn-parasite-brief-help-function . 'fsvn-parasite-copy-brief-help)
    ))

(defvar fsvn-parasite-copy-subcommand-args nil)
(defvar fsvn-parasite-copy-from-files nil)
(defvar fsvn-parasite-copy-destination nil)

(define-minor-mode fsvn-parasite-copy-mode
  "
Keybindings:
\\{fsvn-parasite-copy-mode-map}"
  nil " (Copy)" fsvn-parasite-copy-mode-map
  (fsvn-parasite-check-mode 'fsvn-message-edit-mode)
  (fsvn-parasite-make-buffer-variables fsvn-parasite-copy-buffer-local-variables))

(defun fsvn-parasite-copy-internal (buffer message)
  (let (proc)
    (setq proc (fsvn-start-command-display
                "copy" buffer
                (if message
                    (list "--file" message)
                  (list "--message" ""))
                "--encoding" (fsvn-coding-system-name fsvn-message-edit-file-encoding)
                fsvn-parasite-copy-subcommand-args
                fsvn-parasite-copy-from-files
                fsvn-parasite-copy-destination))
    (process-put proc 'fsvn-parasite-cleanup-buffers (fsvn-parasite-related-buffers))
    (set-process-sentinel proc 'fsvn-parasite-copy-sentinel)
    (set-process-filter proc 'fsvn-popup-process-filter-in-buffer)
    proc))

(defun fsvn-parasite-copy-sentinel (proc event)
  (fsvn-process-exit-handler proc event
    (when (= (process-exit-status proc) 0)
      (fsvn-parasite-cleanup-buffers (process-get proc 'fsvn-parasite-cleanup-buffers))
      (run-hooks 'fsvn-parasite-commit-after-commit-hook))))

(defun fsvn-parasite-copy-brief-help ()
  (concat "Type \\[fsvn-parasite-copy-execute] to copy and commit files, "
          "\\[fsvn-parasite-copy-quit] to quit edit."))

(defun fsvn-parasite-copy-execute ()
  (interactive)
  (run-hooks 'fsvn-parasite-commit-before-commit-hook)
  (let ((buffer (fsvn-popup-result-create-buffer))
        (message (fsvn-message-edit-create-message-file))
        proc)
    (prog1
        (fsvn-restore-window-buffer
         (fsvn-parasite-copy-internal buffer message))
      (fsvn-buffer-popup-as-information buffer))))

(defalias 'fsvn-parasite-copy-quit 'fsvn-parasite-quit-message-edit)
(defalias 'fsvn-parasite-copy-cleanup-buffer 'fsvn-parasite-cleanup-message-edit)



;;; add

(defvar fsvn-parasite-add-mode-map nil)

(unless fsvn-parasite-add-mode-map
  (setq fsvn-parasite-add-mode-map
        (let ((map (make-sparse-keymap)))

          (define-key map "\C-c\C-c" 'fsvn-parasite-add-execute)
          (define-key map "\C-c\C-q" 'fsvn-parasite-quit)
          (define-key map "\C-c\C-k" 'fsvn-parasite-quit)

          map)))

(defconst fsvn-parasite-add-buffer-local-variables
  '(
    (fsvn-parasite-add-subcommand-args)
    (fsvn-parasite-add-target-files)
    (fsvn-parasite-brief-help-function . 'fsvn-parasite-add-brief-help)
    ))

(defvar fsvn-parasite-add-target-files nil)
(defvar fsvn-parasite-add-subcommand-args nil)

(define-minor-mode fsvn-parasite-add-mode
  "
Keybindings:
\\{fsvn-parasite-add-mode-map}"
  nil " (Add)" fsvn-parasite-add-mode-map
  (fsvn-parasite-check-mode 'fsvn-select-file-mode)
  (setq fsvn-select-file-draw-list-function 'fsvn-parasite-add-draw-list)
  (fsvn-parasite-make-buffer-variables fsvn-parasite-add-buffer-local-variables))

(defun fsvn-parasite-add-draw-list ()
  (fsvn-parasite-in-select-file
   (fsvn-parasite-add-draw-applicant fsvn-parasite-add-target-files)))

(defun fsvn-parasite-add-draw-applicant (files)
  (fsvn-select-file-draw-applicant files "Add marked files below." "No Unknown files.")
  (setq fsvn-parasite-add-target-files files))

(defun fsvn-parasite-add-brief-help ()
  (concat "Type \\[fsvn-parasite-add-execute] to add files, "
          "\\[fsvn-parasite-quit] to quit edit."))

(defun fsvn-parasite-add-setup-window (select)
  (delete-other-windows)
  (let* ((file-win (selected-window)))
    (set-window-buffer file-win select)
    (set-frame-selected-window (selected-frame) file-win)
    (fsvn-set-buffer-local-variable
     select
     'fsvn-default-window-configuration (current-window-configuration))
    (switch-to-buffer select)
    (fsvn-parasite-show-brief-help)))

(defun fsvn-parasite-add-get-buffer (files)
  (catch 'found
    (fsvn-select-file-each-buffers 'fsvn-parasite-add-mode
      (when (equal fsvn-parasite-add-target-files files)
        (throw 'found (current-buffer))))
    (fsvn-select-file-generate-buffer)))

(defun fsvn-parasite-add-execute ()
  (interactive)
  (let ((files (fsvn-select-file-gather-marked-files))
        (dir default-directory)
        (source (current-buffer))
        buffer targets)
    (if (= (length files) 0)
        (message "No file to be added.")
      (fsvn-restore-window-buffer
       (setq buffer (fsvn-popup-result-create-buffer))
       (setq targets (fsvn-make-targets-file files))
       (fsvn-call-command-display "add" buffer "--targets" targets fsvn-parasite-add-subcommand-args)
       (fsvn-parse-result-cmd-add buffer))
      (fsvn-buffer-popup-as-information buffer)
      (fsvn-run-recursive-status (fsvn-find-most-top-buffer-directory dir))
      (kill-buffer source))))



(provide 'fsvn-parasite)

;;; fsvn-parasite.el ends here
