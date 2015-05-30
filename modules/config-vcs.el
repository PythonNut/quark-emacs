(eval-when-compile
  (with-demoted-errors
    (require 'magit)
    (require 'diff-hl)
    (require 'psvn)))

(add-hook 'find-file-hook
  (lambda ()
    (diff-hl-mode +1)
    (diff-hl-update)))

(defun diff-hl-make-temp-file-name (file rev &optional manual)
  "Return a backup file name for REV or the current version of FILE.
If MANUAL is non-nil it means that a name for backups created by
the user should be returned."
  (let* ((diff-hl-temp-location
           (if (file-directory-p "/dev/shm/")
             "/dev/shm/"
             temporary-file-directory))
          (auto-save-file-name-transforms
            `((".*" ,diff-hl-temp-location t))))
    (expand-file-name
      (concat (make-auto-save-file-name)
        ".~" (subst-char-in-string
               ?/ ?_ rev)
        (unless manual ".") "~")
      diff-hl-temp-location)))

(defun diff-hl-create-revision (file revision)
  "Read REVISION of FILE into a buffer and return the buffer."
  (let ((automatic-backup (diff-hl-make-temp-file-name file revision))
         (filebuf (get-file-buffer file))
         (filename (diff-hl-make-temp-file-name file revision 'manual)))
    (unless (file-exists-p filename)
      (if (file-exists-p automatic-backup)
        (rename-file automatic-backup filename nil)
        (with-current-buffer filebuf
          (let ((failed t)
                 (coding-system-for-read 'no-conversion)
                 (coding-system-for-write 'no-conversion))
            (unwind-protect
              (with-temp-file filename
                (let ((outbuf (current-buffer)))
                  ;; Change buffer to get local value of
                  ;; vc-checkout-switches.
                  (with-current-buffer filebuf
                    (vc-call find-revision file revision outbuf))))
              (setq failed nil)
              (when (and failed (file-exists-p filename))
                (delete-file filename)))))))
    filename))

(defun diff-hl-diff-buffer-with-head ()
  "View the differences between BUFFER and its associated file.
This requires the external program `diff' to be in your `exec-path'."
  (interactive)
  (vc-ensure-vc-buffer)
  (with-current-buffer (get-buffer (current-buffer))
    (let ((rev (diff-hl-create-revision
                 buffer-file-name
                 (vc-working-revision buffer-file-name
                   (vc-responsible-backend buffer-file-name)
                   t)))
           (temporary-file-directory
             (if (file-directory-p "/dev/shm/")
               "/dev/shm/"
               temporary-file-directory)))
      (if (called-interactively-p 'any)
        (diff rev (current-buffer) "-U 0" 'noasync)
        (diff-no-select rev (current-buffer) "-U 0" 'noasync
          (get-buffer-create " *diff-hl-diff*"))))))

(with-eval-after-load 'vc
  (defadvice vc-working-revision
    (around concrete-revision (file &optional backend concrete) activate preactivate compile)
    (setq ad-return-value
      (if concrete
        (vc-call-backend backend 'working-revision file t)
        (or (vc-file-getprop file 'vc-working-revision)
          (progn
            (setq backend (or backend (vc-responsible-backend file)))
            (when backend
              (vc-file-setprop file 'vc-working-revision
                (vc-call-backend backend 'working-revision file)))))))))

(with-eval-after-load 'vc-git
  (defadvice vc-git-working-revision
    (around use-hashes-only (file &optional concrete) activate preactivate compile)
    "Git-specific version of `vc-working-revision'."
    (if concrete
      (setq ad-return-value (vc-git--rev-parse ad-do-it))
      ad-do-it)))

(with-eval-after-load 'diff-hl
  (defvar diff-hl-modified-tick 0)
  (make-variable-buffer-local 'diff-hl-modified-tick)

  (setq diff-hl-draw-borders nil)

  (defadvice diff-hl-update
    (around flydiff activate preactivate compile)
    (unless (= diff-hl-modified-tick (buffer-modified-tick))
      ad-do-it))

  (defadvice diff-hl-changes
    (around flydiff activate preactivate compile)
    (setq ad-return-value
      (let* ((file buffer-file-name)
              (backend (vc-backend file)))
        (when backend
          (let ((state (vc-state file backend)))
            (cond
              ((or
                 (buffer-modified-p)
                 (eq state 'edited)
                 (and (eq state 'up-to-date)
                   ;; VC state is stale in after-revert-hook.
                   (or revert-buffer-in-progress-p
                     ;; Diffing against an older revision.
                     diff-hl-reference-revision)))
                (let (diff-auto-refine-mode res)
                  (with-current-buffer (diff-hl-diff-buffer-with-head)
                    (goto-char (point-min))
                    (unless (eobp)
                      (ignore-errors
                        (diff-beginning-of-hunk t))
                      (while (looking-at diff-hunk-header-re-unified)
                        (let ((line (string-to-number (match-string 3)))
                               (len (let ((m (match-string 4)))
                                      (if m (string-to-number m) 1)))
                               (beg (point)))
                          (diff-end-of-hunk)
                          (let* ((inserts (diff-count-matches "^\\+" beg (point)))
                                  (deletes (diff-count-matches "^-" beg (point)))
                                  (type (cond ((zerop deletes) 'insert)
                                          ((zerop inserts) 'delete)
                                          (t 'change))))
                            (when (eq type 'delete)
                              (setq len 1)
                              (cl-incf line))
                            (push (list line len type) res))))))
                  (setq diff-hl-modified-tick (buffer-modified-tick))
                  (nreverse res)))
              ((eq state 'added)
                `((1 ,(line-number-at-pos (point-max)) insert)))
              ((eq state 'removed)
                `((1 ,(line-number-at-pos (point-max)) delete)))))))))

  (defadvice diff-hl-overlay-modified
    (around preserve-overlays activate preactivate compile))

  (add-hook 'diff-hl-mode-hook
    (lambda ()
      (remove-hook 'after-change-functions #'diff-hl-edit t)))

  (run-with-idle-timer 0.3 t #'diff-hl-update))

(setq magit-last-seen-setup-instructions "1.4.0")
(with-eval-after-load 'magit
  (diminish 'magit-auto-revert-mode)
  (setq magit-completing-read-function
    #'magit-ido-completing-read)

  (evil-set-initial-state 'magit-status-mode 'insert)
  (evil-set-initial-state 'magit-log-mode 'insert)

  (define-key magit-log-mode-map (kbd "k") #'previous-line)
  (define-key magit-log-mode-map (kbd "j") #'next-line)
  (define-key magit-status-mode-map (kbd "k") #'previous-line)
  (define-key magit-status-mode-map (kbd "K") #'magit-discard-item)
  (define-key magit-status-mode-map (kbd "j") #'next-line)

  (unless (version< emacs-version "24.4")
    (require 'magit-filenotify)
    (diminish 'magit-filenotify-mode))

  ;; disable regular key chords by switching input methods
  (add-hook 'magit-status-mode-hook
    (lambda ()
      (magit-filenotify-mode +1)
      (set-input-method "TeX")))

  (add-hook 'magit-log-mode-hook
    (lambda () (set-input-method "TeX"))))

;; and psvn for svn not-so-awesomeness
(with-eval-after-load 'psvn
  (evil-set-initial-state 'svn-status-mode 'insert)
  (setq svn-status-verbose nil))

(global-set-key (kbd "C-c C-c s") #'svn-status)

(with-eval-after-load 'projectile
  (require 'magit)
  (setq projectile-mode-line
    '(:eval (format (if (display-graphic-p) " ↠" " pro")))))

(cl-macrolet
  ((define-temp-projectile-binding (key func)
     `(progn
        (autoload ,func "projectile")
        (global-set-key ,(kbd (concat "C-c p " key)) ,func))))

  (with-no-warnings
    (generate-calls define-temp-projectile-binding
      (
        ("4 a" #'projectile-find-other-file-other-window)
        ("4 b" #'projectile-switch-to-buffer-other-window)
        ("4 C-o" #'projectile-display-buffer)
        ("4 d" #'projectile-find-dir-other-window)
        ("4 f" #'projectile-find-file-other-window)
        ("4 g" #'projectile-find-file-dwim-other-window)
        ("4 t" #'projectile-find-implementation-or-test-other-window)
        ("!" #'projectile-run-shell-command-in-root)
        ("&" #'projectile-run-async-shell-command-in-root)
        ("a" #'projectile-find-other-file)
        ("b" #'projectile-switch-to-buffer)
        ("c" #'projectile-compile-project)
        ("d" #'projectile-find-dir)
        ("D" #'projectile-dired)
        ("e" #'projectile-recentf)
        ("f" #'projectile-find-file)
        ("g" #'projectile-find-file-dwim)
        ("F" #'projectile-find-file-in-known-projects)
        ("i" #'projectile-invalidate-cache)
        ("I" #'projectile-ibuffer)
        ("j" #'projectile-find-tag)
        ("k" #'projectile-kill-buffers)
        ("l" #'projectile-find-file-in-directory)
        ("m" #'projectile-commander)
        ("o" #'projectile-multi-occur)
        ("p" #'projectile-switch-project)
        ("P" #'projectile-test-project)
        ("r" #'projectile-replace)
        ("R" #'projectile-regenerate-tags)
        ("s a" #'helm-projectile-ack)
        ("s g" #'projectile-grep)
        ("s s" #'helm-projectile-ag)
        ("S" #'projectile-save-project-buffers)
        ("t" #'projectile-toggle-between-implementation-and-test)
        ("T" #'projectile-find-test-file)
        ("v" #'projectile-vc)
        ("z" #'projectile-cache-current-file)
        ("ESC" #'projectile-project-buffers-other-buffer)))))

(provide 'config-vcs)
