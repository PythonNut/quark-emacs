(eval-when-compile
  (with-demoted-errors
    (require 'magit)
    (require 'diff-hl)))

(add-hook 'find-file-hook
  (lambda ()
    (diff-hl-mode +1)
    (diff-hl-update)))

(defun diff-hl-make-temp-file-name (file rev &optional manual)
  "Return a backup file name for REV or the current version of FILE.
If MANUAL is non-nil it means that a name for backups created by
the user should be returned."
  (let* ((auto-save-file-name-transforms
           `((".*" ,temporary-file-directory t))))
    (expand-file-name
      (concat (make-auto-save-file-name)
        ".~" (subst-char-in-string
               ?/ ?_ rev)
        (unless manual ".") "~")
      temporary-file-directory)))

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
      (diff-no-select rev (current-buffer) "-U 0" 'noasync
        (get-buffer-create " *diff-hl-diff*")))))

(with-eval-after-load 'vc
  (defun nadvice/vc-working-revision (file &optional backend concrete)
    (if concrete
      (vc-call-backend backend 'working-revision file t)
      (or (vc-file-getprop file 'vc-working-revision)
        (progn
          (setq backend (or backend (vc-responsible-backend file)))
          (when backend
            (vc-file-setprop file 'vc-working-revision
              (vc-call-backend backend 'working-revision file)))))))
  (advice-add #'vc-working-revision :override #'nadvice/vc-working-revision))

(with-eval-after-load 'vc-git
  (defun nadvice/vc-git-working-revision (old-fun file &optional concrete)
    "Git-specific version of `vc-working-revision'."
    (if concrete
      (vc-git--rev-parse (funcall old-fun file))
      (funcall old-fun file)))

  (advice-add #'vc-git-working-revision :around
    #'nadvice/vc-git-working-revision))

(with-eval-after-load 'diff-hl
  (defvar diff-hl-modified-tick 0)
  (make-variable-buffer-local 'diff-hl-modified-tick)

  (setq diff-hl-draw-borders nil)

  (defun nadvice/diff-hl-update (old-fun &optional auto)
    (unless (and auto
              (or
                (= diff-hl-modified-tick (buffer-modified-tick))
                (file-remote-p default-directory)
                (not (buffer-modified-p))))
      (funcall old-fun)))

  (defun nadvice/diff-hl-changes (&rest args)
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
              `((1 ,(line-number-at-pos (point-max)) delete))))))))

  (defun nadvice/diff-hl-overlay-modified (&rest args))

  (advice-add #'diff-hl-update :around #'nadvice/diff-hl-update)
  (advice-add #'diff-hl-changes :override #'nadvice/diff-hl-changes)
  (advice-add #'diff-hl-overlay-modified :override
    #'nadvice/diff-hl-overlay-modified)

  (add-hook 'diff-hl-mode-hook
    (lambda ()
      (remove-hook 'after-change-functions #'diff-hl-edit t)))

  (run-with-idle-timer 0.3 t #'diff-hl-update t))

(setq magit-last-seen-setup-instructions "1.4.0")
(with-eval-after-load 'magit
  (setq
    magit-push-always-verify nil
    magit-completing-read-function #'magit-ido-completing-read
    magit-diff-refine-hunk t)

  (evil-set-initial-state 'magit-status-mode 'insert)
  (evil-set-initial-state 'magit-log-mode 'insert)
  (evil-set-initial-state 'magit-popup-mode 'insert)

  (define-key magit-log-mode-map (kbd "j") #'next-line)
  (define-key magit-status-mode-map (kbd "j") #'next-line)
  (eval-and-compile
    (cl-macrolet
      ((magit-setup-section-k (mode &optional command)
         `(with-demoted-errors
            (define-key ,mode (kbd "<C-tab>") nil)
            (define-key ,mode (kbd "<S-tab>") #'magit-section-cycle)
            (define-key ,mode (kbd "<backtab>") #'magit-section-cycle)
            (define-key ,mode (kbd "k") #'previous-line)
            ,(when command
               `(define-key ,mode (kbd "K") ,command)))))

      (with-no-warnings
        (my/generate-calls magit-setup-section-k
          (
            (magit-branch-section-map #'magit-branch-delete)
            (magit-commit-section-map)
            (magit-file-section-map #'magit-discard)
            (magit-hunk-section-map #'magit-discard)
            (magit-log-mode-map)
            (magit-module-commit-section-map)
            (magit-remote-section-map)
            (magit-staged-section-map #'magit-discard)
            (magit-stash-section-map #'magit-stash-drop)
            (magit-stashes-section-map)
            (magit-status-mode-map)
            (magit-tag-section-map #'magit-tag-delete)
            (magit-unpulled-section-map)
            (magit-unpushed-section-map)
            (magit-unstaged-section-map #'magit-discard)
            (magit-untracked-section-map #'magit-discard))))))

  (with-eval-after-load 'magit-filenotify
    (diminish 'magit-filenotify-mode))

  ;; disable regular key chords by switching input methods
  (add-hook 'magit-status-mode-hook
    (lambda ()
      (magit-filenotify-mode +1)
      (set-input-method "TeX")))

  (add-hook 'with-editor-mode-hook 'evil-insert-state)

  (add-hook 'magit-log-mode-hook
    (lambda () (set-input-method "TeX"))))

(global-set-key (kbd "C-c C-c s") #'svn-status)

(with-eval-after-load 'projectile
  (projectile-global-mode +1)
  (require 'magit)
  (setq projectile-mode-line
    '(:eval (format (if (display-graphic-p) " ↠" " pro"))))
  (define-key projectile-mode-map (kbd "C-c p") #'my/smart-projectile-tools))

(defun my/smart-projectile-tools ()
  (interactive)
  (unless (fboundp 'hydra/projectile-tools/body)
    (require 'hydra)
    (defhydra hydra/projectile-tools (:color blue :hint nil :idle 0.3)
      "
Find^^             Operate on project^^      Other window
_f_ file           _c_ compile project       _O f_ file
_d_ dir            _R_ regenerate tags       _O d_ dir
_g_ file dwim      _S_ save project          _O g_ file dwim
_a_ other file     ^!^ command in /          _O a_ other file
_l_ file in dir    ^&^ async command in /    _O b_ switch buffer
_T_ test file      _z_ cache current file    _O t_ implementation←→test
_j_ tag            _i_ invalidate cache      _O C-o_ display buffer

Special^^       Buffers^^                    Search and replace
_D_ dired       _b_ switch buffer            _o_ multi-occur
_e_ recentf     _k_ kill buffers             _r_ replace
_I_ ibuffer     _p_ switch project           _s g_ grep
_v_ vc          _ESC_ project other buffer   _s s_ ag
_m_ commander   _F_ file any project         _s a_ ack

Tests   _P_ test-project    _t_ toggle implementation←→test"
      ("O a" projectile-find-other-file-other-window)
      ("O b" projectile-switch-to-buffer-other-window)
      ("O C-o" projectile-display-buffer)
      ("O d" projectile-find-dir-other-window)
      ("O f" projectile-find-file-other-window)
      ("O g" projectile-find-file-dwim-other-window)
      ("O t" projectile-find-implementation-or-test-other-window)
      ("!" projectile-run-shell-command-in-root)
      ("&" projectile-run-async-shell-command-in-root)
      ("a" projectile-find-other-file)
      ("b" projectile-switch-to-buffer)
      ("c" projectile-compile-project)
      ("d" projectile-find-dir)
      ("D" projectile-dired)
      ("e" projectile-recentf)
      ("f" projectile-find-file)
      ("g" projectile-find-file-dwim)
      ("F" projectile-find-file-in-known-projects)
      ("i" projectile-invalidate-cache)
      ("I" projectile-ibuffer)
      ("j" projectile-find-tag)
      ("k" projectile-kill-buffers)
      ("l" projectile-find-file-in-directory)
      ("m" projectile-commander)
      ("o" projectile-multi-occur)
      ("p" projectile-switch-project)
      ("P" projectile-test-project)
      ("r" projectile-replace)
      ("R" projectile-regenerate-tags)
      ("s a" helm-projectile-ack)
      ("s g" projectile-grep)
      ("s s" helm-projectile-ag)
      ("S" projectile-save-project-buffers)
      ("t" projectile-toggle-between-implementation-and-test)
      ("T" projectile-find-test-file)
      ("v" projectile-vc)
      ("z" projectile-cache-current-file)
      ("ESC" projectile-project-buffers-other-buffer)))

  (hydra/projectile-tools/body))

(global-set-key (kbd "C-c p") #'my/smart-projectile-tools)

(provide 'config-vcs)
