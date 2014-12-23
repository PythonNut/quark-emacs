(eval-when-compile
  (progn
    (require 'cl)
    (require 'cl-lib)
    (require 'diff-hl)
    (require 'git-gutter+)
    (require 'ispell)
    (require 'key-chord)
    (require 'config-modes)
    (require 'magit)
    (require 'projectile)
    (require 'projectile)
    (require 'psvn)))

(add-hook 'find-file-hook
  (lambda ()
    (git-gutter+-mode +1)
    (diff-hl-mode +1)
    (diff-hl-update)))

(with-eval-after-load 'git-gutter+
  ;; leave highlighting to diff-hl
  (diminish 'git-gutter+-mode)
  (setq git-gutter+-view-diff-function (lambda (&rest args))
    git-gutter+-clear-function (lambda (&rest args))
    git-gutter+-window-config-change-function nil))

;; (defun diff-buffer-with-file-unified ()
;;   "View the differences between BUFFER and its associated file.
;; This requires the external program `diff' to be in your `exec-path'."
;;   (interactive)
;;   (with-current-buffer (get-buffer (current-buffer))
;;     (diff-no-select buffer-file-name (current-buffer) "-U 0" 'noasync)))


(defun diff-hl-make-temp-file-name (file rev &optional manual)
  "Return a backup file name for REV or the current version of FILE.
If MANUAL is non-nil it means that a name for backups created by
the user should be returned."
  (let* ((diff-hl-temp-location
           (if (file-directory-p "/dev/shm/")
             "/dev/shm/" "/tmp/"))
          (auto-save-file-name-transforms
            `((".*" ,diff-hl-temp-location t))))
    (expand-file-name
      (concat (make-auto-save-file-name)
        ".~" (subst-char-in-string
               ?/ ?_ rev)
        (unless manual ".") "~")
      diff-hl-temp-location)))

(defun diff-hl-create-revision (file revision)
  "Read REVISION of FILE into a buffer and return the buffer.
Use BACKEND as the VC backend if specified."
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
    (diff-no-select
      (diff-hl-create-revision
        buffer-file-name
        (vc-working-revision buffer-file-name))
      (current-buffer)
      "-U 0" 'noasync
      (get-buffer-create " *diff-hl-diff*"))))

(with-eval-after-load 'diff-hl
  (setq diff-hl-draw-borders nil)
  (defun diff-hl-changes ()
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
                (nreverse res)))
            ((eq state 'added)
              `((1 ,(line-number-at-pos (point-max)) insert)))
            ((eq state 'removed)
              `((1 ,(line-number-at-pos (point-max)) delete))))))))
  ;; (add-hook 'post-command-hook #'diff-hl-update)
  (run-with-idle-timer 1 t #'diff-hl-update))

(with-eval-after-load 'magit
  (diminish 'magit-auto-revert-mode " ⥀")
  (setq magit-completing-read-function
    #'magit-ido-completing-read)
  (evil-set-initial-state 'magit-status-mode 'insert)
  (define-key magit-log-mode-map (kbd "k") #'previous-line)
  (define-key magit-log-mode-map (kbd "j") #'next-line)
  (define-key magit-status-mode-map (kbd "k") #'previous-line)
  (define-key magit-status-mode-map (kbd "K") #'magit-discard-item)
  (define-key magit-status-mode-map (kbd "j") #'next-line)

  ;; disable regular key chords by switching input methods
  (add-hook 'magit-status-mode-hook
    (lambda ()
      (set-input-method "TeX")
      (unless (version< emacs-version "24.4")
        (require 'magit-filenotify)
        (diminish 'magit-filenotify-mode)
        (magit-filenotify-mode +1)))))

(evil-leader/set-key
  "m" #'magit-key-mode-popup-dispatch)

(with-eval-after-load 'git-commit-mode
  (add-hook 'git-commit-mode-hook 'ac-ispell-ac-setup))

(with-eval-after-load 'ac-ispell
  (ac-ispell-setup)
  (set-face-foreground 'ac-ispell-fuzzy-candidate-face
    (face-foreground 'default))
  (setq ispell-alternate-dictionary "/usr/share/dict/words"))

;; and psvn for svn not-so-awesomeness

(with-eval-after-load 'psvn
  (evil-set-initial-state 'svn-status-mode 'insert)
  (setq svn-status-verbose nil))

(global-set-key (kbd "C-c C-c s") #'svn-status)

(with-eval-after-load 'projectile
  (require 'magit)
  (setq projectile-mode-line
    '(:eval (format " ↠"))))

(cl-macrolet
  ((define-temp-projectile-binding (key func)
     `(global-set-key ,(kbd (concat "C-c p " key)) ,func)))

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
      ("s a" #'projectile-ack)
      ("s g" #'projectile-grep)
      ("s s" #'projectile-ag)
      ("S" #'projectile-save-project-buffers)
      ("t" #'projectile-toggle-between-implementation-and-test)
      ("T" #'projectile-find-test-file)
      ("v" #'projectile-vc)
      ("z" #'projectile-cache-current-file)
      ("ESC" #'projectile-project-buffers-other-buffer))))

(provide 'config-vcs)
