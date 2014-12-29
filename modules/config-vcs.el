(eval-when-compile
  (require 'cl)
  (require 'magit)
  (require 'diff-hl)
  (require 'git-gutter+)
  (require 'psvn))

(add-hook 'find-file-hook
  (lambda ()
    (diff-hl-mode +1)
    (diff-hl-update)))

(with-eval-after-load 'diff-hl
  (setq diff-hl-draw-borders nil))

(with-eval-after-load 'magit
  (when (display-graphic-p)
    (diminish 'magit-auto-revert-mode " ⥀"))
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
    '(:eval (format (if (display-graphic-p) " ↠" " pro")))))

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
