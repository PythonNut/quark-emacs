;; -*- lexical-binding: t -*-

(eval-when-compile
  (with-demoted-errors
    (require 'magit)
    (require 'diff-hl)))

(unless my/slow-device
  (add-hook 'find-file-hook
    (lambda ()
      (when (display-graphic-p)
        (diff-hl-mode +1)
        (diff-hl-update)))))

(with-eval-after-load 'diff-hl
  (setq diff-hl-draw-borders nil)
  (diff-hl-flydiff-mode +1))

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
    '(:eval (format (if (display-graphic-p) " ↠" " /"))))
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
