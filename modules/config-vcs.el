(eval-when-compile
  (progn
    (require 'cl)
    (require 'magit)
    (require 'diff-hl)
    (require 'git-gutter+)
    (require 'psvn)))

(add-hook 'find-file-hook
  (lambda ()
    (git-gutter+-mode +1)
    (diff-hl-mode +1)
    (diff-hl-update)))

(with-eval-after-load 'git-gutter+
  ;; leave highlighting to diff-hl
  (progn
    (setq git-gutter+-view-diff-function (lambda (&rest args))
      git-gutter+-clear-function (lambda (&rest args))
      git-gutter+-window-config-change-function nil)))

(with-eval-after-load 'diff-hl
  (setq diff-hl-draw-borders nil))

(with-eval-after-load 'magit
  (diminish 'magit-auto-revert-mode " ⥀")
  (setq magit-completing-read-function
    'magit-ido-completing-read)
  (evil-set-initial-state 'magit-status-mode 'insert)
  (define-key magit-log-mode-map (kbd "k") 'previous-line)
  (define-key magit-log-mode-map (kbd "j") 'next-line)
  (define-key magit-status-mode-map (kbd "k") 'previous-line)
  (define-key magit-status-mode-map (kbd "K") 'magit-discard-item)
  (define-key magit-status-mode-map (kbd "j") 'next-line)

  ;; disable regular key chords by switching input methods
  (add-hook 'magit-status-mode-hook
    (lambda ()
      (set-input-method "TeX")
      (unless (version< emacs-version "24.4")
        (require 'magit-filenotify)
        (diminish 'magit-filenotify-mode)
        (magit-filenotify-mode +1)))))

;; and psvn for svn not-so-awesomeness

(with-eval-after-load 'psvn
  (evil-set-initial-state 'svn-status-mode 'insert)
  (setq svn-status-verbose nil))

(global-set-key (kbd "C-c C-c s") 'svn-status)

(provide 'config-vcs)
