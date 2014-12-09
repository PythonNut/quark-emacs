(eval-when-compile
  (progn
    (require 'cl)
    (require 'magit)
    (require 'psvn)))

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
      (require 'magit-filenotify)
      (diminish 'magit-filenotify-mode)
      (magit-filenotify-mode +1))))

;; and psvn for svn not-so-awesomeness

(with-eval-after-load 'psvn
  (evil-set-initial-state 'svn-status-mode 'insert)
  (setq svn-status-verbose nil))

(global-set-key (kbd "C-c C-c s") 'svn-status)

(provide 'config-vcs)
