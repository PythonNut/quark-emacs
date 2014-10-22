;;; =============================
;;; Magit - fast, interactive git
;;; =============================
(global-set-key (kbd "C-c C-m") 'projectile-vc)

(evil-set-initial-state 'svn-status-mode 'insert)
(evil-set-initial-state 'magit-status-mode 'insert)

(setq magit-completing-read-function
  'magit-ido-completing-read)

(eval-after-load 'magit
  '(progn
     (raise-minor-mode-map-alist 'magit-status-mode-map)
     (define-key magit-log-mode-map (kbd "k") 'previous-line)
     (define-key magit-log-mode-map (kbd "j") 'next-line)
     (define-key magit-status-mode-map (kbd "k") 'previous-line)
     (define-key magit-status-mode-map (kbd "K") 'magit-discard-item)
     (define-key magit-status-mode-map (kbd "j") 'next-line)))

;; disable regular key chords by switching input methods
(add-hook 'magit-status-mode-hook
  '(lambda ()
     (set-input-method "TeX")))

;; and psvn for svn not-so-awesomeness
(autoload 'svn-status "psvn")

(eval-after-load 'psvn
  '(progn
     (setq svn-status-verbose nil)))

(global-set-key (kbd "C-c C-c s") 'svn-status)

