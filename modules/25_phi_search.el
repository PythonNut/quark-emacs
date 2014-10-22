;;; ==========
;;; Phi search
;;; ==========
(autoload 'phi-search "phi-search")
(autoload 'phi-search-backward "phi-search")
(autoload 'phi-replace-query "phi-replace")

(setq
  phi-search-case-sensitive  'guess
  phi-replace-case-sensitive 'guess)

(global-set-key (kbd "C-s") 'phi-search)
(global-set-key (kbd "C-r") 'phi-search-backward)
(global-set-key (kbd "M-%") 'phi-replace-query)

(defadvice phi-search (after kill-gratio activate)
  (progn
    (evil-insert-state)
    (define-key evil-insert-state-map (kbd "RET") 'phi-search-complete)
    (shrink-window (window-total-height))))

(defadvice phi-replace--initialize (after kill-gratio activate)
  (progn
    (evil-emacs-state)
    (define-key evil-insert-state-map (kbd "RET") 'phi-search-complete)
    (shrink-window (window-total-height))))

(defadvice phi-search-complete (after restart-gratio activate)
  (progn
    (define-key evil-insert-state-map (kbd "RET") 'smart-newline)))

(defadvice phi-replace-complete (after restart-gratio activate)
  (progn
    (define-key evil-insert-state-map (kbd "RET") 'smart-newline)))

(eval-after-load 'phi-search
  '(progn
     (define-key phi-search-default-map (kbd "RET") 'phi-search-complete)))

