;;; ==========
;;; Julia mode
;;; ==========
;; (require 'ess-site)
(add-hook 'julia-mode 'inferior-ess-mode)

(add-hook 'julia-mode-hook
  '(lambda ()
     (require 'comint)

     (defun my-julia-compile ()
       "Use compile to run python programs"
       (interactive)
       (compile (concat "julia -L " (buffer-file-name)) t))

     (setq compilation-scroll-output t)
     (global-set-key (kbd "<f5>") 'my-julia-compile)
     (define-key evil-insert-state-map (kbd "<f5>") 'my-julia-compile)
     (define-key evil-normal-state-map (kbd "<f5>") 'my-julia-compile)
     (define-key evil-emacs-state-map (kbd "<f5>") 'my-julia-compile)))

(add-hook 'comint-exec-hook
  (lambda () (set-process-query-on-exit-flag (get-buffer-process (current-buffer)) nil)))

