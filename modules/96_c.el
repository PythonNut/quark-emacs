(add-hook 'c++-mode-hook
  '(lambda ()
     (defun my-c++-compile ()
       (interactive)
       (let ((executable (file-name-sans-extension (buffer-file-name))))
         (compile (concat "g++ -o " executable " " (buffer-file-name) "; " executable))))

     (require 'disaster)
     (define-key c-mode-base-map (kbd "C-c d") 'disaster)

     (global-set-key (kbd "<f5>") 'my-c++-compile)
     (define-key evil-insert-state-map (kbd "<f5>") 'my-c++-compile)
     (define-key evil-normal-state-map (kbd "<f5>") 'my-c++-compile)
     (define-key evil-emacs-state-map (kbd "<f5>") 'my-c++-compile)))

(add-hook 'c-mode-hook
  '(lambda ()
     (defun my-c-compile ()
       (interactive)
       (let ((executable (file-name-sans-extension (buffer-file-name))))
         (compile (concat "gcc -o " executable " " (buffer-file-name) "; " executable))))

     (require 'disaster)
     (define-key c-mode-base-map (kbd "C-c d") 'disaster)

     (global-set-key (kbd "<f5>") 'my-c-compile)
     (define-key evil-insert-state-map (kbd "<f5>") 'my-c-compile)
     (define-key evil-normal-state-map (kbd "<f5>") 'my-c-compile)
     (define-key evil-emacs-state-map (kbd "<f5>") 'my-c-compile)))


