(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
  '(custom-safe-themes
     (quote
       ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
  )

(add-to-list 'load-path
  (concat user-emacs-directory "modules/"))

(load-library "config-setq")
(load-library "config-package")
(load-library "config-modes")
(load-library "config-desktop")
(load-library "config-pre-bindings")
(load-library "config-safety")
(load-library "config-evil")
(load-library "config-ui")
(load-library "config-whitespace")
(load-library "config-paste")
(load-library "config-auto-complete")
(load-library "config-projects")
(load-library "config-minibuffer")
(load-library "config-intel")
(load-library "config-solarized")

(setq load-dirs (concat
                  user-emacs-directory
                  "modules/modes/"))
