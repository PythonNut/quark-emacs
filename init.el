(require 'cl)
(require 'cl-lib)

(add-to-list 'load-path
  (concat user-emacs-directory "modules/"))

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
 '(flycheck-error ((((supports :underline (:style wave)) (class color) (min-colors 89)) (:underline (:style wave :color "#dc322f") :inherit unspecified)) (((class color) (min-colors 89)) (:foreground "#990A1B" :background "#FF6E64" :weight bold :underline t))))
 '(flycheck-info ((((supports :underline (:style wave)) (class color) (min-colors 89)) (:underline (:style wave :color "#268bd2") :inherit unspecified)) (((class color) (min-colors 89)) (:foreground "#00629D" :background "#69B7F0" :weight bold :underline t))))
 '(flycheck-warning ((((supports :underline (:style wave)) (class color) (min-colors 89)) (:underline (:style wave :color "#b58900") :inherit unspecified)) (((class color) (min-colors 89)) (:foreground "#7B6000" :background "#DEB542" :weight bold :underline t)))))

(load-library "config-setq.el")
(load-library "config-modes.el")
(load-library "config-package.el")
(load-library "config-desktop.el")
(load-library "config-pre-bindings.el")
(load-library "config-safety.el")
(load-library "config-evil.el")
(load-library "config-ui.el")
(load-library "config-whitespace.el")
(load-library "config-paste.el")
(load-library "config-auto-complete")
(load-library "config-projects.el")
(load-library "config-minibuffer.el")
(load-library "config-intel.el")
(load-library "config-solarized.el")

(setq load-dirs (concat
		  user-emacs-directory
		  "modules/modes/"))
(load-dirs)
