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
       ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
  '(flycheck-error ((t (:inherit nil :underline "#dc322f"))))
  '(flycheck-info ((t (:inherit nil :underline "#268bd2"))))
  '(flycheck-warning ((t (:inherit nil :underline "#b58900")))))

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
