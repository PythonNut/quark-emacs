(require 'cl)
(require 'cl-lib)

(add-to-list 'load-path
	     (concat user-emacs-directory "modules/"))

(load-library "config-setq.el")
(load-library "config-package.el")
(load-library "config-ido.el")
