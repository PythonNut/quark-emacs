;;; ===========
;;; Cython mode
;;; ===========
;; (require 'cython-mode)
(autoload 'cython-mode "cython-mode")
(autoload 'sage-mode "sage-mode")
(add-hook 'sage-mode-hook 'jedi:setup)
(add-to-list 'auto-mode-alist '("\\.pyx\\'" . cython-mode))
(add-to-list 'auto-mode-alist '("\\.pxd\\'" . cython-mode))
(add-to-list 'auto-mode-alist '("\\.pxi\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.sage\\'" . sage-mode))
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))

