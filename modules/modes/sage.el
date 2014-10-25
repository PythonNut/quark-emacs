(autoload 'sage-mode "sage-mode")
(add-hook 'sage-mode-hook 'jedi:setup)
(add-to-list 'auto-mode-alist '("\\.sage\\'" . sage-mode))
