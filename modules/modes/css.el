;;; ========
;;; CSS mode
;;; ========
(add-hook 'css-mode-hook 'rainbow-mode)
(sp-local-pair 'css-mode ":" ";")
(add-hook 'scss-mode-hook 'auto-complete-mode)
(add-hook 'scss-mode-hook 'flyspell-prog-mode)

