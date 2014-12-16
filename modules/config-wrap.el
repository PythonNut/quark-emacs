(require 'adaptive-wrap)
(add-hook 'visual-line-mode-hook 'adaptive-wrap-prefix-mode)
(global-visual-line-mode +1)
(setq
  line-move-visual t)

(add-hook 'visual-line-mode-hook
  (lambda ()
    (setq adaptive-wrap-extra-indent 2)
    (diminish 'visual-line-mode)))

(provide 'config-wrap)
