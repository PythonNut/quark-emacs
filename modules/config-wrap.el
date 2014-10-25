(require 'adaptive-wrap)
(add-hook 'visual-line-mode-hook 'adaptive-wrap-prefix-mode)
(global-visual-line-mode +1)
(setq line-move-visual t)
