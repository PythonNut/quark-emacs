(projectile-global-mode +1)

(eval-when-compile (require 'cl))

(with-eval-after-load 'projectile
  (progn
    (setq projectile-mode-line
      '(:eval (format " ↠")))))
