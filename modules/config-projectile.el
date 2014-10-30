(projectile-global-mode +1)
(eval-when-compile (require 'cl))

(eval-after-load 'projectile
  '(progn
     (setq projectile-mode-line
       '(:eval (format " [%s]" (projectile-project-name))))))
