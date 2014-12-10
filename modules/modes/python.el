(eval-when-compile (require 'jedi))

(add-hook 'python-mode-hook 'jedi:setup)

(with-eval-after-load 'python-mode
  (setq
    jedi:use-shortcuts t
    jedi:install-imenu t))
