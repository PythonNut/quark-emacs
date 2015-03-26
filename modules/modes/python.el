(eval-when-compile
  (with-demoted-errors
    (require 'evil)))

(add-hook 'python-mode-hook
  (lambda ()
    (run-at-time 1 nil #'python-indent-guess-indent-offset)))

(with-eval-after-load 'python
  (setq python-indent-guess-indent-offset t))
