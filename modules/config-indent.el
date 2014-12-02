(eval-when-compile (require 'cl))
(eval-when-compile (require 'auto-indent-mode))
(eval-when-compile (require 'smie))

(add-hook 'auto-indent-global-mode-hook
  (lambda ()
    (diminish 'auto-indent-mode " ⇉")))

(auto-indent-global-mode +1)
(add-hook 'find-file-hook 'dtrt-indent-mode)

(with-eval-after-load 'smie
  (add-hook 'change-major-mode-hook
    (lambda ()
      (unless (eq smie-grammar 'unset)
        (smie-config-guess)))))
