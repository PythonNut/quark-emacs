(eval-when-compile (require 'cl))

(add-hook 'auto-indent-global-mode-hook
  (lambda ()
    (diminish 'auto-indent-mode " ⇉")))

(auto-indent-global-mode +1)
(dtrt-indent-mode +1)
