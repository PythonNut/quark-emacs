(eval-when-compile (require 'cl))

(add-hook 'auto-indent-global-mode-hook
  (lambda ()
    (diminish 'auto-indent-mode " ⇉")))

(auto-indent-global-mode +1)
(add-hook 'find-file-hook 'dtrt-indent-mode)
