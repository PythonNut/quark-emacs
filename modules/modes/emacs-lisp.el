(setq
 lisp-indent-offset 2
 lisp-body-indent 2)

(add-hook 'emacs-lisp-mode-hook
  '(lambda ()
     (aggressive-indent-mode +1)
     (require 'auto-async-byte-compile)
     (auto-async-byte-compile-mode +1)))
