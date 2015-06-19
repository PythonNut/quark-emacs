(eval-when-compile
  (with-demoted-errors
    (require 'js2-mode)
    (require 'js2-refactor)))

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(with-eval-after-load 'js2-mode
  (set-face-foreground 'js2-external-variable
    (face-foreground 'default))
  (set-face-attribute 'js2-external-variable nil :weight 'extra-bold)
  (set-face-attribute 'js2-external-variable nil :underline t)
  (js2r-add-keybindings-with-prefix "C-c C-r")
  (setq
    js2-basic-offset 2
    js2-bounce-indent-p t))
