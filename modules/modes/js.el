(eval-when-compile
  (progn
    (require 'js2-mode)
    (require 'js2-refactor)
    (require 'ac-js2)))

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(add-hook 'js2-mode-hook 'ac-js2-mode)

(with-eval-after-load 'js2-mode
  (set-face-foreground 'js2-external-variable
    (face-foreground 'default))
  (set-face-attribute 'js2-external-variable nil :weight 'extra-bold)
  (set-face-attribute 'js2-external-variable nil :underline t)
  (js2r-add-keybindings-with-prefix "C-c C-r")
  (setq
    ac-js2-evaluate-calls t
    js2-basic-offset 2
    js2-bounce-indent-p t))
