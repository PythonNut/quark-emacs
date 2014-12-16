(eval-when-compile
  (progn
    (require 'js2-mode)
    (require 'js2-refactor)
    (require 'ac-js2)))

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(add-hook 'js2-mode-hook 'ac-js2-mode)

(with-eval-after-load 'js2-mode
  (js2r-add-keybindings-with-prefix "C-c C-r")
  (setq
    ac-js2-evaluate-calls t
    js2-basic-offset 2
    js2-bounce-indent-p t))
