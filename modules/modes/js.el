;;; ===============
;;; Javascript mode
;;; ===============
(add-hook 'js2-mode-hook 'ac-js2-mode)
;; (add-hook 'js3-mode-hook 'ac-js2-mode)
;; (autoload 'tern-mode "tern.el" nil t)
;; (add-hook 'js2-mode-hook 'tern-mode)
;; (add-hook 'js3-mode-hook 'tern-mode)
;; (add-to-list 'ac-modes 'js3-mode)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(setq tern-ac-on-dot t)
(setq ac-js2-evaluate-calls t)

(eval-after-load 'js2-mode
  '(progn
     (js2r-add-keybindings-with-prefix "C-c C-r")
     (setq
       js2-basic-offset 2
       js2-bounce-indent-p t)))

(eval-after-load 'tern
  '(progn
     (require 'tern-auto-complete)
     (tern-ac-setup)))


