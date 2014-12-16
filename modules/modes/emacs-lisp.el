(eval-when-compile (require 'evil))

(defun emacs-lisp-goto-definition ()
  (interactive)
  (find-function (function-called-at-point)))

(add-hook 'emacs-lisp-mode-hook
  (lambda ()
    (setq
      lisp-indent-offset 2
      lisp-body-indent 2
      mode-name "λ")

    (aggressive-indent-mode +1)
    (require 'auto-async-byte-compile)
    (auto-async-byte-compile-mode +1)

    (local-set-key (kbd "M-.") #'emacs-lisp-goto-definition)
    (local-set-key (kbd "M-,") #'evil-jump-backward)
    (evil-define-key 'normal (current-local-map) "gd" #'emacs-lisp-goto-definition)))

(with-eval-after-load 'aggressive-indent
  (diminish 'aggressive-indent-mode " ⇶"))
