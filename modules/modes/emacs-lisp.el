(eval-when-compile
  (with-demoted-errors
    (require 'evil)))

(defun emacs-lisp-goto-definition ()
  (interactive)
  (find-function (function-called-at-point)))

(defun replace-last-sexp ()
  (interactive)
  (let ((value (eval (preceding-sexp))))
    (kill-sexp -1)
    (insert (format "%S" value))))

(defun auto-compile-onetime-setup ()
  (require 'auto-compile)
  (auto-compile-on-save-mode +1)
  (remove-hook 'before-save-hook #'auto-compile-onetime-setup t))

(add-hook 'emacs-lisp-mode-hook
  (lambda ()
    (setq
      lisp-indent-offset 2
      lisp-body-indent 2
      mode-name (if (display-graphic-p) "λ" "EL"))

    (auto-indent-mode -1)
    (aggressive-indent-mode +1)
    (add-hook 'before-save-hook #'auto-compile-onetime-setup nil t)
    (local-set-key (kbd "C-c e") 'replace-last-sexp)
    (local-set-key (kbd "M-.") #'emacs-lisp-goto-definition)
    (local-set-key (kbd "M-,") #'evil-jump-backward)
    (evil-define-key 'normal (current-local-map) "gd" #'emacs-lisp-goto-definition)))

(with-eval-after-load 'aggressive-indent
  (diminish 'aggressive-indent-mode (if (display-graphic-p) " ⇶" " *→")))
