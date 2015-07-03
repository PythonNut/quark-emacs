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
    (add-hook 'before-save-hook #'auto-compile-onetime-setup nil t)))

(with-eval-after-load 'smartparens
  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
  (sp-local-pair 'emacs-lisp-mode "`" nil :when '(sp-in-string-p)))

(with-eval-after-load 'aggressive-indent
  (diminish 'aggressive-indent-mode (if (display-graphic-p) " ⇶" " *→")))

(with-eval-after-load 'elisp-mode
  (define-key emacs-lisp-mode-map (kbd "C-c e") #'replace-last-sexp)
  (define-key emacs-lisp-mode-map (kbd "M-.") #'emacs-lisp-goto-definition)
  (define-key emacs-lisp-mode-map (kbd "M-,") #'evil-jump-backward)

  (evil-define-key 'normal emacs-lisp-mode-map "gd"
    #'emacs-lisp-goto-definition))
