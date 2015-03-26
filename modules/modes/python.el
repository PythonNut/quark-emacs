(eval-when-compile
  (with-demoted-errors
    (require 'evil)
    (require 'anaconda-mode)
    (require 'company-anaconda)))

(add-hook 'python-mode-hook
  (lambda ()
    (anaconda-mode +1)
    (eldoc-mode +1)
    (run-at-time 1 nil #'python-indent-guess-indent-offset)
    (local-set-key (kbd "M-.") #'anaconda-mode-goto)
    (evil-define-key 'normal (current-local-map) "gd" #'anaconda-mode-goto)))

(with-eval-after-load 'python
  (setq python-indent-guess-indent-offset t))
