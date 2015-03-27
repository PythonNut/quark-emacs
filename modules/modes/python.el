(eval-when-compile
  (with-demoted-errors
    (require 'evil)
    (require 'python)
    (require 'anaconda-mode)
    (require 'company-anaconda)))

(add-hook 'python-mode-hook
  (lambda ()
    (anaconda-mode +1)
    (eldoc-mode +1)
    (global-semantic-idle-summary-mode -1)

    (run-at-time 1 nil #'python-indent-guess-indent-offset)))

(with-eval-after-load 'python
  (setq python-indent-guess-indent-offset t)
  (evil-define-key 'normal python-mode-map "gd" #'anaconda-mode-goto)
  (define-key python-mode-map (kbd "M-.") #'anaconda-mode-goto))
