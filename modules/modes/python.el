(eval-when-compile
  (with-demoted-errors
    (require 'evil)
    (require 'python)
    (require 'anaconda-mode)
    (require 'company-anaconda)))

(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'eldoc-mode)

(add-hook 'python-mode-hook
  (lambda ()
    ;; conflicts with `eldoc-mode'
    (semantic-idle-summary-mode -1)
    (setq mode-name "Py")))

(with-eval-after-load 'python
  (evil-define-key 'normal python-mode-map "gd" #'anaconda-mode-goto)
  (define-key python-mode-map (kbd "M-.") #'anaconda-mode-goto))

(with-eval-after-load 'anaconda-mode
  (diminish 'anaconda-mode " ✶"))

(autoload #'cython-mode "cython-mode")
(add-to-list 'auto-mode-alist '("\\.pyx\\'" . cython-mode))
(add-to-list 'auto-mode-alist '("\\.pxd\\'" . cython-mode))
(add-to-list 'auto-mode-alist '("\\.pxi\\'" . python-mode))

(with-eval-after-load 'sage-mode
  (require 'sage))

(autoload #'sage-mode "sage-mode")
(add-to-list 'auto-mode-alist '("\\.sage\\'" . sage-mode))
