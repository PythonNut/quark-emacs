;; -*- lexical-binding: t -*-

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

(add-to-list 'auto-mode-alist '("\\.pyx\\'" . cython-mode))
(add-to-list 'auto-mode-alist '("\\.pxd\\'" . cython-mode))
(add-to-list 'auto-mode-alist '("\\.pxi\\'" . python-mode))

(evil-set-initial-state 'sage-shell-mode 'insert)

(add-hook 'sage-shell-mode-hook #'eldoc-mode)
(add-hook 'sage-shell:sage-mode-hook #'eldoc-mode)

(add-hook 'sage-shell-mode-hook
  (lambda () (semantic-idle-summary-mode -1)))

(add-hook 'sage-shell:sage-mode-hook
  (lambda () (semantic-idle-summary-mode -1)))
