;; -*- lexical-binding: t -*-

(eval-when-compile
  (with-demoted-errors "Load error: %s"
    (require 'evil)
    (require 'python)
    (require 'anaconda-mode)
    (require 'company-anaconda)))

(add-hook 'python-mode-hook #'anaconda-mode)
(add-hook 'python-mode-hook #'eldoc-mode)

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

(autoload 'run-sage "sage-shell-mode" nil t)
(autoload 'run-new-sage "sage-shell-mode" nil t)
(autoload 'sage-mode "sage-shell-mode" nil t)

(with-eval-after-load 'sage-shell-mode
  (sage-shell:define-alias)
  (evil-set-initial-state 'sage-shell-mode 'insert)

  (add-hook 'sage-shell-mode-hook #'eldoc-mode)
  (add-hook 'sage-mode-hook #'eldoc-mode)

  (add-hook 'sage-shell-mode-hook
            (lambda () (semantic-idle-summary-mode -1)))

  (add-hook 'sage-mode-hook
            (lambda () (semantic-idle-summary-mode -1))))
