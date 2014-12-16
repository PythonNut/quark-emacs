(ido-mode +1)
(ido-ubiquitous-mode +1)

(eval-when-compile
  (progn
    (require 'cl)
    (require 'cl-lib)
    (require 'ido)
    (require 'ido-ubiquitous)
    (require 'smex)
    (require 'imenu)))

(add-hook 'ido-minibuffer-setup-hook
  (lambda ()
    (flx-ido-mode +1)
    (setq
      ido-enable-flex-matching t
      ido-save-directory-list-file "~/.emacs.d/ido.last"
      ido-use-faces nil)
    (ido-vertical-mode +1)))

(autoload 'smex "smex" "smex" t)
(with-eval-after-load 'smex
  (setq smex-save-file
    (concat
      user-emacs-directory
      "smex-items")))

(global-set-key (kbd "M-x") #'smex)

(global-set-key (kbd "M-=") #'idomenu)

(global-set-key (kbd "C-M-s") #'flx-isearch-forward)
(global-set-key (kbd "C-M-r") #'flx-isearch-backward)

(provide 'config-ido)
