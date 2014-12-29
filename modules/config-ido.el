(eval-when-compile
  (require 'ido)
  (require 'idomenu)
  (require 'ido-vertical-mode)
  (require 'flx-isearch)
  (require 'ido-ubiquitous)
  (require 'smex))

(ido-mode +1)
(ido-ubiquitous-mode +1)

(with-eval-after-load 'ido
  (setq
    ido-enable-flex-matching t
    ido-save-directory-list-file "~/.emacs.d/ido.last"
    ido-use-faces nil))

(defun ido-onetime-setup ()
  (unless (featurep 'flx-ido)
    (flx-ido-mode +1))
  (unless (featurep 'ido-vertical-mode)
    (ido-vertical-mode +1))

  (remove-hook 'ido-minibuffer-setup-hook 'ido-onetime-setup))

(add-hook 'ido-minibuffer-setup-hook 'ido-onetime-setup)

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
