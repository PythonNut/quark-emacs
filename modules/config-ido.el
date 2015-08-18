(eval-when-compile
  (with-demoted-errors
    (require 'ido)
    (require 'idomenu)
    (require 'ido-vertical-mode)
    (require 'flx-isearch)
    (require 'ido-ubiquitous)
    (require 'smex)))

(setq
  ido-enable-flex-matching t
  ido-save-directory-list-file (concat user-emacs-directory "ido.last")
  ido-use-faces nil)

(ido-mode +1)

(with-eval-after-load 'ido-ubiquitous
  (ido-ubiquitous-mode +1))

(defun nadvice/completing-read-ido (old-fun &rest args)
  (require 'ido-ubiquitous)
  (advice-remove #'completing-read #'nadvice/completing-read-ido))

(advice-add 'completing-read :before #'nadvice/completing-read-ido)

(global-set-key (kbd "C-x b") #'ido-switch-buffer)
(global-set-key (kbd "C-x f") #'ido-find-file)

(defun ido-onetime-setup ()
  (unless (and
            (featurep 'flx-ido)
            flx-ido-mode)
    (flx-ido-mode +1))
  (unless (and
            (featurep 'ido-vertical-mode)
            ido-vertical-mode)
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
