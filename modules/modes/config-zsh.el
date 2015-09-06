;; -*- lexical-binding: t -*-

;; bind zsh files to sh-mode
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))

;; bind zsh files to the zsh submode of sh-mode
(add-hook 'sh-mode-hook
          (lambda ()
            (setq mode-name "sh")
            (if (string-match "\\.zsh$" buffer-file-name)
                (sh-set-shell "zsh"))))
