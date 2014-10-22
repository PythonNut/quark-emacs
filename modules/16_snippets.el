;;; ====================================
;;; snippets - prebaked code just for me
;;; ====================================
(require 'yasnippet)
(setq yas-snippet-dirs "~/.emacs.d/data/snippets/")
(setq yas-trigger-key "C-c TAB")
(yas-global-mode 1)

;; automatic yasnippets
(autoload 'aya-create "auto-yasnippet")
(autoload 'aya-expand "auto-yasnippet")
(global-set-key (kbd "C-c C") 'aya-create)
(global-set-key (kbd "C-c E") 'aya-expand)

;; emmet mode - extensible html/css snippets
(eval-after-load 'emmet-mode
  '(progn
     (defun try-expand-emmet (arg)
       (emmet-expand-yas))
     (add-to-list 'hippie-expand-try-functions-list
       'try-expand-emmet)
     (emmet-mode +1)))

;; Auto-start on any markup modes
(add-hook 'sgml-mode-hook '(lambda () (require 'emmet-mode)))
(add-hook 'css-mode-hook  '(lambda () (require 'emmet-mode)))

