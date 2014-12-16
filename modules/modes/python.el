(eval-when-compile
  (progn
    (require 'evil)
    (require 'jedi)))

(add-hook 'python-mode-hook
  (lambda ()
    (python-indent-guess-indent-offset)
    (add-to-list 'evil-overriding-maps 'jedi-mode-map)
    (evil-define-key 'normal jedi-mode-map (kbd "gd") #'jedi:goto-definition)
    (jedi:setup)))

(with-eval-after-load 'python
  (setq
    jedi:use-shortcuts t
    jedi:complete-on-dot t))

(defun helm-traad-commands ()
  (interactive)
  (minibuffer-with-setup-hook
    (lambda ()
      (insert "traad-"))
    (call-interactively #'helm-M-x)))

(evil-leader/set-key
  "t" #'helm-traad-commands)
