(eval-when-compile
  (require 'evil)
  (require 'jedi))

(add-hook 'python-mode-hook
  (lambda ()
    (add-to-list 'evil-overriding-maps 'jedi-mode-map)
    (evil-define-key 'normal jedi-mode-map (kbd "gd") #'jedi:goto-definition)
    (jedi:setup)))

(add-hook 'python-mode-hook
  (lambda ()
    (run-at-time 1 nil #'python-indent-guess-indent-offset)))

(with-eval-after-load 'python
  (setq
    python-indent-guess-indent-offset t
    jedi:use-shortcuts t
    jedi:complete-on-dot t))

(with-eval-after-load 'jedi
  (define-key jedi-mode-map (kbd "<C-tab>") nil))

(defun helm-traad-commands ()
  (interactive)
  (minibuffer-with-setup-hook
    (lambda ()
      (insert "traad-"))
    (call-interactively #'helm-M-x)))

(evil-leader/set-key
  "t" #'helm-traad-commands)
