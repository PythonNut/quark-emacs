;;; ===========
;;; Python mode
;;; ===========
(add-hook 'inferior-python-mode-hook
  '(lambda ()
     (auto-complete-mode +1)
     (flycheck-mode +1)
     (jedi:ac-setup)))

(defun annotate-pdb ()
  (interactive)
  (highlight-lines-matching-regexp "pdb.set_trace()"))

(add-hook 'python-mode-hook 'annotate-pdb)
;;(require 'ipython)
(add-hook 'python-mode-hook
  '(lambda ()
     (require 'comint)
     (setq
       python-shell-interpreter "ipython"
       python-shell-interpreter-args ""
       python-shell-prompt-regexp "In \\[[0-9]+\\]: "
       python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
       python-shell-completion-setup-code
       "from IPython.core.completerlib import module_completion"
       python-shell-completion-module-string-code
       "';'.join(module_completion('''%s'''))\n"
       python-shell-completion-string-code
       "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")
     (defun my-python-compile ()
       "Use compile to run python programs"
       (interactive)
       (compile (concat "/bin/ipython " (buffer-file-name)) t))
     (setq compilation-scroll-output t)
     (setq auto-indent-eol-char ":")
     (global-set-key (kbd "<f5>") 'my-python-compile)

     ;; guess indentation level from first block
     (run-at-time "0.1 sec" nil 'python-indent-guess-indent-offset)
     (setq jedi:complete-on-dot t)
     (jedi:ac-setup)))

