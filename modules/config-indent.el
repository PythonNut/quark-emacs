(eval-when-compile (require 'cl))
(eval-when-compile (require 'auto-indent-mode))
(eval-when-compile (require 'smie))

(add-hook 'auto-indent-global-mode-hook
  (lambda ()
    (diminish 'auto-indent-mode " ⇉")))

(add-hook 'auto-indent-mode-hook
  (lambda ()
    (diminish 'auto-indent-mode " ⇉")))

(defun auto-indent-onetime-setup ()
  (auto-indent-global-mode +1)
  (remove-hook 'first-change-hook
    #'auto-indent-onetime-setup))

(add-hook 'first-change-hook #'auto-indent-onetime-setup)
(add-hook 'find-file-hook #'dtrt-indent-mode)

(defun smie-auto-guess ()
  (when (featurep 'smie)
    (unless (eq smie-grammar 'unset)
      (smie-config-guess))))

(add-hook 'after-change-major-mode-hook #'smie-auto-guess)

(provide 'config-indent)
