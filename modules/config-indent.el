(eval-when-compile
  (with-demoted-errors
    (require 'auto-indent-mode)
    (require 'smie)
    (require 'evil)
    (require 'multiple-cursors)))

(add-hook 'auto-indent-global-mode-hook
  (lambda ()
    (diminish 'auto-indent-mode (if (display-graphic-p) " ⇉" " →"))))

(add-hook 'auto-indent-mode-hook
  (lambda ()
    (diminish 'auto-indent-mode (if (display-graphic-p) " ⇉" " →"))))

(defun auto-indent-onetime-setup ()
  (auto-indent-global-mode +1)
  (remove-hook 'first-change-hook
    #'auto-indent-onetime-setup))

(add-hook 'emacs-startup-hook
  (lambda ()
    (add-hook 'first-change-hook #'auto-indent-onetime-setup)))

(add-hook 'find-file-hook #'dtrt-indent-mode)

(defun smie-auto-guess ()
  (when (featurep 'smie)
    (unless (eq smie-grammar 'unset)
      (smie-config-guess))))

(add-hook 'after-change-major-mode-hook #'smie-auto-guess)

(defun back-to-indentation-or-beginning ()
  (interactive)
  (if (= (point)
        (save-excursion (back-to-indentation) (point)))
    (beginning-of-line)
    (unless (and (bolp)
              (featurep 'multiple-cursors)
              multiple-cursors-mode)
      (back-to-indentation))))

(define-key evil-insert-state-map (kbd "C-a")
  #'back-to-indentation-or-beginning)
(define-key evil-motion-state-map (kbd "C-a")
  #'back-to-indentation-or-beginning)

(define-key evil-insert-state-map (kbd "<home>")
  #'back-to-indentation-or-beginning)
(define-key evil-motion-state-map (kbd "<home>")
  #'back-to-indentation-or-beginning)

(define-key evil-insert-state-map (kbd "C-e") #'end-of-visual-line)
(define-key evil-insert-state-map (kbd "<end>") #'end-of-visual-line)

(provide 'config-indent)
