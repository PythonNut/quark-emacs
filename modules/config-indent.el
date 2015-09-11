;; -*- lexical-binding: t -*-

(eval-when-compile
  (with-demoted-errors "Load error: %s"
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

(with-eval-after-load 'aggressive-indent
  (diminish 'aggressive-indent-mode (if (display-graphic-p) " ⇶" " *→")))

(defun my/auto-indent-onetime-setup ()
  (auto-indent-global-mode +1)
  (remove-hook 'first-change-hook
               #'my/auto-indent-onetime-setup))

(add-hook 'emacs-startup-hook
          (lambda ()
            (add-hook 'first-change-hook #'my/auto-indent-onetime-setup)))

(add-hook 'find-file-hook #'dtrt-indent-mode)

(defun my/smie-auto-guess ()
  (when (featurep 'smie)
    (unless (eq smie-grammar 'unset)
      (smie-config-guess))))

(add-hook 'after-change-major-mode-hook #'my/smie-auto-guess)

(evil-define-command back-to-indentation-or-beginning ()
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
