;; -*- lexical-binding: t -*-

(eval-when-compile
  (with-demoted-errors "Load error: %s"
    (require 'diminish)
    (require 'smie)
    (require 'evil)))

(setq require-final-newline t
      line-move-visual t)

(with-eval-after-load 'adaptive-wrap
  (eval-when-compile
    (with-demoted-errors "Load error: %s"
      (require 'adaptive-wrap)))

  (setq-default adaptive-wrap-extra-indent 2))

(add-hook 'visual-line-mode-hook #'adaptive-wrap-prefix-mode)
(add-hook 'visual-line-mode-hook
          (lambda ()
            (diminish 'visual-line-mode)))

(global-visual-line-mode +1)

;; always ensure UTF-8
(defun cleanup-buffer-safe ()
  (interactive)
  (set-buffer-file-coding-system 'utf-8))

(defun cleanup-buffer-unsafe ()
  (interactive)
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace)
  (set-buffer-file-coding-system 'utf-8))

(add-hook 'before-save-hook #'cleanup-buffer-safe)

(with-eval-after-load 'ws-butler
  (diminish 'ws-butler-mode " β"))

;; autoload ws-butler on file open
(defun my/ws-butler-onetime-setup ()
  (ws-butler-global-mode +1)
  (remove-hook 'find-file-hook #'my/ws-butler-onetime-setup))

(add-hook 'find-file-hook #'my/ws-butler-onetime-setup)
(add-hook 'find-file-hook #'dtrt-indent-mode)

;; ws-butler also loads highlight-changes-mode
(add-hook 'highlight-changes-mode-hook
          (lambda ()
            (diminish 'highlight-changes-mode)))

(with-eval-after-load 'aggressive-indent
  (diminish 'aggressive-indent-mode (if (display-graphic-p) " ⇶" " *→")))

(with-eval-after-load 'auto-indent-mode
  (eval-when-compile
    (with-demoted-errors "Load error: %s"
      (require 'auto-indent-mode)))
  (setq auto-indent-assign-indent-level-variables nil)
  (diminish 'auto-indent-mode (if (display-graphic-p) " ⇉" " →")))

(defun my/auto-indent-onetime-setup ()
  (auto-indent-global-mode +1)
  (remove-hook 'first-change-hook #'my/auto-indent-onetime-setup))

(add-hook 'emacs-startup-hook
          (lambda ()
            (add-hook 'first-change-hook #'my/auto-indent-onetime-setup)))

(defun my/smie-auto-guess ()
  (when (and
         (featurep 'smie)
         (not (eq smie-grammar 'unset)))
    (smie-config-guess)))

(add-hook 'after-change-major-mode-hook #'my/smie-auto-guess)

(eval-when-compile
  (with-demoted-errors "Load error: %s"
    (require 'evil)))

(defun back-to-indentation-visual-or-beginning (&optional n)
  (interactive "^p")
  (or n (setq n 1))
  (if (/= n 1)
      (let ((line-move-visual t))
        (line-move (1- n) t)))
  (let ((bol (save-excursion (beginning-of-line) (point)))
        (bti (save-excursion (back-to-indentation) (point)))
        (bovl (save-excursion (beginning-of-visual-line) (point))))
    (if (bound-and-true-p multiple-cursors-mode)
        (cond
         ((= (point) bti)
          (beginning-of-line))
         ((/= (point) bol)
          (back-to-indentation)))
      (cond
       ((= (point) bol)
        (back-to-indentation))
       ((= (point) bti)
        (beginning-of-visual-line))
       ((= (point) bovl)
        (back-to-indentation))
       ((/= bol bovl)
        (beginning-of-visual-line))
       (t
        (back-to-indentation))))))

(define-key evil-insert-state-map (kbd "C-a")
  #'back-to-indentation-visual-or-beginning)
(define-key evil-motion-state-map (kbd "C-a")
  #'back-to-indentation-visual-or-beginning)

(define-key evil-insert-state-map (kbd "<home>")
  #'back-to-indentation-visual-or-beginning)
(define-key evil-motion-state-map (kbd "<home>")
  #'back-to-indentation-visual-or-beginning)

(defun end-of-visual-line-or-end (&optional n)
  (interactive "^p")
  (or n (setq n 1))
  (if (/= n 1)
      (let ((line-move-visual t))
        (line-move (1- n) t)))
  (let ((eovl (save-excursion (end-of-visual-line) (point))))
    (cond
     ((bound-and-true-p multiple-cursors-mode)
      (end-of-line))
     ((= (point) eovl)
      (end-of-line))
     (t
      (end-of-visual-line)))))

(define-key evil-insert-state-map (kbd "C-e") #'end-of-visual-line-or-end)
(define-key evil-insert-state-map (kbd "<end>") #'end-of-visual-line-or-end)

(provide 'config-whitespace)
