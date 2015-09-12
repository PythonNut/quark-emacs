;; -*- lexical-binding: t -*-

(eval-when-compile
  (with-demoted-errors "Load error: %s"
    (require 'auto-indent-mode)
    (require 'adaptive-wrap)
    (require 'diminish)
    (require 'smie)
    (require 'evil)))

(setq require-final-newline t
      line-move-visual t)

(with-eval-after-load 'adaptive-wrap
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

(with-eval-after-load 'auto-indent
  (add-hook 'auto-indent-global-mode-hook
            (lambda ()
              (diminish 'auto-indent-mode (if (display-graphic-p) " ⇉" " →"))))

  (add-hook 'auto-indent-mode-hook
            (lambda ()
              (diminish 'auto-indent-mode (if (display-graphic-p) " ⇉" " →")))))

(defun my/auto-indent-onetime-setup ()
  (auto-indent-global-mode +1)
  (remove-hook 'first-change-hook
               #'my/auto-indent-onetime-setup))

(add-hook 'emacs-startup-hook
          (lambda ()
            (add-hook 'first-change-hook #'my/auto-indent-onetime-setup)))

(defun my/smie-auto-guess ()
  (when (featurep 'smie)
    (unless (eq smie-grammar 'unset)
      (smie-config-guess))))

(add-hook 'after-change-major-mode-hook #'my/smie-auto-guess)

(evil-define-command back-to-indentation-or-beginning ()
  (if (= (point)
         (save-excursion (back-to-indentation) (point)))
      (if (bound-and-true-p multiple-cursors-mode)
          (beginning-of-line)
        (beginning-of-visual-line))
    (unless (and (bolp)
                 (bound-and-true-p multiple-cursors-mode))
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

(provide 'config-whitespace)
