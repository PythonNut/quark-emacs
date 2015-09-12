;; -*- lexical-binding: t -*-

(eval-when-compile
  (with-demoted-errors "Load error: %s"
    (require 'auto-indent-mode)
    (require 'adaptive-wrap)
    (require 'diminish)))

(require 'config-indent)

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

;; ws-butler also loads highlight-changes-mode
(add-hook 'highlight-changes-mode-hook
          (lambda ()
            (diminish 'highlight-changes-mode)))

(provide 'config-whitespace)
