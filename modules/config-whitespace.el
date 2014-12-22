(eval-when-compile
  (progn
    (require 'cl)
    (require 'auto-indent-mode)
    (require 'diminish)))

(require 'config-indent)

(require 'adaptive-wrap)

(add-hook 'visual-line-mode-hook
  (lambda ()
    (setq
      adaptive-wrap-extra-indent 2
      line-move-visual t)
    (adaptive-wrap-prefix-mode +1)
    (diminish 'visual-line-mode)))

(global-visual-line-mode +1)

;; always ensure UTF-8
(defun cleanup-buffer-safe ()
  (interactive)
  ;; (untabify (point-min) (point-max))
  (set-buffer-file-coding-system 'utf-8))

(defun cleanup-buffer-unsafe ()
  (interactive)
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace)
  (set-buffer-file-coding-system 'utf-8))

(add-hook 'before-save-hook #'cleanup-buffer-safe)

;; autoload ws-butler on file open
(add-hook 'find-file-hooks #'ws-butler-global-mode)
(setq require-final-newline t)

;; ws-butler also load highlight-changes-mode
(with-eval-after-load 'ws-butler
  ;; (diminish 'ws-butler-global-mode)
  (diminish 'ws-butler-mode " β"))

(add-hook 'highlight-changes-mode-hook
  (lambda ()
    (diminish 'highlight-changes-mode)))

(provide 'config-whitespace)
