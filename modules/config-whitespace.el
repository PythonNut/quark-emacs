(load-library "config-indent")
(load-library "config-wrap")
(eval-when-compile (require 'cl))

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

(add-hook 'before-save-hook 'cleanup-buffer-safe)

;; autoload ws-butler on file open
(add-hook 'find-file-hooks 'ws-butler-global-mode)
(setq require-final-newline t)

;; ws-butler also load highlight-changes-mode
(with-eval-after-load 'ws-butler
  ;; (diminish 'ws-butler-global-mode)
  (diminish 'ws-butler-mode " β"))

(add-hook 'highlight-changes-mode-hook
  (lambda ()
    (diminish 'highlight-changes-mode)))
