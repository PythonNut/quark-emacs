(load-library "config-indent.el")
(load-library "config-wrap.el")

;; always ensure UTF-8
(defun cleanup-buffer-safe ()
  (interactive)
  ;; (untabify (point-min) (point-max))
  (set-buffer-file-coding-system 'utf-8))

(add-hook 'before-save-hook 'cleanup-buffer-safe)

;; autoload ws-butler on file open
(add-hook 'find-file-hooks 'ws-butler-global-mode)
(setq require-final-newline t)
