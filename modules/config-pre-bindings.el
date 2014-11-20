(key-chord-mode +1)

(defun enable-debugging ()
  (interactive)
  (setq debug-on-error t))

(defun disable-debugging ()
  (interactive)
  (setq debug-on-error nil))

