;;; =============================
;;; Dired, the emacs file manager
;;; =============================
(eval-after-load 'dired+
  '(progn
     (define-key dired-mode-map (kbd "<down-mouse-3>") 'strokes-do-stroke)
     (toggle-diredp-find-file-reuse-dir 1)))

(add-hook 'dired-load-hook
  '(lambda ()
     (require 'dired-x)
     (require 'dired+)))

(defun dired-here ()
  (interactive)
  (dired (file-name-directory (buffer-file-name))))

