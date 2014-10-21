;;; ======================================
;;; Whitespace mode - visualize whitespace
;;; ======================================
;;(require 'whitespace)
;;(global-whitespace-mode +1)
;; (add-hook 'prog-mode-hook '(lambda ()
;;   (whitespace-mode +1)))

(set-face-background 'trailing-whitespace "#744")
(setq show-trailing-whitespace t)

(eval-after-load 'whitespace
  '(progn
     (setq whitespace-style
       '(
          face
          ;; tabs
          ;; spaces
          ;; trailing
          trailing
          ;; space-mark
          ;; tab-mark
          ))

     ;; dim grey dots are good for guiding the eye
     (set-face-background 'whitespace-space 'nil)
     (set-face-foreground 'whitespace-space "grey30")
     (set-face-attribute 'whitespace-space nil :weight 'light)

     ;; may trailing whitespace blaze like the noonday sun
     (set-face-foreground 'whitespace-trailing "grey60")
     (set-face-background 'whitespace-trailing 'nil)
     (set-face-attribute 'whitespace-trailing 'nil :weight 'extra-bold)

     (set-face-foreground 'whitespace-newline "grey10")
     (set-face-background 'whitespace-newline 'nil)
     (set-face-attribute 'whitespace-newline 'nil :weight 'light)))

(autoload 'indent-guide-mode "indent-guide")
;; (require 'indent-guide)
;; (setq indent-guide-char "│")
;; (set-face-foreground 'indent-guide-face "grey30")
;; (set-face-background 'indent-guide-face 'nil)

;; (indent-guide-global-mode)
;; (require 'indent-vline)

(setq require-final-newline t)
(add-hook 'find-file-hooks
  (lambda ()
    (require 'ws-butler)
    (ws-butler-global-mode +1)))

;; (add-hook 'prog-mode-hook 'indent-hint-mode)

(defun cleanup-buffer-safe ()
  (interactive)
  (untabify (point-min) (point-max))
  (set-buffer-file-coding-system 'utf-8))

(add-hook 'before-save-hook 'cleanup-buffer-safe)

;;; =====================
;;; Intelligent wrap mode
;;; =====================
(require 'adaptive-wrap)
(add-hook 'visual-line-mode-hook 'adaptive-wrap-prefix-mode)
(global-visual-line-mode +1)
(add-hook 'term-mode
  '(lambda ()
     (adaptive-wrap-prefix-mode -1)))

