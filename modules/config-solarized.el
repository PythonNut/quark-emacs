;; -*- lexical-binding: t -*-
(eval-when-compile
  (require 'config-macros)
  (require 'config-package))

(use-package solarized-theme)

(define-advice load-theme
    (:before (&rest _args) unload-themes)
  ;; TODO: This is probably a horrible hack
  (mapc #'disable-theme custom-enabled-themes))

(define-advice load-theme
    (:after (&rest _args) run-hook)
  (run-hooks 'load-theme-hook))

(add-hook
 'load-theme-hook
 (my/defun-as-value my/setup-faces ()
   (set-face-attribute 'mode-line nil
                       :underline nil
                       :box nil
                       :overline nil)
   (set-face-attribute 'mode-line-inactive nil
                       :underline nil
                       :box nil
                       :overline nil)
   (set-face-attribute 'mode-line-buffer-id nil
                       :foreground nil)
   (if (display-graphic-p)
       (set-face-attribute 'vertical-border nil
                           :foreground nil
                           :inherit 'region)

     (let ((display-table (or standard-display-table (make-display-table))))
       (set-display-table-slot display-table 'vertical-border (make-glyph-code ?│))
       (setq standard-display-table display-table)))))


(if (<= (display-color-cells) 256)
    (progn
      (load-theme 'tango-dark)
      (set-face-attribute 'font-lock-keyword-face nil
                          :weight 'extra-bold)
      (set-face-attribute 'hl-line nil
                          :foreground nil
                          :background "grey20"
                          :inherit nil))
  (load-theme 'solarized-dark t)
  (set-face-background 'fringe "#022F3A")
  (set-face-background 'mode-line-inactive "#073642"))

(provide 'config-solarized)
