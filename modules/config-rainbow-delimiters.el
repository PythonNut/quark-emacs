;; -*- lexical-binding: t -*-

(eval-when-compile
  (with-demoted-errors
    (require 'rainbow-delimiters)))

(defvar my/rainbow-delimiters-switch nil
  "t if rainbow-delimiters are currently punched")
(defvar my/rainbow-delimiters-face-cookies nil
  "a list of face-remap-add-relative cookies to reset")

(make-variable-buffer-local 'rainbow-delimiters-switch)
(make-variable-buffer-local 'rainbow-delimiters-face-cookies)

;; the equivalent of a global mode, but does not
;; turn on for odd non-programming modes
(add-hook 'prog-mode-hook
  (lambda ()
    (when (display-graphic-p)
      (rainbow-delimiters-mode +1))))

(add-hook 'text-mode-hook
  (lambda ()
    (when (display-graphic-p)
      (rainbow-delimiters-mode +1))))

(with-eval-after-load 'rainbow-delimiters
  (set-face-foreground 'rainbow-delimiters-depth-1-face "#889899")
  (set-face-foreground 'rainbow-delimiters-depth-2-face "#9b7b6b")
  (set-face-foreground 'rainbow-delimiters-depth-3-face "#7b88a5")
  (set-face-foreground 'rainbow-delimiters-depth-4-face "#889899")
  (set-face-foreground 'rainbow-delimiters-depth-5-face "#839564")
  (set-face-foreground 'rainbow-delimiters-depth-6-face "#6391aa")
  (set-face-foreground 'rainbow-delimiters-depth-7-face "#9d748f")
  (set-face-foreground 'rainbow-delimiters-depth-8-face "#7b88a5")
  (set-face-foreground 'rainbow-delimiters-depth-9-face "#659896")

  (defun my/rainbow-delimiters-focus-on ()
    "Punch the rainbow-delimiters"
    (setq my/rainbow-delimiters-face-cookies
      (list
        (face-remap-add-relative 'rainbow-delimiters-depth-1-face
          '((:foreground "#3B9399") rainbow-delimiters-depth-1-face))
        (face-remap-add-relative 'rainbow-delimiters-depth-2-face
          '((:foreground "#9B471D") rainbow-delimiters-depth-2-face))
        (face-remap-add-relative 'rainbow-delimiters-depth-3-face
          '((:foreground "#284FA5") rainbow-delimiters-depth-3-face))
        (face-remap-add-relative 'rainbow-delimiters-depth-4-face
          '((:foreground "#3B9399") rainbow-delimiters-depth-4-face))
        (face-remap-add-relative 'rainbow-delimiters-depth-5-face
          '((:foreground "#679519") rainbow-delimiters-depth-5-face))
        (face-remap-add-relative 'rainbow-delimiters-depth-6-face
          '((:foreground "#0E73AA") rainbow-delimiters-depth-6-face))
        (face-remap-add-relative 'rainbow-delimiters-depth-7-face
          '((:foreground "#9D2574") rainbow-delimiters-depth-7-face))
        (face-remap-add-relative 'rainbow-delimiters-depth-8-face
          '((:foreground "#284FA5") rainbow-delimiters-depth-8-face))
        (face-remap-add-relative 'rainbow-delimiters-depth-9-face
          '((:foreground "#199893") rainbow-delimiters-depth-9-face)))
      my/rainbow-delimiters-switch t))

  (defun my/rainbow-delimiters-focus-off ()
    "Reset the rainbow-delimiters faces"
    (mapc #'face-remap-remove-relative my/rainbow-delimiters-face-cookies)
    (setq my/rainbow-delimiters-switch nil))

  (defun my/rainbow-delimiters-focus-on-maybe ()
    "Punch the rainbow-delimiters if the point is on a paren"
    (when (or (looking-at "[][(){}]")
            (and
              (evil-insert-state-p)
              (looking-back "[][(){}]" (1- (point)))))
      (unless (or my/rainbow-delimiters-switch (minibufferp))
        (my/rainbow-delimiters-focus-on))))

  (defun my/rainbow-delimiters-focus-off-maybe ()
    "Reset the rainbow-delimiters if the point is not on a paren"
    (unless (or (looking-at "[][(){}]")
              (and
                (evil-insert-state-p)
                (looking-back "[][(){}]" (1- (point)))))
      (when my/rainbow-delimiters-switch
        (my/rainbow-delimiters-focus-off))))

  (run-with-idle-timer 0.6 t 'my/rainbow-delimiters-focus-on-maybe)
  (run-with-idle-timer 0.1 t 'my/rainbow-delimiters-focus-off-maybe))

(provide 'config-rainbow-delimiters)
