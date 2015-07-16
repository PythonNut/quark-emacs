(eval-when-compile
  (with-demoted-errors
    (require 'smartparens)
    (require 'hexrgb)))

(defmacro rainbow-delimiters-saturate (face &optional degree)
  "Adjust the saturation of the given face by the given degree"
  `(face-remap-add-relative ,face
     (list (list ':foreground (hexrgb-increment-saturation
                                (face-attribute ,face :foreground) 0.5))
       ,face)))

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

  (setq
    rainbow-delimiters-switch nil
    rainbow-delimiters-face-cookies nil)

  (make-variable-buffer-local 'rainbow-delimiters-switch)
  (make-variable-buffer-local 'rainbow-delimiters-face-cookies)

  (defun rainbow-delimiters-focus-on ()
    (unless (featurep 'hexrgb) (require 'hexrgb))
    (setq rainbow-delimiters-face-cookies
      (list
        (rainbow-delimiters-saturate 'rainbow-delimiters-depth-1-face)
        (rainbow-delimiters-saturate 'rainbow-delimiters-depth-2-face)
        (rainbow-delimiters-saturate 'rainbow-delimiters-depth-3-face)
        (rainbow-delimiters-saturate 'rainbow-delimiters-depth-4-face)
        (rainbow-delimiters-saturate 'rainbow-delimiters-depth-5-face)
        (rainbow-delimiters-saturate 'rainbow-delimiters-depth-6-face)
        (rainbow-delimiters-saturate 'rainbow-delimiters-depth-7-face)
        (rainbow-delimiters-saturate 'rainbow-delimiters-depth-8-face)
        (rainbow-delimiters-saturate 'rainbow-delimiters-depth-9-face))
      rainbow-delimiters-switch t))

  (defun rainbow-delimiters-focus-off ()
    (mapc #'face-remap-remove-relative rainbow-delimiters-face-cookies)
    (setq rainbow-delimiters-switch nil))

  (defun rainbow-delimiters-focus-on-maybe ()
    "Display the show pair overlays."
    (when (or (looking-at "[][(){}]")
            (and
              (evil-insert-state-p)
              (looking-back "[][(){}]")))
      (unless (or rainbow-delimiters-switch (minibufferp))
        (rainbow-delimiters-focus-on))))

  (defun rainbow-delimiters-focus-off-maybe ()
    "Display the show pair overlays."
    (unless (or (looking-at "[][(){}]")
              (and
                (evil-insert-state-p)
                (looking-back "[][(){}]")))
      (when rainbow-delimiters-switch
        (rainbow-delimiters-focus-off))))

  (run-with-idle-timer 0.6 t 'rainbow-delimiters-focus-on-maybe)
  (run-with-idle-timer 0.1 t 'rainbow-delimiters-focus-off-maybe))

(provide 'config-rainbow-delimiters)
