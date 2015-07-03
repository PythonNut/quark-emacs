(eval-when-compile
  (with-demoted-errors
    (require 'smartparens)
    (require 'hexrgb)))

(defvar rainbow-delimiters-switch nil)

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
  (defun rainbow-define-faces ()
    (interactive)
    (setq rainbow-delimiters-switch nil)
    (set-face-foreground 'rainbow-delimiters-depth-1-face "#889899")
    (set-face-foreground 'rainbow-delimiters-depth-2-face "#9b7b6b")
    (set-face-foreground 'rainbow-delimiters-depth-3-face "#7b88a5")
    (set-face-foreground 'rainbow-delimiters-depth-4-face "#889899")
    (set-face-foreground 'rainbow-delimiters-depth-5-face "#839564")
    (set-face-foreground 'rainbow-delimiters-depth-6-face "#6391aa")
    (set-face-foreground 'rainbow-delimiters-depth-7-face "#9d748f")
    (set-face-foreground 'rainbow-delimiters-depth-8-face "#7b88a5")
    (set-face-foreground 'rainbow-delimiters-depth-9-face "#659896"))

  ;; currently solarized colors
  (add-hook 'after-change-major-mode-hook #'rainbow-define-faces)

  ;; punch the color
  (defun rainbow-delimiters-saturate (face &optional degree)
    (unless (featurep 'hexrgb) (require 'hexrgb))
    "Adjust the saturation of the given face by the given degree"
    (set-face-foreground face
      (hexrgb-increment-saturation
        (face-attribute face :foreground) 0.5)))

  ;; punch the parens
  (defvar rainbow-delimiters-face-delta 0.1)
  (defun rainbow-delimiters-focus (arg)
    (rainbow-delimiters-saturate 'rainbow-delimiters-depth-1-face arg)
    (rainbow-delimiters-saturate 'rainbow-delimiters-depth-2-face arg)
    (rainbow-delimiters-saturate 'rainbow-delimiters-depth-3-face arg)
    (rainbow-delimiters-saturate 'rainbow-delimiters-depth-4-face arg)
    (rainbow-delimiters-saturate 'rainbow-delimiters-depth-5-face arg)
    (rainbow-delimiters-saturate 'rainbow-delimiters-depth-6-face arg)
    (rainbow-delimiters-saturate 'rainbow-delimiters-depth-7-face arg)
    (rainbow-delimiters-saturate 'rainbow-delimiters-depth-8-face arg)
    (rainbow-delimiters-saturate 'rainbow-delimiters-depth-9-face arg))

  (defun rainbow-delimiters-focus-on-maybe ()
    "Display the show pair overlays."
    (when (or (looking-at "[][(){}]")
            (and
              (evil-insert-state-p)
              (looking-back "[][(){}]")))
      (unless (or rainbow-delimiters-switch (minibufferp))
        (rainbow-delimiters-focus rainbow-delimiters-face-delta)
        (setq rainbow-delimiters-switch t))))

  (run-with-idle-timer 0.6 t 'rainbow-delimiters-focus-on-maybe)

  (defun rainbow-delimiters-focus-off-maybe ()
    "Display the show pair overlays."
    (unless (or (looking-at "[][(){}]")
              (and
                (evil-insert-state-p)
                (looking-back "[][(){}]")))
      (when rainbow-delimiters-switch
        (rainbow-delimiters-focus (- rainbow-delimiters-face-delta))
        (setq rainbow-delimiters-switch nil))))

  (run-with-idle-timer 0.1 t 'rainbow-delimiters-focus-off-maybe))

(provide 'config-rainbow-delimiters)
