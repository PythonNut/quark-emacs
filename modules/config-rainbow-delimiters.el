(eval-when-compile
  (progn
    (require 'cl)
    (require 'smartparens)
    (require 'hexrgb)))

(require 'rainbow-delimiters)

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
  (defun rainbow-wash-out-color (color &optional amount)
    "Return a color string specifying a washed-out version of COLOR."
    (let ((basec (color-values
                   (face-attribute 'default :foreground)))
           (col (color-values color))
           (list nil))
      (while col
        (push (/ (/ (+ (or (pop col) 128)
                      (* amount (or (pop basec) 128)))
                   (1+ amount))
                256)
          list))
      (apply 'format "#%02x%02x%02x" (nreverse list))))

  (defun rainbow-wash-out-face (face &optional amount)
    "Make the foreground color of FACE appear a bit more pale."
    (let ((color (face-attribute face :foreground)))
      (unless (eq color 'unspecified)
        (set-face-attribute face nil
          :foreground (rainbow-wash-out-color color amount)))))

  (defun rainbow-delimiters-wash (arg)
    (rainbow-wash-out-face 'rainbow-delimiters-depth-1-face arg)
    (rainbow-wash-out-face 'rainbow-delimiters-depth-2-face arg)
    (rainbow-wash-out-face 'rainbow-delimiters-depth-3-face arg)
    (rainbow-wash-out-face 'rainbow-delimiters-depth-4-face arg)
    (rainbow-wash-out-face 'rainbow-delimiters-depth-5-face arg)
    (rainbow-wash-out-face 'rainbow-delimiters-depth-6-face arg)
    (rainbow-wash-out-face 'rainbow-delimiters-depth-7-face arg)
    (rainbow-wash-out-face 'rainbow-delimiters-depth-8-face arg)
    (rainbow-wash-out-face 'rainbow-delimiters-depth-9-face arg))

  (defun rainbow-define-faces ()
    (interactive)
    (set-face-foreground 'rainbow-delimiters-depth-1-face "#93a1a1")
    (set-face-foreground 'rainbow-delimiters-depth-2-face "#cb4b16")
    (set-face-foreground 'rainbow-delimiters-depth-3-face "#6c71c4")
    (set-face-foreground 'rainbow-delimiters-depth-4-face "#93a1a1")
    (set-face-foreground 'rainbow-delimiters-depth-5-face "#859900")
    (set-face-foreground 'rainbow-delimiters-depth-6-face "#258bd2")
    (set-face-foreground 'rainbow-delimiters-depth-7-face "#d33682")
    (set-face-foreground 'rainbow-delimiters-depth-8-face "#6c71c4")
    (set-face-foreground 'rainbow-delimiters-depth-9-face "#2aa198")
    (rainbow-delimiters-wash 2))

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

  (defvar rainbow-delimiters-switch nil)

  (defun rainbow-delimiters-on-maybe ()
    (unless (or rainbow-delimiters-switch (minibufferp))
      (rainbow-delimiters-focus rainbow-delimiters-face-delta)
      (setq rainbow-delimiters-switch t)))

  (defun rainbow-delimiters-off-maybe ()
    (when rainbow-delimiters-switch
      (rainbow-delimiters-focus (- rainbow-delimiters-face-delta))
      (setq rainbow-delimiters-switch nil)))

  (defun rainbow-delimiters-focus-on-maybe ()
    "Display the show pair overlays."
    (let* ((pair-list (sp--get-allowed-pair-list))
            (opening (sp--get-opening-regexp pair-list))
            (closing (sp--get-closing-regexp pair-list))
            (allowed (and sp-show-pair-from-inside (sp--get-allowed-regexp))))
      (when (or
              (or (sp--looking-at (if sp-show-pair-from-inside allowed opening))
                (and (memq major-mode sp-navigate-consider-stringlike-sexp)
                  (looking-at (sp--get-stringlike-regexp))))

              (or (sp--looking-back (if sp-show-pair-from-inside allowed closing))
                (and (memq major-mode sp-navigate-consider-stringlike-sexp)
                  (sp--looking-back (sp--get-stringlike-regexp)))))
        (rainbow-delimiters-on-maybe))))

  (run-with-idle-timer 0.6 t 'rainbow-delimiters-focus-on-maybe)

  (defun rainbow-delimiters-focus-off-maybe ()
    "Display the show pair overlays."
    (let* ((pair-list (sp--get-allowed-pair-list))
            (opening (sp--get-opening-regexp pair-list))
            (closing (sp--get-closing-regexp pair-list))
            (allowed (and sp-show-pair-from-inside (sp--get-allowed-regexp))))
      (unless (or
                (or (sp--looking-at (if sp-show-pair-from-inside allowed opening))
                  (and (memq major-mode sp-navigate-consider-stringlike-sexp)
                    (looking-at (sp--get-stringlike-regexp))))

                (or (sp--looking-back (if sp-show-pair-from-inside allowed closing))
                  (and (memq major-mode sp-navigate-consider-stringlike-sexp)
                    (sp--looking-back (sp--get-stringlike-regexp)))))
        (rainbow-delimiters-off-maybe))))

  (run-with-idle-timer 0.1 t 'rainbow-delimiters-focus-off-maybe))

(provide 'config-rainbow-delimiters)
