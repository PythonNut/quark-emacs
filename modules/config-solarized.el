(if (<= (display-color-cells) 256)
  (load-theme 'tango-dark)
  (load-theme 'solarized-dark))

(set-face-background 'fringe "#022F3A")
(set-face-attribute 'mode-line nil :underline nil)
(set-face-attribute 'mode-line-inactive nil :underline nil)

(set-face-background 'mode-line-inactive "#073642")

(if (display-graphic-p)
  (set-face-attribute 'vertical-border nil
    :foreground nil
    :inherit 'region)

  (let ((display-table (or standard-display-table (make-display-table))))
    (set-display-table-slot display-table 'vertical-border (make-glyph-code ?│))
    (setq standard-display-table display-table)))

(provide 'config-solarized)
