(eval-and-compile
  (use-package solarized-theme)
  (require 'solarized))
(eval-when-compile
  (require 'solarized-palettes))

(setq quark/solarized-faces                                 
      '("My personal solarized theme customization."
        (custom-theme-set-faces
         theme-name
         `(mode-line
           ((,class (:inverse-video unspecified
                                    :foreground ,s-mode-line-fg
                                    :background ,s-mode-line-bg))))
         `(mode-line-inactive
           ((,class (:inverse-video unspecified
                                    :underline ,s-mode-line-underline
                                    :foreground ,s-mode-line-inactive-fg
                                    :background ,base02))))
         `(mode-line-buffer-id ((,class (:weight bold))))
         `(fringe ((,class (:foreground ,s-fringe-fg :background "#022F3A"))))
         `(vertical-border ((,class (:background nil :foreground ,base02))))
         `(diff-hl-change ((,class (:background ,s-diff-fine-C-bg :foreground ,s-diff-C-fg))))
         `(diff-hl-delete ((,class (:background ,s-diff-fine-A-bg :foreground ,s-diff-A-fg))))
         `(diff-hl-insert ((,class (:background ,s-diff-fine-B-bg :foreground ,s-diff-B-fg))))
         `(avy-background-face ((,class (:foreground ,base01))))
         `(avy-lead-face ((,class (:foreground ,yellow :background nil))))
         `(avy-lead-face-0 ((,class (:weight extra-bold :foreground ,red :background nil))))
         `(avy-lead-face-1 ((,class (:foreground ,base0 :background nil))))
         `(ivy-minibuffer-match-face-1 ((,class (:background nil))))
         `(ivy-minibuffer-match-face-2 ((,class (:background nil :foreground ,blue))))
         `(rainbow-delimiters-depth-1-face ((,class (:foreground "#889899"))))
         `(rainbow-delimiters-depth-2-face ((,class (:foreground "#9b7b6b"))))
         `(rainbow-delimiters-depth-3-face ((,class (:foreground "#7b88a5"))))
         `(rainbow-delimiters-depth-4-face ((,class (:foreground "#889899"))))
         `(rainbow-delimiters-depth-5-face ((,class (:foreground "#839564"))))
         `(rainbow-delimiters-depth-6-face ((,class (:foreground "#6391aa"))))
         `(rainbow-delimiters-depth-7-face ((,class (:foreground "#9d748f"))))
         `(rainbow-delimiters-depth-8-face ((,class (:foreground "#7b88a5"))))
         `(rainbow-delimiters-depth-9-face ((,class (:foreground "#659896"))))
         `(evil-snipe-matches-face ((,class (:background ,base00 :foreground ,base03))))
         )))

(deftheme quark-solarized-dark "The dark variant of the Solarized colour theme")

(solarized-with-color-variables
  'dark 'quark-solarized-dark solarized-dark-color-palette-alist quark/solarized-faces)

(provide-theme 'quark-solarized-dark)
