;; Copyright (C) 2012 Sergey Ovechkin

;; Author: Sergey Ovechkin <me@sovechkin.com>
;; URL: https://github.com/pomeo/oblivion-emacs
;; Version: 0.1
;; Package-Requires: ((color-theme "6.6.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Comment:
;;
;; A port of the Gedit theme Oblivion for Emacs.
;;
;;; Installation:
;;
;;   (require 'color-theme-oblivion)
;;   (color-theme-oblivion)
;;
;;; Credits
;;
;; Paolo Borelli <pborelli@gnome.org> created the original theme for Gedit
;; on such this port is based.
;;
;;; Code

;; requires
(require 'color-theme)

;; color definitions
(defvar butter1 "#fce94f")
(defvar butter2 "#edd400")
(defvar butter3 "#c4a000")
(defvar chameleon1 "#8ae234")
(defvar chameleon2 "#73d216")
(defvar chameleon3 "#4e9a06")
(defvar orange1 "#fcaf3e")
(defvar orange2 "#f57900")
(defvar orange3 "#ce5c00")
(defvar skyblue1 "#729fcf")
(defvar skyblue2 "#3465a4")
(defvar skyblue3 "#204a87")
(defvar plum1 "#ad7fa8")
(defvar plum2 "#75507b")
(defvar plum3 "#5c3566")
(defvar chocolate1 "#e9b96e")
(defvar chocolate2 "#c17d11")
(defvar chocolate3 "#8f5902")
(defvar scarletred1 "#ef2929")
(defvar scarletred2 "#cc0000")
(defvar scarletred3 "#a40000")
(defvar aluminium1 "#eeeeec")
(defvar aluminium2 "#d3d7cf")
(defvar aluminium3 "#babdb6")
(defvar aluminium4 "#888a85")
(defvar aluminium5 "#555753")
(defvar aluminium6 "#2e3436")

(defun color-theme-oblivion ()
  (interactive)
  (color-theme-install
   `(color-theme-oblivion
     ;;; color-theme mapping
     ((foreground-color . ,aluminium2)
      (background-color . ,aluminium6)
      (background-mode . dark)
      (cursor-color . ,aluminium2))

     ;;; define some reusable faces that we can inherit from afterwards
     (strong-face ((t (:weight bold))))
     (warning-face ((t (:foreground ,orange3 :weight bold :underline t))))
     (error-face ((t (:foreground ,scarletred3 :weight bold :underline t))))

     ;;; basic coloring
     (default ((t (:foreground ,aluminium2))))
     (cursor
      ((t (:foreground ,aluminium2))))
     (escape-glyph-face ((t (:foreground ,butter2))))
     (fringe ((t (:foreground ,aluminium2 :background ,aluminium6))))
     (header-line ((t (:foreground ,aluminium2 :background ,aluminium5))))
     (highlight ((t (:background ,aluminium5))))

     ;; faces used by isearch
     (isearch ((t (:foreground ,aluminium6 :background ,aluminium1))))
     (isearch-fail ((t (:foreground ,aluminium2 :background ,scarletred3))))
     (lazy-highlight ((t (:foreground ,"black" :background ,butter1))))

     (menu ((t (:foreground ,aluminium2 :background ,aluminium6))))
     (minibuffer-prompt ((t (:foreground ,skyblue1))))
     (mode-line
      ((t (:foreground ,aluminium6 :background ,aluminium1))))
     (mode-line-buffer-id ((t (:inherit strong-face))))
     (mode-line-inactive
      ((t (:foreground ,aluminium2 :background ,aluminium5))))
     (region ((t (:foreground ,aluminium1 :background ,aluminium4))))
     (secondary-selection ((t (:foreground ,aluminium1 :background ,skyblue1))))
     (trailing-whitespace ((t (:background ,butter2))))
     (vertical-border ((t (:foreground ,aluminium2))))

     ;;; font lock
     (font-lock-builtin-face ((t (:foreground ,skyblue1))))
     (font-lock-comment-face ((t (:foreground ,aluminium4 :slant italic))))
     (font-lock-comment-delimiter-face ((t (:foreground ,aluminium4))))
     (font-lock-constant-face ((t (:foreground ,scarletred1))))
     (font-lock-doc-face ((t (:foreground ,aluminium4 :slant italic))))
     (font-lock-doc-string-face ((t (:foreground ,skyblue1))))
     (font-lock-function-name-face ((t (:foreground ,skyblue1))))
     (font-lock-keyword-face ((t (:inherit plum1))))
     (font-lock-negation-char-face ((t (:foreground ,chameleon1))))
     (font-lock-preprocessor-face ((t (:foreground ,scarletred1))))
     (font-lock-string-face ((t (:foreground ,butter2))))
     (font-lock-type-face ((t (:foreground , chameleon1))))
     (font-lock-variable-name-face ((t (:foreground ,chameleon1 :weight bold))))
     (font-lock-warning-face ((t (:inherit warning-face))))

     (c-annotation-face ((t (:inherit font-lock-constant-face))))

     ;; auto-complete
     (ac-candidate-face ((t (:background ,aluminium1 :foreground "black"))))
     (ac-selection-face ((t (:background ,skyblue2 :foreground ,aluminium1))))
     (popup-tip-face ((t (:background ,butter1 :foreground "black"))))
     (popup-scroll-bar-foreground-face ((t (:background ,skyblue1))))
     (popup-scroll-bar-background-face ((t (:background ,aluminium5))))
     (popup-isearch-match ((t (:background ,aluminium6 :foreground ,aluminium2))))

     ;; diff
     (diff-added ((t (:foreground ,chameleon1))))
     (diff-changed ((t (:foreground ,butter1))))
     (diff-removed ((t (:foreground ,scarletred1))))
     (diff-header ((t (:background ,aluminium5))))
     (diff-file-header
      ((t (:background ,skyblue3 :foreground ,aluminium1 :bold t))))

     ;; eshell
     (eshell-prompt ((t (:inherit strong-face))))
     (eshell-ls-archive ((t (:foreground ,scarletred1 :weight bold))))
     (eshell-ls-backup ((t (:inherit font-lock-comment))))
     (eshell-ls-clutter ((t (:inherit font-lock-comment))))
     (eshell-ls-directory ((t (:foreground ,skyblue1 :weight bold))))
     (eshell-ls-executable ((t (:foreground ,chameleon2 :weight bold))))
     (eshell-ls-unreadable ((t (:foreground ,aluminium2))))
     (eshell-ls-missing ((t (:inherit font-lock-warning))))
     (eshell-ls-product ((t (:inherit font-lock-doc))))
     (eshell-ls-special ((t (:inherit strong-face))))
     (eshell-ls-symlink ((t (:foreground ,plum1 :weight bold))))

     ;; flymake
     (flymake-errline ((t (:inherit error-face))))
     (flymake-warnline ((t (:inherit warning-face))))

     ;; flyspell
     (flyspell-duplicate ((t (:inherit warning-face))))
     (flyspell-incorrect ((t (:inherit error-face))))

     ;; hl-line-mode
     (hl-line-face ((t (:background ,aluminium5))))

     ;; ido-mode
     (ido-first-match ((t (:inherit strong-face))))
     (ido-only-match ((t (:inherit strong-face))))
     (ido-subdir ((t (:foreground ,aluminium3))))

     ;; js2-mode
     (js2-warning-face ((t (:underline ,orange1))))
     (js2-error-face ((t (:inherit error-face))))
     (js2-jsdoc-tag-face ((t (:foreground ,chameleon1))))
     (js2-jsdoc-type-face ((t (:foreground ,orange2))))
     (js2-jsdoc-value-face ((t (:foreground ,aluminium1 :weight bold))))
     (js2-function-param-face ((t (:foreground ,orange1 :slant italic))))
     (js2-jsdoc-html-tag-name-face ((t (:foreground ,skyblue1))))
     (js2-jsdoc-html-tag-delimiter-face ((t (:foreground ,skyblue1))))
     (js2-external-variable-face ((t (:foreground ,orange2))))

     ;; linum-mode
     (linum ((t (:foreground ,aluminium5 :background "#000000"))))

     ;; magit
     (magit-section-title ((t (:inherit strong-face))))
     (magit-branch ((t (:inherit strong-face))))

     ;; nxhtml
     (nxml-tag-delimiter ((t (:foreground ,skyblue1))))
     (nxml-tag-delimiter-face ((t (:foreground ,skyblue1))))

     ;; css-mode
     (css-property ((t (:inherit bold :foreground "#ffffff"))))
     (css-selector ((t (:foreground "#d3d7cf"))))
     
     ;; mumamo
     (mumamo-background-chunk-major ((t (:background ,aluminium6))))
     (mumamo-background-chunk-submode1 ((t (:background ,aluminium6))))
     (mumamo-background-chunk-submode2 ((t (:background ,aluminium6))))
     (mumamo-background-chunk-submode3 ((t (:background ,aluminium6))))
     (mumamo-background-chunk-submode4 ((t (:background ,aluminium6))))
     (mumamo-background-chunk-submode5 ((t (:background ,aluminium6))))

     ;; outline
     (outline-8 ((t (:inherit default))))
     (outline-7 ((t (:inherit outline-8 :height 1.0))))
     (outline-6 ((t (:inherit outline-7 :height 1.0))))
     (outline-5 ((t (:inherit outline-6 :height 1.0))))
     (outline-4 ((t (:inherit outline-5 :height 1.0))))
     (outline-3 ((t (:inherit outline-4 :height 1.0))))
     (outline-2 ((t (:inherit outline-3 :height 1.0))))
     (outline-1 ((t (:inherit outline-2 :height 1.0))))

     ;; show-paren
     (show-paren-mismatch ((t (:foreground ,aluminium1 :weight bold :background ,aluminium3))))
     (show-paren-match ((t (:foreground ,aluminium1 :weight bold :background ,aluminium3))))
    )
  )
)

(add-to-list 'color-themes '(color-theme-oblivion
                             "Oblivion"
                             "Sergey Ovechkin <me@sovechkin.com>"))

(provide 'color-theme-oblivion)

;;; color-theme-oblivion.el ends here.
