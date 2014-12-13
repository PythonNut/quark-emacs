(eval-when-compile
  (progn
    (require 'cl-lib)
    (require 'key-chord)
    (require 'evil)
    (require 'diminish)
    (require 'evil-easymotion)
    (require 'config-modes)))

(require 'config-scroll)
(require 'config-line-numbers)
(require 'config-parens)
(require 'config-ace-easymotion)
(require 'config-undo)
;; (load-library "config-ace-easymotion")

(eval-when-compile
  (progn
    (require 'key-chord)
    (require 'config-modes)))

(cl-macrolet
  ((autoload-multiple-cursors (func)
     `(autoload ,func "multiple-cursors")))
  (generate-calls-single autoload-multiple-cursors
    (
      'mc/mark-lines
      'mc/mark-next-lines
      'mc/mark-previous-lines
      'mc/unmark-next-like-this
      'mc/unmark-previous-like-this
      'mc/skip-to-previous-like-this
      'mc/mark-all-like-this
      'mc/mark-all-words-like-this
      'mc/mark-all-symbols-like-this
      'mc/mark-all-in-region
      'mc/mark-all-in-region-regexp
      'mc/mark-more-like-this-extended
      'mc/mmlte--up
      'mc/mmlte--down
      'mc/mmlte--left
      'mc/mmlte--right
      'mc/mark-all-like-this-dwim
      'mc/mark-all-dwim
      'mc/mark-all-like-this-in-defun
      'mc/mark-all-words-like-this-in-defun
      'mc/mark-all-symbols-like-this-in-defun
      'mc/add-cursor-on-click
      'mc/mark-sgml-tag-pair
      'mc/mark-pop
      'set-rectangular-region-anchor
      'rrm/switch-to-multiple-cursors
      'mc/insert-numbers
      'mc/reverse-regions
      'mc/sort-regions
      'hum/keyboard-quit
      'mc-hide-unmatched-lines-mode
      )))

(global-set-key (kbd "C-c l") 'mc/edit-lines)
(global-set-key (kbd "C-c a") 'mc/mark-all-like-this-dwim)
(global-set-key (kbd "C-c s") 'mc/mark-all-in-region)
(global-set-key (kbd "C-c M") 'mc/mark-pop)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)

;; directional window movement
(add-hook 'window-configuration-change-hook
  (lambda ()
    (windmove-default-keybindings 'meta)))

;; directional frame movement too
(add-hook 'before-make-frame-hook
  (lambda ()
    (windmove-default-keybindings 'meta)
    (unless (featurep 'framemove)
      (require 'framemove)
      (setq framemove-hook-into-windmove t))))

(global-set-key (kbd "C-.") 'er/expand-region)

(provide 'config-ui)
