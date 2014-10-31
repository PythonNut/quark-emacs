(eval-when-compile (require 'cl))

(load-library "config-scroll")
(load-library "config-line-numbers")
(load-library "config-parens")
(load-library "config-ace-easymotion")
(load-library "config-icicles")
(load-library "config-undo")

(require 'multiple-cursors)
(global-set-key (kbd "C-c l") 'mc/edit-lines)
(global-set-key (kbd "C-c a") 'mc/mark-all-like-this-dwim)
(global-set-key (kbd "C-c m") 'mc/mark-more-like-this-extended)
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
    (unless (fboundp 'framemove)
      (require 'framemove)
      (setq framemove-hook-into-windmove t))))
