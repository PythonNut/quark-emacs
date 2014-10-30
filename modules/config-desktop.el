;; Automatically save and restore sessions
(setq
  desktop-dirname             "~/.emacs.d/desktop/"
  desktop-base-file-name      "emacs.desktop"
  desktop-base-lock-name      "lock"
  desktop-path                (list desktop-dirname)
  desktop-save                t
  desktop-files-not-to-save   "^$" ;reload tramp paths
  desktop-load-locked-desktop nil)

(setq save-place-file "~/.emacs.d/saveplace")
(setq-default save-place t)
(require 'saveplace)

(setq savehist-file "~/.emacs.d/savehist")
(savehist-mode 1)
(setq history-length 100)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables
  '(kill-ring
     search-ring
     regexp-search-ring))


;; remember more recent files
(setq
  recentf-max-saved-items 200
  recentf-max-menu-items 30)
