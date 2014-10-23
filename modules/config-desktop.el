;; Automatically save and restore sessions
(setq
  desktop-dirname             "~/.emacs.d/desktop/"
  desktop-base-file-name      "emacs.desktop"
  desktop-base-lock-name      "lock"
  desktop-path                (list desktop-dirname)
  desktop-save                t
  desktop-files-not-to-save   "^$" ;reload tramp paths
  desktop-load-locked-desktop nil)

(desktop-save-mode +1)
