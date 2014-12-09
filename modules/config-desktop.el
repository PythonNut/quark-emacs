;; Automatically save and restore sessions

(eval-when-compile
  (progn
    (require 'recentf)
    (require 'desktop)
    (require 'saveplace)
    (require 'savehist)))

(setq
  desktop-dirname             "~/.emacs.d/desktop/"
  desktop-base-file-name      "emacs.desktop"
  desktop-base-lock-name      "lock"
  desktop-path                (list desktop-dirname)
  desktop-save                t
  desktop-files-not-to-save   "^$" ;reload tramp paths
  desktop-load-locked-desktop nil)

(setq save-place-file "~/.emacs.d/.saveplace")
(setq-default save-place t)

(add-hook 'find-file-hook
  (lambda ()
    (require 'saveplace)))

(setq
  savehist-file "~/.emacs.d/.savehist"
  savehist-autosave-interval 60
  history-length 100
  history-delete-duplicates t
  savehist-save-minibuffer-history 1
  savehist-additional-variables
  '(kill-ring
     search-ring
     regexp-search-ring))

(savehist-mode 1)

;; text properties severely bloat the kill-ring so delete them
(defun unpropertize-kill-ring ()
  (setq kill-ring (mapcar 'substring-no-properties kill-ring)))

(add-hook 'kill-emacs-hook 'unpropertize-kill-ring)

;; remember more recent files
(setq
  recentf-save-file (concat user-emacs-directory ".recentf")
  recentf-max-saved-items 200
  recentf-max-menu-items 30)

(provide 'config-desktop)
