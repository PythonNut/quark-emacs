;; Automatically save and restore sessions

(eval-when-compile
  (require 'key-chord)
  (require 'recentf)
  (require 'desktop)
  (require 'saveplace)
  (require 'savehist)
  (require 'helm-grep)
  (require 'evil-ex)
  (require 'config-modes))

(setq
  desktop-dirname "~/.emacs.d/desktop/"
  desktop-base-file-name "emacs.desktop"
  desktop-base-lock-name "lock"
  desktop-path (list desktop-dirname)
  desktop-save t
  desktop-files-not-to-save "^$" ;reload tramp paths
  desktop-load-locked-desktop nil)

(setq save-place-file "~/.emacs.d/.saveplace")
(setq-default save-place t)

(require 'saveplace)

(setq
  savehist-file "~/.emacs.d/.savehist"
  savehist-autosave-interval 60
  history-length 100
  history-delete-duplicates t
  savehist-save-minibuffer-history t
  savehist-additional-variables
  '(kill-ring
     search-ring
     regexp-search-ring))

(savehist-mode 1)

;; text properties severely bloat the history so delete them
(defun unpropertize-savehist ()
  (cl-macrolet
    ((unpropertize-list (list)
       `(ignore-errors
          (setq ,list
            (mapcar #'substring-no-properties ,list)))))

    (generate-calls-single unpropertize-list
      (
        kill-ring
        minibuffer-history
        helm-grep-history
        file-name-history
        read-expression-history
        extended-command-history
        evil-ex-history))))

(add-hook 'kill-emacs-hook #'unpropertize-savehist)
(add-hook 'savehist-save-hook #'unpropertize-savehist)

;; remember more recent files
(setq
  recentf-save-file (concat user-emacs-directory ".recentf")
  recentf-max-saved-items 200
  recentf-max-menu-items 30)


(provide 'config-desktop)
