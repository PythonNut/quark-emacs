;; Automatically save and restore sessions

(eval-when-compile
  (with-demoted-errors
    (require 'cl-lib)
    (require 'key-chord)
    (require 'recentf)
    (require 'saveplace)
    (require 'savehist)
    (require 'helm-grep)
    (require 'evil-ex)
    (require 'config-modes)))

(setq save-place-file (concat user-emacs-directory ".saveplace"))
(setq-default save-place t)

(require 'saveplace)

(setq
  savehist-file (concat user-emacs-directory ".savehist")
  savehist-autosave-interval 180
  history-length 100
  history-delete-duplicates t
  savehist-save-minibuffer-history t
  savehist-additional-variables
  '(kill-ring
     search-ring
     regexp-search-ring)
  )

(require 'savehist)
(savehist-mode 1)

;; text properties severely bloat the history so delete them
(defun unpropertize-savehist ()
  (cl-macrolet
    ((unpropertize-list (list)
       `(with-demoted-errors
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

(defadvice recentf-cleanup (around quiet activate preactivate compile)
  (cl-letf (((symbol-function 'message) #'format))
    ad-do-it))

(provide 'config-desktop)
