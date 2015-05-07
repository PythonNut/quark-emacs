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

(defvar file-name-mode-alist '())

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
     file-name-mode-alist
     search-ring
     regexp-search-ring))

(require 'savehist)
(savehist-mode 1)

;; text properties severely bloat the history so delete them
(defun unpropertize-savehist ()
  (mapc (lambda (list)
          (with-demoted-errors
            (when (boundp list)
              (set list (mapcar #'substring-no-properties (eval list))))))
    '(
       kill-ring
       minibuffer-history
       helm-grep-history
       helm-ff-history
       file-name-history
       read-expression-history
       extended-command-history
       evil-ex-history)))

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

(setq auto-mode-alist (append auto-mode-alist file-name-mode-alist))

(defun my-compress-alist (alist)
  "Remove shadowed keys from alist"
  (let ((result))
    (mapc (lambda (elem)
            (unless (assoc (car elem) result)
              (setq result (cons elem result))))
      alist)
    (nreverse result)))

(add-hook 'savehist-save-hook
  (lambda ()
    (require 'dash)
    (setq file-name-mode-alist
      (-take history-length
        (my-compress-alist file-name-mode-alist)))))

(add-hook 'after-change-major-mode-hook
  (lambda ()
    (when (and
            buffer-file-name
            (not
              (file-name-extension
                buffer-file-name)))
      (setq file-name-mode-alist
        (cons
          (cons buffer-file-name major-mode)
          file-name-mode-alist))
      (setq auto-mode-alist
        (append auto-mode-alist
          (list (cons buffer-file-name major-mode)))))))

(provide 'config-desktop)
