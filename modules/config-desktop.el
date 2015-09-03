;; Automatically save and restore sessions

(eval-when-compile
  (with-demoted-errors
    (require 'dash)
    (require 'cl-lib)
    (require 'recentf)
    (require 'saveplace)
    (require 'savehist)
    (require 'helm-grep)
    (require 'evil-ex)
    (require 'config-modes)))

(defvar file-name-mode-alist nil)

(setq
  history-length 100
  history-delete-duplicates t

  save-place-file (concat user-emacs-directory ".saveplace")

  savehist-file (concat user-emacs-directory ".savehist")
  savehist-autosave-interval 180
  savehist-save-minibuffer-history t
  savehist-additional-variables
  '(kill-ring
     file-name-mode-alist
     search-ring
     regexp-search-ring)

  ;; remember more recent files
  recentf-save-file (concat user-emacs-directory ".recentf")
  recentf-max-saved-items 200
  recentf-max-menu-items 30)

(if (fboundp 'save-place-mode)
  (save-place-mode +1)
  (setq-default save-place t))

(require 'savehist)
(savehist-mode +1)

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

(defun nadvice/recentf-quiet (old-fun &rest args)
  (cl-letf (((symbol-function 'message) #'format))
    (apply old-fun args)))

(advice-add 'recentf-cleanup :around #'nadvice/recentf-quiet)

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

(with-eval-after-load 'desktop
  (setq desktop-path '("~/.emacs.d/desktop"))
  (setq desktop-dirname "~/.emacs.d/desktop")
  (setq desktop-base-file-name "emacs-desktop"))

(defun desktop-autosave ()
  (cl-letf (((symbol-function 'message) #'format)
             ((symbol-function 'y-or-n-p) (lambda (prompt) t)))
    (desktop-save-in-desktop-dir)))

(add-hook 'auto-save-hook 'desktop-autosave)

(provide 'config-desktop)
