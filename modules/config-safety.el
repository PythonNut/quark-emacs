;; -*- lexical-binding: t -*-

(defvar backup-location
  (expand-file-name "data/backups/" user-emacs-directory))
(defvar autosave-location
  (expand-file-name "data/autosave/" user-emacs-directory))
(defvar tramp-backup-directory
  (expand-file-name "data/tramp-backups/" user-emacs-directory))

(setq backup-directory-alist `((".*" . ,backup-location))
      auto-save-file-name-transforms `((".*" ,autosave-location t))
      tramp-backup-directory `((".*" . ,tramp-backup-directory)))

(defun my/save-buffer-maybe ()
  (with-current-buffer (current-buffer)
    (let ((buffer-file-name (buffer-file-name (current-buffer))))
      (when (and buffer-file-name
                 (buffer-modified-p (current-buffer))
                 (ignore-errors
                   (file-writable-p buffer-file-name))
                 (not (file-remote-p buffer-file-name)))
        (with-demoted-errors "%s"
          (save-buffer))))))

;; automatically save buffers associated with files on buffer switch
;; and on windows switch
(defun nadvice/save-buffer-maybe (&rest _args)
  (my/save-buffer-maybe))

(advice-add 'switch-to-buffer :before #'nadvice/save-buffer-maybe)
(advice-add 'other-window     :before #'nadvice/save-buffer-maybe)

(with-eval-after-load 'windmove
  (advice-add 'windmove-up    :before #'nadvice/save-buffer-maybe)
  (advice-add 'windmove-down  :before #'nadvice/save-buffer-maybe)
  (advice-add 'windmove-left  :before #'nadvice/save-buffer-maybe)
  (advice-add 'windmove-right :before #'nadvice/save-buffer-maybe))

;; save backups too
(setq version-control t ;; Use version numbers for backups
      kept-new-versions 30  ;; Number of newest versions to keep
      kept-old-versions 0   ;; Number of oldest versions to keep
      delete-old-versions t ;; Don't Ask to delete excess backup versions
      backup-by-copying t   ;; Copy linked files, don't rename.
      backup-by-copying-when-linked t ;; copy links too
      auto-save-timeout 3    ;; auto-save after 10s of idle time
      auto-save-interval 200 ;; auto-save after 200 chars
      vc-make-backup-files t ;; because we don't commit every save
      )

(setq-default auto-save-default t)

(defun my/force-backup-of-buffer ()
  (setq buffer-backed-up nil))

(add-hook 'before-save-hook #'my/force-backup-of-buffer)

;; save buffers on blur
(add-hook 'focus-out-hook
          (lambda ()
            (cl-letf (((symbol-function 'message) #'format))
              (unless (file-remote-p default-directory)
                (save-some-buffers t)))))

(with-eval-after-load 'autorevert
  (eval-when-compile
    (with-demoted-errors "Load error: %s"
      (require 'autorevert)))
  (setq global-auto-revert-non-file-buffers t
        auto-revert-verbose nil))

(defun my/auto-revert-onetime-setup ()
  (global-auto-revert-mode +1)
  (remove-hook 'find-file-hook
               #'my/auto-revert-onetime-setup))

(add-hook 'find-file-hook #'my/auto-revert-onetime-setup)

(eval-when-compile
  (with-demoted-errors "Load error: %s"
    (require 'config-package)))

(package-deferred-install 'backup-walker
    :autoload-names '('backup-walker-start))

(provide 'config-safety)
