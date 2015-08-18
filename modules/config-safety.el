(eval-when-compile
  (with-demoted-errors
    (require 'autorevert)))

(defvar backup-location (concat user-emacs-directory "data/backups"))
(defvar autosave-location (concat user-emacs-directory "data/autosave"))
(defvar tramp-backup-directory (concat user-emacs-directory "data/tramp-backups"))

(setq backup-directory-alist
  `((".*" . ,backup-location)))

(setq auto-save-file-name-transforms
  `((".*" ,autosave-location t)))

(defun save-buffer-maybe ()
  (when (and
          buffer-file-name
          (buffer-modified-p)
          (file-writable-p buffer-file-name)
          (not (file-remote-p default-directory)))
    (save-buffer)))

;; automatically save buffers associated with files on buffer switch
;; and on windows switch
(defun nadvice/save-buffer-maybe (&rest args)
  (save-buffer-maybe))

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
  auto-save-timeout 10   ;; auto-save after 10s of idle time
  auto-save-interval 200 ;; auto-save after 200 chars
  vc-make-backup-files t ;; because we don't commit every save
  )

(setq-default auto-save-default t)

(defun force-backup-of-buffer ()
  (setq buffer-backed-up nil))

(add-hook 'before-save-hook #'force-backup-of-buffer)

;; save buffers on blur
(add-hook 'focus-out-hook
  (lambda ()
    (cl-letf (((symbol-function 'message) #'format))
      (unless (file-remote-p default-directory)
        (save-some-buffers t)))))

(defun auto-revert-onetime-setup ()
  (global-auto-revert-mode +1)
  (remove-hook 'find-file-hook
    #'auto-revert-onetime-setup))

(add-hook 'find-file-hook #'auto-revert-onetime-setup)

(provide 'config-safety)
