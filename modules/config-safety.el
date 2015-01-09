(eval-when-compile
  (with-demoted-errors
    (require 'cl)
    (require 'auto-save)
    (require 'autorevert)))

(defvar backup-location "~/.emacs.d/data/backups")
(defvar autosave-location "~/.emacs.d/data/autosave")
(defvar backup-directory "~/.emacs.d/data/backups")
(defvar tramp-backup-directory "~/.emacs.d/data/tramp-backups")

(setq backup-directory-alist
  `((".*" . ,backup-location)))

(setq auto-save-file-name-transforms
  `((".*" ,autosave-location t)))

(auto-save-mode +1)

(defun save-buffer-maybe ()
  (and
    (buffer-modified-p)
    buffer-file-name
    (save-buffer)))

;; automatically save buffers associated with files on buffer switch
;; and on windows switch
(defadvice switch-to-buffer
  (before save-buffer-maybe activate preactivate compile) (save-buffer-maybe))
(defadvice other-window
  (before save-buffer-maybe activate preactivate compile) (save-buffer-maybe))
(defadvice windmove-up
  (before save-buffer-maybe activate preactivate compile) (save-buffer-maybe))
(defadvice windmove-down
  (before save-buffer-maybe activate preactivate compile) (save-buffer-maybe))
(defadvice windmove-left
  (before save-buffer-maybe activate preactivate compile) (save-buffer-maybe))
(defadvice windmove-right
  (before save-buffer-maybe activate preactivate compile) (save-buffer-maybe))

;; save backups too
(setq version-control t ;; Use version numbers for backups
  kept-new-versions 30  ;; Number of newest versions to keep
  kept-old-versions 0   ;; Number of oldest versions to keep
  delete-old-versions t ;; Don't Ask to delete excess backup versions
  backup-by-copying t   ;; Copy linked files, don't rename.
  backup-by-copying-when-linked t ;; copy links too
  auto-save-default t    ;; also auto-save
  auto-save-timeout 10   ;; auto-save after 10s of idle time
  auto-save-interval 200 ;; auto-save after 200 chars
  vc-make-backup-files t ;; because we don't commit every save
  )

(defun force-backup-of-buffer ()
  (setq buffer-backed-up nil))

(add-hook 'before-save-hook #'force-backup-of-buffer)

;; save buffers on blur
(add-hook 'focus-out-hook
  (lambda ()
    (cl-letf (((symbol-function 'message) #'format))
      (save-some-buffers t))))

(defun auto-revert-onetime-setup ()
  (setq
    auto-revert-use-notify t)
  (global-auto-revert-mode +1)
  (remove-hook 'find-file-hook
    #'auto-revert-onetime-setup))

(add-hook 'find-file-hook #'auto-revert-onetime-setup)

(provide 'config-safety)
