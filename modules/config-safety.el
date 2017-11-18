;; -*- lexical-binding: t -*-

(defvar my/unnamed-autosave-location
  (locate-user-emacs-file "data/unnamed-autosave")
  "Directory in which to store auto-save files for non-file buffers,
when `auto-save-mode' is invoked manually.")

(setq backup-directory-alist
      `((,(rx (zero-or-more not-newline))
         . ,(expand-file-name (locate-user-emacs-file "data/backups/"))))
      auto-save-file-name-transforms
      `((,(rx (zero-or-more not-newline))
         ,(expand-file-name (locate-user-emacs-file "data/autosave/")) t)))

;; Use a unified directory for buffers that don't visit files
(defun nadvice/auto-save-mode (old-fun &rest args)
  "Use a standard location for auto-save files for non-file buffers"
  (if buffer-file-name
      (apply old-fun args)
    (let ((default-directory my/unnamed-autosave-location))
      (unless (file-directory-p default-directory)
        (mkdir default-directory))
      (apply old-fun args))))

(advice-add 'auto-save-mode :around #'nadvice/auto-save-mode)

(defun nadvice/recover-this-file (old-fun &rest args)
  "Restore BUFFER (or current buffer if omitted) from the autosave archive."
  (interactive)
  (if buffer-file-name
      (apply old-fun args)
    (let* ((base-file-name
            (cl-letf* ((default-directory my/unnamed-autosave-location)
                       ((symbol-function #'make-temp-file) (lambda (arg &rest _args) arg)))
              (make-auto-save-file-name)))
           (file-name-list
            (cl-sort (mapcar
                      (lambda (file)
                        (cons file
                              (nth 5 (file-attributes file))))
                      (file-expand-wildcards (concat base-file-name "*")))
                     #'>
                     :key (lambda (file)
                            (float-time (cdr file)))))
           (file (if (and (boundp 'my/last-restored-buffer)
                          (ignore-errors
                            (string= (car my/last-restored-buffer)
                                     (buffer-name))))
                     (catch 'found-autosave
                       (dolist (file file-name-list)
                         (cl-destructuring-bind
                             (_file-name . modification-time) file


                         (when (< (float-time modification-time)
                                  (cdr my/last-restored-buffer))
                           (setcdr my/last-restored-buffer
                                   (float-time modification-time))
                           (throw 'found-autosave file))))
                     (setq my/last-restored-buffer nil)
                     (user-error "No older autosave... Wrapping around."))
                 (defvar my/last-restored-buffer)
                 (setq my/last-restored-buffer
                       (cons (buffer-name)
                             (float-time (cdar file-name-list))))
                 (car file-name-list))))
    (cl-destructuring-bind (file-name . modification-time) file
      (widen)
      (erase-buffer)
      (goto-char (point-min))
      (insert-file-contents file-name)
      (message "File: %s (%s)"
               (replace-regexp-in-string (regexp-quote base-file-name)
                                         ""
                                         file-name)
               (format-time-string "%r %D" modification-time))))))

(advice-add 'recover-this-file :around #'nadvice/recover-this-file)

(defun my/save-buffer-maybe ()
  (when (and buffer-file-name
             (buffer-modified-p)
             (with-demoted-errors "%s"
               (file-writable-p buffer-file-name))
             (not (file-remote-p buffer-file-name)))
    (with-demoted-errors "%s"
      (save-buffer))))

;; automatically save buffers associated with files on buffer switch
;; and on windows switch
(defun nadvice/save-buffer-maybe (&rest _args)
  (my/save-buffer-maybe))

(advice-add 'switch-to-buffer :before #'nadvice/save-buffer-maybe)
(advice-add 'other-window     :before #'nadvice/save-buffer-maybe)

(use-package windmove
             :config
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
            (let ((inhibit-message t))
              (save-some-buffers t))))

(use-package autorevert
  :diminish auto-revert-mode
  :config
  (setq global-auto-revert-non-file-buffers t
        auto-revert-remote-files t
        auto-revert-verbose nil))

(defun my/auto-revert-onetime-setup ()
  (global-auto-revert-mode +1)
  (remove-hook 'find-file-hook
               #'my/auto-revert-onetime-setup))

(add-hook 'find-file-hook #'my/auto-revert-onetime-setup)

(use-package backup-walker
  :defer-install t
  :commands (backup-walker-start)
  :config
  (evil-set-initial-state 'backup-walker-mode 'motion))

(provide 'config-safety)
