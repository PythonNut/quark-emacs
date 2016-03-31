;; -*- lexical-binding: t -*-

(eval-when-compile
  (with-demoted-errors "Load error: %s"
    (require 'tramp)))

(defvar my/tramp-backup-directory
  (locate-user-emacs-file "data/tramp-backups/"))

(with-eval-after-load 'password-cache
  (eval-when-compile
    (with-demoted-errors "Load error: %s"
      (require 'password-cache)))
  ;; cache passwords for the duration of the session
  ;; note that said cache is _not_ persistent
  (setq password-cache-expiry nil))

(with-eval-after-load 'tramp
  (with-eval-after-load 'tramp-cache
    (eval-when-compile
      (with-demoted-errors "Load error: %s"
        (require 'tramp-cache)))
    (setq tramp-persistency-file-name
          (locate-user-emacs-file "/data/tramp")))

  (setq tramp-default-method "scp"
        tramp-backup-directory-alist `((".*" . ,my/tramp-backup-directory))))

;; =================================
;; automatically request root access
;; =================================
(defun my/root-file-name-p (file-name)
  (and (featurep 'tramp)
       (tramp-tramp-file-p file-name)
       (with-parsed-tramp-file-name file-name parsed
         (string= "root" (substring-no-properties parsed-user)))))

(defun my/make-root-file-name (file-name)
  (require 'tramp)
  (let ((sudo (let ((default-directory
                      (file-name-directory file-name))
                    (process-file-side-effects nil))
                (with-demoted-errors "sudo check failed: %s"
                  (or (= (process-file "sudo" nil nil nil "-n" "true") 0)
                      ;; Detect if sudo can be run with a password
                      (string-match-p
                       "\\(askpass\\|password\\)"
                       (with-output-to-string
                         (with-current-buffer standard-output
                           (process-file "sudo" nil t nil "-vS")))))))))
    (if (tramp-tramp-file-p file-name)
        (with-parsed-tramp-file-name file-name parsed
          (tramp-make-tramp-file-name
           (if sudo "sudo" "su")
           "root"
           parsed-host
           parsed-localname
           (let ((tramp-postfix-host-format "|")
                 (tramp-prefix-format))
             (tramp-make-tramp-file-name
              (if (string= "scp" parsed-method)
                  "ssh"
                parsed-method)
              parsed-user
              parsed-host
              ""
              parsed-hop))))
      (concat (if sudo "/sudo::" "/su::")
              file-name))))

(defun edit-file-as-root ()
  "Find file as root"
  (interactive)
  (find-alternate-file (my/make-root-file-name buffer-file-name)))

(defun my/edit-file-as-root-maybe ()
  "Find file as root if necessary."
  (when (and buffer-file-name
             (not (file-writable-p buffer-file-name))
             (not (string= user-login-name
                           (nth 3 (file-attributes buffer-file-name 'string))))
             (not (my/root-file-name-p buffer-file-name)))
    (setq buffer-read-only nil)
    (add-hook 'first-change-hook #'root-save-mode nil t)
    (run-with-idle-timer
     0.5 nil
     (lambda ()
       (message "Modifications will require root permissions to save.")))))

(add-hook 'find-file-hook #'my/edit-file-as-root-maybe)

;; also fallback to root if file cannot be read
(defun nadvice/find-file-noselect-1 (old-fun buf filename &rest args)
  (condition-case err
      (apply old-fun buf filename args)
    (file-error
     (if (and (not (my/root-file-name-p filename))
              (y-or-n-p "File is not readable. Open with root? "))
         (let ((filename (my/make-root-file-name (file-truename filename))))
           (apply #'find-file-noselect-1
                  (or (get-file-buffer filename)
                      (create-file-buffer filename))
                  filename
                  args))
       (signal (car err) (cdr err))))))

(advice-add #'find-file-noselect-1 :around #'nadvice/find-file-noselect-1)

(defun nadvice/semantic-find-file-noselect (old-fun &rest args)
  (cl-letf* ((old-aff (symbol-function #'after-find-file))
             ((symbol-function #'after-find-file)
              (lambda (&rest args)
                (let ((find-file-hook))
                  (apply old-aff args)))))
    (apply old-fun args)))

(advice-add #'semantic-find-file-noselect :around
            #'nadvice/semantic-find-file-noselect)

(defvar root-save-mode-lighter
  (list " " (propertize "root" 'face 'tty-menu-selected-face))
  "The mode line lighter for root-save-mode.")

;; Required for the face to be displayed
(put 'root-save-mode-lighter 'risky-local-variable t)

(defun root-save-mode/before-save ()
  "Switch the visiting file to a TRAMP su or sudo name if applicable"
  (when (and (buffer-modified-p)
             (not (my/root-file-name-p buffer-file-name))
             (or (not (= (process-file "sudo" nil nil nil "-n" "true") 0))
                 (yes-or-no-p "File is not writable. Save with root? ")))
    (let ((change-major-mode-with-file-name nil))
      (set-visited-file-name (my/make-root-file-name buffer-file-name) t t))
    (remove-hook 'before-save-hook #'root-save-mode/before-save t)))

(defun nadvice/find-file-noselect (old-fun &rest args)
  (cl-letf* ((old-fwp (symbol-function #'file-writable-p))
             ((symbol-function #'file-writable-p)
              (lambda (&rest iargs)
                (or (member 'root-save-mode first-change-hook)
                    (bound-and-true-p root-save-mode)
                    (apply old-fwp iargs)))))
    (apply old-fun args)))

(advice-add 'find-file-noselect :around #'nadvice/find-file-noselect)

(define-minor-mode root-save-mode
  "Automatically save buffer as root"
  :lighter root-save-mode-lighter
  (if root-save-mode
      ;; Ensure that root-save-mode is visible by promoting it to rank 1
      (progn
        (let ((root-save-mode-alist-entry
               (assoc 'root-save-mode minor-mode-alist)))
          (setq minor-mode-alist
                (delete root-save-mode-alist-entry minor-mode-alist))
          (push root-save-mode-alist-entry minor-mode-alist))
        (add-hook 'before-save-hook #'root-save-mode/before-save nil t))
    (remove-hook 'before-save-hook #'root-save-mode/before-save t)))

(provide 'config-tramp)
