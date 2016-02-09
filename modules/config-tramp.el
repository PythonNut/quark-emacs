;; -*- lexical-binding: t -*-

(eval-when-compile
  (with-demoted-errors "Load error: %s"
    (require 'tramp)))

(defvar my/tramp-backup-directory
  (expand-file-name "data/tramp-backups/" user-emacs-directory))

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
          (expand-file-name "/data/tramp" user-emacs-directory)))

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
                (or (= (process-file "sudo" nil nil nil "-n" "true") 0)
                    ;; Detect if sudo can be run with a password
                    (string-match-p
                     "askpass"
                     (with-output-to-string
                       (with-current-buffer standard-output
                         (process-file "sudo" nil t nil "-v"))))))))
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
             (not (my/root-file-name-p buffer-file-name))
             (y-or-n-p "File is not writable. Open with root? "))
    (edit-file-as-root)))

(add-hook 'find-file-hook #'my/edit-file-as-root-maybe)

;; also fallback to root if file cannot be read
(defun nadvice/find-file-noselect-1 (old-fun buf filename &rest args)
  (condition-case nil
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
       (signal 'file-error (list "File is not readable" filename))))))

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

(provide 'config-tramp)
