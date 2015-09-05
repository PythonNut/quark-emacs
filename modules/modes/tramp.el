(eval-when-compile
  (with-demoted-errors
    (require 'tramp)))

(with-eval-after-load 'tramp
  ;; cache passwords for the duration of the session
  ;; note that said cache is _not_ persistent
  (setq
    password-cache-expiry nil
    tramp-default-method "ssh"))

;; =================================
;; automatically request root access
;; =================================
(defun my/root-file-name-p (file-name)
  (and
    (featurep 'tramp)
    (tramp-tramp-file-p file-name)
    (with-parsed-tramp-file-name file-name parsed
      (string= "root"
        (substring-no-properties parsed-user)))))

(defun my/make-root-file-name (file-name)
  (require 'tramp)
  (let ((sudo (let ((default-directory
                      (file-name-directory file-name)))
                (= (process-file "sudo" nil nil nil "-n" "true") 0))))
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
  (find-alternate-file
    (my/make-root-file-name buffer-file-name)))

(defun my/edit-file-as-root-maybe ()
  "Find file as root if necessary."
  (when (and
          buffer-file-name
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
      (if (and
            (not (my/root-file-name-p filename))
            (y-or-n-p "File is not readable. Open with root? "))
        (setq ad-return-value
          (let ((filename (my/make-root-file-name (file-truename filename))))
            (apply #'find-file-noselect-1
              (or
                (get-file-buffer filename)
                (create-file-buffer filename))
              filename
              args)))
        (signal 'file-error (list "File is not readable" filename))))))

(advice-add #'find-file-noselect-1 :around #'nadvice/find-file-noselect-1)
