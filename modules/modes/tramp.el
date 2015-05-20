(eval-when-compile
  (with-demoted-errors
    (require 'tramp)))

(with-eval-after-load 'tramp
  ;; cache passwords for the duration of the session
  ;; note that said cache is _not_ persistent
  (setq
    password-cache-expiry nil
    tramp-default-method "scp"))

;; =================================
;; automatically request root access
;; =================================

(defun my-edit-file-as-root ()
  "Find file as root"
  (interactive)
  (require 'tramp)
  (let*
    ((sudo (= (process-file "sudo" nil nil "-n" "true") 0))
      (file-name
        (if (tramp-tramp-file-p buffer-file-name)
          (with-parsed-tramp-file-name buffer-file-name parsed
            (tramp-make-tramp-file-name
              (if sudo "sudo" "su")
              "root"
              parsed-host
              parsed-localname
              (let ((tramp-postfix-host-format "|")
                     (tramp-prefix-format))
                (tramp-make-tramp-file-name
                  parsed-method
                  parsed-user
                  parsed-host
                  ""
                  parsed-hop))))
          (concat (if sudo
                    "/sudo::"
                    "/su::")
            buffer-file-name))))
    (find-alternate-file file-name)))

(defun my-edit-file-as-root-maybe ()
  "Find file as root if necessary."
  (unless
    (and
      buffer-file-name
      (file-writable-p buffer-file-name))
    (when
      (and (not (string= user-login-name
                  (nth 3 (file-attributes buffer-file-name 'string))))
        (y-or-n-p "File is not writable. Open with root? "))
      (my-edit-file-as-root))))

(add-hook 'find-file-hook #'my-edit-file-as-root-maybe)
