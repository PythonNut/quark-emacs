;; -*- lexical-binding: t -*-
(eval-when-compile
  (require 'config-macros)
  (require 'config-package))

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

(with-eval-after-load 'tramp-cache
  (eval-when-compile (require 'tramp-cache))
  (setq tramp-persistency-file-name
        (locate-user-emacs-file "data/tramp")))

(with-eval-after-load 'tramp
  (eval-when-compile (require 'tramp))
  (setq tramp-completion-use-auth-sources nil)
  ;; Define a rsyncx method analogous to scpx
  (add-to-list 'tramp-methods
               `("rsyncx"
                 (tramp-login-program "ssh")
                 (tramp-login-args
                  (("-l" "%u")
                   ("-p" "%p")
                   ("%c")
                   ("-e" "none")
                   ("-t" "-t")
                   ("-o" "RemoteCommand=\"%l\"")
                   ("%h")))
                 (tramp-async-args (("-q")))
                 (tramp-remote-shell "/bin/sh")
                 (tramp-remote-shell-login ("-l"))
                 (tramp-remote-shell-args ("-c"))
                 (tramp-copy-program
                  ,(cond
                    ((and (eq system-type 'darwin)
                          (file-executable-p "/opt/homebrew/bin/rsync"))
                     "/opt/homebrew/bin/rsync")
                    ((and (eq system-type 'darwin)
                          (file-executable-p "/usr/local/bin/rsync"))
                     "/usr/local/bin/rsync")
                    (t "rsync")))
                 (tramp-copy-args (("-t" "%k") ("-p") ("-r") ("-s") ("-c")))
                 (tramp-copy-env (("RSYNC_RSH") ("ssh") ("%c")))
                 (tramp-copy-keep-date t)
                 (tramp-copy-keep-tmpfile t)
                 (tramp-copy-recursive t)))

  (define-advice tramp-read-passwd
      (:around (old-fun &rest args) disable-auth-sources )
    (let ((auth-sources))
      (apply old-fun args)))

  (setq tramp-backup-directory-alist `((,(rx (zero-or-more anything))
                                        . ,my/tramp-backup-directory))))

(with-eval-after-load 'tramp-sh
  (eval-when-compile (require 'cl-lib))
  (setq tramp-use-ssh-controlmaster-options
        (eval-when-compile
          (not
           (let ((config
                  (with-output-to-string
                    (with-current-buffer
                        standard-output
                      (call-process "ssh" nil '(t nil) nil
                                    "-G"
                                    "quark-emacs-nonexistent-host")))))
             (string-match-p (rx bol "controlmaster auto" eol) config)))))

  (define-advice tramp-do-copy-or-rename-file-directly
      (:filter-args (args) no-preserve-uid-gid-msdos )
    (cl-destructuring-bind
        (op filename newname ok-if-already-exists keep-date preserve-uid-gid)
        args
      (list op filename newname ok-if-already-exists keep-date
            (unless (my/msdos-fs (if (tramp-tramp-file-p newname)
                                     (file-remote-p newname 'localname)
                                   newname))
              preserve-uid-gid)))))

;; =================================
;; automatically request root access
;; =================================

(use-package su
  :recipe (su :type git :host github :repo "PythonNut/su.el")
  :init
  (eval-when-compile
    (with-demoted-errors "Load error: %s"
      (require 'el-patch)))

  (el-patch-feature su)

  (el-patch-defcustom su-auto-make-directory t
    "Automatically become other users to create directories"
    :type 'boolean
    :group 'su)

  (el-patch-defcustom su-auto-write-file t
    "Automatically become other users to write files"
    :type 'boolean
    :group 'su)

  (el-patch-defcustom su-auto-read-file t
    "Automatically become other users to read files"
    :type 'boolean
    :group 'su)

  (autoload #'su--nadvice-make-directory-auto-root "su")
  (autoload #'su--nadvice-find-file-noselect "su")
  (autoload #'su--nadvice-supress-find-file-hook "su")
  (autoload #'su--nadvice-find-file-noselect-1 "su")

  (el-patch-define-minor-mode su-mode
    "Automatically read and write files as users"
    :init-value nil
    :group 'su
    :global t
    (if su-mode
        (progn
          (when su-auto-make-directory
            (advice-add 'basic-save-buffer :around
                        #'su--nadvice-make-directory-auto-root))

          (when su-auto-write-file
            (add-hook 'find-file-hook #'su--edit-file-as-root-maybe)
            (advice-add 'find-file-noselect :around
                        #'su--nadvice-find-file-noselect))

          (when su-auto-read-file
            (advice-add 'find-file-noselect-1 :around
                        #'su--nadvice-find-file-noselect-1)))

      (remove-hook 'find-file-hook #'su--edit-file-as-root-maybe)
      (advice-remove 'basic-save-buffer
                     #'su--nadvice-make-directory-auto-root)
      (advice-remove 'find-file-noselect
                     #'su--nadvice-find-file-noselect)
      (advice-remove 'find-file-noselect-1
                     #'su--nadvice-find-file-noselect-1)))

  (el-patch-define-minor-mode su-helm-integration-mode
    "Enable su-mode integration with helm."
    :init-value nil
    :group 'su
    :global t
    (if su-helm-integration-mode
        (advice-add 'helm-find-file-or-marked :around
                    #'su--nadvice-make-directory-auto-root)
      (advice-remove 'helm-find-file-or-marked
                     #'su--nadvice-make-directory-auto-root)))

  (su-mode +1)

  (with-eval-after-load 'helm-files
    (require 'su)
    (su-helm-integration-mode +1))
  (with-eval-after-load 'semantic/fw
    (require 'su)
    (su-semantic-integration-mode +1))

  :config
  (eval-when-compile
    (with-demoted-errors "Load error: %s"
      (require 'el-patch)))

  (defun nadvice/su-disable-maybe-setup (flag)
    (if (and (not flag) (bound-and-true-p su-auto-save-mode))
        (su-auto-save-mode -1)))

  (el-patch-define-minor-mode su-auto-save-mode
    "Automatically save buffer as root"
    :lighter su-auto-save-mode-lighter
    (if su-auto-save-mode
        ;; Ensure that su-auto-save-mode is visible by moving it to the
        ;; beginning of the minor mode list
        (progn
          (el-patch-add
            (advice-add 'set-buffer-modified-p :before
                        #'nadvice/su-disable-maybe-setup))
          (let ((su-auto-save-mode-alist-entry
                 (assoc 'su-auto-save-mode minor-mode-alist)))
            (setq minor-mode-alist
                  (delete su-auto-save-mode-alist-entry minor-mode-alist))
            (push su-auto-save-mode-alist-entry minor-mode-alist))
          (add-hook 'before-save-hook #'su--before-save-hook nil t))

      (el-patch-add
        (advice-remove 'set-buffer-modified-p
                       #'nadvice/su-disable-maybe-setup))
      (remove-hook 'before-save-hook #'su--before-save-hook t))))

(defun tramp-switch-method (method &optional file-name)
  (interactive (list
                (completing-read "Choose method: "
                                 (mapcar #'car tramp-methods))))
  (find-alternate-file
   (with-parsed-tramp-file-name (or file-name buffer-file-name) parsed
     (tramp-make-tramp-file-name
      method
      parsed-user
      parsed-domain
      parsed-host
      parsed-port
      parsed-localname
      parsed-hop))))

(provide 'config-tramp)
