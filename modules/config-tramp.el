;; -*- lexical-binding: t -*-
(eval-when-compile (require 'config-macros))

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
  (setq tramp-backup-directory-alist `((,(rx (zero-or-more not-newline))
                                        . ,my/tramp-backup-directory))))

;; =================================
;; automatically request root access
;; =================================

(use-package su
  :recipe (su :type git :host github :repo "PythonNut/su.el")
  :init
  (eval-when-compile
    (with-demoted-errors "Load error: %s"
      (require 'el-patch)))

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

  (el-patch-defcustom su-enable-helm-integration t
    "Enable integration with helm"
    :type 'boolean
    :group 'su)

  (el-patch-defcustom su-enable-semantic-integration t
    "Enable integration with semantic"
    :type 'boolean
    :group 'su)

  (autoload #'su--nadvice-make-directory-auto-root "su")
  (autoload #'su--nadvice/find-file-noselect "su")
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
                        #'su--nadvice-make-directory-auto-root)

            (when su-enable-helm-integration
              (with-eval-after-load 'helm-files
                (advice-add 'helm-find-file-or-marked :around
                            #'su--nadvice-make-directory-auto-root))))

          (when su-auto-write-file
            (add-hook 'find-file-hook #'su--edit-file-as-root-maybe)
            (advice-add 'find-file-noselect :around
                        #'su--nadvice/find-file-noselect)

            (when su-enable-semantic-integration
              (with-eval-after-load 'semantic/fw
                (advice-add 'semantic-find-file-noselect :around
                            #'su--nadvice-supress-find-file-hook))))

          (when su-auto-read-file
            (advice-add 'find-file-noselect-1 :around
                        #'su--nadvice-find-file-noselect-1)))

      (remove-hook 'find-file-hook #'su--edit-file-as-root-maybe)
      (advice-remove 'basic-save-buffer
                     #'su--nadvice-make-directory-auto-root)
      (advice-remove 'helm-find-file-or-marked
                     #'su--nadvice-make-directory-auto-root)
      (advice-remove 'find-file-noselect
                     #'su--nadvice/find-file-noselect)
      (advice-remove 'semantic-find-file-noselect
                     #'su--nadvice-supress-find-file-hook)
      (advice-remove 'find-file-noselect-1
                     #'su--nadvice-find-file-noselect-1)))

  (su-mode +1))

(provide 'config-tramp)
