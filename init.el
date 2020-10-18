;; -*- lexical-binding: t -*-
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;;(package-initialize)
(setq load-prefer-newer t)

(if (member "-M" command-line-args)
    (progn
      ;; Abort and load minimal init instead
      ;; This is useful if we are running in a resource constrained
      ;; environment or have broken the main config
      (delete "-M" command-line-args)
      (load (locate-user-emacs-file "init-minimal")))

  (require 'cl-lib)

  (cl-letf* (;; In fact, never GC during initialization to save time.
             (gc-cons-threshold 402653184)
             (gc-cons-percentage 0.6)
             (file-name-handler-alist nil)
             (load-source-file-function nil)

             ;; Also override load to hide  superfluous loading messages
             (old-load (symbol-function #'load))
             ((symbol-function #'load)
              (lambda (file &optional noerror _nomessage &rest args)
                (apply old-load
                       file
                       noerror
                       (not (eq debug-on-error 'startup))
                       args))))

    (message "[               ]")

    (defvar my/slow-device nil)

    (menu-bar-mode -1)
    (when (fboundp 'scroll-bar-mode)
      (scroll-bar-mode -1))
    (when (fboundp 'tool-bar-mode)
      (tool-bar-mode -1))

    (when (member "-F" command-line-args)
      (delete "-F" command-line-args)
      (setq my/slow-device t))

    (eval-and-compile
      (add-to-list 'load-path (locate-user-emacs-file "modules/")))

    ;; suppress the GNU spam
    (fset 'display-startup-echo-area-message #'ignore)
    (add-hook 'emacs-startup-hook (lambda () (message "")))

    (setq custom-file (locate-user-emacs-file "custom.el"))
    (condition-case nil
        (load custom-file)
      (error (with-temp-file custom-file)))

    (defun my/require-config-module-maybe-byte-compile (feature)
      (let* ((gc-cons-threshold 800000)
             (modules-dir (locate-user-emacs-file "modules/"))
             (basename (symbol-name feature))
             (source (expand-file-name (concat basename ".el") modules-dir))
             (dest (expand-file-name (concat basename ".elc") modules-dir)))
        (when (or (not (file-exists-p dest))
                  (file-newer-than-file-p source dest))
          (message "Byte-compiling %s..." basename)
          (if (if (and nil (require 'async nil t))
                  (async-get
                   (async-start
                    `(lambda ()
                       (add-to-list 'load-path
                                    (locate-user-emacs-file "modules/"))
                       (setq load-prefer-newer t)
                       (require 'config-core)
                       (require 'config-package)
                       (byte-compile-file ,source))))
                (require feature)
                (byte-compile-file source))
              (message "Byte-compiling %s...done" basename)
            (message "Byte-compiling %s...failed" basename))))
      (require feature))

    (defun my/maybe-byte-compile-init-el ()
      (let ((init-elc (concat (file-name-sans-extension user-init-file)
                              ".elc")))
        (when (file-newer-than-file-p user-init-file init-elc)
          (byte-compile-file user-init-file)
          (when (and (fboundp 'restart-emacs)
                     (y-or-n-p (format "%s was newer than %s. Restart?"
                                       user-init-file
                                       init-elc))))
          (restart-emacs))))

    (add-hook 'after-init-hook #'my/maybe-byte-compile-init-el)

    (eval-when-compile
      (require 'config-macros))

    (my/require-config-module 'config-core)
    (message "[=              ] package")
    (my/require-config-module 'config-package)
    (message "[==             ] desktop")
    (my/require-config-module 'config-desktop)
    (message "[===            ] safety")
    (my/require-config-module 'config-safety)
    (message "[====           ] evil")
    (my/require-config-module 'config-evil)
    (message "[=====          ] ui")
    (my/require-config-module 'config-ui)
    (message "[======         ] whitespace")
    (my/require-config-module 'config-whitespace)
    (message "[=======        ] paste")
    (my/require-config-module 'config-paste)
    (message "[========       ] company")
    (my/require-config-module 'config-company)
    (message "[=========      ] vcs")
    (my/require-config-module 'config-vcs)
    (message "[==========     ] ivy")
    (my/require-config-module 'config-ivy)
    (message "[===========    ] helm")
    (my/require-config-module 'config-helm)
    (message "[=============  ] intel")
    (my/require-config-module 'config-intel)
    (message "[============== ] modes")
    (my/require-config-module 'config-modes)
    (message "[===============] solarized")
    (my/require-config-module 'config-solarized)
    (message "[===============] done")))
