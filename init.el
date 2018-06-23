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

    (message "[                ]")

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

    (require 'config-setq)
    (message "[=              ] package")
    (require 'config-package)
    (message "[==             ] desktop")
    (require 'config-desktop)
    (message "[===            ] safety")
    (require 'config-safety)
    (message "[====           ] evil")
    (require 'config-evil)
    (message "[=====          ] ui")
    (require 'config-ui)
    (message "[======         ] whitespace")
    (require 'config-whitespace)
    (message "[=======        ] paste")
    (require 'config-paste)
    (message "[========       ] company")
    (require 'config-company)
    (message "[=========      ] vcs")
    (require 'config-vcs)
    (message "[==========     ] ivy")
    (require 'config-ivy)
    (message "[===========    ] helm")
    (require 'config-helm)
    (message "[=============  ] intel")
    (require 'config-intel)
    (message "[============== ] modes")
    (require 'config-modes)
    (message "[===============] solarized")
    (require 'config-solarized)
    (message "[===============] done")))
