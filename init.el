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
  ;; Modern machines don't need to run GC for every 8MB allocated.
  (setq gc-cons-threshold 20000000)

  (cl-letf* (;; In fact, never GC during initialization to save time.
             (gc-cons-threshold most-positive-fixnum)

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
    (when (file-exists-p custom-file)
      (load custom-file))

    (require 'config-setq)

    ;; Automatic repair system attempts to recompile potentially
    ;; broken bytecode if the init does not complete, and offers to
    ;; do more extreme things if the init cannot be saved.
    (unless debug-on-error
      (defun my/automatic-repair ()
        (message "Init did not complete! Attempting automatic repairs.")
        (sit-for 1)
        (if (save-window-excursion
              (not (prog1
                       (byte-recompile-config t)
                     (delete-file (locate-user-emacs-file
                                   "data/package-cache.el")))))
            (when (my/y-or-n-p-optional
                   "Automatic repair succeed. Press \"y\" to restart.")
              (restart-emacs))
          (when (my/y-or-n-p-optional
                 "Automatic repair may have failed. Press \"y\" to try emergency rebuild.")
            (emergency-fix-config))))

      ;; This hook will be removed if the init completes successfully.
      (add-hook 'emacs-startup-hook #'my/automatic-repair))

    (message "[=               ]")
    (require 'config-package)
    (message "[==              ]")
    (require 'config-desktop)
    (message "[===             ]")
    (require 'config-safety)
    (message "[====            ]")
    (require 'config-evil)
    (message "[=====           ]")
    (require 'config-ui)
    (message "[======          ]")
    (require 'config-whitespace)
    (message "[=======         ]")
    (require 'config-paste)
    (message "[========        ]")
    (require 'config-company)
    (message "[=========       ]")
    (require 'config-vcs)
    (message "[==========      ]")
    (require 'config-ivy)
    (message "[===========     ]")
    (require 'config-helm)
    (message "[============    ]")
    (unless (bound-and-true-p my/slow-device)
      (require 'config-icicles))
    (message "[=============   ]")
    (require 'config-intel)
    (message "[==============  ]")
    (require 'config-modes)
    (message "[=============== ]")
    (require 'config-solarized)
    (message "[================]")

    ;; No need to run the init repair system.
    (remove-hook 'emacs-startup-hook #'my/automatic-repair)))
