;; -*- lexical-binding: t -*-
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;;(package-initialize)
(setq load-prefer-newer t)

(if (member "-E" command-line-args)
    (progn
      ;; skip and load minimal init instead
      (delete "-E" command-line-args)
      (load (concat user-emacs-directory "init-minimal")))

  (eval-when-compile (require 'cl-lib))
  (cl-letf* ((old-load (symbol-function #'load))
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
      (add-to-list 'load-path (expand-file-name "modules/"
                                                user-emacs-directory)))

    ;; suppress the GNU spam
    (fset 'display-startup-echo-area-message #'ignore)
    (add-hook 'emacs-startup-hook (lambda () (message "")))


    (load (setq custom-file (expand-file-name "custom.el" user-emacs-directory)))

    (require 'config-setq)

    (unless debug-on-error
      (defun my/automatic-repair ()
        (message "Init did not complete! Attempting automatic repairs.")
        (sit-for 1)
        (cl-flet ((my/y-or-n-p
                   (prompt)
                   (let ((query-replace-map (copy-keymap query-replace-map)))
                     (define-key query-replace-map [t] 'skip)
                     (y-or-n-p prompt))))
          (if (save-window-excursion
                (not (byte-recompile-config t)))
              (when (my/y-or-n-p "Automatic repair succeed. Press \"y\" to restart.")
                (restart-emacs))
            (when (my/y-or-n-p "Automatic repair failed. Press \"y\" to try emergency rebuild.")
              (emergency-fix-config)))))
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

    (remove-hook 'emacs-startup-hook #'my/automatic-repair)))
