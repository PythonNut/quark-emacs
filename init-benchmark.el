;; -*- lexical-binding: t -*-
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;;(package-initialize)
(setq load-prefer-newer t)
(require 'cl-lib)
(setq gc-cons-threshold 20000000)

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


(load (setq custom-file (locate-user-emacs-file "custom.el")))

(require 'config-setq)

(unless debug-on-error
  (defun my/automatic-repair ()
    (message "Init did not complete! Attempting automatic repairs.")
    (sit-for 1)
    (if (save-window-excursion (not (byte-recompile-config t)))
        (when (my/y-or-n-p-optional
               "Automatic repair succeed. Press \"y\" to restart.")
          (restart-emacs))
      (when (my/y-or-n-p-optional
             "Automatic repair failed. Press \"y\" to try emergency rebuild.")
        (emergency-fix-config))))
  (add-hook 'emacs-startup-hook #'my/automatic-repair))


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
;; (require 'config-mode
(message "[=============== ]")
(require 'config-solarized)
(message "[================]")

(remove-hook 'emacs-startup-hook #'my/automatic-repair)
