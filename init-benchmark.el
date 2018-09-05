;; -*- lexical-binding: t -*-
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;;(package-initialize)
(setq load-prefer-newer t)
(require 'cl-lib)

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
(message "[===============] done")
