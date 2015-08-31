;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;;(package-initialize)
(setq load-prefer-newer t)

(eval-when-compile
  (require 'cl)
  (require 'cl-lib))

(defun nadvice/load-quiet (args)
  (cl-destructuring-bind
    (file &optional noerror nomessage nosuffix must-suffix)
    args
    (let ((nomessage t))
      (list
        file noerror nomessage nosuffix must-suffix))))

(if (member "-E" command-line-args)
  (progn
    ;; skip and load minimal init instead
    (delete "-E" command-line-args)
    (load (concat user-emacs-directory "init-minimal")))

  (defvar my/slow-device nil)

  (when (member "-F" command-line-args)
    (delete "-F" command-line-args)
    (setq my/slow-device t))

  (eval-and-compile
    (add-to-list 'load-path
      (concat user-emacs-directory "modules/"))
    
    (defmacro when* (condition &rest body)
      (when condition
        `(progn ,@body))))

  ;; suppress the GNU spam
  (with-eval-after-load 'startup
    (fset 'display-startup-echo-area-message (lambda ())))
  (add-hook 'emacs-startup-hook (lambda () (message "")))

  (advice-add 'load :filter-args #'nadvice/load-quiet)

  (load (setq custom-file (concat user-emacs-directory "custom.el")))

  (message "[                ]")
  (require 'config-setq)
  (message "[=               ]")
  (require 'config-package)
  (message "[==              ]")
  (require 'config-modes)
  (message "[===             ]")
  (require 'config-desktop)
  (message "[====            ]")
  (require 'config-safety)
  (message "[=====           ]")
  (require 'config-evil)
  (message "[======          ]")
  (require 'config-ui)
  (message "[=======         ]")
  (require 'config-whitespace)
  (message "[========        ]")
  (require 'config-paste)
  (message "[=========       ]")
  (require 'config-company)
  (message "[==========      ]")
  (require 'config-vcs)
  (message "[===========     ]")
  (require 'config-ido)
  (message "[============    ]")
  (require 'config-helm)
  (message "[=============   ]")
  (require 'config-minibuffer)
  (message "[==============  ]")
  (require 'config-intel)
  (message "[=============== ]")
  (require 'config-solarized)
  (message "[================]")

  (eval-when-compile
    (ignore-errors
      (require 'load-dir)))

  (setq
    load-dir-debug nil
    load-dirs (concat
                user-emacs-directory
                "modules/modes/"))

  (advice-remove 'load #'nadvice/load-quiet))
