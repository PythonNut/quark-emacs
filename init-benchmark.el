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

(defvar my/slow-device nil)
(eval-when-compile (require 'cl-lib))

(when (member "-F" command-line-args)
  (delete "-F" command-line-args)
  (setq my/slow-device t))

(eval-and-compile
  (add-to-list 'load-path
    (expand-file-name "modules/" user-emacs-directory))

  (defmacro when* (condition &rest body)
    (when condition
      `(progn ,@body))))

;; suppress the GNU spam
(fset 'display-startup-echo-area-message #'ignore)
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
(require 'config-ivy)
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

(advice-remove 'load #'nadvice/load-quiet)
