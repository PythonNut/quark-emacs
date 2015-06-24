;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;;(package-initialize)

(setq load-prefer-newer t)

(eval-when-compile (require 'cl-lib))

(eval-and-compile
  (add-to-list 'load-path
    (concat user-emacs-directory "modules/"))

  (defmacro when* (condition &rest body)
    (when condition
      `(progn ,@body))))

;; polyfill with-eval-after-load
(when* (version< emacs-version "24.4")
  (defmacro with-eval-after-load (thing &rest sexps)
    `(eval-after-load ,thing '(progn ,@sexps))))

;; suppress the GNU spam
(add-hook 'emacs-startup-hook (lambda () (message "")))

(defadvice load (before quiet-loading
                  (FILE &optional NOERROR NOMESSAGE NOSUFFIX MUST-SUFFIX)
                  activate preactivate compile)
  (setq NOMESSAGE t))

(load (setq custom-file (concat user-emacs-directory "custom.el")))

(message "[              ]")
(require 'config-setq)
(message "[=             ]")
(require 'config-package)
(message "[==            ]")
(require 'config-modes)
(message "[===           ]")
(require 'config-desktop)
(message "[====          ]")
(require 'config-safety)
(message "[=====         ]")
(require 'config-evil)
(message "[======        ]")
(require 'config-ui)
(message "[=======       ]")
(require 'config-whitespace)
(message "[========      ]")
(require 'config-paste)
(message "[=========     ]")
(require 'config-company)
(message "[==========    ]")
(require 'config-vcs)
(message "[===========   ]")
(require 'config-minibuffer)
(message "[============  ]")
(require 'config-intel)
(message "[============= ]")
(require 'config-solarized)
(message "[==============]")

(eval-when-compile
  (ignore-errors
    (require 'load-dir)))

(setq
  load-dir-debug nil
  load-dirs (concat
              user-emacs-directory
              "modules/modes/"))

(ad-disable-advice 'load 'before 'quiet-loading)
(ad-activate 'load)
