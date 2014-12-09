(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; polyfill with-eval-after-load
(when (version< emacs-version "24.4")
  (defmacro with-eval-after-load (thing &rest sexps)
    `(eval-after-load ,thing '(progn ,@sexps))))

(add-to-list 'load-path
  (concat user-emacs-directory "modules/"))

(defadvice load (before quiet-loading
                  (FILE &optional NOERROR NOMESSAGE NOSUFFIX MUST-SUFFIX)
                  activate)
  (setq NOMESSAGE t))

(defadvice message (around supress-messages activate))

;; force a message through, temporarily disabling
;; message advice, let style
(defun message-force (str)
  (ad-disable-advice 'message 'around 'supress-messages)
  (ad-activate 'message)
  (message str)
  (ad-enable-advice 'message 'around 'supress-messages)
  (ad-activate 'message))

(message-force "[               ]")
(require 'config-setq)
(message-force "[=              ]")
(require 'config-package)
(message-force "[==             ]")
(require 'config-modes)
(message-force "[===            ]")
(require 'config-desktop)
(message-force "[====           ]")
(require 'config-safety)
(message-force "[=====          ]")
(require 'config-evil)
(message-force "[======         ]")
(require 'config-ui)
(message-force "[=======        ]")
(require 'config-whitespace)
(message-force "[========       ]")
(require 'config-paste)
(message-force "[=========      ]")
(require 'config-auto-complete)
(message-force "[==========     ]")
(require 'config-projectile)
(message-force "[===========    ]")
(require 'config-vcs)
(message-force "[============   ]")
(require 'config-minibuffer)
(message-force "[=============  ]")
(require 'config-intel)
(message-force "[============== ]")
(require 'config-solarized)
(message-force "[===============]")

(setq load-dirs (concat
                  user-emacs-directory
                  "modules/modes/"))

(ad-disable-advice 'load 'before 'quiet-loading)
(ad-activate 'load)
(ad-disable-advice 'message 'around 'supress-messages)
(ad-activate 'message)
