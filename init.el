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
(load-library "config-setq")
(message-force "[=              ]")
(load-library "config-package")
(message-force "[==             ]")
(load-library "config-modes")
(message-force "[===            ]")
(load-library "config-desktop")
(message-force "[====           ]")
(load-library "config-pre-bindings")
(message-force "[=====          ]")
(load-library "config-safety")
(message-force "[======         ]")
(load-library "config-evil")
(message-force "[=======        ]")
(load-library "config-ui")
(message-force "[========       ]")
(load-library "config-whitespace")
(message-force "[=========      ]")
(load-library "config-paste")
(message-force "[==========     ]")
(load-library "config-auto-complete")
(message-force "[===========    ]")
(load-library "config-projects")
(message-force "[============   ]")
(load-library "config-minibuffer")
(message-force "[=============  ]")
(load-library "config-intel")
(message-force "[============== ]")
(load-library "config-solarized")
(message-force "[===============]")

(setq load-dirs (concat
                  user-emacs-directory
                  "modules/modes/"))

(ad-disable-advice 'load 'before 'quiet-loading)
(ad-activate 'load)
(ad-disable-advice 'message 'around 'supress-messages)
(ad-activate 'message)
