(defmacro when* (condition &rest body)
  (when condition
    `(progn ,@body)))

;; polyfill with-eval-after-load
(when* (version< emacs-version "24.4")
  (defmacro with-eval-after-load (thing &rest sexps)
    `(eval-after-load ,thing '(progn ,@sexps))))

;; supress the GNU spam
(setq
  inhibit-startup-echo-area-message "pythonnut"
  load-prefer-newer t)
(add-hook 'emacs-startup-hook (lambda () (message "")))

(eval-and-compile
  (add-to-list 'load-path
    (concat user-emacs-directory "modules/")))

(defadvice load (before quiet-loading
                  (FILE &optional NOERROR NOMESSAGE NOSUFFIX MUST-SUFFIX)
                  activate preactivate compile)
  (setq NOMESSAGE t))

(defadvice message (around supress-messages activate preactivate compile))

;; force a message through, temporarily disabling
;; message advice, let style
(defun message-force (str)
  (ad-disable-advice 'message 'around 'supress-messages)
  (ad-activate 'message)
  (message str)
  (ad-enable-advice 'message 'around 'supress-messages)
  (ad-activate 'message))

(defmacro load-module (name &optional method)
  (if method
    `(load-library ,name)
    `(require ',(make-symbol name))))

(load (setq custom-file "~/.emacs.d/custom.el"))

(message-force "[              ]")
(load-module "config-setq" t)
(message-force "[=             ]")
(load-module "config-package" t)
(message-force "[==            ]")
(load-module "config-modes" t)
(message-force "[===           ]")
(load-module "config-desktop" t)
(message-force "[====          ]")
(load-module "config-safety" t)
(message-force "[=====         ]")
(load-module "config-evil" t)
(message-force "[======        ]")
(load-module "config-ui" t)
(message-force "[=======       ]")
(load-module "config-whitespace" t)
(message-force "[========      ]")
(load-module "config-paste" t)
(message-force "[=========     ]")
(load-module "config-auto-complete" t)
(message-force "[==========    ]")
(load-module "config-vcs" t)
(message-force "[===========   ]")
(load-module "config-minibuffer" t)
(message-force "[============  ]")
(load-module "config-intel" t)
(message-force "[============= ]")
(load-module "config-solarized" t)
(message-force "[==============]")

(eval-when-compile
  (require 'load-dir))

(setq
  load-dir-debug nil
  load-dirs (concat
              user-emacs-directory
              "modules/modes/"))

(ad-disable-advice 'load 'before 'quiet-loading)
(ad-activate 'load)
(ad-disable-advice 'message 'around 'supress-messages)
(ad-activate 'message)
