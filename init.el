(setq load-prefer-newer t)

(if (member "-F" command-line-args)
  (progn
    ;; skip and load minimal init instead
    (delete "-F" command-line-args)
    (load (concat user-emacs-directory "init-minimal")))

  (eval-when-compile (require 'cl-lib))
  (defmacro when* (condition &rest body)
    (when condition
      `(progn ,@body)))

  ;; polyfill with-eval-after-load
  (when* (version< emacs-version "24.4")
    (defmacro with-eval-after-load (thing &rest sexps)
      `(eval-after-load ,thing '(progn ,@sexps))))

  ;; supress the GNU spam
  (setq
    inhibit-startup-echo-area-message "pythonnut")
  (add-hook 'emacs-startup-hook (lambda () (message "")))

  (eval-and-compile
    (add-to-list 'load-path
      (concat user-emacs-directory "modules/")))

  (defadvice load (before quiet-loading
                    (FILE &optional NOERROR NOMESSAGE NOSUFFIX MUST-SUFFIX)
                    activate preactivate compile)
    (setq NOMESSAGE t))

  (defmacro load-module (name &optional method)
    (if method
      `(cl-letf (((symbol-function 'message) #'format))
         (load-library ,name))
      `(cl-letf (((symbol-function 'message) #'format))
         (require ',(make-symbol name)))))

  (load (setq custom-file (concat user-emacs-directory "custom.el")))

  (message "[              ]")
  (load-module "config-setq" t)
  (message "[=             ]")
  (load-module "config-package" t)
  (message "[==            ]")
  (load-module "config-modes" t)
  (message "[===           ]")
  (load-module "config-desktop" t)
  (message "[====          ]")
  (load-module "config-safety" t)
  (message "[=====         ]")
  (load-module "config-evil" t)
  (message "[======        ]")
  (load-module "config-ui" t)
  (message "[=======       ]")
  (load-module "config-whitespace" t)
  (message "[========      ]")
  (load-module "config-paste" t)
  (message "[=========     ]")
  (load-module "config-auto-complete" t)
  (message "[==========    ]")
  (load-module "config-vcs" t)
  (message "[===========   ]")
  (load-module "config-minibuffer" t)
  (message "[============  ]")
  (load-module "config-intel" t)
  (message "[============= ]")
  (load-module "config-solarized" t)
  (message "[==============]")

  (eval-when-compile
    (require 'load-dir))

  (setq
    load-dir-debug nil
    load-dirs (concat
                user-emacs-directory
                "modules/modes/"))

  (ad-disable-advice 'load 'before 'quiet-loading)
  (ad-activate 'load))
