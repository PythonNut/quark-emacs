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
      (expand-file-name "modules/" user-emacs-directory))
    
    (defmacro when* (condition &rest body)
      (when condition
        `(progn ,@body))))

  ;; suppress the GNU spam
  (setq inhibit-startup-echo-area-message "pythonnut")
  (with-eval-after-load 'startup
    (fset 'display-startup-echo-area-message (lambda ())))
  (add-hook 'emacs-startup-hook (lambda () (message "")))

  (advice-add 'load :filter-args #'nadvice/load-quiet)

  (load (setq custom-file (concat user-emacs-directory "custom.el")))

  (message "[                ]")
  (require 'config-setq)

  (defun my/automatic-repair ()
    (message "Init did not complete! Attempting automatic repairs.")
    (byte-recompile-config))

  (add-hook 'emacs-startup-hook #'my/automatic-repair)

  (message "[=                ]")
  (require 'config-package)
  (message "[==               ]")
  (require 'config-modes)
  (message "[===              ]")
  (require 'config-desktop)
  (message "[====             ]")
  (require 'config-safety)
  (message "[=====            ]")
  (require 'config-evil)
  (message "[======           ]")
  (require 'config-ui)
  (message "[=======          ]")
  (require 'config-whitespace)
  (message "[========         ]")
  (require 'config-paste)
  (message "[=========        ]")
  (require 'config-company)
  (message "[==========       ]")
  (require 'config-vcs)
  (message "[===========      ]")
  (require 'config-ido)
  (message "[============     ]")
  (require 'config-helm)
  (message "[=============    ]")
  (require 'config-minibuffer)
  (message "[==============   ]")
  (require 'config-intel)
  (message "[===============  ]")
  (require 'config-solarized)
  (message "[================ ]")

  (defun my/recursively-load-dir (dir)
    (let ((suffixes (get-load-suffixes))
           (already-loaded))
      (dolist (f (directory-files dir t
                   directory-files-no-dot-files-regexp))
        (if (file-directory-p f)
          (my/recursively-load-dir f)
          (when (member (file-name-extension f t) suffixes)
            (setq f (file-name-sans-extension f))
            (unless (member f already-loaded)
              (load f)
              (push f already-loaded)))))))

  (my/recursively-load-dir
    (expand-file-name
      "modules/modes/"
      user-emacs-directory))

  (message "[=================]")

  (advice-remove 'load #'nadvice/load-quiet)
  (remove-hook 'emacs-startup-hook #'my/automatic-repair))
