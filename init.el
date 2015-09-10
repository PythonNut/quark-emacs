;; -*- lexical-binding: t -*-
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;;(package-initialize)
(setq load-prefer-newer t)

(if (member "-E" command-line-args)
    (progn
      ;; skip and load minimal init instead
      (delete "-E" command-line-args)
      (load (concat user-emacs-directory "init-minimal")))

  (eval-when-compile (require 'cl-lib))
  (cl-letf* ((old-load (symbol-function 'load))
             ((symbol-function 'load)
              (lambda (file &optional noerror nomessage &rest args)
                (apply old-load
                       file
                       noerror
                       (not (eq debug-on-error 'startup))
                       args))))

    (message "[                 ]")

    (defvar my/slow-device nil)

    (when (member "-F" command-line-args)
      (delete "-F" command-line-args)
      (setq my/slow-device t))

    (eval-and-compile
      (add-to-list 'load-path (expand-file-name "modules/"
                                                user-emacs-directory)))

    ;; suppress the GNU spam
    (setq inhibit-startup-echo-area-message "pythonnut")
    (with-eval-after-load 'startup
      (fset 'display-startup-echo-area-message (lambda ())))
    (add-hook 'emacs-startup-hook (lambda () (message "")))


    (load (setq custom-file (expand-file-name "custom.el" user-emacs-directory)))

    (require 'config-setq)

    (unless debug-on-error
      (defun my/automatic-repair ()
        (message "Init did not complete! Attempting automatic repairs.")
        (sit-for 1)
        (cl-flet ((my/y-or-n-p
                   (prompt)
                   (let ((query-replace-map (copy-keymap query-replace-map)))
                     (define-key query-replace-map [t] 'skip)
                     (y-or-n-p prompt))))
          (if (save-window-excursion
                (when (fboundp 'package-upgrade-all)
                  (package-upgrade-all t)
                  (package-initialize))
                (byte-recompile-config t))
              (when (my/y-or-n-p "Automatic repair succeed. Press \"y\" to restart.")
                (restart-emacs))
            (when (my/y-or-n-p "Automatic repair failed. Press \"y\" to try emergency rebuild.")
              (emergency-fix-config))))))

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

    (defun my/recursively-load-dir (dir)
      (let ((suffixes (get-load-suffixes))
            (already-loaded))
        (dolist (file (directory-files dir t
                                       directory-files-no-dot-files-regexp))
          (if (file-directory-p file)
              (my/recursively-load-dir file)
            (when (member (file-name-extension file t) suffixes)
              (setq file (file-name-sans-extension file))
              (unless (member file already-loaded)
                (load file)
                (push file already-loaded)))))))

    (my/recursively-load-dir (expand-file-name "modules/modes/"
                                               user-emacs-directory))

    (message "[================ ]")
    (require 'config-solarized)
    (message "[=================]")

    (remove-hook 'emacs-startup-hook #'my/automatic-repair)))
