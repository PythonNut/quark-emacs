;; -*- lexical-binding: t -*-
(require 'cl-lib)
(require 'package)
(eval-when-compile
  (require 'config-setq))

;; =============================================
;; Setup straight.el
;; =============================================

(setq straight-recipe-overrides '((nil . ((straight :type git :host github :repo "PythonNut/straight.el" :branch "develop" :files ("straight.el"))))))

(let ((bootstrap-file (concat user-emacs-directory "straight/bootstrap.el"))
      (bootstrap-version 2))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; ================================================
;; Require packages in the background after startup
;; ================================================

(add-to-list 'load-path (locate-user-emacs-file "personal/"))

(defun my/sit-for (seconds)
  "Redisplay, then wait for SECONDS seconds.  Stop when input is available.
SECONDS may be a floating-point value.
\(On operating systems that do not support waiting for fractions of a
second, floating-point values are rounded down to the nearest integer.)"
  (unless (input-pending-p t)
    (redisplay)
    (let ((read (let ((input-method-function nil))
                  (read-event nil t seconds))))
      (when read
        ;; This is from the normal definition of sit-for, but
        ;; "(cons t read)" has been replaced by "read".
        ;; This is to avoid nasty "<t> is undefined" errors.
        (push read unread-command-events)))))

(defvar idle-require-symbols '(helm-files
                               helm-ring
                               helm-projectile
                               helm-semantic
                               counsel
                               which-key
                               evil-snipe
                               avy
                               ace-jump-helm-line
                               multiple-cursors
                               hydra)
  "Symbols which need to be autoloaded.")

(defvar idle-require-timer (run-with-idle-timer 0.1 t 'idle-require-load-next))

(defun idle-require-load-next ()
  "Load symbols from `idle-require-symbols' until input occurs."
  (let (symbol)
    (while (and idle-require-symbols
                (not (input-pending-p)))
      (cl-letf* ((old-load (symbol-function #'load))
                 ((symbol-function #'load)
                  (lambda (file &optional noerror _nomessage &rest args)
                    (apply old-load
                           file
                           noerror
                           (not (eq debug-on-error 'startup))
                           args))))
        (require (pop idle-require-symbols)))
      (my/sit-for 0.1))))

(defun nadvice/straight-use-package-ensure-function (old-fun &rest args)
  (cl-letf* (((symbol-function #'y-or-n-p) (lambda (prompt) t)))
    (apply old-fun args)))

(advice-add 'straight-use-package-ensure-function :around
            #'nadvice/straight-use-package-ensure-function)

(straight-use-package '(use-package
                         :type git
                         :host github
                         :repo "PythonNut/use-package"))

(require 'use-package)
(setq use-package-always-ensure t
      use-package-always-defer t)

(use-package hydra)
(use-package s)
(use-package el-patch)

(provide 'config-package)
