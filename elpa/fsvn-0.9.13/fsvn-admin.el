;;; fsvn-admin.el --- Interface for svnadmin.


;;; History:
;; 

;;; Commentary:
;; 

;;; Code:



(require 'fsvn-deps)
(require 'fsvn-proc)
(require 'fsvn-debug)
(require 'fsvn-ui)



(defvar process-environment)
(defvar current-prefix-arg)



(defgroup fsvn-admin nil
  "`svnadmin' interfaces."
  :group 'fsvn)

(defun fsvn-admin-start-command (subcommand buffer &rest args)
  (fsvn-process-environment
   (let ((real-args (fsvn-command-args-canonicalize args)))
     (fsvn-debug real-args)
     (apply 'start-process "fsvn admin" buffer fsvn-svnadmin-command-internal subcommand real-args))))

(defun fsvn-admin-call-command (subcommand buffer &rest args)
  (fsvn-process-environment
   (let ((real-args (fsvn-command-args-canonicalize args)))
     (fsvn-debug real-args)
     (prog1
         (apply 'call-process fsvn-svnadmin-command-internal nil buffer nil subcommand real-args)
       (fsvn-debug buffer)))))

(defun fsvn-admin-call-command-discard (subcommand buffer &rest args)
  (unless (= (apply 'fsvn-admin-call-command subcommand buffer args) 0)
    (signal 'fsvn-command-error (list subcommand args (buffer-string))))
  t)

(defun fsvn-admin-create-empty-hook (repos name)
  (let* ((hookdir (fsvn-expand-file "hooks" repos))
         (hook (fsvn-expand-file name hookdir)))
    (write-region "#!/bin/sh\n\nexit 0\n" nil hook nil 'no-msg)
    (set-file-modes hook ?\744)))

(defun fsvn-admin-create-repository (dir &optional args)
  "Create repository to current directory."
  (interactive (list (fsvn-expand-file default-directory)
                     (when current-prefix-arg
                       (fsvn-read-svnadmin-subcommand-args "create"))))
  (let ((buffer (fsvn-make-temp-buffer)))
    (prog1
        (fsvn-admin-start-command "create" buffer dir args)
      (fsvn-buffer-popup-as-information buffer))))

(defun fsvn-admin-command (subcommand args)
  "Execute `svnadmin' command with completing read."
  (interactive (let (subcommand args)
                 (setq subcommand (fsvn-read-svnadmin-subcommand))
                 (setq args (fsvn-read-svnadmin-subcommand-args subcommand))
                 (list subcommand args)))
  (let ((buffer (fsvn-make-temp-buffer)))
    (prog1
        (fsvn-admin-start-command subcommand buffer args)
      (fsvn-buffer-popup-as-information buffer))))

(defun fsvn-admin-show-svnadmin-help (subcommand)
  "Show `svnadmin' help for SUBCOMMAND."
  (interactive (list (fsvn-read-svnadmin-subcommand)))
  (let ((buffer (fsvn-make-temp-buffer))
        (fsvn-process-environment-lang fsvn-help-locale))
    (fsvn-admin-call-command "help" buffer subcommand)
    (fsvn-buffer-popup-as-information buffer)))

(provide 'fsvn-admin)

;;; fsvn-admin.el ends here
