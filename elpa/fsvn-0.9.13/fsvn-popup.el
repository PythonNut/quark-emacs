;;; fsvn-popup.el --- Subversion result buffer for fsvn.el


;;; History:
;; 

;;; Commentary:
;; 

;;; Code:



(require 'fsvn-deps)
(require 'fsvn-mode)
(require 'fsvn-cmd)



(defvar text-mode-map)

(defconst fsvn-popup-result-mode-line-process
  '(
    (fsvn-popup-result-process " Running process...")
    ))

(defconst fsvn-popup-result-buffer-local-variables
  '(
    (fsvn-popup-result-buffer-p . t)
    (fsvn-popup-result-process)
    (fsvn-popup-result-end-of-output)
    (fsvn-process-filter-for-update-parsed-end)
    ))

(defvar fsvn-popup-result-buffer-p nil)
(defvar fsvn-popup-result-process nil)
(defvar fsvn-popup-result-end-of-output nil)
(defvar fsvn-popup-result-mode-map nil)

(unless fsvn-popup-result-mode-map
  (setq fsvn-popup-result-mode-map
        (let ((map (make-sparse-keymap)))
          (set-keymap-parent map text-mode-map)

          (define-key map "\C-c\C-c" 'fsvn-popup-result-kill-process)
          (define-key map "\C-c\C-k" 'fsvn-popup-result-kill-process)
          (define-key map "\C-c\C-p" 'fsvn-popup-result-send-password)
          (define-key map "\C-m" 'fsvn-popup-result-send-string)
          map)))

(defcustom fsvn-popup-result-mode-hook nil
  "Run at the very end of `fsvn-popup-result-mode'."
  :group 'fsvn
  :type 'hook)

;; * fsvn-popup-result-mode internal function

(defun fsvn-popup-result-mode ()
  "Major mode for viewing Subversion command output.

Entry to this mode calls the value of `fsvn-popup-result-mode-hook'.

Keybindings:
\\{fsvn-popup-result-mode-map}

"
  (fsvn-global-initialize-mode)
  (use-local-map fsvn-popup-result-mode-map)
  (setq major-mode 'fsvn-popup-result-mode)
  (setq mode-name "Fsvn Result")
  (setq buffer-undo-list t)
  (fsvn-make-buffer-variables fsvn-popup-result-buffer-local-variables)
  (fsvn-popup-result-setup-mode-line)
  (run-mode-hooks 'fsvn-popup-result-mode-hook))

(defun fsvn-popup-result-setup-mode-line ()
  (or (assq 'fsvn-popup-result-process mode-line-process)
      (setq mode-line-process
            (append fsvn-popup-result-mode-line-process mode-line-process))))

(defun fsvn-popup-result-buffer-list ()
  (fsvn-mapitem
   (lambda (b)
     (with-current-buffer b
       (when fsvn-popup-result-buffer-p
         b)))
   (buffer-list)))

(defun fsvn-popup-result-create-buffer ()
  (let (tmp ret)
    (while (get-buffer (setq tmp (format-time-string "*Fsvn Result %H:%M:%S"))))
    (setq ret (get-buffer-create tmp))
    (with-current-buffer ret
      (fsvn-popup-result-mode))
    ret))

;; * fsvn-popup-result-mode interactive commands

(defun fsvn-popup-result-send-string ()
  "Insert newline and send string if at end-of-buffer."
  (interactive)
  (insert "\n")
  (when (eobp)
    (let ((proc (get-buffer-process (current-buffer)))
          string)
      (when (and proc
                 (eq (process-status proc) 'run)
                 fsvn-popup-result-end-of-output)
        (setq string (buffer-substring-no-properties fsvn-popup-result-end-of-output (point)))
        (set-marker fsvn-popup-result-end-of-output (point-max))
        (process-send-string proc (concat string))))))

(defun fsvn-popup-result-send-password ()
  (interactive)
  (let ((pass (read-passwd "Password: "))
        (proc (get-buffer-process (current-buffer))))
    (process-send-string proc (concat pass "\n"))))

(defun fsvn-popup-result-kill-process ()
  "Terminate process current buffer has."
  (interactive)
  (let ((proc (get-buffer-process (current-buffer))))
    (unless  (and proc (memq (process-status proc) '(run stop)))
      ;; delete from variable (kill process by external command e.g. kill or taskmanager)
      (setq fsvn-popup-result-process nil)
      (error "No process to kill"))
    (when (y-or-n-p "Active process running.  kill it? ")
      (kill-process proc)
      (setq fsvn-popup-result-process nil))))



(defvar fsvn-popup-start-process-buffer nil
  "As external arguments. Sequential command execute.")

(defun fsvn-popup-start-process (subcommand &rest args)
  "SUBCOMMAND svn command.
ARGS is svn subcommand args."
  (let ((buffer (or fsvn-popup-start-process-buffer (fsvn-popup-result-create-buffer)))
        proc)
    (setq proc (fsvn-start-command-display subcommand buffer args))
    (set-process-sentinel proc 'fsvn-popup-general-process-sentinel)
    (set-process-filter proc 'fsvn-popup-process-filter-in-buffer)
    (with-current-buffer buffer
      (when (eq major-mode 'fsvn-popup-result-mode)
        (setq fsvn-popup-result-process proc)))
    (fsvn-buffer-popup-as-information buffer)
    proc))

(defvar fsvn-popup-call-process-buffer nil
  "As external arguments. Sequential command execute.")

(defun fsvn-popup-call-process (command &rest args)
  "COMMAND svn subcommand.
ARGS is svn subcommand args.  Accepts nil (but not sended).
return buffer of result output."
  (let ((buffer (or fsvn-popup-call-process-buffer (fsvn-popup-result-create-buffer)))
        ret)
    (setq ret (fsvn-call-command-display command buffer args))
    (fsvn-buffer-popup-as-information buffer)
    (unless (= ret 0)
      (error "Execution error while `%s'" command))
    (fsvn-save-window-only (get-buffer-window buffer)
      (goto-char (point-min)))
    buffer))

(defun fsvn-popup-call-process-multi (command files &rest args)
  "`call-process' accepts multiple files.
subcommand must accept `--targets' argument.

Argument COMMAND svn subcommand.
Argument FILES target files.
Optional argument ARGS svn command arguments."
  (let ((buffer (fsvn-popup-result-create-buffer))
        ret)
    (setq args
          (if (> (length files) 1)
              (append args (list "--targets" (fsvn-make-targets-file files)))
            (append args files)))
    (setq ret (apply 'fsvn-call-command-display command buffer args))
    (fsvn-buffer-popup-as-information buffer)
    (unless (= ret 0)
      (error "Execution error while `%s'" command))
    buffer))

(defun fsvn-popup-start-copy/move-process (command files destination &optional args)
  "For subcommand `copy' or `move'.

Argument COMMAND svn subcommand.
Argument FILES target files.
Argument DESTINATION target directory.
Optional argument ARGS svn command arguments."
  (let (proc)
    (setq proc (fsvn-popup-start-process command args files destination))
    (fsvn-process-add-sentinel proc 'fsvn-popup-copy/move-process-sentinel)
    proc))

(defun fsvn-popup-process-filter-in-buffer (proc event)
  (fsvn-debug event)
  (with-current-buffer (process-buffer proc)
    (let (keep-point)
      (unless (= (point) (point-max))
        (setq keep-point (point))
        (goto-char (point-max)))
      (insert event)
      (fsvn-parse-result-if-auth-prompt proc)
      (setq fsvn-popup-result-end-of-output (point-marker))
      (when keep-point
        (goto-char keep-point)))))

(defun fsvn-popup-general-process-sentinel (proc event)
  (fsvn-process-exit-handler proc event
    (when (local-variable-p 'fsvn-popup-result-process)
      (setq fsvn-popup-result-process nil))))

(defun fsvn-popup-copy/move-process-sentinel (proc event)
  (fsvn-process-exit-handler proc event
    (fsvn-parse-result-cmd-add (current-buffer))
    (fsvn-parse-result-cmd-delete (current-buffer))))



(defconst fsvn-popup-result-mode-menu-spec
  '("fsvn"
     ["Kill running process" fsvn-popup-result-kill-process t]
     ["Send password" fsvn-popup-result-send-password t]
     ["Send input string to process" fsvn-popup-result-send-string t]
    ))

(easy-menu-define fsvn-popup-result-mode-menu
  fsvn-popup-result-mode-map
  "Menu used in Fsvn Result mode."
  fsvn-popup-result-mode-menu-spec)



(provide 'fsvn-popup)

;;; fsvn-popup.el ends here
