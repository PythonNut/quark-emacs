;;; fsvn-mode.el --- fsvn.el major-mode/minor-mode internal utilities


;;; History:
;; 

;;; Commentary:
;; 

;;; Code:



(require 'easymenu)

(require 'fsvn-deps)
(require 'fsvn-env)
(require 'fsvn-ui)
(require 'fsvn-proc)
(require 'fsvn-cmd)

(defvar directory-listing-before-filename-regexp)



(defcustom fsvn-default-args-commit nil
  "Default args for `commit'"
  :group 'fsvn
  :type '(list string))

(defcustom fsvn-default-args-update nil
  "Default args for `update'"
  :group 'fsvn
  :type '(list string))

(defcustom fsvn-default-args-info nil
  "Default args for `info'"
  :group 'fsvn
  :type '(list string))

(defcustom fsvn-default-args-revert nil
  "Default args for `revert'"
  :group 'fsvn
  :type '(list string))

(defcustom fsvn-default-args-cleanup nil
  "Default args for `cleanup'"
  :group 'fsvn
  :type '(list string))

(defcustom fsvn-default-args-resolved nil
  "Default args for `resolved'"
  :group 'fsvn
  :type '(list string))

(defcustom fsvn-default-args-lock nil
  "Default args for `lock'"
  :group 'fsvn
  :type '(list string))

(defcustom fsvn-default-args-unlock nil
  "Default args for `unlock'"
  :group 'fsvn
  :type '(list string))

(defcustom fsvn-default-args-blame nil
  "Default args for `blame'"
  :group 'fsvn
  :type '(list string))

(defcustom fsvn-default-args-diff nil
  "Default args for `diff'"
  :group 'fsvn
  :type '(list string))

(defcustom fsvn-default-args-add nil
  "Default args for `add'"
  :group 'fsvn
  :type '(list string))

(defcustom fsvn-default-args-delete nil
  "Default args for `delete'"
  :group 'fsvn
  :type '(list string))

(defcustom fsvn-default-args-switch nil
  "Default args for `switch'"
  :group 'fsvn
  :type '(list string))

(defcustom fsvn-default-args-export nil
  "Default args for `export'"
  :group 'fsvn
  :type '(list string))

(defcustom fsvn-default-args-move nil
  "Default args for `move'"
  :group 'fsvn
  :type '(list string))

(defcustom fsvn-default-args-copy nil
  "Default args for `copy'"
  :group 'fsvn
  :type '(list string))

(defcustom fsvn-default-args-merge nil
  "Default args for `merge'"
  :group 'fsvn
  :type '(list string))

(defcustom fsvn-default-args-mergeinfo nil
  "Default args for `mergeinfo'"
  :group 'fsvn
  :type '(list string))

(defcustom fsvn-default-args-mkdir nil
  "Default args for `mkdir'"
  :group 'fsvn
  :type '(list string))

(defcustom fsvn-default-args-import nil
  "Default args for `import'"
  :group 'fsvn
  :type '(list string))

(defcustom fsvn-default-args-checkout nil
  "Default args for `checkout'"
  :group 'fsvn
  :type '(list string))

(defcustom fsvn-default-args-patch nil
  "Default args for `patch'"
  :group 'fsvn
  :type '(list string))



(defconst fsvn-global-buffer-local-variables
  '(
    (fsvn-default-window-configuration)
    (fsvn-previous-window-configuration)
    ))

(defvar fsvn-previous-window-configuration nil)
(defvar fsvn-default-window-configuration nil)

(defmacro fsvn-restore-window-buffer (&rest form)
  "Set window setting according to variable `fsvn-previous-window-configuration' after evaluate FORM."
  `(let ((WIN-CONFIGURE fsvn-previous-window-configuration))
     (prog1
         (progn ,@form)
       (when (and WIN-CONFIGURE
                  (window-configuration-p WIN-CONFIGURE))
         (set-window-configuration WIN-CONFIGURE)))))

(defun fsvn-restore-previous-window-setting ()
  (interactive)
  (fsvn-restore-window-buffer
   ))

(defun fsvn-restore-default-window-setting ()
  (interactive)
  (when (and fsvn-default-window-configuration
             (window-configuration-p fsvn-default-window-configuration))
    (set-window-configuration fsvn-default-window-configuration)))

(defun fsvn-restore-default-window-display ()
  (interactive)
  (let ((prev (current-buffer)))
    (when (and fsvn-default-window-configuration
               (window-configuration-p fsvn-default-window-configuration))
      (set-window-configuration fsvn-default-window-configuration))
    (when (get-buffer-window prev)
      (set-frame-selected-window (selected-frame) (get-buffer-window prev)))))

(defvar fsvn-buffer-repos-info nil)

(defun fsvn-buffer-repos-info (&optional buffer)
  (or
   (and buffer (buffer-local-value 'fsvn-buffer-repos-info buffer))
   fsvn-buffer-repos-info))

(defun fsvn-buffer-repos-uuid (&optional info)
  (let ((info (or info fsvn-buffer-repos-info)))
    (and info (aref info 0))))

(defun fsvn-buffer-repos-root (&optional info)
  (let ((info (or info fsvn-buffer-repos-info)))
    (and info (aref info 1))))

(defun fsvn-buffer-new-repos-info (url)
  (let ((info (fsvn-get-info-entry url)))
    (when info
      (fsvn-buffer-xml-info->repos-info info))))

(defun fsvn-buffer-xml-info->repos-info (info)
  (let ((v (make-vector 2 nil)))
    (aset v 0 (fsvn-xml-info->entry=>repository=>uuid$ info))
    (aset v 1 (fsvn-xml-info->entry=>repository=>root$ info))
    v))

(defun fsvn-buffer-new-repos-info-upward (url)
  (let ((info (fsvn-get-info-upward url)))
    (fsvn-buffer-xml-info->repos-info info)))

(defun fsvn-make-buffer-variables (variables)
  (fsvn-make-buffer-variables-internal fsvn-global-buffer-local-variables)
  (fsvn-make-buffer-variables-internal variables))

(defun fsvn-make-buffer-variables-internal (variables)
  (mapc
   (lambda (cell)
     (set (make-local-variable (car cell)) (eval (cdr cell))))
   variables))

(defun fsvn-kill-buffer-variables-internal (variables)
  (mapc
   (lambda (cell)
     (kill-local-variable (car cell)))
   variables))

(defun fsvn-set-buffer-local-variable (buffer var value)
  (with-current-buffer buffer
    (make-local-variable var)
    (set var value)))

(defun fsvn-buffer-major-mode (buffer)
  (buffer-local-value 'major-mode buffer))

(defmacro fsvn-each-buffer-mode (major &rest form)
  (declare (indent 1))
  `(let (RET)
     (mapc
      (lambda (b)
        (with-current-buffer b
          (save-excursion
            (when (eq major-mode ,major)
              (setq RET (cons (progn ,@form) RET))))))
      (buffer-list))
     (nreverse RET)))

(defcustom fsvn-no-confirm nil
  "Control hide confirm prompt.
`t' means completely ignore all.
List of command symbol means, each of command's prompt will not be shown."
  :group 'fsvn
  :type '(choice
          (const nil)
          (const t)
          (repeat symbol)))

(defun fsvn-confirm-prompt (op-symbol prompt)
  "Show PROMPT unless `fsvn-no-confirm' indicate non-confirm."
  (cond
   ((eq fsvn-no-confirm t))
   ((memq op-symbol fsvn-no-confirm))
   (t
    (y-or-n-p prompt))))



;; window utility

(defun fsvn-switch-buffer-window (buffer &optional displayed-only)
  "Focus to BUFFER if that is displayed in this frame then switch frame.
Or only switch to BUFFER.
DISPLAYED-ONLY non-nil means never switch if BUFFER is not displayed."
  (when buffer
    (when (buffer-live-p buffer)
      (cond
       ((get-buffer-window buffer)
        (set-frame-selected-window (selected-frame) (get-buffer-window buffer)))
       ((not displayed-only)
        (switch-to-buffer buffer))
       (t
        )))))

(defmacro fsvn-scroll-window-buffer (buffer-or-window scroller mover sig)
  `(let ((ORIGIN-WIN (selected-window))
         (RET t)
         WIN)
     (setq WIN
           (if (windowp buffer-or-window)
               buffer-or-window
             (get-buffer-window buffer-or-window)))
     (unwind-protect
         (progn
           (set-frame-selected-window (selected-frame) WIN)
           (condition-case err
               ,scroller
             (,sig ,mover (setq RET nil))))
       (set-frame-selected-window (selected-frame) ORIGIN-WIN))
     RET))

(defun fsvn-scroll-window-buffer-up (buffer-or-window &optional arg)
  (fsvn-scroll-window-buffer
   buffer-or-window
   (scroll-up arg) (goto-char (point-max)) end-of-buffer))

(defun fsvn-scroll-window-buffer-down (buffer-or-window &optional arg)
  (fsvn-scroll-window-buffer
   buffer-or-window
   (scroll-down arg) (goto-char (point-min)) beginning-of-buffer))



(defun fsvn-next-file (&optional arg)
  "Move down lines then position at filename.
Optional prefix ARG says how many lines to move; default is one line."
  (interactive "p")
  (forward-line arg)
  (fsvn-move-to-filename))

(defun fsvn-previous-file (&optional arg)
  "Move up lines then position at filename.
Optional prefix ARG says how many lines to move; default is one line."
  (interactive "p")
  (forward-line (- arg))
  (fsvn-move-to-filename))



(defun fsvn-current-filename ()
  "File name of current point.  only filename not include path."
  (let ((points (fsvn-points-of-filename)))
    (when points
      (buffer-substring-no-properties (car points) (cdr points)))))

(defun fsvn-move-to-filename ()
  "Move to filename."
  (forward-line 0)
  (let ((points (fsvn-points-of-filename)))
    (when points
      (goto-char (car points)))))

(defun fsvn-points-of-filename ()
  (let* ((bol (line-beginning-position))
         (eol (line-end-position))
         (start (next-single-property-change bol 'fsvn-filename nil eol))
         end)
    (when start
      (setq end (next-single-property-change start 'fsvn-filename nil eol))
      (when (and end (< start end))
        (cons start end)))))



(defmacro fsvn-save-browse-directory-excursion (dir &rest form)
  "Goto DIR and execute FORM with no point move.
"
  (declare (indent 1))
  `(let ((PREV-MARKER (point-marker))
         (BUFFER (fsvn-local-directory-buffer ,dir)))
     (when BUFFER
       (unwind-protect
           (progn
             (set-buffer BUFFER)
             (save-excursion
               (when (fsvn-browse-goto-directory ,dir)
                 (progn ,@form))))
         (set-buffer (marker-buffer PREV-MARKER))
         (goto-char PREV-MARKER)))))

(defmacro fsvn-save-browse-file-excursion (file &rest form)
  "Goto FILE and execute FORM with no point move.
"
  (declare (indent 1))
  `(let ((DIR (fsvn-file-name-directory ,file)))
     (fsvn-save-browse-directory-excursion DIR
       (save-excursion
         (when (fsvn-browse-goto-file ,file)
           (progn ,@form))))))

(defun fsvn-goto-browse-directory (dir)
  "Goto DIR in current buffer and return non-nil value if DIR is found.
"
  (let ((buffer (fsvn-local-directory-buffer dir)))
    ;;BUG cannot keep point 
    (when buffer
      (set-buffer buffer)
      (fsvn-browse-goto-directory dir))))

(defun fsvn-goto-browse-file (file)
  "Goto FILE in current buffer and return non-nil value if FILE is found.
"
  (let ((dir (fsvn-file-name-directory file))
        buffer)
    ;;BUG cannot keep point 
    (when (setq buffer (fsvn-get-exists-browse-buffer dir))
      (set-buffer buffer)
      (fsvn-browse-goto-file file))))

(defmacro fsvn-each-browse-buffer (&rest form)
  "Execute FORM in each `fsvn-browse-mode' buffer."
  `(fsvn-each-buffer-mode 'fsvn-browse-mode
     ,@form))



(defun fsvn-toggle-mode-line-variable (arg var on-value message)
  (let ((value (symbol-value var))
        (propertize-on (list :propertize on-value 'face 'fsvn-warning-face)))
    (setq value (set var (if (or arg (not value)) propertize-on nil)))
    (force-mode-line-update)
    (message "Now %s `%s'" message (if value "ON" "OFF"))
    value))

;; defined before keymap
(defun fsvn-readonly-mode-keymap (map)
  (define-key map "!" 'shell-command)
  (define-key map "\C-c\ec" 'fsvn-global-cleanup-buffer)
  )

(defun fsvn-global-initialize-mode ()
  (kill-all-local-variables)
  (set-buffer-modified-p nil))

(defun fsvn-set-default-directory (dir)
  (setq default-directory (file-name-as-directory dir)))



(defun fsvn-get-view-buffer (file)
  (unless (file-exists-p file)
    (error "%s does not exist" file))
  (let ((had-a-buf (get-file-buffer file))
        buffer)
    (cond
     ((null had-a-buf))
     ((verify-visited-file-modtime had-a-buf))
     (t
      ;; entrust to find-file-noselect
      (setq had-a-buf nil)))
    (setq buffer (or had-a-buf (find-file-noselect file)))))

(defun fsvn-view-buffer (buffer)
  (view-buffer buffer
               `(lambda
                  (buffer)
                  (kill-buffer buffer)
                  (when (buffer-live-p ,(current-buffer))
                    (switch-to-buffer ,(current-buffer))))))



(provide 'fsvn-mode)

;;; fsvn-mode.el ends here
