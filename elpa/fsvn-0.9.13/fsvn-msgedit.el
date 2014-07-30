;;; fsvn-msgedit.el --- Subversion message edit mode for fsvn.el


;;; History:
;; 

;;; Commentary:
;; 

;;; Code:



(require 'fsvn-mode)
(require 'fsvn-url)



(defvar text-mode-map)
(defvar last-command)
(defvar dired-re-no-dot)



(fsvn-defstruct log-edit-message
  file region)

(defconst fsvn-message-edit-buffer-name "Fsvn Log Message")
(defconst fsvn-message-edit-buffer-local-variables
  '(
    (fsvn-message-edit-file-select-buffer)
    (fsvn-message-edit-last-message)
    (fsvn-buffer-repos-info)
    ))

(defvar fsvn-message-edit-file-encoding fsvn-svn-common-coding-system)

(defvar fsvn-message-edit-last-message nil)
(defvar fsvn-message-edit-file-select-buffer nil)
(defvar fsvn-message-edit-mode-map nil)
(unless fsvn-message-edit-mode-map
  (setq fsvn-message-edit-mode-map
        (let ((map (make-sparse-keymap)))
          (set-keymap-parent map text-mode-map)

          (define-key map "\C-c\C-l" 'fsvn-restore-default-window-setting)

          (define-key map "\en" 'fsvn-message-edit-next-message)
          (define-key map "\ep" 'fsvn-message-edit-previous-message)
          (define-key map "\er" 'fsvn-message-edit-re-search-message-forward)
          (define-key map "\e\C-r" 'fsvn-message-edit-re-search-message-backward)

          map)))

(defcustom fsvn-message-edit-mode-hook nil
  "Run at the very end of `fsvn-message-edit-mode'."
  :group 'fsvn
  :type 'hook)

(defcustom fsvn-message-edit-mode-prepared-hook nil
  "Run at the very end of `fsvn-message-edit-mode' is prepared."
  :group 'fsvn
  :type 'hook)

;; * fsvn-message-edit-mode internal function

(defun fsvn-message-edit-mode ()
  "Major mode for editing text in Subversion subcommand.

Entry to this mode calls the value of `fsvn-message-edit-mode-hook'.

Keybindings:
\\{fsvn-message-edit-mode-map}

"
  (fsvn-global-initialize-mode)
  (use-local-map fsvn-message-edit-mode-map)
  (setq major-mode 'fsvn-message-edit-mode)
  (setq mode-name "Fsvn Log Message Edit")
  (setq buffer-undo-list nil)
  (setq buffer-read-only nil)
  (fsvn-make-buffer-variables fsvn-message-edit-buffer-local-variables)
  (erase-buffer)
  (run-mode-hooks 'fsvn-message-edit-mode-hook))

(defun fsvn-message-edit-generate-buffer ()
  (generate-new-buffer fsvn-message-edit-buffer-name))

(defmacro fsvn-message-edit-each-buffers (minor-var &rest form)
  `(fsvn-each-buffer-mode 'fsvn-message-edit-mode
     (when (symbol-value ,minor-var)
       (progn ,@form))))

(defun fsvn-message-edit-insert-log-file (file)
  (let ((start (point-marker))
        end)
    (save-excursion
      (let ((coding-system-for-read fsvn-message-edit-file-encoding))
        (forward-char (cadr (insert-file-contents file)))
        (setq end (point-marker))))
    (setq fsvn-message-edit-last-message
          (fsvn-struct-log-edit-message-make :file file :region (cons start end)))))

(defun fsvn-message-edit-delete-if-repeated ()
  (when (and (fsvn-message-edit-repeated-command-p)
             fsvn-message-edit-last-message)
    (let ((region (fsvn-struct-log-edit-message-get-region fsvn-message-edit-last-message)))
      (delete-region (car region) (cdr region)))))

(defun fsvn-message-edit-repeated-command-p ()
  (memq last-command 
        '(fsvn-message-edit-previous-message 
          fsvn-message-edit-next-message
          fsvn-message-edit-re-search-message-backward
          fsvn-message-edit-re-search-message-forward)))

(defun fsvn-message-edit-find-file (reverse)
  (let* ((message fsvn-message-edit-last-message)
         (find-list (fsvn-message-edit-message-files))
         (len (length find-list))
         (i 0)
         file)
    (if (null message)
        (car (last find-list))
      (when reverse
        (setq find-list (nreverse find-list)))
      (setq file (fsvn-struct-log-edit-message-get-file message))
      (catch 'found
        (mapc
         (lambda (f)
           (when (fsvn-file= file f)
             (throw 'found (nth (mod (1- i) len) find-list)))
           (setq i (1+ i)))
         find-list)
        nil))))

(defun fsvn-message-edit-search-file (regexp reverse)
  (let* ((message fsvn-message-edit-last-message)
         (find-list (fsvn-message-edit-message-files)))
    (if (null message)
        (car (last find-list))
      (when reverse
        (setq find-list (nreverse find-list)))
      (catch 'found
        (let ((coding-system-for-read fsvn-message-edit-file-encoding))
          (mapc
           (lambda (f)
             (with-temp-buffer
               (insert-file-contents f)
               (goto-char (point-min))
               (when (re-search-forward regexp nil t)
                 (throw 'found f))))
           find-list)
          nil)))))

(defun fsvn-message-edit-message-files ()
  (let* ((dir (fsvn-message-edit-get-message-directory))
         (ret (directory-files-and-attributes dir t dired-re-no-dot)))
    (setq ret
          (sort ret (lambda (x y)
                      (time-less-p (nth 5 (cdr x)) (nth 5 (cdr y))))))
    (mapcar 'car ret)))



(defun fsvn-message-edit-get-message-directory ()
  (let ((dir (fsvn-expand-file (md5 (fsvn-buffer-repos-root)) (fsvn-logmessage-directory))))
    (unless (fsvn-file-exact-directory-p dir)
      (make-directory dir t))
    dir))

(defun fsvn-message-edit-create-message-file ()
  (unless (eq major-mode 'fsvn-message-edit-mode)
    (error "Can't execute this function in this mode"))
  (let (tmpfile)
    (if (= (buffer-size) 0)
        (when (fsvn-config-log-empty-warnings (fsvn-buffer-repos-root))
          (unless (y-or-n-p "Log message is empty.  Really commit? ")
            (fsvn-quit "Empty message")))
      (setq tmpfile (fsvn-message-edit-make-message-file))
      (let ((coding-system-for-write fsvn-message-edit-file-encoding))
        (write-region (point-min) (point-max) tmpfile nil 'no-msg))
      tmpfile)))

(defun fsvn-message-edit-make-message-file ()
  (let* ((dir (fsvn-message-edit-get-message-directory))
         (temporary-file-directory dir))
    (make-temp-file (format-time-string "%s"))))

;; * fsvn-message-edit-mode interactive commands

(defun fsvn-message-edit-previous-message ()
  (interactive)
  (let (file)
    (fsvn-message-edit-delete-if-repeated)
    (when (setq file (fsvn-message-edit-find-file nil))
      (fsvn-message-edit-insert-log-file file))))

(defun fsvn-message-edit-next-message ()
  (interactive)
  (let (file)
    (fsvn-message-edit-delete-if-repeated)
    (when (setq file (fsvn-message-edit-find-file t))
      (fsvn-message-edit-insert-log-file file))))

(defun fsvn-message-edit-re-search-message-forward (regexp)
  (interactive (list (read-from-minibuffer "Regexp: ")))
  (let (file)
    (unless (setq file (fsvn-message-edit-search-file regexp nil))
      (error "No matched file."))
    (fsvn-message-edit-delete-if-repeated)
    (fsvn-message-edit-insert-log-file file)))

(defun fsvn-message-edit-re-search-message-backward (regexp)
  (interactive (list (read-from-minibuffer "Regexp: ")))
  (let (file)
    (unless (setq file (fsvn-message-edit-search-file regexp t))
      (error "No matched file."))
    (fsvn-message-edit-delete-if-repeated)
    (fsvn-message-edit-insert-log-file file)))



(defconst fsvn-message-edit-mode-menu-spec
  '("fsvn"
    ["Restore Window" fsvn-restore-default-window-setting t]
    ["Next Message in History" fsvn-message-edit-next-message t]
    ["Previous Message in History" fsvn-message-edit-previous-message t]
    ["Search Message Forward" fsvn-message-edit-re-search-message-forward t]
    ["Search Message Backward" fsvn-message-edit-re-search-message-backward t]
    ))

(easy-menu-define fsvn-message-edit-mode-menu
  fsvn-message-edit-mode-map
  "Menu used in Fsvn Log Message Edit mode."
  fsvn-message-edit-mode-menu-spec)



(provide 'fsvn-msgedit)

;;; fsvn-msgedit.el ends here
