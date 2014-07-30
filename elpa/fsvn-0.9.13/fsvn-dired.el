;;; fsvn-dired.el --- Dired like functions for fsvn-browse-mode


;;; History:
;; 

;;; Commentary:
;; 

;;; Code:



(require 'dired)
(require 'dired-aux)
(require 'bytecomp)

(require 'fsvn-env)
(require 'fsvn-ui)
(require 'fsvn-browse)



(defvar transient-mark-mode)
(defvar current-prefix-arg)

(declare-function byte-compile-dest-file "bytecomp" (filename))
(declare-function byte-compile-file "bytecomp" (filename &optional load))



;; fsvn-dired group define
;;  * Connect between `dired-mode' and `fsvn-browse-mode'.
;;  * dired like command for `fsvn-browse-mode'.

(defgroup fsvn-dired nil
  "*Dired and Fsvn "
  :group 'fsvn
  )

(defvar fsvn-dired-force-dired nil)

(defun fsvn-dired-force-dired (dirname &optional switches)
  "If load this package, `fsvn-browse-mode' executed automatically.
This function suppress this behavior."
  (interactive (list (fsvn-read-directory-name "Dired dir: " default-directory) nil))
  (unwind-protect
      (progn
        (setq fsvn-dired-force-dired t)
        (dired dirname switches))
    (setq fsvn-dired-force-dired nil)))

(defmacro fsvn-dired-switch-by-major-mode (dired-form fsvn-form)
  `(cond 
     ((eq major-mode 'dired-mode)
      ,dired-form)
     ((eq major-mode 'fsvn-browse-mode)
      ,fsvn-form)
     (t
      nil)))

(defun fsvn-dired-toggle-browser ()
  "Browse between `dired-mode' and `fsvn-browse-mode'."
  (interactive)
  (fsvn-dired-switch-by-major-mode
   ;; change to fsvn
   (let ((file (fsvn-dired-get-filename nil t))
         (dir (fsvn-dired-current-directory)))
     (if (not (fsvn-directory-versioned-p dir))
         (message "Not a working copy.")
       (fsvn-working-copy (directory-file-name dir))
       (when file
         (fsvn-browse-goto-file file))))
   ;; change to dired
   (if fsvn-browse-repos-p
       (message "Not a local directory.")
     (let (file)
       (when (fsvn-current-filename)
         (setq file (fsvn-expand-file (fsvn-current-filename) (fsvn-browse-current-directory-url))))
       (fsvn-dired-force-dired (directory-file-name (fsvn-browse-current-directory-url)))
       (when file
         (dired-goto-file file))))))

(defmacro fsvn-dired-each-buffer (&rest form)
  `(fsvn-mapitem
    (lambda (buffer)
      (with-current-buffer buffer
        (when (memq major-mode '(fsvn-browse-mode dired-mode))
          ,@form)))
    (buffer-list)))

(defun fsvn-dired-get-filename (&optional localp no-error-if-not-filep)
  (fsvn-dired-switch-by-major-mode
   (dired-get-filename localp no-error-if-not-filep)
   (fsvn-dired-pseudo-get-filename localp no-error-if-not-filep)))

(defun fsvn-dired-goto-file (file)
  (fsvn-dired-switch-by-major-mode
   (dired-goto-file file)
   (fsvn-browse-goto-file file)))

(defun fsvn-dired-current-directory ()
  (fsvn-dired-switch-by-major-mode
   (dired-current-directory)
   (fsvn-browse-current-directory)))

(defun fsvn-dired-dwim-target-directory ()
  (fsvn-dired-switch-by-major-mode
   (dired-dwim-target-directory)
   (fsvn-browse-current-directory)))

(defun fsvn-dired-get-filenames ()
  (fsvn-dired-switch-by-major-mode
   (fsvn-dired-gather-selected-files)
   (fsvn-browse-gather-selected-files)))

(defun fsvn-dired-unmark-all-files (mark)
  (fsvn-dired-switch-by-major-mode
   (dired-unmark-all-files mark)
   (fsvn-browse-mark-all-unmark mark)))

(defun fsvn-dired-goto-first-file ()
  (fsvn-dired-switch-by-major-mode
   (let ((dir (fsvn-dired-current-directory)))
     (dired-goto-subdir dir)
     (while (and (not (eobp))
                 (not (dired-move-to-filename)))
       (forward-line 1)))
   (fsvn-browse-goto-first-file)))

(defun fsvn-dired-put-mark-on-current (&optional mark)
  (fsvn-dired-switch-by-major-mode
   (save-excursion
     (when (dired-move-to-filename)
       (let (buffer-read-only)
         (forward-line 0)
         (delete-char 1)
         (insert (or mark dired-marker-char)))))
   (when (fsvn-current-filename)
     (fsvn-browse-put-mark-point (or mark fsvn-mark-mark-char)))))

(defun fsvn-dired-get-marked-files (&optional localp arg filter distinguish-one-marked)
  (fsvn-dired-switch-by-major-mode
   (dired-get-marked-files localp arg filter distinguish-one-marked)
   (fsvn-dired-pseudo-get-marked-files localp arg filter distinguish-one-marked)))

(defun fsvn-dired-add-file (file &optional marker-char)
  (fsvn-dired-switch-by-major-mode 
   (condition-case nil
       ;;FIXME when tramp like remote directory makes error in `dired-move-to-end-of-filename'
       (dired-add-file (expand-file-name file) marker-char)
     (error))
   (fsvn-browse-add-file-entry file)))



;; dired extension

(defun fsvn-dired-gather-selected-files (&optional get-dot-file)
  "If region not activate, get current line's filename."
  (let (temp)
    (cond
     ((and transient-mark-mode mark-active)
      (fsvn-dired-region-files get-dot-file))
     ((setq temp (fsvn-dired-marked-files get-dot-file))
      temp)
     ((setq temp (fsvn-dired-get-filename))
      (list temp))
     (t
      nil))))

(defun fsvn-dired-region-files (&optional get-dot-file)
  (let ((start (region-beginning))
        (end (region-end))
        temp ret)
    (save-excursion
      (goto-char start)
      (while (and (not (= end (point-max)))
                  (not (fsvn-dired-get-filename nil t)))
        (forward-line 1))
      (while (and (>= end (point))
                  (dired-move-to-filename))
        (when (and (setq temp (fsvn-dired-get-filename nil t))
                   (or get-dot-file
                       (not (string-match "/\\.\\.?$" temp))))
          (setq ret (cons temp ret)))
        (forward-line 1)))
    (nreverse ret)))

(defun fsvn-dired-marked-files (&optional get-dot-file)
  (let ((regexp (dired-marker-regexp))
        ret temp)
    (save-excursion
      (fsvn-dired-goto-first-file)
      (while (and (not (eobp))
                  (dired-move-to-filename))
        (forward-line 0)
        (when (and (looking-at regexp)
                   (setq temp (fsvn-dired-get-filename))
                   (or get-dot-file
                       (not (string-match "/\\.\\.?$" temp))))
          (setq ret (cons temp ret)))
        (forward-line 1)))
    ret))



;; pseudo dired like function

(defun fsvn-dired-create-directory ()
  "Act like `dired-create-directory'. But not equals of this."
  (interactive)
  (fsvn-browse-wc-only
   (let* ((current (file-name-as-directory (fsvn-browse-current-path)))
          (dir (fsvn-read-directory-name "Create directory: " current)))
     (make-directory dir t)
     (when (fsvn-file-directly-under-p current dir)
       (forward-line 1)
       (let (buffer-read-only)
         (fsvn-browse-ls-insert-wc-entry dir)
         (fsvn-browse-put-status-1 dir ??)
         (forward-line -1))
       (fsvn-move-to-filename)))))

(defun fsvn-dired-do-load (files)
  "Act like `dired-do-load'. But not equals of this."
  (interactive (fsvn-dired-cmd-selected-files))
  (if (or (not (fsvn-interactive-p))
          (fsvn-browse-dired-confirm files 'load))
      (progn
        (mapc 'load-file files)
        (message "%d Load done." (length files)))
    (message "(No load performed)")))

(defun fsvn-dired-do-byte-compile (files)
  "Act like `dired-do-byte-compile'. But not equals of this."
  (interactive (fsvn-dired-cmd-selected-files))
  (if (or (not (fsvn-interactive-p))
          (fsvn-browse-dired-confirm files 'byte-compile))
      (progn
        (mapc
         (lambda (file)
           (let ((elc-file (byte-compile-dest-file file)))
             (byte-compile-file file)
             (fsvn-browse-redraw-wc-file-entry elc-file)))
         files)
        (message "%d Byte compile done." (length files)))
    (message "(No byte-compile performed)")))

(defun fsvn-dired-do-compress (files)
  ;;todo
  (interactive (fsvn-dired-cmd-selected-files))
  )

(defun fsvn-dired-do-delete (files)
  "Act like `dired-do-delete'. But not equals of this."
  (interactive (fsvn-dired-cmd-selected-files))
  (fsvn-dired-do-marked-delete files))

(defmacro fsvn-dired-do-move-overwrap (mover message &rest form)
  `(let (OVERWRITE-QUERY DEST)
     (when (and (= (length files) 1)
                (not (file-directory-p destination)))
       (setq DEST destination))
     (mapc
      (lambda (from)
        (let* ((to (or DEST (fsvn-expand-file (fsvn-file-name-nondirectory from) destination)))
               (overwrite (file-exists-p to))
               (dired-overwrite-confirmed
                (and overwrite
                     (dired-query 'OVERWRITE-QUERY "Overwrite `%s'?" to))))
          (,mover from to dired-overwrite-confirmed)
          (fsvn-browse-redraw-wc-file-entry from)
          (fsvn-browse-redraw-wc-file-entry to)))
      files)
     (message (format "%d file(s) %s." (length files) ,message))))

(defun fsvn-dired-do-rename (files destination)
  "Act like `dired-do-rename'. But not equals of this."
  (interactive (fsvn-dired-cmd-read-destination "Move" 'move))
  (fsvn-dired-do-move-overwrap dired-rename-file "renamed"))

(defun fsvn-dired-do-copy (files destination)
  "Act like `dired-do-copy'. But not equals of this."
  (interactive (fsvn-dired-cmd-read-destination "Copy" 'copy))
  (fsvn-dired-do-move-overwrap dired-copy-file "copied"))

(defun fsvn-dired-do-marked-delete (files)
  "Act like `dired-do-flagged-delete'. But not equals of this."
  (interactive (let ((files (fsvn-browse-gather-marked-files fsvn-mark-delete-char)))
                 (when (fsvn-file-member (fsvn-browse-current-directory) files)
                   (error "Cannot operate on `.'"))
                 (list files)))
  (if (and files
           (or (not (fsvn-interactive-p))
               (fsvn-browse-dired-confirm files 'delete dired-deletion-confirmer)))
      (progn
        (mapc
         (lambda (file)
           (dired-delete-file file dired-recursive-deletes)
           (fsvn-browse-redraw-wc-file-entry file))
         files)
        (message "%d deletions done." (length files)))
    (message "(No deletions performed)")))

(defun fsvn-dired-mark-delete-backup-files ()
  "Act like `dired-flag-backup-files'. But not equals of this."
  (interactive)
  (fsvn-browse-mark-matched-file "~$" fsvn-mark-delete-char))

(defun fsvn-dired-show-file-type (file)
  "Act like `dired-show-file-type'. But not equals of this."
  (interactive (list (fsvn-current-filename)))
  (with-temp-buffer
    (call-process "file" nil t t "--" file)
    (when (bolp)
      (backward-delete-char 1))
    (message "%s" (buffer-string))))

(defun fsvn-dired-copy-filename-as-kill (files)
  "Copy names of marked (or next ARG) files into the kill ring.
Act like `dired-copy-filename-as-kill' but not equal."
  (interactive (fsvn-dired-cmd-selected-files))
  (let ((string (mapconcat 'fsvn-urlrev-filename
                           files fsvn-dired-copy-filename-separator)))
    (kill-new string)
    (message "%s" string)))

(defun fsvn-dired-copy-filename-fullpath (files)
  "Copy names of marked (or next ARG) files into the kill ring.
See `fsvn-dired-copy-filename-as-kill' but kills full path."
  (interactive (fsvn-dired-cmd-selected-files))
  (let ((string (mapconcat 'identity files fsvn-dired-copy-filename-separator)))
    (kill-new string)
    (message "%s" string)))

(defun fsvn-dired-copy-repository-url (files)
  "Copy names of marked (or next ARG) files into the kill ring.
See `fsvn-dired-copy-filename-as-kill' but kills full path."
  (interactive (fsvn-dired-cmd-selected-files))
  (let* ((repos-dir (fsvn-browse-current-repository-url))
         (dir (fsvn-browse-current-path))
         (string (mapconcat (lambda (file)
                              (let (name)
                                (setq name 
                                      (if (fsvn-file= file dir)
                                          "."
                                        (fsvn-file-name-nondirectory file)))
                                (fsvn-expand-url name repos-dir)))
                            files fsvn-dired-copy-filename-separator)))
    (kill-new string)
    (message "%s" string)))

(defun fsvn-dired-pseudo-get-filename (&optional localp no-error-if-not-filep)
  (let ((file (fsvn-magic-point-file)))
    (when (and (null file) (not no-error-if-not-filep))
      (error "No file on this line"))
    file))

(defun fsvn-dired-pseudo-get-marked-files (&optional localp arg filter distinguish-one-marked)
  (let* (all-of-them)
    (if arg
        (cons (fsvn-dired-pseudo-get-filename) nil)
      (setq all-of-them (fsvn-browse-gather-marked-files))
      (cond
       ((and filter all-of-them)
        (fsvn-mapitem
         (lambda (file)
           (when (funcall filter file)
             file))
         all-of-them))
       ((and distinguish-one-marked (= (length all-of-them) 1))
        (cons t all-of-them))
       (all-of-them
        all-of-them)
       (t
        (cons (fsvn-dired-pseudo-get-filename) nil))))))

(defun fsvn-dired-cmd-read-destination (act op-symbol)
  (let ((from (car (fsvn-dired-cmd-selected-files)))
        to confirmer args)
    (cond
     ((= (length from) 1)
      (setq confirmer 'read-file-name)
      (setq args (list (format "%s to filename: " act))))
     (t
      (setq confirmer 'read-directory-name)
      (setq args (list (format "%s to directory: " act) nil nil t))))
    (setq to (apply 'dired-mark-pop-up act op-symbol
                    (mapcar 'fsvn-file-name-nondirectory from)
                    confirmer args))
    (list from to)))

(defun fsvn-dired-cmd-selected-files ()
  (let (tmp)
    (cond
     ((and current-prefix-arg
           (setq tmp (fsvn-browse-point-url)))
      (list (cons tmp nil)))
     ((setq tmp (fsvn-browse-gather-selected-files))
      (list tmp))
     (t
      (error "No file on this point")))))

(defun fsvn-dired-define-key (key def)
  (add-hook 'dired-mode-hook
            `(lambda () (define-key dired-mode-map ,key ',def)))
  (add-hook 'fsvn-browse-mode-hook
            `(lambda () (define-key fsvn-browse-mode-map ,key ',def))))

(defun fsvn-dired-define-browse-key (key def)
  (add-hook 'fsvn-browse-mode-hook
            `(lambda () (define-key fsvn-browse-mode-map ,key ',def))))

(defun fsvn-dired-define-browse-prefix-key (key def)
  (add-hook 'fsvn-browse-mode-hook
            `(lambda () (define-key fsvn-browse-prefix-map ,key ',def))))



;; modify dired definition

(defadvice dired
  (around fsvn-dired-mode (dirname &optional switches) disable)
  (if (and (not fsvn-dired-force-dired)
           (fsvn-directory-versioned-p dirname)
           (stringp fsvn-svn-command-internal)
           (executable-find fsvn-svn-command-internal))
      (condition-case err
          (fsvn-working-copy dirname)
        (error ad-do-it))
    ad-do-it))

(defadvice dired-goto-file
  (around fsvn-dired-goto-file-ad (file) disable)
  (if (eq major-mode 'fsvn-browse-mode)
      (fsvn-browse-goto-file file)
    ad-do-it))

(fsvn-dired-define-key "\C-c\C-d" 'fsvn-dired-toggle-browser)



;; modify fsvn-browse definition

(fsvn-dired-define-browse-key "+" 'fsvn-dired-create-directory)
(fsvn-dired-define-browse-key "B" 'fsvn-dired-do-byte-compile)
(fsvn-dired-define-browse-key "C" 'fsvn-dired-do-copy)
(fsvn-dired-define-browse-key "D" 'fsvn-dired-do-delete)
(fsvn-dired-define-browse-key "L" 'fsvn-dired-do-load)
(fsvn-dired-define-browse-key "R" 'fsvn-dired-do-rename)
(fsvn-dired-define-browse-key "W" 'fsvn-dired-copy-filename-fullpath)
(fsvn-dired-define-browse-key "Z" 'fsvn-dired-do-compress)
(fsvn-dired-define-browse-key "w" 'fsvn-dired-copy-filename-as-kill)
(fsvn-dired-define-browse-key "x" 'fsvn-dired-do-marked-delete)
(fsvn-dired-define-browse-key "y" 'fsvn-dired-show-file-type)
(fsvn-dired-define-browse-key "~" 'fsvn-dired-mark-delete-backup-files)

(fsvn-dired-define-browse-prefix-key "w" 'fsvn-dired-copy-repository-url)



(provide 'fsvn-dired)

;;; fsvn-dired.el ends here
