;;; fsvn-diff.el --- Diff utility for fsvn.el


;;; History:
;; 

;;; Commentary:
;; 

;;; Code:



(require 'diff)
(require 'diff-mode)
(require 'fsvn-deps)
(require 'fsvn-ui)



(defvar auto-mode-alist)
(defvar ediff-window-A)
(defvar ediff-window-B)



(defvar fsvn-ediff-previous-configuration nil)

(defcustom fsvn-diff-highlight-trailing-whitespace t
  "Specifies where to highlight whitespace errors.
`t' means in all diffs, and `nil' means nowhere."
  :group 'fsvn)

(defcustom fsvn-diff-highlight-added-whitespace nil
  "Specifies what to highlight whitespace errors.
`t' means in only added trailing whitespace. See
`fsvn-diff-highlight-trailing-whitespace'"
  :group 'fsvn)

(fsvn-defstruct ediff-config
  window file1 file2)

(defun fsvn-ediff-files (file1 file2)
  (let* ((fsvn-ediff-previous-configuration
          (fsvn-struct-ediff-config-make
           :window (current-window-configuration)
           :file1 (cons file1 (get-file-buffer file1))
           :file2 (cons file2 (get-file-buffer file2))))
         (hook '(fsvn-ediff-startup-hooks)))
    (ediff-files file1 file2 hook)
    (fsvn-ediff-cleanup-file-history)))

(defun fsvn-ediff-cleanup-file-history ()
  (setq file-name-history
        (fsvn-mapitem
         (lambda (x)
           (and (fsvn-url-descendant-p (fsvn-ediff-directory) (expand-file-name x))
                x))
         file-name-history)))

(defun fsvn-ediff-directories (dir1 dir2)
  (ediff-directories dir1 dir2 nil))

(defun fsvn-ediff-startup-hooks ()
  (let ((func `(lambda () (fsvn-ediff-exit-hook ',fsvn-ediff-previous-configuration))))
    (add-hook 'ediff-after-quit-hook-internal func nil 'local)))

(defun fsvn-ediff-exit-hook (prev-config)
  (set-window-configuration (fsvn-struct-ediff-config-get-window prev-config))
  (mapc
   (lambda (file)
     (let ((name (car file))
           (buffer (cdr file)))
       (unless buffer
         (when (get-file-buffer name)
           (kill-buffer (get-file-buffer name))))))
   (list (fsvn-struct-ediff-config-get-file1 prev-config)
         (fsvn-struct-ediff-config-get-file2 prev-config))))

(defun fsvn-ediff-hash-directory (urlrev)
  (let ((dir (fsvn-expand-file (md5 (fsvn-urlrev-dirname urlrev)) 
                               (fsvn-ediff-directory))))
    (unless (file-directory-p dir)
      (make-directory dir t))
    dir))

(defun fsvn-ediff-make-temp-file (urlrev)
  (let* ((topdir (fsvn-ediff-hash-directory urlrev))
         (file (fsvn-expand-file (fsvn-url-ediff-filename urlrev) topdir)))
    (write-region (point-min) (point-min) file nil 'no-msg)
    file))

(defun fsvn-ediff-make-temp-directory (urlrev)
  (let* ((topdir (fsvn-ediff-hash-directory urlrev))
         (dir (fsvn-expand-file (fsvn-url-ediff-filename urlrev) topdir)))
    (unless (file-directory-p dir)
      (make-directory dir t))
    dir))

(defun fsvn-ediff-prepared-file (urlrev)
  (if (fsvn-url-urlrev-p urlrev)
      (let ((file (fsvn-ediff-make-temp-file urlrev)))
        (unless (fsvn-save-file urlrev file t)
          (error "Error occur while saving remote file"))
        file)
    urlrev))

(defun fsvn-ediff-between-urlrevs (urlrev1 urlrev2 directory-p)
  (if directory-p
      (fsvn-ediff-urlrev-directories urlrev1 urlrev2)
    (fsvn-ediff-urlrev-files urlrev1 urlrev2)))

(defun fsvn-ediff-urlrev-files (urlrev1 urlrev2)
  (let ((file1 (fsvn-ediff-prepared-file urlrev1))
        (file2 (fsvn-ediff-prepared-file urlrev2)))
    (fsvn-ediff-files file1 file2)))

;;FIXME not well designed
;;    ex: after async process, suddenly prompt to minibuffer "Execute ediff?"
(defun fsvn-ediff-urlrev-directories (urlrev1 urlrev2)
  (cond
   ((and (fsvn-url-urlrev-p urlrev1)
         (fsvn-url-urlrev-p urlrev2))
    (fsvn-async-let ((export-dir1 (fsvn-ediff-make-temp-directory urlrev1))
                     (export-dir2 (fsvn-ediff-make-temp-directory urlrev2))
                     (buffer (fsvn-make-temp-buffer))
                     (urlrev2 urlrev2))
      (fsvn-start-command "export" buffer "--force" urlrev1 export-dir1)
      (fsvn-start-command "export" buffer "--force" urlrev2 export-dir2)
      (kill-buffer buffer)
      (when (y-or-n-p "Execute ediff? ")
        (fsvn-ediff-directories export-dir1 export-dir2))))
   ((and (fsvn-url-urlrev-p urlrev1)
         (not (fsvn-url-urlrev-p urlrev2)))
    (let* ((export-dir (fsvn-ediff-make-temp-directory urlrev1))
           (buffer (fsvn-make-temp-buffer))
           (proc (fsvn-start-command "export" buffer "--force" urlrev1 export-dir))
           (sentinel (fsvn-ediff-directories-create-sentinel export-dir urlrev2 buffer)))
      (set-process-sentinel proc sentinel)))
   ((and (not (fsvn-url-urlrev-p urlrev1))
         (fsvn-url-urlrev-p urlrev2))
    (let* ((export-dir (fsvn-ediff-make-temp-directory urlrev2))
           (buffer (fsvn-make-temp-buffer))
           (proc (fsvn-start-command "export" buffer "--force" urlrev2 export-dir))
           (sentinel (fsvn-ediff-directories-create-sentinel urlrev1 export-dir buffer)))
      (set-process-sentinel proc sentinel)))
   (t
    (fsvn-ediff-directories urlrev1 urlrev2)))
  t)

(defun fsvn-ediff-directories-create-sentinel (dir1 dir2 buffer)
  `(lambda (p e)
     (fsvn-process-exit-handler p e
       (kill-buffer ,buffer)
       (let ((inhibit-quit t))
         (when (y-or-n-p "Execute ediff? ")
           (fsvn-ediff-directories ,dir1 ,dir2))))))

(defun fsvn-diff-files (file1 file2 switches)
  (let ((buffer (diff file1 file2 switches)))
    (fsvn-diff-setup-mode buffer (list file1 file2 switches))))

;; subcommand `diff' utility

(defvar fsvn-diff-buffer-subcommand-args nil)

(defun fsvn-diff-start-process (&rest args)
  (let ((buffer (fsvn-diff-get-buffer args))
        proc)
    (prog1
        (setq proc (fsvn-start-command-display "diff" buffer args))
      (fsvn-diff-setup-mode buffer args)
      (fsvn-buffer-popup-as-information buffer)
      (set-process-sentinel proc 'fsvn-diff-process-sentinel))))

(defun fsvn-diff-start-files-process (new-file old-file &rest args)
  (let ((diff-args (list
                    (format "--new=%s" new-file)
                    (format "--old=%s" old-file))))
    (fsvn-diff-start-process diff-args args)))

(defun fsvn-diff-process-sentinel (proc event)
  (let ((buf (process-buffer proc)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when fsvn-diff-highlight-trailing-whitespace
          (fsvn-diff-highlight-hunk))))))

(defun fsvn-diff-get-buffer (diff-args)
  (let ((args (fsvn-command-args-canonicalize diff-args))
        buffer)
    (catch 'found
      (mapc
       (lambda (b)
         (with-current-buffer b
           (when (and fsvn-diff-buffer-subcommand-args
                      (equal fsvn-diff-buffer-subcommand-args args))
             (let ((inhibit-read-only t)
                   buffer-read-only)
               (erase-buffer)
               (throw 'found b)))))
       (buffer-list))
      (generate-new-buffer (format "*Fsvn diff %s*" (fsvn-diff-buffer-key-name args))))))

(defun fsvn-diff-setup-mode (buffer args)
  (with-current-buffer buffer
    (diff-mode)
    (let ((real-args (fsvn-command-args-canonicalize args)))
      (set (make-local-variable 'diff-added-face) fsvn-diff-add-face)
      (set (make-local-variable 'diff-removed-face) fsvn-diff-delete-face)
      (set (make-local-variable 'fsvn-popup-result-buffer-p) t)
      (set (make-local-variable 'fsvn-diff-buffer-subcommand-args) real-args))
    (setq buffer-read-only t)))

(defun fsvn-diff-highlight-hunk ()
  (save-excursion
    (goto-char (point-min))
    (let ((regexp (concat
                   "^"
                   (if fsvn-diff-highlight-added-whitespace
                       "[+]"
                     "[-+]")
                   ".*?\\([ \t]+\\)$")))
      (while (re-search-forward regexp nil t)
        (let ((ov (make-overlay (match-beginning 1) (match-end 1))))
          (overlay-put ov 'face 'fsvn-diff-whitespace-warning-face))))))

(defun fsvn-diff-buffer-key-name (args)
  (catch 'decide
    (mapc
     (lambda (x)
       (cond
        ((fsvn-url-local-p x)
         (throw 'decide (fsvn-url-filename x)))
        ((fsvn-url-repository-p x)
         (throw 'decide (fsvn-url-filename x)))
        ((string-match fsvn-diff-subcommand-arg-regexp x)
         (throw 'decide (fsvn-urlrev-filename (match-string 2 x))))))
     args)
    (error "Diff keyname not found")))



(defun fsvn-diff-file-alist (file)
  (let (base-line wc-line ret)
    (with-temp-buffer
      (unless (= (fsvn-call-command "diff" (current-buffer) file) 0)
        (error "Executing error while `diff'"))
      (goto-char (point-min))
      (while (re-search-forward fsvn-diff-separated-regexp nil t)
        (setq base-line (string-to-number (match-string 1))
              wc-line (string-to-number (match-string 3)))
        (forward-line 1)
        (while (not (or (looking-at "^@@") (eobp)))
          (cond
           ((looking-at "^-")
            (setq base-line (1+ base-line))
            (setq ret (cons (cons nil base-line) ret)))
           ((looking-at "^\\+")
            (setq wc-line (1+ wc-line))
            (setq ret (cons (cons wc-line nil) ret)))
           (t
            (setq base-line (1+ base-line))
            (setq wc-line (1+ wc-line))))
          (forward-line 1))))
    (nreverse ret)))



(defun fsvn-diff-create-patch (patch-file &rest args)
  (let (proc)
    (write-region "" nil patch-file nil 'no-msg)
    (setq proc (apply 'fsvn-start-process nil "diff" args))
    (set-process-sentinel proc (lambda (proc event) 
                                 (message "Patch was created.")))
    (set-process-filter proc `(lambda (proc event)
                                (write-region event nil ,patch-file t 'no-msg)))
    proc))



(provide 'fsvn-diff)

;;; fsvn-diff.el ends here
