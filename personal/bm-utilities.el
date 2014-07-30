
;; functions common to both backups-mode and backup-walker
(defun bm-kill-all-buffers (data)
  (let* ((minor-buffers (bm-other-buffers 'backups-minor-mode-p))
	 (backups-buffer (cdr (assq :backups-buffer data)))
	 (walking-buffer (cdr (assq :walking-buffer data)))
	 (first-config (bm-get-first-config data)))

    ;; first, cleanup all minor mode open buffers
    (bm-kill-open-buffers minor-buffers)

    ;; then, cleanup the backups-buffer
    (when (buffer-live-p backups-buffer)
      (kill-buffer backups-buffer))

    ;; then, cleanup the backup-walker buffer
    (when (buffer-live-p walking-buffer)
      (kill-buffer walking-buffer))

    ;; finally, reset the window configuration
    (set-window-configuration
     (bm-get-first-config data))))

(defun bm-kill-open-buffers (buffers)
  (mapc (lambda (buffer)
	  (when (buffer-live-p buffer)
	    (kill-buffer buffer)))
	buffers))

(defun bm-kill-popup-buffer (data)
  (let ((second-config (bm-get-second-config data))
	others)
    (kill-buffer)
    (setq others (bm-other-buffers 'backups-minor-mode-p))
    (if others
	(switch-to-buffer (car others))
      (set-window-configuration second-config))))

(defun bm-other-buffers (pred)
  (filter (lambda (buf)
	    (with-current-buffer buf
	      (funcall pred)))
	  (buffer-list)))

(defun bm-switch-to-window (buffer-or-name pred)
  (let ((similar-window (get-window-with-predicate (make-backups-window-p pred)))
	(window-count (length (window-list)))
	width
	height)
    (if (and similar-window (window-live-p similar-window))
	(select-window similar-window)
      (if (>= window-count 4)
	  (other-window 1) ;; don't split if 4+ windows exist
	(progn
	  (select-window (get-largest-window))
	  (setq width (window-width))
	  (setq height (window-height))
	  (split-window nil nil
			(> (/ width height) 2.1))))) ;; heuristic to popup vertically or horizontally
    (switch-to-buffer buffer-or-name)))

(defun backups-major-mode-p ()
  (and (or (eq major-mode 'backups-mode)
	   (eq major-mode 'backup-walker-mode))
       (not diff-backup-mode)))

(defun backups-minor-mode-p ()
  (or diff-backup-mode
      view-backup-mode
      backup-walker-minor-mode))

(defun make-backups-window-p (pred)
  (lexical-let ((pred pred))
    (lambda (window)
      (set-buffer (window-buffer window))
      (funcall pred))))

;;; list-backups helper methods
(defun bm-backup-files (original-file)
  (let* ((backup-file (file-name-sans-versions
		       (make-backup-file-name (expand-file-name original-file))))
	 (backup-directory (file-name-directory backup-file)))
    (mapcar
     (lambda (f) (concat backup-directory f))
     (file-name-all-completions
      (file-name-nondirectory backup-file)
      backup-directory))))

(defun bm-get-sorted-backups (original-file backup-files-function)
  (flet ((file-sort-p (file-name1 file-name2)
		      (let ((version1 (bm-make-version-number file-name1))
			    (version2 (bm-make-version-number file-name2)))
			(> version1 version2))))
    (mapcar 'bm-make-file
	    (cons original-file (sort
				 (funcall backup-files-function original-file)
				 'file-sort-p)))))

(defun bm-make-version-number (file-name)
  (let ((try-version-index (string-match "~[0-9]+~$" file-name)))
    (when try-version-index
      (bm-full-version-number file-name try-version-index))))

(defun bm-full-version-number (file-name start &optional number-str)
  (let* ((number-str (or number-str ""))
	 (number (string-to-number number-str)))
    (if (< start (length file-name))
	(let ((current-char (substring file-name (+ 1 start) (+ 2 start))))
	  (cond ((equal current-char "0") (bm-full-version-number file-name (+ 1 start) (concat number-str current-char)))
		((equal (string-to-number current-char) 0) number)
		(t (bm-full-version-number file-name (+ 1 start) (concat number-str current-char)))))
      number)))

;;; file structure methods
(defun bm-make-file (file-name)
  (flet ((make-last-modified-date
	  (file-name)
	  (let ((last-modified-date
		 (shell-command-to-string
		  (funcall last-modified-date-command-function file-name))))
	    (when (not (equal (car (split-string last-modified-date)) unknown-last-modified-date))
	      last-modified-date))))
    (list
     (bm-make-version-number file-name)
     (make-last-modified-date file-name)
     file-name)))

(defun bm-get-version (file)
  (nth 0 file))

(defun bm-get-last-modified-date (file)
  (nth 1 file))

(defun bm-get-file-name (file)
  (nth 2 file))

;; data-alist helper methods
(defun bm-get-original-file (data)
  (cdr (assq :original-file data)))

(defun bm-get-backups (data)
  (cdr (assq :backups data)))

(defun bm-get-marked-for-purging (data)
  (cdr (assq :marked-for-purging data)))

(defun bm-get-first-config (data)
  (cdr (assq :first-config data)))

(defun bm-get-second-config (data)
  (cdr (assq :second-config data)))

;; generic helper functions
(defun filter (condp lst)
  (delq nil
	(mapcar (lambda (x) (and (funcall condp x) x)) lst)))

(defun diff-no-select (old new &optional switches no-async)
  (save-window-excursion
    (diff old new switches no-async))
  (get-buffer-create "*Diff*"))

(defun bm-rename-buffer (filename data)
  (rename-buffer (concat (file-name-nondirectory (bm-get-original-file data))
			 " "
			 (number-to-string (bm-make-version-number filename)))))

;;; variables common to both backups-mode and backup-walker
;; minor mode variables
(defvar backup-walker-minor-mode nil "non-nil if backup walker minor mode is enabled")
(make-variable-buffer-local 'backup-walker-minor-mode)

(defvar view-backup-mode nil "non-nil if viewing a backup from backups-mode")
(make-variable-buffer-local 'view-backup-mode)

(defvar diff-backup-mode nil "non-nil if diffing a backup from backups-mode")
(make-variable-buffer-local 'diff-backup-mode)

;; common string literals
(defvar bm-revert-message "Use a capital R to revert")
(defvar bm-current-file-message "Cannot view the current file in read-only mode")
(defvar bm-no-file-message "No file on this line")
