;;; fsvn-fs.el --- FileSystem utility for fsvn.el


;;; History:
;; 

;;; Commentary:
;; 

;;; Code:



(require 'dired)
(require 'fsvn-url)



(defvar default-file-name-coding-system)
(defvar file-name-coding-system)



(defun fsvn-set-file-read-only (file value)
  "Set or unset FILE owner writable bit."
  (let* ((oldmode (file-modes file))
         (newmode (if value (logxor oldmode ?\200) (logior oldmode ?\200))))
    (set-file-modes file newmode)))

(defun fsvn-file-read-only-p (file)
  "FILE is read only or not.  See owner's bit."
  (= (logand ?\200 (file-modes file)) 0))

(defun fsvn-delete-directory (directory)
  "Recursive delete of DIRECTORY."
  (mapc
   (lambda (f)
     (if (not (eq t (car (file-attributes f))))
         (delete-file f)
       (fsvn-delete-directory f)))
   (directory-files directory t dired-re-no-dot))
  (if (file-symlink-p directory)
      (delete-file directory)
    (delete-directory directory))
  nil)

(defun fsvn-copy-directory (source destination &optional ignore-dot-svn)
  "Copy SOURCE directory to DESTINATION.
Overwrite all existing files.
IGNORE-DOT-SVN non-nil means do not copy subversion meta directory (Probablly .svn).
"
  (unless (file-directory-p destination)
    (make-directory destination t))
  (mapc
   (lambda (src)
     (let* ((filename (fsvn-file-name-nondirectory src))
            (dest (fsvn-expand-file filename destination)))
       (cond
        ((not (fsvn-file-exact-directory-p src))
         (copy-file src dest t t))
        ((and ignore-dot-svn (string= (fsvn-meta-dir-name) filename)))
        (t
         (fsvn-copy-directory src dest ignore-dot-svn)))))
   (directory-files source t dired-re-no-dot))
  nil)

(defun fsvn-file-device-equals (file1 file2)
  (equal (nth 11 (file-attributes file1)) (nth 11 (file-attributes file2))))

(defun fsvn-file-exact-file-p (file)
  (eq nil (car (file-attributes file))))

(defun fsvn-file-exact-directory-p (file)
  ;; dired have this code.
  (eq t (car (file-attributes file))))

(defun fsvn-file-symlink-p (file)
  (let ((attr (file-attributes file)))
    (and (stringp (car attr)) (car attr))))

(defun fsvn-file-size (file)
  (nth 7 (file-attributes file)))

(defun fsvn-file-name-coding-system ()
  (or file-name-coding-system default-file-name-coding-system))

(defun fsvn-recursive-directory-files (directory)
  "List DIRECTORY files recursively ignoring `.svn' directory."
  (let (files)
    (mapc
     (lambda (file)
       (cond
        ((string= (fsvn-file-name-nondirectory file) (fsvn-meta-dir-name)))
        ((fsvn-file-exact-directory-p file)
         (setq files (append files (list file) (fsvn-recursive-directory-files file))))
        (t
         (setq files (append files (list file))))))
     (directory-files directory t dired-re-no-dot))
    files))

(defun fsvn-guessed-recursive-count (directory threshold)
  "Return t if DIRECTORY has over THRESHOLD directories as child."
  (not (catch 'ng
         (fsvn-guessed-recursive-count-internal directory threshold 0)
         nil)))

(defun fsvn-guessed-recursive-count-internal (directory threshold count)
  (mapc
   (lambda (file)
     (cond
      ((string= (fsvn-file-name-nondirectory file) (fsvn-meta-dir-name)))
      ((fsvn-file-exact-directory-p file)
       (setq count (1+ count))
       (when (> count threshold)
         (throw 'ng t))
       (setq count (fsvn-guessed-recursive-count-internal file threshold count)))))
   (directory-files directory t dired-re-no-dot))
  count)

(defun fsvn-search-same-name-files (base-dir search-file threshold)
  "Search files with SEARCH-FILE name in BASE-DIR.
THRESHOLD controls recursive count."
  (let ((name (fsvn-file-name-nondirectory search-file))
        ret)
    (mapc
     (lambda (file)
       (let ((filename (fsvn-file-name-nondirectory file)))
         (cond
          ((fsvn-file= search-file file))
          ((fsvn-file= filename name)
           (setq ret (cons file ret))))
         (cond
          ((= threshold 0))
          ((string= (fsvn-meta-dir-name) name))
          ((fsvn-file-exact-directory-p file)
           (setq ret (append ret (fsvn-search-same-name-files
                                  file search-file (1- threshold))))))))
     (directory-files base-dir t dired-re-no-dot))
    (nreverse ret)))



(provide 'fsvn-fs)

;;; fsvn-fs.el ends here
