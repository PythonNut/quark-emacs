;;; fsvn-magic.el --- Magic utility for subversion


;;; History:
;; 

;;; Commentary:
;; 

;;; Code:



(require 'fsvn-deps)
(require 'fsvn-env)
(require 'fsvn-ui)
(require 'fsvn-browse)
(require 'fsvn-url)
(require 'fsvn-xml)



(defvar system-type)
(defvar last-coding-system-used)
(defvar backup-enable-predicate)
(defvar inhibit-file-name-handlers)
(defvar inhibit-file-name-operation)
(defvar find-file-hook)



(defconst fsvn-magic-file-name-regexp
  (let ((top "\\`/fsvn")
        tmp)
    (concat top "@" "\\(" "HEAD" "\\|" "[0-9]+" "\\|" fsvn-revision-date-regexp "\\)" "/")))

(defconst fsvn-magic-handler-alist
  '(
    (copy-file                           .  fsvn-magic-copy-file)
    (delete-directory                    .  fsvn-magic-delete-directory)
    (delete-file                         .  fsvn-magic-delete-file)
    (directory-file-name                 .  fsvn-magic-directory-file-name)
    (directory-files                     .  fsvn-magic-directory-files)
    (directory-files-and-attributes      .  fsvn-magic-directory-files-and-attributes)
    (expand-file-name                    .  fsvn-magic-expand-file-name)
    (file-attributes                     .  fsvn-magic-file-attributes)
    (file-directory-p                    .  fsvn-magic-file-directory-p)
    (file-name-directory                 .  fsvn-magic-file-name-directory)
    (file-executable-p                   .  fsvn-magic-file-executable-p)
    (file-exists-p                       .  fsvn-magic-file-exists-p)
    (file-modes                          .  fsvn-magic-file-modes)
    (file-name-all-completions           .  fsvn-magic-file-name-all-completions)
    (file-name-completion                .  fsvn-magic-file-name-completion)
    (file-readable-p                     .  fsvn-magic-file-readable-p)
    (file-regular-p                      .  fsvn-magic-file-regular-p)
    (file-remote-p                       .  fsvn-magic-file-remote-p)
    (file-symlink-p                      .  fsvn-magic-file-symlink-p)
    (file-truename                       .  fsvn-magic-file-truename)
    (file-writable-p                     .  fsvn-magic-file-writable-p)
    (get-file-buffer                     .  fsvn-magic-get-file-buffer)
    (insert-directory                    .  fsvn-magic-insert-directory)
    (insert-file-contents                .  fsvn-magic-insert-file-contents)
    (make-directory                      .  fsvn-magic-make-directory)
    (make-directory-internal             .  fsvn-magic-make-directory-internal)
    (set-file-modes                      .  fsvn-magic-set-file-modes)
    (set-file-times                      .  fsvn-magic-set-file-times)
    (set-visited-file-modtime            .  fsvn-magic-set-visited-file-modtime)
    (file-local-copy                     .  fsvn-magic-file-local-copy)
    (file-name-sans-versions             .  fsvn-magic-file-name-sans-versions)
    (find-file-noselect                  .  fsvn-magic-find-file-noselect)
    (find-backup-file-name               .  fsvn-magic-find-backup-file-name)
    (file-newer-than-file-p              .  fsvn-magic-file-newer-than-file-p)
    (make-auto-save-file-name            .  fsvn-magic-make-auto-save-file-name)
    (rename-file                         .  fsvn-magic-rename-file)
    (access-file                         .  fsvn-magic-access-file)
    (write-region                        .  fsvn-magic-write-region)
    (diff-latest-backup-file             .  fsvn-magic-diff-latest-backup-file)
    (load                                .  fsvn-magic-load)
    (file-ownership-preserved-p          .  fsvn-magic-file-ownership-preserved-p)
    (file-accessible-directory-p         .  fsvn-magic-file-accessible-directory-p)
    (verify-visited-file-modtime         .  fsvn-magic-verify-visited-file-modtime)
    (unhandled-file-name-directory       .  fsvn-magic-unhandled-file-name-directory)

    ;; todo not implements
    (start-file-process                  .  fsvn-magic-start-file-process)
    (make-symbolic-link                  .  fsvn-magic-make-symbolic-link)

    ;; no need to implement
    ;;     (substitute-in-file-name             .  fsvn-magic-substitute-in-file-name)
    ;;     (file-name-as-directory              .  fsvn-magic-file-name-as-directory)
    ;;     (byte-compiler-base-file-name        .  fsvn-magic-byte-compiler-base-file-name)
    ;;     (process-file                        .  fsvn-magic-process-file)
    ;;     (dired-uncache                       .  fsvn-magic-dired-uncache)
    ;;     (shell-command                       .  fsvn-magic-shell-command)
    ;;     (vc-registered                       .  fsvn-magic-vc-registered)
    ;;     (add-name-to-file                    .  fsvn-magic-add-name-to-file)
    ;;     (dired-compress-file                 .  fsvn-magic-dired-compress-file)
    ;;     (file-name-nondirectory              .  fsvn-magic-file-name-nondirectory)
    ))

(defconst fsvn-magic-version-dependent-functions
  '(
    (22 add-name-to-file copy-file delete-directory delete-file
        diff-latest-backup-file directory-file-name directory-files
        dired-call-process dired-compress-file dired-uncache
        expand-file-name file-accessible-directory-p file-attributes
        file-directory-p file-executable-p file-exists-p
        file-local-copy file-modes file-name-all-completions
        file-name-as-directory file-name-completion
        file-name-directory file-name-nondirectory
        file-name-sans-versions file-newer-than-file-p
        file-ownership-preserved-p file-readable-p file-regular-p
        file-symlink-p file-truename file-writable-p
        find-backup-file-name get-file-buffer insert-directory
        insert-file-contents load make-directory make-symbolic-link
        rename-file set-file-modes set-visited-file-modtime
        shell-command unhandled-file-name-directory vc-registered
        verify-visited-file-modtime write-region )

    (23 access-file add-name-to-file byte-compiler-base-file-name
        copy-file delete-directory delete-file
        diff-latest-backup-file directory-file-name directory-files
        directory-files-and-attributes dired-compress-file
        dired-uncache expand-file-name file-accessible-directory-p
        file-attributes file-directory-p file-executable-p
        file-exists-p file-local-copy file-modes
        file-name-all-completions file-name-as-directory
        file-name-completion file-name-directory
        file-name-nondirectory file-name-sans-versions
        file-newer-than-file-p file-ownership-preserved-p
        file-readable-p file-regular-p file-remote-p file-symlink-p
        file-truename file-writable-p find-backup-file-name
        find-file-noselect get-file-buffer insert-directory
        insert-file-contents load make-auto-save-file-name
        make-directory make-directory-internal make-symbolic-link
        process-file rename-file set-file-modes set-file-times
        set-visited-file-modtime shell-command start-file-process
        substitute-in-file-name unhandled-file-name-directory
        vc-registered verify-visited-file-modtime write-region ))
  "For the time being make it.
FIXME Does Emacs have list like this? "
  )

(defconst fsvn-magic-modify-repository-functions
  '(
    copy-file
    delete-directory
    delete-file
    make-directory
    make-directory-internal
    make-symbolic-link
    rename-file
    write-region
    ))

(defvar fsvn-magic-cache nil)

(defvar fsvn-magic-disable-cache nil
  "When use asynchronous process.")

(defvar fsvn-magic-repository-roots nil)

;; for `fsvn-browse-mode'
(defun fsvn-magic-point-file ()
  (fsvn-magic-current-name fsvn-browse-point-urlrev))

(defun fsvn-magic-current-directory ()
  (fsvn-magic-current-name fsvn-browse-current-directory-urlrev))

(defun fsvn-magic-file-name-handler (operation &rest args)
  (let ((handler (assq operation fsvn-magic-handler-alist)))
    (cond
     ((and handler (functionp (cdr handler)))
      (prog1
          (apply (cdr handler) args)
        (when (memq operation fsvn-magic-modify-repository-functions)
          ;;FIXME
          (setq fsvn-magic-cache nil))
        ))
     (t
      ;; default handler
      (apply 'fsvn-magic-file-call-underlying operation args)))))

(defun fsvn-magic-insert-directory (file switches &optional wildcard full-directory-p)
  ;;FIXME other args
  (let* ((url (fsvn-magic-parse-file-name file)))
    (if full-directory-p
        (progn
          (insert (format "URL: %s\n" url))
          (insert "\n")
          (cond
           ((file-directory-p file)
            (mapc
             (lambda (x)
               (fsvn-magic-insert-directory-entry x))
             (fsvn-magic-get-ls url)))
           (t
            (let ((entry (fsvn-magic-get-ls-entry url)))
              (when entry
                (fsvn-magic-insert-directory-entry entry)))))
          nil)
      (fsvn-magic-insert-directory-entry
       (fsvn-magic-get-list/info-entry url)))))

(defun fsvn-magic-insert-directory-entry (entry)
  (let ((dirp (eq (fsvn-magic-xml-list/info-entry.kind entry) 'dir))
        (filename (fsvn-magic-xml-list/info-entry=name entry)))
    (fsvn-set-filename-property filename)
    (insert (format "  %c %s %s %s %s %s\n"
                    (if dirp ?d fsvn-space-char)
                    (fsvn-magic-ls-revision entry)
                    (fsvn-magic-ls-author-column entry)
                    (fsvn-generic-format-file-size
                     (fsvn-safe-xml-lists->list->entry=>size$ entry))
                    (format-time-string
                     fsvn-generic-datetime-format 
                     (fsvn-magic-xml-list/info-entry=>commit=date entry))
                    filename
                    )))
  (save-excursion
    (forward-line -1)
    (delete-char 2)))

(defun fsvn-magic-ls-revision (entry)
  (fsvn-string-lpad
   (number-to-string (fsvn-magic-xml-list/info-entry=>commit.revision entry))
   fsvn-browse-ls-revision-length))

(defun fsvn-magic-ls-author-column (entry)
  (fsvn-browse-ls-author-string (fsvn-magic-xml-list/info-entry=>commit.author entry) nil))

(defun fsvn-magic-insert-file-contents (filename &optional visit begin end replace)
  (barf-if-buffer-read-only)
  (let* ((urlrev (fsvn-magic-parse-file-name filename))
         (tmpfile (fsvn-make-temp-file))
         (dest (current-buffer))
         ret)
    (when visit
      (setq buffer-file-name filename)
      (set-visited-file-modtime)
      ;; always disabled.
      (setq backup-enable-predicate (lambda (x) nil))
      (set-buffer-modified-p nil))
    (unless (fsvn-save-file urlrev tmpfile 'no-msg)
      (signal 'file-error (list "Opening svn file" filename)))
    (when replace
      (erase-buffer))
    (setq ret (insert-file-contents tmpfile nil begin end))
    (cons filename (cdr ret))))

(defun fsvn-magic-file-call-underlying (operation &rest args)
  (let* ((inhibit-file-name-handlers
          (cons 'fsvn-magic-file-name-handler
                (and (eq inhibit-file-name-operation operation)
                     inhibit-file-name-handlers)))
         (inhibit-file-name-operation operation))
    (apply operation args)))

(defmacro fsvn-magic-current-name (func)
  `(cond
    ((not fsvn-browse-repos-p)
     (,func))
    (t
     (fsvn-magic-create-remote-name (,func)))))

(defun fsvn-magic-create-name (urlrev)
  (cond
   ((fsvn-url-local-p urlrev)
    urlrev)
   ((fsvn-url-repository-p urlrev)
    (fsvn-magic-create-remote-name urlrev))
   ((fsvn-magic-file-name-absolute-p urlrev)
    urlrev)
   (t
    nil)))

(defmacro fsvn-magic-each-directory-entry (directory ls-var &rest form)
  (declare (indent 2))
  `(let ((DIR (fsvn-magic-parse-file-name ,directory))
         LIST RET TMP)
     (when (fsvn-magic-file-directory-p ,directory)
       (setq LIST (fsvn-magic-get-ls DIR))
       (mapc
        (lambda (,ls-var)
          (when (setq TMP (progn ,@form))
            (setq RET (cons TMP RET))))
        LIST)
       (nreverse RET))))

(defun fsvn-magic-create-remote-name (repos-urlrev)
  ;; FIXME dirty code
  (let (scheme remain segment-list rev url)
    (setq rev (fsvn-urlrev-revision repos-urlrev))
    (setq url (fsvn-urlrev-url repos-urlrev))
    (setq segment-list (split-string url ":"))
    (setq scheme (car segment-list))
    (setq segment-list (split-string (mapconcat 'identity (cdr segment-list) ":") "/"))
    ;;todo what is this cond.
    (while (and segment-list (string= (car segment-list) ""))
      (setq segment-list (cdr segment-list)))
    (setq remain (fsvn-magic-create-remote-absolute-path scheme segment-list))
    (concat "/fsvn" "@" (fsvn-get-revision-string rev) "/" scheme "/" remain)))

(defun fsvn-magic-create-remote-absolute-path (scheme segments)
  ;;FIXME dirty
  (when (and scheme segments)
    (when (and (string= scheme "file") (memq system-type '(windows-nt)))
      (setcar segments (replace-regexp-in-string "^\\([a-zA-Z]\\):$" "\\1|" (car segments)))))
  (mapconcat 'identity segments "/"))

(defun fsvn-magic-parse-file-as-writable (file)
  (let ((urlrev (fsvn-magic-parse-file-name file)))
    (fsvn-urlrev-url urlrev)))

(defun fsvn-magic-parse-file-name (file)
  (cond
   ((string-match fsvn-magic-file-name-regexp file)
    ;;FIXME dirty
    (let* ((rev (match-string 1 file))
           (ret (replace-match "" nil nil file))
           absolute-path segment-list)
      (setq segment-list (split-string ret "/"))
      (setq absolute-path (fsvn-magic-parse-file-name-absolute-path 
                           (car segment-list) (cdr segment-list)))
      (fsvn-url-urlrev (concat 
                        (fsvn-svn-url-scheme-segment (car segment-list))
                        absolute-path) rev)))
   ((and (not (file-name-absolute-p file))
         default-directory
         (string-match fsvn-magic-file-name-regexp default-directory))
    (fsvn-magic-parse-file-name (fsvn-magic-expand-file-name file default-directory)))
   (t
    file)))

(defun fsvn-magic-parse-file-name-revision (file)
  (when (string-match fsvn-magic-file-name-regexp file)
    (match-string 1 file)))

(defun fsvn-magic-parse-file-name-absolute-path (scheme segments)
  ;;FIXME dirty
  (when (and scheme segments)
    (when (and (string= scheme "file") (memq system-type '(windows-nt)))
      (setcar segments (replace-regexp-in-string "^\\([a-zA-Z]\\)|$" "\\1:" (car segments)))))
  (mapconcat 'identity segments "/"))

(defun fsvn-magic-expand-file-name (file &optional directory)
  (if (and (file-name-absolute-p file)
           (not (fsvn-magic-file-name-absolute-p file)))
      (fsvn-magic-file-call-underlying 'expand-file-name file directory)
    (let ((lst
           (if (fsvn-magic-file-name-absolute-p file)
               (fsvn-magic-split-file-name file)
             (nconc
              (fsvn-magic-split-file-name (directory-file-name (or directory default-directory)))
              (fsvn-magic-split-file-name file)))))
      (fsvn-url-concat-split-path lst))))

(defun fsvn-magic-file-directory-p (filename)
  (let ((entry (fsvn-magic-get-list/info-entry (fsvn-magic-parse-file-name filename))))
    (eq (fsvn-magic-xml-list/info-entry.kind entry) 'dir)))

(defun fsvn-magic-file-regular-p (filename)
  (let ((entry (fsvn-magic-get-list/info-entry (fsvn-magic-parse-file-name filename))))
    (eq (fsvn-magic-xml-list/info-entry.kind entry) 'file)))

(defun fsvn-magic-file-exists-p (filename)
  (let ((entry (fsvn-magic-get-list/info-entry (fsvn-magic-parse-file-name filename))))
    (not (null entry))))

(defun fsvn-magic-file-readable-p (filename)
  (fsvn-magic-file-exists-p filename))

(defun fsvn-magic-file-ownership-preserved-p (file)
  ;; only check exists or not.
  ;; When commited ownership will be always changed.
  (not (fsvn-magic-file-exists-p file)))

(defun fsvn-magic-file-accessible-directory-p (filename)
  (fsvn-magic-file-directory-p filename))

(defun fsvn-magic-verify-visited-file-modtime (buf)
  (with-current-buffer buf
    (let* ((urlrev (fsvn-magic-parse-file-name buffer-file-name))
           (rev (fsvn-urlrev-revision urlrev))
           ls)
      (cond
       ((null buffer-file-name))
       ((not (string= rev "HEAD")))
       ((not (consp (visited-file-modtime))))
       ((setq ls (fsvn-magic-get-ls-entry urlrev))
        (time-less-p (visited-file-modtime) (fsvn-xml-lists->list->entry=>commit=>date$ ls)))
       (t
        nil)))))

(defun fsvn-magic-file-modes (filename)
  (let* ((file (fsvn-magic-parse-file-name filename))
         (entry (fsvn-magic-get-list/info-entry file))
         (bit 0)
         exec)
    (when entry
      (setq exec (fsvn-get-boolean-prop-value file "svn:executable"))
      (unless exec
        (setq exec (eq (fsvn-magic-xml-list/info-entry.kind entry) 'dir)))
      (when exec
        (setq bit (+ bit 1)))
      (setq bit (+ bit 6))
      ;;todo group and other bit must be weaker than owner
      (logior (lsh bit 6) (logxor (string-to-number "700" 8) (default-file-modes))))))

(defun fsvn-magic-set-file-times (filename &optional time)
  ;; do nothing
  )

(defun fsvn-magic-set-file-modes (filename mode)
  ;; do nothing
  )

(defun fsvn-magic-set-visited-file-modtime (&optional time-list)
  ;; do nothing
  )

(defun fsvn-magic-file-executable-p (filename)
  (fsvn-get-boolean-prop-value (fsvn-magic-parse-file-name filename) "svn:executable"))

(defun fsvn-magic-file-attributes (filename &optional id-format)
  (let* ((file (fsvn-magic-parse-file-name filename))
         (info (fsvn-magic-get-info-entry file)))
    (when info
      (list
       ;; 0. t for directory, string (name linked to) for symbolic link, or nil.
       (eq (fsvn-xml-info->entry.kind info) 'dir)
       ;; 1. Number of links to file.
       (length (fsvn-magic-directory-files filename))
       ;; 2. GID
       (if (or (null id-format) (eq id-format 'integer))
           0
         (fsvn-xml-info->entry=>commit=>author$ info))
       ;; 3. UID
       (if (or (null id-format) (eq id-format 'integer))
           0
         (fsvn-xml-info->entry=>commit=>author$ info))
       ;; 4. Last access time
       (current-time)
       ;; 5. Last modification time
       (fsvn-xml-info->entry=>commit=>date$ info)
       ;; 6. Last status change time
       (fsvn-xml-info->entry=>commit=>date$ info)
       ;; 7. Size in bytes.
       (if (eq (fsvn-xml-info->entry.kind info) 'dir)
           0
         (let ((ls (fsvn-magic-get-ls-entry file)))
           (fsvn-safe-xml-lists->list->entry=>size$ ls)))
       ;; todo 8. File modes FIXME to String
       (default-file-modes)
       ;; todo 9. t iff file's gid would change if file were deleted and recreated.
       nil
       ;; 10. inode number. **** Don't use this. Not reliable.****
       (fsvn-magic-info-pseudo-inode-number info)
       ;; 11. Device number. **** Don't use this. Not reliable.****
       (fsvn-magic-info-pseudo-device-number info)))))

(defun fsvn-magic-file-symlink-p (filename)
  (let* ((file (fsvn-magic-parse-file-name filename))
         (parent (file-name-directory (directory-file-name file)))
         (name (fsvn-urlrev-filename file))
         (pinfo (fsvn-magic-get-info-entry parent))
         value values)
    (when (setq value (fsvn-get-propget parent "svn:externals"))
      (catch 'found
        (mapc
         (lambda (item)
           (when (string= (cdr item) name)
             (throw 'found (car item))))
         (fsvn-svn:externals-parse-value pinfo value))
        nil))))

(defun fsvn-magic-file-truename (filename)
  filename)

(defun fsvn-magic-directory-file-name (directory)
  (let* ((urlrev (fsvn-magic-parse-file-name directory))
         (roots fsvn-magic-repository-roots))
    (cond
     ((member (fsvn-urlrev-url urlrev) roots)
      ;; when guessed root directory..
      directory)
     ((string-match "/$" directory)
      (substring directory 0 -1))
     (t
      directory))))

(defun fsvn-magic-file-name-directory (filename)
  (let* ((urlrev (fsvn-magic-parse-file-name filename))
         (roots fsvn-magic-repository-roots))
    (cond
     ((member (concat (fsvn-urlrev-url urlrev) "/") roots)
      ;; when guessed root directory..
      filename)
     ((string-match "/$" filename)
      filename)
     (t
      (fsvn-magic-create-name (fsvn-urlrev-dirname urlrev))))))

(defun fsvn-magic-directory-files (directory &optional full match nosort)
  "NOSORT no effect (always sort)."
  (fsvn-magic-each-directory-entry directory entry
    (let ((name (fsvn-xml-lists->list->entry=>name$ entry)))
      (when (or (null match)
                (string-match match name))
        (if full
            (fsvn-magic-expand-file-name name directory)
          name)))))

(defun fsvn-magic-directory-files-and-attributes (directory &optional full match nosort id-format)
  (let (fullname)
    (mapcar
     (lambda (file)
       (setq fullname (fsvn-magic-expand-file-name file directory))
       (cons
        (if full fullname file)
        (fsvn-magic-file-attributes fullname id-format)))
     (fsvn-magic-directory-files directory nil match nosort))))

(defun fsvn-magic-write-region (start end filename &optional append visit lockname mustbenew)
  (let ((url (fsvn-magic-parse-file-as-writable filename))
        (tmpfile (fsvn-make-temp-file)))
    (if (fsvn-magic-file-exists-p filename)
        (progn
          (unless (fsvn-save-file url tmpfile t)
            (signal 'fsvn-command-error (list "Error while saving file. " url)))
          (write-region start end tmpfile append 'no-msg)
          (fsvn-asap-modify-url-from-file tmpfile url))
      (write-region start end tmpfile append 'no-msg nil mustbenew)
      (fsvn-asap-add-file tmpfile (fsvn-url-dirname url) (fsvn-file-name-nondirectory url)))
    ;; overwrite this
    (setq last-coding-system-used buffer-file-coding-system)
    (when (or (memq visit '(t nil)) (stringp visit))
      (if append
          (message "Added to %s" filename)
        (message "Wrote %s" filename))))
  nil)

(defun fsvn-magic-delete-directory (directory)
  (let* ((dir (fsvn-magic-parse-file-name directory))
         (entry (fsvn-magic-get-list/info-entry dir)))
    (unless entry
      (signal 'file-error (list "No such directory." directory)))
    (unless (eq (fsvn-magic-xml-list/info-entry.kind entry) 'dir)
      (signal 'file-error (list "Removing svn directory." directory)))
    (unless (= (length (fsvn-magic-get-ls dir)) 0)
      (signal 'file-error (list "Removing svn directory" "directory not empty" dir)))
    (fsvn-call-command-discard "delete"
                               "--message" (fsvn-config-magic-remote-commit-message dir)
                               dir)))

(defun fsvn-magic-delete-file (filename)
  (let* ((file (fsvn-magic-parse-file-name filename))
         (entry (fsvn-magic-get-list/info-entry file)))
    (unless entry
      (signal 'file-error (list "No such file." file)))
    (unless (eq (fsvn-magic-xml-list/info-entry.kind entry) 'file)
      (signal 'file-error (list "Removing svn file." file)))
    (let ((file (fsvn-magic-parse-file-name filename)))
      (fsvn-call-command-discard "delete"
                                 "--message" (fsvn-config-magic-remote-commit-message file)
                                 file))))

;; FIXME error occur move and copy
;;    svn: Moves between the working copy and the repository are not supported
;;    and between a repository and other repository

(defun fsvn-magic-copy-file (file newname &optional ok-if-already-exists keep-time preserve-uid-gid)
  "
remote -> remote (same repository): svn copy
remote -> remote (different repository) : svn export -> svn add -> svn commit
remote -> local (same wc) : svn copy
remote -> local (different wc or any local path) : svn export
local (wc) -> remote (same repository) : svn copy
local (non-wc) -> remote : svn add -> svn commit
"
  (let ((from (fsvn-magic-parse-file-name file))
        (to (fsvn-magic-parse-file-name newname))
        from-entry)
    (if (fsvn-url-repository-p from)
        (cond
         ((fsvn-url-repository-p to)
          (setq to (fsvn-urlrev-url to))
          (fsvn-call-command-discard "copy" from to
                                     "--message" (fsvn-config-magic-remote-commit-message to)))
         ((fsvn-file-versioned-directory-p to)
          (fsvn-call-command-discard "copy" from to))
         (t
          (when (and (not ok-if-already-exists)
                     (file-exists-p to))
            (signal 'file-already-exists (list "File already exists." to)))
          (when keep-time
            (unless (setq from-entry (fsvn-magic-get-list/info-entry from))
              (signal 'file-error (list "Copying svn file." from to))))
          (unless (fsvn-save-file from to 'no-msg)
            (signal 'file-error (list "Copying svn file." from to)))
          (when from-entry
            (set-file-times to (fsvn-magic-xml-list/info-entry=>commit=date from-entry)))
          ))
      (cond
       ((not (fsvn-url-repository-p to))
        (signal 'file-error (list "Copy error.")))
       ((and ok-if-already-exists (file-exists-p newname))
        (setq to (fsvn-urlrev-url to))
        (fsvn-asap-modify-url-from-file from to))
       (t
        (setq to (fsvn-urlrev-url to))
        (fsvn-asap-add-file file to))
       ))))

(defun fsvn-magic-rename-file (file newname &optional ok-if-already-exists)
  (let ((from (fsvn-magic-parse-file-name file))
        (to (fsvn-magic-parse-file-name newname))
        from-info)
    (if (fsvn-url-repository-p from)
        (progn
          (setq from (fsvn-urlrev-url from))
          (cond
           ((fsvn-url-repository-p to)
            (setq to (fsvn-urlrev-url to))
            (fsvn-call-command-discard "move" from to
                                       "--message" (fsvn-config-magic-remote-commit-message to)))
           ((fsvn-file-versioned-directory-p to)
            (fsvn-call-command-discard "move" from to))
           (t
            (when (and (not ok-if-already-exists)
                       (file-exists-p to))
              (signal 'file-already-exists (list "File already exists." to)))
            (unless (fsvn-save-file from to 'no-msg)
              (signal 'file-error (list "Moving svn file." from to)))
            (fsvn-asap-delete-url from))))
      (cond
       ((not (fsvn-url-repository-p to))
        (signal 'file-error (list "Move error.")))
       ((fsvn-file-versioned-directory-p from)
        (fsvn-call-command-discard "move" file to
                                   "--message" (fsvn-config-magic-remote-commit-message to)))
       (t
        (fsvn-asap-add-file file (fsvn-url-dirname to))
        (delete-file file))))))

(defun fsvn-magic-load (file &optional noerror nomessage nosuffix must-suffix)
  (let ((tmpfile (fsvn-magic-file-local-copy file)))
    (load tmpfile noerror nomessage nosuffix must-suffix)))

(defun fsvn-magic-access-file (filename string)
  (let* ((urlrev (fsvn-magic-parse-file-name filename))
         (entry (fsvn-magic-get-list/info-entry urlrev)))
    (cond
     ((null entry)
      (error "%s: %s" string filename))
     (t
      nil))))

(defun fsvn-magic-file-writable-p (filename)
  (let ((urlrev (fsvn-magic-parse-file-name filename)))
    (string= (fsvn-urlrev-revision urlrev) "HEAD")))

(defun fsvn-magic-make-directory (dir &optional parents)
  (let ((url (fsvn-magic-parse-file-as-writable dir)))
    (fsvn-call-command-discard "mkdir"
                               (when parents "--parents")
                               "--message" (fsvn-config-magic-remote-commit-message url)
                               url)))

(defun fsvn-magic-make-directory-internal (dir)
  (let ((url (fsvn-magic-parse-file-as-writable dir)))
    (fsvn-call-command-discard "mkdir"
                               "--message" (fsvn-config-magic-remote-commit-message url)
                               url)))

(defun fsvn-magic-file-newer-than-file-p (file1 file2)
  (let ((attr1 (file-attributes file1))
        (attr2 (file-attributes file2)))
    (cond
     ((and attr1 attr2)
      (time-less-p (nth 5 attr2) (nth 5 attr1)))
     (attr1 t)
     (t nil))))

(defun fsvn-magic-file-name-absolute-p (file)
  (eq (string-match fsvn-magic-file-name-regexp file) 0))

(defun fsvn-magic-file-name-all-completions (file directory)
  (let ((regexp (concat "^" (regexp-quote file))))
    (fsvn-magic-each-directory-entry directory entry
      (let ((name (fsvn-xml-lists->list->entry=>name$ entry)))
        (cond
         ((not (string-match regexp name)))
         ((eq (fsvn-xml-lists->list->entry.kind entry) 'dir)
          (concat name "/"))
         (t
          name))))))

(defun fsvn-magic-file-name-completion (file directory &optional predicate)
  (let ((regexp (concat "^" (regexp-quote file)))
        completions)
    (unless predicate
      (setq predicate (lambda (x) t)))
    (fsvn-magic-each-directory-entry directory entry
      (let ((name (fsvn-xml-lists->list->entry=>name$ entry)))
        (cond
         ((not (string-match regexp name)))
         ((not (funcall predicate (fsvn-expand-file name directory))))
         ((eq (fsvn-xml-lists->list->entry.kind entry) 'dir)
          (setq completions (cons (concat name "/") completions)))
         (t
          (setq completions (cons name completions))))))
    (try-completion file completions)))

(defun fsvn-magic-file-remote-p (file &optional identification connected)
  t)

(defun fsvn-magic-get-file-buffer (filename)
  (fsvn-magic-file-call-underlying 'get-file-buffer filename))

(defun fsvn-magic-file-name-sans-versions (name &optional keep-backup-version)
  ;; do nothing.
  name)

(defun fsvn-magic-file-local-copy (file)
  (let* ((urlrev (fsvn-magic-parse-file-name file))
         (dir (fsvn-temp-directory))
         (filename (fsvn-urlrev-filename file))
         (name (file-name-sans-extension filename))
         (ext (file-name-extension filename))
         (i 1)
         tmp)
    (setq tmp (fsvn-expand-file (concat name "." ext) dir))
    (while (file-exists-p tmp)
      (setq tmp (fsvn-expand-file (format "%s(%d).%s" name i ext) dir))
      (setq i (1+ i)))
    (unless (fsvn-save-file urlrev tmp 'no-msg)
      (error "Error while copying"))
    tmp))

(defun fsvn-magic-find-file-noselect (filename &optional nowarn rawfile wildcards)
  (let* ((urlrev (fsvn-magic-parse-file-name filename))
         (buffer (fsvn-get-cat-buffer urlrev)))
    ;;todo other args
    (if buffer
        (with-current-buffer buffer
          (setq buffer-file-name filename)
          (current-buffer))
      (setq buffer (create-file-buffer filename))
      (with-current-buffer buffer
        (setq buffer-file-name filename))
      buffer)))

(defun fsvn-magic-find-backup-file-name (fn)
  (fsvn-expand-file (md5 fn) (fsvn-backup-file-directory)))

(defun fsvn-magic-make-auto-save-file-name ()
  (unless buffer-file-name
    (error "`make-auto-save-file-name' specification changed"))
  (fsvn-expand-file (md5 buffer-file-name) (fsvn-auto-save-file-directory)))

(defun fsvn-magic-diff-latest-backup-file (fn)
  nil)

(defun fsvn-magic-unhandled-file-name-directory (filename)
  (fsvn-temp-directory))

(defun fsvn-magic-info-pseudo-inode-number (info)
  (fsvn-magic-pseudo-hash-number
   (fsvn-url-urlrev
    (fsvn-xml-info->entry.path info)
    (fsvn-xml-info->entry.revision info))))

(defun fsvn-magic-info-pseudo-device-number (info)
  (fsvn-magic-pseudo-hash-number (fsvn-xml-info->entry=>repository=>root$ info)))

(defun fsvn-magic-split-file-name (file)
  (split-string file "/"))

;; FIXME
(defun fsvn-magic-pseudo-hash-number (ascii-string)
  (let* ((list (string-to-list ascii-string))
         (tmp 0))
    (mapc
     (lambda (ascii)
       (setq tmp (lsh (logxor tmp ascii) (% ascii 28))))
     list)
    tmp))

(defun fsvn-magic-clear-cache-if-toplevel ()
  (when (= (recursion-depth) 0)
    (setq fsvn-magic-cache nil)))

;;TODO hit -> get
(defun fsvn-magic-hit-cache (key url)
  (cdr (fsvn-string-assoc (fsvn-urlrev-directory-file-name url) (cdr (assq key fsvn-magic-cache)))))

(defun fsvn-magic-push-cache (key url value)
  (let (cache cache-value)
    (setq url (fsvn-urlrev-directory-file-name url))
    (unless (setq cache (assq key fsvn-magic-cache))
      (setq cache (cons key nil))
      (setq fsvn-magic-cache (cons cache fsvn-magic-cache)))
    (unless (setq cache-value (fsvn-string-assoc url (cdr cache)))
      (setq cache-value (cons url nil))
      (setcdr cache (cons cache-value (cdr cache))))
    (setcdr cache-value value)))

(defmacro fsvn-magic-use-cache-form (function internal-function url)
  `(let (CACHE)
     (cond
      (fsvn-magic-disable-cache
       (,function ,url))
      ((setq CACHE (fsvn-magic-hit-cache ',function ,url))
       CACHE)
      (t
       (setq CACHE (,internal-function ,url))
       (fsvn-magic-push-cache ',function ,url CACHE)
       CACHE))))

(defun fsvn-magic-get-ls (url)
  (fsvn-magic-use-cache-form fsvn-magic-get-ls fsvn-get-ls url))

(defun fsvn-magic-get-ls-entry (url)
  "Do not use this function if URL certain file."
  (let ((entries (fsvn-magic-get-ls (fsvn-urlrev-dirname url))))
    (fsvn-find-first
     (lambda (key item)
       (string= (fsvn-xml-lists->list->entry=>name$ item) key))
     (fsvn-urlrev-filename url)
     entries)))

(defun fsvn-magic-get-info-entry (url)
  (let ((info (fsvn-magic-use-cache-form fsvn-magic-get-info-entry fsvn-get-info-entry url)))
    ;;FIXME patch work
    (let ((root (fsvn-xml-info->entry=>repository=>root$ info)))
      (when root
        (unless (member root fsvn-magic-repository-roots)
          (setq fsvn-magic-repository-roots
                (cons root fsvn-magic-repository-roots)))))
    info))

(defun fsvn-magic-get-list/info-entry (url)
  "`fsvn-magic-get-ls-entry' faster than `fsvn-magic-get-info-entry' because of cache hit rate is high.
But `fsvn-magic-get-ls-entry' is not perfect for under repository root directories. "
  (or (fsvn-magic-get-ls-entry url)
      (fsvn-magic-get-info-entry url)))

(defmacro fsvn-magic-xml-list/info-switch (list-func info-func node)
  `(cond
    ((null ,node)
     nil)
    ((fsvn-magic-list/info-entry-list-p node)
     (,list-func node))
    (t
     (,info-func node))))

(defun fsvn-magic-xml-list/info-entry.kind (node)
  (fsvn-magic-xml-list/info-switch
   fsvn-xml-lists->list->entry.kind
   fsvn-xml-info->entry.kind
   node))

(defun fsvn-magic-xml-list/info-entry=>commit=date (node)
  (fsvn-magic-xml-list/info-switch
   fsvn-xml-lists->list->entry=>commit=>date$
   fsvn-xml-info->entry=>commit=>date$
   node))

(defun fsvn-magic-xml-list/info-entry=name (node)
  (fsvn-magic-xml-list/info-switch
   fsvn-xml-lists->list->entry=>name$
   (lambda (x)
     (fsvn-url-filename (fsvn-xml-info->entry=>url$ x)))
   node))

(defun fsvn-magic-xml-list/info-entry=>commit.revision (node)
  (fsvn-magic-xml-list/info-switch
   fsvn-xml-lists->list->entry=>commit.revision
   fsvn-xml-info->entry=>commit.revision
   node))

(defun fsvn-magic-xml-list/info-entry=>commit.author (node)
  (fsvn-magic-xml-list/info-switch
   fsvn-xml-lists->list->entry=>commit=>author$
   fsvn-xml-info->entry=>commit=>author$
   node))

(defun fsvn-magic-xml-lists->list/info-entry=>commit=author (node)
  (fsvn-magic-xml-list/info-switch
   fsvn-xml-lists->list->entry=>commit=>author$
   fsvn-xml-info->entry=>commit=>author$
   node))

(defun fsvn-magic-list/info-entry-list-p (node)
  (and node (fsvn-xml-lists->list->entry=>name$ node)))

(defun fsvn-magic-list/info-entry-info-p (node)
  (and node (not (fsvn-magic-list/info-entry-list-p node))))



;; suppress vc-find-file-hook because this makes slow
(defadvice after-find-file 
  (around fsvn-after-find-file () disable)
  (let ((find-file-hook find-file-hook))
    (when (and buffer-file-name (fsvn-magic-file-name-absolute-p buffer-file-name))
      (setq find-file-hook nil))
    ad-do-it))



(provide 'fsvn-magic)

;;; fsvn-magic.el ends here
