;;; fsvn-data.el --- Manipulate fsvn internal data structure utilities.


;;; History:
;; 

;;; Commentary:
;; 

;;; Code:



(defvar user-login-name)



(defun fsvn-create-local-file-entry (file)
  (let ((attr (file-attributes file)))
    (list
     (cons 'path (file-name-directory file))
     (cons 'directory (eq (car attr) t))
     (cons 'symlink (and (stringp (car attr)) (car attr)))
     (cons 'uid (nth 2 attr))
     (cons 'size (nth 7 attr))
     (cons 'modified (nth 5 attr))
     (cons 'name (fsvn-file-name-nondirectory file))
     (cons 'fullname (fsvn-expand-file file))
     )))

(defun fsvn-local-file.path (file)
  (cdr (assq 'path file)))

(defun fsvn-local-file.directory-p (file)
  (cdr (assq 'directory file)))

(defun fsvn-local-file.symlink-p (file)
  (cdr (assq 'symlink file)))

(defun fsvn-local-file.author (file)
  user-login-name)

(defun fsvn-local-file.size (file)
  (cdr (assq 'size file)))

(defun fsvn-local-file.modified-time (file)
  (cdr (assq 'modified file)))

(defun fsvn-local-file.name (file)
  (cdr (assq 'name file)))

(defun fsvn-local-file.fullname (file)
  (cdr (assq 'fullname file)))



;; status utility

(defun fsvn-status-stronger-than (all x y)
  (let ((xl (memq x all))
        (yl (memq y all)))
    (cond
     ((null xl)
      nil)
     ((null yl)
      t)
     (t
      (memq y (cdr xl))))))

(defun fsvn-status-dir-status-stronger-than-p (x y)
  "Return non-nil if X is stronger than Y."
  (let* ((all '(?C ?M)))
    (fsvn-status-stronger-than all x y)))

(defun fsvn-status-file-status-stronger-than-p (x y)
  "Return non-nil if X is stronger than Y."
  (let* ((all '(?C ?A ?D ?I ?M)))
    (fsvn-status-stronger-than all x y)))

(defun fsvn-status-file-status-to-dir-status (col1 col2 col7)
  (cond
   ((memq ?C (list col1 col2 col7)) ?C)
   ((memq col1 '(?M ?A ?D)) ?M)
   ((eq col2 ?M) ?M)
   ;;FIXME what mean replaced?
   (t nil)))

(defun fsvn-status-dir-status-stronger (x y)
  (if (fsvn-status-dir-status-stronger-than-p x y) x y))

(defun fsvn-status-file-status-stronger (x y)
  (if (fsvn-status-file-status-stronger-than-p x y) x y))

(defun fsvn-status-string-to-dir-status (status-string)
  (let ((list (string-to-list status-string)))
    (fsvn-status-file-status-to-dir-status (nth 0 list) (nth 1 list) (nth 6 list))))

(defun fsvn-status-get-status (entry)
  (let (col1 col2 col3 col4 col5 col6 col7)
    (setq col1 (fsvn-status-get-status-1 entry))
    (setq col2 (fsvn-status-get-status-2 entry))
    (setq col3 (fsvn-status-get-status-3 entry))
    (setq col4 (fsvn-status-get-status-4 entry))
    (setq col5 (fsvn-status-get-status-5 entry))
    (setq col6 (fsvn-status-get-status-6 entry))
    (setq col7 (fsvn-status-get-status-7 entry))
    (concat (list col1 col2 col3 col4 col5 col6 col7))))

(defun fsvn-status-get-status-1 (entry)
  "status of entry text."
  (let ((val (fsvn-xml-status->target->entry=>wc-status.item entry)))
    (cond
     ((eq 'added val) ?A)
     ((eq 'conflicted val) ?C)
     ((eq 'deleted val) ?D)
     ((eq 'ignored val) ?I)
     ((eq 'modified val) ?M)
     ((eq 'replaced val) ?R)
     ((eq 'external val) ?X)
     ((eq 'unversioned val) ??)
     ((eq 'missing val) ?!)
     ((eq 'incomplete val) ?!)
     ((eq 'obstructed val) ?~)
     (t ?.))))

(defun fsvn-status-get-status-2 (entry)
  "status of entry property."
  (let ((val (fsvn-xml-status->target->entry=>wc-status.props entry)))
    (cond
     ((eq 'modified val) ?M)
     ((eq 'conflicted val) ?C)
     (t ?.))))

(defun fsvn-status-get-status-3 (entry)
  (if (fsvn-xml-status->target->entry=>wc-status.wc-locked entry)
      ?L
    ?.))

(defun fsvn-status-get-status-4 (entry)
  (if (eq (fsvn-xml-status->target->entry=>wc-status.copied entry) t)
      ?+
    ?.))

(defun fsvn-status-get-status-5 (entry)
  (cond
   ((fsvn-xml-status->target->entry=>wc-status.switched entry)
    ?S)
   ((fsvn-xml-status->target->entry=>wc-status.file-external entry)
    ?X)
   (t
    ?.)))

(defun fsvn-status-get-status-6 (entry)
  (let ((repos (fsvn-xml-status->target->entry=>repos-status entry))
        (wc (fsvn-xml-status->target->entry=>wc-status entry)))
    (cond
     ((and repos wc)
      ;; when `status' with --show-updates
      (let ((repos-token (fsvn-xml-status->target=>entry->repos-status=>lock=>token$ entry))
            (wc-token (fsvn-xml-status->target->entry=>wc-status=>lock=>token$ entry)))
        (cond
         ((and repos-token wc-token)
          (cond
           ((string= repos-token wc-token)
            ;; locked in repository, lock toKen present
            ?K)
           (t
            ;; locked in repository, lock token present but sTolen
            ?T)))
         (repos-token
          ;; locked in repository, lock token in some Other working copy
          ?O)
         (wc-token
          ;; not locked in repository, lock token present but Broken
          ;; FIXME never into here, because not locked in repository, repos-status node is not returned.
          ?B)
         (t
          ?.))))
     (repos
      ?.)
     ((fsvn-xml-status->target->entry=>wc-status=>lock entry)
      ;; locked in repository, lock toKen present
      ?K)
     (t
      ?.))))

(defun fsvn-status-get-status-7 (entry)
  (if (eq (fsvn-xml-status->target->entry=>wc-status.tree-conflicted entry) t)
      ?C
    ?.))



;; misc

(defun fsvn-logs-create-path-chain (log-entries path)
  "Return list of cell like (revision . path).
This list sorted revision descending.
"
  (let ((current path)
        ret rev)
    ;;TODO space or something contains. find better solution
    (setq current (fsvn-url-decode-string current))
    (mapc
     (lambda (logentry)
       (setq rev (fsvn-xml-log->logentry.revision logentry))
       (mapc
        (lambda (path)
          (unless (string= (fsvn-xml-log->logentry->path.copyfrom-path path) "")
            (let ((logpath (fsvn-xml-log->logentry->paths->path$ path))
                  (copyfrom (fsvn-xml-log->logentry->path.copyfrom-path path)))
              (cond
               ((string= logpath current)
                (setq current copyfrom)
                (setq ret (cons (cons rev current) ret)))
               ((string-match (concat "^" (regexp-quote logpath) "/") current)
                (setq current (fsvn-expand-url (substring current (match-end 0)) copyfrom))
                (setq ret (cons (cons rev current) ret)))))))
        (fsvn-xml-log->logentry->paths logentry)))
     log-entries)
    (nreverse ret)))

(defun fsvn-logs-chain-find (log-entries rev path)
  (let ((chain (fsvn-logs-create-path-chain log-entries path))
        (prev path))
    (catch 'found
      (mapc
       (lambda (item)
         (let ((revision (car item))
               (name (cdr item)))
           (when (>= rev revision)
             (throw 'found prev))
           (setq prev name)))
       chain)
      (or prev path))))

(defun fsvn-logs-find-logentry (logs rev)
  (fsvn-find-first
   (lambda (key item)
     (= key (fsvn-xml-log->logentry.revision item)))
   rev logs))

(defun fsvn-logs-unique-merge (&rest entries)
  (let ((all (apply 'append entries))
        ret rev)
    (setq all (sort all
                    (lambda (x y)
                      (> (fsvn-xml-log->logentry.revision x)
                         (fsvn-xml-log->logentry.revision y)))))
    (mapc
     (lambda (x)
       (unless (equal rev (fsvn-xml-log->logentry.revision x))
         (setq rev (fsvn-xml-log->logentry.revision x))
         (setq ret (cons x ret))))
     all)
    (nreverse ret)))

(defun fsvn-logs-terminate-entries (entries)
  (when entries
    (let ((first (car (last entries)))
          (last (car entries)))
      (when (> (fsvn-xml-log->logentry.revision first)
               (fsvn-xml-log->logentry.revision last))
        (setq entries (nreverse entries))
        (setq first (car (last entries))
              last (car entries)))
      (cons first last))))

(defun fsvn-find-status-entry (status-entries file)
  (fsvn-find-first
   (lambda (key item)
     (string= key (fsvn-xml-status->target->entry.path item)))
   (directory-file-name file) status-entries))

(defun fsvn-find-logentry-path (path logentry)
  (fsvn-find-first
   (lambda (key path-entry)
     (string= key (fsvn-xml-log->logentry->paths->path$ path-entry)))
   path (fsvn-xml-log->logentry->paths logentry)))

(defun fsvn-revision-range-to-string (rev-range)
  "REV-RANGE is cons cell car is start revision, cdr is end revision."
  (format "%s:%s"
          (fsvn-get-revision-string (car rev-range))
          (fsvn-get-revision-string (cdr rev-range))))

(defun fsvn-set-filename-property (filename)
  (fsvn-string-put-property filename 'fsvn-filename t))

(defun fsvn-info-repos-path (info)
  (let ((root (fsvn-xml-info->entry=>repository=>root$ info))
        (url (fsvn-xml-info->entry=>url$ info))
        ret)
    (when (string-match (concat "^\\(" (regexp-quote root) "\\)") url)
      (setq ret (replace-match "" nil nil url 1))
      (when (string= ret "")
        (setq ret "/")))
    ret))

(defun fsvn-delete (elt seq)
  (delete elt (copy-sequence seq)))

(defun fsvn-take (count seq)
  (let ((i 0)
        ret)
    (while (< i count)
      (setq ret (cons (nth i seq) ret))
      (setq i (1+ i)))
    (nreverse ret)))

(defun fsvn-find-first (predicate elt list)
  "PREDICATE accept two args like (elt list-each-item). "
  (catch 'found
    (mapc
     (lambda (x)
       (when (funcall predicate elt x)
         (throw 'found x)))
     list)
    nil))

(defun fsvn-group-by-directory (urlrev-list)
  (let (ret dir cell)
    (mapc
     (lambda (urlrev)
       (setq dir (directory-file-name (fsvn-url-dirname urlrev)))
       (unless (setq cell (assoc dir ret))
         (setq cell (cons dir nil))
         (setq ret (cons cell ret)))
       (setcdr cell (cons (fsvn-urlrev-filename urlrev) (cdr cell))))
     urlrev-list)
    (nreverse ret)))

(defun fsvn-sole-major-mode (mode)
  (fsvn-find-first
   (lambda (key item)
     (eq (fsvn-buffer-major-mode item) mode))
   mode (buffer-list)))

(defcustom fsvn-common-property-list nil
  "List of variable symbol of completing applicant list."
  :group 'fsvn
  :type '(list symbol))

(defun fsvn-propname-completion-alist (file)
  (let (ret root)
    (mapc
     (lambda (var)
       (mapc
        (lambda (x)
          (setq ret (cons (cons x nil) ret)))
        (eval var)))
     (cons 'fsvn-const-property-list fsvn-common-property-list))
    (when (and (fsvn-url-local-p file)
               (setq root (fsvn-get-root file))
               (fsvn-config-tortoise-property-use root))
      (setq ret (append ret (fsvn-tortoise-tsvn:user*properties file))))
    ret))

(defun fsvn-wc-file-repository-path (file)
  (let ((info (fsvn-get-info-entry file))
        root url)
    (setq root (fsvn-xml-info->entry=>repository=>root$ info))
    (setq url (fsvn-xml-info->entry=>url$ info))
    (fsvn-repository-path root url)))

(defun fsvn-wc-file-repository-url (file)
  (let ((info (fsvn-get-info-entry file)))
    (fsvn-xml-info->entry=>url$ info)))

(defun fsvn-repository-path (root urlrev)
  (let ((url (fsvn-urlrev-url urlrev)))
    (when (string-match (concat "^" (regexp-quote  root)) url)
      (replace-match "" nil nil url 0))))

(defun fsvn-find-parent-working-copy (directory)
  (let ((prev directory)
        (curr directory))
    (while (and (setq curr (fsvn-file-name-directory curr))
                (not (string= curr prev))
                (not (fsvn-directory-versioned-p curr)))
      (setq prev curr))
    (when (fsvn-directory-versioned-p curr)
      curr)))

(defun fsvn-find-most-top-buffer-directory (directory)
  (defvar fsvn-browse-subdir-alist)
  (let ((top directory))
    (fsvn-each-browse-buffer
     (mapc
      (lambda (subdir)
        (when (fsvn-url-contains-p (car subdir) top)
          (setq top (car subdir))))
      fsvn-browse-subdir-alist))
    top))

(defun fsvn-gather-root ()
  (defvar fsvn-browse-subdir-alist)
  (defvar fsvn-repository-alist)
  (let (ret)
    (setq ret (mapcar (lambda (x) (fsvn-url-as-directory (car x))) fsvn-repository-alist))
    (fsvn-each-browse-buffer
     (mapc
      (lambda (subdir)
        (let* ((info (fsvn-browse-subdir.info subdir))
               (root (fsvn-url-as-directory (fsvn-xml-info->entry=>repository=>root$ info))))
          (unless (member root ret)
            (setq ret (cons root ret)))))
      fsvn-browse-subdir-alist))
    ret))



(provide 'fsvn-data)

;;; fsvn-data.el ends here
