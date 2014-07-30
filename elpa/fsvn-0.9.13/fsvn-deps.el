;;; fsvn-deps.el --- Depend on subversion internal specification variables/functions.


;;; History:
;; 

;;; Commentary:
;; 

;;; Code:



(require 'dired)
(require 'fsvn-env)



(defvar system-type)
(defvar process-environment)



(defconst fsvn-svn-status-length 7)

;; FIXME space started filename. and fsvn-svn-status-length problem
(defconst fsvn-svn-status-versioned-regexp "^\\([^?][A-Z+!?~ ]\\{5,6\\}\\) \\([^ ].+\\)\n")
(defconst fsvn-svn-status-unversioned-regexp "^\\([?][ ]\\{5,6\\}\\) \\([^ ].+\\)\n")



(defvar fsvn-svn-common-coding-system 'utf-8
  "Coding system of default svn implementation.")

(defvar fsvn-prop-file-default-coding-system fsvn-svn-common-coding-system)

(defcustom fsvn-svn-command "svn"
  "Subversion command. Must be set before loading this file.
If are problems while executing this command, check `exec-path' or PATH environment variable.
Otherwise set absolute path.

After change this variable, will not take effect immediately. 
Please call `fsvn-initialize-loading' function.
"
  :group 'fsvn
  :type  '(choice
           string
           file))

(defcustom fsvn-svnadmin-command "svnadmin"
  "Subversion Administrator command. Must be set before loading this file.
If are problems while executing this command, check `exec-path' or PATH environment variable.
Otherwise set absolute path.

After change this variable, will not take effect immediately. 
Please call `fsvn-initialize-loading' function.
"
  :group 'fsvn
  :type  '(choice
           string
           file))

(defvar fsvn-svn-command-internal nil)
(defvar fsvn-svnadmin-command-internal nil)

;; svnsync is optional implements.
;; So won't prepare custom variable.
(defvar fsvn-svnsync-command-internal nil)

(defmacro fsvn-deps-process-environment (&rest form)
  `(let ((process-environment (copy-sequence process-environment)))
     (setenv "LC_MESSAGES" "C")
     ,@form))


;; access to subversion meta directory

;; http://svn.collab.net/repos/svn/trunk/subversion/libsvn_wc/adm_files.c
;;  more test and reverse engineering
(defun fsvn-meta--get-property (propname file)
  "Access to meta directory. (very fast)"
  (fsvn-meta--get-properties file propname))

(defun fsvn-meta--get-properties (file &optional propname)
  "Access to meta directory. (very fast)

\(fn FILE)"
  (let ((propfile
         (if (fsvn-file-exact-directory-p file)
             (fsvn-meta--prop-directory-file file)
           (fsvn-meta--prop-file file)))
        (prop-regex (format "^K \\([0-9]+\\)"))
        propname-length)
    (when propfile
      (with-temp-buffer
        ;; todo check source coding-system
        (let ((coding-system-for-read fsvn-svn-common-coding-system)
              alist)
          (insert-file-contents propfile)
          (goto-char (point-min))
          (catch 'done
            (while (re-search-forward prop-regex nil t)
              (let ((name-length (string-to-number (match-string 1)))
                    (bytes 0)
                    name val-length value start c)
                (forward-line 1)
                (unless (looking-at "^\\(.+\\)$")
                  (signal 'fsvn-parsing-error '("Unrecognized format.")))
                (setq name (match-string 1))
                (forward-line 1)
                (unless (looking-at "^V \\([0-9]+\\)$")
                  (signal 'fsvn-parsing-error '("Unrecognized format.")))
                (setq val-length (string-to-number (match-string 1)))
                (forward-line 1)
                (setq start (point))
                (fsvn-forward-bytes val-length)
                (setq value (fsvn-meta--prop-string-region start (point)))
                (when (and propname (string= propname name))
                  (throw 'done value))
                (setq alist (cons (cons name value) alist))))
            (unless propname
              (nreverse alist))))))))

(defun fsvn-meta--prop-string-region (start end)
  ;;FIXME
  (replace-regexp-in-string "\r" "" (buffer-substring start end)))

(defun fsvn-meta--dir-directory (dir)
  (fsvn-expand-file (fsvn-meta-dir-name) dir))

(defun fsvn-meta--file-directory (file)
  (let* ((dir (fsvn-file-name-directory2 file)))
    (fsvn-expand-file (fsvn-meta-dir-name) dir)))

(defun fsvn-meta--text-base-file-name (file)
  (let* ((admindir (fsvn-meta--file-directory file))
         (name (fsvn-file-name-nondirectory file))
         (metadir (fsvn-expand-file "text-base" admindir)))
    (concat (fsvn-expand-file name metadir) ".svn-base")))

(defun fsvn-meta--text-base-file (file)
  (let ((tmp (fsvn-meta--text-base-file-name file)))
    (when (file-exists-p tmp)
      tmp)))

(defun fsvn-meta--file-registered-p (file)
  (fsvn-meta--text-base-file file))

(defun fsvn-meta--prop-file (file)
  (let* ((admindir (fsvn-meta--file-directory file))
         (name (fsvn-file-name-nondirectory file))
         metadir tmp)
    (setq metadir (fsvn-expand-file "props" admindir))
    (setq tmp (concat (fsvn-expand-file name metadir) ".svn-work"))
    (if (file-exists-p tmp)
        tmp
      (setq metadir (fsvn-expand-file "prop-base" admindir))
      (setq tmp (concat (fsvn-expand-file name metadir) ".svn-base"))
      (when (file-exists-p tmp)
        tmp))))

(defun fsvn-meta--prop-directory-file (dir)
  (let* ((admindir (fsvn-meta--dir-directory (file-name-as-directory dir)))
         base wc)
    (cond
     ((file-exists-p (setq wc (fsvn-expand-file "dir-props" admindir)))
      wc)
     ((file-exists-p (setq base (fsvn-expand-file "dir-prop-base" admindir)))
      base))))

(defun fsvn-meta-dir-name ()
  "svn meta data directory."
  (if (and (eq system-type 'windows-nt)
           (getenv "SVN_ASP_DOT_NET_HACK"))
      "_svn" ".svn"))



(defun fsvn-deps-get-property (propname file)
  (if (version< fsvn-svn-version "1.7")
      (fsvn-meta--get-properties file propname)
    (fsvn-get-propget file propname)))

(defun fsvn-deps-file-registered-p (file)
  (if (version< fsvn-svn-version "1.7")
      (fsvn-meta--text-base-file file)
    (fsvn-get-info-entry file)))

(defun fsvn-deps-text-base-file (file)
  (if (version< fsvn-svn-version "1.7")
      (fsvn-meta--text-base-file file)
    (let* ((info (fsvn-get-info-entry file))
           (checksum (fsvn-xml-info->entry=>wc-info=>checksum$ info))
           (root (fsvn-xml-info->entry=>wc-info=>wcroot-abspath$ info)))
      (expand-file-name 
       (concat "pristine/" (substring checksum 0 2) "/" checksum ".svn-base")
       (expand-file-name (fsvn-meta-dir-name) root)))))



;; set bottom of fsvn.el
(defvar fsvn-svn-version nil)
(defvar fsvn-svn-subcommand-completion-alist nil)
(defvar fsvn-svn-subcommand-arguments-alist nil)
(defvar fsvn-svnadmin-subcommand-completion-alist nil)
(defvar fsvn-svnadmin-subcommand-arguments-alist nil)

(defconst fsvn-svn-subcommand-global-options
  '(
    "--username"
    "--password"
    "--no-auth-cache"
    "--non-interactive"
    "--config-dir"
    ))

(defun fsvn-set-command-information ()
  (unless (setq fsvn-svn-command-internal (executable-find fsvn-svn-command))
    (error "No executable \"%s\" in `exec-path'" fsvn-svn-command))
  (unless (setq fsvn-svnadmin-command-internal (executable-find fsvn-svnadmin-command))
    (error "No executable \"%s\" in `exec-path'" fsvn-svnadmin-command))
  (fsvn-set-version))

(defun fsvn-set-version ()
  (with-temp-buffer
    (fsvn-deps-process-environment
     ;;TODO 1.6.9 stderr "svn: warning: cannot set LC_CTYPE locale"
     ;; not depend on fsvn-call-process
     (call-process fsvn-svn-command-internal nil (list (current-buffer) nil) nil "--version" "--quiet")
     (let ((raw-version (car (fsvn-text-buffer-line-as-list)))
           version)
       (when (fboundp 'version<=)
         (when (string-match "^\\([0-9]+\\.[0-9]+\\.[0-9]+\\)" raw-version)
           (setq version (match-string 1 raw-version)))
         (setq fsvn-svn-version (or version raw-version))
         (when (version<= fsvn-svn-version  "1.4")
           (error "Svn command must be 1.4.x or later")))))))

(defun fsvn-build-subcommand (&optional force)
  (mapc
   (lambda (x)
     (let ((command (nth 0 x))
           (subcommand-var (nth 1 x))
           (arguments-var (nth 2 x))
           (prefix (nth 3 x))
           subcache argcache cache-dir)
       (setq cache-dir (fsvn-cache-command-directory))
       (unless (file-directory-p cache-dir)
         (make-directory cache-dir t))
       (setq subcache (fsvn-expand-file (concat prefix "-subcommand") cache-dir))
       (setq argcache (fsvn-expand-file (concat prefix "-arguments") cache-dir))
       (set subcommand-var (fsvn-lisp-load subcache))
       (set arguments-var (fsvn-lisp-load argcache))
       (when (or force (null (symbol-value subcommand-var)))
         (set subcommand-var (fsvn-subcommand-alist-build command))
         (set arguments-var
              (mapcar
               (lambda (subcommand)
                 (cons subcommand (fsvn-subcommand-argument-list command subcommand)))
               (fsvn-subcommand-formal-list (symbol-value subcommand-var))))
         (fsvn-lisp-save (symbol-value subcommand-var) subcache)
         (fsvn-lisp-save (symbol-value arguments-var) argcache))))
   (list
    (list fsvn-svn-command
          'fsvn-svn-subcommand-completion-alist
          'fsvn-svn-subcommand-arguments-alist
          (concat "svn-" fsvn-svn-version))
    (list fsvn-svnadmin-command
          'fsvn-svnadmin-subcommand-completion-alist
          'fsvn-svnadmin-subcommand-arguments-alist
          (concat "svnadmin-" fsvn-svn-version)))) ;; version is guessed as `svn'
  (setq fsvn-svnsync-command-internal
        (fsvn-svn-command-sibling-find "svnsync")))


(defun fsvn-subcommand-argument-list (command subcommand)
  "Parse SUBCOMMAND help."
  (with-temp-buffer
    (fsvn-deps-process-environment
     (call-process command nil t nil "help" subcommand))
    (goto-char (point-min))
    (let (ret)
      (while (re-search-forward "^ \\{2\\}\\(?:\\(--[^ ]+\\)\\|\\(-[^-]\\) +\\[\\(--[^ ]+\\)\\]\\) +\\([^ :]+\\)?" nil t)
        (let ((long (or (match-string 1) (match-string 3)))
              (short (match-string 2))
              (arg (match-string 4))
              args applicant)
          (when arg
            (save-excursion
              (let ((start (line-beginning-position))
                    (end
                     (if (re-search-forward "^[ ]\\{2\\}-" nil t)
                         (progn (forward-line 0) (point))
                       (point-max))))
                (save-restriction
                  (narrow-to-region start end)
                  (goto-char (point-min))
                  (while (re-search-forward "^ \\{3,\\}\\(?:\\(--[^: ]+\\)\\|\\(-[^-]\\) +(\\(--[^ ]+\\))\\)" nil t)
                    (let ((long (or (match-string 1) (match-string 3)))
                          (short (match-string 2)))
                      (setq args (cons (cons (cons long short) nil) args))))
                  (unless args
                    (when (re-search-forward "(" nil t)
                      (let (start end)
                        (backward-char 1)
                        (setq start (1+ (point)))
                        (forward-sexp)
                        (setq end (1- (point)))
                        (setq args (fsvn-subcommand-parse-command-args (buffer-substring start end))))))
                  ))))
          (setq args (nreverse args))
          (setq applicant
                (cond
                 (args args)
                 (arg t)))
          (setq ret (cons (cons (cons long short) applicant) ret))))
      (nreverse ret))))

(defun fsvn-subcommand-parse-command-args (string)
  (let (ret)
    (mapc
     (lambda (x)
       (when (string-match "'\\([^']+\\)'" x)
         (setq ret (cons (cons (cons (match-string 1 x) nil) nil) ret))))
     (split-string string "[, \n\t]" t))
    (nreverse ret)))

(defun fsvn-subcommand-alist-build (command)
  (with-temp-buffer
    (fsvn-deps-process-environment
     (call-process command nil t nil "help"))
    (goto-char (point-min))
    (let (ret)
      (when (re-search-forward "^Available subcommands:")
        (forward-line 1)
        ;; match to "proplist (plist, pl)" like string
        (while (looking-at "^[ \t]*\\([^ \t\n]+\\)\\(?:[ \t\n]*\\(?:(\\(.+\\))\\)\\)?")
          (let ((subcommand (match-string 1))
                (aliases (match-string 2)))
            (when aliases
              (mapc
               (lambda (alias)
                 (setq ret (cons (cons alias subcommand) ret)))
               (split-string aliases ", ")))
            (setq ret (cons (cons subcommand subcommand) ret)))
          (forward-line 1))
        (nreverse ret)))))

(defun fsvn-subcommand-formal-list (subcommand-alist)
  (let (ret)
    (mapc
     (lambda (x)
       (unless (member (cdr x) ret)
         (setq ret (cons (cdr x) ret))))
     subcommand-alist)
    (nreverse ret)))

(defun fsvn-subcommand-assoc-argument (argument list)
  (fsvn-find-first
   (lambda (key item)
     (or (string= (caar item) key)
         (string= (cdar item) key)))
   argument list))

(defun fsvn-subcommand-accepted-argument (alist subcommand arg)
  (let (cell)
    (when (setq cell (assoc subcommand alist))
      (fsvn-subcommand-assoc-argument arg (cdr cell)))))

(defun fsvn-subcommand-list-of (alist argument)
  "Return subcommand list that accept ARGUMENT. "
  (let (ret)
    (mapc
     (lambda (alist)
       (when (fsvn-subcommand-assoc-argument argument (cdr alist))
         (setq ret (cons (car alist) ret))))
     alist)
    (nreverse ret)))

(defun fsvn-svn-subcommand-accepted-argument (subcommand arg)
  (fsvn-subcommand-accepted-argument fsvn-svn-subcommand-arguments-alist subcommand arg))

(defun fsvn-svn-subcommand-list-of (argument)
  "Return subcommand list that accept ARGUMENT. "
  (fsvn-subcommand-list-of fsvn-svn-subcommand-arguments-alist argument))

(defun fsvn-svn-command-sibling-find (command)
  (let ((dir (fsvn-file-name-directory (executable-find fsvn-svn-command-internal))))
    (executable-find (fsvn-expand-file command dir))))


;; revision definition

(defconst fsvn-revision-string-list
  '(
    ("HEAD")
    ("BASE")
    ("COMMITTED")
    ("PREV")
    ))

(defconst fsvn-revision-date-regexp "{[0-9]\\{8\\}}")

(defconst fsvn-revision-regexp
  (let ((symbols (regexp-opt (mapcar 'car fsvn-revision-string-list))))
    (concat symbols "\\|" "[0-9]+" "\\|" fsvn-revision-date-regexp)))

(defconst fsvn-svn-url-scheme-list
  '("http" "svn" "svn+ssh" "https" "file"))

(defun fsvn-svn-url-scheme-segment (scheme)
  "Return SCHEME svn client can understand."
  (let ((ret (concat scheme "://")))
    (when (string= scheme "file")
      (setq ret (concat ret "/")))
    ret))

(defconst fsvn-svn-url-scheme-segment-list
  (mapcar
   (lambda (x)
     (fsvn-svn-url-scheme-segment x))
   fsvn-svn-url-scheme-list))

(defun fsvn-revision-number-p (string)
  (if (string-match "^[0-9]+$" string)
      (string-to-number string)
    nil))

(defun fsvn-revision-date-p (string)
  (if (string-match (concat "^" fsvn-revision-date-regexp "$") string)
      t
    nil))

(defun fsvn-revision-symbol-p (string)
  (assoc string fsvn-revision-string-list))

(defun fsvn-get-revision-string (rev)
  (cond
   ((null rev)
    "HEAD")
   ((stringp rev)
    (upcase rev))
   ((integerp rev)
    (number-to-string rev))
   (t
    (error "Not a supported type"))))



;; property definition

(defconst fsvn-property-list
  '(
    "svn:ignore"
    ("svn:keywords" unable-directory)
    ("svn:executable"  unable-directory)
    ("svn:eol-style" unable-directory)
    ("svn:mime-type" unable-directory)
    "svn:externals"
    ("svn:needs-lock" unable-directory)
    "svn:mergeinfo"
    ))

(defconst fsvn-revprop-list
  '(
    "svn:log"
    "svn:date"
    "svn:author"
    ))

;; see below
;; http://tortoisesvn.tigris.org/svn/tortoisesvn/trunk/src/TortoiseProc/EditPropertyValueDlg.cpp
(defconst fsvn-const-property-list
  (fsvn-mapitem
   (lambda (propname)
     (cond
      ((stringp propname)
       propname)
      ((consp propname)
       (cond
        ((stringp (car propname))
         (car propname))))))
   fsvn-property-list))

(defconst fsvn-nondirectory-property-list
  (fsvn-mapitem
   (lambda (propname)
     (cond
      ((and (consp propname)
            (memq 'unable-directory propname))
       (car propname))))
   fsvn-property-list))

(defconst fsvn-svn-valid-propname-regexp "^[a-zA-Z:_][a-zA-Z-.:_]*$")

;; http://svn.collab.net/repos/svn/trunk/subversion/libsvn_subr/properties.c
(defun fsvn-svn-valid-propname-p (propname)
  "Valid PROPNAME or not."
  (string-match fsvn-svn-valid-propname-regexp propname))

(defun fsvn-propname-svn-texture-p (propname)
  (string-match "^svn:" propname))

(defcustom fsvn-svn-home-directory
  (let (tmp)
    (cond
     ((and (memq system-type '(windows-nt))
           (file-exists-p (setq tmp (expand-file-name "Subversion" (getenv "APPDATA")))))
      tmp)
     (t
      (expand-file-name ".subversion" (getenv "HOME")))))
  "*Directory of `svn' home directory."
  :group 'fsvn
  :type 'directory)

(defun fsvn-svn-auth-directory ()
  (fsvn-expand-file "auth" fsvn-svn-home-directory))

(defun fsvn-svn-gather-server ()
  (let ((regexp (concat "^<\\(" (regexp-opt fsvn-svn-url-scheme-segment-list) "[^/\n>]+\\)>" ))
        servers)
    (mapc
     (lambda (dir)
       (mapc
        (lambda (file)
          (let (server)
            (with-temp-buffer
              (insert-file-contents file)
              (goto-char (point-min))
              (while (re-search-forward regexp nil t)
                (setq server (match-string 1))
                (setq servers (cons server servers))))))
        (directory-files dir t dired-re-no-dot)))
     (directory-files (fsvn-svn-auth-directory) t dired-re-no-dot))
    servers))

(defun fsvn-prop-file-coding-system (propname)
  (cond
   ((member propname '("svn:ignore"))
    (fsvn-file-name-coding-system))
   ((member propname '("svn:log"))
    (terminal-coding-system))
   (t
    fsvn-prop-file-default-coding-system)))

(defun fsvn-file-prop-acceptable-p (file propname)
  (cond
   ((not (file-exists-p file))
    nil)
   ((not (fsvn-svn-valid-propname-p propname))
    nil)
   ((fsvn-file-exact-directory-p file)
    (null (member propname fsvn-nondirectory-property-list)))
   (t t)))



(defun fsvn-directory-under-versioned-p (directory)
  "Return non-nil when DIRECTORY guessed just under the versioned directory or subordinate."
  (let* ((dir (fsvn-expand-file directory))
         (before dir))
    (catch 'versioned
      (while (not (fsvn-file-name-root-p dir))
        (when (fsvn-directory-versioned-p dir)
          (throw 'versioned t))
        (setq dir (fsvn-url-dirname dir))
        ;; avoid invalid filename and eternal recurse.
        (when (string= before dir)
          (throw 'versioned nil))
        (setq before dir)))))

(defun fsvn-directory-versioned-p (directory)
  "Return non-nil when DIRECTORY guessed under versioned."
  (not (not (fsvn-file-control-directory directory))))

(defun fsvn-file-control-directory (file)
  (cond
   ((version< fsvn-svn-version "1.7.0")
    (let ((control (fsvn-expand-file (fsvn-meta-dir-name) file)))
      (and (fsvn-file-exact-directory-p control)
           control)))
   ((string-match "/\\.svn\\($\\|/\\)" file) nil)
   ((fsvn-magic-file-name-absolute-p file) nil)
   ;; TODO ignored status
   (t
    (let* ((dir (directory-file-name (fsvn-expand-file file)))
           ;; dummy
           (prev ""))
      (catch 'found
        (while (not (string= dir prev))
          (let ((control (concat dir "/.svn")))
            (when (file-exists-p control)
              (throw 'found control)))
          (setq prev dir)
          (setq dir (fsvn-file-name-directory dir))))))))

(defun fsvn-file-versioned-directory-p (file)
  (let* ((dir (fsvn-file-name-directory2 file)))
    (fsvn-directory-versioned-p dir)))

;; file: subversion/libsvn_wc/upgrade.c
;; function: version_string_from_format
(defun fsvn-file-wc-svn-version (file)
  (let ((format (fsvn-file-wc-version file)))
    (cond
     ((eq format 4) "1.3")
     ((eq format 8) "1.4")
     ((eq format 9) "1.5")
     ((eq format 10) "1.6")
     ((eq format 12) "1.7")
     (t (error "Not a supported format")))))

(defun fsvn-file-wc-version (file)
  (let ((ctl (fsvn-file-control-directory file)))
    (when ctl
      (with-temp-buffer
        (insert-file-contents (fsvn-expand-file "entries" ctl) nil 0 16)
        (and (looking-at "^\\([0-9]+\\)$")
             (string-to-number (match-string 1)))))))



(defconst fsvn-svn-date-regexp
  "^\
\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)\
T\
\\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\)\
\\.\
\\([0-9]\\{6\\}\\)\
Z\
$"
  )

;; todo elint bug?
;; (setq elint-unknown-builtin-args (delq (assq 'encode-time elint-unknown-builtin-args) elint-unknown-builtin-args))
;; (put 'encode-time 'elint-args nil)

(defun fsvn-svn-parse-date (date)
  (when (string-match fsvn-svn-date-regexp date)
    (let ((year (string-to-number (match-string 1 date)))
          (month (string-to-number (match-string 2 date)))
          (day (string-to-number (match-string 3 date)))
          (hh (string-to-number (match-string 4 date)))
          (mm (string-to-number (match-string 5 date)))
          (ss (string-to-number (match-string 6 date))))
      (encode-time ss mm hh day month year t))))

(defun fsvn-svn-parse-boolean (boolean)
  (if (string= "true" boolean)
      t
    nil))

(defun fsvn-svn:externals-parse-value (pinfo value)
  (mapcar
   (lambda (x)
     (let ((values (split-string x "[ \t]" t))
           real sym base-url)
       (setq real (car values))
       (setq sym (cadr values))
       (cond
        ((fsvn-url-repository-p real))
        ((string-match "^\\.\\./" real)
         ;; relate path from current directory
         (setq base-url (file-name-directory (fsvn-xml-info->entry=>url$ pinfo)))
         (setq real (fsvn-expand-url (replace-match "" nil nil real) base-url)))
        ((string-match "^\\^/" real)
         ;; relate path from repository root
         (setq base-url (fsvn-xml-info->entry=>repository=>root$ pinfo))
         (setq real (fsvn-expand-url (replace-match "" nil nil real) base-url)))
        ((string-match "^//" real)
         ;;FIXME not depend on scheme
         )
        ((string-match "^/" real)
         ;;FIXME relate path from server root
         ))
       (cons real sym)))
   (fsvn-string-line-to-list value)))

(defun fsvn-svn-autoprop-wildcard->regexp (prop)
  (let* ((alist '(("?" . ".?") ("*" . ".*"))))
    (concat "^" (fsvn-svn-autoprop-wildcard->regexp-internal prop alist) "$")))

(defun fsvn-svn-autoprop-wildcard->regexp-internal (string alist)
  (let ((item (car alist)))
    (cond
     ((null item)
      (regexp-quote string))
     (t
      (mapconcat
       'identity
       (mapcar
        (lambda (string)
          (fsvn-svn-autoprop-wildcard->regexp-internal string (cdr alist)))
        (split-string string (car item)))
       (cdr item))))))



(defconst fsvn-diff-subcommand-arg-regexp "^--\\(new\\|old\\)=\\(.+\\)")
(defconst fsvn-diff-separated-regexp
   "^@@ -\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? \\+\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? @@")



(defun fsvn-parse-result-cmd-lock (buffer &optional min)
  (with-current-buffer buffer
    (save-excursion
      (goto-char (or min (point-min)))
      (while (not (eobp))
        (when (looking-at "^'\\([^']+\\)' locked by user '\\([^']+\\)'\\.$")
          ;; (un)lock command in `fsvn-browse-mode' effected in a directory.
          (let ((file (fsvn-expand-file (match-string 1))))
            (fsvn-browse-redraw-wc-file-entry file)))
        (forward-line 1)))))

(defun fsvn-parse-result-cmd-unlock (buffer &optional min)
  (with-current-buffer buffer
    (save-excursion
      (goto-char (or min (point-min)))
      (while (not (eobp))
        (when (looking-at "^'\\([^']+\\)' unlocked\\.$")
          (let ((file (fsvn-expand-file (match-string 1))))
            (fsvn-browse-redraw-wc-file-entry file)))
        (forward-line 1)))))

(defun fsvn-parse-result-cmd-delete (buffer &optional min)
  (fsvn-parse-result-modify-cmd-wrapper-internal
   (format "^%c[ \t]+\\([^ \t].+\\)$" ?D)
   buffer min (lambda (f) (fsvn-browse-put-status-1 f ?D))))

(defun fsvn-parse-result-cmd-add (buffer &optional min)
  (let (files info)
    (setq files
          (fsvn-parse-result-modify-cmd-wrapper-internal
           (format "^%c\\(?:[ \t]+(bin)[ \t]+\\|[ \t]+\\)\\([^ \t].+\\)$" ?A)
           buffer min (lambda (f) (fsvn-browse-put-status-1 f ?A))))
    (when (> (length files) 0)
      (setq info (fsvn-get-info-entry (car files)))
      (when (fsvn-config-tortoise-property-use (fsvn-xml-info->entry=>repository=>root$ info))
        ;; todo asynchronous
        (fsvn-tortoise-tsvn:autoprops-set files buffer)))
    files))

(defun fsvn-parse-result-cmd-resolved (buffer &optional min)
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when (looking-at "^Resolved conflicted state of '\\([^']+\\)'$")
          (let* ((file (fsvn-expand-file (match-string 1)))
                 (dir (fsvn-file-name-directory2 file))
                 (name (fsvn-file-name-nondirectory file))
                 (regexp (concat "^" (regexp-quote name)))
                 entries)
            (fsvn-browse-redraw-wc-file-entry file)
            (fsvn-save-browse-directory-excursion dir
              (fsvn-browse-each-file fn dir
                (when (and (string-match regexp fn)
                           (not (file-exists-p (fsvn-expand-file fn dir))))
                  (setq entries (cons (fsvn-expand-file fn dir) entries))))
              (mapc
               (lambda (file)
                 (fsvn-browse-remove-wc-file-entry-internal file))
               entries))))
        (forward-line 1)))))

(defmacro fsvn-parse-result-modify-cmd-wrapper-internal (regexp buffer min action)
  `(let (dir files)
     (with-current-buffer ,buffer
       (save-excursion
         (goto-char (or min (point-min)))
         (while (re-search-forward ,regexp nil t)
           (setq files (cons (fsvn-expand-file (match-string 1)) files)))
         (mapc
          (lambda (file)
            (setq dir (fsvn-file-name-directory file))
            (fsvn-save-browse-directory-excursion dir
              (unless (fsvn-browse-goto-file file)
                (fsvn-browse-add-wc-raw-entry dir (fsvn-file-name-nondirectory file) file))
              (,action file)))
          files)))))

(defun fsvn-parse-result-cmd-revert (buffer &optional min)
  (fsvn-parse-result-modify-cmd-wrapper-internal
   (format "^Reverted[ \t]+'\\([^ \t].+\\)'$")
   buffer min (lambda (f) (fsvn-browse-clear-status f))))

(defun fsvn-parse-result-cmd-commit (buffer &optional min)
  ;;  for D ^Deleting
  ;;  for M ^Sending
  ;;     property is same
  ;;  for A ^Adding
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (fsvn-parse-result-cmd-commit-parse-line)
        (forward-line 1)))))

(defun fsvn-parse-result-cmd-commit-parse-line ()
  (let ((alist fsvn-parse-result-cmd-commit-behavior-alist)
        stamp func file)
    (while alist
      (setq stamp (caar alist))
      (setq func (cdar alist))
      (when (looking-at (format "^%s[ \t]+\\(.+\\)" stamp))
        (setq file (match-string 1))
        (setq alist nil)
        (funcall func (fsvn-expand-file file)))
      (setq alist (cdr alist)))))

(defun fsvn-parse-result-cmd-commit-modified (file)
  (fsvn-save-browse-file-excursion file
    ;;FIXME lock column, user column
    (fsvn-browse-draw-status-this-line))
  (fsvn-redraw-file-fancy-status file))

(defun fsvn-parse-result-cmd-commit-deleted (file)
  (fsvn-save-browse-file-excursion file
    (fsvn-browse-remove-wc-file-entry-internal file)))

(defun fsvn-parse-result-cmd-commit-added (file)
  (fsvn-save-browse-file-excursion file
    (fsvn-browse-draw-status-this-line))
  (fsvn-redraw-file-fancy-status file))

(defconst fsvn-parse-result-cmd-commit-behavior-alist
  '(
    ("Sending" . fsvn-parse-result-cmd-commit-modified)
    ("Deleting" . fsvn-parse-result-cmd-commit-deleted)
    ("Adding" . fsvn-parse-result-cmd-commit-added)
    ))

(defun fsvn-parse-result-instant-sentinel (proc parser)
  (set-process-sentinel proc
                        `(lambda (proc event)
                           (fsvn-process-exit-handler proc event
                             (,parser (current-buffer)))))
  proc)

(defun fsvn-parse-result-if-auth-prompt (proc)
  (let (string)
    (save-excursion
      (forward-line 0)
      (cond
       ((looking-at "^Username: ")
        (setq string (read-from-minibuffer "Username: ")))
       ((looking-at "^\\(?:Password\\|Passphrase\\) for '[^']*': ")
        (setq string (read-passwd (match-string 0))))
       ((looking-at "^Store \\(?:password\\|passphrase\\) unencrypted")
        (setq string (if (y-or-n-p "Store password unencrypted? ") "yes" "no")))))
    (when string
      (process-send-string proc (concat string "\n"))
      (insert "\n")
      t)))



;; * subcommand `update' utility.

;;TODO make obsolete
(defcustom fsvn-popup-result-update-parsed-threshold 1000000000
  "Threshold of `update' output size for parsing.
Huge value makes Emacs slow down."
  :group 'fsvn
  :type 'integer)

(defconst fsvn-process-filter-update-action-alist
  '(
    (?A . fsvn-process-filter-update-for-added)
    (?D . fsvn-process-filter-update-for-deleted)
    (?U . fsvn-process-filter-update-for-updated)
    (?C . fsvn-process-filter-update-for-conflicted)
    (?G . fsvn-process-filter-update-for-merged)
    (?E . fsvn-process-filter-update-for-existed)
    ))

(defvar fsvn-process-filter-for-update-parsed-end nil)

(defun fsvn-process-filter-for-update (proc event)
  (fsvn-process-event-handler proc event
    (let ((prev (or fsvn-process-filter-for-update-parsed-end (point-min)))
          end line matched-obj)
      (when (< prev fsvn-popup-result-update-parsed-threshold)
        (goto-char (or prev (point-min)))
        (while (and (not (eobp))
                    (looking-at "^\\(.*\\)\n"))
          (setq line (match-string 1)
                end (match-end 0))
          (when (setq matched-obj (fsvn-regexp-match "^\\([ADUCGE]\\) +\\(.+\\)$" line))
            (let ((direction (string-to-char (fsvn-regexp-matched matched-obj 1)))
                  (file (fsvn-expand-file (fsvn-regexp-matched matched-obj 2)))
                  actor-cell)
              (setq actor-cell (assq direction fsvn-process-filter-update-action-alist))
              (unless actor-cell
                (signal 'fsvn-parsing-error `("Assertion failed (Non defined mark)")))
              (funcall (cdr actor-cell) file)))
          (forward-line 1))
        (setq fsvn-process-filter-for-update-parsed-end end)))))

(defun fsvn-process-filter-update-for-added (file)
  (fsvn-browse-add-wc-file-entry file t))

(defun fsvn-process-filter-update-for-deleted (file)
  ;; not concern about exists buffer.
  (fsvn-browse-remove-wc-file-entry file))

(defun fsvn-process-filter-update-for-updated (file)
  (fsvn-file-buffer-updated file)
  (fsvn-save-browse-file-excursion file
    (fsvn-browse-draw-attr-this-line)))

(defun fsvn-process-filter-update-for-conflicted (file)
  (fsvn-file-buffer-updated file)
  (let ((dir (fsvn-file-name-directory2 file)))
    (fsvn-save-browse-directory-excursion dir
      (let ((filename (fsvn-file-name-nondirectory file)))
        (save-excursion
          (mapc
           (lambda (fn)
             (if (fsvn-browse-goto-file fn)
                 (when (fsvn-file= fn file)
                   ;; conflicted file is rare so not concern about `svn' execute.
                   (fsvn-browse-redraw-wc-file-entry fn))
               (fsvn-browse-add-wc-raw-entry dir (fsvn-file-name-nondirectory fn) fn)))
           (directory-files dir t (concat "^" (regexp-quote filename)))))))))

(defalias 'fsvn-process-filter-update-for-merged 'fsvn-process-filter-update-for-updated)

(defun fsvn-process-filter-update-for-existed (file)
  ;;todo do nothing? how to occur this.
  )

(defun fsvn-file-buffer-updated (file)
  (let ((buffer (get-file-buffer file)))
    (when (and buffer (not (buffer-modified-p buffer)))
      (with-current-buffer buffer
        (revert-buffer nil t)))))





(provide 'fsvn-deps)

;;; fsvn-deps.el ends here
