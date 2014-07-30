;;; fsvn-tortoise.el --- Provide TortoiseSVN like functions for fsvn.el


;;; History:
;; 

;;; Commentary:
;; 

;;; Code:



(require 'fsvn-ui)
(require 'fsvn-data)



(defconst fsvn-tortoise-property-list
  '(
    "bugtraq:url"
    "bugtraq:logregex"
    "bugtraq:label"
    "bugtraq:message"
    "bugtraq:number"
    "bugtraq:warnifnoissue"
    "bugtraq:append"
    "tsvn:logtemplate"
    "tsvn:logwidthmarker"
    "tsvn:logminsize"
    "tsvn:lockmsgminsize"
    "tsvn:logfilelistenglish"
    "tsvn:logsummary"
    "tsvn:projectlanguage"
    "tsvn:userfileproperties"
    "tsvn:userdirproperties"
    "tsvn:autoprops"
    "webviewer:revision"
    "webviewer:pathrevision"
    ))

(add-to-list 'fsvn-common-property-list 'fsvn-tortoise-property-list)

(defun fsvn-tortoise-commit-check (files dir)
  (fsvn-tortoise-bugtraq:*-commit-check dir)
  (fsvn-tortoise-tsvn:*-commit-check dir))

(defun fsvn-tortoise-tsvn:*-commit-check (dir)
  (let ((logminsize (fsvn-get-directory-parent-property dir "tsvn:logminsize"))
        (logwidthmarker (fsvn-get-directory-parent-property dir "tsvn:logwidthmarker")))
    (when logminsize
      (setq logminsize (fsvn-string-force-number logminsize)))
    (when logwidthmarker
      (setq logwidthmarker (fsvn-string-force-number logwidthmarker 80)))
    (when logminsize
      (unless (>= (buffer-size) logminsize)
        (unless (y-or-n-p (format "tsvn:logminsize is %d. Really commit? " logminsize))
          (fsvn-quit "Log min size error."))))
    (when logwidthmarker
      (when (catch 'width-over
              (save-excursion
                (goto-char (point-min))
                (while (not (eobp))
                  (forward-line 1)
                  (forward-char -1)
                  (when (> (current-column) logwidthmarker)
                    (throw 'width-over t))
                  (forward-line 1))))
        (unless (y-or-n-p (format "tsvn:logwidthmarker is %d. Really commit? " logwidthmarker))
          (fsvn-quit "Fill buffer."))))))

(defun fsvn-tortoise-tsvn:user*properties (target-file)
  (let* ((parent-dir (fsvn-file-name-directory2 target-file))
         userproperties userdirproperties userfileproperties)
    (if (not (fsvn-file-exact-directory-p target-file))
        (setq userfileproperties (fsvn-get-directory-parent-property parent-dir "tsvn:userfileproperties"))
      (setq userdirproperties (fsvn-get-directory-parent-property target-file "tsvn:userdirproperties"))
      (setq userfileproperties (fsvn-get-directory-parent-property target-file "tsvn:userfileproperties")))
    (when userdirproperties
      (setq userdirproperties (fsvn-string-line-to-list userdirproperties)))
    (when userfileproperties
      (setq userfileproperties (fsvn-string-line-to-list userfileproperties)))
    (setq userproperties (append userdirproperties userfileproperties))
    (when userproperties
      (mapcar
       (lambda (x) (cons x nil))
       userproperties))))

(defmacro fsvn-tortoise-bugtraq:*-exec (dir &rest form)
  (declare (indent 1))
  `(let ((URL (fsvn-get-directory-parent-property ,dir "bugtraq:url"))
         (MESSAGE (fsvn-get-directory-parent-property ,dir "bugtraq:message"))
         (LOGREGEX (fsvn-get-directory-parent-property ,dir "bugtraq:logregex"))
         (WARNIFNOISSUE (fsvn-get-directory-parent-property ,dir "bugtraq:warnifnoissue"))
         (NUMBER (fsvn-get-directory-parent-property ,dir "bugtraq:number"))
         (LABEL (fsvn-get-directory-parent-property ,dir "bugtraq:label"))
         (APPEND (fsvn-get-directory-parent-property ,dir "bugtraq:append"))
         MESSAGE-REGEX LOGREGEX1 LOGREGEX2)
     (when LOGREGEX
       (setq LOGREGEX (split-string LOGREGEX "[\n\r]" t))
       (when (= (length LOGREGEX) 2)
        (setq LOGREGEX1 (fsvn-string-rm-space (fsvn-tr1:wregex->regexp (car LOGREGEX)))
              LOGREGEX2 (fsvn-string-rm-space (fsvn-tr1:wregex->regexp (cadr LOGREGEX))))))
     (setq NUMBER (fsvn-tortoise-boolean-value NUMBER nil))
     (setq WARNIFNOISSUE (fsvn-tortoise-boolean-value WARNIFNOISSUE t))
     (setq APPEND  (fsvn-tortoise-boolean-value APPEND t))
     (setq MESSAGE-REGEX (fsvn-tortoise-bugtraq-create-message-regexp MESSAGE APPEND NUMBER))
     ,@form
     ))

(defun fsvn-tortoise-bugtraq:*-commit-check (dir)
  (fsvn-tortoise-bugtraq:*-exec dir
    (let (real-label)
      (setq real-label
            (if LABEL
                (fsvn-string-rm-rspace LABEL)
              "Bug ID/Issue ID: "))
      (cond
       ;; bugtraq:logregex priority to bugtraq:message
       ;; http://tortoisesvn.net/docs/release/TortoiseSVN_ja/tsvn-dug-bugtracker.html
       ((and LOGREGEX1 LOGREGEX2)
        (let (ng)
          (save-excursion
            (goto-char (point-min))
            (if (not (re-search-forward LOGREGEX1 nil t))
                (setq ng t)
              (let* ((matched (fsvn-tortoise-gather-matched LOGREGEX1)))
                (unless (catch 'found
                          (mapc
                           (lambda (m)
                             (when (and m (string-match LOGREGEX2 m))
                               (throw 'found t)))
                           matched)
                          nil)
                  (setq ng t)))))
          (when ng
            (unless (or (fsvn-tortoise-bugtraq-commit-set-bugid MESSAGE NUMBER real-label APPEND)
                        (null WARNIFNOISSUE)
                        (y-or-n-p (format "Log message contains no \"%s\". Really commit? " real-label)))
              (fsvn-quit "Miss \"%s\" in Log message" real-label)))))
       (MESSAGE-REGEX
        (save-excursion
          (goto-char (point-min))
          (unless (re-search-forward MESSAGE-REGEX nil t)
            (unless (or (fsvn-tortoise-bugtraq-commit-set-bugid MESSAGE NUMBER real-label APPEND)
                        (null WARNIFNOISSUE)
                        (y-or-n-p (format "Log message contains no \"%s\". Really commit? " real-label)))
              (fsvn-quit "Miss \"%s\" in Log message" real-label)))))
       (LABEL
        (unless (y-or-n-p (format "Log message contains no \"%s\". Really commit? " real-label))
          (fsvn-quit "Set property `bugtraq:logregex' or `bugtraq:message' correctly")))))))

(defun fsvn-tortoise-bugtraq-create-message-regexp (message append number)
  (when (and message (string-match "%BUGID%" message))
    (let (mleft mright)
      (setq mleft (regexp-quote (substring message 0 (match-beginning 0)))
            mright (regexp-quote (substring message (match-end 0))))
      (concat (if append "^" "\\`")
              mleft
              "\\(" (if number "[0-9]+" ".+") "\\)"
              mright
              (if append "\\'" "$")))))

(defun fsvn-tortoise-bugtraq-create-url (url bugid)
  (when (and url (string-match "%BUGID%" url))
    (replace-match bugid nil nil url)))

(defun fsvn-tortoise-bugtraq-commit-set-bugid (message number label append)
  (when (and message (string-match "%BUGID%" message))
    ;;FIXME % escape..
    (setq message (replace-match "%s" t nil message))
    (let (id)
      (setq id (fsvn-tortoise-bugtraq-commit-read-bugid number label))
      (when id
        (save-excursion
          (goto-char (if append (point-max) (point-min)))
          (when append
            (insert "\n"))
          (insert (format message id))
          (unless append
            (insert "\n"))
          id)))))

(defun fsvn-tortoise-bugtraq-commit-read-bugid (number label)
  (let (tmp)
    (cond
     (number
      (setq tmp (fsvn-read-number (format "%s " label) 'allow-null))
      (when tmp
        (number-to-string tmp)))
     (t
      (setq tmp (read-string (format "%s " label)))
      (unless (zerop (length tmp))
        tmp)))))

(defun fsvn-tortoise-gather-matched (regexp &optional string)
  (let* ((depth (regexp-opt-depth regexp))
         (i 1)
         matched)
    (while (<= i depth)
      (setq matched (cons (match-string i string) matched))
      (setq i (1+ i)))
    (nreverse matched)))

(defun fsvn-tortoise-boolean-value (value default)
  (unless (stringp value)
    (setq value (if default "true" "false")))
  (setq value (downcase value))
  ;; http://tortoisesvn.tigris.org/svn/tortoisesvn/trunk/src/TortoiseProc/ProjectProperties.cpp
  (if default
      (or (string= value "true") (string= value "yes"))
    (not (or (string= value "false") (string= value "no")))))

(defun fsvn-tortoise-tsvn:autoprops-parse (value)
  (fsvn-mapitem
   (lambda (line)
     (when (string-match "\\([^=]+\\)=\\(.*\\)" line)
       (let ((key (match-string 1 line))
             (value (match-string 2 line)))
         (setq key (fsvn-string-rm-space key))
         (setq value (fsvn-string-rm-space value))
         (let ((values (split-string value "[; \t]" t))
               alist)
           (setq alist
                 (fsvn-mapitem
                  (lambda (value)
                    (when (string-match "^\\([^=]+\\)=\\(.*\\)" value)
                      (cons (match-string 1 value) (match-string 2 value))))
                  values))
           (cons (fsvn-svn-autoprop-wildcard->regexp key) alist)))))
   (split-string value "\n")))

(defun fsvn-tortoise-tsvn:autoprops-set (files buffer)
  (let (prop filename regexp prop-alist propname value tmpfile)
    (mapc
     (lambda (file)
       (setq prop (fsvn-get-file-parent-property file "tsvn:autoprops"))
       (setq filename (fsvn-url-filename file))
       (when prop
         (mapc
          (lambda (regexp-prop-alist)
            (setq regexp (car regexp-prop-alist))
            (setq prop-alist (cdr regexp-prop-alist))
            (when (string-match regexp filename)
              (mapc
               (lambda (prop-value)
                 (setq propname (car prop-value))
                 (setq value (cdr prop-value))
                 (when (fsvn-file-prop-acceptable-p file propname)
                   (setq tmpfile (fsvn-get-prop-temp-file propname value))
                   (fsvn-insert-string-to-buffer "\n" buffer)
                   (fsvn-call-command-display "propset" buffer propname "--file" tmpfile file)))
               prop-alist)))
          (fsvn-tortoise-tsvn:autoprops-parse prop))))
     files)))

(defun fsvn-tortoise-commit-initialize ()
  (let ((template (fsvn-get-directory-parent-property default-directory "tsvn:logtemplate"))
        (logwidthmarker (fsvn-get-directory-parent-property default-directory "tsvn:logwidthmarker")))
    (when template
      (insert template))
    (when logwidthmarker
      (setq logwidthmarker (fsvn-string-force-number logwidthmarker 80)))
    (when logwidthmarker
      (setq fill-column logwidthmarker)
      (auto-fill-mode 1))))

(defun fsvn-tortoise-fontify-buffer ()
  (fsvn-tortoise-bugtraq:*-exec default-directory
    (when URL
      (cond
       ((and LOGREGEX1 LOGREGEX2)
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward LOGREGEX1 nil t)
            (save-restriction
              (narrow-to-region (match-beginning 0) (match-end 0))
              (fsvn-tortoise-fontify-buffer-url URL LOGREGEX2)))))
       (MESSAGE-REGEX
        (fsvn-tortoise-fontify-buffer-url URL MESSAGE-REGEX))))))

(defun fsvn-tortoise-fontify-buffer-url (url regexp)
  (let (buffer-read-only real-url)
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward regexp nil t)
        (save-match-data
          (setq real-url (fsvn-tortoise-bugtraq-create-url url (match-string 1))))
        (put-text-property (match-beginning 0) (match-end 0) 'face fsvn-link-face)
        (put-text-property (match-beginning 0) (match-end 0) 'fsvn-url-link real-url)))))

(provide 'fsvn-tortoise)

;;; fsvn-tortoise.el ends here
