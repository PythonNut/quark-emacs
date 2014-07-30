;;; fsvn-minibuf.el --- Read from minibuffer utility for fsvn.el


;;; History:
;; 

;;; Commentary:
;; 

;;; Code:



(require 'minibuffer)
(require 'fsvn-deps)
(require 'fsvn-url)



(defvar minibuffer-local-map)
(defvar last-command)
(defvar this-command)



(defun fsvn-read-versioned-directory (&optional prompt)
  (let (dir message-log-max)
    (setq prompt (or prompt "Versioned directory: "))
    (while (and (setq dir (fsvn-read-directory-name prompt))
                (not (fsvn-directory-versioned-p dir)))
      (message "Non versioned directory.")
      (sit-for 1))
    dir))

(defun fsvn-read-file-under-versioned (prompt init-value)
  (let ((init init-value)
        file message-log-max)
    (while (and (setq file (fsvn-expand-file
                            (read-file-name prompt nil nil nil
                                            (and init (fsvn-urlrev-filename init)))))
                (not (fsvn-directory-under-versioned-p file)))
      (setq init file)
      (message "Non versioned directory.")
      (sit-for 1))
    file))

(defun fsvn-read-mkdir-directory (dir)
  (if (fsvn-url-repository-p dir)
      (fsvn-completing-read-url "Directory: " (fsvn-url-as-directory dir) t)
    (fsvn-read-directory-name "Directory: " (fsvn-url-as-directory dir) nil nil)))

(defun fsvn-read-directory-name (prompt &optional dir default-dirname mustmatch initial)
  (fsvn-expand-file (read-directory-name prompt dir default-dirname mustmatch initial)))

(defun fsvn-read-file-name (prompt &optional dir default-filename mustmatch initial predicate)
  (setq dir (file-name-as-directory (or dir default-directory)))
  (fsvn-expand-file (read-file-name prompt dir default-filename mustmatch initial predicate)))

(defun fsvn-read-number (prompt &optional default)
  "Read a numeric value in the minibuffer, prompting with PROMPT.
DEFAULT specifies a default value to return if the user just types RET.
The value of DEFAULT is not a number, allow to enter a nil value."
  (let ((n nil))
    (while
        (progn
          (let ((str (read-from-minibuffer prompt nil nil nil nil
                                           (and (numberp default)
                                                (number-to-string default)))))
            (condition-case nil
                (setq n (cond
                         ((and default (not (numberp default)) (zerop (length str))) nil)
                         ((zerop (length str)) default)
                         ((stringp str) (read str))))
              (error nil)))
          (unless (or (numberp n) (and default (null n)))
            (message "Please enter a number.")
            (sit-for 1)
            t)))
    n))

(defvar fsvn-read-subcommand-history nil)
(defun fsvn-read-subcommand (alist &optional prompt)
  (let ((readed  (completing-read
                  (concat prompt "svn subcommand: ")
                  alist
                  nil t nil 'fsvn-read-subcommand-history)))
    (cdr (assoc readed alist))))

(defun fsvn-read-svn-subcommand (&optional prompt)
  (fsvn-read-subcommand fsvn-svn-subcommand-completion-alist prompt))

(defun fsvn-read-svnadmin-subcommand (&optional prompt)
  (fsvn-read-subcommand fsvn-svnadmin-subcommand-completion-alist prompt))

(defvar fsvn-read-propname-history nil)
(defun fsvn-read-propname (file)
  (let (propname)
    (while (null propname)
      (setq propname (completing-read "SVN Property: "
                                      (fsvn-propname-completion-alist file)
                                      nil nil nil 'fsvn-read-propname-history))
      (unless (fsvn-svn-valid-propname-p propname)
        (let (message-log-max)
          (message "%s is not valid svn property name." propname)
          (sit-for 1)
          (setq propname nil))))
    propname))

(defvar fsvn-read-revprop-history nil)
(defun fsvn-read-revprop ()
  (completing-read "SVN Revprop: "
                   fsvn-revprop-list nil nil nil 'fsvn-read-revprop-history))

(defvar fsvn-read-changelist-history nil)
(defun fsvn-read-changelist-name ()
  (read-from-minibuffer "Changelist Name: " nil nil nil 'fsvn-read-changelist-history))

(defun fsvn-read-resolve-accept-arg ()
  (let ((args (cdr (fsvn-complete-reading-subcommand fsvn-svn-subcommand-arguments-alist "resolve" t)))
        completions ret)
    (unless args
      (error "svn `resolve' not supported."))
    (setq completions (fsvn-complete-subcommand-create-completions '("--accept") nil args))
    (setq ret (completing-read "Resolve `--accept' arg: " completions nil 'no nil nil ))
    (when (string= ret "")
      (error "Accept arg must be selected"))
    ret))



(defvar fsvn-revision-read-history nil)
(defvar fsvn-completing-revision-map nil)

(unless fsvn-completing-revision-map
  (setq fsvn-completing-revision-map
        (let ((map (copy-keymap minibuffer-local-map)))

          (define-key map "\C-i" 'fsvn-complete-revision-action)
          (define-key map " " 'fsvn-complete-revision-action)

          map)))

(defun fsvn-completing-read-revision (&optional prompt initial default complete-url)
  "Read revision string from minibuffer.
"
  (setq default (or default "HEAD"))
  (catch 'done
    (let ((value (when initial (fsvn-get-revision-string initial)))
          (fsvn-complete-processing-revision-urlrev complete-url)
          completions)
      (while t
        (setq value
              (read-from-minibuffer
               (or prompt "Revision: ")
               value
               fsvn-completing-revision-map nil
               'fsvn-revision-read-history))
       (setq completions (all-completions value fsvn-revision-string-list))
        (cond
         ((string= value "")
          (setq value default))
         ((> (length completions) 1)
          (setq value (try-completion value fsvn-revision-string-list)))
         ((= (length completions) 1)
          (when (string= (car completions) value)
            (throw 'done value))
          (setq value (car completions)))
         ((or (fsvn-revision-date-p value)
              (fsvn-revision-number-p value))
          (throw 'done value)))))))

(defun fsvn-completing-read-revision-range (initial-from initial-to &optional url)
  (let* ((from (fsvn-completing-read-revision "Revision From: " initial-from nil url))
         (to (fsvn-completing-read-revision 
              (format "Revision %s -> Revision To: " from) initial-to nil url)))
    (cons from to)))



(defvar fsvn-completing-read-repository-history nil)

(defvar fsvn-completing-repository-map nil)
(unless fsvn-completing-repository-map
  (setq fsvn-completing-repository-map
        (let ((map (copy-keymap minibuffer-local-map)))

          (define-key map "\C-i" 'fsvn-complete-url-action)
          (define-key map " " 'fsvn-complete-url-action)

          map)))

(defvar fsvn-completing-repository-only-repos nil)

(defun fsvn-completing-read-url (&optional prompt default only-repository)
  (fsvn-completing-read-url-initialize)
  (setq fsvn-completing-repository-only-repos only-repository)
  (let ((url-string))
    (while (or (null url-string)
               (and (not (fsvn-url-local-p url-string))
                    (not (fsvn-url-repository-p url-string))))
      (setq url-string (read-from-minibuffer
                 (or prompt "URL: ")
                 (or url-string default) fsvn-completing-repository-map nil
                 'fsvn-completing-read-repository-history)))
    (fsvn-expand-url url-string)))

(defun fsvn-completing-read-urlrev (&optional prompt default-urlrev only-repository)
  (let* ((default-url (and default-urlrev (fsvn-urlrev-url default-urlrev)))
         (default-rev (and default-urlrev (fsvn-urlrev-revision default-urlrev)))
         (url (fsvn-completing-read-url prompt default-url only-repository))
         (rev (fsvn-completing-read-revision 
               (format "%s%s Revision: " (or prompt "URL: ") url)
               default-rev nil url)))
    (fsvn-url-urlrev url rev)))

(defun fsvn-completing-read-url-initialize ()
  (setq fsvn-complete-completion-repository-cache nil))



(defvar fsvn-read-subcommand-args-map nil
  "Local keymap for minibuffer to read fsvn long options.")

(unless fsvn-read-subcommand-args-map
  (setq fsvn-read-subcommand-args-map
        (let ((map (copy-keymap minibuffer-local-map)))

          (define-key map " "    'self-insert-command)
          (define-key map "\C-i" 'fsvn-complete-subcommand-args-action)

          map)))

(defun fsvn-read-subcommand-args (subcommand &optional non-global &rest default-args)
  (setq fsvn-complete-completion-saved-configuration nil)
  (setq default-args (fsvn-command-args-canonicalize default-args))
  (unless fsvn-complete-reading-subcommand
    (error "Subcommand %s not found" subcommand))
  (unwind-protect
      (fsvn-complete-subcommand-expand-arguments
       (read-from-minibuffer (format "Args for `%s' " subcommand)
                             (fsvn-command-args-to-command-line default-args)
                             fsvn-read-subcommand-args-map))
    (setq fsvn-complete-completion-saved-configuration nil)
    (setq fsvn-complete-reading-subcommand nil)))

(defun fsvn-read-svn-subcommand-args (subcommand &optional non-global &rest default-args)
  (setq fsvn-complete-reading-subcommand
        (fsvn-complete-reading-subcommand fsvn-svn-subcommand-arguments-alist subcommand non-global))
  (apply 'fsvn-read-subcommand-args subcommand non-global default-args))

(defun fsvn-read-svnadmin-subcommand-args (subcommand &rest default-args)
  (setq fsvn-complete-reading-subcommand
        (fsvn-complete-reading-subcommand fsvn-svnadmin-subcommand-arguments-alist subcommand nil))
  (apply 'fsvn-read-subcommand-args subcommand nil default-args))

(defun fsvn-command-args-to-command-line (args)
  (mapconcat 
   (lambda (arg)
     (cond
      ((string-match "[ \t]" arg)
       (concat "\"" arg "\""))
      (t
       arg)))
   args
   " "))


;;
;; completing read utility
;;

(defvar fsvn-complete-reading-subcommand nil)
(defun fsvn-complete-reading-subcommand (alist subcommand non-global)
  (let ((tmp (cdr (copy-sequence (assoc subcommand alist))))
        ret)
    (setq ret tmp)
    (when non-global
      (setq ret nil)
      (mapc
       (lambda (x)
         (unless (member (caar x) fsvn-svn-subcommand-global-options)
           (setq ret (cons x ret))))
       tmp)
      (setq ret (nreverse ret)))
    (cons subcommand ret)))

;; todo refactor fsvn-complete-completion-window-show
(defun fsvn-complete-completion-window-control (string collections)
  (let ((list (all-completions string collections)))
    (if (= (length list) 1)
        (fsvn-complete-completion-window-delete)
      (fsvn-complete-completion-window-show list))))

(defconst fsvn-complete-completion-buffer " *Fsvn completion*")

(defvar fsvn-complete-completion-saved-configuration nil)
(defvar fsvn-complete-completion-saved-applicant nil)

(defun fsvn-complete-completion-window-show (list)
  (unless fsvn-complete-completion-saved-configuration
    (setq fsvn-complete-completion-saved-configuration (current-window-configuration)))
  (let ((win (get-buffer-window fsvn-complete-completion-buffer)))
    (cond
     ((and win list (equal fsvn-complete-completion-saved-applicant list))
      (save-selected-window
        (select-window win)
        (condition-case err
            (scroll-up)
          (end-of-buffer (progn (goto-char (point-min)))))))
     ((or win list)
      (with-output-to-temp-buffer fsvn-complete-completion-buffer
        (display-completion-list list)))
     (t
      (fsvn-display-momentary-message " [No completions here]")))
    (setq fsvn-complete-completion-saved-applicant list)))

(defun fsvn-complete-completion-window-delete ()
  (when (and fsvn-complete-completion-saved-configuration
             (window-configuration-p fsvn-complete-completion-saved-configuration))
    (set-window-configuration fsvn-complete-completion-saved-configuration)
    (setq fsvn-complete-completion-saved-configuration nil
          fsvn-complete-completion-saved-applicant nil)))

(defvar fsvn-complete-word-class " \n\t")
(defun fsvn-complete-non-word-class ()
  (concat "^" fsvn-complete-word-class))

(defvar fsvn-complete-processing-revision-urlrev nil)

(defun fsvn-complete-revision-action ()
  (interactive)
  (let ((value (fsvn-complete-revision-current-value)))
    (cond
     ((or (null value) (string= "" value))
      (if (or (null fsvn-complete-processing-revision-urlrev)
              (not (eq last-command 'fsvn-complete-revision-action)))
          (fsvn-complete-revision-symbol "")
        (let (urlrev)
          (setq urlrev (fsvn-electric-select-log fsvn-complete-processing-revision-urlrev))
          (when urlrev
            (insert (fsvn-get-revision-string (fsvn-urlrev-revision urlrev)))))))
     ((string-match "^[0-9]+$" value)
      ;; do nothing
      (fsvn-complete-completion-window-delete)
      (fsvn-display-momentary-message " [Number context]"))
     ((string-match "^{" value)
      ;; do nothing
      (fsvn-complete-completion-window-delete)
      (fsvn-display-momentary-message " [Date context]"))
     (t
      (fsvn-complete-revision-symbol value)))
    (when (and fsvn-complete-processing-revision-urlrev 
               (string= (fsvn-complete-current-value) ""))
      (fsvn-display-momentary-message " [Type again]"))))

(defun fsvn-complete-revision-current-value ()
  (let* ((value (fsvn-complete-current-value))
         values)
    (when value
      (setq values (split-string value ":"))
      (or (cadr values)
          (car values)))))

(defun fsvn-complete-revision-symbol (value)
  (let ((comp (try-completion (upcase value) fsvn-revision-string-list)))
    (cond
     ((eq comp t))
     ((and (stringp comp) (> (length comp) 0))
      (delete-char (- (length value)))
      (insert comp))
     (t
      (fsvn-complete-completion-window-control value fsvn-revision-string-list)))))

(defun fsvn-complete-current-value ()
  ""
  (let ((parsed (fsvn-complete-parse-current-values)))
    (when (caar parsed)
      (nth (caar parsed) (cdr parsed)))))

(defun fsvn-complete-previous-values ()
  ""
  (let ((parsed (fsvn-complete-parse-current-values)))
    (remove nil (fsvn-take (1+ (cdar parsed)) (cdr parsed)))))

(defun fsvn-complete-parse-current-values ()
  (let (start end current)
    (if (minibufferp)
        (setq start (or (next-single-property-change (point-min) 'read-only) (point-min))
              end (point-max))
      (setq start (line-beginning-position)
            end (line-end-position)))
    (setq current (- (point) start))
    (fsvn-complete-reading-split-arguments (buffer-substring start end) current)))

(defun fsvn-complete-url-action ()
  (interactive)
  ;;todo contains space char filename
  (skip-chars-forward "^/ \n\t")
  (let ((value (fsvn-complete-current-value)))
    (cond
     ((null value)
      (fsvn-complete-url-repos-scheme ""))
     ((or (string= value "") (try-completion value fsvn-svn-url-scheme-list))
      (fsvn-complete-url-repos-scheme value))
     ((or fsvn-completing-repository-only-repos
          (fsvn-complete-url-repository-p value))
      (fsvn-complete-url-repository-url value))
     ((fsvn-url-local-p value)
      (fsvn-complete-url-local-file value))
     (t
      (fsvn-complete-url-no-completions value)))))

(defun fsvn-complete-url-repos-scheme (contents)
  (let* (comp)
    (setq comp (try-completion contents fsvn-svn-url-scheme-segment-list))
    (unless (string= comp "")
      (fsvn-complete-url-clear-segment)
      (if (stringp comp)
          (insert comp)
        (insert contents)))
    (fsvn-complete-completion-window-control contents fsvn-svn-url-scheme-list)))

(defun fsvn-complete-url-repository-url (value)
  (cond
   ((fsvn-complete-url-local-repository-p value)
    (fsvn-complete-url-local-file value))
   ((eq (char-before) ?:)
    (fsvn-complete-url-repos-root value))
   ((fsvn-complete-url-host-segment-p value)
    (fsvn-complete-url-repos-root value))
   (t
    (fsvn-complete-url-repos-file value))))

(defun fsvn-complete-url-repos-root (value)
  ;;todo gathered in buffers -> file cache. which one.
  (let ((applicant (fsvn-gather-root))
        comp)
    (fsvn-complete-completion-window-control value applicant)
    (when (and (setq comp (try-completion value applicant))
               (stringp comp))
      (fsvn-complete-url-clear-sentence)
      (insert comp))))

(defun fsvn-complete-url-repos-file (value)
  (let* ((name (fsvn-complete-url-filename value))
         (dirname (fsvn-complete-url-previous-segment value))
         applicant comp)
    (setq applicant (fsvn-complete-completion-repos-alist dirname))
    (fsvn-complete-completion-window-control name applicant)
    (when (and (setq comp (try-completion name applicant))
               (stringp comp))
      (fsvn-complete-url-clear-segment)
      (insert comp))))

(defun fsvn-complete-url-local-file (init)
  (when (fsvn-complete-url-clear-segment)
    (insert (fsvn-file-name-nondirectory init)))
  (let* ((value (fsvn-complete-current-value))
         (name (fsvn-complete-url-filename value))
         (dirname (fsvn-complete-url-local-file-previous-segment value))
         (regexp (and (string= "" name) (concat "^" (regexp-quote name))))
         applicant comp)
    (setq applicant (fsvn-complete-url-dir-files-completions dirname regexp))
    (fsvn-complete-completion-window-control name applicant)
    (when (setq comp (try-completion name applicant))
      (fsvn-complete-url-clear-segment)
      (if (stringp comp)
          (insert comp)
        (insert name)))))

(defun fsvn-complete-url-no-completions (init)
  "No applicant as local directory."
  (fsvn-complete-url-clear-segment)
  (insert (fsvn-expand-file init))
  (let (applicant comp)
    (setq applicant (fsvn-complete-url-dir-files-completions default-directory (concat "^" (regexp-quote init))))
    (fsvn-complete-completion-window-control init applicant)
    (when (setq comp (try-completion init applicant))
      (fsvn-complete-url-clear-segment)
      (insert comp))))

(defun fsvn-complete-url-dir-files-completions (dir &optional match)
  (when (file-directory-p dir)
    (let (name)
      (mapcar
       (lambda (file)
         (setq name
               (if (eq t (car (file-attributes file)))
                   (concat file "/")
                 file))
         (fsvn-complete-url-last-segment name))
       (directory-files dir t (or match dired-re-no-dot))))))

(defun fsvn-complete-url-clear-sentence ()
  (let ((start (fsvn-complete-url-start-sentence))
        (end (fsvn-complete-url-end-sentence)))
    (if (= start end)
        nil
      (delete-region start end)
      t)))
  
(defun fsvn-complete-url-start-sentence ()
  (save-excursion 
    (skip-chars-backward (fsvn-complete-non-word-class))
    (point)))

(defun fsvn-complete-url-end-sentence ()
  (save-excursion 
    (skip-chars-forward (fsvn-complete-non-word-class))
    (point)))

(defun fsvn-complete-url-clear-segment ()
  (let ((start (fsvn-complete-url-start-segment))
        (end (fsvn-complete-url-end-segment)))
    (if (= start end)
        nil
      (delete-region start end)
      t)))

(defun fsvn-complete-url-start-segment ()
  (save-excursion 
    (skip-chars-backward "^/ \t\n")
    (point)))

(defun fsvn-complete-url-end-segment ()
  (save-excursion 
    (skip-chars-forward "^/ \t\n")
    (point)))

(defun fsvn-complete-url-local-file-previous-segment (string)
  (cond
   ;; for windows
   ((string-match "^file:///\\([a-zA-Z]:.*\\)/[^/]*$" string)
    (match-string 1 string))
   ;; for windows
   ((string-match "^file:///\\([a-zA-Z]:\\)" string)
    (match-string 1 string))
   ((string-match "^file://\\(/.*?\\)[^/]*$" string)
    (match-string 1 string))
   ((string-match "^file://\\(/.*?\\)/$" string)
    (match-string 1 string))
   (t
    (fsvn-complete-url-previous-segment string))))

(defun fsvn-complete-url-previous-segment (string)
  (cond
   ((fsvn-file-name-root-p string)
    string)
   ((string-match "^\\(.*\\)/$" string)
    (match-string 1 string))
   ((string-match "^\\(.*\\)/[^/]+$" string)
    (match-string 1 string))))

(defun fsvn-complete-url-host-segment-p (contents)
  (let ((regexp (concat "^" (regexp-opt (fsvn-delete "file" fsvn-svn-url-scheme-list) t) ":/+\\([^/]+\\)?$")))
    (string-match regexp contents)))

(defun fsvn-complete-url-local-repository-p (contents)
  (let ((regexp "^file:///"))
    (when (string-match regexp contents)
      (not (fsvn-any-startswith (fsvn-gather-root) contents)))))

(defun fsvn-complete-url-last-segment (url)
  (when (string-match "/\\([^/]+/?\\)$" url)
    (match-string 1 url)))

(defun fsvn-complete-url-filename (string)
  (if (string-match "/\\([^/]+\\)$" string)
      (match-string 1 string)
    ""))

(defvar fsvn-complete-completion-repository-cache nil)

(defun fsvn-complete-completion-repos-alist (url)
  (let ((dir (fsvn-urlrev-url (directory-file-name url)))
        tmp)
    (unless (fsvn-string-assoc dir fsvn-complete-completion-repository-cache)
      (setq tmp
            (mapcar
             (lambda (entry)
               (let ((name (fsvn-xml-lists->list->entry=>name$ entry)))
                 ;;todo
                 (setq name (replace-regexp-in-string " " "%20" name))
                 (cons
                  (if (eq (fsvn-xml-lists->list->entry.kind entry) 'dir)
                      (concat name "/")
                    name)
                  nil)))
             ;; append HEAD probablly svn bug of parse url.
             (fsvn-get-ls (fsvn-url-urlrev dir "HEAD"))))
      (setq fsvn-complete-completion-repository-cache
            (cons
             (cons dir tmp)
             fsvn-complete-completion-repository-cache)))
    (cdr (fsvn-string-assoc dir fsvn-complete-completion-repository-cache))))

(defun fsvn-complete-url-repository-p (url)
  (string-match (concat "^" (regexp-opt fsvn-svn-url-scheme-list) ":") url))

(defun fsvn-complete-subcommand-args-action ()
  (interactive)
  (let ((prevs (fsvn-complete-previous-values)))
    (fsvn-complete-subcommand-forward-word)
    (let* ((args (cdr fsvn-complete-reading-subcommand))
           (curr (fsvn-complete-subcommand-previous-argument prevs))
           (completions (fsvn-complete-subcommand-create-completions prevs curr args)))
      (if (and completions (functionp completions))
          (funcall completions)
        (let* ((applicant (all-completions (or curr "") completions))
               (complete (try-completion (or curr "") completions)))
          (cond
           ((= (length applicant) 0)
            (fsvn-complete-completion-window-show nil))
           ((= (length applicant) 1)
            (fsvn-complete-completion-window-delete)
            (fsvn-complete-subcommand-previous-delete)
            (insert (car applicant))
            (fsvn-display-momentary-message " [Sole Match]"))
           (complete
            (fsvn-complete-subcommand-previous-delete)
            (insert complete)
            (fsvn-complete-completion-window-show applicant))
           (t
            (fsvn-complete-completion-window-show applicant))))))))

(defun fsvn-complete-subcommand-previous-delete ()
  (let ((end (point))
        start)
    (skip-chars-backward (fsvn-complete-non-word-class))
    (setq start (point))
    (delete-region start end)))

(defun fsvn-complete-subcommand-previous-argument (prevs)
  (let ((c (char-before)))
    ;; space == 0x20 or tab == 0x9
    (if (memq c '(#x20 #x9))
        nil
      (car (nreverse (copy-sequence prevs))))))

(defun fsvn-complete-subcommand-forward-word ()
  (let ((string (buffer-substring (point) (line-end-position))))
    (skip-chars-forward "^\"/ \t\n")))

(defvar fsvn-complete-reading-argument-syntax
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\" "\"" st)
    (modify-syntax-entry ?\' "\"" st)
    (modify-syntax-entry ?\x020 "-" st)
    (modify-syntax-entry ?\x09 "-" st)
    st))

(defun fsvn-complete-subcommand-expand-arguments (string)
  (let (tmp)
    (mapcar
     (lambda (x)
       (cond
        ((and (string-match "^-[^-]$" x)
              (setq tmp (fsvn-subcommand-assoc-argument x (cdr fsvn-complete-reading-subcommand))))
         ;; get long option
         (caar tmp))
        ((fsvn-url-repository-p x)
         (fsvn-url-string-to-urlrev x))
        (t
         x)))
     (fsvn-complete-reading-split-arguments string))))

(defun fsvn-complete-reading-split-arguments (string &optional cursor)
  (with-temp-buffer
    (let ((st fsvn-complete-reading-argument-syntax)
          (index 0)
          cursor-index cursor-index2
          begin finish ret)
      (set-syntax-table st)
      (insert string)
      (goto-char (point-min))
      (skip-syntax-forward "-")
      (setq begin (point))
      (condition-case err
          (while (not (eobp))
            (if (= (char-syntax (char-after (point))) ?\")
                (progn
                  (forward-char 1)
                  (setq begin (point))
                  (skip-syntax-forward "^\"")
                  (setq finish (point))
                  (skip-syntax-forward "\""))
              (skip-syntax-forward "^-")
              (setq finish (point)))
            (setq ret (cons (buffer-substring begin finish) ret))
            (when cursor
              (when (and (<= begin cursor) (< cursor finish))
                (setq cursor-index index))
              (when (and (null cursor-index2) (< cursor (point)))
                (setq cursor-index2 index)))
            (skip-syntax-forward "-")
            (setq index (1+ index))
            (setq begin (point)))
        (scan-error
         (setq ret (cons (buffer-substring begin (point-max)) ret))
         (unless cursor-index
           (setq cursor-index (1+ index)))))
      (unless cursor-index2
        (setq cursor-index2 index))
      (if cursor
          (cons (cons cursor-index cursor-index2) (nreverse ret))
        (nreverse ret)))))

(defun fsvn-complete-subcommand-create-completions (prevs current all-applicant)
  (let* ((toplevel (fsvn-complete-subcommand-create-completions-toplevel all-applicant))
         (collection toplevel)
         (next-applicant all-applicant)
         prev completions item)
    (while prevs
      (setq prev (car prevs))
      (setq prevs (cdr prevs))
      (setq completions (all-completions prev collection))
      (setq item (car (member prev completions)))
      (cond
       ((and (null prevs) current)
        )
       ((null item)
        (setq next-applicant all-applicant))
       ((not (string= item prev))
        (setq next-applicant all-applicant))
       ((null next-applicant)
        (setq next-applicant (cdr (fsvn-subcommand-assoc-argument item all-applicant))))
       (t
        ;; completely match then enter node.
        (setq next-applicant (cdr (fsvn-subcommand-assoc-argument item next-applicant)))))
      (cond
       ((null next-applicant)
        (setq collection (fsvn-complete-subcommand-create-completions-toplevel all-applicant)))
       ((eq next-applicant t)
        (cond
         ((and (member item '("--revision" "-r"))
               (or (and (= (length prevs) 1) current)
                   (and (null prevs) (null current))))
          (setq collection 'fsvn-complete-revision-action)
          (setq prevs nil))
         (t
          (setq collection nil))))
       ((atom next-applicant)
        (setq collection nil))
       (t
        (setq collection (fsvn-complete-subcommand-create-completions-toplevel next-applicant)))))
    (cond
     ((and (eq next-applicant all-applicant)
           (or (null current)
               (not (string-match "^-" current))))
      (setq collection 'fsvn-complete-url-action))
     ((and (null current) (null next-applicant))
      (setq collection toplevel)))
    collection))

(defun fsvn-complete-subcommand-create-completions-toplevel (applicant)
  (cond
   ((null applicant)
    nil)
   ((atom applicant)
    nil)
   (t
    (let (ret)
      (mapc
       (lambda (x)
         (let ((opt (car x)))
           (mapc
            (lambda (c)
              (cond
               ((null c))
               (t
                (setq ret (cons (cons c (cdr x)) ret)))))
            ;; car must appear and is long option cdr is short option
            (list (car opt) (cdr opt)))))
       applicant)
      (nreverse ret)))))



(provide 'fsvn-minibuf)

;;; fsvn-minibuf.el ends here
