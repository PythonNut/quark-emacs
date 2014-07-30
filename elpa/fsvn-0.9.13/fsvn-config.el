;;; fsvn-config.el --- Configuration for fsvn.el


;;; History:
;; 

;;; Commentary:
;; 

;;; Code:



(defgroup fsvn-config nil
  "Configuration of `fsvn' group.
This configuration switch by repository root by `fsvn-repository-alist'."
  :group 'fsvn)

(defun fsvn-config-get-value (url key &optional default-value)
  (let ((config (catch 'matched
                  (mapc
                   (lambda (x)
                     (when (and x (string-match (concat "^" (regexp-quote (car x))) url))
                       (throw 'matched x)))
                   fsvn-repository-alist)
                  nil))
        default value)
    (setq key (cond
               ((symbolp key)
                (symbol-name key))
               (t
                key)))
    (setq default (intern (concat "fsvn-config-" key)))
    (or
     (cond
      ((null config)
       (symbol-value default))
      ((setq value (fsvn-config-assoc (cdr config) key))
       (cdr value))
      (t
       (symbol-value default)))
     default-value)))

(defun fsvn-config-assoc (config key)
  (catch 'found
    (mapc
     (lambda (x)
       (cond
        ((and (symbolp (car x))
              (string= (symbol-name (car x)) key))
         (throw 'found x))
        ((and (stringp (car x))
              (string= (car x) key))
         (throw 'found x))))
     config)
    nil))

(defcustom fsvn-repository-alist nil
  "Repository root and any settings.
Key is repository root url.
cdr is any settings.
See variables below.
variable `fsvn-config-browse-show-update'
variable `fsvn-config-magic-remote-commit-message'
variable `fsvn-config-commit-default-file-select-p'
variable `fsvn-config-log-empty-warnings'
variable `fsvn-config-log-limit-count'
variable `fsvn-config-tortoise-property-use'
variable `fsvn-config-repository-default-coding-system'

An example is as follows:

\(setq fsvn-repository-alist
       `(
         (\"svn://localhost\"
          (browse-show-update . t))
        ))

If not matched any settings, then use `fsvn-config-' prefixed variable will be used."
  :group 'fsvn
  :type  '(alist :key-type string
                 ;;todo not correct
                 :value-type
                 (list symbol)))

(defcustom fsvn-config-browse-show-update t
  "Control `fsvn-browse-mode' access to repository or not.
This is usefull for slow repository cause of far network.
This variable overwritten by `fsvn-repository-alist'"
  :group 'fsvn-config
  :type 'boolean)

(defcustom fsvn-config-repository-default-coding-system nil
  "Repository file's default coding system.
nil value means access to repository and guess each file coding-system.
This variable overwritten by `fsvn-repository-alist'"
  :group 'fsvn-config
  :type 'coding-system)

(defcustom fsvn-config-magic-remote-commit-message "Remote commit."
  "When modify repository using fsvn magic interface, automatically set this as log message.
This variable overwritten by `fsvn-repository-alist'"
  :group 'fsvn-config
  :type 'string)

(defcustom fsvn-config-commit-default-file-select-p t
  "When `fsvn-select-file-mode' and `fsvn-log-message-mode' was prepared,
move focus to file select buffer.
This variable overwritten by `fsvn-repository-alist'"
  :group 'fsvn-config
  :type 'boolean)

(defcustom fsvn-config-log-empty-warnings t
  "Warn or not if log message is empty when commit file(s).
This variable overwritten by `fsvn-repository-alist'"
  :group 'fsvn-config
  :type 'boolean)

(defcustom fsvn-config-log-limit-count 50
  "Log entry limit in `fsvn-log-list-mode'
This variable overwritten by `fsvn-repository-alist'"
  :group 'fsvn-config
  :type 'integer)

(defcustom fsvn-config-tortoise-property-use t
  "Use or not TortoiseSVN custom property (bugtraq:* tsvn:*)
This variable overwritten by `fsvn-repository-alist'"
  :group 'fsvn-config
  :type 'boolean)

(defun fsvn-config-browse-show-update (url)
  (fsvn-config-get-value url 'browse-show-update))

(defun fsvn-config-magic-remote-commit-message (url)
  (fsvn-config-get-value url 'magic-remote-commit-message ""))

(defun fsvn-config-commit-default-file-select-p (url)
  (fsvn-config-get-value url 'commit-default-file-select-p))

(defun fsvn-config-log-empty-warnings (url)
  (fsvn-config-get-value url 'log-empty-warnings))

(defun fsvn-config-log-limit-count (url)
  (fsvn-config-get-value url 'log-limit-count))

(defun fsvn-config-tortoise-property-use (url)
  (fsvn-config-get-value url 'tortoise-property-use))

(defun fsvn-config-repository-default-coding-system (url)
  (fsvn-config-get-value url 'repository-default-coding-system))



(provide 'fsvn-config)

;;; fsvn-config.el ends here
