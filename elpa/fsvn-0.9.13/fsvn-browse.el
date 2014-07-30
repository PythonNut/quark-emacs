;;; fsvn-browse.el --- Dired like mode for Subversion working-copy/repository.


;;; History:
;; 

;;; Commentary:
;; 

;;; Code:



(require 'fsvn-mode)
(require 'fsvn-logview)
(require 'fsvn-propview)
(require 'fsvn-select)
(require 'fsvn-popup)
(require 'fsvn-msgedit)
(require 'fsvn-parasite)
(require 'fsvn-cmd)



(defvar transient-mark-mode)



(fsvn-defstruct browse-file
  name directory-p)

(defconst fsvn-browse-ls-dir-status-length 1)
(defconst fsvn-browse-ls-status-column 4)
(defconst fsvn-browse-ls-revision-length 7)
(defconst fsvn-browse-ls-author-length 15)
(defconst fsvn-browse-ls-size-length 7)
(defconst fsvn-browse-ls-dir-status-column
  (+ fsvn-browse-ls-status-column
     fsvn-svn-status-length
     1))
(defconst fsvn-browse-ls-revision-column
  (+ fsvn-browse-ls-dir-status-column
     fsvn-browse-ls-dir-status-length
     1))
(defconst fsvn-browse-ls-author-column
  (+ fsvn-browse-ls-revision-column
     fsvn-browse-ls-revision-length
     1))
(defconst fsvn-browse-ls-size-column
  (+ fsvn-browse-ls-author-column
     fsvn-browse-ls-author-length
     1))
(defconst fsvn-browse-ls-date-column
  (+ fsvn-browse-ls-size-column
     fsvn-browse-ls-size-length
     1))

(defconst fsvn-browse-re-status-ignored "^....I")
(defconst fsvn-browse-re-status-added "^....\\(A\\)")
(defconst fsvn-browse-re-status-modified "^....\\([MDR]\\)")
(defconst fsvn-browse-re-dirstatus-modified "^............\\([MC]\\)")
(defconst fsvn-browse-re-status-conflicted "^....\\([C!]\\)")
(defconst fsvn-browse-re-status-other "^....\\([^.]\\)")
(defconst fsvn-browse-re-mark "^[^ \n]")
(defconst fsvn-browse-re-dir "^..d ")
(defconst fsvn-browse-re-symlink "^..l ")
(defconst fsvn-browse-re-root "^ \\(Root\\): \\(.*\\)")
(defconst fsvn-browse-re-revision "^ \\(Revision\\): \\(.*\\)")
(defconst fsvn-browse-re-subdir "^ \\(Path\\): \\(.*\\)")
(defconst fsvn-browse-re-format-subdir "^ Path: %s$")
(defconst fsvn-browse-re-locked-user
  (format "^.\\{%d\\}+\\(\\[[^]\n]+\\]\\)" fsvn-browse-ls-author-column))

(defconst fsvn-browse-buffer-local-variables
  '(
    (fsvn-browse-subdir-alist)
    (fsvn-browse-repos-p)
    (revert-buffer-function . 'fsvn-browse-revert-buffer)
    (font-lock-defaults . '(fsvn-browse-font-lock-keywords t nil nil beginning-of-line))
    (fsvn-browse-buffer-files-status-process)
    (fsvn-browse-buffer-directories-status-process)
    (fsvn-browse-buffer-cleanup-process)
    (fsvn-buffer-repos-info)
    (fsvn-browse-revision)
    (fsvn-browse-ls-comparer)
    (dired-directory)
    ))

(defconst fsvn-browse-mode-line-process
  '(
    (fsvn-browse-buffer-files-status-process " Checking file status... ")
    (fsvn-browse-buffer-directories-status-process " Checking directory status... ")
    (fsvn-browse-buffer-cleanup-process " Cleaning...")
    ))

(defconst fsvn-browse-ls-comparator-alist
  '(
    (repository fsvn-browse-ls-entry-name-comparer)
    (working-copy fsvn-browse-ls-file-name-comparer fsvn-browse-ls-file-time-comparer)
    ))

(defvar fsvn-browse-ls-comparer nil)

(defvar fsvn-browse-repos-p nil)
(defvar fsvn-browse-subdir-alist nil)
(defvar fsvn-browse-buffer-files-status-process nil)
(defvar fsvn-browse-buffer-directories-status-process nil)
(defvar fsvn-browse-buffer-cleanup-process nil)
(defvar fsvn-browse-file-name-function 'fsvn-browse-point-urlrev) ;; TODO obsolete
(defvar fsvn-browse-revision nil)

(defvar fsvn-browse-font-lock-keywords 
  (list

   ;; Directory headers.
   (list fsvn-browse-re-root '(1 fsvn-header-key-face) '(2 fsvn-header-face))
   (list fsvn-browse-re-revision '(1 fsvn-header-key-face) '(2 fsvn-header-face))

   (list fsvn-browse-re-subdir '(1 fsvn-header-key-face) '(2 fsvn-header-face))

   ;; Fsvn marks.
   (list fsvn-browse-re-mark '(0 fsvn-mark-face))

   ;; We make heavy use of MATCH-ANCHORED, since the regexps don't identify the
   ;; file name itself.  We search for Fsvn defined regexps, and then use the
   ;; Fsvn defined function `fsvn-move-to-filename' before searching for the
   ;; simple regexp ".+".  It is that regexp which matches the file name.
   ;;
   ;; Marked files.
   (list (concat "^[" (char-to-string fsvn-mark-mark-char) "]")
         '(".+" (fsvn-move-to-filename) nil (0 fsvn-marked-face)))

   ;; Flagged files.
   (list (concat "^[" (char-to-string fsvn-mark-delete-char) "]")
         '(".+" (fsvn-move-to-filename) nil (0 fsvn-flagged-face)))

   ;; Locked by other user.
   (list fsvn-browse-re-locked-user '(1 fsvn-warning-face))

   ;; People who are paranoid about security would consider this more
   ;; important than other things such as whether it is a directory.
   ;; But we don't want to encourage paranoia, so our default
   ;; should be what's most useful for non-paranoids. -- rms.
   ;; Subdirectories.
   (list fsvn-browse-re-dir
         '(".+" (fsvn-move-to-filename) nil (0 fsvn-directory-face)))

   (list fsvn-browse-re-symlink
         '(".+" (fsvn-move-to-filename) nil (0 fsvn-symlink-face)))

   (list fsvn-browse-re-status-ignored
         '(".+" (fsvn-move-to-filename) nil (0 fsvn-ignored-face)))

   ;; colorize status
   (list fsvn-browse-re-status-modified '(1 'fsvn-status-modified-face))
   (list fsvn-browse-re-status-added '(1 'fsvn-status-added-face))
   (list fsvn-browse-re-status-conflicted '(1 'fsvn-status-conflicted-face))
   (list fsvn-browse-re-dirstatus-modified '(1 'fsvn-status-modified-face))
   ))

(defvar fsvn-browse-diff-map nil)
(unless fsvn-browse-diff-map
  (setq fsvn-browse-diff-map
        (let ((map (make-sparse-keymap)))
          (suppress-keymap map)
          (define-key map "=" 'fsvn-browse-diff-this)
          (define-key map "l" 'fsvn-browse-diff-local)
          (define-key map "d" 'fsvn-browse-diff-this)
          (define-key map "D" 'fsvn-browse-diff-path)
          (define-key map "e" 'fsvn-browse-ediff-this)
          (define-key map "E" 'fsvn-browse-ediff-path)
          (define-key map "p" 'fsvn-browse-create-patch-selected)
          (define-key map "P" 'fsvn-browse-create-patch-path)
          (define-key map "V" 'fsvn-browse-diff-path-with-previous)
          (define-key map "v" 'fsvn-browse-diff-this-with-previous)
          map)))

(defvar fsvn-browse-often-use-map nil)
(unless fsvn-browse-often-use-map
  (setq fsvn-browse-often-use-map
        (let ((map (make-sparse-keymap)))
          (suppress-keymap map)

          (define-key map "C" 'fsvn-browse-commit-path)
          (define-key map "E" 'fsvn-browse-export-path)
          (define-key map "I" 'fsvn-browse-info-path)
          (define-key map "K" 'fsvn-browse-unlock-selected)
          (define-key map "L" 'fsvn-browse-logview-path)
          (define-key map "P" 'fsvn-browse-propview-path)
          (define-key map "S" 'fsvn-browse-switch-path)
          (define-key map "T" 'fsvn-browse-remove-changelist-selected)
          (define-key map "U" 'fsvn-browse-update-path)
          (define-key map "a" 'fsvn-browse-add-selected)
          (define-key map "b" 'fsvn-browse-blame-this)
          (define-key map "c" 'fsvn-browse-commit-selected)
          (define-key map "d" 'fsvn-browse-delete-selected)
          (define-key map "e" 'fsvn-browse-export-this)
          (define-key map "h" 'fsvn-show-svn-help)
          (define-key map "i" 'fsvn-browse-info-selected)
          (define-key map "k" 'fsvn-browse-lock-selected)
          (define-key map "l" 'fsvn-browse-logview-this)
          (define-key map "m" 'fsvn-browse-magic-head)
          (define-key map "o" 'fsvn-browse-open-repository)
          (define-key map "p" 'fsvn-browse-propview-this)
          (define-key map "t" 'fsvn-browse-add-changelist-selected)
          (define-key map "u" 'fsvn-browse-update-selected)
          ;; Do not use "v" 

          map)))

(defvar fsvn-browse-prefix-map nil)
(unless fsvn-browse-prefix-map
  (setq fsvn-browse-prefix-map
        (let ((map (make-sparse-keymap)))
          (suppress-keymap map)

          (define-key map "!" 'fsvn-command)
          (define-key map "+" 'fsvn-browse-mkdir)
          (define-key map "=" fsvn-browse-diff-map)

          (define-key map "\C-c" 'fsvn-browse-commit-path)
          (define-key map "\C-u" 'fsvn-browse-update-path)
          (define-key map "\C-mI" 'fsvn-browse-mergeinfo-path)
          (define-key map "\C-mi" 'fsvn-browse-mergeinfo-this)
          (define-key map "\C-mM" 'fsvn-browse-merge-path)
          (define-key map "\C-mm" 'fsvn-browse-merge-this)
          (define-key map "\C-p" 'fsvn-browse-propview-this)
          (define-key map "\C-r" 'fsvn-browse-resolved-selected)
          (define-key map "\C-bm" 'fsvn-browse-safe-move-this)
          (define-key map "\C-bc" 'fsvn-browse-safe-copy-this)
          (define-key map "\C-b\C-m" 'fsvn-browse-smart-move-this)
          (define-key map "\C-b\C-c" 'fsvn-browse-smart-copy-this)

          ;; C-v map to fsvn-browse-often-use-map
          (define-key map "\C-vc" 'fsvn-browse-copy-this)
          (define-key map "\C-vm" 'fsvn-browse-move-this)
          (define-key map "\C-v\ec" 'fsvn-browse-cleanup-path)
          (define-key map "\C-vr" 'fsvn-browse-revert-selected)
          (define-key map "\C-vR" 'fsvn-browse-revert-path)
          (define-key map "\C-vp" 'fsvn-browse-create-patch-selected)
          (define-key map "\C-vP" 'fsvn-browse-create-patch-path)
          (define-key map "\C-v\C-p" 'fsvn-browse-patch-path)
          (define-key map "\C-va" 'fsvn-browse-add-selected)
          (define-key map "\C-v\C-c" 'fsvn-browse-copy-selected)
          (define-key map "\C-v\C-d" 'fsvn-browse-delete-selected)
          (define-key map "\C-v\C-m" 'fsvn-browse-move-selected)

          ;;    (define-key map "\C-b\C-m" 'fsvn-browse-safe-move-selected)
          ;;    (define-key map "\C-b\C-c" 'fsvn-browse-safe-copy-selected)
          (define-key map "\ec" 'fsvn-global-cleanup-buffer)
          (define-key map "\ei" 'fsvn-browse-prop-add-svn:ignore-selected)
          (define-key map "\er" 'fsvn-browse-resolve-selected)

          ;; C-c C-v * -> z v *
          (map-keymap
           (lambda (key command)
             ;; only C-v map
             (when (and (fsvn-characterp key) (= key ?\026))
               (define-key fsvn-browse-often-use-map "v" command)))
           map)

          map)))

(defvar fsvn-browse-mode-map nil)
(unless fsvn-browse-mode-map
  (setq fsvn-browse-mode-map
        (let ((map (make-sparse-keymap)))
          (suppress-keymap map)
          (fsvn-readonly-mode-keymap map)

          (define-key map " " 'fsvn-browse-next-file)
          (define-key map "%" (make-sparse-keymap))
          (define-key map "%d" 'fsvn-browse-mark-delete-regexp)
          (define-key map "%m" 'fsvn-browse-mark-file-regexp)
          (define-key map "=" fsvn-browse-diff-map)
          (define-key map "U" 'fsvn-browse-mark-all-unmark)
          (define-key map "\C-c" fsvn-browse-prefix-map)
          (define-key map "\C-m" 'fsvn-browse-file-this)
          (define-key map "\C-n" 'fsvn-browse-next-file)
          (define-key map "\C-p" 'fsvn-browse-previous-file)
          (define-key map "^" 'fsvn-browse-up-directory)
          (define-key map "d" 'fsvn-browse-mark-file-delete)
          (define-key map "g" 'revert-buffer)
          (define-key map "m" 'fsvn-browse-mark-file-mark)
          (define-key map "n" 'fsvn-browse-next-file)
          (define-key map "p" 'fsvn-browse-previous-file)
          (define-key map "s" 'fsvn-browse-toggle-sort)
          (define-key map "u" 'fsvn-browse-mark-file-unmark)
          (define-key map "z" fsvn-browse-often-use-map)

          map)))

(defcustom fsvn-browse-mode-hook nil
  "Run at the very end of `fsvn-browse-mode'."
  :group 'fsvn
  :type 'hook)

(defcustom fsvn-browse-mode-prepared-hook nil
  "Run at the very end of `fsvn-browse-mode' is prepared."
  :group 'fsvn
  :type 'hook)

(defcustom fsvn-browse-before-commit-hook nil
  "Run before prepared file-select-buffer.  Called with a list argument as filenames."
  ;; TODO called one arg then type is not hook?
  :group 'fsvn
  :type 'hook)

(defcustom fsvn-browse-cleanup-buffer t
  "Kill `fsvn-browse-mode' buffer,  when leave.
Effected `fsvn-browse-up-directory' or `fsvn-browse-file-this' to directory."
  :group 'fsvn
  :type 'boolean)

;; * fsvn-browse-mode internal function

(defun fsvn-browse-mode ()
  "Major mode for browsing Subversion working copy and repository.

Entry to this mode calls the value of `fsvn-browse-mode-hook'.

Keybindings:
\\{fsvn-browse-mode-map}"
  (fsvn-global-initialize-mode)
  (use-local-map fsvn-browse-mode-map)
  (setq major-mode 'fsvn-browse-mode)
  (setq mode-name "Fsvn Browse")
  (setq truncate-lines t)
  (setq buffer-undo-list t)
  (fsvn-make-buffer-variables fsvn-browse-buffer-local-variables)
  (fsvn-browse-setup-mode-line)
  (run-mode-hooks 'fsvn-browse-mode-hook))

(defconst fsvn-browse-dired-confirm-alist
  '(
    (fsvn-unsaved " *fsvn Unsaved*" "Save %s ")
    (fsvn-browse-update-selected " *svn Update*" "Update %s ")
    (fsvn-browse-resolved-selected " *svn Resolved*" "Resolved %s ")
    (fsvn-browse-add-selected " *svn Add*" "Add %s ")
    (fsvn-browse-delete-selected " *svn Delete*" "Delete %s ")
    (fsvn-browse-lock-selected " *svn Lock*" "Lock %s ")
    (fsvn-browse-unlock-selected " *svn Unock*" "Unlock %s ")
    (fsvn-browse-add-changelist-selected " *svn Add Changelist*" "Add Changelist %s ")
    (fsvn-browse-remove-changelist-selected " *svn Remove Changelist*" "Remove Changelist %s ")
    (fsvn-browse-revert-selected " *svn Revert*" "Revert %s ")
    (fsvn-browse-prop-add-svn:ignore-selected " *svn Add to svn:ignore*" "Ignore %s ")
    (fsvn-browse-prop-toggle-svn:needs-lock-selected " *svn Toggle svn:needs-lock*" "Needs-Lock %s ")
    (fsvn-browse-move-selected " *svn Move*" "Move to: ")
    (fsvn-browse-copy-selected " *svn Copy*" "Copy to: ")
    (fsvn-browse-export-this " *svn Export*" "Export %s")
    (fsvn-browse-svn:externals-selected " *svn Externals*" "Externals Property from %s ")
    (load " *Load*" "Load %s ")
    (byte-compile " *Byte-Compile*" "Byte-compile %s ")
    (delete " *Deletions*" "Delete %s ")
    )
  "KEY BUF-NAME PROMPT.")

(defun fsvn-browse-dired-confirm (objects op-symbol &optional confirmer)
  (let (file-list buf-name prompt message)
    (setq file-list
          (mapcar 
           (lambda (x)
             (cond
              ((bufferp x)
               (buffer-name x))
              ((stringp x)
               (fsvn-file-name-nondirectory x))
              (t
               (error "Not supported type"))))
           objects))
    (setq message (cdr (assq op-symbol fsvn-browse-dired-confirm-alist)))
    (setq buf-name (nth 0 message)
          prompt (nth 1 message))
    (dired-mark-pop-up
     buf-name op-symbol file-list 
     `(lambda (prompt) (let ((default-directory ,default-directory))
                         (funcall ',(or confirmer dired-deletion-confirmer) prompt)))
     (format prompt (dired-mark-prompt t file-list)))))

(defun fsvn-browse-status-draw-status (target=cl)
  ;; todo cl not have path
  (let* ((path (fsvn-xml-status->target.path target=cl))
         entries)
    (fsvn-save-browse-directory-excursion path
      (setq entries (fsvn-xml-status->target&cl->entries target=cl))
      (mapc
       (lambda (entry)
         (fsvn-browse-draw-status-internal entry))
       entries))))

(defun fsvn-browse-status-process-sentinel (proc event)
  (fsvn-process-exit-handler proc event
    (fsvn-browse-process-unlock proc)
    (when (= (process-exit-status proc) 0)
      (mapc
       (lambda (target)
         (fsvn-browse-status-draw-status target))
       (fsvn-xml-parse-status)))
    (kill-buffer (current-buffer))))

(defun fsvn-browse-status-process-filter (proc event)
  (fsvn-process-event-handler proc event
    (fsvn-debug event)
    (goto-char (point-max))
    (insert event)))

(defun fsvn-browse-ls-insert-wc-directory (directory)
  (let* (file-entries status-entries status-entry entries)
    (setq file-entries (fsvn-browse-ls-directory-files directory))
    (setq status-entries (fsvn-get-directory-files-status directory))
    (setq entries (fsvn-browse-ls-merge-wc-entries directory file-entries status-entries))
    (fsvn-browse-draw-path directory)
    (mapc
     (lambda (entry)
       (fsvn-browse-ls-insert-wc-entry (car entry) (cdr entry)))
     entries)))

(defun fsvn-browse-ls-merge-wc-entries (directory file-entries status-entries)
  (let (status-hash key ret status path)
    ;; create hash for a lot of files.
    ;;   3000 files 82sec -> 13sec.
    (setq status-hash (make-hash-table :test 'equal))
    (mapc
     (lambda (status-entry)
       (setq key (fsvn-file-absolute-name (fsvn-xml-status->target->entry.path status-entry)))
       (puthash key status-entry status-hash))
     status-entries)
    (mapc
     (lambda (file)
       (setq key (fsvn-file-absolute-name file))
       (setq status (gethash key status-hash))
       (setq ret (cons (cons file status) ret)))
     file-entries)
    (mapc
     (lambda (status-entry)
       (setq path (fsvn-xml-status->target->entry.path status-entry))
       (unless (or (fsvn-file= path directory) (fsvn-file-assoc path ret))
         (setq ret (cons (cons path status-entry) ret))))
     status-entries)
    (sort ret fsvn-browse-ls-comparer)))

(defun fsvn-browse-ls-entry-name-comparer (ent1 ent2)
  (string-lessp
   (fsvn-xml-lists->list->entry=>name$ ent1)
   (fsvn-xml-lists->list->entry=>name$ ent2)))

(defun fsvn-browse-ls-file-name-comparer (file1 file2)
  (string-lessp
   (if (stringp file1) file1 (car file1))
   (if (stringp file2) file2 (car file2))))

(defun fsvn-browse-ls-file-time-comparer (file1 file2)
  (let ((attr1 (file-attributes (if (stringp file1) file1 (car file1))))
        (attr2 (file-attributes (if (stringp file2) file2 (car file2)))))
    (cond
     ((and attr1 attr2)
      (not (time-less-p (nth 5 attr1) (nth 5 attr2))))
     ((and attr1 (null attr2))
      t)
     ((and (null attr1) attr2)
      nil)
     (t
      nil))))

(defun fsvn-browse-ls-entry-time-comparer (ent1 ent2)
  (not (time-less-p
        (fsvn-xml-lists->list->entry=>commit=>date$ ent1)
        (fsvn-xml-lists->list->entry=>commit=>date$ ent2))))

(defun fsvn-browse-ls-directory-files (directory)
  (let ((files (directory-files directory nil))
        ret)
    (mapc
     (lambda (file)
       (cond
        ((string= file (fsvn-meta-dir-name)))
        ((string= ".." file))
        ((string= "." file)
         (setq ret (cons (concat (file-name-as-directory directory) "." ) ret)))
        (t
         (setq ret (cons (fsvn-expand-file file directory) ret)))))
     files)
    ret))

(defun fsvn-browse-directory-files (directory &optional full match)
  (let ((files (directory-files directory nil (or match dired-re-no-dot)))
        (filename-func (if full 
                           (lambda (name) (fsvn-expand-file name directory))
                         'identity))
        ret)
    (mapc
     (lambda (file)
       (unless (or (string= file (fsvn-meta-dir-name)))
         (setq ret (cons (funcall filename-func file) ret))))
     files)
    (nreverse ret)))

(defun fsvn-browse-redraw-wc-file-entry (file)
  (let ((dir (fsvn-file-name-directory2 file))
        (status-entry (fsvn-get-file-status file)))
    (fsvn-save-browse-directory-excursion dir
      (cond
       ((null status-entry)
        (fsvn-browse-remove-wc-file-entry-internal file))
       ((fsvn-browse-goto-file file)
        (fsvn-browse-draw-status-this-line status-entry))
       (t
        (fsvn-browse-add-wc-file-entry file status-entry))))))

(defun fsvn-browse-add-wc-file-entry (file &optional status)
  "Add working copy file entry.
FILE
STATUS `t' means force to add entry."
  (let ((dir (fsvn-file-name-directory2 file))
        (status-entry (or status (fsvn-get-file-status file)))
        (filename (fsvn-file-name-nondirectory file)))
    (fsvn-save-browse-directory-excursion dir
      (when (and status-entry
                 (not (fsvn-browse-goto-file file)))
        (when (eq status-entry t)
          (setq status-entry nil))
        (fsvn-browse-add-wc-raw-entry dir filename file status-entry)
        nil))))

(defun fsvn-browse-add-file-entry (file)
  (save-excursion
    (unless (fsvn-current-filename)
      (unless (fsvn-browse-goto-first-file)
        (goto-char (point-max))))
    (forward-line 0)
    (let (buffer-read-only)
      (fsvn-browse-ls-insert-wc-entry file))
    (fsvn-move-to-filename)
    (set-buffer-modified-p nil)))

(defun fsvn-browse-add-wc-raw-entry (dir filename file &optional status-entry)
  (catch 'done
    (fsvn-browse-each-file f dir
      (unless (funcall fsvn-browse-ls-comparer f filename)
        (let (buffer-read-only)
          (forward-line 0)
          (fsvn-browse-ls-insert-wc-entry file status-entry))
        (throw 'done t)))))

(defun fsvn-browse-remove-wc-file-entry (file)
  (let ((dir (fsvn-file-name-directory2 file)))
    (fsvn-save-browse-directory-excursion dir
      (fsvn-browse-remove-wc-file-entry-internal file))))

(defun fsvn-browse-remove-wc-file-entry-internal (file)
  (save-excursion
    (when (fsvn-browse-goto-file file)
      (fsvn-browse-remove-current-entry))))

(defun fsvn-browse-remove-current-entry ()
  (let (buffer-read-only)
    (delete-region (line-beginning-position) (line-beginning-position 2))))

(defun fsvn-browse-ls-insert-repos-entry (entry)
  (let ((dirp (eq (fsvn-xml-lists->list->entry.kind entry) 'dir))
        (filename (fsvn-xml-lists->list->entry=>name$ entry)))
    (fsvn-set-filename-property filename)
    (insert (format "  %c %s %s %s %s %s\n"
                    (if dirp ?d fsvn-space-char)
                    (fsvn-browse-ls-revision entry)
                    (fsvn-browse-ls-author-column entry)
                    (fsvn-generic-format-file-size 
                     (fsvn-safe-xml-lists->list->entry=>size$ entry) fsvn-browse-ls-size-length)
                    (format-time-string fsvn-generic-datetime-format 
                                        (fsvn-xml-lists->list->entry=>commit=>date$ entry))
                    filename))))

(defun fsvn-browse-ls-insert-wc-entry (filename &optional status-entry)
  (let* ((file-entry (fsvn-create-local-file-entry filename))
         (dirp (fsvn-local-file.directory-p file-entry))
         (linkp (fsvn-local-file.symlink-p file-entry))
         (filename (fsvn-local-file.name file-entry))
         size time)
    (setq size
          (or (fsvn-local-file.size file-entry)
              0))
    (setq time
          (or (fsvn-local-file.modified-time file-entry)
              (fsvn-xml-status->target->entry=>wc-status=>commit=>date$ status-entry)))
    (fsvn-set-filename-property filename)
    (insert (format "  %c %s %s %s %s %s %s %s%s\n"
                    (cond (dirp ?d) (linkp ?l) (t fsvn-space-char))
                    (fsvn-status-get-status status-entry)
                    "."
                    (fsvn-browse-status-revision status-entry)
                    (fsvn-browse-status-wc-author-column status-entry)
                    (fsvn-generic-format-file-size size fsvn-browse-ls-size-length)
                    (format-time-string fsvn-generic-datetime-format time)
                    filename
                    (fsvn-ui-symlink-trailer linkp)))))

(defun fsvn-browse-ls-revision (entry)
  (fsvn-string-lpad 
   (number-to-string (fsvn-xml-lists->list->entry=>commit.revision entry))
   fsvn-browse-ls-revision-length))

(defun fsvn-browse-status-revision (entry)
  (let ((rev (fsvn-xml-status->target->entry=>wc-status=>commit.revision entry)))
    (fsvn-string-lpad (if (numberp rev) (number-to-string rev) rev)
                      fsvn-browse-ls-revision-length)))

(defun fsvn-browse-ls-author-string (author locker)
  (fsvn-string-rpad
   (if locker
       (concat "[" locker "]")
     author)
   fsvn-browse-ls-author-length))

(defun fsvn-browse-ls-author-column (entry)
  (fsvn-browse-ls-author-string (fsvn-xml-lists->list->entry=>commit=>author$ entry)
                           (fsvn-xml-lists->list->entry=>lock=>owner$ entry)))

(defun fsvn-browse-status-wc-author-column (entry)
  (fsvn-browse-ls-author-string (fsvn-xml-status->target->entry=>wc-status=>commit=>author$ entry)
                           (fsvn-xml-status->target->entry=>wc-status=>lock=>owner$ entry)))

(defun fsvn-browse-status-repos-author-column (entry)
  (fsvn-browse-ls-author-string (fsvn-xml-status->target->entry=>wc-status=>commit=>author$ entry)
                           (fsvn-xml-status->target->entry=>repos-status=>lock=>owner$ entry)))

(defun fsvn-browse-status-author-column (entry)
  (if (fsvn-xml-status->target->entry=>repos-status entry)
      (fsvn-browse-status-repos-author-column entry)
    (fsvn-browse-status-wc-author-column entry)))

(defmacro fsvn-browse-wc-only (&rest form)
  `(if fsvn-browse-repos-p
       (message "this command only executable in working copy.")
     ,@form))

(defmacro fsvn-browse-repos-only (&rest form)
  `(if (not fsvn-browse-repos-p)
       (message "this command only executable in repository.")
     ,@form))

(defmacro fsvn-browse-each-file (var path &rest form)
  "Move point and execute FORM.
VAR set `fsvn-current-filename'
PATH is each executed path."
  (declare (indent 2))
  `(save-excursion
     (let (,var RET)
       (when (fsvn-browse-goto-directory (or ,path (fsvn-browse-current-path)))
         (when (fsvn-browse-goto-first-file)
           (while (setq ,var (fsvn-current-filename))
             (setq RET (cons (progn ,@form) RET))
             (fsvn-browse-next-file))
           (nreverse RET))))))

(defmacro fsvn-browse-mark-if (predicate msg)
  `(let ((COUNT 0))
     (save-excursion
       (goto-char (point-min))
       (while (not (eobp))
         (when ,predicate
           (fsvn-browse-put-mark-point fsvn-mark-mark-char))
         (forward-line 1)))
     (if ,msg (message "%s %s%s %s%s."
                       COUNT
                       ,msg
                       (dired-plural-s COUNT)
                       (if (eq fsvn-mark-mark-char fsvn-space-char) "un" "")
                       "marked"))))

(defun fsvn-browse-directories ()
  (apply 'append
         (fsvn-mapitem
          (lambda (b)
            (with-current-buffer b
              (when (eq major-mode 'fsvn-browse-mode)
                (mapcar 'car fsvn-browse-subdir-alist))))
          (buffer-list))))

(defun fsvn-browse-process-locked-p ()
  fsvn-browse-buffer-files-status-process)
(make-obsolete 'fsvn-browse-process-locked-p nil nil)

(defun fsvn-browse-process-lock (proc)
  (setq fsvn-browse-buffer-files-status-process proc))

(defun fsvn-browse-process-unlock (proc)
  (let ((buffer (fsvn-find-buffer-by-variable 'fsvn-browse-buffer-files-status-process proc)))
    (when buffer
      (with-current-buffer buffer
        (setq fsvn-browse-buffer-files-status-process nil)))))

(defun fsvn-browse-status (info directory)
  "non recurse status check."
  (let (proc args)
    (setq args (list "--xml" "--no-ignore" "--non-recursive" "--non-interactive"))
    (when (fsvn-config-browse-show-update (fsvn-xml-info->entry=>repository=>root$ info))
      (setq args (append args (list "--show-updates"))))
    (setq args (append args (list directory)))
    (setq proc (fsvn-start-command "status"
                                   (fsvn-make-temp-buffer)
                                   args))
    (set-process-sentinel proc 'fsvn-browse-status-process-sentinel)
    (set-process-filter proc 'fsvn-browse-status-process-filter)
    (fsvn-browse-process-lock proc)))

(defun fsvn-browse-cleanup-process-sentinel (proc event)
  (fsvn-process-exit-handler proc event
    (let ((buffer (process-get proc 'fsvn-browse-cleaning-buffer)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (setq fsvn-browse-buffer-cleanup-process nil))))
    (if (= (process-exit-status proc) 0)
        (message "Cleanup process finished.")
      (message "Cleanup process failed."))
    (kill-buffer (current-buffer))))

(defun fsvn-smart-move/copy-file-alist (src-file dest-file)
  (let ((prefix (fsvn-file-name-changed-prefix src-file dest-file))
        (src-dir (fsvn-file-name-directory src-file))
        (dest-dir (fsvn-file-name-directory dest-file))
        regexp src-files)
    (setq src-files 
          (fsvn-mapitem
           (lambda (file)
             (when (fsvn-deps-file-registered-p file)
               file))
           (directory-files src-dir t (concat "^" (regexp-quote (car prefix))))))
    (setq regexp (concat "^" (regexp-quote (car prefix)) "\\(.*\\)$"))
    (mapcar
     (lambda (src-file)
       (let ((src-name (fsvn-file-name-nondirectory src-file))
             dest-name)
         (unless (string-match regexp src-name)
           (error "File name `%s' is not matched" src-file))
         (setq dest-name (concat (cdr prefix) (match-string 1 src-name)))
         (cons src-file (fsvn-expand-file dest-name dest-dir))))
     src-files)))

(defun fsvn-browse-setup-mode-line ()
  (or (assq 'fsvn-browse-buffer-files-status-process mode-line-process)
      (setq mode-line-process
            (append fsvn-browse-mode-line-process mode-line-process))))

(defun fsvn-browse-check-unsave-buffer (files)
  (let ((unsaved (fsvn-files-unsaved-buffers files)))
    (when (and unsaved (fsvn-browse-dired-confirm unsaved 'fsvn-unsaved))
      (mapc
       (lambda (buffer)
         (with-current-buffer buffer
           (save-buffer)))
       unsaved))))

(defun fsvn-browse-goto-directory (directory)
  (let ((saved (point)))
    (goto-char (point-min))
    (if (re-search-forward (format fsvn-browse-re-format-subdir (regexp-quote directory)) nil t)
        (progn
          (forward-line 0)
          (forward-char 1)
          (point))
      (goto-char saved)
      nil)))

(defun fsvn-browse-goto-file (file)
  (let ((filename (fsvn-urlrev-filename file))
        point)
    (setq point (catch 'found
                  (fsvn-browse-each-file f nil
                    (when (fsvn-file= f filename)
                      (throw 'found (point))))
                  nil))
    (when point
      (goto-char point))))

(defun fsvn-browse-goto-first-file ()
  (let ((saved (point)))
    (goto-char (point-min))
    (cond
     ((re-search-forward fsvn-browse-re-subdir nil t)
      (forward-line 2)
      (or (fsvn-move-to-filename)
          (and (goto-char saved)
               nil)))
     (t (goto-char saved)))))

(defun fsvn-browse-move-to-dir-status ()
  (forward-line 0)
  (forward-char fsvn-browse-ls-dir-status-column))

(defun fsvn-browse-draw-dir-status-this-line (&optional mark)
  (fsvn-browse-move-to-dir-status)
  (delete-char 1)
  (insert (or mark ?.)))

(defun fsvn-browse-put-dir-status-current (&optional mark)
  (let (buffer-read-only)
    (fsvn-browse-draw-dir-status-this-line mark)
    (set-buffer-modified-p nil)))

(defun fsvn-browse-get-dir-status-current ()
  (char-after))

(defun fsvn-browse-draw-dir-status (dir &optional mark)
  (save-excursion
    (when (fsvn-browse-goto-file dir)
      (fsvn-browse-move-to-dir-status)
      (let ((current (char-after)))
        (when (fsvn-status-dir-status-stronger-than-p mark current)
          (fsvn-browse-put-dir-status-current mark))))))

(defun fsvn-browse-put-status-if-weak-1 (file mark)
  (fsvn-browse-put-status-if-weak-internal file mark 0))

(defun fsvn-browse-put-status-if-weak-2 (file mark)
  (fsvn-browse-put-status-if-weak-internal file mark 1))

(defun fsvn-browse-put-status-if-weak-internal (file mark column)
  (let ((curr (fsvn-browse-get-status-internal file column)))
    (when (and curr (or (= curr ?.) (fsvn-status-file-status-stronger-than-p mark curr)))
      (fsvn-browse-put-status-internal file mark column))))

(defun fsvn-browse-put-status-1 (file mark)
  (fsvn-browse-put-status-internal file mark 0))

(defun fsvn-browse-put-status-2 (file mark)
  (fsvn-browse-put-status-internal file mark 1))

(defun fsvn-browse-put-status-3 (file mark)
  (fsvn-browse-put-status-internal file mark 2))

(defun fsvn-browse-put-status-4 (file mark)
  (fsvn-browse-put-status-internal file mark 3))

(defun fsvn-browse-put-status-5 (file mark)
  (fsvn-browse-put-status-internal file mark 4))

(defun fsvn-browse-put-status-6 (file mark)
  (fsvn-browse-put-status-internal file mark 5))

(defmacro fsvn-browse-with-move-status (file column &rest form)
  (declare (indent 2))
  `(save-excursion
     (when (fsvn-browse-goto-file ,file)
       (forward-line 0)
       (forward-char (+ fsvn-browse-ls-status-column ,column))
       ,@form)))

(defun fsvn-browse-put-status-internal (file mark column)
  (fsvn-browse-with-move-status file column
    (let (buffer-read-only)
      (save-excursion
        (delete-char 1)
        (insert mark)
        (set-buffer-modified-p nil)))))

(defun fsvn-browse-get-status-internal (file column)
  (fsvn-browse-with-move-status file column
    (char-after)))

(defun fsvn-browse-draw-status-internal (entry)
  (save-excursion
    (when (fsvn-browse-goto-file (fsvn-xml-status->target->entry.path entry))
      (fsvn-browse-draw-status-this-line entry))))

(defun fsvn-browse-draw-file-status (file)
  (let ((status (fsvn-get-file-status file)))
    (when status
      (fsvn-browse-draw-status-internal status))))

(defun fsvn-browse-draw-status-string-this-line (status &optional author)
  (forward-line 0)
  ;; goto status first column
  (forward-char fsvn-browse-ls-status-column)
  (delete-char fsvn-svn-status-length)
  (insert status)
  (when author
    (fsvn-browse-draw-author-this-line author)))

(defun fsvn-browse-draw-status-this-line (&optional status-entry)
  (let (buffer-read-only)
    (let (status author)
      (when status-entry
        (setq status  (fsvn-status-get-status status-entry))
        (setq author (fsvn-browse-status-author-column status-entry)))
      (setq status (or status (make-string fsvn-svn-status-length ?.)))
      (fsvn-browse-draw-status-string-this-line status author)
      (set-buffer-modified-p nil))))

(defun fsvn-browse-draw-attr-this-line ()
  (let* ((attr (file-attributes (fsvn-browse-point-local-filename)))
         (size (nth 7 attr))
         (time (nth 5 attr))
         buffer-read-only start end)
    (forward-line 0)
    ;; goto status first column
    (forward-char fsvn-browse-ls-size-column)
    (setq start (point)
          end (1- (next-single-property-change (point) 'fsvn-filename nil (line-end-position))))
    (delete-region start end)
    (fsvn-generic-format-file-size size fsvn-browse-ls-size-length)
    (insert (fsvn-generic-format-file-size size fsvn-browse-ls-size-length) " "
            (format-time-string fsvn-generic-datetime-format time))
    (set-buffer-modified-p nil)))

(defun fsvn-browse-draw-author-this-line (user-string)
  (let (buffer-read-only)
    (forward-line 0)
    (forward-char fsvn-browse-ls-author-column)
    (delete-char fsvn-browse-ls-author-length)
    (insert user-string)))

(defun fsvn-browse-status-string-to-display-status (status-string)
  ;;TODO more fast 
  (fsvn-string-rpad (replace-regexp-in-string " " "." status-string) fsvn-svn-status-length ?.))

(defun fsvn-browse-clear-status (file)
  (when (fsvn-browse-goto-file file)
    (fsvn-browse-draw-status-this-line)))

(defun fsvn-browse-put-mark (mark-char file)
  (save-excursion
    (when (fsvn-browse-goto-file file)
      (fsvn-browse-put-mark-point mark-char))))

(defun fsvn-browse-put-mark-point (mark-char)
  (save-excursion
    (forward-line 0)
    (let ((buffer-read-only))
      (delete-char 1)
      (insert (or mark-char fsvn-space-char))
      (buffer-modified-p nil))))

(defun fsvn-browse-put-mark-current-file (mark)
  (cond
   ((fsvn-current-filename)
    (fsvn-browse-put-mark-point mark)
    (fsvn-browse-next-file))
   (t
    (error "Cannot put mark on this line."))))

(defun fsvn-browse-switch-directory-buffer (directory-urlrev &optional revert goto-file)
  (if (fsvn-url-local-p directory-urlrev)
      (fsvn-browse-draw-local-directory directory-urlrev revert)
    (fsvn-browse-draw-repos-directory directory-urlrev revert goto-file))
  (set-visited-file-modtime (current-time))
  (setq buffer-read-only t)
  (switch-to-buffer (current-buffer))
  (run-hooks 'fsvn-browse-mode-prepared-hook))

(defun fsvn-browse-draw-repos-directory (directory-urlrev &optional type goto-file)
  (let (buffer comparer)
    (set-buffer
     (cond
      ((eq type 'revert)
       (current-buffer))
      ((setq buffer (fsvn-get-exists-browse-buffer directory-urlrev))
       buffer)
      (t
       (setq type 'draw)
       (fsvn-browse-create-repos-buffer directory-urlrev))))
    (setq comparer fsvn-browse-ls-comparer)
    (when (memq type '(revert draw))
      (let (buffer-read-only)
        (erase-buffer)
        (fsvn-browse-mode)
        (fsvn-browse-repos-start-info-process directory-urlrev)
        (setq fsvn-browse-ls-comparer 
              (or
               (and comparer 
                    (memq comparer 
                          (cdr (assq 'repository fsvn-browse-ls-comparator-alist)))
                    comparer)
               'fsvn-browse-ls-entry-name-comparer))
        (insert (format " Revision: \n"))
        (insert (format " Root: \n"))
        (insert (format " Path: \n"))
        (insert (format "\n"))
        (fsvn-browse-repos-start-process directory-urlrev goto-file)
        (set-buffer-modified-p nil)))))

(defun fsvn-browse-draw-local-directory (directory &optional type)
  (let ((parent-info (fsvn-browse-working-copy-info directory))
        buffer comparer)
    (unless parent-info
      (error "Cannot draw `%s'" directory))
    (set-buffer
     (cond
      ((eq type 'revert)
       (current-buffer))
      ((setq buffer (fsvn-get-exists-browse-buffer directory))
       buffer)
      (t
       (setq type 'draw)
       (fsvn-browse-create-local-buffer directory))))
    (setq comparer fsvn-browse-ls-comparer)
    (cond
     ((memq type '(revert draw))
      (let (buffer-read-only)
        (erase-buffer)
        (fsvn-browse-mode)
        (setq fsvn-browse-ls-comparer (or comparer 'fsvn-browse-ls-file-name-comparer))
        (fsvn-browse-set-wc-local-variables parent-info directory)
        (fsvn-browse-draw-topmost-header parent-info)
        (fsvn-browse-ls-insert-wc-directory directory)
        (set-buffer-modified-p nil))
      (fsvn-browse-set-wc-directory (file-name-as-directory directory))
      (when (fsvn-directory-versioned-p directory)
        (fsvn-browse-status parent-info directory)
        (fsvn-run-recursive-status directory))
      (fsvn-browse-goto-first-file))
     ;;TODO not works fine
     ;; ((dired-directory-changed-p directory)
     ;;  (message "%s"
     ;;                (substitute-command-keys
     ;;                 "Directory has changed on disk; type \\[revert-buffer] to update Dired")))
     )))

(defun fsvn-browse-working-copy-info (directory)
  "Get svn info DIRECTORY or any parent versioned directory.
This implements consider svn:ignored directory."
  (let ((target
         (if (fsvn-directory-versioned-p directory)
             directory
           (fsvn-find-parent-working-copy directory))))
    (and target
         (fsvn-get-info-entry target t))))

(defun fsvn-browse-draw-path (directory)
  (goto-char (point-min))
  (cond
   ((or (re-search-forward (format fsvn-browse-re-format-subdir directory) nil t)
        ;; this is considering repository buffer
        (re-search-forward (format fsvn-browse-re-format-subdir ".*") nil t))
    (replace-match "")
    ;;delete newline
    (delete-char 1)
    (when (looking-at "^[ ]*\n")
      (replace-match "")))
   (t
    (goto-char (point-max))))
  (insert (format " Path: %s\n" (fsvn-url-decode-string directory)))
  (insert "\n"))

(defun fsvn-browse-draw-topmost-header (info)
  (save-excursion
    (let (revision root)
      (goto-char (point-min))
      (when (re-search-forward fsvn-browse-re-revision nil t)
        (replace-match "")
        (delete-char 1))
      (when (re-search-forward fsvn-browse-re-root nil t)
        (replace-match "")
        (delete-char 1)
        (when (looking-at "^[ ]*\n")
          (replace-match "")))
      (goto-char (point-min))
      (setq revision (fsvn-xml-info->entry.revision info))
      (setq root (fsvn-xml-info->entry=>repository=>root$ info))
      (insert (format " Revision: %d\n" revision))
      (insert (format " Root: %s\n" root)))))

(defun fsvn-browse-set-wc-directory (dir)
  (fsvn-set-default-directory dir)
  (setq dired-directory default-directory)
  (set (make-local-variable 'list-buffers-directory) default-directory))

(defun fsvn-browse-set-repos-directory (info)
  (let* ((url (fsvn-xml-info->entry=>url$ info))
         (rev (fsvn-xml-info->entry.revision info))
         (urlrev (fsvn-url-urlrev url rev)))
    (fsvn-set-default-directory (fsvn-magic-create-name urlrev))
    ;;   (set (make-local-variable 'list-buffers-directory) default-directory)
    (setq dired-directory default-directory)))

(defun fsvn-browse-set-repos-local-variables (info)
  (setq fsvn-buffer-repos-info (fsvn-buffer-xml-info->repos-info info))
  (setq fsvn-browse-revision (fsvn-xml-info->entry.revision info))
  (setq fsvn-browse-repos-p t)
  (let* ((subdir (fsvn-browse-subdir (fsvn-info-repos-path info) t)))
    (fsvn-browse-subdir!info subdir info)))

(defun fsvn-browse-set-wc-local-variables (info directory)
  (setq fsvn-buffer-repos-info (fsvn-buffer-xml-info->repos-info info))
  (setq fsvn-browse-revision (fsvn-xml-info->entry.revision info))
  (setq fsvn-browse-repos-p nil)
  (let* ((subdir (fsvn-browse-subdir directory nil)))
    (fsvn-browse-subdir!info subdir info)))

(defun fsvn-browse-create-repos-buffer (urlrev)
  (generate-new-buffer (fsvn-urlrev-url urlrev)))

(defun fsvn-browse-create-local-buffer (directory)
  (generate-new-buffer (fsvn-file-name-nondirectory directory)))

(defmacro fsvn-browse-subdir.setter (container key value)
  `(let (CELL)
     (unless (setq CELL (assq ,key (cdr (symbol-value ,container))))
       (setq CELL (cons ,key nil))
       (nconc (symbol-value ,container) (list CELL)))
     (setcdr CELL ,value)))

(defmacro fsvn-browse-subdir.getter (container key)
  `(cdr (assq ,key (cdr (symbol-value ,container)))))

(defun fsvn-browse-subdir.info (subdir)
  (fsvn-browse-subdir.getter 'subdir 'info))

(defun fsvn-browse-subdir!info (subdir data)
  (fsvn-browse-subdir.setter 'subdir 'info data))

(defun fsvn-browse-subdir-directory-url (subdir)
  (let ((info (fsvn-browse-subdir.info subdir)))
    (if (not fsvn-browse-repos-p)
        (car subdir)
      (fsvn-url-urlrev
       (fsvn-xml-info->entry=>url$ info)
       (fsvn-xml-info->entry.revision info)))))

(defun fsvn-browse-subdir (path repos-p)
  (let* ((canon (directory-file-name path))
         (key (if repos-p canon (fsvn-expand-file canon)))
         ;;todo case fold
         (subdir (fsvn-string-assoc key fsvn-browse-subdir-alist)))
    (when (string= key "")
      (setq key "/"))
    (unless subdir
      (setq subdir (cons key nil))
      (setq fsvn-browse-subdir-alist
            (cons subdir fsvn-browse-subdir-alist)))
    subdir))

(defun fsvn-browse-gather-selected-files ()
  "If region not activate, get current line's filename."
  (let (temp)
    (cond
     ((and transient-mark-mode mark-active)
      (fsvn-browse-gather-region-files))
     ((setq temp (fsvn-browse-gather-marked-files))
      temp)
     ((setq temp (funcall fsvn-browse-file-name-function))
      (list temp))
     (t
      nil))))

(defun fsvn-browse-mark-matched-file (regexp mark)
  (let ((count 0))
    (fsvn-browse-each-file file nil
      (when (string-match regexp file)
        (fsvn-browse-put-mark-point mark)
        (setq count (1+ count))))
    (message (if (= count 1) "1 file marked."
               "%d files marked") count)))

(defun fsvn-browse-point-marked-p (&optional mark)
  (save-excursion
    (forward-line 0)
    (looking-at (concat "^" (char-to-string (or mark fsvn-mark-mark-char))))))

(defun fsvn-browse-gather-marked-files (&optional mark)
  (let* ((marker-char (or mark fsvn-mark-mark-char))
         (regex (concat "^" (regexp-quote (char-to-string marker-char))))
         ret temp)
    (save-excursion
      (when (fsvn-browse-goto-first-file)
        (forward-line 0)
        (while (and (not (eobp))
                    (setq temp (funcall fsvn-browse-file-name-function)))
          (when (looking-at regex)
            (setq ret (cons temp ret)))
          (forward-line 1))
        (nreverse ret)))))

(defun fsvn-browse-gather-region-files ()
  (let ((start (region-beginning))
        (end (region-end))
        temp ret)
    (save-excursion
      (goto-char start)
      (while (and (>= end (point))
                  (fsvn-move-to-filename))
        (when (setq temp (funcall fsvn-browse-file-name-function))
          (setq ret (cons temp ret)))
        (forward-line 1)))
    (nreverse ret)))

(defun fsvn-browse-propview-mode (file directory-p)
  (let ((info fsvn-buffer-repos-info)
        (working-dir
         (if (fsvn-url-local-p file)
             (fsvn-browse-current-directory-url)
           (fsvn-browse-current-magic-directory))))
    (fsvn-open-propview-mode info file directory-p working-dir)))

(defun fsvn-browse-revert-buffer (ignore-auto noconfirm)
  (let ((file (fsvn-current-filename))
        (dir (fsvn-browse-current-directory-url))
        ;;FIXME when multiple subdir and D mark
        (marked (fsvn-browse-gather-marked-files))
        (opoint (point)))
    (mapc
     (lambda (subdir)
       (let* ((dir (car subdir))
              (url (fsvn-browse-subdir-directory-url subdir)))
         (fsvn-browse-goto-directory dir)
         (fsvn-browse-switch-directory-buffer url 'revert file)
         (mapc
          (lambda (file)
            (fsvn-browse-put-mark fsvn-mark-mark-char file))
          marked)))
     fsvn-browse-subdir-alist)
    (or (and dir
             (fsvn-browse-goto-directory dir)
             file
             (fsvn-browse-goto-file file))
        (goto-char opoint))))

(defun fsvn-browse-upgrade-source-tree-internal (wcpath new-source-path)
  (let* ((old-entries (fsvn-get-ls wcpath))
         (new-files (fsvn-browse-directory-files new-source-path))
         (old-files (mapcar 'fsvn-xml-lists->list->entry=>name$ old-entries))
         message-log-max)
    (mapc
     (lambda (new-file)
       (let ((nf (fsvn-expand-file new-file new-source-path))
             (of (fsvn-expand-file new-file wcpath))
             (oldfile (car (fsvn-file-member new-file old-files))))
         (when (and oldfile (not (file-exists-p of)))
           (error "Old file missing"))
         (message "Copying %s" new-file)
         (cond
          ((not (fsvn-file-exact-directory-p nf))
           (copy-file nf of t t))
          (oldfile
           (fsvn-browse-upgrade-source-tree-internal of nf))
          (t
           (fsvn-copy-directory nf of t)))
         (unless oldfile
           (fsvn-call-command-discard "add" of))))
     new-files)
    (mapc
     (lambda (old-file)
       (unless (fsvn-file-member old-file new-files)
         (let ((of (fsvn-expand-file old-file wcpath)))
           (fsvn-call-command-discard "delete" of))
         (message "Deleting %s" old-file)))
     old-files)
    t))

(defmacro fsvn-browse-open-message-edit (buffer &rest form)
  (declare (indent 1))
  `(let ((INFO fsvn-buffer-repos-info)
         (WIN-CONFIGURE (current-window-configuration)))
     (with-current-buffer ,buffer
       (fsvn-message-edit-mode)
       (setq fsvn-previous-window-configuration WIN-CONFIGURE)
       (setq fsvn-buffer-repos-info INFO)
       ,@form
       (run-hooks 'fsvn-message-edit-mode-prepared-hook))))

(defmacro fsvn-browse-quick-message-edit (&rest form)
  `(let ((buffer (fsvn-message-edit-generate-buffer)))
     (fsvn-browse-open-message-edit buffer
       ,@form
       (fsvn-parasite-setup-message-edit-window buffer))))

(defun fsvn-browse-add-file-select (files args)
  (let* ((win-configure (current-window-configuration))
         (info fsvn-buffer-repos-info)
         (select-buffer (fsvn-parasite-add-get-buffer files))
         win)
    (with-current-buffer select-buffer
      (fsvn-select-file-mode)
      (setq fsvn-previous-window-configuration win-configure)
      (fsvn-parasite-add-mode 1)
      (setq fsvn-parasite-add-subcommand-args args)
      (fsvn-select-file-draw-root info)
      (fsvn-parasite-add-draw-applicant files)
      (setq buffer-read-only t)
      (fsvn-select-file-first-file)
      (run-hooks 'fsvn-select-file-mode-prepared-hook))
    (fsvn-parasite-add-setup-window select-buffer)))

(defun fsvn-browse-commit-mode (files args)
  (let* ((browse-buffer (current-buffer))
         (win-configure (current-window-configuration))
         (info fsvn-buffer-repos-info)
         (buffers (fsvn-parasite-commit-get-buffers files))
         (select-buffer (car buffers))
         (msgedit-buffer (cdr buffers))
         win)
    ;; fsvn-message-edit-mode prior than fsvn-select-file-mode
    (with-current-buffer select-buffer
      (fsvn-select-file-mode)
      (setq fsvn-previous-window-configuration win-configure)
      (setq fsvn-select-file-draw-list-function 'fsvn-parasite-commit-draw-list)
      (setq fsvn-select-file-msgedit-buffer msgedit-buffer)
      (fsvn-parasite-commit-mode 1)
      (fsvn-select-file-draw-root info)
      (fsvn-parasite-commit-draw-applicant files)
      (setq buffer-read-only t)
      (fsvn-select-file-first-file)
      (run-hooks 'fsvn-select-file-mode-prepared-hook))
    (fsvn-browse-open-message-edit msgedit-buffer
      (setq fsvn-message-edit-file-select-buffer select-buffer)
      (fsvn-parasite-commit-mode 1)
      (fsvn-parasite-commit-set-subcommand-args args)
      (when (fsvn-config-tortoise-property-use (fsvn-buffer-repos-root info))
        (fsvn-tortoise-commit-initialize)))
    (fsvn-parasite-commit-setup-window msgedit-buffer select-buffer)))

(defun fsvn-browse-delete-message-edit (files args)
  (fsvn-browse-quick-message-edit
   (fsvn-parasite-delete-mode 1)
   (setq fsvn-parasite-delete-target-files files)
   (setq fsvn-parasite-delete-subcommand-args args)))

(defun fsvn-browse-mkdir-message-edit (dir args)
  (fsvn-browse-quick-message-edit
   (fsvn-parasite-mkdir-mode 1)
   (setq fsvn-parasite-mkdir-target-directory dir)
   (setq fsvn-parasite-mkdir-subcommand-args args)))

(defun fsvn-browse-lock-message-edit (files args)
  (fsvn-browse-quick-message-edit
   (fsvn-parasite-lock-mode 1)
   (setq fsvn-parasite-lock-target-files files)
   (fsvn-parasite-lock-set-subcommand-args args)))

;; ** current point function

(defun fsvn-browse-point-directory-p ()
  "Return non-nil when current point indicate directory."
  (save-excursion
    (forward-line 0)
    (looking-at fsvn-browse-re-dir)))

(defun fsvn-browse-point-symlink-p ()
  "Return non-nil when current point indicate symlink."
  (save-excursion
    (forward-line 0)
    (looking-at fsvn-browse-re-symlink)))

;;TODO symlink?
(defun fsvn-browse-point-file-p ()
  "Return non-nil when current point indicate file."
  (and (fsvn-current-filename) (not (fsvn-browse-point-directory-p))))

(defun fsvn-browse-point-repository-urlrev ()
  "Current point URL."
  (if fsvn-browse-repos-p
      (fsvn-browse-point-urlrev)
    (let ((info (fsvn-get-info-entry (fsvn-current-filename))))
      (fsvn-xml-info->entry=>url$ info))))

(defun fsvn-browse-point-local-filename ()
  "Current point local filename."
  (if (or fsvn-browse-repos-p (null (fsvn-current-filename)))
      nil
    (fsvn-expand-file (fsvn-current-filename) (fsvn-browse-current-path))))

(defun fsvn-browse-point-url ()
  "Current point URL. "
  (let ((file (fsvn-current-filename)))
    (cond
     ((null file)
      nil)
     ((not fsvn-browse-repos-p)
      (fsvn-expand-file (fsvn-current-filename) (fsvn-browse-current-path)))
     (t
      (fsvn-expand-url
       file
       (fsvn-expand-url
        (fsvn-browse-current-path)
        (fsvn-buffer-repos-root)))))))

(defun fsvn-browse-point-urlrev ()
  (let ((url (fsvn-browse-point-url)))
    (cond
     ((null url) nil)
     ((not fsvn-browse-repos-p)
      url)
     (t
      (fsvn-url-urlrev url (fsvn-browse-current-revision))))))

(defun fsvn-browse-point-canonicalized-urlrev ()
  (let ((urlrev (fsvn-browse-point-urlrev)))
    (if (string-match "@" (fsvn-url-filename urlrev))
        (concat urlrev "@")
      urlrev)))

(defun fsvn-browse-current-magic-directory ()
  (let ((url (fsvn-browse-current-repository-url)))
    (when url
      (fsvn-magic-create-name url))))

(defun fsvn-browse-point-magic-name ()
  (let ((urlrev (fsvn-browse-point-repository-urlrev)))
    (when urlrev
      (fsvn-magic-create-name urlrev))))

(defun fsvn-browse-current-path ()
  "Return top of buffer PATH."
  (save-excursion
    ;; current line is Path
    (forward-line 1)
    (cond
     ((or (re-search-backward fsvn-browse-re-subdir nil t)
          ;; search first path
          (re-search-forward fsvn-browse-re-subdir nil t))
      (match-string-no-properties 2)))))

(defun fsvn-browse-current-info ()
  "Current point source info."
  (let ((subdir (fsvn-string-assoc (fsvn-browse-current-path) fsvn-browse-subdir-alist)))
    (fsvn-browse-subdir.info subdir)))

(defun fsvn-browse-current-revision ()
  "Return current buffer indicate revision."
  fsvn-browse-revision)

(defun fsvn-browse-current-directory ()
  (file-name-as-directory
   (cond
    ((not fsvn-browse-repos-p)
     (fsvn-browse-current-path))
    (t
     default-directory))))

(defun fsvn-browse-current-directory-url ()
  (cond
   ((not fsvn-browse-repos-p)
    (fsvn-browse-current-path))
   (t
    (fsvn-expand-url
     (fsvn-browse-current-path)
     (fsvn-buffer-repos-root)))))

(defun fsvn-browse-current-directory-urlrev ()
  "Current point URL."
  (cond
   ((not fsvn-browse-repos-p)
    (fsvn-browse-current-path))
   (t
    (fsvn-url-urlrev
     (fsvn-browse-current-repository-url)
     (fsvn-browse-current-revision)))))

(defun fsvn-browse-current-repository-url ()
  (if fsvn-browse-repos-p
      (fsvn-expand-url
       (fsvn-browse-current-path)
       (fsvn-buffer-repos-root))
    (let* ((subdir (fsvn-browse-subdir (fsvn-browse-current-path) nil))
           (info (fsvn-browse-subdir.info subdir)))
      (fsvn-xml-info->entry=>url$ info))))

;; ** interactive arguments

(defmacro fsvn-browse-cmd-wc-only (&rest form)
  `(if fsvn-browse-repos-p
       (error "this command only executable in working copy.")
     ,@form))

(defun fsvn-browse-cmd-this-urlrev ()
  (let ((urlrev (fsvn-browse-point-urlrev)))
    (unless urlrev
      (error "No file on this line"))
    urlrev))

(defun fsvn-browse-cmd-this-wc-file ()
  (let ((file (fsvn-browse-point-local-filename)))
    (unless file
      (error "No file on this line"))
    file))

(defun fsvn-browse-cmd-selected-urlrevs ()
  (let ((fsvn-browse-file-name-function 'fsvn-browse-point-urlrev))
    (fsvn-browse-cmd-selected-targets)))

(defun fsvn-browse-cmd-selected-urls ()
  (let ((fsvn-browse-file-name-function 'fsvn-browse-point-url))
    (fsvn-browse-cmd-selected-targets)))

(defun fsvn-browse-cmd-selected-targets ()
  (let ((files (fsvn-browse-gather-marked-files fsvn-mark-mark-char)))
    (unless files
      (when (setq files (funcall fsvn-browse-file-name-function))
        (setq files (list files))))
    (unless files
      (error "No file on this line"))
    files))

;; async repository browser

(defvar fsvn-browse-repos-main-process nil)
(defvar fsvn-browse-repos-entries nil)

(defun fsvn-browse-repos-process-filter (proc event)
  (fsvn-process-event-handler proc event
    (goto-char (process-mark proc))
    (insert-before-markers event)
    (goto-char (point-min))
    (let (entries start end)
      ;;FIXME if --xml output changed..
      (while (and (re-search-forward "^[ \t]*<entry\\b" nil t)
                  (setq start (match-beginning 0))
                  (re-search-forward "^[ \t]*</entry>" nil t)
                  (setq end (match-end 0)))
        (setq entries
              (cons (fsvn-xml-parse-lists-entry start end) entries))
        (delete-region (point-min) end))
      (setq entries (nreverse entries))
      (let ((buffer (process-get proc 'fsvn-browse-repos-buffer)))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            ;; avoid when other process is running.
            (when (eq fsvn-browse-repos-main-process proc)
              (save-excursion
                (fsvn-browse-repos-async-draw-entries entries)))))))))

(defun fsvn-browse-repos-process-sentinel (proc event)
  (fsvn-process-exit-handler proc event
    (let ((buffer (process-get proc 'fsvn-browse-repos-buffer)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (setq fsvn-browse-repos-main-process nil)
          (let ((start (process-get proc 'fsvn-browse-start-marker))
                (goto-file (process-get proc 'fsvn-browse-goto-file)))
            ;; goto a intuitive file as long as program can.
            ;; Stay the cursor if async process spent too much seconds.
            (when (= start (point-marker))
              (or (and goto-file (fsvn-browse-goto-file goto-file))
                  (fsvn-browse-goto-first-file)))))))
    (kill-buffer (current-buffer))))

(defun fsvn-browse-repos-info-process-sentinel (proc event)
  (fsvn-process-exit-handler proc event
    (let ((info (car (fsvn-xml-parse-info)))
          (buffer (process-get proc 'fsvn-browse-repos-buffer)))
      (cond
       ((null info)
        (message "Not a repository"))
       ((buffer-live-p buffer)
        (with-current-buffer buffer
          (let ((path (fsvn-info-repos-path info))
                buffer-read-only)
            (save-excursion
              (fsvn-browse-set-repos-local-variables info)
              (fsvn-browse-draw-topmost-header info)
              (fsvn-browse-draw-path path)
              (fsvn-browse-set-repos-directory info)))))))
    (kill-buffer (current-buffer))))

(defun fsvn-browse-repos-start-info-process (urlrev)
  (let* ((buffer (fsvn-make-temp-buffer))
        (proc (fsvn-start-command "info" buffer "--xml" urlrev)))
    (set-process-sentinel proc 'fsvn-browse-repos-info-process-sentinel)
    (process-put proc 'fsvn-browse-repos-buffer (current-buffer))
    proc))

(defun fsvn-browse-repos-point-entry ()
  (let ((name (fsvn-current-filename)))
    (when name
      (catch 'found
        (mapc
         (lambda (x)
           (when (string= (fsvn-xml-lists->list->entry=>name$ x) name)
             (throw 'found x)))
         fsvn-browse-repos-entries)
        nil))))

(defun fsvn-browse-repos-insert-entry (entry)
  (fsvn-browse-ls-insert-repos-entry entry)
  (setq fsvn-browse-repos-entries 
        (cons entry fsvn-browse-repos-entries)))

(defun fsvn-browse-repos-async-draw-entries (entries)
  (let* ((proc fsvn-browse-repos-main-process)
         buffer-read-only)
    (save-excursion
      (mapc
       (lambda (entry)
         (goto-char (point-max))
         (fsvn-browse-repos-insert-entry entry))
       entries))
    (set-buffer-modified-p nil)))

(defun fsvn-browse-repos-start-process (urlrev goto-file)
  (let* ((buffer (fsvn-make-temp-buffer))
         (proc (fsvn-start-command "list" buffer
                                   "--xml"
                                   urlrev)))
    (set-process-filter proc 'fsvn-browse-repos-process-filter)
    (set-process-sentinel proc 'fsvn-browse-repos-process-sentinel)
    (process-put proc 'fsvn-browse-repos-buffer (current-buffer))
    (process-put proc 'fsvn-browse-start-marker (point-marker))
    (process-put proc 'fsvn-browse-goto-file goto-file)
    (set (make-local-variable 'fsvn-browse-repos-main-process) proc)
    (set (make-local-variable 'fsvn-browse-repos-entries) nil)
    proc))



;; general read function

(defun fsvn-browse-cmd-read-url-selected ()
  "Return selected files, for working copy or repository (url with non-revision)."
  (list (fsvn-browse-cmd-selected-urls)))

(defun fsvn-browse-cmd-read-url-selected-with-args (subcommand &optional default-args)
  (let* ((urls (fsvn-browse-cmd-selected-urls))
         (args (fsvn-cmd-read-subcommand-args subcommand default-args)))
    (list urls args)))

(defun fsvn-browse-cmd-read-urlrev-this-file ()
  (let ((urlrev (fsvn-browse-cmd-this-urlrev)))
    (list urlrev)))

(defun fsvn-browse-cmd-read-urlrev-this-file-with-args (subcommand &optional default-args)
  (let ((urlrev (fsvn-browse-cmd-this-urlrev))
        (args (fsvn-cmd-read-subcommand-args subcommand default-args)))
    (list urlrev args)))

(defun fsvn-browse-cmd-read-urlrev-selected (subcommand &optional default-args)
  (let ((urlrevs (fsvn-browse-cmd-selected-urlrevs)))
    (list urlrevs)))

(defun fsvn-browse-cmd-read-urlrev-selected-with-args (subcommand &optional default-args)
  (let* ((urlrevs (fsvn-browse-cmd-selected-urlrevs))
         (args (fsvn-cmd-read-subcommand-args subcommand default-args)))
    (list urlrevs args)))

(defun fsvn-browse-cmd-read-urlrev-path-with-args (subcommand &optional default-args)
  (let* ((args (fsvn-cmd-read-subcommand-args subcommand default-args)))
    (list args)))

(defun fsvn-browse-cmd-read-wc-this-file ()
  (fsvn-browse-cmd-wc-only
   (let ((file (fsvn-browse-cmd-this-wc-file)))
     (list file))))

(defun fsvn-browse-cmd-read-wc-selected ()
  (fsvn-browse-cmd-wc-only
   (fsvn-browse-cmd-read-url-selected)))

(defun fsvn-browse-cmd-read-wc-selected-with-args (subcommand &optional default-args)
  (fsvn-browse-cmd-wc-only
   (fsvn-browse-cmd-read-url-selected-with-args subcommand default-args)))

(defun fsvn-browse-cmd-read-wc-path-with-args (subcommand &optional default-args)
  (fsvn-browse-cmd-wc-only
   (let ((args (fsvn-cmd-read-subcommand-args subcommand default-args)))
     (list args))))

(defun fsvn-browse-cmd-read-commit-selected ()
  (fsvn-browse-cmd-wc-only
   (let* ((files (fsvn-browse-cmd-selected-urls))
          (args (fsvn-cmd-read-subcommand-args "commit" fsvn-default-args-commit)))
     (fsvn-browse-check-unsave-buffer files)
     (list files args))))

(defun fsvn-browse-cmd-read-commit-path ()
  (fsvn-browse-cmd-wc-only
   (let ((args (fsvn-cmd-read-subcommand-args "commit" fsvn-default-args-commit)))
     (fsvn-browse-check-unsave-buffer (list (fsvn-browse-current-path)))
     (list args))))

(defun fsvn-browse-cmd-read-smart-copy-this ()
  (fsvn-browse-cmd-read-smart-copy/move-this 
   (fsvn-browse-cmd-this-urlrev) t))

(defun fsvn-browse-cmd-read-smart-move-this ()
  (fsvn-browse-cmd-wc-only
   (fsvn-browse-cmd-read-smart-copy/move-this 
    (fsvn-browse-cmd-this-wc-file) nil)))

(defun fsvn-browse-cmd-read-smart-copy/move-this (from copy-p)
  (let* ((subcommand (if copy-p "copy" "move"))
         (default-args (if copy-p 
                           fsvn-default-args-copy
                          fsvn-default-args-move))
         (from (fsvn-browse-cmd-this-urlrev))
         (prompt (format "%s `%s' -> " (capitalize subcommand) (fsvn-url-filename from)))
         (to (fsvn-read-file-under-versioned prompt from))
         (args (fsvn-cmd-read-subcommand-args subcommand default-args))
         (alist (fsvn-smart-move/copy-file-alist from to))
         (directory (fsvn-browse-current-directory-url))
         (prompt (format "Select %s files. "
                         (if copy-p "copying" "moving")))
         selected)
    (when (> (length alist) 1)
      (setq selected (fsvn-electric-select-files 
                      directory
                      (mapcar
                       (lambda (item)
                         (list (car item) 
                               t
                               (format "%s to %s" 
                                       (capitalize subcommand)
                                       (fsvn-url-relative-name (cdr item) directory))
                               (cdr item)))
                       alist)
                      prompt))
      (setq alist (mapcar 
                   (lambda (item) 
                     (cons (nth 0 item) (nth 3 item)))
                   selected)))
    (list alist args)))

;; specific read function

(defun fsvn-browse-cmd-read-this-file-struct ()
  (let ((name (fsvn-browse-cmd-this-urlrev))
        (dirp (fsvn-browse-point-directory-p)))
    (list (fsvn-struct-browse-file-make :name name :directory-p dirp))))

(defun fsvn-browse-cmd-read-copy-this ()
  (let* ((from (fsvn-browse-cmd-this-urlrev))
         (prompt (format "Copy `%s' -> " (fsvn-url-filename from)))
         (to (fsvn-read-file-under-versioned prompt from))
         (args (fsvn-cmd-read-subcommand-args "copy" fsvn-default-args-copy)))
    (list from to args)))

(defun fsvn-browse-cmd-read-copy-selected ()
  (let* ((files (fsvn-browse-cmd-selected-urlrevs))
         (dir (fsvn-browse-dired-confirm files 'fsvn-browse-copy-selected 'fsvn-read-versioned-directory))
         (args (fsvn-cmd-read-subcommand-args "copy" fsvn-default-args-copy)))
    (list files dir args)))

(defun fsvn-browse-cmd-read-move-this ()
  (fsvn-browse-cmd-wc-only
   (let* ((from (fsvn-browse-cmd-this-wc-file))
          (prompt (format "Move `%s' -> " (fsvn-url-filename from)))
          (to (fsvn-read-file-under-versioned prompt from))
          (args (fsvn-cmd-read-subcommand-args "move" fsvn-default-args-move)))
     (list from to args))))

(defun fsvn-browse-cmd-read-move-selected ()
  (fsvn-browse-cmd-wc-only
   (let* ((files (fsvn-browse-cmd-selected-urls))
          (dir (fsvn-browse-dired-confirm files 'fsvn-browse-move-selected 'fsvn-read-versioned-directory))
          (args (fsvn-cmd-read-subcommand-args "move" fsvn-default-args-move)))
     (list files dir args))))

(defun fsvn-browse-cmd-read-copy-this-in-repository ()
  (let (from-url to-url args)
    (fsvn-brief-message-showing
     (setq from-url (fsvn-browse-point-repository-urlrev))
     (fsvn-brief-message-add-message (format "Source URL: %s" from-url))
     (setq to-url (fsvn-completing-read-url "Destination URL: " from-url t))
     (fsvn-brief-message-add-message (format "Destination URL: %s" to-url))
     (setq args (fsvn-cmd-read-subcommand-args "copy" fsvn-default-args-copy))
     (list from-url to-url args))))

(defun fsvn-browse-cmd-read-add-changelist-selected ()
  "Return selected files in working copy and `changelist' name."
  (fsvn-browse-cmd-wc-only
   (let* ((files (fsvn-browse-cmd-selected-urls))
          (name (fsvn-read-changelist-name)))
     (list name files))))

(defun fsvn-browse-cmd-read-switch ()
  "Return `switch' arguments name."
  (fsvn-browse-cmd-wc-only
   (let* ((repository (fsvn-completing-read-url 
                       "Switch to URL: "
                       (fsvn-url-as-directory (fsvn-browse-current-repository-url))
                       t))
          (args (fsvn-cmd-read-subcommand-args "switch" fsvn-default-args-switch)))
     (list repository args))))

(defun fsvn-browse-cmd-read-export-this ()
  (let* ((file (fsvn-browse-cmd-this-urlrev))
         (to-file (fsvn-read-file-name "Export to: "))
         (args (fsvn-cmd-read-subcommand-args "export" fsvn-default-args-export)))
    (when (file-exists-p to-file)
      ;; svn export file overwrite file with no confirm
      (cond
       ((file-directory-p to-file)
        (error "Destination directory exists"))
       ((not (y-or-n-p "Desitination file exists. Really overwrite? "))
        (fsvn-quit))))
    (list file to-file args)))

(defun fsvn-browse-cmd-read-export-path ()
  (let* ((to-dir (fsvn-read-directory-name "Export to: "))
         (args (fsvn-cmd-read-subcommand-args "export" fsvn-default-args-export)))
    (list to-dir args)))

(defun fsvn-browse-cmd-read-diff-this ()
  (fsvn-browse-cmd-wc-only
   (let* ((file (fsvn-browse-cmd-this-wc-file))
          (args (fsvn-cmd-read-subcommand-args "diff" fsvn-default-args-diff)))
     (list file args))))

(defun fsvn-browse-cmd-read-diff-path ()
  (fsvn-browse-cmd-wc-only
   (let* ((args (fsvn-cmd-read-subcommand-args "diff" fsvn-default-args-diff)))
     (list args))))

(defun fsvn-browse-cmd-read-diff-local ()
  (fsvn-browse-cmd-wc-only
   (let* ((src-file (fsvn-browse-cmd-this-wc-file))
          (dest-file (fsvn-read-file-name "Local file: " nil nil t))
          (args (if (stringp diff-switches)
                    diff-switches
                  (mapconcat 'identity diff-switches " "))))
     (list src-file dest-file args))))

(defun fsvn-browse-cmd-read-resolve-selected ()
  "Return selected files in working copy and `resolve' --accept args."
  (fsvn-browse-cmd-wc-only
   (let* ((files (fsvn-browse-cmd-selected-urls))
          (arg (fsvn-read-resolve-accept-arg)))
     (list arg files))))

(defun fsvn-browse-cmd-read-svn:externals-selected ()
  (let ((src-files (fsvn-browse-cmd-selected-urlrevs))
        dest confirmer)
    (cond
     ((= (length src-files) 1)
      (setq confirmer
            (lambda (prompt)
              (fsvn-read-file-under-versioned prompt nil)))
      (setq dest (fsvn-browse-dired-confirm 
                  src-files 'fsvn-browse-svn:externals-selected confirmer))
      (list src-files dest))
     (t
      (setq dest (fsvn-browse-dired-confirm 
                  src-files 'fsvn-browse-svn:externals-selected 'fsvn-read-versioned-directory))
      (list src-files dest)))))

(defun fsvn-browse-cmd-read-mkdir ()
  (let* ((dir (fsvn-read-mkdir-directory (fsvn-browse-current-directory-url)))
         (args (fsvn-cmd-read-subcommand-args "mkdir" fsvn-default-args-mkdir)))
    (list dir args)))

(defun fsvn-browse-cmd-read-merge-this ()
  (fsvn-browse-cmd-wc-only
   (let* ((file (fsvn-browse-cmd-this-wc-file))
          (args (fsvn-browse-cmd-read-merge-path)))
     (append (list file) args))))

(defun fsvn-browse-cmd-read-merge-path ()
  (fsvn-browse-cmd-wc-only
   (fsvn-brief-message-showing
    (let (from to args)
      (setq from (fsvn-completing-read-urlrev "URL1: "))
      (fsvn-brief-message-add-message (format "URL1: %s" from))
      (setq to (fsvn-completing-read-urlrev "URL2: " (fsvn-urlrev-url from)))
      (fsvn-brief-message-add-message (format "URL2: %s" to))
      (setq args (fsvn-cmd-read-subcommand-args "merge" fsvn-default-args-merge))
      (list from to args)))))

(defun fsvn-browse-cmd-read-mergeinfo-this ()
  (fsvn-browse-cmd-wc-only
   (let* ((file (fsvn-browse-cmd-this-wc-file))
          (args (fsvn-browse-cmd-read-mergeinfo)))
     (list file args))))

(defun fsvn-browse-cmd-read-mergeinfo ()
  (fsvn-browse-cmd-wc-only
   (let ((args (fsvn-cmd-read-subcommand-args "mergeinfo" fsvn-default-args-mergeinfo)))
     (list args))))

(defun fsvn-browse-cmd-read-branch/tag (prompt default-dirname)
  (let ((repos-url (fsvn-browse-current-repository-url))
        dest-url args)
    (setq repos-url (fsvn-completing-read-urlrev "Source URL: "  repos-url))
    (when (string-match "^\\(.*\\)/trunk" repos-url)
      (setq dest-url (concat (match-string 1 repos-url) "/" default-dirname "/")))
    (setq dest-url (fsvn-completing-read-url (format "New %s URL: " prompt) dest-url t))
    (setq args (fsvn-cmd-read-subcommand-args "copy" fsvn-default-args-copy))
    (list repos-url dest-url args)))

(defun fsvn-browse-cmd-read-upgrade-source-tree-args ()
  (fsvn-browse-cmd-wc-only
   (let ((dir (fsvn-read-directory-name "New Source: " nil nil t)))
     (unless (y-or-n-p "This takes many seconds. ok? ")
       (fsvn-quit))
     (list dir))))

(defun fsvn-browse-cmd-read-paste-properties-to-this ()
  (fsvn-browse-cmd-wc-only
   (let ((src-file)
         (dest-file (fsvn-browse-point-local-filename))
         (arg current-prefix-arg))
     (unless dest-file
       (error "No file on this line"))
     (setq src-file (fsvn-read-file-name "Properties copy from: " nil nil t))
     (list src-file dest-file arg))))

(defun fsvn-browse-cmd-read-create-patch-selected ()
  (fsvn-browse-cmd-wc-only
   (let* ((files (fsvn-browse-cmd-selected-urls))
          (patch (car (fsvn-cmd-read-patch-file))))
    (list files patch))))

(defun fsvn-browse-cmd-read-patch-args ()
  (let* ((patch (fsvn-read-file-name "Patch file: " nil nil t))
         (args (fsvn-cmd-read-subcommand-args "patch" fsvn-default-args-patch)))
    (list patch args)))

;; * fsvn-browse-mode interactive commands

(defun fsvn-browse-up-directory ()
  "Upward directory."
  (interactive)
  (let* ((path (fsvn-browse-current-path))
         (filename (fsvn-file-name-nondirectory path))
         (paren-dir (fsvn-file-name-directory2 path))
         (prev-buffer (current-buffer)))
    (cond
     (fsvn-browse-repos-p
      (let ((above (fsvn-urlrev-dirname (fsvn-browse-current-directory-urlrev))))
        (fsvn-browse-switch-directory-buffer above nil filename)))
     ((not (fsvn-file-versioned-directory-p path))
      (dired paren-dir)
      (dired-goto-file path))
     (t
      (fsvn-browse-switch-directory-buffer paren-dir)
      (fsvn-browse-goto-file filename)))
    (when fsvn-browse-cleanup-buffer
      (kill-buffer prev-buffer))))

(defun fsvn-browse-file-this (urlrev)
  "View file or directory."
  (interactive (fsvn-browse-cmd-read-urlrev-this-file))
  (when (fsvn-file= urlrev (fsvn-browse-current-directory))
    (error "Can't operate on `.'"))
  (let ((prev-buffer (current-buffer)))
    ;;TODO symlink
    (cond
     ((fsvn-browse-point-file-p)
      (cond
       (fsvn-browse-repos-p
        (fsvn-cat-async urlrev))
       (t
        (fsvn-view-buffer (fsvn-get-view-buffer urlrev)))))
     ((fsvn-url-local-p urlrev)
      ;; fall back to fsvn when working-copy by `dired' advice
      (dired urlrev)
      (when fsvn-browse-cleanup-buffer
        (kill-buffer prev-buffer)))
     ((fsvn-browse-point-directory-p)
      ;; next repository buffer
      (fsvn-browse-switch-directory-buffer urlrev)
      (when fsvn-browse-cleanup-buffer
        (kill-buffer prev-buffer)))
     (t
      (message "No file on this line.")))))

(defun fsvn-browse-mark-file-delete ()
  "Put `delete' mark current file or directory in working copy."
  (interactive)
  (fsvn-browse-wc-only
   (fsvn-browse-put-mark-current-file fsvn-mark-delete-char)))

(defun fsvn-browse-mark-file-mark ()
  "Put mark current file or directory."
  (interactive)
  (fsvn-browse-put-mark-current-file fsvn-mark-mark-char))

(defun fsvn-browse-mark-file-unmark ()
  "Remote any mark current file or directory."
  (interactive)
  (fsvn-browse-put-mark-current-file nil))

(defun fsvn-browse-mark-all-unmark (char)
  "Remote any mark current file or directory."
  (interactive "cRemove marks (RET means all): ")
  (let ((regexp (format "^%c" (if (eq char 13) ?. char)))
        (case-fold-search nil)
        (count 0))
    (fsvn-browse-each-file file nil
      (when (save-excursion
              (forward-line 0)
              (and (looking-at regexp)
                   (not (looking-at "^ "))))
        (fsvn-browse-put-mark-point nil)
        (setq count (1+ count))))
    (message (if (= count 1) "1 mark removed"
               "%d marks removed") count)))

(defun fsvn-browse-mark-file-regexp (regexp &optional mark)
  (interactive
   (list (dired-read-regexp (concat (if current-prefix-arg "Unmark" "Mark")
                                    " files (regexp): "))
         (if current-prefix-arg fsvn-space-char fsvn-mark-mark-char)))
  (fsvn-browse-mark-matched-file regexp mark))

(defun fsvn-browse-mark-delete-regexp (regexp)
  (interactive
   (list (dired-read-regexp (concat (if current-prefix-arg "Unmark" "Mark")
                                    " files (regexp): "))))
  (fsvn-browse-mark-matched-file regexp fsvn-mark-delete-char))

(defun fsvn-browse-open-repository ()
  "Open repository by `fsvn-browse-mode'"
  (interactive)
  (unless (fsvn-browse-current-path)
    (error "This point has no directory"))
  (let* ((info (fsvn-browse-subdir.info (fsvn-browse-subdir (fsvn-browse-current-path) fsvn-browse-repos-p)))
         (currev (fsvn-get-revision-string (fsvn-browse-current-revision)))
         (newrev (fsvn-completing-read-revision nil nil nil (fsvn-browse-current-path)))
         (urlrev (fsvn-url-urlrev (fsvn-xml-info->entry=>url$ info) newrev))
         (file (fsvn-current-filename)))
    (fsvn-browse-switch-directory-buffer 
     urlrev (and fsvn-browse-repos-p 
                 (if (string= currev newrev) 'draw 'revert))
     file)
    (when file
      (fsvn-browse-goto-file file))))

(defun fsvn-browse-magic-head ()
  "Open repository by `fsvn-magic'."
  (interactive)
  (dired (fsvn-browse-current-magic-directory)))

(defun fsvn-browse-commit-selected (files &optional args)
  "Prepare `commit' buffer for selected FILES.
"
  (interactive (fsvn-browse-cmd-read-commit-selected))
  (run-hook-with-args 'fsvn-browse-before-commit-hook files)
  (fsvn-browse-commit-mode files args))

(defun fsvn-browse-commit-path (&optional args)
  "Prepare `commit' buffer for changing files in this directory.
"
  (interactive (fsvn-browse-cmd-read-commit-path))
  (fsvn-browse-commit-selected (list (fsvn-browse-current-path)) args))

(defun fsvn-browse-cleanup-path (&optional args)
  (interactive (fsvn-browse-cmd-read-wc-path-with-args "cleanup" fsvn-default-args-cleanup))
  (if (or (not (fsvn-interactive-p))
          current-prefix-arg
          (fsvn-confirm-prompt 'fsvn-browse-cleanup-path "Svn: Cleanup directory? "))
      (let ((buffer (fsvn-make-temp-buffer))
            proc)
        (setq proc (fsvn-start-command "cleanup" buffer args))
        (process-put proc 'fsvn-browse-cleaning-buffer (current-buffer))
        (setq fsvn-browse-buffer-cleanup-process proc)
        (fsvn-process-add-sentinel proc 'fsvn-browse-cleanup-process-sentinel)
        proc)
    (message "(No svn Cleanup performed)")))

(defun fsvn-browse-mkdir (directory &optional args)
  "Execute `mkdir' to read DIRECTORY.
Optional ARGS (with \\[universal-argument]) means read svn subcommand arguments.
"
  (interactive (fsvn-browse-cmd-read-mkdir))
  (if (fsvn-url-repository-p directory)
      (fsvn-browse-mkdir-message-edit directory args)
    (fsvn-popup-call-process "mkdir" args directory)
    (fsvn-browse-add-wc-file-entry directory)))

(defun fsvn-browse-update-selected (files &optional args)
  "Execute `update' to selected FILES.
Optional ARGS (with \\[universal-argument]) means read svn subcommand arguments.
"
  (interactive (fsvn-browse-cmd-read-wc-selected-with-args "update" fsvn-default-args-update))
  (if (or (not (fsvn-interactive-p))
          current-prefix-arg
          (fsvn-browse-dired-confirm files 'fsvn-browse-update-selected))
      (let ((proc (fsvn-popup-start-process "update" args files)))
        (fsvn-process-add-filter proc 'fsvn-process-filter-for-update)
        proc)
    (message "(No svn Update performed)")))

(defun fsvn-browse-update-path (&optional args)
  "Execute `update' to current directory.
Optional ARGS (with \\[universal-argument]) means read svn subcommand arguments.
"
  (interactive (fsvn-browse-cmd-read-wc-path-with-args "update" fsvn-default-args-update))
  (if (or (not (fsvn-interactive-p))
          current-prefix-arg
          (fsvn-confirm-prompt 'fsvn-browse-update-path "Svn: Update current directory? "))
      (let ((proc (fsvn-popup-start-process "update" args)))
        (fsvn-process-add-filter proc 'fsvn-process-filter-for-update)
        proc)
    (message "(No svn Update performed)")))

(defun fsvn-browse-switch-path (repository &optional args)
  "Execute `switch' to current directory.
Optional ARGS (with \\[universal-argument]) means read svn subcommand arguments.
"
  (interactive (fsvn-browse-cmd-read-switch))
  (fsvn-popup-start-process "switch" args repository))

(defun fsvn-browse-resolved-selected (files &optional args)
  "Execute `resolved' to current directory.
Optional ARGS (with \\[universal-argument]) means read svn subcommand arguments.
"
  (interactive (fsvn-browse-cmd-read-wc-selected-with-args "resolved" fsvn-default-args-resolved))
  (if (or (not (fsvn-interactive-p))
          current-prefix-arg
          (fsvn-browse-dired-confirm files 'fsvn-browse-resolved-selected))
      (fsvn-parse-result-instant-sentinel
       (fsvn-popup-start-process "resolved" args files)
       'fsvn-parse-result-cmd-resolved)
    (message "(No svn Resolved performed)")))

(defun fsvn-browse-add-selected (files &optional args)
  "Execute `add' to selected FILES.
If ARGS contains `--non-recursive' or `-N', then confirm buffer will be shown.
Optional ARGS (with \\[universal-argument]) means read svn subcommand arguments.
"
  (interactive (fsvn-browse-cmd-read-wc-selected-with-args "add" fsvn-default-args-add))
  (cond
   ((and (not (fsvn-wc-files-only-non-recursive-p files))
         (member "--non-recursive" args))
    (fsvn-browse-add-file-select files args))
   ((or (not (fsvn-interactive-p))
        current-prefix-arg
        (fsvn-browse-dired-confirm files 'fsvn-browse-add-selected))
    (fsvn-parse-result-instant-sentinel
     (fsvn-popup-start-process "add" args files)
     'fsvn-parse-result-cmd-add))
   (t
    (message "(No svn Add performed)"))))

(defun fsvn-browse-delete-selected (files &optional args)
  "Execute `delete' to selected FILES.
Optional ARGS (with \\[universal-argument]) means read svn subcommand arguments.

In the repository buffer, confirm buffer will be shown.
"
  (interactive (fsvn-browse-cmd-read-url-selected-with-args "delete" fsvn-default-args-delete))
  (cond
   (fsvn-browse-repos-p
    (fsvn-browse-delete-message-edit files args))
   ((or (not (fsvn-interactive-p))
        current-prefix-arg
        (fsvn-browse-dired-confirm files 'fsvn-browse-delete-selected))
    (fsvn-parse-result-instant-sentinel
     (fsvn-popup-start-process "delete" args files)
     'fsvn-parse-result-cmd-delete))
   (t
    (message "(No svn Delete performed)"))))

(defun fsvn-browse-lock-selected (files &optional args)
  "Execute `lock' to selected FILES.
Optional ARGS (with \\[universal-argument]) means read svn subcommand arguments.

"
  (interactive (fsvn-browse-cmd-read-url-selected-with-args "lock" fsvn-default-args-lock))
  (cond
   ((member "--message" args)
    (fsvn-browse-lock-message-edit files args))
   ((or (not (fsvn-interactive-p))
        current-prefix-arg
        (fsvn-browse-dired-confirm files 'fsvn-browse-lock-selected))
    (fsvn-parse-result-cmd-lock
     (fsvn-popup-call-process-multi "lock" files args)))
   (t
    (message "(No svn Lock performed)"))))

(defun fsvn-browse-unlock-selected (files &optional args)
  "Execute `unlock' to selected FILES.
Optional ARGS (with \\[universal-argument]) means read svn subcommand arguments.
"
  (interactive (fsvn-browse-cmd-read-url-selected-with-args "unlock" fsvn-default-args-unlock))
  (if (or (not (fsvn-interactive-p))
          current-prefix-arg
          (fsvn-browse-dired-confirm files 'fsvn-browse-unlock-selected))
      (fsvn-parse-result-cmd-unlock
       (fsvn-popup-call-process-multi "unlock" files args))
    (message "(No svn Unlock performed)")))

(defun fsvn-browse-export-this (file to-file &optional args)
  "Execute `export' from point FILE to TO-FILE.
Optional ARGS (with \\[universal-argument]) means read svn subcommand arguments.
"
  (interactive (fsvn-browse-cmd-read-export-this))
  (fsvn-popup-start-process "export" file args to-file))

(defun fsvn-browse-export-path (to-dir &optional args)
  "Execute `export' from current directory to TO-DIR
Optional ARGS (with \\[universal-argument]) means read svn subcommand arguments.
"
  (interactive (fsvn-browse-cmd-read-export-path))
  (if (or (not (fsvn-interactive-p))
          current-prefix-arg
          (fsvn-confirm-prompt 'fsvn-browse-export-path "Svn: Export current directory? "))
      (fsvn-popup-start-process "export" args (fsvn-browse-current-directory-urlrev) to-dir)
    (message "(No svn Export performed)")))

(defun fsvn-browse-add-changelist-selected (name files)
  (interactive (fsvn-browse-cmd-read-add-changelist-selected))
  (if (or (not (fsvn-interactive-p))
          (fsvn-browse-dired-confirm files 'fsvn-browse-add-changelist-selected))
      (fsvn-popup-start-process "changelist" name files)
    (message "(No svn Changelist performed)")))

(defun fsvn-browse-remove-changelist-selected (files)
  (interactive (fsvn-browse-cmd-read-wc-selected))
  (if (or (not (fsvn-interactive-p))
          (fsvn-browse-dired-confirm files 'fsvn-browse-remove-changelist-selected))
      (fsvn-popup-start-process "changelist" "--remove" files)
    (message "(No svn Changelist performed)")))

(defun fsvn-browse-resolve-selected (accept-arg files)
  "Execute `resolve' to selected FILES."
  (interactive (fsvn-browse-cmd-read-resolve-selected))
  (fsvn-popup-start-process "resolve" "--accept" accept-arg files))

(defun fsvn-browse-move-this (src-file dest-file &optional args)
  "Execute `move' from point SRC-FILE to DEST-FILE.
Optional ARGS (with \\[universal-argument]) means read svn subcommand arguments.
"
  (interactive (fsvn-browse-cmd-read-move-this))
  (fsvn-popup-start-copy/move-process "move" (list src-file) dest-file args))

;;NOTE 1.4.x cannot copy/move multiple files
(defun fsvn-browse-move-selected (src-files dest-dir &optional args)
  "Execute `move' from selected SRC-FILES to DEST-DIR
Optional ARGS (with \\[universal-argument]) means read svn subcommand arguments.
"
  (interactive (fsvn-browse-cmd-read-move-selected))
  (fsvn-popup-start-copy/move-process "move" src-files dest-dir args))

(defun fsvn-browse-copy-this (src-file dest-file &optional args)
  "Execute `copy' from point SRC-FILE to DEST-FILE.
Optional ARGS (with \\[universal-argument]) means read svn subcommand arguments.
"
  (interactive (fsvn-browse-cmd-read-copy-this))
  (fsvn-popup-start-copy/move-process "copy" (list src-file) dest-file args))

;;NOTE 1.4.x cannot copy/move multiple files
(defun fsvn-browse-copy-selected (src-files dest-dir &optional args)
  "Execute `copy' from selected SRC-FILES to DEST-DIR
Optional ARGS (with \\[universal-argument]) means read svn subcommand arguments.
"
  (interactive (fsvn-browse-cmd-read-copy-selected))
  (fsvn-popup-start-copy/move-process "copy" src-files dest-dir args))

(defun fsvn-browse-safe-move-this (src-file dest-file &optional args)
  "Execute `move' from SRC-FILE at current point to DEST-FILE.
Same as `fsvn-browse-move-this' but allows you to DEST-FILE existence.
This means DEST-FILE contents will be stashed and finally restored.
This is useful for integrating other source management.
"
  (interactive (fsvn-browse-cmd-read-move-this))
  (if (not (file-exists-p dest-file))
      (apply 'fsvn-browse-move-this src-file dest-file args)
    (let (tmpfile)
      (cond
       ((and (fsvn-file-exact-file-p src-file)
             (fsvn-file-exact-file-p dest-file)))
       ((and (fsvn-file-exact-file-p src-file)
             (fsvn-file-exact-directory-p dest-file))
        (setq dest-file (fsvn-expand-file (fsvn-file-name-nondirectory src-file) dest-file)))
       ((and (fsvn-file-exact-directory-p src-file)
             (fsvn-file-exact-directory-p dest-file)))
       (t
        (error "if SRC-FILE is directory, DEST-FILE must be a directory.")))
      (setq tmpfile (fsvn-make-temp-filename src-file))
      (rename-file dest-file tmpfile t)
      (unwind-protect 
          (fsvn-popup-call-process "move" (list src-file) dest-file args)
        (cond
         ((fsvn-file-exact-directory-p tmpfile)
          (fsvn-copy-directory tmpfile dest-file))
         (t
          (rename-file tmpfile dest-file t)))))))

(defun fsvn-browse-safe-copy-this (src-file dest-file &optional args)
  "Execute `copy' to current file as SRC-FILE to DEST-FILE.
Same as `fsvn-browse-copy-this' but allows you to DEST-FILE existence.
This means DEST-FILE contents will be stashed and finally restored.
This is useful for integrating other source management.
"
  (interactive (fsvn-browse-cmd-read-copy-this))
  (if (not (file-exists-p dest-file))
      (apply 'fsvn-browse-copy-this src-file dest-file args)
    (let (tmpfile)
      (setq tmpfile (fsvn-make-temp-filename src-file))
      (rename-file dest-file tmpfile t)
      (unwind-protect 
          (fsvn-popup-call-process "copy" (list src-file) dest-file args)
        (cond
         ((fsvn-file-exact-directory-p tmpfile)
          (fsvn-copy-directory tmpfile dest-file))
         (t
          (rename-file tmpfile dest-file t)))))))

(defun fsvn-browse-smart-move-this (alist &optional args)
  "Execute `move' to point file.
If that file indicate multiple files, electric prompt these files.
Optional ARGS (with \\[universal-argument]) means read svn subcommand arguments.
"
  (interactive (fsvn-browse-cmd-read-smart-move-this))
  (let ((strategies
         (mapcar
          (lambda (item)
            (list 'fsvn-popup-start-copy/move-process "move" (car item) (cdr item) args))
          alist)))
    (fsvn-async-invoke strategies)))

(defun fsvn-browse-smart-copy-this (alist &optional args)
  "Execute `copy' to point file.
If that file indicate multiple files, electric prompt these files.
Optional ARGS (with \\[universal-argument]) means read svn subcommand arguments.
"
  (interactive (fsvn-browse-cmd-read-smart-copy-this))
  (let ((strategies
         (mapcar
          (lambda (item)
            (list 'fsvn-popup-start-copy/move-process "copy" (car item) (cdr item) args))
          alist)))
    (fsvn-async-invoke strategies)))

(defun fsvn-browse-create-branch (urlrev branch-url &optional args)
  "Create branch executing `copy'.
Optional ARGS (with \\[universal-argument]) means read svn subcommand arguments.
"
  (interactive (fsvn-browse-cmd-read-branch/tag "Branch" "branches"))
  (fsvn-browse-copy-this-in-repository urlrev branch-url args))

(defun fsvn-browse-create-tag (urlrev tag-url &optional args)
  "Create tag executing `copy'.
Optional ARGS (with \\[universal-argument]) means read svn subcommand arguments.
"
  (interactive (fsvn-browse-cmd-read-branch/tag "Tag" "tags"))
  (fsvn-browse-copy-this-in-repository urlrev tag-url args))

(defun fsvn-browse-copy-this-in-repository (from-url to-url &optional args)
  "Execute `copy' to repository file corresponding local file.
Optional ARGS (with \\[universal-argument]) means read svn subcommand arguments.

This makes faster copy than in working copy.
"
  (interactive (fsvn-browse-cmd-read-copy-this-in-repository))
  (fsvn-browse-quick-message-edit
   (fsvn-parasite-copy-mode 1)
   (setq fsvn-parasite-copy-from-files (list from-url))
   (setq fsvn-parasite-copy-destination to-url)
   (setq fsvn-parasite-copy-subcommand-args args)))

(defun fsvn-browse-revert-selected (files &optional args)
  "Execute `revert' to selected FILES.
Optional ARGS (with \\[universal-argument]) means read svn subcommand arguments.
"
  (interactive (fsvn-browse-cmd-read-wc-selected-with-args "revert" fsvn-default-args-revert))
  (if (or (not (fsvn-interactive-p))
          current-prefix-arg
          (fsvn-browse-dired-confirm files 'fsvn-browse-revert-selected))
      (fsvn-parse-result-instant-sentinel
       (fsvn-popup-start-process "revert" args files)
       'fsvn-parse-result-cmd-revert)
    (message "(No svn Revert performed)")))

(defun fsvn-browse-revert-path (&optional args)
  "Execute `revert' to current directory.
Optional ARGS (with \\[universal-argument]) means read svn subcommand arguments.
"
  (interactive (fsvn-browse-cmd-read-wc-path-with-args "revert" fsvn-default-args-revert))
  (if (or (not (fsvn-interactive-p))
          current-prefix-arg
          (fsvn-confirm-prompt 'fsvn-browse-revert-path "Svn: Revert current directory? "))
      (fsvn-parse-result-instant-sentinel
       (fsvn-popup-start-process "revert" (fsvn-browse-current-directory-url) args)
       'fsvn-parse-result-cmd-revert)
    (message "(No svn Revert performed)")))

(defun fsvn-browse-merge-this (file source1 source2 &optional args)
  "Execute `merge' to FILE at point between SOURCE1 and SOURCE2.
Optional ARGS (with \\[universal-argument]) means read svn subcommand arguments.

This command covers `merge' first and second form (see \\[fsvn-show-svn-help] and type \"merge\")
"
  (interactive (fsvn-browse-cmd-read-merge-this))
  (fsvn-popup-start-process "merge" source1 source2 args file))

(defun fsvn-browse-merge-path (source1 source2 &optional args)
  "Execute `merge' to current directory between SOURCE1 and SOURCE2.
Optional ARGS (with \\[universal-argument]) means read svn subcommand arguments.

This command covers `merge' first and second form (see \\[fsvn-show-svn-help] and type \"merge\")
"
  (interactive (fsvn-browse-cmd-read-merge-path))
  (fsvn-popup-start-process "merge" source1 source2 args))

(defun fsvn-browse-mergeinfo-path (&optional args)
  "Execute `mergeinfo' to current directory.
Optional ARGS (with \\[universal-argument]) means read svn subcommand arguments.
"
  (interactive (fsvn-browse-cmd-read-mergeinfo))
  (fsvn-popup-start-process "mergeinfo" args (fsvn-browse-current-directory-urlrev)))

(defun fsvn-browse-mergeinfo-this (file &optional args)
  "Execute `mergeinfo' to point FILE.
Optional ARGS (with \\[universal-argument]) means read svn subcommand arguments.
"
  (interactive (fsvn-browse-cmd-read-mergeinfo-this))
  (fsvn-popup-start-process "mergeinfo" file args))

(defun fsvn-browse-logview-this (file-struct)
  "Execute `log' to current file showing by `fsvn-log-list-mode'."
  (interactive (fsvn-browse-cmd-read-this-file-struct))
  (fsvn-open-logview-mode
   (fsvn-struct-browse-file-get-name file-struct)
   (fsvn-struct-browse-file-get-directory-p file-struct)))

(defun fsvn-browse-logview-path ()
  "Execute `log' to current directory showing by `fsvn-log-list-mode'."
  (interactive)
  (fsvn-open-logview-mode (fsvn-browse-current-directory-urlrev) t))

(defun fsvn-browse-propview-this (file)
  "Execute `proplist' by `fsvn-proplist-mode' to point FILE"
  (interactive (fsvn-browse-cmd-read-this-file-struct))
  (fsvn-browse-propview-mode
   (fsvn-struct-browse-file-get-name file)
   (fsvn-struct-browse-file-get-directory-p file)))

(defun fsvn-browse-propview-path ()
  "Execute `proplist' by `fsvn-proplist-mode' to current directory."
  (interactive)
  (fsvn-browse-propview-mode (fsvn-browse-current-directory-url) t))

(defun fsvn-browse-info-selected (files &optional args)
  "Execute `info' to selected FILES.
Optional ARGS (with \\[universal-argument]) means read svn subcommand arguments.
"
  (interactive (fsvn-browse-cmd-read-urlrev-selected-with-args "info" fsvn-default-args-info))
  (fsvn-popup-call-process-multi "info" files args))

(defun fsvn-browse-info-path (&optional args)
  (interactive (fsvn-browse-cmd-read-urlrev-path-with-args "info" fsvn-default-args-info))
  "Execute `info' to current directory.
Optional ARGS (with \\[universal-argument]) means read svn subcommand arguments.
"
  (fsvn-popup-call-process "info" args))

(defun fsvn-browse-blame-this (file &optional args)
  (interactive (fsvn-browse-cmd-read-urlrev-this-file-with-args "blame" fsvn-default-args-blame))
  "Execute `blame'/`prase'/`annotate' to point FILE.
Optional ARGS (with \\[universal-argument]) means read svn subcommand arguments.
"
  (fsvn-popup-start-process "blame" file args))

(defun fsvn-browse-patch-path (patch-file &optional args)
  "Execute `patch' to current directory.
Optional ARGS (with \\[universal-argument]) means read svn subcommand arguments.
"
  (interactive (fsvn-browse-cmd-read-patch-args))
  (fsvn-popup-start-process "patch" patch-file args))

(defun fsvn-browse-prop-add-svn:ignore-selected (files)
  "Ignore point FILE by using `svn:ignore' property."
  (interactive (fsvn-browse-cmd-read-wc-selected))
  (if (or (not (fsvn-interactive-p))
          (fsvn-browse-dired-confirm files 'fsvn-browse-prop-add-svn:ignore-selected))
      (mapc
       (lambda (cell)
         (fsvn-add-prop-svn:ignore (car cell) (cdr cell)))
       (fsvn-group-by-directory files))
    (message "(No svn Ignore performed)")))

(defun fsvn-browse-prop-toggle-svn:needs-lock-selected (files)
  "Toggle `svn:needs-lock' property value."
  (interactive (fsvn-browse-cmd-read-url-selected))
  (if (or (not (fsvn-interactive-p))
          (fsvn-browse-dired-confirm files 'fsvn-browse-prop-toggle-svn:needs-lock-selected))
      (mapc
       (lambda (file)
         (fsvn-set-prop-svn:needs-lock file (not (fsvn-get-prop-svn:needs-lock file))))
       files)
    (message "(No svn Needs-Lock performed)")))

(defalias 'fsvn-browse-next-file 'fsvn-next-file)
(defalias 'fsvn-browse-previous-file 'fsvn-previous-file)

(defun fsvn-browse-toggle-sort ()
  "Toggle sorting condition mod-time and file-name."
  (interactive)
  (let* ((key
          (if fsvn-browse-repos-p 'repository 'working-copy))
         (next (fsvn-cycle-next
                (cdr (assq key fsvn-browse-ls-comparator-alist))
                fsvn-browse-ls-comparer)))
    (when (eq next fsvn-browse-ls-comparer)
      (error "Toggle sorting is disabled"))
    (setq fsvn-browse-ls-comparer next)
    (revert-buffer)))

(defun fsvn-browse-diff-this (file &optional args)
  "Execute `diff' to point FILE.
Optional ARGS (with \\[universal-argument]) means read svn subcommand arguments.
"
  (interactive (fsvn-browse-cmd-read-diff-this))
  (fsvn-browse-wc-only
   (let ((diff-args (list file args)))
     (fsvn-diff-start-process diff-args))))

(defun fsvn-browse-diff-path (&optional args)
  "Execute `diff' for current directory.
Optional ARGS (with \\[universal-argument]) means read svn subcommand arguments.
"
  (interactive (fsvn-browse-cmd-read-diff-path))
  (fsvn-browse-wc-only
   (fsvn-diff-start-process (fsvn-browse-current-path) args)))

(defun fsvn-browse-diff-this-with-previous (file &optional args)
  "Execute `diff' between current directory andith previous commited revision.
Optional ARGS (with \\[universal-argument]) means read svn subcommand arguments.

Like `git show'
"
  (interactive (fsvn-browse-cmd-read-diff-this))
  (fsvn-browse-wc-only
   (let ((diff-args (list file args "--revision" "PREV")))
     (fsvn-diff-start-process diff-args))))

(defun fsvn-browse-diff-path-with-previous (&optional args)
  "Execute `diff' between current directory and previous commited revision.
Optional ARGS (with \\[universal-argument]) means read svn subcommand arguments.

Like `git show'
"
  (interactive (fsvn-browse-cmd-read-diff-path))
  (fsvn-browse-wc-only
   (let ((diff-args (list args "--revision" "PREV")))
     (fsvn-diff-start-process (fsvn-browse-current-path) diff-args))))

(defun fsvn-browse-ediff-this (file)
  "Ediff for point FILE."
  (interactive (fsvn-browse-cmd-read-wc-this-file))
  (fsvn-browse-wc-only
   (let* ((urlrev (fsvn-url-urlrev file "BASE"))
          (directory-p (file-directory-p file)))
     (fsvn-ediff-between-urlrevs urlrev file directory-p))))

(defun fsvn-browse-ediff-path ()
  "Ediff for current directory."
  (interactive)
  (fsvn-browse-wc-only
   (let* ((urlrev (fsvn-url-urlrev (fsvn-browse-current-directory-url) "BASE")))
     (fsvn-ediff-between-urlrevs urlrev (fsvn-browse-current-directory-url) t))))

(defun fsvn-browse-diff-local (src-file dest-file &optional switches)
  "Same as `dired-diff'."
  (interactive (fsvn-browse-cmd-read-diff-local))
  (fsvn-browse-wc-only
   (fsvn-diff-files dest-file src-file switches)))

(defun fsvn-browse-rename-case-missing-file (file)
  "This occation if windows environment."
  (interactive (fsvn-browse-cmd-read-wc-this-file))
  (fsvn-browse-wc-only
   (fsvn-rename-case-missing-file file)))

;; not works 1.4.x
(defun fsvn-browse-svn:externals-selected (src-files dest)
  "Create `svn:externals' property SRC-FILES to DEST
DEST must be a directory when SRC-FILES is multiple.
When SRC-FILES is single list, DEST allows non existence filename."
  (interactive (fsvn-browse-cmd-read-svn:externals-selected))
  (let (targetdir prop externals value)
    (cond
     ((fsvn-file-exact-directory-p dest)
      (setq targetdir dest)
      (setq prop (fsvn-get-propget targetdir "svn:externals"))
      (when prop
        (setq externals
              (split-string prop "\n" t)))
      (mapc
       (lambda (src)
         (setq value (format "%s %s" (fsvn-file-relative src targetdir) (fsvn-file-name-nondirectory src)))
         (setq externals (cons value externals)))
       src-files))
     ((file-exists-p dest)
      (error "Destination file already exists."))
     (t
      (setq targetdir (fsvn-file-name-directory dest))
      (setq prop (fsvn-get-propget targetdir "svn:externals"))
      (when prop
        (setq externals
              (split-string prop "\n" t)))
      (setq value (format "%s %s" (fsvn-file-relative (car src-files) targetdir) (fsvn-file-name-nondirectory dest)))
      (setq externals (cons value externals))))
    (fsvn-popup-start-process "propset" "svn:externals" (mapconcat 'identity externals "\n") targetdir)))

(defun fsvn-browse-upgrade-source-tree (new-source)
  "Upgrade current directory tree by NEW-SOURCE tree.
NEW-SOURCE is intent on unpacked `source.tar.gz'.
After executing this command, current directory tree will be exactly equals NEW-SOURCE.
You have to \\<fsvn-browse-mode-map>\\[fsvn-browse-commit-path] to commit all changes."
  (interactive (fsvn-browse-cmd-read-upgrade-source-tree-args))
  (fsvn-browse-wc-only
   (let ((wcpath (fsvn-browse-current-directory)))
     (fsvn-call-command-discard "update" wcpath)
     (fsvn-browse-upgrade-source-tree-internal wcpath new-source))))

(defun fsvn-browse-paste-properties-to-this (src-file dest-file &optional full)
  "Copy SRC-FILE properties to DEST-FILE.
FULL non-nil means DEST-FILE will have exactly same properties of SRC-FILE."
  (interactive (fsvn-browse-cmd-read-paste-properties-to-this))
  (let ((alist (fsvn-get-prop-value-alist src-file)))
    (mapc
     (lambda (key-value)
       (let ((prop (car key-value))
             (val (cdr key-value)))
         (fsvn-set-propset dest-file prop val)))
     alist)
    (when full
      (let ((list (fsvn-get-proplist dest-file)))
        (mapc
         (lambda (prop)
           (unless (assoc prop alist)
             (fsvn-set-propdel dest-file prop)))
         list)))
    (fsvn-browse-redraw-wc-file-entry dest-file)))

(defun fsvn-browse-create-patch-path (patch-file)
  "Create PATCH-FILE for current directory."
  (interactive (fsvn-cmd-read-patch-file))
  (fsvn-browse-wc-only
   (fsvn-browse-create-patch-selected (list (fsvn-browse-current-directory-url)) patch-file)))

(defun fsvn-browse-create-patch-selected (files patch-file)
  "Create PATCH-FILE for for selected FILES."
  (interactive (fsvn-browse-cmd-read-create-patch-selected))
  (fsvn-browse-wc-only
   (fsvn-diff-create-patch patch-file (mapcar 'fsvn-file-relative files))))



(defconst fsvn-browse-mode-menu-spec
  '("fsvn"
    ["Update" fsvn-browse-update-path t]
    ["Commit" fsvn-browse-commit-path t]
    ["Log" fsvn-browse-logview-path t]
    ["Diff" fsvn-browse-diff-this t]
    "----"
    ("Current Directory"
     ["Cleanup" fsvn-browse-cleanup-path t]
     ["Commit" fsvn-browse-commit-path t]
     ["Export" fsvn-browse-export-path t]
     ["Diff" fsvn-browse-diff-path t]
     ["Diff Show" fsvn-browse-diff-path-with-previous t]
     ["Ediff" fsvn-browse-ediff-path t]
     ["Info" fsvn-browse-info-path t]
     ["Log" fsvn-browse-logview-path t]
     ["Merge" fsvn-browse-merge-path t]
     ["Mergeinfo" fsvn-browse-mergeinfo-path t]
     ["Mkdir" fsvn-browse-mkdir t]
     ["Patch" fsvn-browse-create-patch-path t]
     ["Proplist" fsvn-browse-propview-path t]
     ["Revert" fsvn-browse-revert-path t]
     ["Switch" fsvn-browse-switch-path t]
     ["Update" fsvn-browse-update-path t]
     ["Patch" fsvn-browse-patch-path t]
     ["Upgrade source tree" fsvn-browse-upgrade-source-tree t]
     )
    ("Selected Files"
     ["Add" fsvn-browse-add-selected t]
     ["Add to changelist" fsvn-browse-add-changelist-selected t]
     ["Commit" fsvn-browse-commit-selected t]
     ["Copy" fsvn-browse-copy-selected t]
     ["Delete" fsvn-browse-delete-selected t]
     ["Externals" fsvn-browse-svn:externals-selected t]
     ["Info" fsvn-browse-info-selected t]
     ["Lock" fsvn-browse-lock-selected t]
     ["Move" fsvn-browse-move-selected t]
     ["Patch" fsvn-browse-create-patch-selected t]
     ["Remove from changelist" fsvn-browse-remove-changelist-selected t]
     ["Resolve" fsvn-browse-resolve-selected t]
     ["Resolved" fsvn-browse-resolved-selected t]
     ["Revert" fsvn-browse-revert-selected t]
     ["Toggle svn:ignore" fsvn-browse-prop-add-svn:ignore-selected t]
     ["Toggle svn:needs-lock" fsvn-browse-prop-toggle-svn:needs-lock-selected t]
     ["Unlock" fsvn-browse-unlock-selected t]
     ["Update" fsvn-browse-update-selected t]
     )
    ("File At Point"
     ["Blame/Annotate" fsvn-browse-blame-this t]
     ["Copy in repository" fsvn-browse-copy-this-in-repository t]
     ["Copy" fsvn-browse-copy-this t]
     ["Diff" fsvn-browse-diff-this t]
     ["Diff Show" fsvn-browse-diff-this-with-previous t]
     ["EDiff" fsvn-browse-ediff-this t]
     ["Export" fsvn-browse-export-this t]
     ["Fix Filename Case" fsvn-browse-rename-case-missing-file t]
     ["Log" fsvn-browse-logview-this t]
     ["Merge" fsvn-browse-merge-this t]
     ["Mergeinfo" fsvn-browse-mergeinfo-this t]
     ["Move" fsvn-browse-move-this t]
     ["Paste properties" fsvn-browse-paste-properties-to-this t]
     ["Proplist" fsvn-browse-propview-this t]
     ["Safe Copy" fsvn-browse-safe-copy-this t]
     ["Safe Move" fsvn-browse-safe-move-this t]
     ["Smart Copy" fsvn-browse-smart-copy-this t]
     ["Smart Move" fsvn-browse-smart-move-this t]
     )
    ("Repository"
     ["Browser" fsvn-browse-open-repository t]
     ["Create Branch" fsvn-browse-create-branch t]
     ["Create Tag" fsvn-browse-create-tag t]
     ["Open dired by Magic" fsvn-browse-magic-head t]
     )
    "----"
    ("Emmulate Dired"
     ["Diff" fsvn-browse-diff-local t]
     ["Mark Delete Regexp" fsvn-browse-mark-delete-regexp t]
     ["Mark Delete" fsvn-browse-mark-file-delete t]
     ["Mark Regexp" fsvn-browse-mark-file-regexp t]
     ["Mark" fsvn-browse-mark-file-mark t]
     ["Next" fsvn-browse-next-file t]
     ["Previous" fsvn-browse-previous-file t]
     ["Sort" fsvn-browse-toggle-sort t]
     ["Unmark All" fsvn-browse-mark-all-unmark t]
     ["Unmark File" fsvn-browse-mark-file-unmark t]
     ["Up" fsvn-browse-up-directory t]
     ["View" fsvn-browse-file-this t]
     )
    ))

(easy-menu-define fsvn-browse-mode-menu
  fsvn-browse-mode-map
  "Menu used in Fsvn Browse mode."
  fsvn-browse-mode-menu-spec)



(provide 'fsvn-browse)

;;; fsvn-browse.el ends here
