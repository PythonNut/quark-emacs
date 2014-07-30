;;; fsvn-select.el --- File selector mode for fsvn.el


;;; History:
;; 

;;; Commentary:
;; 

;;; Code:



(require 'fsvn-env)
(require 'fsvn-ui)
(require 'fsvn-mode)
(require 'fsvn-cmd)
(require 'fsvn-url)
(require 'fsvn-xml)
(require 'fsvn-proc)
(require 'fsvn-popup)
(require 'fsvn-diff)



(defvar current-prefix-arg)



(defconst fsvn-select-file-buffer-local-variables
  '(
    (fsvn-select-file-files-status)
    (fsvn-select-file-draw-list-function)
    (fsvn-buffer-repos-info)
    (font-lock-defaults . '(fsvn-select-file-font-lock-keywords t nil nil beginning-of-line))

    (fsvn-select-file-msgedit-buffer)
    ;; BUG only works commit
    (revert-buffer-function . 'fsvn-select-file-revert-buffer)
    ))

(defconst fsvn-select-file-re-mark "^[^ \n]")
(defconst fsvn-select-file-re-dir "^..d ")
(defconst fsvn-select-file-re-symlink "^..l ")
(defconst fsvn-select-file-re-root "^ \\(Root\\): \\(.+\\)")
(defconst fsvn-select-file-buffer-name "Fsvn File Select")

(defvar fsvn-select-file-files-status nil
  "Optional")
(defvar fsvn-select-file-msgedit-buffer nil)
(defvar fsvn-select-file-draw-list-function nil)

(defvar fsvn-select-file-font-lock-keywords 
  (list
   (list fsvn-select-file-re-root '(1 fsvn-header-key-face) '(2 fsvn-header-face))
   (list fsvn-select-file-re-mark '(0 fsvn-mark-face))
   (list (concat "^[" (char-to-string fsvn-mark-mark-char) "]")
         '(".+" (fsvn-move-to-filename) nil (0 fsvn-marked-face)))
   (list fsvn-select-file-re-dir
         '(".+" (fsvn-move-to-filename) nil (0 fsvn-directory-face)))
   (list fsvn-select-file-re-symlink
         '(".+" (fsvn-move-to-filename) nil (0 fsvn-symlink-face)))
   ))

(defvar fsvn-select-file-diff-map nil)
(defvar fsvn-select-file-mode-map nil)
(unless fsvn-select-file-diff-map
  (setq fsvn-select-file-diff-map
        (let ((map (make-sparse-keymap)))

          (define-key map "=" 'fsvn-select-file-diff-base)
          (define-key map "e" 'fsvn-select-file-ediff-base)

          map)))

(unless fsvn-select-file-mode-map
  (setq fsvn-select-file-mode-map
        (let ((map (make-sparse-keymap)))
          (suppress-keymap map)

          (fsvn-readonly-mode-keymap map)

          (define-key map "," 'scroll-other-window-down)
          (define-key map "." 'scroll-other-window)
          (define-key map "\C-c\C-l" 'fsvn-restore-default-window-setting)
          (define-key map "\C-c\ei" 'fsvn-select-file-ignore-this)
          (define-key map "\C-m" 'fsvn-select-file-view-file)
          (define-key map "\C-n" 'fsvn-select-file-next-file)
          (define-key map "\C-p" 'fsvn-select-file-previous-file)
          (define-key map "g" 'revert-buffer)
          (define-key map "m" 'fsvn-select-file-mark)
          (define-key map "n" 'fsvn-select-file-next-file)
          (define-key map "p" 'fsvn-select-file-previous-file)
          (define-key map "u" 'fsvn-select-file-mark-clear)
          (define-key map "D" 'fsvn-select-file-do-delete-this)
          (define-key map "w" 'fsvn-select-file-copy-filename)
          
          (define-key map "zd" 'fsvn-select-file-delete-this)

          ;; TODO
          ;; (define-key map "za" 'fsvn-select-file-add-this)
          ;;TODO commit
          (define-key map "\C-c\C-vr" 'fsvn-select-file-revert-this)
          (define-key map "=" fsvn-select-file-diff-map)

          ;;todo not implement
          ;;    (define-key map "" 'fsvn-select-file-show-changelist-member)

          map)))

(defcustom fsvn-select-file-mode-hook nil
  "Run at the very end of `fsvn-select-file-mode'."
  :group 'fsvn
  :type 'hook)

(defcustom fsvn-select-file-mode-prepared-hook nil
  "Run at the very end of `fsvn-select-file-mode' is prepared."
  :group 'fsvn
  :type 'hook)

;; * fsvn-select-file-mode internal function

(defun fsvn-select-file-mode ()
  "Major mode for selecting files in Subversion subcommand.

Entry to this mode calls the value of `fsvn-select-file-mode-hook'.

Keybindings:
\\{fsvn-select-file-mode-map}

"
  (fsvn-global-initialize-mode)
  (use-local-map fsvn-select-file-mode-map)
  (setq major-mode 'fsvn-select-file-mode)
  (setq mode-name "Fsvn File Select")
  (setq truncate-lines t)
  (setq buffer-undo-list t)
  (fsvn-make-buffer-variables fsvn-select-file-buffer-local-variables)
  (run-mode-hooks 'fsvn-select-file-mode-hook))

(defun fsvn-select-file-generate-buffer ()
  (generate-new-buffer fsvn-select-file-buffer-name))

(defmacro fsvn-select-file-each-buffers (minor-var &rest form)
  `(fsvn-each-buffer-mode 'fsvn-select-file-mode
     (when (symbol-value ,minor-var)
       (progn ,@form))))

(defun fsvn-select-file-revert-buffer (ignore-auto noconfirm)
  (let ((file (fsvn-current-filename))
        ;;FIXME when multiple subdir and D mark
        (marked (fsvn-select-file-gather-marked-files))
        (opoint (point)))
    (funcall fsvn-select-file-draw-list-function)
    (mapc
     (lambda (file)
       (fsvn-select-file-file-put-mark file t))
     marked)
    (or (and file
             (fsvn-select-file-goto-file file))
        (goto-char opoint))))

(defun fsvn-select-file-cmd-file (&optional subcommand default-args)
  (let ((file (fsvn-current-filename))
        args)
    (unless file
      (error "No file on this line"))
    (setq file (fsvn-expand-file file))
    (setq args
          (if (and current-prefix-arg subcommand)
              (fsvn-read-svn-subcommand-args subcommand t default-args)
            default-args))
    (if args
        (list file args)
      (list file))))

(defmacro fsvn-select-file-each-file (var &rest form)
  (declare (indent 1))
  `(save-excursion
     (let (,var RET)
       (fsvn-select-file-first-file)
       (while (setq ,var (fsvn-current-filename))
         (setq RET (cons ,@form RET))
         (fsvn-select-file-next-file))
       (nreverse RET))))

(defun fsvn-select-file-first-file ()
  (let ((saved (point)))
    (goto-char (point-min))
    (while (and (not (eobp)) (null (fsvn-current-filename)))
      (forward-line 1))
    (if (eobp)
        (goto-char saved)
      (fsvn-move-to-filename))))

(defun fsvn-select-file-choice-unversioned (files)
  (fsvn-mapitem
   (lambda (file)
     (let ((status (fsvn-select-file-file-status file)))
       (when (or (null status)
                 (eq (fsvn-xml-status->target->entry=>wc-status.item status) 'unversioned))
         file)))
     files))

(defun fsvn-select-file-choice-just-locked (files)
  (fsvn-mapitem
   (lambda (file)
     (let ((status (fsvn-select-file-file-status file)))
       (when (and status
                  (eq (fsvn-xml-status->target->entry=>wc-status.item status) 'normal)
                  (eq (fsvn-xml-status->target->entry=>wc-status.props status) 'none)
                  (fsvn-xml-status->target->entry=>wc-status=>lock status))
         file)))
   files))

(defun fsvn-select-file-file-status (file)
  (fsvn-find-status-entry (mapcar 'cdr fsvn-select-file-files-status) file))

(defun fsvn-select-file-point-put-mark-internal (mark)
  (save-excursion
    (when (fsvn-move-to-filename)
      (forward-line 0)
      (let ((inhibit-read-only t))
        (delete-char 1)
        (insert (if mark fsvn-mark-mark-char fsvn-space-char))
        (set-buffer-modified-p nil)))))

(defun fsvn-select-file-point-put-mark (mark)
  (let ((filename (fsvn-current-filename)))
    (when filename
      (fsvn-select-file-file-put-mark filename mark))))

(defun fsvn-select-file-goto-file (filename)
  (let ((prev (point))
        file)
    (catch 'found
      (fsvn-select-file-first-file)
      (while (setq file (fsvn-current-filename))
        (when (string= (fsvn-expand-file file) filename)
          (throw 'found t))
        (fsvn-select-file-next-file))
      (goto-char prev)
      nil)))

(defun fsvn-select-file-file-put-mark (filename mark)
  (let (status status1 status2 dirp)
    (save-excursion
      (fsvn-select-file-goto-file filename)
      (setq status (fsvn-select-file-file->status filename))
      (setq status1 (fsvn-status-get-status-1 status))
      (setq status2 (fsvn-status-get-status-2 status))
      (setq dirp (file-directory-p filename))
      ;; status nil means unversioned
      (when (and dirp (or (not status)
                          (memq status1 '(?A ?M ?? ?D))
                          (memq status2 '(?A ?M))))
        (fsvn-select-file-mark-below filename mark))
      (cond
       ((and mark (or (not status)
                      (memq status1 '(?A ??))))
        (fsvn-select-file-mark-above-only filename t))
       ((and (not mark) (eq status1 ?D))
        (fsvn-select-file-mark-above-only filename nil)))
      (fsvn-select-file-point-put-mark-internal mark))))

(defun fsvn-select-file-mark-above-only (file mark)
  (fsvn-select-file-each-file f
    (let ((regexp (concat "^" (regexp-quote f) "/")))
      (when (string-match regexp file)
        (fsvn-select-file-point-put-mark-internal mark)))))

(defun fsvn-select-file-mark-below (dir mark)
  (fsvn-select-file-each-file f
    (let ((regexp (concat "^" (regexp-quote dir) "/")))
      (when (string-match regexp f)
        (fsvn-select-file-point-put-mark-internal mark)))))

(defun fsvn-select-file-draw-root (info)
  (let ((root (fsvn-buffer-repos-root info))
        buffer-read-only)
    (save-excursion
      (goto-char (point-min))
      (when (looking-at "^ Root: .*\n")
        (replace-match "" nil nil nil 0))
      (insert (format " Root: %s\n" root))
      (insert "\n")
      (setq fsvn-buffer-repos-info info))))

(defun fsvn-select-file-draw-applicant (files msg no-msg)
  (setq fsvn-select-file-files-status nil)
  (let (buffer-read-only saved point)
    (goto-char (point-min))
    (forward-line 2)
    (save-restriction
      (narrow-to-region (point) (point-max))
      (delete-region (point-min) (point-max))
      (insert "\n")
      (insert "\n")
      (setq saved (point))
      (mapc
       (lambda (target)
         (fsvn-select-file-draw-target target))
       (fsvn-get-files-status files))
      (save-excursion
        (setq point (point))
        (goto-char (point-min))
        (if (= saved point)
            (insert (format " %s" no-msg))
          (insert (format " %s" msg))))
      (insert "\n")
      (set-buffer-modified-p nil))))

(defun fsvn-select-file-gather-marked-files ()
  (let (ret filename)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when (looking-at fsvn-select-file-re-mark)
          (setq filename (fsvn-current-filename))
          (when filename
            (setq ret (cons (fsvn-expand-file filename) ret))))
        (forward-line 1)))
    (nreverse ret)))

(defun fsvn-select-file-draw-target (target=cl)
  (let (entries)
    (setq entries (fsvn-xml-status->target&cl->entries target=cl))
    (mapcar
     (lambda (entry)
       (fsvn-select-file-draw-internal entry))
     (sort entries
           (lambda (x y)
             (string-lessp
              (fsvn-xml-status->target->entry.path x)
              (fsvn-xml-status->target->entry.path y)))))))

(defun fsvn-select-file-draw-internal (entry)
  (let* ((status (fsvn-status-get-status entry))
         (status1 (fsvn-status-get-status-1 entry))
         (status2 (fsvn-status-get-status-2 entry))
         (mark (or (memq status1 '(?R ?A ?M ?D)) (memq status2 '(?M))))
         (filename (fsvn-select-file-filename entry))
         (dirp (fsvn-file-exact-directory-p filename))
         (linkp (fsvn-file-symlink-p filename))
         cell buffer-read-only)
    (fsvn-select-file-buffer!status entry)
    (fsvn-select-file-set-filename-property filename)
    (insert (format "%c %c %s %s%s\n"
                    (if mark fsvn-mark-mark-char fsvn-space-char)
                    (cond (dirp ?d) (linkp ?l) (t fsvn-space-char))
                    status
                    filename
                    (fsvn-ui-symlink-trailer linkp)))
    (when (and (file-directory-p filename)
               (eq status1 ??))
      (fsvn-select-file-draw-unversioned-directory-files entry))
    (set-buffer-modified-p nil)))

(defun fsvn-select-file-draw-unversioned-directory-files (entry)
  (let* ((dirname (fsvn-xml-status->target->entry.path entry))
         ;; todo deep recursive make slow
         (files (fsvn-select-file-recursive-files dirname)))
    (mapc
     (lambda (filename)
       (let ((dirp (fsvn-file-exact-directory-p filename))
             (linkp (fsvn-file-symlink-p filename)))
         (fsvn-select-file-set-filename-property filename)
         (insert (format "%c %c %s %s%s\n"
                         fsvn-space-char
                         (cond (dirp ?d) (linkp ?l) (t fsvn-space-char))
                         (fsvn-string-rpad "?" fsvn-svn-status-length ?.)
                         filename
                         (fsvn-ui-symlink-trailer linkp)))))
     files)))

(defun fsvn-select-file-filename (entry)
  (let* ((path (fsvn-xml-status->target->entry.path entry))
         (ret (fsvn-file-relative path)))
    ret))

(defun fsvn-select-file-recursive-files (directory)
  (mapcar
   'fsvn-file-relative
   (fsvn-recursive-directory-files directory)))

(defun fsvn-select-file-buffer!status (entry)
  (let ((filename (fsvn-select-file-filename entry))
        cell)
    (setq cell (fsvn-string-assoc filename fsvn-select-file-files-status))
    (unless cell
      (setq cell (cons filename nil))
      (setq fsvn-select-file-files-status (cons cell fsvn-select-file-files-status)))
    (setcdr cell entry)))

(defun fsvn-select-file-buffer->status (entry)
  (fsvn-select-file-file->status (fsvn-select-file-filename entry)))

(defun fsvn-select-file-file->status (filename)
  (cdr (fsvn-string-assoc filename fsvn-select-file-files-status)))

(defun fsvn-select-file-set-filename-property (filename)
  (fsvn-set-filename-property filename)
  (fsvn-string-put-property filename 'face fsvn-directory-face))

(defun fsvn-select-file-command-file ()
  (let ((file (fsvn-current-filename)))
    (unless file
      (error "No file on this line"))
    (list (fsvn-expand-file file))))

(defun fsvn-select-file-remove-file (file)
  (save-excursion
    (when (fsvn-select-file-goto-file file)
      (let ((buffer-read-only))
        (delete-region (line-beginning-position) (line-beginning-position 2)))))
  (fsvn-move-to-filename))

(defun fsvn-select-file-remove-file-hierarchy (file)
  (let ((regexp (format "^%s\\(/\\|$\\)" (directory-file-name file)))
        cur)
    (save-excursion
      (fsvn-select-file-first-file)
      (while (setq cur (fsvn-current-filename))
        (setq cur (fsvn-expand-file cur))
        (if (string-match regexp cur)
            (fsvn-select-file-remove-file cur)
          (fsvn-select-file-next-file))))
    (fsvn-move-to-filename)))

(defun fsvn-select-file-redraw-file (file)
  (let ((status (fsvn-get-file-status file)))
    (save-excursion
      (when (fsvn-select-file-goto-file file)
        (fsvn-select-file-remove-file file))
      (forward-line 0)
      (fsvn-select-file-draw-internal status))))

;; * fsvn-select-file-mode interactive commands

(defun fsvn-select-file-view-file (file)
  (interactive (fsvn-select-file-command-file))
  ;;TODO when open directory not view-mode 
  (let ((buffer (fsvn-get-view-buffer file)))
    (fsvn-view-buffer buffer)))

(defun fsvn-select-file-diff-base (file &optional args)
  "Execute `svn diff' file with base version."
  (interactive (fsvn-select-file-cmd-file "diff" fsvn-default-args-diff))
  (let (diff-args)
    (setq diff-args (list file args))
    (fsvn-diff-start-process diff-args)))

(defun fsvn-select-file-ediff-base (file)
  "Execute ediff file with base version."
  (interactive (fsvn-select-file-command-file))
  (let* ((urlrev (fsvn-url-urlrev file "BASE"))
         (directory-p (file-directory-p file)))
    (fsvn-ediff-between-urlrevs urlrev file directory-p)))

(defun fsvn-select-file-mark ()
  (interactive)
  (fsvn-select-file-point-put-mark t)
  (fsvn-select-file-next-file))

(defun fsvn-select-file-mark-clear ()
  (interactive)
  (fsvn-select-file-point-put-mark nil)
  (fsvn-select-file-next-file))

(defun fsvn-select-file-ignore-this (file)
  "Append to `svn:ignore' property the FILE."
  (interactive (fsvn-select-file-cmd-file))
  (let ((dir (fsvn-file-name-directory2 file)))
    (fsvn-add-prop-svn:ignore
     dir
     (fsvn-file-name-nondirectory file))
    (fsvn-select-file-remove-file-hierarchy file)
    (fsvn-select-file-redraw-file dir)
    (fsvn-move-to-filename)))

(defun fsvn-select-file-revert-this (file &optional args)
  "Execute `revert' to point FILE.
Optional ARGS (with \\[universal-argument]) means read svn subcommand arguments.
"
  (interactive (fsvn-select-file-cmd-file "revert" fsvn-default-args-revert))
  (when (y-or-n-p "Svn: Revert this file? ")
    (let (buffer reverted)
      (setq buffer (fsvn-popup-call-process "revert" args file))
      (setq reverted (fsvn-parse-result-cmd-revert buffer))
      (mapc
       (lambda (file)
         (fsvn-select-file-remove-file file))
       reverted))))

(defun fsvn-select-file-do-delete-this (file)
  "Delete FILE from file-system."
  (interactive (list (fsvn-expand-file (fsvn-current-filename))))
  (when (or (not (fsvn-interactive-p))
            (fsvn-confirm-prompt 'fsvn-select-file-do-delete-this "Delete this file? "))
    (if (fsvn-file-exact-directory-p file)
        (fsvn-delete-directory file)
      (delete-file file))
    (fsvn-select-file-remove-file file)))

(defun fsvn-select-file-delete-this (file &optional args)
  "Execute `delete' to point FILE.
Optional ARGS (with \\[universal-argument]) means read svn subcommand arguments.

This is usefull for missing file (marked `!')
"
  (interactive (list (fsvn-expand-file (fsvn-current-filename)) 
                     (when current-prefix-arg
                       (fsvn-read-svn-subcommand-args "delete" t nil))))
  (if (or (not (fsvn-interactive-p))
          (fsvn-confirm-prompt 'fsvn-select-file-delete-this "Svn: Delete this file? "))
      (progn
        (fsvn-popup-call-process "delete" args (list file))
        (fsvn-select-file-redraw-file file))
    (message "(No svn Delete performed)")))

(defun fsvn-select-file-copy-filename ()
  "Copy file name"
  (interactive)
  (let ((file (fsvn-current-filename)))
    (if (null file)
        (message "No file here.")
      (setq file (fsvn-expand-file file))
      (kill-new file)
      (message "%s" file))))

(defun fsvn-select-file-rename-case-missing-file (file)
  (interactive (list (fsvn-expand-file (fsvn-current-filename)) ))
  (fsvn-rename-case-missing-file file))

(defalias 'fsvn-select-file-next-file 'fsvn-next-file)
(defalias 'fsvn-select-file-previous-file 'fsvn-previous-file)



(defconst fsvn-select-file-mode-menu-spec
  '("fsvn"
    ("File At Point"
     ["View" fsvn-select-file-view-file t]
     ["Diff" fsvn-select-file-diff-base t]
     ["Ediff" fsvn-select-file-ediff-base t]
     ["Mark" fsvn-select-file-mark t]
     ["Unmark" fsvn-select-file-mark-clear t]
     ["Ignore" fsvn-select-file-ignore-this t]
     ["Revert" fsvn-select-file-revert-this t]
     ["Delete" fsvn-select-file-delete-this t]
     ["Rename Case Changed File" fsvn-select-file-rename-case-missing-file t])

    ("General"
     ["Next" fsvn-select-file-next-file t]
     ["Previous" fsvn-select-file-previous-file t]
     ["Copy filename" fsvn-select-file-copy-filename t]
     ["Delete from Filesystem" fsvn-select-file-do-delete-this t])
    ))

(easy-menu-define fsvn-select-file-mode-menu
  fsvn-select-file-mode-map
  "Menu used in Fsvn File Select mode."
  fsvn-select-file-mode-menu-spec)



(provide 'fsvn-select)

;;; fsvn-select.el ends here
