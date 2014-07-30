;;; fsvn-propview.el --- Subversion property viewer for fsvn.el


;;; History:
;; 

;;; Commentary:
;; 

;;; Code:



(require 'fsvn-mode)



(defvar text-mode-map)



(defvar fsvn-propview-target-urlrev nil)
(defvar fsvn-propview-target-directory-p nil)




(fsvn-defstruct proplist-prop
  name mark recursive-p)

(defconst fsvn-proplist-re-header
  "^ \\(Properties on\\) \\(.+\\)")

(defconst fsvn-proplist-buffer-name "*Fsvn proplist*")

(defconst fsvn-proplist-recursive-mark-char ?R)

(defconst fsvn-proplist-buffer-local-variables
  '(
    (font-lock-defaults . '(fsvn-proplist-font-lock-keywords t nil nil beginning-of-line))
    (revert-buffer-function . 'fsvn-proplist-revert-buffer)
    (fsvn-buffer-repos-info)
    (fsvn-propview-target-urlrev)
    (fsvn-propview-target-directory-p)
    (fsvn-proplist-target-mode)
    ))

(defvar fsvn-proplist-target-mode nil
  "`revprop', `properties'")

(defvar fsvn-proplist-font-lock-keywords 
  (list
   (list fsvn-proplist-re-header '(1 fsvn-header-key-face) '(2 fsvn-header-face))
   (list (concat "^[" (char-to-string fsvn-mark-delete-char) "]")
         '(".+" (fsvn-proplist-move-to-propname) nil (0 fsvn-flagged-face)))
   (list "^.."
         '(".+" (fsvn-proplist-move-to-propname) nil (0 fsvn-keyname-face)))
   ))

(defvar fsvn-proplist-mode-map nil)
(unless fsvn-proplist-mode-map
  (setq fsvn-proplist-mode-map
        (let ((map (make-sparse-keymap)))
          (suppress-keymap map)

          (fsvn-readonly-mode-keymap map)

          (define-key map "\C-c\C-c" 'fsvn-proplist-do-marked-execute)
          (define-key map "\C-c\C-k" 'fsvn-restore-previous-window-setting)
          (define-key map "\C-c\C-l" 'fsvn-restore-default-window-setting)
          (define-key map "\C-c\C-o" 'fsvn-proplist-propedit-window)
          (define-key map "\C-m" 'fsvn-proplist-show-value)
          (define-key map "\C-n" 'fsvn-proplist-next-line)
          (define-key map "\C-p" 'fsvn-proplist-previous-line)
          (define-key map "a" 'fsvn-proplist-add-property)
          (define-key map "d" 'fsvn-proplist-mark-delete)
          (define-key map "e" 'fsvn-proplist-edit-property)
          (define-key map "g" 'revert-buffer)
          (define-key map "n" 'fsvn-proplist-next-line)
          (define-key map "p" 'fsvn-proplist-previous-line)
          (define-key map "q" 'fsvn-restore-previous-window-setting)
          (define-key map "r" 'fsvn-proplist-mark-recursive)
          (define-key map "u" 'fsvn-proplist-mark-unmark)
          (define-key map "x" 'fsvn-proplist-do-marked-execute)

          map)))

(defcustom fsvn-proplist-mode-hook nil
  "Run at the very end of `fsvn-proplist-mode'."
  :group 'fsvn
  :type 'hook)

(defcustom fsvn-proplist-mode-prepared-hook nil
  "Run at the very end of `fsvn-proplist-mode' is prepared."
  :group 'fsvn
  :type 'hook)

;; * fsvn-proplist-mode internal function

(defun fsvn-proplist-mode ()
  "Major mode for browsing Subversion properties.

Entry to this mode calls the value of `fsvn-proplist-mode-hook'.

Keybindings:
\\{fsvn-proplist-mode-map}
"
  (fsvn-global-initialize-mode)
  (use-local-map fsvn-proplist-mode-map)
  (setq major-mode 'fsvn-proplist-mode)
  (setq mode-name "Fsvn Property List")
  (setq truncate-lines t)
  (setq buffer-undo-list t)
  (fsvn-make-buffer-variables fsvn-proplist-buffer-local-variables)
  (run-mode-hooks 'fsvn-proplist-mode-hook))

(defmacro fsvn-proplist-wc-only (&rest form)
  `(if (fsvn-url-repository-p fsvn-propview-target-urlrev)
       (message "Cannot edit repository property.")
     ,@form))

(defun fsvn-proplist-prepared-buffer ()
  (fsvn-sole-major-mode 'fsvn-proplist-mode))

(defun fsvn-proplist-get-buffer ()
  (get-buffer-create fsvn-proplist-buffer-name))

(defun fsvn-proplist-draw-list (file)
  (let (buffer-read-only)
    (erase-buffer)
    (insert (format " Properties on %s\n" file))
    (insert "\n")
    (mapc
     (lambda (propname)
       (fsvn-proplist-insert-propname propname))
     (fsvn-proplist-get-proplist file))))

(defun fsvn-proplist-insert-propname (propname)
  (let (buffer-read-only)
    (goto-char (point-max))
    (insert (format "   %s\n" (fsvn-string-put-property propname 'fsvn-propname t)))))

(defun fsvn-proplist-delete-entry (propname)
  (when (fsvn-proplist-goto-propname propname)
    (let (buffer-read-only)
      (delete-region (line-beginning-position) (line-beginning-position 2)))))

(defun fsvn-proplist-goto-propname (propname)
  (let ((saved (point))
        name)
    (if (catch 'found
          (when (fsvn-proplist-goto-first-property)
            (while (and (not (eobp))
                        (setq name (fsvn-proplist-current-propname)))
              (when (string= name propname)
                (fsvn-proplist-move-to-propname)
                (throw 'found t))
              (forward-line 1))))
        (point)
      (goto-char saved)
      nil)))

(defun fsvn-proplist-setup-window ()
  (let ((list-buffer (fsvn-proplist-get-buffer))
        (value-buffer (fsvn-propedit-get-buffer))
        first-win second-win)
    (delete-other-windows)
    (setq first-win (get-buffer-window (current-buffer)))
    (set-window-buffer first-win list-buffer)
    (setq second-win (split-window))
    (set-window-buffer second-win value-buffer)))

(defun fsvn-proplist-move-to-propname ()
  (let ((eol (line-end-position)))
    (beginning-of-line)
    (let ((change (next-single-property-change (point) 'fsvn-propname nil eol)))
      (cond
       ((and change (< change eol))
        (goto-char change))))))

(defun fsvn-proplist-get-proplist (urlrev)
  (cond
    ((eq fsvn-proplist-target-mode 'revprop)
     (fsvn-get-revprops urlrev))
    ((eq fsvn-proplist-target-mode 'properties)
     (fsvn-get-proplist urlrev))))

(defmacro fsvn-proplist-move-line (&rest form)
  `(progn
     (fsvn-proplist-check-propedit-status)
     ,@form
     (fsvn-proplist-move-to-propname)
     (when (fsvn-proplist-subwindow-display-p)
       (fsvn-proplist-draw-value (fsvn-proplist-current-propname)))))

(defun fsvn-proplist-gather-marked-properties (mark)
  (let (ret)
    (mapc
     (lambda (prop)
       (when (eq (fsvn-struct-proplist-prop-get-mark prop) mark)
         (setq ret (cons prop ret))))
     (fsvn-proplist-gather-properties))
    (nreverse ret)))

(defun fsvn-proplist-gather-properties ()
  (let (ret col1 col2 struct)
    (save-excursion
      (fsvn-proplist-goto-first-property)
      (forward-line 0)
      (while (not (eobp))
        (when (looking-at "^\\(.\\)\\(.\\) ")
          (setq col1 (match-string 1))
          (setq col2 (match-string 2))
          (setq struct (fsvn-struct-proplist-prop-make
                        :name (fsvn-proplist-current-propname)
                        :mark (string-to-char col1)
                        :recursive-p (eq (string-to-char col2) fsvn-proplist-recursive-mark-char)))
          (setq ret (cons struct ret)))
        (forward-line 1))
      (nreverse ret))))

(defun fsvn-proplist-gather-propnames ()
  (mapcar
   (lambda (prop)
     (fsvn-struct-proplist-prop-get-name prop))
   (fsvn-proplist-gather-properties)))

(defun fsvn-proplist-switch-for-edit (&optional modified)
  (cond
   ((fsvn-proplist-subwindow-display-p)
    (set-frame-selected-window (selected-frame) (get-buffer-window (fsvn-propedit-get-buffer))))
   (t
    (switch-to-buffer (fsvn-propedit-get-buffer))))
  (setq buffer-read-only nil)
  (set-buffer-modified-p modified)
  (setq buffer-undo-list nil)
  (message
   (substitute-command-keys "Type \\[fsvn-propedit-save] to finish edit, \
\\[fsvn-propedit-restore-window] to quit edit.")))

(defun fsvn-proplist-goto-first-property ()
  (goto-char (point-min))
  (catch 'found
    (while (not (eobp))
      (forward-line 1)
      (when (fsvn-proplist-move-to-propname)
        (throw 'found t)))
    nil))

(defun fsvn-proplist-check-propedit-status ()
  (when (fsvn-propedit-buffer-modified-p)
    (unless (y-or-n-p "Propedit buffer is changed.  Discard it? ")
      (fsvn-quit "Discard postponed"))
    (fsvn-propedit-buffer-discard-changes)))

(defun fsvn-proplist-current-propname ()
  (save-excursion
    (when (fsvn-proplist-move-to-propname)
      (buffer-substring-no-properties (point) (line-end-position)))))

(defun fsvn-proplist-put-mark (col &optional mark)
  (save-excursion
    (when (fsvn-proplist-move-to-propname)
      (let (buffer-read-only)
        (forward-line 0)
        (forward-char col)
        (delete-char 1)
        (insert (or mark fsvn-space-char))
        (set-buffer-modified-p nil)))))

(defun fsvn-proplist-subwindow-display-p ()
  (let ((buffers (mapcar 'window-buffer (window-list))))
    (memq (fsvn-propedit-prepared-buffer) buffers)))

(defun fsvn-proplist-revert-buffer (ignore-auto noconfirm)
  (let ((propname (fsvn-proplist-current-propname))
        (opoint (point)))
    (fsvn-proplist-draw-list fsvn-propview-target-urlrev)
    (or (and propname
             (fsvn-proplist-goto-propname propname))
        (goto-char opoint))
    (fsvn-proplist-draw-value (fsvn-proplist-current-propname))))

(defun fsvn-proplist-draw-value (propname)
  (let ((file fsvn-propview-target-urlrev)
        (prev-config fsvn-previous-window-configuration)
        (buffer (fsvn-propedit-get-buffer))
        (dirp fsvn-propview-target-directory-p)
        (win-config fsvn-default-window-configuration)
        (info fsvn-buffer-repos-info)
        (working-dir default-directory)
        value)
    (with-current-buffer buffer
      (fsvn-propedit-mode)
      (setq fsvn-previous-window-configuration prev-config)
      (setq fsvn-default-window-configuration win-config)
      (setq fsvn-buffer-repos-info info)
      (setq fsvn-propview-target-urlrev file)
      (setq fsvn-propview-target-directory-p dirp)
      (setq fsvn-propedit-propname propname)
      (setq buffer-read-only t)
      (fsvn-set-default-directory working-dir)
      (let (buffer-read-only)
        (erase-buffer)
        (when propname
          (setq value (fsvn-get-propget file propname))
          (when value
            (insert value)))
        (set-buffer-modified-p nil))
      (run-hooks 'fsvn-propedit-mode-prepared-hook))))

(defun fsvn-proplist-command-propname ()
  (let ((propname (fsvn-proplist-current-propname)))
    (unless propname
      (error "No propname on this line"))
    (list (fsvn-proplist-current-propname))))

;; * fsvn-proplist-mode interactive commands

(defun fsvn-proplist-next-line (&optional arg)
  (interactive "p")
  (fsvn-proplist-move-line
   (forward-line arg)))

(defun fsvn-proplist-previous-line (&optional arg)
  (interactive "p")
  (fsvn-proplist-move-line
   (forward-line (- arg))))

(defun fsvn-proplist-mark-delete (&optional recursive)
  "Put delete mark."
  (interactive "P")
  (fsvn-proplist-wc-only
   ;; not move a point because property delete merely occur.
   (fsvn-proplist-put-mark 0 fsvn-mark-delete-char)
   (when (and recursive fsvn-propview-target-directory-p)
     (fsvn-proplist-put-mark 1 fsvn-proplist-recursive-mark-char))))

(defun fsvn-proplist-mark-recursive ()
  "Put recursive mark."
  (interactive)
  (fsvn-proplist-wc-only
   (if (not fsvn-propview-target-directory-p)
       (message "Cannot put `%c' because not a directory." fsvn-proplist-recursive-mark-char)
     (fsvn-proplist-put-mark 1 fsvn-proplist-recursive-mark-char))))

(defun fsvn-proplist-mark-unmark ()
  "Remove all marks."
  (interactive)
  (fsvn-proplist-put-mark 0)
  (fsvn-proplist-put-mark 1))

(defun fsvn-proplist-show-value ()
  (interactive)
  (fsvn-proplist-check-propedit-status)
  (fsvn-proplist-setup-window)
  (fsvn-proplist-draw-value (fsvn-proplist-current-propname)))

(defun fsvn-proplist-propedit-window ()
  (interactive)
  (let ((edit-buffer (fsvn-propedit-prepared-buffer)))
    (when edit-buffer
      (fsvn-restore-default-window-setting)
      (fsvn-switch-buffer-window edit-buffer))))

(defun fsvn-proplist-add-property (&optional recurse)
  (interactive "P")
  (fsvn-proplist-wc-only
   (let ((propname (fsvn-read-propname fsvn-propview-target-urlrev))
         (file fsvn-propview-target-urlrev)
         ret)
     (when (member propname (fsvn-proplist-get-proplist file))
       (error "Property `%s' is already exists" propname))
     (fsvn-proplist-draw-value propname)
     (fsvn-proplist-switch-for-edit t))))

(defun fsvn-proplist-edit-property (propname)
  (interactive (fsvn-proplist-command-propname))
  (fsvn-proplist-wc-only
   (fsvn-proplist-draw-value propname)
   (fsvn-proplist-switch-for-edit)))

(defun fsvn-proplist-do-marked-execute ()
  (interactive)
  (fsvn-proplist-wc-only
   (let ((file fsvn-propview-target-urlrev)
         (fsvn-call-process-buffer (fsvn-popup-result-create-buffer))
         value-file buffer-read-only)
     (unwind-protect
         (mapc
          (lambda (prop)
            (let ((propname (fsvn-struct-proplist-prop-get-name prop)))
              (cond
               ((eq (fsvn-struct-proplist-prop-get-mark prop) fsvn-mark-delete-char)
                (fsvn-popup-call-process
                 "propdel" propname
                 (when (fsvn-struct-proplist-prop-get-recursive-p prop)
                   "--recursive")
                 file)
                (fsvn-proplist-delete-entry propname))
               ((fsvn-struct-proplist-prop-get-recursive-p prop)
                (unless value-file
                  (setq value-file (fsvn-get-propget-file file propname)))
                (fsvn-popup-call-process
                 "propset" propname
                 "--recursive"
                 "--file" value-file
                 file)))))
          (fsvn-proplist-gather-properties))
       (set-buffer-modified-p nil)))))



(defconst fsvn-propedit-buffer-name "*Fsvn propedit*")
(defconst fsvn-propedit-buffer-local-variables
  '(
    (fsvn-buffer-repos-info)
    (fsvn-propview-target-urlrev)
    (fsvn-propview-target-directory-p)
    (fsvn-propedit-propname)
    (fsvn-propedit-recursive-save)
    ))

(defvar fsvn-propedit-propname nil)
(defvar fsvn-propedit-recursive-save nil)

(defvar fsvn-propedit-mode-map nil)
(unless fsvn-propedit-mode-map
  (setq fsvn-propedit-mode-map
        (let ((map (make-sparse-keymap)))
          (set-keymap-parent map text-mode-map)

          (define-key map "\C-c\C-c" 'fsvn-propedit-save)
          (define-key map "\C-c\C-k" 'fsvn-propedit-restore-window)
          (define-key map "\C-c\C-l" 'fsvn-restore-default-window-setting)
          (define-key map "\C-c\C-o" 'fsvn-propedit-proplist-window)
          (define-key map "\C-c\C-r" 'fsvn-propedit-toggle-recursive)
          (define-key map "\C-x\C-s" 'fsvn-propedit-save)
          map)))

(defcustom fsvn-propedit-mode-hook nil
  "Run at the very end of `fsvn-propedit-mode'."
  :group 'fsvn
  :type 'hook)

(defcustom fsvn-propedit-mode-prepared-hook nil
  "Run at the very end of `fsvn-propedit-mode' is prepared."
  :group 'fsvn
  :type 'hook)

;; * fsvn-propedit-mode internal function

(defun fsvn-propedit-mode ()
  "Major mode for browsing Subversion property its value.

Entry to this mode calls the value of `fsvn-propedit-mode-hook'.

Keybindings:
\\{fsvn-propedit-mode-map}
"
  (fsvn-global-initialize-mode)
  (use-local-map fsvn-propedit-mode-map)
  (setq major-mode 'fsvn-propedit-mode)
  (setq mode-name '("Fsvn Property Edit" (:eval fsvn-propedit-recursive-save)))
  (setq truncate-lines t)
  (setq buffer-undo-list nil)
  (fsvn-make-buffer-variables fsvn-propedit-buffer-local-variables)
  (run-mode-hooks 'fsvn-propedit-mode-hook))

(defun fsvn-propedit-prepared-buffer ()
  (fsvn-sole-major-mode 'fsvn-propedit-mode))

(defun fsvn-propedit-get-buffer ()
  (get-buffer-create fsvn-propedit-buffer-name))

(defun fsvn-propedit-get-value-file ()
  (with-current-buffer (fsvn-propedit-get-buffer)
    (let* ((propname fsvn-propedit-propname)
           (tmpfile (fsvn-make-temp-file))
           (coding-system-for-write (fsvn-prop-file-coding-system propname)))
      (write-region (point-min) (point-max) tmpfile nil 'no-msg)
      tmpfile)))

(defun fsvn-propedit-buffer-modified-p ()
  (buffer-modified-p (fsvn-propedit-get-buffer)))

(defun fsvn-propedit-buffer-discard-changes ()
  (with-current-buffer (fsvn-propedit-get-buffer)
    (set-buffer-modified-p nil)))

(defun fsvn-propedit-clear ()
  (with-current-buffer (fsvn-propedit-get-buffer)
    (let (buffer-read-only)
      (erase-buffer))))

;; * fsvn-propedit-mode interactive commands

(defun fsvn-propedit-save ()
  (interactive)
  (unless (buffer-modified-p)
    (error "Value unchanged"))
  (let ((file (fsvn-propedit-get-value-file))
        (propname fsvn-propedit-propname)
        (target fsvn-propview-target-urlrev)
        (recursive fsvn-propedit-recursive-save)
        ret output)
    (with-temp-buffer
      (setq ret (fsvn-call-command
                 "propset" (current-buffer)
                 propname
                 "--file" file
                 (when recursive
                   "--recursive")
                 ;;svn: --encoding option applies only to textual Subversion-controlled properties
                 (when (fsvn-propname-svn-texture-p propname)
                   (list "--encoding" (fsvn-coding-system-name (fsvn-prop-file-coding-system propname))))
                 target))
      (setq output (buffer-string)))
    (unless (= ret 0)
      (error "Error occur while setting property.  See below message\n\n%s" output))
    (set-buffer-modified-p nil)
    (setq buffer-read-only t)
    (setq buffer-undo-list nil)
    (with-current-buffer (fsvn-proplist-get-buffer)
      (let (propnames)
        (setq propnames (fsvn-proplist-gather-propnames))
        ;; svn:eol-style like non applicable property for directory, but can set recursively.
        (when (and (member propname (fsvn-proplist-get-proplist fsvn-propview-target-urlrev))
                   (not (member propname propnames)))
          (fsvn-proplist-insert-propname propname))
        (fsvn-proplist-draw-value propname)))
    (fsvn-save-browse-file-excursion target
      (fsvn-browse-draw-file-status target))
    (fsvn-restore-default-window-setting)))

(defun fsvn-propedit-restore-window ()
  (interactive)
  (if buffer-read-only
      (fsvn-restore-previous-window-setting)
    (fsvn-restore-default-window-setting))
  (when (eq major-mode 'fsvn-proplist-mode)
    (fsvn-proplist-draw-value (fsvn-proplist-current-propname))))

(defun fsvn-propedit-toggle-recursive (&optional arg)
  (interactive "P")
  (fsvn-toggle-mode-line-variable
   arg 'fsvn-propedit-recursive-save
   " (Recursive)" "recursive save"))

(defun fsvn-propedit-proplist-window ()
  (interactive)
  (let ((list-buffer (fsvn-proplist-prepared-buffer)))
    (when list-buffer
      (fsvn-restore-default-window-setting)
      (fsvn-switch-buffer-window list-buffer))))



(defconst fsvn-proplist-mode-menu-spec
  '("fsvn"
    ["Mark Delete" fsvn-proplist-mark-delete t]
    ["Mark Recursive" fsvn-proplist-mark-recursive t]
    ["Unmark" fsvn-proplist-mark-unmark t]
    ["Edit" fsvn-proplist-edit-property t]
    ["Add" fsvn-proplist-add-property t]
    ["Show" fsvn-proplist-show-value t]
    ["Execute to Marked Properties" fsvn-proplist-do-marked-execute t]
    ("Move"
     ["Prev" fsvn-proplist-previous-line t]
     ["Next" fsvn-proplist-next-line t]
     ["Cycle Window" fsvn-proplist-propedit-window t]
     )
    ))

(easy-menu-define fsvn-proplist-mode-menu
  fsvn-proplist-mode-map
  "Menu used in Fsvn Property List mode."
  fsvn-proplist-mode-menu-spec)

(defconst fsvn-propedit-mode-menu-spec
  '("fsvn"
    ["Save" fsvn-propedit-save t]
    ["Toggle Recursive" fsvn-propedit-toggle-recursive t]
    ["Cycle Window" fsvn-propedit-proplist-window t]
    ["Restore Window Setting" fsvn-propedit-restore-window t]
    ))

(easy-menu-define fsvn-propedit-mode-menu
  fsvn-propedit-mode-map
  "Menu used in Fsvn Property Edit mode."
  fsvn-propedit-mode-menu-spec)



(provide 'fsvn-propview)

;;; fsvn-propview.el ends here
