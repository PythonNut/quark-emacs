;;; fsvn-proclist.el --- Process control mode for fsvn.el


;;; Commentary:
;; 

;;; Code:



(require 'fsvn-mode)



(defconst fsvn-process-list-re-mark "^[^ \n]")

(defconst fsvn-process-list-buffer-local-variables
  '(
    (fsvn-process-list-process-alist . fsvn-process-list-process-alist)
    (fsvn-process-list-showing-process)
    (fsvn-process-list-processes . fsvn-process-list-processes)
    (fsvn-process-list-timer)
    
    (font-lock-defaults . '(fsvn-process-list-font-lock-keywords nil nil nil beginning-of-line))
    (revert-buffer-function . 'fsvn-process-list-revert-buffer)
    (kill-buffer-hook . kill-buffer-hook)
    ))

(defconst fsvn-process-list-buffer-name " *Fsvn Processes*")
(defconst fsvn-process-list-re-mark-format "^[%s]")
(defvar fsvn-process-list-process-alist nil)
(defvar fsvn-process-list-showing-process nil)
(defvar fsvn-process-list-processes nil)
(defvar fsvn-process-list-display-p-function 'fsvn-process-list-default-display-p)
(defvar fsvn-process-list-timer nil)
(defvar fsvn-process-list-timer-interval 1
  "Seconds of updating buffer interval.")

(defconst fsvn-process-list-column-alist
  '(
    ("M" 1)
    ("Buffer-Size" 12)
    ("Status" -8)
    ("PWD" -50)
    ("Command")))

(defvar fsvn-process-list-font-lock-keywords 
  (list

   (list (concat "\\`" (car (car fsvn-process-list-column-alist)))
         '(".+" (forward-line 0) nil (0 fsvn-header-key-face)))

   (list (concat "^-[- ]+" )
         '(".+" (forward-line 0) nil (0 fsvn-header-face)))

   ;; Fsvn marks.
   (list fsvn-process-list-re-mark '(0 fsvn-mark-face))

   (list (format fsvn-process-list-re-mark-format (char-to-string fsvn-mark-mark-char))
         '(".+" (fsvn-process-list-move-to-command-line) nil (0 fsvn-marked-face)))

   (list (format fsvn-process-list-re-mark-format (char-to-string fsvn-mark-delete-char))
         '(".+" (fsvn-process-list-move-to-command-line) nil (0 fsvn-flagged-face)))
   ))

(defvar fsvn-process-list-mode-map nil)
(unless fsvn-process-list-mode-map
  (setq fsvn-process-list-mode-map
        (let ((map (make-sparse-keymap)))
          (suppress-keymap map)

          (define-key map "U" 'fsvn-process-list-unmark-all)
          (define-key map "d" 'fsvn-process-list-put-delete)
          (define-key map "g" 'revert-buffer)
          (define-key map "m" 'fsvn-process-list-put-mark)
          (define-key map "n" 'fsvn-process-list-next-process)
          (define-key map "p" 'fsvn-process-list-previous-process)
          (define-key map "q" 'fsvn-process-list-quit)
          (define-key map "u" 'fsvn-process-list-unmark)
          (define-key map "x" 'fsvn-process-list-mark-execute)
          (define-key map "\C-c\C-t" 'fsvn-process-list-toggle-show-all)
          (define-key map "\C-c\C-c" 'fsvn-process-list-mark-execute)
          (define-key map "\C-c\C-k" 'fsvn-process-list-quit)
          (define-key map "\C-c\C-p" 'fsvn-process-list-send-password-selected)
          (define-key map "\C-c\C-q" 'fsvn-process-list-quit)
          (define-key map "\C-m" 'fsvn-process-list-show-buffer)
          (define-key map "\C-n" 'fsvn-process-list-next-process)
          (define-key map "\C-p" 'fsvn-process-list-previous-process)

          map)))

(defcustom fsvn-process-list-mode-hook nil
  "Run at the very end of `fsvn-process-list-mode'."
  :group 'fsvn
  :type 'hook)

(defcustom fsvn-process-list-mode-prepared-hook nil
  "Run at the very end of `fsvn-process-list-mode' is prepared."
  :group 'fsvn
  :type 'hook)

(defun fsvn-process-list-mode ()
  "Major mode to control fsvn background processes.

Entry to this mode calls the value of `fsvn-process-list-mode-hook'.

Keybindings:
\\{fsvn-process-list-mode-map}"
  (fsvn-global-initialize-mode)
  (use-local-map fsvn-process-list-mode-map)
  (setq major-mode 'fsvn-process-list-mode)
  (setq mode-name "Fsvn Processes")
  (setq truncate-lines t)
  (setq buffer-undo-list t)
  (fsvn-make-buffer-variables fsvn-process-list-buffer-local-variables)
  (add-hook 'post-command-hook 'fsvn-process-list-after-move nil t)
  (fsvn-browse-setup-mode-line)
  (run-mode-hooks 'fsvn-process-list-mode-hook))

(defun fsvn-process-list-prepared-buffer ()
  (fsvn-sole-major-mode 'fsvn-process-list-mode))

(defun fsvn-process-list-get-buffer ()
  (get-buffer-create fsvn-process-list-buffer-name))

(defun fsvn-process-list-point-process ()
  (catch 'found
    (mapc
     (lambda (o)
       (let ((p (overlay-get o 'fsvn-process-list-process)))
         (when p (throw 'found p))))
     (overlays-at (point)))
    nil))

(defmacro fsvn-process-list-retrieve-mark (column &rest form)
  (declare (indent 1))
  `(save-excursion
     (let (overlays buffer-read-only)
       (forward-line 0)
       (forward-char ,column)
       (setq overlays (overlays-at (point)))
       (unless overlays
         (error "No process here"))
       ,@form
       (mapc
        (lambda (o)
          (move-overlay o (line-beginning-position) (overlay-end o)))
        overlays)
       (set-buffer-modified-p nil))))

(defun fsvn-process-list-draw-list ()
  (let ((buffer (fsvn-process-list-get-buffer))
        processes)
    (with-current-buffer buffer
      (let (buffer-read-only)
        (erase-buffer)
        (fsvn-process-list-mode)
        (mapc 'delete-overlay (overlays-in (point-min) (point-max)))
        (let (header1 header2)
          (mapc
           (lambda (def)
             (let* ((key (car def))
                    (size (fsvn-process-list-column:size key))
                    (name key))
               (when size
                 (setq name (fsvn-filled-column key size)))
               (setq header1 (cons name header1))
               (when size
                 (setq header2 (cons (fsvn-header-tail size) header2)))))
           fsvn-process-list-column-alist)
          (insert (mapconcat 'identity (nreverse header1) " ") "\n")
          (insert (mapconcat 'identity (nreverse header2) " ") " ")
          (fsvn-header-tail-fill-line))
        (mapc
         (lambda (p)
           (when (funcall fsvn-process-list-display-p-function p)
             (fsvn-process-list-insert-process p)
             (setq processes (cons p processes))))
         (fsvn-union (process-list) fsvn-process-list-processes 'memq)))
      (fsvn-process-list-activate-timer)
      (setq buffer-read-only t)
      (setq fsvn-process-list-processes processes)
      ;; Buffer name that starts with space must do this.
      (font-lock-fontify-buffer)
      (run-hooks 'fsvn-process-list-mode-prepared-hook))))

(defun fsvn-process-list-insert-process (process)
  (forward-line 0)
  (let ((cmdline (mapconcat 'identity (process-command process) " "))
        (start (point))
        (buffer (process-buffer process))
        (dir "")
        dirstr size sizestr status end overlay)
    (when (string= cmdline "")
      (setq cmdline (prin1-to-string process)))
    (when (and buffer (buffer-live-p buffer))
      (setq size (buffer-size buffer))
      (setq dir (with-current-buffer buffer
                  (or
                   (and default-directory (abbreviate-file-name default-directory))
                   ""))))
    (setq sizestr (fsvn-filled-column size (fsvn-process-list-column:size "Buffer-Size")))
    (setq status (fsvn-filled-column (process-status process) (fsvn-process-list-column:size "Status")))
    (setq dir (fsvn-string-truncate dir (fsvn-process-list-column:size "PWD")))
    (setq dirstr (fsvn-string-put-property dir 'face fsvn-directory-face))
    (insert (format "  %s %s %s %s\n" sizestr status dirstr cmdline))
    (setq end (point))
    (setq overlay (make-overlay start end nil t nil))
    (overlay-put overlay 'fsvn-process-list-process process)))

(defun fsvn-process-list-deactivate-timer ()
  (cancel-function-timers 'fsvn-process-list-redraw-list)
  (setq fsvn-process-list-timer nil))

(defun fsvn-process-list-activate-timer ()
  (fsvn-process-list-deactivate-timer)
  (setq fsvn-process-list-timer
        (run-at-time t fsvn-process-list-timer-interval 'fsvn-process-list-redraw-list))
  (add-hook 'kill-buffer-hook
            (lambda ()
              (fsvn-process-list-deactivate-timer))))

(defun fsvn-process-list-redraw-list ()
  (let ((buffer (fsvn-process-list-prepared-buffer))
        prev)
    (when (and (buffer-live-p buffer) (eq (current-buffer) buffer))
      (with-current-buffer buffer
        (unwind-protect
            (progn
              (setq prev (point))
              (mapc
               (lambda (p)
                 (cond
                  ((fsvn-process-list-goto-process p)
                   (let ((buffer (process-buffer p))
                         size)
                     ;;TODO
                     (when (looking-at "^..\\([ 0-9]\\{12\\} [ a-z]\\{8\\}\\)")
                       (save-match-data
                         (when (buffer-live-p buffer)
                           (setq size (buffer-size buffer))))
                       (let ((sizestr (fsvn-filled-column size (fsvn-process-list-column:size "Buffer-Size")))
                             (status (fsvn-filled-column (process-status p) (fsvn-process-list-column:size "Status")))
                             buffer-read-only)
                         
                         (replace-match (format "%s %s" sizestr status) nil nil nil 1)
                         (goto-char prev)))))
                  ((funcall fsvn-process-list-display-p-function p)
                   (goto-char (point-max))
                   (let (buffer-read-only)
                     (fsvn-process-list-insert-process p)
                     (setq fsvn-process-list-processes
                           (cons p fsvn-process-list-processes))))))
               (fsvn-union (process-list) fsvn-process-list-processes 'memq)))
          ;;restore point
          (goto-char prev))))))

(defun fsvn-process-list-default-display-p (process)
  (string-match "^fsvn" (process-name process)))

(defun fsvn-process-list-move-to-command-line ()
  (let ((eol (line-end-position)))
    (beginning-of-line)
    (forward-char 75)))

(defun fsvn-process-list-column:size (column)
  (let ((def (assoc column fsvn-process-list-column-alist)))
    (nth 1 def)))

(defun fsvn-process-list-goto-process (process)
  (fsvn-process-list-goto-first)
  (let (p)
    (catch 'found
      (while (and (not (eobp))
                  (setq p (fsvn-process-list-point-process)))
        (when (eq p process)
          (throw 'found p))
        (forward-line 1)))))

(defun fsvn-process-list-goto-first ()
  (goto-char (point-min))
  (while (and (not (eobp))
              (not (fsvn-process-list-point-process)))
    (forward-line 1)))

(defun fsvn-process-list-revert-buffer (ignore-auto noconfirm)
  (let ((p (fsvn-process-list-point-process)))
    (fsvn-process-list-draw-list)
    (when p
      (fsvn-process-list-goto-process p))))

(defun fsvn-process-list-showing-window ()
  (when fsvn-process-list-showing-process
    (let ((alist (mapcar (lambda (w) (cons (window-buffer w) w)) (window-list)))
          (buffer (process-buffer fsvn-process-list-showing-process))
          win)
      (when (setq win (assq buffer alist))
        (cdr win)))))

(defun fsvn-process-list-after-move ()
  (let* ((win (fsvn-process-list-showing-window))
         (process (fsvn-process-list-point-process))
         (buffer (when process (process-buffer process))))
    (when win
      (if (and process (buffer-live-p buffer))
          (set-window-buffer win buffer)
        (delete-window win))
      (setq fsvn-process-list-showing-process process))))

(defun fsvn-process-list-gather-marked-processes (&optional mark)
  (let* ((marker-char (or mark fsvn-mark-mark-char))
         (regex (concat "^" (regexp-quote (char-to-string marker-char))))
         ret temp)
    (save-excursion
      (fsvn-process-list-goto-first)
      (while (not (eobp))
        (when (looking-at regex)
          (setq ret (cons (fsvn-process-list-point-process) ret)))
        (forward-line 1))
      (nreverse ret))))



(defun fsvn-process-list ()
  (interactive)
  (let ((win-configure (current-window-configuration)))
    (setq fsvn-process-list-display-p-function 'fsvn-process-list-default-display-p)
    (fsvn-process-list-draw-list)
    (let ((buffer (fsvn-process-list-get-buffer)))
      (switch-to-buffer buffer)
      (setq fsvn-previous-window-configuration win-configure)
      (fsvn-process-list-goto-first))))

(defun fsvn-process-list-toggle-show-all ()
  (interactive)
  (setq fsvn-process-list-display-p-function
        (if (eq fsvn-process-list-display-p-function 'fsvn-process-list-default-display-p)
            (lambda (p) t)
          'fsvn-process-list-default-display-p))
  (fsvn-process-list-draw-list))

(defun fsvn-process-list-quit ()
  (interactive)
  (fsvn-restore-window-buffer
   (kill-buffer (fsvn-process-list-get-buffer))))

(defun fsvn-process-list-put-delete ()
  (interactive)
  (fsvn-process-list-retrieve-mark 0
    (delete-char 1)
    (insert fsvn-mark-delete-char))
  (fsvn-process-list-next-process))

(defun fsvn-process-list-put-mark ()
  (interactive)
  (fsvn-process-list-retrieve-mark 0
    (delete-char 1)
    (insert fsvn-mark-mark-char))
  (fsvn-process-list-next-process))

(defun fsvn-process-list-unmark ()
  (interactive)
  (fsvn-process-list-retrieve-mark 0
    (delete-char 1)
    (insert ?\s))
  (fsvn-process-list-next-process))

(defun fsvn-process-list-unmark-all (char)
  (interactive "cRemove marks (RET means all): ")
  (let ((all (eq char 13))
        (count 0))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when (fsvn-process-list-point-process)
          (fsvn-process-list-retrieve-mark 0
            (when (or all (eq (char-after) char))
              (delete-char 1)
              (insert ?\s)
              (setq count (1+ count)))))
        (forward-line 1)))
    (message (if (= count 1)
                 "1 mark removed"
               "%d marks removed") count)))

(defun fsvn-process-list-next-process (&optional arg)
  (interactive "p")
  (forward-line arg)
  (fsvn-process-list-after-move))

(defun fsvn-process-list-previous-process (&optional arg)
  (interactive "p")
  (forward-line (- arg))
  (fsvn-process-list-after-move))

(defun fsvn-process-list-show-buffer ()
  (interactive)
  (let ((process (fsvn-process-list-point-process)))
    (when process
      (let ((buffer (process-buffer process)))
        (unless buffer
          (error "This process has no buffer"))
        (display-buffer buffer)
        (with-current-buffer buffer
          (goto-char (point-max))
          (recenter))))
    (setq fsvn-process-list-showing-process process)))

(defun fsvn-process-list-send-password-selected (processes password)
  (interactive (let ((procs (fsvn-process-list-gather-marked-processes)))
                 (unless procs
                   (error "No process was selected"))
                 (list procs (read-passwd "Password: "))))
  (mapc
   (lambda (p)
     (let ((buffer (process-buffer p)))
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           ;; remove password/passphrase prompt
           (goto-char (point-max))
           (forward-line 0)
           (when (looking-at "^.*\\(?:[Pp]assword\\|[Pp]assphrase\\).*")
             (replace-match ""))))
       (when (eq (process-status p) 'run)
         (process-send-string p (concat password "\n")))))
   processes))

(defun fsvn-process-list-mark-execute ()
  (interactive)
  (let ((regexp (format fsvn-process-list-re-mark-format (char-to-string fsvn-mark-delete-char)))
        process buffer-read-only)
    (save-excursion
      (fsvn-process-list-goto-first)
      (while (not (eobp))
        (setq process (fsvn-process-list-point-process))
        (when (and process (looking-at regexp))
          ;; will be updated after timer
          (delete-process process))
        (forward-line 1)))))



(defconst fsvn-process-list-mode-menu-spec
  '("fsvn"
    ("General"
     ["Next" fsvn-process-list-next-process t]
     ["Prev" fsvn-process-list-previous-process t]
     ["Quit" fsvn-process-list-quit t]
     )
    ("Mark"
     ["Execute Delete Mark" fsvn-process-list-mark-execute t]
     ["Unmark" fsvn-process-list-unmark t]
     ["Unmark All" fsvn-process-list-unmark-all t]
     ["Put Delete Mark" fsvn-process-list-put-delete t]
     ["Put Mark" fsvn-process-list-put-mark t]
     )
    ("Misc"
     ["Send Password to Marked" fsvn-process-list-send-password-selected t]
     ["Show Process Buffer" fsvn-process-list-show-buffer t]
     ["Toggle Visibility All Processes" fsvn-process-list-toggle-show-all t]
     )
    ))

(easy-menu-define fsvn-process-list-mode-menu
  fsvn-process-list-mode-map
  "Menu used in Fsvn Processes mode."
  fsvn-process-list-mode-menu-spec)



(provide 'fsvn-proclist)

;;; fsvn-proclist.el ends here
