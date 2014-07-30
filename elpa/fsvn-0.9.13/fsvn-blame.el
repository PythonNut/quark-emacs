;;; fsvn-blame.el --- svn subcommand `blame' utility


;;; History:
;; 

;;; Commentary:
;; 

;;; Code:



(require 'fsvn-mode)



(defvar current-prefix-arg)



(defvar fsvn-blame-subwindow-mode-map nil)

(unless fsvn-blame-subwindow-mode-map
  (setq fsvn-blame-subwindow-mode-map
        (let ((map (make-sparse-keymap)))
          (suppress-keymap map)

          map)))

(defcustom fsvn-blame-subwindow-mode-hook nil
  "Run at the very end of `fsvn-blame-subwindow-mode'."
  :group 'fsvn
  :type 'hook)

(defconst fsvn-blame-subwindow-buffer-name " *Fsvn Blame Control*")

(defcustom fsvn-blame-subwindow-height 10
  "Blame minor mode control window height."
  :group 'fsvn
  :type 'integer)

(defconst fsvn-blame-subwindow-buffer-local-variables
  `(
    (font-lock-defaults . '(fsvn-blame-subwindow-font-lock-keywords t nil nil beginning-of-line))
    (font-lock-verbose)
    ))

(defvar fsvn-blame-subwindow-font-lock-keywords 
  (list
   (list "^\\(\\(?:Revision\\|Date\\|Author\\):\\) \\(.*\\)" 
         '(1 fsvn-header-key-face) '(2 fsvn-header-face))
   ))

(defun fsvn-blame-subwindow-mode ()
  "Major mode for viewing Subversion log message that is related `fsvn-blame-minor-mode'.

Entry to this mode calls the value of `fsvn-blame-subwindow-mode-hook'.

Keybindings:
\\{fsvn-blame-subwindow-mode-map}

"
  (fsvn-global-initialize-mode)
  (use-local-map fsvn-blame-subwindow-mode-map)
  (setq major-mode 'fsvn-blame-subwindow-mode)
  (setq mode-name "Fsvn Blame Control")
  (setq buffer-undo-list t)
  (fsvn-make-buffer-variables fsvn-blame-subwindow-buffer-local-variables)
  (font-lock-fontify-buffer))

(defun fsvn-blame-subwindow-insert-message (line string)
  (with-current-buffer (fsvn-blame-get-subwindow-buffer)
    (let ((new-list (copy-sequence fsvn-blame-subwindow-message-list))
          point)
      (cond
       ((null line)
        (if string
            (fsvn-blame-subwindow-redraw-message (split-string string "\n"))
          (fsvn-blame-subwindow-redraw-message nil)))
       (t
        (when (<= (length new-list) line)
          (setq new-list (append new-list (make-list (1+ (- line (length new-list))) nil))))
        (when (setq point (nthcdr line new-list))
          (setcar point string))
        (fsvn-blame-subwindow-redraw-message new-list))))))

(defvar fsvn-blame-subwindow-message-list nil)

(defun fsvn-blame-subwindow-redraw-message (list)
  (with-current-buffer (fsvn-blame-get-subwindow-buffer)
    (unless (equal list fsvn-blame-subwindow-message-list)
      (save-excursion
        (let (buffer-read-only)
          (erase-buffer)
          (goto-char (point-min))
          (mapc
           (lambda (msg)
             (delete-region (line-beginning-position) (line-end-position))
             (when msg
               (insert msg))
             (insert "\n"))
           list)))
      (setq fsvn-blame-subwindow-message-list list)
      (set-buffer-modified-p nil))))

(defconst fsvn-blame-minor-buffer-local-variables
  '(
    (fsvn-blame-blame-logs)
    (fsvn-blame-previous-revision)
    (fsvn-blame-blame-data)
    (fsvn-blame-log-data)
    (fsvn-blame-spent-time . (cons (float-time) nil))
    (fsvn-blame-subwindow-message-list)
    (fsvn-blame-revision-range)
    ))

(defvar fsvn-blame-blame-logs nil)
(defvar fsvn-blame-blame-data nil)
(defvar fsvn-blame-log-data nil)
(defvar fsvn-blame-previous-revision nil)
(defvar fsvn-blame-timer nil)
(defvar fsvn-blame-spent-time nil)
(defvar fsvn-blame-subwindow-buffer nil)
(defvar fsvn-blame-revision-range nil)
(defvar fsvn-blame-revision-let-range nil)

(defun fsvn-blame-minor-cmd-read-with-range-args ()
  (list
   (fsvn-completing-read-revision-range 
    1 "HEAD" 
    (fsvn-blame-buffer-urlrev))))

(define-minor-mode fsvn-blame-minor-mode 
  "Minor mode for visualized Subversion annotate/blame/praise interface.

Keybindings: none

"
  nil nil nil
  (unless (buffer-file-name)
    (fsvn-blame-minor-mode-quit)
    (error "This buffer has no associated file"))
  (if fsvn-blame-minor-mode
      (fsvn-blame-minor-mode-start)
    (fsvn-blame-minor-mode-quit)))

(defun fsvn-blame-minor-mode-with-range (range)
  "Enter `fsvn-blame-minor-mode ' with revision RANGE."
  (interactive (fsvn-blame-minor-cmd-read-with-range-args))
  (let ((fsvn-blame-revision-let-range range))
    (fsvn-blame-minor-mode 1)))

(defun fsvn-blame-minor-mode-start ()
  (fsvn-blame-file-logs fsvn-blame-revision-let-range)
  (fsvn-make-buffer-variables-internal fsvn-blame-minor-buffer-local-variables)
  (setq fsvn-blame-revision-range fsvn-blame-revision-let-range)
  (add-to-list (make-local-variable 'after-change-functions)
               'fsvn-blame-after-change-function)
  (fsvn-blame-activate-timer)
  (add-hook 'kill-buffer-hook 'fsvn-blame-kill-buffer nil t))

(defun fsvn-blame-minor-mode-quit ()
  (interactive)
  (fsvn-kill-buffer-variables-internal fsvn-blame-minor-buffer-local-variables)
  (fsvn-blame-deactivate-timer)
  (fsvn-blame-clear-all-overlay)
  (fsvn-blame-tidy-up-subwindow)
  (fsvn-blame-cleanup-process))

(defun fsvn-blame-after-change-function (start end len)
  (when fsvn-blame-minor-mode
    (let ((ov (fsvn-blame-overlay-at end)))
      (when ov
        (fsvn-blame-split-overlay ov start end)))))

(defun fsvn-blame-split-overlay (ov start end)
  (save-excursion 
    (let ((ov-start (overlay-start ov))
          (ov-end (overlay-end ov))
          top-beg top-fin bottom-beg bottom-fin
          start-fin end-start ov2)
      (goto-char ov-start)
      (setq top-beg (line-beginning-position)
            top-fin (line-end-position))
      (goto-char ov-end)
      (setq bottom-beg (line-beginning-position)
            bottom-fin (line-end-position))
      (goto-char start)
      (forward-line 0)
      (setq start-fin (line-beginning-position))
      (goto-char end)
      (forward-line 1)
      (setq end-start (line-beginning-position))
      (cond
       ((and (<= top-beg start) (<= start top-fin)
             (< bottom-beg end) (<= end bottom-fin))
        ;; change whole overlay
        (delete-overlay ov))
       ((and (<= top-beg start) (<= start top-fin))
        ;; only top of target
        (let ((ov1 (fsvn-blame-overlay-at top-beg)))
          (when ov1
            (move-overlay ov1 (overlay-start ov1) top-beg)))
        (move-overlay ov end-start ov-end))
       ((and (< bottom-beg end) (<= end bottom-fin))
        ;; only bottom of target
        (move-overlay ov ov-start bottom-beg))
       (t
        ;; split to overlays
        (move-overlay ov ov-start start-fin)
        (setq ov2 (copy-overlay ov))
        (move-overlay ov2 end-start ov-end))))))

(defun fsvn-blame-activate-timer ()
  (unless fsvn-blame-timer
    (setq fsvn-blame-timer (run-at-time t 0.5 'fsvn-blame-highlight-in-timer))))

(defun fsvn-blame-deactivate-timer ()
  (when (and fsvn-blame-timer
             (not (catch 'found
                    (mapc
                     (lambda (b)
                       (with-current-buffer b
                         (when fsvn-blame-minor-mode
                           (throw 'found b))))
                     (buffer-list))
                    nil)))
    (cancel-timer fsvn-blame-timer)
    (setq fsvn-blame-timer nil)))

(defun fsvn-blame-kill-buffer ()
  (when fsvn-blame-minor-mode
    (fsvn-blame-minor-mode-quit)))

(defun fsvn-blame-highlight-in-timer ()
  (if (not fsvn-blame-minor-mode)
      (fsvn-blame-tidy-up-subwindow)
    (let* ((overlay
            (catch 'found
              (mapc 
               (lambda (o)
                 (when (overlay-get o 'fsvn-blame-revision)
                   (throw 'found o)))
               (overlays-at (point)))
              nil))
           (data fsvn-blame-blame-logs)
           (prev-rev fsvn-blame-previous-revision)
           (start (car fsvn-blame-spent-time))
           (control-buffer (fsvn-blame-get-subwindow-buffer))
           (target-buffer (current-buffer))
           rev)
      (with-current-buffer control-buffer
        (condition-case err
            (progn
              (let (buffer-read-only)
                (cond
                 ((null data)
                  (if (null (fsvn-blame-get-processes target-buffer))
                      (fsvn-blame-subwindow-insert-message nil "Process exited.")
                    (fsvn-blame-subwindow-insert-message 
                     0 (format "Progressing%s" (make-string (truncate (- (float-time) start)) ?.)))))
                 ((or (null overlay)
                      (null (setq rev (overlay-get overlay 'fsvn-blame-revision))))
                  (erase-buffer)
                  (insert "No revision here.\n"))
                 ((and (eq prev-rev rev)
                       (> (buffer-size) 0))) ;; do nothing
                 (t
                  (erase-buffer)
                  (let ((entry (fsvn-blame-logs-find-logentry data rev))
                        msg date)
                    (setq msg (fsvn-xml-log->logentry=>msg$ entry))
                    (setq date (format-time-string fsvn-generic-datetime-format 
                                                   (fsvn-xml-log->logentry=>date$ entry)))
                    (insert (format "Revision: %d\n" rev))
                    (insert (format "Author: %s\n" (fsvn-xml-log->logentry=>author$ entry)))
                    (insert (format "Date: %s\n" date))
                    (insert "\n")
                    (when msg
                      (insert msg))))))
              (set-buffer-modified-p nil)
              (setq buffer-read-only t)
              (fsvn-blame-minor-setup-subwindow control-buffer)
              (font-lock-fontify-buffer))
          ;;FIXME if error occur
          (error (insert (format "%s" err)))))
      (cond
       ((null rev)
        (fsvn-blame-highlight-revision nil))
       ((eq prev-rev rev))
       (t
        (fsvn-blame-highlight-revision rev)))
      (setq fsvn-blame-previous-revision rev))))

(defun fsvn-blame-logs-find-logentry (data rev)
  (fsvn-find-first
   (lambda (key item)
     (when item
       (= key (fsvn-xml-log->logentry.revision item))))
   rev data))

(defun fsvn-blame-highlight-revision (rev)
  (mapc
   (lambda (o)
     (let ((orev (overlay-get o 'fsvn-blame-revision)))
       (cond
        ((null orev))
        ((eq orev rev)
         (overlay-put o 'face 'highlight))
        (t
         (overlay-put o 'face (overlay-get o 'fsvn-blame-face))))))
   (overlays-in (point-min) (point-max))))

(defun fsvn-blame-defined-colors ()
  "return (fore-color . background-colors)"
  (let ((bgmode (cdr (assoc 'background-mode (frame-parameters)))))
    (if (eq bgmode 'dark)
	(cons "white" (fsvn-blame-color-scale
                       "0c" "04" "24" "1c" "2c" "34" "14" "3c"))
      (cons "black" (fsvn-blame-color-scale
                     "c4" "d4" "cc" "dc" "f4" "e4" "fc" "ec")))))

;; copy from git-blame.el
(defun fsvn-blame-color-scale (&rest elements)
  "Given a list, returns a list of triples formed with each
elements of the list.

a b => bbb bba bab baa abb aba aaa aab"
  (let (ret)
    (mapc 
     (lambda (a)
       (mapc
        (lambda (b)
          (mapc
           (lambda (c)
             (setq ret (cons (concat "#" a b c) ret)))
           elements))
        elements))
     elements)
    ret))

(defun fsvn-blame-make-buffer-overlay (blame-logs diff-alist)
  (let* ((colors (fsvn-blame-defined-colors))
         (fgcolor (car colors))
         (bgcolors (cdr colors))
         (line 0)
         bgcolor face-alist)
    (fsvn-blame-clear-all-overlay)
    (mapc
     (lambda (entry)
       (let ((rev (fsvn-xml-log->logentry.revision entry)))
         (when rev
           (unless (assq rev face-alist)
             (setq bgcolor (nth (% rev (length bgcolors)) bgcolors))
             (setq face-alist
                   (cons
                    (cons rev (list
                               (cons 'foreground-color fgcolor)
                               (cons 'background-color bgcolor)))
                    face-alist))))))
     blame-logs)
    (fsvn-blame-group-by-revision blame-logs face-alist diff-alist)))

(defun fsvn-blame-group-by-revision (blame-logs face-alist diff-alist)
  (let ((wc-line 1)
        (blame-line 1)
        entry prev-end
        curr-rev prev-rev flg loop)
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (setq prev-end (point-min))
        (while (not (eobp))
          (cond
           ;; line added in wc
           ((assq wc-line diff-alist)
            (when flg
              (fsvn-blame-create-overlay-internal 
               prev-end
               (line-beginning-position 0) prev-rev face-alist)
              (setq flg nil))
            (setq prev-end (point)))
           (t
            (setq flg t)
            ;; line removed in wc
            (setq loop (if (rassq blame-line diff-alist) 2 1))
            (fsvn-loop loop
              (setq entry (car blame-logs))
              (setq blame-logs (cdr blame-logs))
              (setq blame-line (1+ blame-line))
              (setq curr-rev (fsvn-xml-log->logentry.revision entry)))
            (when (or (null blame-logs) (and prev-rev (not (eq curr-rev prev-rev))))
              (fsvn-blame-create-overlay-internal prev-end (point) prev-rev face-alist)
              (setq prev-end (point)))))
          (forward-line 1)
          (setq wc-line (1+ wc-line))
          (setq prev-rev curr-rev))
        (fsvn-blame-create-overlay-internal prev-end (point-max) prev-rev face-alist)))))

(defun fsvn-blame-minor-setup-subwindow (subwindow-buffer)
  (let (newwin)
    (when (and (> (window-height) (* fsvn-blame-subwindow-height 2))
               (not (memq subwindow-buffer (mapcar 'window-buffer (window-list)))))
      (setq newwin (split-window nil (- (window-height) fsvn-blame-subwindow-height)))
      (set-window-buffer newwin subwindow-buffer))))

(defun fsvn-blame-tidy-up-subwindow ()
  (let (cwindow)
    (catch 'unable
      (mapc
       (lambda (win)
         (let ((buffer (window-buffer win)))
           (with-current-buffer buffer
             (when fsvn-blame-minor-mode
               (throw 'unable t))
             (when (eq major-mode 'fsvn-blame-subwindow-mode)
               (let (buffer-read-only)
                 (erase-buffer))
               (setq cwindow win)))))
       (window-list))
      (when cwindow
        (delete-window cwindow)))))

(defun fsvn-blame-create-overlay-internal (start end rev face-alist)
  (let (overlay-face overlay)
    (setq overlay-face (cdr (assq rev face-alist)))
    (setq overlay (make-overlay start end nil t t))
    (overlay-put overlay 'fsvn-blame-face overlay-face)
    (overlay-put overlay 'fsvn-blame-name 'fsvn-blame-face-overlay)
    (overlay-put overlay 'fsvn-blame-revision rev)))

(defun fsvn-blame-overlay-at (point)
  (catch 'found
    (mapc
     (lambda (o)
       (when (overlay-get o 'fsvn-blame-revision)
         (throw 'found o)))
     (overlays-at point))
    nil))

(defun fsvn-blame-clear-all-overlay ()
  (save-restriction
    (widen)
    (mapc
     (lambda (o)
       (when (eq (overlay-get o 'fsvn-blame-name) 'fsvn-blame-face-overlay)
         (delete-overlay o)))
     (overlays-in (point-min) (point-max)))))

(defun fsvn-blame-cleanup-process ()
  (mapc
   (lambda (p)
     (delete-process p))
   (fsvn-blame-get-processes)))

(defun fsvn-blame-file-logs (rev-range)
  "Execute `log' and `blame' asynchronous process."
  (let ((log (fsvn-make-temp-buffer))
        (blame (fsvn-make-temp-buffer))
        (urlrev (fsvn-blame-buffer-urlrev))
        (buffer (current-buffer))
        range-arg
        log-proc blame-proc)
    (with-current-buffer (fsvn-blame-get-subwindow-buffer)
      (let (buffer-read-only)
        (erase-buffer)))
    (setq range-arg
          (when rev-range
            (list "--revision" (fsvn-revision-range-to-string rev-range))))
    (setq log-proc (fsvn-start-command "log" log "--xml" "--verbose" range-arg urlrev))
    (setq blame-proc (fsvn-start-command "blame" blame "--xml" range-arg urlrev))
    (mapc
     (lambda (list)
       (let ((p (nth 0 list))
             (name (nth 1 list))
             (line (nth 2 list)))
         (set-process-filter p 'fsvn-blame-process-filter)
         (process-put p 'fsvn-blame-file-buffer buffer)
         (process-put p 'fsvn-blame-process-name name)
         (process-put p 'fsvn-blame-process-line line)
         (fsvn-blame-subwindow-insert-message line (format "%s Received %d bytes" name 0))))
     (list (list blame-proc "Blame" 1) (list log-proc "Log" 2)))
    (set-process-sentinel log-proc
                          (fsvn-blame-create-process-sentinel
                           (fsvn-xml-parse-logentry)
                           fsvn-blame-log-data))
    (set-process-sentinel blame-proc
                          (fsvn-blame-create-process-sentinel
                           (car (fsvn-xml-parse-blame))
                           fsvn-blame-blame-data))))

(defmacro fsvn-blame-create-process-sentinel (parser-form var-symbol)
  `(lambda (proc event)
     (fsvn-process-exit-handler proc event
       (if (= (process-exit-status proc) 0)
           (let ((data ,parser-form)
                 (file-buffer (process-get proc 'fsvn-blame-file-buffer)))
             (when (buffer-live-p file-buffer)
               (with-current-buffer file-buffer
                 (setq ,var-symbol data)
                 (fsvn-blame-merge-and-activate)))
             (kill-buffer (current-buffer)))
         (let ((line (process-get proc 'fsvn-blame-process-line))
               (message (format "Process exited status %d" (process-exit-status proc))))
           (fsvn-blame-subwindow-insert-message line message))))))

(defun fsvn-blame-process-filter (proc event)
  (fsvn-process-event-handler proc event
    (goto-char (point-max))
    (insert event))
  (when (and fsvn-blame-minor-mode
             (eq (process-get proc 'fsvn-blame-file-buffer) (current-buffer)))
    (let ((size (buffer-size (process-buffer proc)))
          (name (process-get proc 'fsvn-blame-process-name))
          (line (process-get proc 'fsvn-blame-process-line)))
      (fsvn-blame-subwindow-insert-message line (format "%s Received %d bytes." name size)))))

(defun fsvn-blame-merge-and-activate ()
  (cond
   ((and fsvn-blame-blame-data
         fsvn-blame-log-data)
    (let ((logs fsvn-blame-log-data)
          (blame fsvn-blame-blame-data)
          (urlrev (fsvn-blame-buffer-urlrev))
          blame-data diff)
      (setq blame-data
            (mapcar
             (lambda (entry)
               (let ((rev (fsvn-xml-blame->target->entry=>commit.revision entry)))
                 (if rev
                     (fsvn-logs-find-logentry logs rev)
                   nil)))
             (fsvn-xml-blame->target->entries blame)))
      (when (fsvn-url-local-p urlrev)
        (setq diff (fsvn-diff-file-alist urlrev)))
      (fsvn-blame-make-buffer-overlay blame-data diff)
      (setq fsvn-blame-blame-logs blame-data)
      (setcdr fsvn-blame-spent-time (float-time))))))

(defun fsvn-blame-get-subwindow-buffer ()
  (let ((buffer fsvn-blame-subwindow-buffer))
    (unless (and buffer (buffer-live-p buffer))
      (setq buffer (get-buffer-create fsvn-blame-subwindow-buffer-name))
      (with-current-buffer buffer
        (fsvn-blame-subwindow-mode)))
    (setq fsvn-blame-subwindow-buffer buffer)))

(defun fsvn-blame-get-processes (&optional buffer)
  (let ((buffer (or buffer (current-buffer))))
    (fsvn-mapitem
     (lambda (p)
       (when (eq (process-get p 'fsvn-blame-file-buffer) buffer)
         p))
     (process-list))))

(defun fsvn-blame-buffer-urlrev ()
  (cond
   ((fsvn-url-local-p (buffer-file-name))
    (buffer-file-name))
   ((fsvn-magic-file-name-absolute-p (buffer-file-name))
    (fsvn-magic-parse-file-name (buffer-file-name)))))



(provide 'fsvn-blame)

;;; fsvn-blame.el ends here
