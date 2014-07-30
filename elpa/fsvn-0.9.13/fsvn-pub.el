;;; fsvn-pub.el --- Fsvn public utilities


;;; History:
;; 

;;; Commentary:
;; 

;;; Code:



(require 'dired)

(require 'fsvn-popup)
(require 'fsvn-browse)
(require 'fsvn-debug)
(require 'fsvn-magic)
(require 'fsvn-ui)
(require 'fsvn-cmd)
(require 'fsvn-env)
(require 'fsvn-minibuf)
(require 'fsvn-fs)
(require 'fsvn-logview)



(defvar iswitchb-buffer-ignore)
(defvar auto-mode-alist)
(defvar current-prefix-arg)
(defvar find-directory-functions)
(defvar file-name-handler-alist)



(defconst fsvn-advised-alist
  '((dired around fsvn-dired-mode)
    (dired-goto-file around fsvn-dired-goto-file-ad)
    (after-find-file around fsvn-after-find-file)
    (vc-find-file-hook after fsvn-ui-fancy-vc-find-file-hook)
    (vc-after-save after fsvn-ui-fancy-vc-after-save)
    (ediff-refresh-mode-lines around fsvn-ui-fancy-ediff-modeline-fixup)
    ))

;; global command
(defun fsvn-cleanup-log-message ()
  "Cleanup cached log messages."
  (interactive)
  (let ((topdir (fsvn-logmessage-directory))
        time file
        (deleted 0)
        (renamed 0))
    (mapc
     (lambda (d)
       (when (file-directory-p d)
         (let (messages msg)
           (mapc
            (lambda (f)
              (setq msg (fsvn-get-file-contents f))
              (cond
               ((fsvn-string-assoc msg messages nil)
                (delete-file f)
                (setq deleted (1+ deleted)))
               (t
                (setq messages (cons msg messages))
                (setq time (format-time-string "%s" (nth 5 (file-attributes f))))
                (unless (string-match (concat "^" time) (fsvn-file-name-nondirectory f))
                  (while (file-exists-p (setq file (make-temp-name (fsvn-expand-file time d)))))
                  (rename-file f file)
                  (setq renamed (1+ renamed))))))
            (directory-files d t dired-re-no-dot)))))
     (directory-files topdir t dired-re-no-dot))
    (message "%d renamed %d deleted." renamed deleted)))

(defun fsvn-show-svn-help (subcommand)
  "Show SUBCOMMAND help."
  (interactive (list (fsvn-read-svn-subcommand)))
  (let ((fsvn-process-environment-lang fsvn-help-locale))
    (fsvn-popup-call-process "help" subcommand)))

(defun fsvn-global-cleanup-buffer ()
  "Cleanup popuped non-active buffers."
  (interactive)
  (when (y-or-n-p "Cleanup popup (process finished) buffer? ")
    (let ((count 0))
      (fsvn-window-with-cleanup
        (setq count (+ count (fsvn-cleanup-temp-buffer)))
        (setq count (+ count (fsvn-cleanup-result-buffer))))
      (cond
       ((= count 0)
        (message "No buffer was killed."))
       ((= count 1)
        (message "killed a buffer."))
       (t
        (message "killed %d buffers." count))))))

(defun fsvn-forward-popup-result-buffer ()
  (interactive)
  (fsvn-cycle-popup-result-buffer))

(defun fsvn-backward-popup-result-buffer ()
  (interactive)
  (fsvn-cycle-popup-result-buffer t))

(defun fsvn-cycle-popup-result-buffer (&optional backward)
  (let ((buffers (fsvn-popup-result-buffer-list))
        comparator)
    (if (null buffers)
        (message "No popup buffer.")
      (setq comparator 
            (if backward
                (lambda (x y) (not (string-lessp (buffer-name x) (buffer-name y))))
              (lambda (x y) (string-lessp (buffer-name x) (buffer-name y)))))
      (setq buffers (sort buffers comparator))
      (let (exists next window)
        (cond
         ((= (length (window-list)) 1)
          (setq next (car buffers)))
         ((setq exists (catch 'found
                         (mapc
                          (lambda (b)
                            (when (memq b (mapcar 'window-buffer (window-list)))
                              (throw 'found b)))
                          buffers)
                         nil))
          (unless (setq next (cadr (memq exists buffers)))
            (setq next (car buffers))))
         (t
          (setq next (car buffers))))
        (cond
         ((= (length (window-list)) 1)
          (split-window)
          (setq window (cadr (window-list))))
         (exists
          (setq window (get-buffer-window exists)))
         (t
          (setq window (cadr (window-list)))))
        (set-window-buffer window next)))))

(defun fsvn-browse/dired-noselect (directory)
  "Wrap `fsvn-browse-wc-noselect' with fallback to `dired'"
  (condition-case err
      (fsvn-browse-wc-noselect directory)
    (error
     (message "fsvn: %s" err)
     (dired directory))))

(defun fsvn-browse-wc-noselect (directory)
  "Like `find-file-noselect'"
  (save-excursion
    (let ((dir (directory-file-name (fsvn-expand-file directory))))
      (when (fsvn-directory-versioned-p dir)
        (fsvn-browse-draw-local-directory dir)
        (set-visited-file-modtime (current-time))
        (setq buffer-read-only t)
        (run-hooks 'fsvn-browse-mode-prepared-hook)
        (current-buffer)))))

(defun fsvn-save-file (urlrev file &optional no-msg revision)
  "Save URLREV as FILE.
Optional argument NO-MSG suppress message.
Optional argument REVISION means point of URLREV log chain."
  (with-temp-buffer
    (if (= (fsvn-call-command "export" (current-buffer)
                              urlrev "--force"
                              (when revision (list "--revision" revision))
                              file) 0)
        (progn
          ;; specification is changed after between 1.6.12 and 1.6.18.
          ;; If exported FILE argument contains revision string, remove revision segment
          ;; by svn command.
          ;; (ex hoge.el@1020 -> hoge.el)
          ;; TODO what should i do when EXPORTED FILE already exists???
          ;;    => about current implementation
          ;;      part of ediff implementation has revision segment.
          ;; after svn 1.7.x specification changed again..
          ;; exported message truncate directory segment from output.
          (goto-char (point-min))
          (unless (looking-at "^A[ \t]+\\(.+\\)")
            (error "Parse error exported information"))
          (let* ((exported (match-string 1))
                 (exported-name
                  (if (file-name-absolute-p exported)
                      (file-name-nondirectory exported)
                    exported)))
            (unless (string= exported-name
                             (file-name-nondirectory file))
              (rename-file exported file t)))
          (unless no-msg
            (message "Save done."))
          ;; return
          t)
      (when (file-exists-p file)
        (delete-file file))
      (unless no-msg
        (message "Save failed."))
      ;; return
      nil)))

(defun fsvn-save-file-background (urlrev file &optional revision)
  "Save URLREV as FILE in background.
Optional argument REVISION means point of URLREV log chain."
  (let* ((buffer (fsvn-make-temp-buffer))
         (proc
          (fsvn-start-command "export" buffer
                              "--quiet" "--force"
                              urlrev (when revision (list "--revision" revision))
                              file)))
    (process-put proc 'fsvn-save-file-name file)
    (set-process-sentinel proc 'fsvn-save-file-sentinel)
    proc))

(defun fsvn-save-file-sentinel (proc event)
  (fsvn-process-exit-handler proc event
    (let ((file (process-get proc 'fsvn-save-file-name)))
      (if (and (= (process-exit-status proc) 0)
               (fsvn-save-file-validate-buffer))
          (message "Save done. \"%s\"" file)
        (when (file-exists-p file)
          (delete-file file))
        (message "Save failed. \"%s\"" file))
      (kill-buffer (current-buffer)))))

(defun fsvn-save-file-validate-buffer ()
  "Validate after cat stderr."
  (save-excursion
    (goto-char (point-min))
    (not (re-search-forward "^svn: warning:" nil t))))

(defun fsvn-cleanup-temp-buffer ()
  (fsvn-cleanup-buffer fsvn-temp-buffer-p))

(defun fsvn-cleanup-result-buffer ()
  (fsvn-cleanup-buffer fsvn-popup-result-buffer-p))

(defvar fsvn-password-prompt-accessible-p t)

(defvar fsvn-authenticate-password-prompt-shown nil)

(defun fsvn-authenticate-repository (repository)
  "Authenticate by `svn' to REPOSITORY."
  (interactive (list (fsvn-completing-read-url "Authenticate URL: " nil t)))
  (if (not fsvn-password-prompt-accessible-p)
      ;;TODO FIXME not works prompt on windows binary.
      (funcall 'fsvn-win-authenticate-repository repository)
    (let ((buffer (fsvn-make-temp-buffer))
          (coding-system-for-write 'unix)
          proc)
      (setq fsvn-authenticate-password-prompt-shown nil)
      (setq proc (fsvn-start-command "info" buffer repository))
      (set-process-sentinel proc 'fsvn-authenticate-sentinel)
      (set-process-filter proc 'fsvn-authenticate-filter)
      (while (eq (process-status proc) 'run)
        (discard-input)
        (sit-for 0.5))
      proc)))

(defun fsvn-authenticate-filter (proc event)
  (fsvn-process-event-handler proc event
    (goto-char (point-max))
    (insert event)
    (let ((prompt (fsvn-parse-result-if-auth-prompt proc)))
      (when prompt
        (setq fsvn-authenticate-password-prompt-shown t)))))

(defun fsvn-authenticate-sentinel (proc event)
  (fsvn-process-exit-handler proc event
    (kill-buffer (current-buffer))
    (cond
     ((/= (process-exit-status proc) 0)
      (message "Failed authenticate."))
     ((not fsvn-authenticate-password-prompt-shown)
      (message "Already authenticated."))
     (t
      (message "Authenticated.")))))



;; global map command
(defun fsvn-checkout (url &optional args)
  "Execute `checkout' URL to current directory.
Optional ARGS (with \\[universal-argument]) means read svn subcommand arguments.
"
  (interactive (fsvn-cmd-read-checkout-args))
  (let ((dir (fsvn-expand-file default-directory)))
    (when (or (= (length (fsvn-directory-files dir)) 0)
              (y-or-n-p "This directory is not empty.  Really checkout? "))
      (fsvn-popup-start-process "checkout" args url dir))))

(defun fsvn-start (repository &optional rev)
  (interactive (let ((url (fsvn-completing-read-url))
                     rev)
                 (when current-prefix-arg
                   (setq rev (fsvn-completing-read-revision nil nil nil url)))
                 (list url rev)))
  (fsvn-browse-switch-directory-buffer (fsvn-url-urlrev repository rev)))

(defun fsvn-import (file url &optional args)
  "Execute `import' FILE to URL.
Optional ARGS (with \\[universal-argument]) means read svn subcommand arguments.
"
  (interactive (fsvn-cmd-read-import-args))
  (let ((info (fsvn-buffer-new-repos-info-upward url))
        (browse-buffer (current-buffer))
        (win-configure (current-window-configuration))
        (msgedit-buffer (fsvn-message-edit-generate-buffer)))
    (unless info
      (error "Unable to get root repository"))
    (with-current-buffer msgedit-buffer
      (fsvn-message-edit-mode)
      (setq fsvn-buffer-repos-info info)
      (fsvn-parasite-import-mode 1)
      (setq fsvn-parasite-import-target-path file)
      (setq fsvn-parasite-import-target-url url)
      (setq fsvn-parasite-import-subcommand-args args)
      (setq fsvn-previous-window-configuration win-configure)
      (run-hooks 'fsvn-message-edit-mode-prepared-hook))
    (fsvn-parasite-setup-message-edit-window msgedit-buffer)))

(defun fsvn-upgrade (directory &optional args)
  "Execute `upgrade' to DIRECTORY.
Optional ARGS (with \\[universal-argument]) means read svn subcommand arguments.
"
  (interactive (fsvn-cmd-read-upgrade-args))
  (let ((proc (fsvn-popup-start-process "upgrade" args)))
    proc))

(defun fsvn-open-repository (urlrev)
  "Open URLREV by repository browser."
  (interactive (list (fsvn-completing-read-urlrev)))
  (fsvn-browse-switch-directory-buffer urlrev))

(defun fsvn-debug-toggle (&optional arg no-msg)
  "Toggle debug output enable/disable.

\(fn ARG)"
  (interactive "P")
  (setq fsvn-debug-enabled
        (fsvn-toggle-command-boolean arg fsvn-debug-enabled))
  (unless no-msg
    (message "fsvn debug %s." (if fsvn-debug-enabled "enabled" "disabled"))))

(defun fsvn-command (subcommand args)
  "Execute `svn SUBCOMMAND ARGS'"
  (interactive (let* ((subcommand (fsvn-read-svn-subcommand))
                      (args (fsvn-read-svn-subcommand-args subcommand)))
                 (list subcommand args)))
  (fsvn-popup-start-process subcommand args))

(defun fsvn-toggle-feature (&optional arg no-msg)
  "Toggle `fsvn' feature enable/disable.

\(fn ARG)"
  (interactive "P")
  (let* ((featured (memq 'fsvn-browse/dired-noselect find-directory-functions))
         (feature (fsvn-toggle-command-boolean arg featured))
         file-handler auto-mode ignore-buffers)
    (mapc
     (lambda (x)
       (let (enabler activator)
         (if feature
             (setq enabler 'ad-enable-advice)
           (setq enabler 'ad-disable-advice))
         (funcall enabler (nth 0 x) (nth 1 x) (nth 2 x))
         (funcall 'ad-activate (nth 0 x))))
     fsvn-advised-alist)
    (setq file-handler (assoc fsvn-magic-file-name-regexp file-name-handler-alist))
    (setq auto-mode (list (concat "@\\(?:" fsvn-revision-regexp "\\)$") 'ignore t))
    (setq ignore-buffers
          (list
           (concat "^" (regexp-quote fsvn-log-sibling-buffer-name) "$")
           (concat "^" (regexp-quote fsvn-log-message-buffer-name) "$")))
    (unless (boundp 'iswitchb-buffer-ignore)
      (setq iswitchb-buffer-ignore nil))
    (cond
     (feature
      ;; for ediff
      (add-to-list 'auto-mode-alist auto-mode)
      ;; update status display
      (add-hook 'after-save-hook 'fsvn-after-save-hook)
      ;; for bookmark or else
      (add-to-list 'find-directory-functions 'fsvn-browse/dired-noselect)
      ;; for magic utility
      (unless file-handler
        (setq file-handler (cons fsvn-magic-file-name-regexp 'fsvn-magic-file-name-handler))
        (add-to-list 'file-name-handler-alist file-handler))
      (add-hook 'pre-command-hook 'fsvn-magic-clear-cache-if-toplevel)
      ;; iswitchb ignore buffers
      (mapc
       (lambda (regexp)
         (add-to-list 'iswitchb-buffer-ignore regexp))
       ignore-buffers))
     (t
      (setq auto-mode-alist (delete auto-mode auto-mode-alist))
      (remove-hook 'after-save-hook 'fsvn-after-save-hook)
      (setq find-directory-functions (delq 'fsvn-browse/dired-noselect find-directory-functions))
      (setq file-name-handler-alist (delq file-handler file-name-handler-alist))
      (remove-hook 'pre-command-hook 'fsvn-magic-clear-cache-if-toplevel)
      (mapc
       (lambda (regexp)
         (setq iswitchb-buffer-ignore (delete regexp iswitchb-buffer-ignore)))
       ignore-buffers)))
    (unless no-msg
      (message "Now fsvn feature `%s'" (if feature "ON" "OFF")))))



(defun fsvn-cmd-read-patch-file ()
  (let* ((default ".patch")
         (patch (fsvn-read-file-name "Patch file: " nil nil nil default)))
    (when (file-exists-p patch)
      (unless (y-or-n-p "File exists. Overwrite? ")
        (fsvn-quit "File already exists")))
    (list (fsvn-expand-file patch))))

(defun fsvn-cmd-read-checkout-args ()
  (let (url args)
    (fsvn-brief-message-showing
     (setq url (fsvn-completing-read-url "Checkout URL: "))
     (fsvn-brief-message-add-message (format "Checkout: %s" url)))
    (setq args (fsvn-cmd-read-subcommand-args "checkout" fsvn-default-args-checkout))
    (list url args)))

(defun fsvn-cmd-read-import-args ()
  (let (file url args)
    (fsvn-brief-message-showing
     (setq file (fsvn-read-file-name "Imported file: " nil nil t))
     (fsvn-brief-message-add-message (format "Imported: %s" file))
     (setq url (fsvn-completing-read-url "Import to URL: "))
     (fsvn-brief-message-add-message (format "Import to: %s" url)))
    (setq args (fsvn-cmd-read-subcommand-args "import" fsvn-default-args-import))
    (list file url args)))

(defun fsvn-cmd-read-upgrade-args ()
  (let* ((dir (fsvn-read-directory-name "Upgrade directory: " nil nil t))
         (args (fsvn-browse-cmd-read-wc-path-with-args "upgrade")))
    (list dir args)))


;; * vc like global utility.

(defun fsvn-vc-print-log ()
  "Execute `log' to current file."
  (interactive)
  (unless buffer-file-name
    (error "Buffer is not associated with a file"))
  (unless (fsvn-deps-file-registered-p buffer-file-name)
    (error "Buffer file is not under versioned"))
  (fsvn-open-logview-mode buffer-file-name nil))

(defun fsvn-vc-commit (&optional args)
  "Prepare `commit' buffer for buffer file.
Optional ARGS (with \\[universal-argument]) means read svn subcommand arguments.
"
  (interactive (list (fsvn-cmd-read-subcommand-args "commit" fsvn-default-args-commit)))
  (fsvn-vc-check-before-commit)
  (let* ((info (fsvn-buffer-new-repos-info default-directory))
         (fsvn-buffer-repos-info info))
    (unless fsvn-buffer-repos-info
      (error "Buffer file is not under versioned"))
    (fsvn-browse-commit-mode (list buffer-file-name) args)))

(defcustom fsvn-vc-commit-non-query-message "*** empty log message ***"
  "Commit message when \\[fsvn-vc-commit-non-query]
Default value is same as vc.el"
  :group 'fsvn
  :type  'string
  )

(defun fsvn-vc-commit-non-query (&optional args)
  "Execute `commit' to editing file.
Optional ARGS (with \\[universal-argument]) means read svn subcommand arguments.
"
  (interactive (list (fsvn-cmd-read-subcommand-args "commit" fsvn-default-args-commit)))
  (fsvn-vc-check-before-commit)
  (let* ((fsvn-buffer-repos-info (fsvn-buffer-new-repos-info default-directory))
         (file buffer-file-name))
    (unless (and fsvn-buffer-repos-info
                 (not (fsvn-file-unversioned-p file)))
      (error "Buffer file is not under versioned"))
    (unless (member "--message" args)
      (setq args (append args (list "--message" (or fsvn-vc-commit-non-query-message "")))))
    (with-temp-buffer
      (unless (= (fsvn-call-command "commit" (current-buffer) file args) 0)
        (error "Commit failed %s" (buffer-string))))
    (fsvn-ui-fancy-redraw)
    (fsvn-save-browse-file-excursion file
      (fsvn-browse-draw-file-status file))
    (message "Successfuly finished.")))

(defun fsvn-vc-check-before-commit ()
  (unless buffer-file-name
    (error "Buffer is not associated with a file"))
  (when (and (buffer-modified-p)
             (y-or-n-p "Buffer modified. Save? "))
    (save-buffer nil)))



(defvar fsvn-initialize-function nil)

(defun fsvn-initialize-loading ()
  (interactive)
  (fsvn-set-command-information)
  (unless (file-directory-p fsvn-home-directory)
    (make-directory fsvn-home-directory t))
  (mapc
   (lambda (dir)
     (let ((dirname (fsvn-expand-file dir fsvn-home-directory)))
       (unless (file-directory-p dirname)
         (make-directory dirname))))
   fsvn-temp-directory-dirs)
  (fsvn-cleanup-temp-directory)
  (fsvn-build-subcommand)
  (when fsvn-initialize-function
    (funcall fsvn-initialize-function))
  (fsvn-toggle-feature t 'no-msg))

(defun fsvn-toggle-command-boolean (optional-arg current-value)
  (cond
   ((and (numberp optional-arg) (= optional-arg 0))
    nil)
   (optional-arg
    t)
   (t
    (not current-value))))

(defun fsvn-working-copy (directory &optional force-reload)
  "Open directory as `fsvn-browse-mode'."
  (let ((canon (directory-file-name (fsvn-expand-file directory))))
    (fsvn-browse-switch-directory-buffer canon)))

(defun fsvn-after-save-hook ()
  (condition-case err
      (let ((file (buffer-file-name)))
        (when (and file
                   (fsvn-file-versioned-directory-p file))
          (cond
           ((version< fsvn-svn-version "1.7")
            (fsvn-after-save-hook-1.7< file))
           (t
            (fsvn-after-save-hook-1.7>= file)))))
    (error nil)))

(defun fsvn-after-save-hook-1.7>= (file)
  (fsvn-save-browse-file-excursion file
    (fsvn-browse-draw-file-status file)))

(defun fsvn-after-save-hook-1.7< (file)
  (let* ((base (fsvn-deps-text-base-file file))
         size1 size2)
    (fsvn-save-browse-file-excursion file
      (if (or (null base)
              (string= 
               (downcase (or (fsvn-deps-get-property "svn:eol-style" file) ""))
               "native"))
          (fsvn-browse-draw-file-status file)
        (setq size1 (fsvn-file-size file)
              size2 (fsvn-file-size base))
        (if (and size1 size2 (/= size1 size2))
            ;; changed file size means certainly modified.
            (fsvn-browse-put-status-if-weak-internal file ?M 0)
          ;; delegate to `status' subcommand.
          (fsvn-browse-draw-file-status file))))))

(defun fsvn-get-exists-browse-buffer (urlrev)
  (catch 'found
    (cond
     ((fsvn-url-repository-p urlrev)
      (fsvn-each-browse-buffer
       (let ((url (fsvn-urlrev-url urlrev))
             (root (fsvn-buffer-repos-root)))
         (when (and root (string-match (concat "^" (regexp-quote root) "\\(.*\\)") url))
           (let ((regexp (format fsvn-browse-re-format-subdir (match-string 1 url))))
             (save-excursion
               (goto-char (point-min))
               (when (re-search-forward regexp nil t)
                 (throw 'found (current-buffer)))))))))
     (t
      (let ((regexp (format fsvn-browse-re-format-subdir (regexp-quote urlrev))))
        (fsvn-each-browse-buffer
         (save-excursion
           (goto-char (point-min))
           (when (re-search-forward regexp nil t)
             (throw 'found (current-buffer))))))))
    nil))

(defun fsvn-local-directory-buffer (directory)
  (let ((dir (directory-file-name (fsvn-expand-file directory))))
    (catch 'found
      (fsvn-each-browse-buffer
       (mapc
        (lambda (subdir)
          (when (string= (car subdir) dir)
            (throw 'found (current-buffer))))
        fsvn-browse-subdir-alist))
      nil)))

(defun fsvn-find-buffer-by-variable (var value)
  (save-excursion
    (catch 'found
      (mapc
       (lambda (b)
         (set-buffer b)
         (when (equal (symbol-value var) value)
           (throw 'found b)))
       (buffer-list))
      nil)))

(defun fsvn-redraw-file-fancy-status (file)
  (let ((buffer (get-file-buffer file)))
    (when buffer
      (with-current-buffer buffer
        (fsvn-ui-fancy-redraw)))))

(defun fsvn-open-logview-mode (urlrev directory-p &optional rev-range count)
  "Open URLREV log buffer.
Argument REV-RANGE revision range cons cell `(start . end)'
Argument COUNT max count of log. If ommited use `fsvn-repository-alist' settings.
"
  (let* ((buffer (fsvn-log-list-get-buffer urlrev))
         ;;FIXME if open log when any other repository..
         (info (fsvn-buffer-repos-info buffer))
         (localp (fsvn-url-local-p urlrev))
         proc win-config)
    (when localp
      (unless (fsvn-get-info-entry urlrev)
        (error "Not a subversion working copy")))
    (unless info
      (setq info (fsvn-buffer-new-repos-info urlrev)))
    (unless info
      (error "Not a subversion repository"))
    (setq win-config (buffer-local-value 'fsvn-previous-window-configuration buffer))
    (unless win-config
      (setq win-config (current-window-configuration)))
    (with-current-buffer buffer
      (let (buffer-read-only)
        (setq fsvn-buffer-repos-info info)
        (setq fsvn-logview-target-directory-p directory-p)
        (setq fsvn-logview-target-urlrev urlrev)
        (setq fsvn-previous-window-configuration win-config)
        (setq fsvn-log-list-target-path
              (if localp
                  (fsvn-wc-file-repository-path urlrev)
                (fsvn-repository-path (fsvn-buffer-repos-root info) urlrev))))
      (setq buffer-read-only t)
      (set-buffer-modified-p nil)
      (setq proc (fsvn-log-list-collect-log-range rev-range count)))
    (switch-to-buffer buffer)
    (fsvn-log-list-setup-window)
    (run-hooks 'fsvn-log-list-mode-prepared-hook)
    proc))



(defun fsvn-open-propview-mode (info urlrev directory-p working-dir)
  (let ((win-configure (current-window-configuration)))
    ;; for proplist mode
    (with-current-buffer (fsvn-proplist-get-buffer)
      (fsvn-proplist-mode)
      (setq fsvn-buffer-repos-info info)
      (setq fsvn-propview-target-urlrev urlrev)
      (setq fsvn-propview-target-directory-p directory-p)
      (setq fsvn-previous-window-configuration win-configure)
      (setq fsvn-proplist-target-mode 'properties)
      (fsvn-set-default-directory working-dir)
      (fsvn-proplist-setup-window)
      (setq fsvn-default-window-configuration (current-window-configuration))
      (setq buffer-read-only t)
      (fsvn-proplist-draw-list urlrev)
      (fsvn-proplist-goto-first-property)
      (fsvn-proplist-draw-value (fsvn-proplist-current-propname))
      (run-hooks 'fsvn-proplist-mode-prepared-hook))
    (switch-to-buffer (fsvn-proplist-get-buffer))))



(defun fsvn-run-recursive-status (directory)
  "Execute recursive `status', and set subordinate directory."
  (let (proc buffer)
    (setq proc (fsvn-recursive-status-running-process directory))
    (if proc
        (when (eq major-mode 'fsvn-browse-mode)
          (setq fsvn-browse-buffer-directories-status-process proc))
      (setq buffer (fsvn-make-temp-buffer))
      (with-current-buffer buffer
        (make-local-variable 'fsvn-recursive-status-parsed))
      (setq proc (fsvn-start-command "status" buffer directory))
      (set-process-sentinel proc 'fsvn-recursive-status-sentinel)
      (set-process-filter proc 'fsvn-recursive-status-filter)
      (process-put proc 'fsvn-recursive-status-top-directory directory)
      (fsvn-recursive-status-set-subordinate-process directory proc)
      proc)))



(provide 'fsvn-pub)

;;; fsvn-pub.el ends here
