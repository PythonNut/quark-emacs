;; -*- lexical-binding: t -*-
(eval-when-compile
  (require 'config-macros)
  (require 'config-package))
(require 'cl-lib)

(setq history-length 100
      history-delete-duplicates t)

(defvar file-name-mode-alist nil)

(add-hook
 'after-change-major-mode-hook
 (my/defun-as-value my/register-file-name-mode-maybe ()
   (when (and buffer-file-name
              (not
               (file-name-extension
                buffer-file-name))
              (not (eq major-mode 'fundamental-mode)))
     (push (cons buffer-file-name major-mode) file-name-mode-alist)
     (push (cons buffer-file-name major-mode) auto-mode-alist))))

(defun my/compress-alist (alist)
  "Remove shadowed keys from `alist'"
  (let ((result))
    (dolist (elem alist)
      (unless (assoc (car elem) result)
        (push elem result)))
    (nreverse result)))

(use-package x-win
  :ensure nil
  :config
  (eval-when-compile
    (with-demoted-errors "Load error: %s"
      (require 'x-win)
      (require 'el-patch)))

  (el-patch-defun emacs-session-filename (session-id)
    "Construct a filename to save the session in based on SESSION-ID.
Return a filename in `user-emacs-directory', unless the session file
already exists in the home directory."
    (let ((basename (concat "session." session-id)))
      (el-patch-swap
        (locate-user-emacs-file basename
                                (concat ".emacs-" basename))
        (expand-file-name basename (locate-user-emacs-file "data"))))))

(use-package savehist
  :ensure nil
  :init
  (autoload 'savehist-minibuffer-hook "savehist")
  (autoload 'savehist-autosave "savehist")
  (autoload 'savehist-uninstall "savehist")

  (el-patch-feature savehist)

  (el-patch-defcustom savehist-file
    (el-patch-swap (locate-user-emacs-file "history" ".emacs-history")
                   (locate-user-emacs-file "data/savehist"))

    "File name where minibuffer history is saved to and loaded from.
The minibuffer history is a series of Lisp expressions loaded
automatically when Savehist mode is turned on.  See `savehist-mode'
for more details."
    :type 'file)

  (el-patch-defcustom savehist-autosave-interval
    (* (el-patch-swap 5 2) 60)
    "The interval between autosaves of minibuffer history.
If set to nil, disables timer-based autosaving."
    :type '(choice (const :tag "Disabled" nil)
                   (integer :tag "Seconds")))

  (el-patch-defcustom savehist-additional-variables
    (el-patch-swap ()
                   '(kill-ring
                     file-name-history
                     file-name-mode-alist
                     search-ring
                     regexp-search-ring))
    "List of additional variables to save.
Each element is a symbol whose value will be persisted across Emacs
sessions that use Savehist.  The contents of variables should be
printable with the Lisp printer.  You don't need to add minibuffer
history variables to this list, all minibuffer histories will be
saved automatically as long as `savehist-save-minibuffer-history' is
non-nil.

User options should be saved with the Customize interface.  This
list is useful for saving automatically updated variables that are not
minibuffer histories, such as `compile-command' or `kill-ring'."
    :type '(repeat variable))

  (el-patch-defvar savehist-timer nil)
  (el-patch-defvar savehist-loaded nil
    "Whether the history has already been loaded.
This prevents toggling Savehist mode from destroying existing
minibuffer history.")

  (el-patch-defun savehist-install ()
    "Hook Savehist into Emacs.
Normally invoked by calling `savehist-mode' to set the minor mode.
Installs `savehist-autosave' in `kill-emacs-hook' and on a timer.
To undo this, call `savehist-uninstall'."
    (add-hook 'minibuffer-setup-hook #'savehist-minibuffer-hook)
    (add-hook 'kill-emacs-hook #'savehist-autosave)
    ;; Install an invocation of savehist-autosave on a timer.  This
    ;; should not cause noticeable delays for users -- savehist-autosave
    ;; executes in under 5 ms on my system.
    (when (and savehist-autosave-interval
	       (null savehist-timer))
      (setq savehist-timer
	    (run-with-timer savehist-autosave-interval
			    savehist-autosave-interval #'savehist-autosave))))

  (el-patch-define-minor-mode savehist-mode
    "Toggle saving of minibuffer history (Savehist mode).

When Savehist mode is enabled, minibuffer history is saved
to `savehist-file' periodically and when exiting Emacs.  When
Savehist mode is enabled for the first time in an Emacs session,
it loads the previous minibuffer histories from `savehist-file'.
The variable `savehist-autosave-interval' controls the
periodicity of saving minibuffer histories.

If `savehist-save-minibuffer-history' is non-nil (the default),
all recorded minibuffer histories will be saved.  You can arrange
for additional history variables to be saved and restored by
customizing `savehist-additional-variables', which by default is
an empty list.  For example, to save the history of commands
invoked via \\[execute-extended-command], add `command-history' to the list in
`savehist-additional-variables'.

Alternatively, you could customize `savehist-save-minibuffer-history'
to nil, and add to `savehist-additional-variables' only those
history variables you want to save.

To ignore some history variables, add their symbols to the list
in `savehist-ignored-variables'.

This mode should normally be turned on from your Emacs init file.
Calling it at any other time replaces your current minibuffer
histories, which is probably undesirable."
    :global t
    (if (not savehist-mode)
        (savehist-uninstall)
      (when (and (not savehist-loaded)
	         (file-exists-p savehist-file))
        (condition-case errvar
	    (progn
	      ;; Don't set coding-system-for-read -- we rely on the
	      ;; coding cookie to convey that information.  That way, if
	      ;; the user changes the value of savehist-coding-system,
	      ;; we can still correctly load the old file.
	      (load savehist-file nil (not (called-interactively-p 'interactive)))
	      (setq savehist-loaded t))
	  (error
	   ;; Don't install the mode if reading failed.  Doing so would
	   ;; effectively destroy the user's data at the next save.
	   (setq savehist-mode nil)
	   (savehist-uninstall)
	   (signal (car errvar) (cdr errvar)))))
      (savehist-install)))

  (savehist-mode +1)
  (setq auto-mode-alist (append auto-mode-alist file-name-mode-alist))

  :config
  (eval-when-compile
    (with-demoted-errors "Load error: %s"
      (require 'cl-lib)))

  (add-hook
   'savehist-save-hook
   (my/defun-as-value my/session-prepare/file-mode-alist ()
     (setq file-name-mode-alist
           (nreverse
            (let ((res)
                  (orig (my/compress-alist file-name-mode-alist)))
              (dotimes (_ (min history-length (length orig)) res)
                (push (pop orig) res)))))))



  (define-advice savehist-save
      (:around (old-fun &rest args) remove-text-properties)
    (let ((vars (append savehist-minibuffer-history-variables
                        savehist-additional-variables)))
      (cl-progv vars
          (mapcar (lambda (sym)
                    (if (boundp sym)
                        (let ((value (symbol-value sym)))
                          (if (and value
                                   (listp value)
                                   (stringp (car value)))
                              (mapcar #'substring-no-properties
                                      value)
                            value))
                      nil))
                  vars)
        (apply old-fun args)))))

(use-package saveplace
  :ensure nil
  :init
  (autoload 'save-place-find-file-hook "saveplace")
  (autoload 'save-place-dired-hook "saveplace")
  (autoload 'save-place-kill-emacs-hook "saveplace")
  (autoload 'load-save-place-alist-from-file "saveplace")

  (el-patch-feature saveplace)

  (el-patch-defcustom save-place-ignore-files-regexp
    "\\(?:COMMIT_EDITMSG\\|hg-editor-[[:alnum:]]+\\.txt\\|svn-commit\\.tmp\\|bzr_log\\.[[:alnum:]]+\\)$"
    "Regexp matching files for which no position should be recorded.
Useful for temporary file such as commit message files that are
automatically created by the VCS.  If set to nil, this feature is
disabled, i.e., the position is recorded for all files."
    :version "24.1"
    :type 'regexp)

  (el-patch-defvar save-place-loaded nil
    "Non-nil means that the `save-place-file' has been loaded.")

  (el-patch-defun save-place-to-alist ()
    ;; put filename and point in a cons box and then cons that onto the
    ;; front of the save-place-alist, if save-place is non-nil.
    ;; Otherwise, just delete that file from the alist.
    ;; first check to make sure alist has been loaded in from the master
    ;; file.  If not, do so, then feel free to modify the alist.  It
    ;; will be saved again when Emacs is killed.
    (el-patch-remove (or save-place-loaded (load-save-place-alist-from-file)))
    (let* ((directory (and (derived-mode-p 'dired-mode)
                           (boundp 'dired-subdir-alist)
                           dired-subdir-alist
                           (dired-current-directory)))
           (item (or buffer-file-name
                     (and directory
                          (expand-file-name (if (consp directory)
                                                (car directory)
                                              directory))))))
      (when (and item
                 (or (not save-place-ignore-files-regexp)
                     (not (string-match save-place-ignore-files-regexp
                                        item))))
        (el-patch-add (or save-place-loaded (load-save-place-alist-from-file)))
        (let ((cell (assoc item save-place-alist))
              (position (cond ((eq major-mode 'hexl-mode)
                               (with-no-warnings
                                 (1+ (hexl-current-address))))
                              ((and (derived-mode-p 'dired-mode) directory)
                               (let ((filename (dired-get-filename nil t)))
                                 (if filename
                                     `((dired-filename . ,filename))
                                   (point))))
                              (t (point)))))
          (if cell
              (setq save-place-alist (delq cell save-place-alist)))
          (if (and save-place
                   (not (and (integerp position)
                             (= position 1)))) ;; Optimize out the degenerate case.
              (setq save-place-alist
                    (cons (cons item position)
                          save-place-alist)))))))

  (el-patch-defun save-place--setup-hooks (add)
    (cond
     (add
      (add-hook 'find-file-hook #'save-place-find-file-hook t)
      (add-hook 'dired-initial-position-hook #'save-place-dired-hook)
      (unless noninteractive
        (add-hook 'kill-emacs-hook #'save-place-kill-emacs-hook))
      (add-hook 'kill-buffer-hook #'save-place-to-alist))
     (t
      ;; We should remove the hooks, but only if save-place-mode
      ;; is nil everywhere.  Is it worth the trouble, tho?
      ;; (unless (or (default-value 'save-place-mode)
      ;;             (cl-some <save-place-local-mode-p> (buffer-list)))
      ;;   (remove-hook 'find-file-hook #'save-place-find-file-hook)
      ;;   (remove-hook 'dired-initial-position-hook #'save-place-dired-hook)
      ;;   (remove-hook 'kill-emacs-hook #'save-place-kill-emacs-hook)
      ;;   (remove-hook 'kill-buffer-hook #'save-place-to-alist))
      )))

  (el-patch-define-minor-mode save-place-mode
    "Non-nil means automatically save place in each file.
This means when you visit a file, point goes to the last place
where it was when you previously visited the same file."
    :global t
    :group 'save-place
    (save-place--setup-hooks save-place-mode))

  (save-place-mode +1)

  :config
  (setq save-place-file (locate-user-emacs-file "data/places")))

(use-package recentf
  :commands (recentf-track-opened-file
             recentf-track-closed-file
             recentf-save-list)
  :init
  (el-patch-feature recentf)

  (el-patch-defconst recentf-used-hooks
    '((find-file-hook       recentf-track-opened-file)
      (write-file-functions recentf-track-opened-file)
      (kill-buffer-hook     recentf-track-closed-file)
      (kill-emacs-hook      recentf-save-list))
    "Hooks used by recentf.")

  (add-hook
   'emacs-startup-hook
   (my/defun-as-value my/recentf-onetime-setup ()
     (dolist (hook recentf-used-hooks) (apply #'add-hook hook))))

  :config
  (defun quark/recentf-keep-predicate (file)
    (cond
     ((file-remote-p file nil t) (file-readable-p file))
     ((file-remote-p file) t)
     ((file-readable-p file))))

  (setq recentf-save-file (locate-user-emacs-file "data/recentf")
        recentf-max-saved-items 1000
        recentf-max-menu-items 50
        recentf-auto-cleanup 30
        recentd-keep '(quark/recentf-keep-predicate))
  (add-to-list 'recentf-exclude (eval-when-compile
                                  (concat (rx line-start)
                                          (expand-file-name
                                           (locate-user-emacs-file "elpa")))))

  ;; TODO: Yeah so this is actually a horrifying hack.
  (dolist (hook recentf-used-hooks) (apply #'remove-hook hook))
  (recentf-mode +1)

  (el-patch-defun recentf-save-list ()
    "Save the recent list.
Write data into the file specified by `recentf-save-file'."
    (interactive)
    (condition-case error
        (with-temp-buffer
          (erase-buffer)
          (set-buffer-file-coding-system recentf-save-file-coding-system)
          (insert (format-message recentf-save-file-header
				  (current-time-string)))
          (recentf-dump-variable 'recentf-list recentf-max-saved-items)
          (recentf-dump-variable 'recentf-filter-changer-current)
          (insert "\n\n;; Local Variables:\n"
                  (format ";; coding: %s\n" recentf-save-file-coding-system)
                  ";; End:\n")
          (el-patch-swap
            (write-file (expand-file-name recentf-save-file))
            (let ((true-file (expand-file-name recentf-save-file))
                  (temp-file (make-temp-file "historian")))
              (write-region nil nil temp-file nil true-file)
              (rename-file temp-file true-file t)))
          (when recentf-save-file-modes
            (set-file-modes recentf-save-file recentf-save-file-modes))
          nil)
      (error
       (warn "recentf mode: %s" (error-message-string error)))))

  (let ((recentf-autosave-timer nil))
    (define-advice recentf-track-opened-file
        (:after (&rest _args) autosave)
      (when (timerp recentf-autosave-timer)
        (cancel-timer recentf-autosave-timer))
      (setq recentf-autosave-timer
            (run-with-idle-timer
             1 nil
             (lambda ()
               (let ((inhibit-message t))
                 (recentf-save-list)))))))

  (define-advice recentf-cleanup
       (:around (old-fun &rest args) inhibit-message)
    (let ((inhibit-message t))
      (apply old-fun args))))

(use-package desktop
  :ensure nil
  :init
  (defun desktop-autosave (&optional arg)
    (interactive "p")
    (if (called-interactively-p 'any)
        (if (= arg 4)
            (let* ((desktop-base-file-name
                    (read-from-minibuffer "Session name: "))
                   (desktop-base-lock-name
                    (concat
                     desktop-base-file-name
                     ".lock")))
              (desktop-save-in-desktop-dir))
          (desktop-save-in-desktop-dir)))
    (cl-letf ((inhibit-message t)
              ((symbol-function #'y-or-n-p) (lambda (_prompt) t)))
      (desktop-save-in-desktop-dir)))

  (defun desktop-load (&optional arg)
    (interactive "p")
    (if (= arg 4)
        (let* ((files (cl-remove-if
                       (lambda (item)
                         (string-match-p
                          (rx line-start
                              (or (and "." (zero-or-more not-newline))
                                  (and (zero-or-more not-newline) ".lock"))
                              line-end)
                          item))
                       (directory-files desktop-dirname)))
               (desktop-base-file-name (completing-read
                                        "Enter a desktop name: "
                                        files
                                        nil t))
               (desktop-base-lock-name
                (concat desktop-base-file-name ".lock")))
          (desktop-read)
          (desktop-remove))
      (desktop-read)))

  :config
  (setq desktop-dirname (locate-user-emacs-file "data/desktop/")
        desktop-path (list desktop-dirname)
        desktop-base-file-name "emacs-desktop"
        desktop-base-lock-name "emacs-desktop.lock")

  ;; don't let a dead emacs own the lockfile
  (define-advice desktop-owner
      (:filter-return (pid) check-alive)
    (when pid
      (let* ((attributes (process-attributes pid))
             (cmd (cdr (assoc 'comm attributes))))
        (when (and cmd (string-prefix-p "emacs" cmd))
          pid))))

  (define-advice desktop-claim-lock
      (:override (&optional dirname) inhibit-message)
    (write-region (number-to-string (emacs-pid)) nil
                   (desktop-full-lock-name dirname) nil 1)))

(use-package server
  :ensure nil
  :init
  (defun send-file-to-server (&optional arg)
    (interactive)
    (server-eval-at (concat "server"
                            (or arg
                                (read-from-minibuffer "Server ID: ")))
                    `(progn (find-file ,(buffer-file-name))
                            nil)))

  (defun send-desktop-to-server ()
    (interactive)
    (save-some-buffers t)
    (let* ((desktop-base-file-name "transplant")
           (desktop-base-lock-name
            (concat desktop-base-file-name ".lock")))
      (desktop-save-in-desktop-dir)
      (desktop-release-lock))
    (server-eval-at (concat "server"
                            (read-from-minibuffer "Server ID: "))
                    `(let* ((desktop-base-file-name "transplant")
                            (desktop-base-lock-name
                             (concat desktop-base-file-name ".lock")))
                       (desktop-clear)
                       (desktop-read)
                       (desktop-remove)))
    (kill-emacs))

  (defun send-all-files-to-server ()
    (let ((count 1))
      (catch 'done
        (while t
          (if (server-running-p
               (concat "server" (number-to-string count)))
              (throw 'done server-name)
            (cl-incf count))
          (when (> 20 count)
            (throw 'done nil))))
      (dolist (buf (buffer-list))
        (with-current-buffer buf
          (when (buffer-file-name (current-buffer))
            (send-file-to-server (number-to-string count)))))
      (kill-emacs)))

  (when (member "-P" command-line-args)
    (setq debug-on-error t)
    (delete "-P" command-line-args)
    (require 'server)
    (add-hook 'emacs-startup-hook #'send-all-files-to-server))

  (when (display-graphic-p)
    (idle-job-add-function #'server-start 'append))

  :config
  (define-advice server-start
      (:around (old-fun &rest args) auto-server-name)
    (catch 'done
      (let ((count 1))
        (while t
          (if (server-running-p server-name)
              (progn
                (setq server-name (concat "server" (number-to-string count)))
                (cl-incf count))
            (apply old-fun args)
            (throw 'done server-name)))))))

(use-package atomic-chrome
  :commands (atomic-chrome-start-server
             atomic-chrome-stop-server)
  :init
  (setq atomic-chrome-default-major-mode 'markdown-mode)

  (when (display-graphic-p)
    (my/onetime-setup atomic-chrome
      :hook 'focus-out-hook
      (atomic-chrome-start-server))

    (idle-job-add-function #'atomic-chrome-start-server 'append))

  :config
  (with-eval-after-load 'evil
    ;; This makes sense because the browser is in "insert" state, so
    ;; this keeps things consistent.
    (evil-set-initial-state 'atomic-chrome-edit-mode 'insert))

  ;; At some point, this should go somewhere more general
  (defun my/get-active-window-id ()
    (cond ((executable-find "xprop")
           (let ((output
                  (with-output-to-string
                    (with-current-buffer
                        standard-output
                      (call-process "xprop" nil t nil
                                    "-root"
                                    "_NET_ACTIVE_WINDOW")))))
             (when (string-match (rx space "0x" (group (one-or-more hex)) "\n")
                                 output)
               (string-to-number (match-string 1 output) 16))))
          ((executable-find "xdotool")
           (string-to-number
            (string-trim
             (with-output-to-string
               (with-current-buffer
                   standard-output
                 (call-process "xdotool" nil t nil
                               "getactivewindow"))))))))

  (defun my/get-emacs-window-id ()
    (string-to-number (frame-parameter (window-frame) 'outer-window-id)))

  (defun my/activate-window-by-id (wid)
    (cond ((executable-find "wmctrl")
           (call-process "wmctrl" nil nil nil
                         "-i"
                         "-a"
                         (format "0x%08X" wid)))
          ((executable-find "xdotool")
           (call-process "xdotool" nil nil nil
                         "windowactivate"
                         (number-to-string wid)))))

  (defun my/activate-emacs ()
    ;; TODO: support more windowing systems, although x-focus-frame is
    ;; likely enough elsewhere
    (cond ((eq window-system 'x)
           (let ((target-wid (my/get-emacs-window-id))
                 (active-wid (my/get-active-window-id)))
             (unless (eq target-wid active-wid)
               (my/activate-window-by-id target-wid))))))

  ;; Sometimes, the standard x-focus-frame is not enough
  (add-hook 'atomic-chrome-edit-mode-hook #'my/activate-emacs)

  (defvar my/atomic-chrome-browser-wid nil)
  (define-advice atomic-chrome-show-edit-buffer
      (:before (&rest _) stash-browser-wid )
    (setq my/atomic-chrome-browser-wid (my/get-active-window-id)))

  (add-hook
   'atomic-chrome-edit-done-hook
   (my/defun-as-value my/atomic-chrome-focus-browser ()
     (if my/atomic-chrome-browser-wid
         (my/activate-window-by-id my/atomic-chrome-browser-wid)
       ;; Fall back to heuristic using window names
       (when-let*
           ((srv (websocket-server-conn
                  (atomic-chrome-get-websocket (current-buffer))))
            (window-name
             (cond ((eq srv (bound-and-true-p
                             atomic-chrome-server-ghost-text))
                    "Firefox")
                   ((eq srv (bound-and-true-p
                             atomic-chrome-server-atomic-chrome))
                    "Google Chrome"))))
         (cond ((memq window-system '(mac ns))
                (call-process "open" nil nil nil "-a" window-name))
               ((and (eq window-system 'x) (executable-find "wmctrl"))
                (call-process "wmctrl" nil nil nil "-a" window-name))))))))

(provide 'config-desktop)
