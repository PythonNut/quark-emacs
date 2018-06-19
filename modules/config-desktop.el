;; -*- lexical-binding: t -*-
(require 'cl-lib)

(setq history-length 100
      history-delete-duplicates t)

(defvar file-name-mode-alist nil)

(defun my/register-file-name-mode-maybe ()
  (when (and buffer-file-name
             (not
              (file-name-extension
               buffer-file-name))
             (not (eq major-mode 'fundamental-mode)))
    (push (cons buffer-file-name major-mode) file-name-mode-alist)
    (push (cons buffer-file-name major-mode) auto-mode-alist)))

(add-hook 'after-change-major-mode-hook #'my/register-file-name-mode-maybe)

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
for more details.

If you want your minibuffer history shared between Emacs and XEmacs,
customize this value and make sure that `savehist-coding-system' is
set to a coding system that exists in both emacsen."
    :type 'file
    :group 'savehist)

  (el-patch-defcustom savehist-autosave-interval
    (* (el-patch-swap 5 2) 60)
    "The interval between autosaves of minibuffer history.
If set to nil, disables timer-based autosaving."
    :type '(choice (const :tag "Disabled" nil)
                   (integer :tag "Seconds"))
    :group 'savehist)

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
    :type '(repeat variable)
    :group 'savehist)


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
    (add-hook 'minibuffer-setup-hook 'savehist-minibuffer-hook)
    (add-hook 'kill-emacs-hook 'savehist-autosave)
    ;; Install an invocation of savehist-autosave on a timer.  This
    ;; should not cause noticeable delays for users -- savehist-autosave
    ;; executes in under 5 ms on my system.
    (when (and savehist-autosave-interval
	       (null savehist-timer))
      (setq savehist-timer
	    (if (featurep 'xemacs)
	        (start-itimer
	         "savehist" 'savehist-autosave savehist-autosave-interval
	         savehist-autosave-interval)
	      (run-with-timer savehist-autosave-interval
			      savehist-autosave-interval 'savehist-autosave)))))

  (el-patch-define-minor-mode savehist-mode
    "Toggle saving of minibuffer history (Savehist mode).
With a prefix argument ARG, enable Savehist mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.

When Savehist mode is enabled, minibuffer history is saved
periodically and when exiting Emacs.  When Savehist mode is
enabled for the first time in an Emacs session, it loads the
previous minibuffer history from `savehist-file'.

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
  (defun my/session-prepare/file-mode-alist ()
    (setq file-name-mode-alist
          (nreverse
           (let ((res)
                 (orig (my/compress-alist file-name-mode-alist)))
             (dotimes (_ (min history-length (length orig)) res)
               (push (pop orig) res))))))

  (defun my/session-prepare/unpropertize ()
    (mapc (lambda (lst)
            (with-demoted-errors "Error: %s"
              (when (boundp lst)
                (set lst (mapcar #'substring-no-properties (eval lst))))))
          '(kill-ring
            minibuffer-history
            helm-grep-history
            helm-ff-history
            file-name-history
            read-expression-history
            extended-command-history
            evil-ex-history)))

  (add-hook 'savehist-save-hook #'my/session-prepare/file-mode-alist)
  (add-hook 'savehist-save-hook #'my/session-prepare/unpropertize))

(use-package saveplace
  :ensure nil
  :init
  (autoload 'save-place-find-file-hook "saveplace")
  (autoload 'save-place-dired-hook "saveplace")
  (autoload 'save-place-kill-emacs-hook "saveplace")
  (autoload 'save-place-to-alist "saveplace")

  (el-patch-feature saveplace)

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
    '(
      (find-file-hook       recentf-track-opened-file)
      (write-file-functions recentf-track-opened-file)
      (kill-buffer-hook     recentf-track-closed-file)
      (kill-emacs-hook      recentf-save-list)
      )
    "Hooks used by recentf.")

  (defun my/recentf-onetime-setup ()
    (dolist (hook recentf-used-hooks) (apply #'add-hook hook)))

  (add-hook 'emacs-startup-hook #'my/recentf-onetime-setup)

  :config
  (setq recentf-save-file (locate-user-emacs-file "data/recentf")
        recentf-max-saved-items 1000
        recentf-max-menu-items 50
        recentf-auto-cleanup 30)
  (add-to-list 'recentf-exclude (eval-when-compile
                                  (concat (rx line-start)
                                          (expand-file-name
                                           (locate-user-emacs-file "elpa")))))

  ;; TODO: Yeah so this is actually a horrifying hack.
  (dolist (hook recentf-used-hooks) (apply #'remove-hook hook))
  (recentf-mode +1)

  (let ((recentf-autosave-timer nil))
    (defun nadvice/recentf-autosave (&rest _args)
      (when (timerp recentf-autosave-timer)
        (cancel-timer recentf-autosave-timer))
      (setq recentf-autosave-timer
            (run-with-idle-timer
             3 nil
             (lambda ()
               ;; TODO: Yeah so this is actually another horrifying hack.
               (let ((inhibit-message t)
                     (write-file-functions
                      (remove 'recentf-track-opened-file
                              write-file-functions)))
                 (recentf-save-list)))))))

  (advice-add 'recentf-track-opened-file :after
              #'nadvice/recentf-autosave)

  (defun nadvice/recentf-quiet (old-fun &rest args)
    (let ((inhibit-message t))
      (apply old-fun args)))

  (advice-add 'recentf-cleanup :around #'nadvice/recentf-quiet))

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

  (unless (daemonp)
    (defvar desktop-auto-save-timer
      (run-with-idle-timer 3 nil #'desktop-autosave))

    (add-hook 'focus-out-hook
              (lambda ()
                (ignore-errors (cancel-timer desktop-auto-save-timer))
                (setq desktop-auto-save-timer
                      (run-with-idle-timer 0.2 nil #'desktop-autosave))))

    (add-hook 'focus-in-hook
              (lambda ()
                (ignore-errors (cancel-timer desktop-auto-save-timer))
                (setq desktop-auto-save-timer
                      (run-with-idle-timer 3 t #'desktop-autosave)))))

  :config
  (setq desktop-dirname (locate-user-emacs-file "data/desktop/")
        desktop-path (list desktop-dirname)
        desktop-base-file-name "emacs-desktop"
        desktop-base-lock-name "emacs-desktop.lock")

  ;; don't let a dead emacs own the lockfile
  (defun nadvice/desktop-owner (pid)
    (when pid
      (let* ((attributes (process-attributes pid))
             (cmd (cdr (assoc 'comm attributes))))
        (if (and cmd (string-prefix-p "emacs" cmd))
            pid
          nil))))

  (defun nadvice/desktop-claim-lock (&optional dirname)
    (write-region (number-to-string (emacs-pid)) nil
                  (desktop-full-lock-name dirname) nil 1))

  (advice-add 'desktop-owner :filter-return #'nadvice/desktop-owner)
  (advice-add 'desktop-claim-lock :override #'nadvice/desktop-claim-lock))

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

  :config
  (defun nadvice/server-mode (old-fun &rest args)
    (catch 'done
      (let ((count 1))
        (while t
          (if (server-running-p server-name)
              (progn
                (setq server-name (concat "server" (number-to-string count)))
                (cl-incf count))
            (apply old-fun args)
            (throw 'done server-name))))))

  (advice-add 'server-start :around #'nadvice/server-mode))

(use-package atomic-chrome
  :commands (atomic-chrome-start-server
             atomic-chrome-stop-server)
  :init
  (when (display-graphic-p)
    (idle-job-add-function #'atomic-chrome-start-server))

  :config
  (defun my/atomic-chrome-focus-browser ()
    (let ((srv (websocket-server-conn
                (atomic-chrome-get-websocket (current-buffer))))
          (srv-ghost (bound-and-true-p atomic-chrome-server-ghost-text))
          (srv-atomic (bound-and-true-p atomic-chrome-server-atomic-chrome)))
      (cond ((memq window-system '(mac ns))
             (cond ((eq srv srv-ghost)
                    (call-process "open" nil nil nil "-a" "Firefox"))
                   ((eq srv srv-atomic)
                    (call-process "open" nil nil nil "-a" "Google Chrome"))))
            ((and (eq window-system 'x) (executable-find "wmctrl"))
             (cond ((eq srv srv-ghost)
                    (call-process "wmctrl" nil nil nil "-a" "Firefox"))
                   ((eq srv srv-atomic)
                    (call-process "wmctrl" nil nil nil "-a" "Google Chrome")))))))

  (add-hook 'atomic-chrome-edit-done-hook #'my/atomic-chrome-focus-browser))

(provide 'config-desktop)
