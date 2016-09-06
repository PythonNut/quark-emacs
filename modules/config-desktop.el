;; -*- lexical-binding: t -*-
(require 'cl-lib)

(eval-when-compile
  (with-demoted-errors "Load error: %s"
    (require 'desktop)))

(defvar file-name-mode-alist nil)

(setq history-length 100
      history-delete-duplicates t)

;; text properties severely bloat the history so delete them
(defun my/unpropertize-session (&rest _args)
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

(defun nadvice/recentf-quiet (old-fun &rest args)
  (cl-letf (((symbol-function #'message)
             (lambda (&rest args)
               (when args
                 (apply #'format args)))))
    (apply old-fun args)))

(advice-add 'recentf-cleanup :around #'nadvice/recentf-quiet)

(with-eval-after-load 'recentf
  (eval-when-compile
    (with-demoted-errors "Load error: %s"
      (require 'recentf)))
  (add-to-list 'recentf-exclude (eval-when-compile
                                  (concat (rx line-start)
                                          (expand-file-name
                                           (locate-user-emacs-file "elpa")))))
  (setq recentf-save-file (locate-user-emacs-file ".recentf")
        recentf-max-saved-items 1000
        recentf-max-menu-items 50
        recentf-auto-cleanup 30))

(with-eval-after-load 'session
  (eval-when-compile
    (with-demoted-errors "Load error: %s"
      (require 'session)))

  (defun nadvice/session-save-session/quiet (old-fun &rest args)
    (if (called-interactively-p 'any)
        (apply old-fun args)
      (cl-letf* ((old-wr (symbol-function #'write-region))
                 ((symbol-function #'y-or-n-p) (lambda (&rest _args) t))
                 ((symbol-function #'write-region)
                  (lambda (start end filename
                                 &optional append _visit &rest args)
                    (apply old-wr
                           start
                           end
                           filename
                           append
                           0
                           args))))
        (apply old-fun args))))

  (defun nadvice/session-save-session/file-name-mode-alist (&rest _args)
    (setq file-name-mode-alist
          (nreverse
           (let ((res)
                 (orig (my/compress-alist file-name-mode-alist)))
             (dotimes (_ (min history-length (length orig)) res)
               (push (pop orig) res))))))

  (setq session-globals-max-string 16384
        session-registers-max-string 16384
        session-globals-max-size 1024
        session-jump-undo-remember 7
        session-jump-undo-threshold 60
        session-name-disable-regexp (eval-when-compile
                                      (rx (or (and line-start "/tmp")
                                              (and "COMMIT_EDITMSG" line-end))))

        session-globals-include '((kill-ring 400)
                                  (session-file-alist 200 t)
                                  (file-name-history 400)
                                  (file-name-mode-alist 400 t)
                                  search-ring
                                  regexp-search-ring)

        session-initialize '(session
                             places
                             keys))

  (run-with-idle-timer 10 t #'session-save-session)

  (advice-add 'session-save-session :around
              #'nadvice/session-save-session/quiet)
  (advice-add 'session-save-session :before
              #'nadvice/session-save-session/file-name-mode-alist)
  (advice-add 'session-save-session :before #'my/unpropertize-session)
  (advice-add 'session-initialize :around #'nadvice/recentf-quiet))

(add-hook 'after-init-hook #'session-initialize)

(setq auto-mode-alist (append auto-mode-alist file-name-mode-alist))

(defun my/compress-alist (alist)
  "Remove shadowed keys from `alist'"
  (let ((result))
    (dolist (elem alist)
      (unless (assoc (car elem) result)
        (push elem result)))
    (nreverse result)))

(defun my/register-file-name-mode-maybe ()
  (when (and buffer-file-name
             (not
              (file-name-extension
               buffer-file-name))
             (not (eq major-mode 'fundamental-mode)))
    (push (cons buffer-file-name major-mode) file-name-mode-alist)
    (push (cons buffer-file-name major-mode) auto-mode-alist)))

(add-hook 'after-change-major-mode-hook #'my/register-file-name-mode-maybe)

(with-eval-after-load 'desktop
  (setq desktop-dirname (locate-user-emacs-file "desktop/")
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
  (cl-letf (((symbol-function #'message)
             (lambda (&rest args)
               (when args
                 (apply #'format args))))
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

(eval-when-compile (require 'server))
(defun send-file-to-server (&optional arg)
  (interactive)
  (server-eval-at (concat "server"
                          (or arg
                              (read-from-minibuffer "Server ID: ")))
                  `(progn (find-file ,(buffer-file-name))
                          nil)))

(with-eval-after-load 'server
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

(provide 'config-desktop)
