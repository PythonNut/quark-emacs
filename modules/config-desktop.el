;; -*- lexical-binding: t -*-

(eval-when-compile
  (with-demoted-errors "Load error: %s"
    (require 'desktop)
    (require 'cl-lib)))

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

(with-eval-after-load 'session
  (eval-when-compile
    (with-demoted-errors "Load error: %s"
      (require 'session)))

  (defun nadvice/session-save-session/quiet (old-fun &rest args)
    (if (called-interactively-p 'any)
        (apply old-fun args)
      (cl-letf* ((old-wr (symbol-function #'write-region))
                 ((symbol-function #'write-region)
                  (lambda (start end filename
                                 &optional append visit &rest args)
                    (apply old-wr
                           start
                           end
                           filename
                           append
                           0
                           args))))
        (apply old-fun args))))

  (setq session-globals-max-string 16384
        session-registers-max-string 16384
        session-globals-max-size 1024
        session-jump-undo-remember 7
        session-jump-undo-threshold 60
        session-initialize '(session
                             places
                             keys))

  (advice-add 'session-save-session :around
              #'nadvice/session-save-session/quiet)
  (run-with-idle-timer 10 t #'session-save-session)
  (advice-add 'session-save-session :before #'my/unpropertize-session)
  (add-hook 'kill-emacs-hook #'my/unpropertize-session)
  (add-to-list 'session-globals-include 'file-name-mode-alist))

(add-hook 'after-init-hook #'session-initialize)

(defun nadvice/recentf-quiet (old-fun &rest args)
  (cl-letf (((symbol-function #'message) #'format))
    (apply old-fun args)))

(advice-add 'recentf-cleanup :around #'nadvice/recentf-quiet)
(advice-add 'session-initialize :around #'nadvice/recentf-quiet)

(with-eval-after-load 'recentf
  (eval-when-compile
    (with-demoted-errors "Load error: %s"
      (require 'recentf)))

  (setq recentf-save-file (expand-file-name ".recentf" user-emacs-directory)
        recentf-max-saved-items 200
        recentf-max-menu-items 30
        recentf-auto-cleanup 3))

(setq auto-mode-alist (append auto-mode-alist file-name-mode-alist))

(defun my/compress-alist (alist)
  "Remove shadowed keys from `alist'"
  (let ((result))
    (dolist (elem alist)
      (unless (assoc (car elem) result)
        (push elem result)))
    (nreverse result)))

(add-hook 'savehist-save-hook
          (lambda ()
            (setq file-name-mode-alist
                  (nreverse
                   (let ((res)
                         (orig (my/compress-alist file-name-mode-alist)))
                     (dotimes (_ (min history-length (length orig)) res)
                       (push (pop orig) res)))))))

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
  (setq desktop-dirname (expand-file-name "desktop/" user-emacs-directory)
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
  (cl-letf (((symbol-function #'message) #'format)
            ((symbol-function #'y-or-n-p) (lambda (_prompt) t)))
    (desktop-save-in-desktop-dir)))

(defun desktop-load (&optional arg)
  (interactive "p")
  (if (= arg 4)
      (let* ((files (cl-remove-if
                     (lambda (item)
                       (string-match-p "^\\(\\..*\\|.*\.lock\\)$" item))
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

(defun run-server ()
  (require 'server)
  (message "Server id is %s"
           (catch 'done
             (let ((count 1))
               (while t
                 (setq server-name (concat "server" (number-to-string count)))
                 (if (server-running-p server-name)
                     (cl-incf count)
                   (server-mode)
                   (throw 'done server-name)))))))

(when (member "-P" command-line-args)
  (setq debug-on-error t)
  (delete "-P" command-line-args)
  (require 'server)
  (add-hook 'emacs-startup-hook #'send-all-files-to-server))

(provide 'config-desktop)
