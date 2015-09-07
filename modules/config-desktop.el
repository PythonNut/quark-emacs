;; -*- lexical-binding: t -*-
;; Automatically save and restore sessions

(eval-when-compile
  (with-demoted-errors "Load error: %s"
    (require 'dash)
    (require 'desktop)
    (require 'cl-lib)
    (require 'recentf)
    (require 'saveplace)
    (require 'savehist)
    (require 'helm-grep)
    (require 'evil-ex)
    (require 'config-modes)))

(defvar file-name-mode-alist nil)

(setq history-length 100
      history-delete-duplicates t

      save-place-file (expand-file-name ".saveplace" user-emacs-directory)

      savehist-file (expand-file-name ".savehist" user-emacs-directory)
      savehist-autosave-interval 180
      savehist-save-minibuffer-history t
      savehist-additional-variables '(kill-ring
                                      file-name-mode-alist
                                      search-ring
                                      regexp-search-ring)

      ;; remember more recent files
      recentf-save-file (expand-file-name ".recentf" user-emacs-directory)
      recentf-max-saved-items 200
      recentf-max-menu-items 30)

(with-eval-after-load 'saveplace
  (if (fboundp 'save-place-mode)
      (save-place-mode +1)
    (setq-default save-place t)))

(add-hook 'find-file-hook (lambda () (require 'saveplace)))

(require 'savehist)
(savehist-mode +1)

;; text properties severely bloat the history so delete them
(defun my/unpropertize-savehist ()
  (mapc (lambda (list)
          (with-demoted-errors "Error: %s"
            (when (boundp list)
              (set list (mapcar #'substring-no-properties (eval list))))))
        '(kill-ring
          minibuffer-history
          helm-grep-history
          helm-ff-history
          file-name-history
          read-expression-history
          extended-command-history
          evil-ex-history)))

(add-hook 'kill-emacs-hook #'my/unpropertize-savehist)
(add-hook 'savehist-save-hook #'my/unpropertize-savehist)

(defun nadvice/recentf-quiet (old-fun &rest args)
  (cl-letf (((symbol-function 'message) #'format))
    (apply old-fun args)))

(advice-add 'recentf-cleanup :around #'nadvice/recentf-quiet)

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
            (require 'dash)
            (setq file-name-mode-alist
                  (-take history-length
                         (my/compress-alist file-name-mode-alist)))))

(defun my/register-file-name-mode-maybe ()
  (when (and buffer-file-name
             (not
              (file-name-extension
               buffer-file-name)))
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
  (cl-letf (((symbol-function 'message) #'format)
            ((symbol-function 'y-or-n-p) (lambda (prompt) t)))
    (desktop-save-in-desktop-dir)))

(defun desktop-load (&optional arg)
  (interactive "p")
  (if (= arg 4)
      (let* ((files (cl-remove-if
                     (lambda (item)
                       (string-match-p "^\\(\\..*\\|.*\.lock\\)$" item))
                     (directory-files desktop-dirname)))
             (desktop-base-file-name (completing-read
                                      "Complete a foo: "
                                      files
                                      nil t))
             (desktop-base-lock-name
              (concat desktop-base-file-name ".lock")))
        (desktop-read)
        (desktop-remove))
    (desktop-read)))

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
                  (run-with-idle-timer 3 t #'desktop-autosave))))

(provide 'config-desktop)
