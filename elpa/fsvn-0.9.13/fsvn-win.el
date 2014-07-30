;;; fsvn-win.el --- Part of fsvn for Windows


;;; History:
;; 

;;; Commentary:
;; 

;; NOTE
;;   when using svn+ssh protocol plinkw as tunnel, then window blinked.
;;   but, TortoisePlink not blinked.

;; extra (TortoiseSVN on Windows)

;;; Code:



(require 'fsvn-deps)



(defvar diff-switches)
(defvar current-prefix-arg)

(defvar fsvn-targets-file-converter)



(defun fsvn-win-x64-p ()
  ;;TODO more efficient way..
  (fsvn-file-exact-directory-p 
   (expand-file-name "syswow64" (getenv "windir"))))

(defun fsvn-win-wow64-p ()
  (let ((windir (getenv "windir")))
    (equal (nth 10 (file-attributes (expand-file-name "system32" windir))) 
           (nth 10 (file-attributes (expand-file-name "syswow64" windir))))))



(unless (featurep 'meadow)
  (require 'mw32cmp))

(defgroup tortoise nil
  "TortoiseSVN"
  :group 'tortoise)

(defcustom tortoise-interface-program
  "c:/Program Files/TortoiseSVN/bin/TortoiseProc.exe"
  "TortoiseSVN main program."
  :group 'tortoise
  :type 'file)

(defun tortoise-call-program (command &optional path)
  (unless (and tortoise-interface-program
               (executable-find tortoise-interface-program))
    (error "Interface program not found"))
  (let ((args nil))
    (setq args (cons (concat "/command:" command) nil))
    (setq args (cons (concat "/path:" (or path default-directory)) args))
    (setq args (nreverse args))
    ;; for meadow
    (let ((default-process-argument-editing-function (lambda (x) (mapconcat 'identity x " "))))
      (apply 'call-process tortoise-interface-program nil 0 nil args))))

(defcustom tortoise-merge-program
  "c:/Program Files/TortoiseSVN/bin/TortoiseMerge.exe"
  "TortoiseSVN visualized merge program."
  :group 'tortoise)

(defun tortoise-diff-program (fromfile tofile &optional switches)
  (call-process tortoise-merge-program nil 0 nil
                (expand-file-name fromfile) (expand-file-name tofile)))

(defun tortoise-show-log (&optional arg)
  (interactive "P")
  (let (file)
    (unless arg
      (setq file (fsvn-browse-point-url)))
    (tortoise-call-program "log" file)
    (message "Starting log viewer...")))

(defun tortoise-show-log-directory ()
  (interactive)
  (tortoise-call-program "log" (fsvn-dired-current-directory))
  (message "Starting log viewer..."))

(defun tortoise-browse-repository ()
  (interactive)
  (let* ((urlrev (fsvn-dired-current-directory))
         rev url)
    (when (fsvn-magic-file-name-absolute-p urlrev)
      (setq urlrev (fsvn-magic-parse-file-name urlrev)))
    (setq rev (fsvn-urlrev-revision urlrev))
    (setq url (fsvn-url-as-directory (fsvn-urlrev-url urlrev)))
    (tortoise-call-program "repobrowser" (fsvn-url-urlrev url rev))))

(defun tortoise-diff-local (file &optional switches)
  "diff by tortoise gui program."
  (interactive (tortoise-diff-read-args))
  (tortoise-diff-program file (fsvn-browse-point-url) switches))

(defun tortoise-diff-read-args ()
  (let ((current (fsvn-browse-point-url))
        (default (if (mark t)
                     (save-excursion (goto-char (mark t))
                                     (fsvn-browse-point-url)))))
    (when (or (equal default current)
              (and (not (equal (fsvn-dired-dwim-target-directory)
                               (fsvn-dired-current-directory)))
                   (not mark-active)))
      (setq default nil))
    (list (read-file-name (format "Diff %s with%s: "
                                  current
                                  (if default
                                      (concat " (default " default ")")
                                    ""))
                          (if default
                              (fsvn-dired-current-directory)
                            (fsvn-dired-dwim-target-directory))
                          default t)
          (when current-prefix-arg
            (require 'diff)
            (read-string "Options for TortoiseMerge: "
                         (if (stringp diff-switches)
                             diff-switches
                           (mapconcat 'identity diff-switches " ")))))))

(defvar tortoise-external-svn-keymap nil)
(setq tortoise-external-svn-keymap
      (let ((map (make-sparse-keymap)))

        (define-key map "l" 'tortoise-show-log)
        (define-key map "L" 'tortoise-show-log-directory)
        (define-key map "b" 'tortoise-browse-repository)
        (define-key map "=" 'tortoise-diff-local)
        map))

(add-hook 'dired-mode-hook
          (lambda ()
            (define-key dired-mode-map "\C-c\C-t" tortoise-external-svn-keymap)
            ))

(add-hook 'fsvn-browse-mode-hook
          (lambda ()
            (define-key fsvn-browse-mode-map "\C-c\C-t" tortoise-external-svn-keymap)
            ))


;; for cygwin

(defvar fsvn-cygwin-drive-prefix-dir nil)
(defun fsvn-cygwin-drive-prefix-dir ()
  (with-temp-buffer
    (when (= (call-process "cygpath" nil (current-buffer) nil "-u" "c:/") 0)
      (substring (buffer-string) 0 -3))))

(defvar fsvn-cygwin-installed-folder nil)
(defun fsvn-cygwin-installed-folder ()
  (or
   (and (fsvn-win-wow64-p)
        (condition-case nil
            (car (mw32-registry-get 
                  "HKEY_LOCAL_MACHINE\\SOFTWARE\\Wow6432Node\\Cygwin\\setup"
                  "rootdir"))
          (error nil)))
   (condition-case nil
       ;; probably cygwin 1.7 or later
       (car (mw32-registry-get 
             "HKEY_LOCAL_MACHINE\\SOFTWARE\\Cygwin\\setup"
             "rootdir"))
     (error nil))
   (condition-case nil
       ;; cygwin 1.6 or earlier
       (car (mw32-registry-get
             "HKEY_LOCAL_MACHINE\\SOFTWARE\\Cygnus Solutions\\Cygwin\\mounts v2/" 
             "native"))
     (error nil))))

(defvar fsvn-cygwin-installed-dir nil)
(defun fsvn-cygwin-installed-dir ()
  (let ((fold (fsvn-cygwin-installed-folder)))
    (and fold
         (file-name-as-directory (dos-to-unix-filename fold)))))

(defvar fsvn-cygwin-guessed-installed nil)
(defun fsvn-cygwin-guessed-installed ()
  (fsvn-cygwin-installed-folder))

(defvar fsvn-cygwin-svn-p nil)
(defun fsvn-cygwin-svn-p ()
  (let ((command (executable-find fsvn-svn-command-internal))
        (cygdir (fsvn-cygwin-installed-dir)))
    (and command
         cygdir
         (string-match (concat "^" (regexp-quote cygdir)) command))))

(defun fsvn-cygwin-expand-path (name &optional default)
  (let ((inst-dir fsvn-cygwin-installed-dir)
        (expanded (expand-file-name name default)))
    (cond
     ((not fsvn-cygwin-guessed-installed)
      expanded)
     ((string-match (concat "^" (regexp-quote inst-dir)) expanded)
      (concat "/" (substring expanded (match-end 0))))
     (t
      (let* ((file (expand-file-name name default))
             (drive (substring file 0 1))
             (name (substring file 2)))
        (concat (file-name-as-directory fsvn-cygwin-drive-prefix-dir) drive name))))))

(defun fsvn-cygwin-to-emacs-path (path)
  (let ((prefix fsvn-cygwin-drive-prefix-dir)
        (installed fsvn-cygwin-installed-folder))
    (cond
     ((string-match (format "^\\(%s\\)/\\([a-zA-Z]\\)/\\(.*\\)" (regexp-quote prefix)) path)
      (format "%s:/%s" (match-string 2 path) (match-string 3 path)))
     ((string-match "^/" path)
      (format "%s/%s" (expand-file-name installed) (substring path 1)))
     (t
      path))))

;; cygwin svn `--targets' arg accept only cygpath
(defun fsvn-win-targets-file-converter (x)
  (fsvn-cygwin-expand-path x))

(defun fsvn-win-initialize-loading ()
  (setq fsvn-cygwin-svn-p (fsvn-cygwin-svn-p))
  (cond
   (fsvn-cygwin-svn-p
    (setq fsvn-cygwin-installed-folder (fsvn-cygwin-installed-folder)
          fsvn-cygwin-guessed-installed (fsvn-cygwin-guessed-installed)
          fsvn-cygwin-installed-dir (fsvn-cygwin-installed-dir)
          fsvn-cygwin-drive-prefix-dir (fsvn-cygwin-drive-prefix-dir)
          fsvn-targets-file-converter 'fsvn-win-targets-file-converter
          fsvn-password-prompt-accessible-p t))
   (t
    (setq fsvn-targets-file-converter nil
          fsvn-password-prompt-accessible-p nil))))

(setq fsvn-initialize-function 'fsvn-win-initialize-loading)



(defun fsvn-win-enable-password-prompt ()
  fsvn-cygwin-svn-p)

(defun fsvn-win-authenticate-repository (repository)
  (fsvn-win-start-external-terminal fsvn-svn-command-internal "info" repository))

;; extra

(defun fsvn-win-start-gui-viewer (&optional arg)
  (interactive "P")
  (fsvn-browse-wc-only 
   (let ((dir (directory-file-name (fsvn-browse-current-directory-url))))
     (let ((file (fsvn-browse-point-url)))
       (fsvn-win-start-explorer dir arg (and file (file-name-nondirectory file)))))))

(defun fsvn-win-start-explorer (dir arg &optional file)
  (let (tree args)
    (setq tree (if arg "/e" "/n"))
    (cond
     ((or (null file) (string-match "^\\.\\.?$" file))
      (setq args (list tree (unix-to-dos-filename dir))))
     (t
      (setq args (list tree "/select" (unix-to-dos-filename (expand-file-name file dir))))))
    (let ((argument (mapconcat 'identity args ",")))
      (message "Starting explorer...")
      ;; for meadow
      (let ((default-process-argument-editing-function (lambda (x) (mapconcat 'identity x " "))))
        (call-process "explorer" nil 0 nil argument)))))

(add-hook 'fsvn-browse-mode-hook
          (lambda ()
            (define-key fsvn-browse-mode-map "\C-c\C-s" 'fsvn-win-start-gui-viewer)
            ))



;; TODO NTEmacs has no fiber.exe
(defun fsvn-win-start-external-terminal (&rest args)
  (let ((tmpfile (fsvn-make-temp-file))
        (command (mapconcat 'identity (cons "/C" (fsvn-command-args-canonicalize args)) " "))
        batfile)
    (with-temp-buffer
      (insert (unix-to-dos-filename (executable-find "cmd.exe")) " " command "\n")
      (insert "@echo off" "\n")
      (insert "sleep 5" "\n")           ; wait several seconds.
      (write-region (point-min) (point-max) tmpfile nil 'no-msg))
    (setq batfile (concat tmpfile ".bat"))
    (rename-file tmpfile batfile)
    (call-process "fiber" nil 0 nil batfile)))



;; modify global settings.



(provide 'fsvn-win)

;;; fsvn-win.el ends here
