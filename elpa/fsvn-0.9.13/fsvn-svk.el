;;; fsvn-svk.el --- svk utility for fsvn.el


;;; History:
;; 

;;; Commentary:
;; 

;;; Code:



(require 'fsvn-proc)
(require 'fsvn-popup)
(require 'fsvn-browse)



(defvar process-environment)
(defvar system-type)
(defvar server-process)



;; svk repository concept has 3 repositories.
;; 1. mirrored repository. (ex svn://hostname/path/want/to/mirror)
;; 2. mirroring repository. (ex file:///homedir/.svk/local/mirror/{hashstring})
;; 3. working repository. (ex file:///homedir/.svk/local/working/{hashstring})
;;   3-1. working copy

;;TODO
;; M-x fsvn-svk-browse-pull 2 -> 3
;; M-x fsvn-svk-browse-push 3 -> 2 -> 1
;; M-x fsvn-svk-browse-resync 1 <-> 2

;; http://svk.bestpractical.com/view/HomePage

(defgroup fsvn-svk nil
  "`svk' Utilities."
  :group 'fsvn)

(defcustom fsvn-svk-perl-command nil
  "Perl command that executing `fsvn-svk-script'.
If there is executing problem in windows/cygwin then set path to perl.exe."
  :group 'fsvn-svk)

(defcustom fsvn-svk-script "svk"
  "Perl script file of `svk'"
  :group 'fsvn-svk)

(defcustom fsvn-svk-perllib (getenv "PERLLIB")
  "Environment variable `PERLLIB' value when executing `svk' command"
  :group 'fsvn-svk)

(defcustom fsvn-svk-mirror-depot "mirror"
  "Name of path to `svk' mirroring repository. (Generally \"mirror\")"
  :group 'fsvn-svk)

(defcustom fsvn-svk-working-depot "working"
  "Name of path to `svk' working repository. (Generally \"\" but I recommend \"working\")"
  :group 'fsvn-svk)

(defcustom fsvn-svk-editor-command
  (or
   (getenv "EDITOR")
   (and (featurep 'meadow)
        (executable-find "gnuclient.exe"))
   "emacsclient")
  "Client program for edit conflicted file."
  :group 'fsvn-svk)

(defvar fsvn-svk-browse-map nil)

(unless fsvn-svk-browse-map
  (setq fsvn-svk-browse-map
        (let ((map (make-sparse-keymap)))
          (suppress-keymap map)
          (define-key map "\eI" 'fsvn-svk-browse-create)
          (define-key map "S" 'fsvn-svk-browse-push)
          (define-key map "L" 'fsvn-svk-browse-pull)
          (define-key map "R" 'fsvn-svk-browse-resync)
          map)))

(add-hook 'fsvn-browse-mode-hook
          (lambda () (define-key fsvn-browse-mode-map "\C-c\C-k" fsvn-svk-browse-map)))

;; fsvn-svk internal function

(defun fsvn-svk-home-directory ()
  (fsvn-expand-file ".svk" (getenv "HOME")))

(defun fsvn-svk-depotmap-directory ()
  (let ((conf-file (fsvn-expand-file "config" (fsvn-svk-home-directory))))
    (when (file-exists-p conf-file)
      (with-temp-buffer
        (insert-file-contents conf-file)
        (goto-char (point-min))
        (when (re-search-forward "^depotmap:" nil t)
          (when (re-search-forward "^[ \t]*\\(?:\"\"\\|''\\):[ \t]*\\(.+\\)" nil t)
            (fsvn-expand-file (match-string 1))))))))

(defun fsvn-svk-depotmap-url ()
  (let ((dir (fsvn-svk-depotmap-directory)))
    (when dir
      (fsvn-directory-name-as-repository dir))))

(defun fsvn-svk-depotpath-url (depotpath)
  (when (string-match "^//\\(.*\\)" depotpath)
    (fsvn-expand-url (match-string 1 depotpath) (fsvn-svk-depotmap-url))))

(defun fsvn-svk-mirrored-repository-url (file)
  (let ((prop (fsvn-get-file-parent-property file "svm:source")))
    (when prop
      (fsvn-svk-svm:source-to-mirrored-url prop))))

(defun fsvn-svk-svm:source-to-mirrored-url (prop)
  (let ((list (split-string prop "!")))
    (concat (car list) (mapconcat 'identity (cdr list) "!"))))

(defun fsvn-svk-mirroring-depotpath (file)
  (let* ((root (fsvn-svk-depotmap-url))
         (uuid (fsvn-get-file-parent-property file "svm:uuid"))
         (top-url (fsvn-expand-url fsvn-svk-mirror-depot root))
         url depot)
    (catch 'found
      (mapc
       (lambda (entry)
         (setq url (fsvn-expand-url (fsvn-xml-lists->list->entry=>name$ entry) top-url))
         (when (string= (fsvn-get-propget url "svm:uuid") uuid)
           (setq depot (concat (fsvn-svk-mirror-top-depotpath) (fsvn-xml-lists->list->entry=>name$ entry)))
           (throw 'found depot)))
       (fsvn-get-ls top-url))
      nil)))

(defun fsvn-svk-file-depotpath (file)
  (let (prop)
    (setq prop (fsvn-get-file-parent-property file "svm:uuid" t))
    (when prop
      (let (uuid1 uuid2 repos url name topdir info)
        (setq topdir (car prop))
        (setq uuid1 (cdr prop))
        (setq repos (fsvn-svk-depotmap-url))
        (setq info (fsvn-get-info-entry topdir))
        (concat "/" (fsvn-info-repos-path info))))))

(defun fsvn-svk-mirror-top-depotpath ()
  (concat "//" fsvn-svk-mirror-depot "/"))

(defun fsvn-svk-working-top-depotpath ()
  (concat "//" fsvn-svk-working-depot "/"))

(defun fsvn-svk-mirror-depotpath (url)
  (concat (fsvn-svk-mirror-top-depotpath) (fsvn-svk-depotpath-url-hash url) "/"))

(defun fsvn-svk-working-depotpath (url)
  (concat (fsvn-svk-working-top-depotpath) (fsvn-svk-depotpath-url-hash url) "/"))

(defun fsvn-svk-depotpath-url-hash (url)
  (md5 (fsvn-url-as-directory url)))

(defun fsvn-svk-browse-depotpath ()
  (fsvn-svk-file-depotpath (fsvn-browse-current-directory)))

(defun fsvn-svk-browse-mirroring-depotpath ()
  (fsvn-svk-mirroring-depotpath (fsvn-browse-current-directory)))

(defun fsvn-svk-browse-wc-p ()
  (not (not (fsvn-svk-browse-depotpath))))

(defun fsvn-svk-browse-check-exec ()
  (unless (or (and fsvn-svk-perl-command
                   (executable-find fsvn-svk-perl-command)
                   (or (and (file-name-absolute-p fsvn-svk-script)
                            (file-exists-p fsvn-svk-script))
                       (executable-find fsvn-svk-script)))
              (executable-find fsvn-svk-script))
    (error "cannot execute %s" fsvn-svk-script))
  (fsvn-svk-server-start))

(defun fsvn-svk-browse-draw-mirrored-url ()
  (when (fsvn-url-local-p default-directory)
    (save-excursion
      (let ((mirroered-url (fsvn-svk-mirrored-repository-url default-directory))
            buffer-read-only)
        (when mirroered-url
          (goto-char (point-min))
          (when (re-search-forward fsvn-browse-re-root nil t)
            (replace-match mirroered-url t nil nil 2)))))))

(defmacro fsvn-svk-process-environment (&rest form)
  `(fsvn-process-environment 
    (let ((process-environment (copy-sequence process-environment)))
      (setenv "PERLLIB" fsvn-svk-perllib)
      (setenv "EDITOR" fsvn-svk-editor-command)
      ,@form)))

(defun fsvn-svk-call-command (subcommand buffer &rest args)
  (fsvn-svk-process-environment
   (let ((real-args (fsvn-command-args-canonicalize args))
         command internal-args script)
     (if (null fsvn-svk-perl-command)
         (setq command fsvn-svk-script
               internal-args (cons subcommand real-args))
       (setq command fsvn-svk-perl-command
             internal-args (append (list fsvn-svk-perl-command subcommand)
                                   real-args)))
     (fsvn-debug internal-args)
     (apply 'call-process command nil buffer nil internal-args))))

(defun fsvn-svk-start-command (subcommand buffer &rest args)
  (fsvn-svk-process-environment
   (let ((real-args (fsvn-command-args-canonicalize args))
         internal-args proc script)
     (setq internal-args 
           (if (null fsvn-svk-perl-command)
               (list fsvn-svk-script)
             (setq script (executable-find fsvn-svk-script))
             (list fsvn-svk-perl-command script)))
     (setq internal-args (append internal-args (cons subcommand real-args)))
     (fsvn-debug internal-args)
     (setq proc (apply 'start-process "fsvn svk" buffer internal-args))
     (set-process-sentinel proc 'fsvn-svk-process-sentinel)
     (set-process-filter proc 'fsvn-popup-process-filter-in-buffer)
     (with-current-buffer buffer
       (when (eq major-mode 'fsvn-popup-result-mode)
         (setq fsvn-popup-result-process proc)))
     proc)))

(defun fsvn-svk-process-sentinel (proc event)
  (fsvn-process-exit-handler proc event
    (when (/= (process-exit-status proc) 0)
      (insert event))
    (setq fsvn-popup-result-process nil)
    (when (and (eq system-type 'windows-nt)
               (/= (process-exit-status proc) 0))
      (fsvn-svk-win-start-external-window (process-command proc)))))

;;FIXME windows native perl.exe cannot accept terminal input
(defun fsvn-svk-win-start-external-window (args)
  (let ((process-environment (copy-sequence process-environment)))
    (setenv "EDITOR" fsvn-svk-editor-command)
    (fsvn-win-start-external-terminal args)))

(defun fsvn-svk-server-start ()
  (unless (and (boundp 'server-process) (eq (process-status server-process) 'listen))
    (server-start)))

(defvar fsvn-svk-read-depotpath-history nil)
(defun fsvn-svk-read-depotpath (prompt &optional initial-contents)
  (read-from-minibuffer prompt initial-contents nil nil 'fsvn-svk-read-depotpath-history))

(defun fsvn-svk-browse-cmd-read-create ()
  (when (eq system-type 'windows-nt)
    (error "Not supported this function on windows."))
  (let (url)
    (setq url (fsvn-browse-current-repository-url))
    (setq url (fsvn-completing-read-url "Mirrored URL: " url t))
    (list url)))

(defconst fsvn-svk-confirm-alist
  '(
    (fsvn-svk-browse-push "Push to mirrored repository? ")
    (fsvn-svk-browse-pull "Pull from mirrored repository? ")
    (fsvn-svk-browse-resync "Synchronize mirrored repository and mirroring path? ")
    ))

(defun fsvn-svk-confirm (op-symbol)
  (let ((message (cdr (assq op-symbol fsvn-svk-confirm-alist))))
    (unless (y-or-n-p (nth 0 message))
      (fsvn-quit))))

(defun fsvn-svk-depotmap-init (buffer)
  (let ((proc (fsvn-svk-start-command "depotmap" buffer "--init")))
    (fsvn-process-add-filter proc 
                             (lambda (proc event)
                               (when (string-match "^Repository .* does not exist, create\\? (y/n)" event)
                                 (process-send-string proc "y\n"))))
    proc))

(defun fsvn-svk-mirror-start (buffer mirrorpath url)
  (unless (fsvn-svk-mirror-path-exists-p mirrorpath)
    (let ((proc (fsvn-svk-start-command "mirror" buffer mirrorpath url)))
      proc)))

(defun fsvn-svk-mirror-path-exists-p (mirrorpath)
  (with-temp-buffer
    (unless (= (fsvn-svk-call-command "mirror" (current-buffer) "--list") 0)
      (error "Failed exit while listing mirrors"))
    (goto-char (point-min))
    (re-search-forward (concat "^" (regexp-quote (fsvn-svk-depotpath-name mirrorpath))) nil t)))

(defun fsvn-svk-sync-start (buffer mirrorpath)
  (let ((proc (fsvn-svk-start-command "sync" buffer mirrorpath)))
    proc))

(defun fsvn-svk-depotpath-name (depotpath)
  (if (string-match "/$" depotpath)
      (substring depotpath 0 -1)
    depotpath))

;; fsvn-svk interactive command

(defun fsvn-svk-browse-create (mirrored-url)
  "Initialize and create `svk' repository to HOME directory."
  (interactive (fsvn-svk-browse-cmd-read-create))
  (fsvn-svk-browse-check-exec)
  (fsvn-async-let ((buffer (fsvn-popup-result-create-buffer))
                   (url mirrored-url)
                   (mirrorpath (fsvn-svk-mirror-depotpath mirrored-url))
                   (depotpath (fsvn-svk-working-depotpath mirrored-url)))
    (fsvn-buffer-popup-as-information buffer)
    ;; create ~/.svk and local repository
    (fsvn-svk-depotmap-init buffer)
    ;; create mirrorpath as mirroring repository and adding svm:* property
    (fsvn-svk-mirror-start buffer mirrorpath url)
    ;; get all revision log and data to mirroring repository
    (fsvn-svk-sync-start buffer mirrorpath)
    ;; copy mirroring repository to working repository
    (fsvn-svk-start-command "copy" buffer "-p" mirrorpath depotpath)
    (with-current-buffer buffer
      (goto-char (point-max))
      (insert "\n")
      (insert "####################################################\n")
      (insert "Done mirrorring.\n")
      (insert "Checkout " (fsvn-svk-depotpath-url depotpath) "\n")
      (insert "####################################################\n"))))

(defun fsvn-svk-browse-push ()
  "Push working copy repository -> mirroring repository -> mirrored repository."
  (interactive)
  (when (fsvn-interactive-p)
    (fsvn-svk-confirm 'fsvn-svk-browse-push))
  (fsvn-svk-browse-check-exec)
  (let ((buffer (fsvn-popup-result-create-buffer))
        (depotpath (fsvn-svk-browse-depotpath)))
    (unless depotpath
      (error "This directory has no depotpath"))
    (prog1
        (fsvn-svk-start-command "push" buffer depotpath)
      (fsvn-buffer-popup-as-information buffer))))

(defun fsvn-svk-browse-pull ()
  "Pull mirroring repository -> working copy repository."
  (interactive)
  (when (fsvn-interactive-p)
    (fsvn-svk-confirm 'fsvn-svk-browse-pull))
  (fsvn-svk-browse-check-exec)
  (let ((buffer (fsvn-popup-result-create-buffer))
        (depotpath (fsvn-svk-browse-depotpath)))
    (unless depotpath
      (error "This directory has no depotpath"))
    (prog1
        (fsvn-svk-start-command "pull" buffer depotpath)
      (fsvn-buffer-popup-as-information buffer))))

(defun fsvn-svk-browse-resync ()
  "Synchronize mirroring repository <-> mirrored repository."
  (interactive)
  (when (fsvn-interactive-p)
    (fsvn-svk-confirm 'fsvn-svk-browse-resync))
  (fsvn-svk-browse-check-exec)
  (let ((buffer (fsvn-popup-result-create-buffer))
        (depotpath (fsvn-svk-browse-mirroring-depotpath)))
    (unless depotpath
      (error "This directory has no mirroring depotpath"))
    (prog1
        (fsvn-svk-start-command "sync" buffer depotpath)
      (fsvn-buffer-popup-as-information buffer))))



;; modify browse-mode buffer

(add-hook 'fsvn-browse-mode-hook 'fsvn-svk-browse-draw-mirrored-url)



(provide 'fsvn-svk)

;;; fsvn-svk.el ends here
