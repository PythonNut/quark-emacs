;;; fsvn-proc.el --- process utility for fsvn


;;; History:
;; 

;;; Commentary:
;; 

;;; Code:



(require 'fsvn-deps)



(defvar process-environment)
(defvar default-buffer-file-coding-system)



(defvar fsvn-process-environment-lang nil)
(defmacro fsvn-process-environment (&rest form)
  `(let ((process-environment (copy-sequence process-environment)))
     (setenv "LC_MESSAGES" (or fsvn-process-environment-lang "C"))
     ,@form))

(defun fsvn-process-coding-system (flatten-args)
  (catch 'found
    (when (and flatten-args (stringp (car flatten-args)))
      (when (member (car flatten-args) '("diff" "blame"))
        ;; get guessed coding-system if arg file is local
        (throw 'found (fsvn-guess-file-contents-coding-system (cdr flatten-args)))))
    (when (member "--xml" flatten-args)
      (throw 'found fsvn-svn-common-coding-system))
    nil))

(defun fsvn-start-process (buffer &rest args)
  (fsvn-process-environment
   (let* ((real-args (fsvn-command-args-canonicalize args))
          (coding-system-for-read (fsvn-process-coding-system real-args)))
     (fsvn-debug real-args)
     (apply 'start-process "fsvn" buffer fsvn-svn-command-internal real-args))))

(defun fsvn-start-command (subcommand buffer &rest args)
  (apply 'fsvn-start-process buffer subcommand args))

(defun fsvn-start-command-display (subcommand buffer &rest args)
  (let ((commandline (concat (fsvn-build-command-string subcommand args) "\n\n"))
        proc)
    (fsvn-insert-string-to-buffer commandline buffer)
    (setq proc (fsvn-start-command subcommand buffer args))
    proc))

(defun fsvn-call-process (buffer &rest args)
  "Execute `call-process' with variable `fsvn-svn-command-internal'.
This is synchronous call, so cannot handle password prompt. Append --non-interactive arg
explicitly in calling function.
"
  (fsvn-process-environment
   (let* ((real-args (fsvn-command-args-canonicalize args))
          (coding-system-for-read (fsvn-process-coding-system real-args)))
     (when (and (bufferp buffer) (> (buffer-size buffer) 0))
       (with-current-buffer buffer
         (goto-char (point-max))))
     (fsvn-debug real-args)
     (prog1
         (apply 'call-process fsvn-svn-command-internal nil buffer nil real-args)
       (fsvn-debug buffer)))))

(defun fsvn-call-command (subcommand buffer &rest args)
  (apply 'fsvn-call-process buffer subcommand (fsvn-command-append-argument subcommand args)))

(defun fsvn-call-command-display (subcommand buffer &rest args)
  "`call-process' and insert executed command line top of buffer."
  (let ((commandline (concat (fsvn-build-command-string
                              subcommand
                              (fsvn-command-append-argument subcommand args)) "\n\n")))
    (fsvn-insert-string-to-buffer commandline buffer)
    (apply 'fsvn-call-command subcommand buffer args)))

(defun fsvn-call-command-discard (subcommand &rest args)
  "`call-process' and discard executed command output.
If error occur in process (exit status with non zero value) then raise error."
  (with-temp-buffer
    (unless (= (apply 'fsvn-call-command subcommand t args) 0)
      (signal 'fsvn-command-error (list subcommand args (buffer-string))))
    t))

(defun fsvn-command-append-argument (subcommand args)
  (if (fsvn-svn-subcommand-accepted-argument subcommand "--non-interactive")
      (cons "--non-interactive" args)
    args))

(defmacro fsvn-process-filter/sentinel-manager (process event property)
  `(let ((list (process-get ,process ,property))
         ret)
     (while list
       (setq ret (funcall (car list) ,process ,event))
       (setq list (cdr list)))
     ret))

(defun fsvn-process-sentinel-manager (process event)
  (fsvn-process-filter/sentinel-manager process event 'fsvn-process-sentinel-list))

(defun fsvn-process-filter-manager (process event)
  (fsvn-process-filter/sentinel-manager process event 'fsvn-process-filter-list))

(defmacro fsvn-process-add-filter/sentinel (proc sentinel/filter manager property getter setter)
  `(let ((current (,getter proc))
         functions)
     (unless (eq current ,property)
       (,setter proc ,manager)
       (when current
         (process-put proc ,property (cons current nil))))
     (setq functions (process-get proc ,property))
     (setq functions (append functions (list ,sentinel/filter)))
     (process-put proc ,property functions)))

(defun fsvn-process-add-sentinel (proc sentinel)
  (fsvn-process-add-filter/sentinel
   proc sentinel 'fsvn-process-sentinel-manager 'fsvn-process-sentinel-list
   process-sentinel set-process-sentinel))

(defun fsvn-process-add-filter (proc filter)
  (fsvn-process-add-filter/sentinel
   proc filter 'fsvn-process-filter-manager 'fsvn-process-filter-list
   process-filter set-process-filter))

(defmacro fsvn-async-let (varlist &rest body)
  "Execute asynchronous process sequentially.
BODY each form that return process object stopping remaining BODY.
Execute remaining BODY in process-sentinel if process exited normally.
Like `let' binding, varlist bound while executing BODY. (sentinel and filter too)"
  (declare (indent 1))
  `(let ,varlist
     (let (fsvn-var-list)
       (mapc
        (lambda (x)
          (setq fsvn-var-list
                (cons
                 (cond
                  ((consp x)
                   (car x))
                  ((symbolp x)
                   x)
                  (t
                   (error "Unknown type")))
                 fsvn-var-list)))
        ',varlist)
       (fsvn-async-executor ',body fsvn-var-list))))

(defun fsvn-async-create-sentinel (fsvn-original-actor fsvn-var-alist)
  "Create process sentinel FSVN-ORIGINAL-ACTOR that executed in FSVN-VAR-ALIST"
  `(lambda (fsvn-async-proc fsvn-async-event)
     (let ,fsvn-var-alist
       ,(if fsvn-original-actor
            `(,fsvn-original-actor fsvn-async-proc fsvn-async-event)
          `(fsvn-async-default-filter/sentinel fsvn-async-proc fsvn-async-event))
       (when (= (process-exit-status fsvn-async-proc) 0)
         (fsvn-async-executor 
          (process-get fsvn-async-proc 'fsvn-async-remain-forms)
          ',(mapcar 'car fsvn-var-alist)
          fsvn-async-proc)))))

(defun fsvn-async-create-filter (fsvn-original-actor fsvn-var-alist)
  "Create process sentinel/filter FSVN-ORIGINAL-ACTOR that executed in FSVN-VAR-ALIST"
  `(lambda (fsvn-async-proc fsvn-async-event)
     (let ,fsvn-var-alist
       ,(if fsvn-original-actor
            `(,fsvn-original-actor fsvn-async-proc fsvn-async-event)
          `(fsvn-async-default-filter/sentinel fsvn-async-proc fsvn-async-event)))))

(defun fsvn-async-default-filter/sentinel (proc event)
  (with-current-buffer (process-buffer proc)
    (save-excursion
      (goto-char (point-max))
      (insert event))))

(defun fsvn-async-executor (forms variables &optional exited-process)
  (let (ret form suspended)
    (while forms
      (setq form (car forms))
      (setq forms (cdr forms))
      (setq ret (eval form))
      (cond
       ((not (processp ret))) ;; do nothing
       ((process-get ret 'fsvn-async-under-controlled-p)
        ;; Child level of `fsvn-asyc-let'
        ;;FIXME althogh current implement not through this condition.

        ;; works well.
        ;; (fsvn-async-let ()
        ;;   (start-process) 
        ;;   (fsvn-async-let ()
        ;;     (start-process))
        ;;   (start-process))

        ;; not work
        ;; (fsvn-async-let ()
        ;;   (start-process) 
        ;;   (fsvn-async-let ()
        ;;     (start-process)
        ;;     (fsvn-async-let ()
        ;;       (start-process))
        ;;     (start-process))
        ;;   (start-process))

        (let ((var-alist
               (mapcar
                (lambda (var)
                  (cons var (cons (list 'quote (eval var)) nil)))
                variables))
              (delegate (process-get ret 'fsvn-async-delegate-function)))
          (process-put ret 'fsvn-async-delegate-function 
                       `(lambda () (fsvn-async-executor ',forms ',variables)))
          (setq forms nil
                suspended t)))
       (t
        ;; Sibling level in `fsvn-asyc-let'
        (let ((var-alist
               (mapcar
                (lambda (var)
                  (cons var (cons (list 'quote (eval var)) nil)))
                variables)))
          (process-put ret 'fsvn-async-under-controlled-p t)
          (process-put ret 'fsvn-async-remain-forms forms)
          (set-process-sentinel ret
                                (fsvn-async-create-sentinel (process-sentinel ret) var-alist))
          (set-process-filter ret 
                              (fsvn-async-create-filter (process-filter ret) var-alist))
          (when exited-process
            (process-put ret 'fsvn-async-delegate-function 
                         (process-get exited-process 'fsvn-async-delegate-function))))
        (setq forms nil
              suspended t))))
    (when exited-process
      (unless suspended
        (let ((func (process-get exited-process 'fsvn-async-delegate-function)))
          (when func
            (funcall func)))))
    ret))

(defun fsvn-async-invoke (strategies)
  (eval
   (append
    (list 'fsvn-async-let ())
    strategies)))

(defmacro fsvn-process-event-handler (proc event &rest form)
  (declare (indent 2))
  `(let ((BUFFER (process-buffer ,proc)))
     (when (buffer-live-p BUFFER)
       (with-current-buffer BUFFER
         (save-excursion
           ,@form)))))

(defmacro fsvn-process-exit-handler (proc event &rest form)
  (declare (indent 2))
  `(when (eq (process-status ,proc) 'exit)
     (fsvn-process-event-handler ,proc ,event
       ,@form)))

(defun fsvn-command-args-canonicalize (list &optional subcommand)
  "

\(fn LIST)"
  (let (ret)
    (mapc
     (lambda (x)
       (cond
        ((null x))
        ((listp x)
         (setq ret (append 
                    (nreverse (fsvn-command-args-canonicalize x subcommand))
                    ret)))
        ((stringp x)
         ;; first item must be a subcommand.
         (unless subcommand
           (setq subcommand x))
         (when (fsvn-url-p x)
           (cond
            ((fsvn-url-descendant-p (fsvn-ediff-directory) x)
             ;; ediff temp file has @ file name...
             )
            ((and (string= subcommand "move")
                  (fsvn-find-if (lambda (x) (fsvn-url-p x)) ret))
             ;; `move' second url arg must unescape revision mark `@'
             )
            (t
             (setq x (fsvn-url-escape-revision-mark x)))))
         (cond
          ((fsvn-url-repository-p x)
           (setq ret (cons (fsvn-url-encode-string x) ret)))
          ((string-match fsvn-diff-subcommand-arg-regexp x)
           ;; for `diff'
           (let ((arg (match-string 1 x))
                 (url (match-string 2 x)))
             (setq ret (cons (format "--%s=%s" arg (fsvn-url-encode-string url)) ret))))
          (t
           (setq ret (cons x ret)))))
        ((numberp x)
         (setq ret (cons (number-to-string x) ret)))
        (t
         (error "Unexpected type `%s'" x))))
     list)
    (nreverse ret)))

(defun fsvn-build-command-string (subcommand &rest args)
  (let ((real-args (fsvn-command-args-canonicalize args)))
    (mapconcat
     (lambda (x)
       (if (or (string= "" x) (string-match " " x))
           (concat "\"" x "\"")
         x))
     (append (list fsvn-svn-command-internal subcommand) real-args)
     " ")))

(defun fsvn-guess-file-contents-coding-system (flatten-args)
  (let (ignore)
    (catch 'guessed
      (mapc
       (lambda (arg)
         (cond
          (ignore
           (setq ignore nil))
          ((string-match fsvn-diff-subcommand-arg-regexp arg)
           (let ((url (match-string 2 arg))
                 cs magic-name)
             (cond
              ((fsvn-url-repository-p url)
               (when (setq cs (fsvn-config-repository-default-coding-system url))
                 (throw 'guessed cs)))
              ((fsvn-url-local-p url)
               (unless (file-directory-p url)
                 (when (setq cs (fsvn-file-guessed-coding-system url))
                   (throw 'guessed cs)))))))
          ((string-match "^--[a-zA-Z]" arg)
           (setq ignore (assoc arg '("--targets" "--file"))))
          ((not (fsvn-url-local-p arg)))
          (t
           (let ((file (fsvn-magic-create-name arg)))
             (unless (file-directory-p file)
               (throw 'guessed (fsvn-file-guessed-coding-system file)))))))
       flatten-args)
      (default-value 'buffer-file-coding-system))))



(provide 'fsvn-proc)

;;; fsvn-proc.el ends here
