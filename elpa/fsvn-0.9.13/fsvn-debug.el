;;; fsvn-debug.el --- Debug utility for fsvn.el


;;; History:
;; 

;;; Commentary:
;; 

;;; Code:



(with-no-warnings
  (defvar obarray))



(defconst fsvn-debug-buffer "*fsvn-debug*")
(defvar fsvn-debug-enabled nil)

(defun fsvn-debug (&rest messages)
  (cond
   ((null fsvn-debug-enabled))
   ((= (length messages) 1)
    ;;todo change to with-current-buffer
    (save-excursion
      (let ((debug (get-buffer-create fsvn-debug-buffer))
            (message (car messages))
            prev)
        ;; this is `call-process' argument...
        (when (eq message t)
          (setq message (current-buffer)))
        (cond
         ((bufferp message)
          (set-buffer message)
          (append-to-buffer debug (point-min) (point-max)))
         ((stringp message)
          (set-buffer debug)
          (goto-char (point-max))
          (insert message)
          (insert "\n"))
         (t
          (set-buffer debug)
          (goto-char (point-max))
          (pp message (current-buffer))
          (insert "\n"))))))
   (t
    (mapc 'fsvn-debug messages))))

(defun fsvn-cleanup-obarray ()
  (interactive)
  (mapatoms
   (lambda (x)
     (when (string-match "^fsvn" (symbol-name x))
       (when (fboundp x)
         (fset x nil))
       (when (boundp x)
         ;; don't change custom value.
         (cond
          ((null (default-value x))
           (makunbound x))
          ((not (equal (symbol-value x) (default-value x)))
           (set x (default-value x)))))))
   obarray))

(provide 'fsvn-debug)

;;; fsvn-debug.el ends here
