(eval-when-compile
  (progn
    (require 'cl)
    (require 'key-chord)))

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
(auto-compression-mode +1)
(transient-mark-mode +1)
(delete-selection-mode +1)
(global-hl-line-mode +1)
(column-number-mode +1)

;; encryption mode
(setq epa-file-name-regexp "\\.\\(gpg\\|asc\\)$")
(epa-file-name-regexp-update)
(setenv "GPG_AGENT_INFO" nil)

(defun raise-minor-mode-map-alist (mode-symbol)
  "Raise `minor-mode-map-alist' priority of MODE-SYMBOL."
  (let ((x (assq mode-symbol minor-mode-map-alist)))
    (and x (setq minor-mode-map-alist (cons x (delq x minor-mode-map-alist))))))

(defun lower-minor-mode-map-alist (mode-symbol)
  "Lower `minor-mode-map-alist' priority of MODE-SYMBOL."
  (let ((rel (assq mode-symbol minor-mode-map-alist)))
    (setq minor-mode-map-alist (append (delete rel minor-mode-map-alist) (list rel)))))

;; basically, a mapcar for macros
(defmacro generate-calls (operator arglists)
  `(progn
     ,@(mapcar (lambda (arglist) `(,operator ,@arglist)) arglists)))

(defmacro generate-calls-single (operator arglists)
  `(progn
     ,@(mapcar (lambda (arglist) `(,operator (,@arglist))) arglists)))

;; disable annoying "blah got redefined" messages
;; (defadvice ad--defalias-fset (around supress-messages activate)
;;   (require 'noflet)
;;   (noflet ((message (&rest args)))
;;     ad-do-it))

(key-chord-mode +1)

(defun enable-debugging ()
  (interactive)
  (setq debug-on-error t))

(defun disable-debugging ()
  (interactive)
  (setq debug-on-error nil))

(defun really-kill-emacs ()
  "Like `kill-emacs', but ignores `kill-emacs-hook'."
  (interactive)
  (let (kill-emacs-hook)
    (kill-emacs)))

(defun brutally-kill-emacs ()
  "Use `call-process' to send ourselves a KILL signal."
  (interactive)
  (call-process "kill" nil nil nil "-9" (number-to-string (emacs-pid))))

(provide 'config-modes)
