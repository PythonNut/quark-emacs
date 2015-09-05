;; -*- lexical-binding: t -*-

(eval-when-compile
  (with-demoted-errors
    (require 'cl-lib)
    (require 'key-chord)))

(delete-selection-mode +1)
(global-hl-line-mode +1)
(subword-mode +1)

(defun my/auto-compression-onetime-setup ()
  (auto-compression-mode +1)
  (remove-hook 'find-file-hook #'my/auto-compression-onetime-setup))

(add-hook 'find-file-hook #'my/auto-compression-onetime-setup)

;; encryption mode
(setq epa-file-name-regexp "\\.\\(gpg\\|asc\\)$")
(epa-file-name-regexp-update)
(setenv "GPG_AGENT_INFO" nil)

(setq-default major-mode
  (lambda ()
    nil
    (if buffer-file-name
      (fundamental-mode)
      (let ((buffer-file-name (buffer-name)))
        (set-auto-mode)))))

(defun my/raise-minor-mode-map-alist (mode-symbol)
  "Raise `minor-mode-map-alist' priority of MODE-SYMBOL."
  (let ((x (assq mode-symbol minor-mode-map-alist)))
    (and x (setq minor-mode-map-alist (cons x (delq x minor-mode-map-alist))))))

(defun my/lower-minor-mode-map-alist (mode-symbol)
  "Lower `minor-mode-map-alist' priority of MODE-SYMBOL."
  (let ((rel (assq mode-symbol minor-mode-map-alist)))
    (setq minor-mode-map-alist (append (delete rel minor-mode-map-alist) (list rel)))))

;; basically, a mapcar for macros
(defmacro my/generate-calls (operator arglists)
  `(progn
     ,@(mapcar (lambda (arglist) `(,operator ,@arglist)) arglists)))

(defmacro my/generate-calls-single (operator arglists)
  `(progn
     ,@(mapcar (lambda (arglist) `(,operator (,@arglist))) arglists)))

(cl-letf (((symbol-function 'message) #'format))
  (key-chord-mode +1))

(defun really-kill-emacs ()
  "Like `kill-emacs', but ignores `kill-emacs-hook'."
  (interactive)
  (let (kill-emacs-hook)
    (kill-emacs)))

(defun brutally-kill-emacs ()
  "Use `call-process' to send ourselves a KILL signal."
  (interactive)
  (call-process "kill" nil nil nil "-9" (number-to-string (emacs-pid))))

(show-paren-mode +1)

(defun nadvice/show-paren-function ()
  "If the matching paren is offscreen, show the matching line in the
echo area. Has no effect if the character before point is not of
the syntax class ')'."
  (let* ((cb (char-before (point)))
          (matching-text (and cb
                           (char-equal (char-syntax cb) ?\) )
                           (blink-matching-open))))
    (when matching-text (message matching-text))))

(advice-add 'show-paren-function :after #'nadvice/show-paren-function)

(provide 'config-modes)
