;;; ==============================================
;;; The Minibuffer - command and minibuf goes here
;;; ==============================================
;;; SMEX - interactive command interface
(setq smex-save-file "~/.emacs.d/smex-items")
(setq smex-history-length 1000)
;; SMEX M-x async load

(global-set-key (kbd "C-x M-x") 'execute-extended-command)

(defun auto-smex ()
  (interactive)
  (and (fboundp 'smex-initialize)
    (or (boundp 'smex-cache)
      (smex-initialize)))
  (global-set-key (kbd "M-x") 'smex)
  (smex))

(global-set-key (kbd "M-x") 'auto-smex)

(defadvice smex (around space-inserts-hyphen activate compile)
  (let ((ido-cannot-complete-command
          `(lambda ()
             (interactive)
             (if (string= " " (this-command-keys))
               (insert ?-)
               (funcall ,ido-cannot-complete-command)))))
    ad-do-it))

;;; recursive minibuffers
(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode t)

;; don't let the cursor go into minibuffer prompt
(setq minibuffer-prompt-properties
  '(read-only t
     point-entered
     minibuffer-avoid-prompt
     face
     minibuffer-prompt))

(define-key evil-normal-state-map (kbd "SPC SPC") 'auto-smex)

