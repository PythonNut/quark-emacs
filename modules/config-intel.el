;; enable semantic code LALR(1) parser
(add-hook 'prog-mode-hook 'semantic-mode)
(eval-after-load 'semantic
  '(progn
     (global-semanticdb-minor-mode +1)
     (global-semantic-idle-scheduler-mode +1)
     (global-semantic-idle-summary-mode +1)))

;;; ====================================
;;; flycheck - real-time syntax checking
;;; ====================================

(add-hook 'prog-mode-hook '(lambda () (require 'flycheck)))

(defun my-display-error-messages-condensed (errors)
  (-when-let (messages (-keep #'flycheck-error-message errors))
    (when (flycheck-may-use-echo-area-p)
      (display-message-or-buffer (s-join "\n" messages)
        flycheck-error-message-buffer))))

(eval-after-load 'flycheck
  '(progn
     (setq flycheck-display-errors-function #'my-display-error-messages-condensed)

     (setq flycheck-indication-mode nil)
     (set-face-background 'flycheck-fringe-warning nil)

     (set-face-background 'flycheck-warning nil)
     (set-face-foreground 'flycheck-warning nil)
     (set-face-background 'flycheck-error nil)
     (set-face-background 'flycheck-error nil)
     (set-face-foreground 'flycheck-info nil)
     (set-face-foreground 'flycheck-info nil)

     (set-face-attribute 'flycheck-error nil :underline "#dc322f")
     (set-face-attribute 'flycheck-warning nil :underline "#b58900")
     (set-face-attribute 'flycheck-info nil :underline "#268bd2")

     ;; please don't give me emacs-lisp stylistic advice
     (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))

     (defun flycheck-mode-line-status-text (&optional status)
       (let ((text (pcase (or status flycheck-last-status-change)
                     (`not-checked "")
                     (`no-checker "-")
                     (`running "*")
                     (`errored "!")
                     (`finished
                       (if flycheck-current-errors
                         (let ((error-counts (flycheck-count-errors
                                               flycheck-current-errors)))
                           (format ":%s/%s"
                             (or (cdr (assq 'error error-counts)) 0)
                             (or (cdr (assq 'warning error-counts)) 0)))
                         ""))
                     (`interrupted "-")
                     (`suspicious "?"))))
         (concat " ✓" text)))
     ;; (add-hook 'flycheck-mode-hook #'flycheck-haskell-setup)
     (global-flycheck-mode +1)))

;;; =======================================
;;; Flyspell - inline real time spell check
;;; =======================================
;; text mode
(eval-after-load 'flyspell-mode
  '(progn
     (when (locate-file "hunspell" exec-path)
       (setq ispell-program-name "hunspell")
       (add-to-list 'ispell-extra-args "--sug-mode=ultra"))
     (set 'flyspell-issue-message-flag nil)))

(add-hook 'flyspell-mode-hook
  '(lambda ()
     (diminish 'flyspell-mode " ῶ")
     (diminish 'flyspell-mode " ῶ")))

(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
