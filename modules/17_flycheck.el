
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

