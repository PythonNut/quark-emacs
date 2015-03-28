(eval-when-compile
  (with-demoted-errors
    (require 's)
    (require 'cl-lib)
    (require 'yasnippet)
    (require 'semantic)
    (require 'flycheck)
    (require 'flyspell)))

;; enable semantic code LALR(1) parser
(add-hook 'prog-mode-hook #'semantic-mode)
(with-eval-after-load 'semantic
  (global-semanticdb-minor-mode +1)
  (global-semantic-idle-scheduler-mode +1)
  (global-semantic-idle-summary-mode +1))

(defadvice semantic-idle-summary-idle-function
  (around show-flycheck-error activate preactivate compile)
  (if (and
        (featurep 'flycheck)
        flycheck-mode
        (progn (require 's)
          (flycheck-overlay-errors-at (point))))
    (flycheck-display-error-at-point)
    ad-do-it))

;;; ====================================
;;; flycheck - real-time syntax checking
;;; ====================================

(add-hook 'prog-mode-hook #'global-flycheck-mode +1)
(add-hook 'text-mode-hook #'global-flycheck-mode +1)

(with-eval-after-load 'flycheck
  (defun my-display-error-messages-condensed (errors)
    (-when-let (messages (-keep #'flycheck-error-message errors))
      (when (flycheck-may-use-echo-area-p)
        (require 's)
        (display-message-or-buffer (s-join "\n" messages)
          flycheck-error-message-buffer))))

  (setq flycheck-display-errors-function #'my-display-error-messages-condensed)

  (setq flycheck-indication-mode nil)
  (set-face-background 'flycheck-fringe-warning nil)

  (set-face-attribute 'flycheck-error nil
    :foreground nil
    :background nil
    :underline "#dc322f")

  (set-face-attribute 'flycheck-warning nil
    :foreground nil
    :background nil
    :underline "#b58900")

  (set-face-attribute 'flycheck-info nil
    :foreground nil
    :background nil
    :underline "#268bd2")

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
                        (format "%s/%s"
                          (or (cdr (assq 'error error-counts)) "")
                          (or (cdr (assq 'warning error-counts)) "")))
                      ""))
                  (`interrupted "-")
                  (`suspicious "?"))))
      (concat (if (display-graphic-p) " ✓" " Γ") text))))

;;; =======================================
;;; Flyspell - inline real time spell check
;;; =======================================
(with-eval-after-load 'ispell
  (defadvice ispell-init-process
    (around hide-startup activate preactivate compile)
    (cl-letf (((symbol-function 'message) #'format))
      ad-do-it)))

(with-eval-after-load 'flyspell
  (setq
    flyspell-issue-message-flag nil
    flyspell-issue-welcome-flag nil)

  (add-hook 'flyspell-mode-hook
    (lambda ()
      (define-key flyspell-mode-map (kbd "C-.") nil)
      (define-key flyspell-mode-map (kbd "C-,") nil)
      (diminish 'flyspell-mode (if (display-graphic-p) " f̲" " f"))))

  (if (executable-find "hunspell")
    (setq ispell-program-name "hunspell")
    (when (executable-find "aspell")
      (add-to-list 'ispell-extra-args "--sug-mode=ultra"))))

(add-hook 'text-mode-hook #'flyspell-mode)
(add-hook 'prog-mode-hook #'flyspell-prog-mode)

;;; =============================================
;;; yasnippet -- extensible programmable snippets
;;; =============================================

(defun yasnippet-onetime-setup ()
  (yas-global-mode +1)
  (remove-hook 'prog-mode-hook
    #'yasnippet-onetime-setup-proxy)
  (remove-hook 'text-mode-hook
    #'yasnippet-onetime-setup-proxy))

(defun yasnippet-onetime-setup-proxy ()
  (add-hook 'first-change-hook #'yasnippet-onetime-setup nil t))

(add-hook 'emacs-startup-hook
  (lambda ()
    (add-hook 'first-change-hook #'yasnippet-onetime-setup nil t)
    (add-hook 'prog-mode-hook #'yasnippet-onetime-setup-proxy)
    (add-hook 'text-mode-hook #'yasnippet-onetime-setup-proxy)))

(setq yas-verbosity 0)

(with-eval-after-load 'yasnippet
  (set-face-attribute 'yas-field-highlight-face nil
    :foreground nil
    :background nil
    :inherit 'region)
  (add-hook 'yas-global-mode-hook
    (lambda ()
      (diminish 'yas-minor-mode (if (display-graphic-p) " ¥" " Y"))))
  (setq
    yas-snippet-dirs (list
                       (concat
                         user-emacs-directory
                         "data/snippets")))
  (unless yas-global-mode
    (yasnippet-onetime-setup))
  (yas-reload-all))

(provide 'config-intel)
