;; -*- lexical-binding: t -*-

(eval-when-compile
  (with-demoted-errors "Load error: %s"
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

(defun nadvice/semantic-idle-summary-idle-function (old-fun &rest args)
  (if (and
       (featurep 'flycheck)
       flycheck-mode
       (progn (require 's)
              (flycheck-overlay-errors-at (point))))
      (flycheck-display-error-at-point)
    (apply old-fun args)))

(advice-add 'semantic-idle-summary-idle-function
            :around
            #'nadvice/semantic-idle-summary-idle-function)

;;; ====================================
;;; flycheck - real-time syntax checking
;;; ====================================

(add-hook 'prog-mode-hook #'global-flycheck-mode +1)
(add-hook 'text-mode-hook #'global-flycheck-mode +1)

(with-eval-after-load 'flycheck
  (defun my/display-error-messages-condensed (errors)
    (require 'dash)
    (-when-let (messages (-keep #'flycheck-error-message errors))
      (when (flycheck-may-use-echo-area-p)
        (require 's)
        (display-message-or-buffer (s-join "\n" messages)
                                   flycheck-error-message-buffer))))

  (setq flycheck-display-errors-function #'my/display-error-messages-condensed)

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

  (defun nadvice/flycheck-mode-line-status-text (&optional status)
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
      (concat (if (display-graphic-p) " ✓" " Γ") text)))

  (advice-add 'flycheck-mode-line-status-text :override
              #'nadvice/flycheck-mode-line-status-text))

;;; =======================================
;;; Flyspell - inline real time spell check
;;; =======================================
(with-eval-after-load 'ispell
  (defun nadvice/ispell-init-process (old-fun &rest args)
    (cl-letf (((symbol-function 'message) #'format))
      (apply old-fun args)))

  (advice-add 'ispell-init-process :around #'nadvice/ispell-init-process))

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

(defun my/yasnippet-onetime-setup ()
  (require 'yasnippet)
  (remove-hook 'first-change-hook #'my/yasnippet-onetime-setup))

(add-hook 'emacs-startup-hook
          (lambda ()
            (add-hook 'first-change-hook #'my/yasnippet-onetime-setup)))

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
                     (expand-file-name
                      "data/snippets"
                      user-emacs-directory)))
  (yas-global-mode +1)
  (yas-reload-all))

(provide 'config-intel)
