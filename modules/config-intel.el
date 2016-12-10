;; -*- lexical-binding: t -*-
(require 'cl-lib)
(require 'config-tramp)

;; enable semantic code LALR(1) parser
(add-hook 'prog-mode-hook #'semantic-mode)
(with-eval-after-load 'semantic
  (eval-when-compile
    (with-demoted-errors "Load error: %s"
      (require 'semantic)))

  (global-semanticdb-minor-mode +1)
  (global-semantic-idle-scheduler-mode +1)
  (global-semantic-idle-summary-mode +1)

  (defun nadvice/semantic-idle-summary-idle-function (old-fun &rest args)
    (if (and (bound-and-true-p flycheck-mode)
             (flycheck-overlay-errors-at (point)))
        (flycheck-display-error-at-point)
      (apply old-fun args)))

  (advice-add 'semantic-idle-summary-idle-function
              :around
              #'nadvice/semantic-idle-summary-idle-function))

;;; ====================================
;;; flycheck - real-time syntax checking
;;; ====================================
(global-flycheck-mode +1)

(with-eval-after-load 'flycheck
  (eval-when-compile
    (with-demoted-errors "Load error: %s"
      (require 'flycheck)))

  (setq flycheck-display-errors-function #'my/display-error-messages-condensed
        flycheck-indication-mode nil)

  (defun my/display-error-messages-condensed (errors)
    (require 'dash)
    (-when-let (messages (-keep #'flycheck-error-message errors))
      (when (flycheck-may-use-echo-area-p)
        (display-message-or-buffer (mapconcat #'identity messages "\n")
                                   flycheck-error-message-buffer))))

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
    (cl-letf (((symbol-function 'message)
               (lambda (&rest args)
                 (when args
                   (apply #'format args)))))
      (apply old-fun args)))

  (advice-add 'ispell-init-process :around #'nadvice/ispell-init-process))

(with-eval-after-load 'flyspell
  (eval-when-compile
    (with-demoted-errors "Load error: %s"
      (require 'flyspell)))

  (setq flyspell-issue-message-flag nil
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

(when (or (executable-find "ispell")
          (executable-find "aspell")
          (executable-find "hunspell"))
  (add-hook 'text-mode-hook #'flyspell-mode)
  (add-hook 'prog-mode-hook #'flyspell-prog-mode))

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

(defun my/ivy-yasnippet (_prompt choices &optional display-fn)
  "Use ivy to select a snippet. Put this into `yas-prompt-functions.'"
  (if (require 'ivy nil t)
      (let* ((disp-fn (or display-fn 'identity))
             (cands (mapcar (lambda (x) (cons (funcall disp-fn x) x)) choices))
             (result (ivy-read "Snippet: " (mapcar #'car cands))))
        (if (null result)
            (signal 'quit "user quit!")
          (cdr (assoc result cands))))
    nil))

(with-eval-after-load 'yasnippet
  (eval-when-compile
    (with-demoted-errors "Load error: %s"
      (require 'yasnippet)))

  (set-face-attribute 'yas-field-highlight-face nil
                      :foreground nil
                      :background nil
                      :inherit 'region)
  (add-hook 'yas-global-mode-hook
            (lambda ()
              (diminish 'yas-minor-mode (if (display-graphic-p) " ¥" " Y"))))
  (setq yas-snippet-dirs (list (locate-user-emacs-file "data/snippets"))
        yas-key-syntaxes (remove "w" yas-key-syntaxes))
  (yas-global-mode +1)
  (yas-reload-all)

  (add-to-list 'yas-prompt-functions #'my/ivy-yasnippet))

;; also use yasnippets for new file templates
(defvar my/yas-template-dir (locate-user-emacs-file "data/templates"))

(defun my/yatemplate-expand-yas-buffer ()
  "Expand the whole buffer with `yas-expand-snippet'."
  (require 'yasnippet)
  (yas-expand-snippet (buffer-string) (point-min) (point-max))
  (evil-insert-state))

(defun my/yatemplate-fill-alist ()
  "Fill `auto-insert-alist'."
  (dolist (filename (nreverse (sort (file-expand-wildcards
                                     (concat my/yas-template-dir
                                             "**/*"))
                                    #'string<)))
    (let* ((split-name (split-string filename "="))
           (file-regex (if (eq (length split-name) 2)
                           (nth 1 split-name)
                         (lwarn "yatemplate" 'error
                                "%s filename does not contain exactly one colon"
                                filename)
                         nil)))
      (when file-regex
        (push (cons (intern file-regex)
                    (vector filename #'my/yatemplate-expand-yas-buffer))
              auto-insert-alist)))))

(with-eval-after-load 'autoinsert
  (setq auto-insert-alist nil)
  (my/yatemplate-fill-alist))

(add-hook 'after-change-major-mode-hook
          (lambda ()
            (when (= (point-min) (point-max))
              (auto-insert))))

;;; ==================================
;;; VLF intelligently edit large files
;;; ==================================
(require 'config-package)

(package-deferred-install 'vlf
    :autoload-names '('vlf))

(defun nadvice/abort-if-file-too-large (_old-fun &rest args)
  (cl-destructuring-bind (size _op-type _filename) args
    (when (and size
               (not (zerop size))
               large-file-warning-threshold
               (< large-file-warning-threshold size))
      (unless (package-installed-p 'vlf)
             (save-window-excursion
               (package-install 'vlf)))
      (advice-remove 'abort-if-file-too-large
                     #'nadvice/abort-if-file-too-large)
           (require 'vlf-setup)
           (apply #'abort-if-file-too-large args))))

(advice-add 'abort-if-file-too-large :around #'nadvice/abort-if-file-too-large)

(defun my/vlf-hook ()
  (setq bidi-display-reordering nil)
  (flyspell-mode -1)
  (flycheck-mode -1)
  (ws-butler-mode -1)
  (visual-line-mode -1)
  (adaptive-wrap-prefix-mode -1)
  (set (make-variable-buffer-local 'global-hl-line-mode) nil)
  (set (make-variable-buffer-local 'column-number-mode) nil)
  (message "Use C-c C-v → VLF"))

(add-hook 'vlf-mode-hook #'my/vlf-hook)

(provide 'config-intel)
