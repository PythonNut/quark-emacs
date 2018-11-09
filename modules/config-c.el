;; -*- lexical-binding: t -*-
(eval-when-compile (require 'config-macros))

;; =============================================================================
;; C-like ======================================================================
;; =============================================================================

(defun c-or-c++-header ()
  "Sets either c-mode or c++-mode, whichever is appropriate."
  (interactive)
  (let ((c-file (concat (file-name-sans-extension
                         (buffer-file-name))
                        ".c")))
    (if (file-exists-p c-file)
        (c-mode)
      (c++-mode))))

(add-to-list 'auto-mode-alist '("\\.h\\'" . c-or-c++-header))

(use-package cc-mode
  :ensure nil
  :config
  (setq c-default-style "k&r")

  (advice-add
   'c-indent-new-comment-line :after
   (my/defun-as-value nadvice/c-indent-new-comment-line (&rest _args)
     (when (and
            (looking-at (rx (zero-or-more (not-char ?\n)) "*/"))
            (not (looking-at (rx (zero-or-more (not-char ?\n)) "/*"))))
       (save-excursion
         (re-search-forward (rx "*/") (line-end-position))
         (forward-char -2)
         (newline)
         (indent-according-to-mode)))))

  (use-package rtags
    :init
    (require 'rtags)

    :config
    (setq rtags-completions-enabled t
          rtags-autostart-diagnostics t
          rtags-tramp-enabled t
          rtags-rc-log-enabled t
          rtags-enable-unsaved-reparsing t
          rtags-use-multiple-cursors t)

    (rtags-set-periodic-reparse-timeout 5))

  (use-package company-rtags)
  (use-package flycheck-rtags)
  (use-package helm-rtags)
  (use-package ivy-rtags)

  (use-package clang-format
    :defer-install t
    :commands (clang-format
               clang-format-region
               clang-format-buffer))

  (use-package flycheck-clang-analyzer
    :ensure t
    :after flycheck
    :config (flycheck-clang-analyzer-setup))

  (eval-and-compile
    (cl-macrolet
        ((company-define-specific-modes
          (mode)
          `(progn
             (add-hook ,mode
                       (lambda ()
                         (require 'company)
                         (require 'cl-lib)
                         (let ((old-backends company-backends))
                           (set (make-local-variable 'company-backends)
                                '((
                                   ;; company-irony-c-headers
                                   ;; company-irony
                                   company-rtags
                                   company-yasnippet
                                   company-files)
                                  (company-dabbrev-code)
                                  company-dabbrev))))))))
      (with-no-warnings
        (my/generate-calls-single
            'company-define-specific-modes
          '('c++-mode-hook
            'objc-mode-hook
            'c-mode-hook)))))

  (defun my/fontify-string (str mode)
    "Return STR fontified according to MODE."
    (with-temp-buffer
      (insert str)
      (delay-mode-hooks (funcall mode))
      (font-lock-default-function mode)
      (font-lock-default-fontify-region
       (point-min) (point-max) nil)
      (buffer-string)))

  (defun my/rtags-eldoc-function ()
    (let ((summary (rtags-get-summary-text)))
      (and summary
           (my/fontify-string
            (replace-regexp-in-string
             "{[^}]*$" ""
             (mapconcat
              (lambda (str) (if (= 0 (length str)) "//" (string-trim str)))
              (split-string summary "\r?\n")
              " "))
            major-mode))))

  (cl-macrolet
      ((my/setup-cc-mode
        (mode hook)
        `(add-hook ,hook (lambda ()
                           (when (eq major-mode ,mode)
                             ;; (irony-mode +1)
                             (setq-local eldoc-documentation-function #'my/rtags-eldoc-function)
                             (eldoc-mode +1)
                             ;; (irony-eldoc +1)
                             (aggressive-indent-mode +1)
                             (helm-gtags-mode +1)
                             (semantic-idle-summary-mode -1))))))

    (with-no-warnings
      (my/generate-calls
          'my/setup-cc-mode
        '(('c++-mode  'c++-mode-hook)
          ('objc-mode 'objc-mode-hook)
          ('c-mode    'c-mode-hook)))))

  (use-package smartparens
    :config
    (sp-with-modes
        '(c++-mode objc-mode c-mode)
      (sp-local-pair "/*" "*/" :post-handlers
                     '(:add
                       ("* [i]|\n[i]" newline evil-ret)
                       (" " c-context-line-break c-indent-new-comment-line)))
      (sp-local-pair "{" nil :post-handlers
                     '(:add
                       ("||\n[i]" "RET")
                       ("| " "SPC")))))

  (define-key c-mode-map (kbd "C-c o") #'ff-find-other-file)
  (define-key c++-mode-map (kbd "C-c o") #'ff-find-other-file)

  (define-key c-mode-map (kbd "RET") #'c-context-line-break)
  (define-key c++-mode-map (kbd "RET") #'c-context-line-break)

  (define-key c-mode-map (kbd "M-RET") #'srefactor-refactor-at-point)
  (define-key c++-mode-map (kbd "M-RET") #'srefactor-refactor-at-point)

  (define-key c-mode-map (kbd "M-.") #'my/jump-to-definition-dwim)
  (define-key c++-mode-map (kbd "M-.") #'my/jump-to-definition-dwim))

(provide 'config-c)
