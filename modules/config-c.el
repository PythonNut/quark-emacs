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
                                '((company-lsp
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

  (cl-macrolet
      ((my/setup-cc-mode
        (mode hook)
        `(add-hook ,hook (lambda ()
                           (when (eq major-mode ,mode)
                             (eldoc-mode +1)
                             (aggressive-indent-mode +1)
                             (helm-gtags-mode +1)
                             (semantic-idle-summary-mode -1))))))

    (with-no-warnings
      (my/generate-calls
          'my/setup-cc-mode
        '(('c++-mode  'c++-mode-hook)
          ('objc-mode 'objc-mode-hook)
          ('c-mode    'c-mode-hook)))))

  (with-eval-after-load 'smartparens
    (eval-when-compile
      (use-package smartparens))
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
