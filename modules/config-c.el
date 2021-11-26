;; -*- lexical-binding: t -*-
(eval-when-compile
  (require 'config-macros)
  (require 'config-package))

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
(add-hook 'c-mode-hook
          (my/defun-as-value my/c-mode-maybe-lsp (&rest _)
            (unless (file-remote-p buffer-file-name)
              (lsp-deferred))))

(use-package cc-mode
  :ensure nil
  :config
  (setq c-default-style "k&r")

  (define-advice c-indent-new-comment-line
      (:after (&rest _args) smart-extend-comments)
    (when (and
            (looking-at (rx (zero-or-more (not-char ?\n)) "*/"))
            (not (looking-at (rx (zero-or-more (not-char ?\n)) "/*"))))
       (save-excursion
         (re-search-forward (rx "*/") (line-end-position))
         (forward-char -2)
         (newline)
         (indent-according-to-mode))))

  (use-package clang-format
    :defer-install t
    :commands (clang-format
               clang-format-region
               clang-format-buffer))

  (use-package flycheck-clang-analyzer
    :ensure t
    :after flycheck
    :config (flycheck-clang-analyzer-setup))

  (cl-macrolet
      ((my/setup-cc-mode
        (mode hook)
        `(add-hook ,hook (lambda ()
                           (when (eq major-mode ,mode)
                             (eldoc-mode +1))))))

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
