;; -*- lexical-binding: t -*-
(require 'cl-lib)
(require 'config-package)

(eval-when-compile
  (with-demoted-errors "Load error: %s"
    (require 'evil)
    (require 'config-setq)))

;; =============================================================================
;; Emacs Lisp ==================================================================
;; =============================================================================

(add-hook 'lisp-interaction-mode-hook #'auto-save-mode)

(defun emacs-lisp-goto-definition ()
  (interactive)
  (find-function (function-called-at-point)))

(defun replace-last-sexp ()
  (interactive)
  (let ((value (eval (elisp--preceding-sexp))))
    (kill-sexp -1)
    (insert (format "%S" value))))

(use-package auto-compile
  :init (defun my/auto-compile-onetime-setup ()
          (require 'auto-compile)
          (auto-compile-on-save-mode +1)
          (remove-hook 'before-save-hook #'my/auto-compile-onetime-setup t))
  :config
  (setq auto-compile-display-buffer nil
        auto-compile-mode-line-counter t))

(use-package eldoc
  :ensure nil
  :diminish eldoc-mode
  :config
  (eval-when-compile
    (with-demoted-errors "Load error: %s"
      (require 'flycheck)))

  (defun nadvice/eldoc-display-message-no-interference-p (old-fun &rest args)
    (and (apply old-fun args)
         (not (and (my/sp-on-delimiter-p)
                   (not (minibufferp))))
         (not (and (bound-and-true-p flycheck-mode)
                   (flycheck-overlay-errors-at (point))))))

  (advice-add 'eldoc-display-message-no-interference-p :around
              #'nadvice/eldoc-display-message-no-interference-p))

(use-package lisp-mode
  :ensure nil
  :config
  (use-package smartparens
    :config
    (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
    (sp-local-pair 'emacs-lisp-mode "`" nil :when '(sp-in-string-p)))

  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (setq mode-name (if (display-graphic-p) "λ" "EL"))

              (eldoc-mode +1)
              (auto-indent-mode -1)
              (aggressive-indent-mode +1)
              (add-hook 'before-save-hook
                        #'my/auto-compile-onetime-setup nil t)))

  (define-key emacs-lisp-mode-map (kbd "C-c e") #'replace-last-sexp)
  (define-key emacs-lisp-mode-map (kbd "M-.") #'emacs-lisp-goto-definition)
  (define-key emacs-lisp-mode-map (kbd "M-,") #'evil-jump-backward)

  (use-package evil
    :config
    (evil-define-key 'normal emacs-lisp-mode-map "gd"
      #'emacs-lisp-goto-definition)))

(use-package suggest
  :defer-install t
  :commands (suggest suggest-update))

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

  (defun nadvice/c-indent-new-comment-line (&rest _args)
    (when (and
           (looking-at (rx (zero-or-more (not-char ?\n)) "*/"))
           (not (looking-at (rx (zero-or-more (not-char ?\n)) "/*"))))
      (save-excursion
        (re-search-forward (rx "*/") (line-end-position))
        (forward-char -2)
        (newline)
        (indent-according-to-mode))))

  (advice-add 'c-indent-new-comment-line :after
              #'nadvice/c-indent-new-comment-line)
  
  (use-package irony-eldoc
    :defer-install t
    :commands (irony-eldoc))

  (use-package flycheck-irony
    :defer-install t
    :commands (flycheck-irony-setup))

  (use-package irony
    :defer-install t
    :commands (irony-mode
               irony-version
               irony-server-kill
               irony-cdb-autosetup-compile-options
               irony-cdb-menu
               irony-cdb-clang-complete
               irony-cdb-json
               irony-cdb-json-add-compile-commands-path
               irony-cdb-libclang
               irony-completion-at-point
               irony-completion-at-point-async)
    :config
    (setq irony-user-dir (locate-user-emacs-file "data/irony"))
    (flycheck-irony-setup)
    (add-hook 'irony-mode-hook 'irony-eldoc)
    (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

  (use-package company-irony
    :defer-install t
    :commands (company-irony
               company-irony-setup-begin-commands)
    :config
    (setq company-irony-ignore-case t)

    (defun nadvice/company-irony--filter-candidates (prefix candidates)
      (let ((regex (concat "\\`"
                           (mapconcat
                            (lambda (x)
                              (setq x (string x))
                              (concat "[^" x "]*" (regexp-quote x)))
                            prefix
                            "")))
            (case-fold-search company-irony-ignore-case))
        (cl-loop for candidate in candidates
                 when (string-match-p regex (car candidate))
                 collect (propertize (car candidate) 'company-irony candidate))))

    (use-package company-irony
      :defer-install t
      :config
      (advice-add 'company-irony--filter-candidates :override
                  #'nadvice/company-irony--filter-candidates))

    (defun nadvice/irony-completion-post-complete (old-fun &rest args)
      (unless (irony-eldoc--which-funcall)
        (apply old-fun args)))

    (advice-add 'irony-completion-post-complete :around
                #'nadvice/irony-completion-post-complete))

  (use-package company-irony-c-headers
    :defer-install t
    :defer-install t
    :commands (company-irony-c-headers))

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
    (with-demoted-errors "Load error: %s"
      (require 'config-setq))

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
                                '((company-irony-c-headers
                                   company-irony
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
                             (irony-mode +1)
                             (eldoc-mode +1)
                             (irony-eldoc +1)
                             (auto-indent-mode -1)
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

  (use-package srefactor
    :defer-install t
    :commands (srefactor-refactor-at-point)
    :config
    (evil-set-initial-state 'srefactor-ui-menu-mode 'emacs))

  (define-key c-mode-map (kbd "C-c o") #'ff-find-other-file)
  (define-key c++-mode-map (kbd "C-c o") #'ff-find-other-file)

  (define-key c-mode-map (kbd "RET") #'c-context-line-break)
  (define-key c++-mode-map (kbd "RET") #'c-context-line-break)

  (define-key c-mode-map (kbd "M-RET") #'srefactor-refactor-at-point)
  (define-key c++-mode-map (kbd "M-RET") #'srefactor-refactor-at-point)

  (define-key c-mode-map (kbd "M-.") #'my/jump-to-definition-dwim)
  (define-key c++-mode-map (kbd "M-.") #'my/jump-to-definition-dwim))

(use-package arduino-mode
  :defer-install t
  :commands (arduino-mode)
  :mode (("\\.pde\\'" . arduino-mode)
         ("\\.ino\\'" . arduino-mode))
  :config
  (use-package company-arduino
    :defer-install t
    :commands (company-arduino-append-include-dirs
               company-arduino-sketch-directory-p
               company-arduino-turn-on
               company-arduino-turn-off))

  (add-hook 'irony-mode-hook 'company-arduino-turn-on))

(use-package cuda-mode
  :defer-install t
  :commands (cuda-mode)
  :mode (("\\.cu\\'" . cuda-mode)
         ("\\.cuh\\'" . cuda-mode)))

(use-package glsl-mode
  :defer-install t
  :commands (glsl-mode)
  :mode (("\\.vert\\'" . glsl-mode)
         ("\\.frag\\'" . glsl-mode)
         ("\\.geom\\'" . glsl-mode)
         ("\\.glsl\\'" . glsl-mode)))

(use-package asy-mode
  :defer-install t
  :commands (asy-mode)
  :recipe (asy-mode :type git
                    :host github
                    :repo "vectorgraphics/asymptote"
                    :files ("base/*.el"))
  :mode ("\\.asy$" . asy-mode))

;; =============================================================================
;; Javascript ==================================================================
;; =============================================================================

(use-package js2-mode
  :defer-install t
  :commands (js2-minor-mode
             js2-mode
             js2-highlight-unused-variables-mode
             js2-imenu-extras-mode
             js2-imenu-extras-setup
             js2-jsx-mode)
  :mode (("\\.js\\'" . js2-mode)
         ("\\.jsx\\'" . js2-jsx-mode))
  :interpreter ("node" . js2-mode)
  :config
  (use-package js2-refactor
    :defer-install t
    :commands (js2r-add-keybindings-with-prefix
               js2r-add-keybindings-with-modifier
               js2r-extract-var
               js2-refactor-mode))

  (set-face-foreground 'js2-external-variable
                       (face-foreground 'default))

  (set-face-attribute 'js2-external-variable nil :weight 'extra-bold)
  (set-face-attribute 'js2-external-variable nil :underline t)
  (js2r-add-keybindings-with-prefix "C-c C-r")

  (use-package smartparens
    :config
    (sp-local-pair 'js2-mode "{" nil :post-handlers
                   '(:add
                     ("||\n[i]" "RET")
                     ("| " "SPC"))))

  (setq js2-basic-offset 2)

  (add-hook 'js2-mode-hook #'js2-refactor-mode)
  (add-hook 'js2-mode-hook (lambda ()
                             (setq mode-name "JS"))))


(use-package json-mode
  :defer-install t
  :commands (json-mode
             json-mode-show-path
             json-mode-beautify)
  :mode (("\\.json$"   . json-mode)
         ("\\.jsonld$" . json-mode)))

;; =============================================================================
;; Shell Scripts ===============================================================
;; =============================================================================

;; bind zsh files to the zsh submode of sh-mode
(use-package sh-script
  :mode ("\\.zsh\\'" . sh-mode)
  :ensure nil
  :config
  (add-hook 'sh-mode-hook
            (lambda ()
              (setq mode-name "sh")
              (if (string-match-p (rx ".zsh" line-end) buffer-file-name)
                  (sh-set-shell "zsh"))))

  (defun my/sh-smart-newline (&optional arg interactive)
    (interactive "*P\np")
    (newline arg interactive)
    (when (looking-at (rx (or "done"
                              "esac"
                              "fi")))
      (save-excursion
        (newline)
        (indent-according-to-mode))
      (indent-according-to-mode)))

  (define-key sh-mode-map (kbd "<remap> <newline>") #'my/sh-smart-newline)

  (sp-with-modes '(sh-mode)
    (sp-local-pair "do" "done"
                   :when '(("SPC" "RET" "<evil-ret>"))
                   :unless '(sp-in-string-p sp-in-comment-p)
                   :actions '(insert navigate)
                   :suffix "")

    (sp-local-pair "case" "esac"
                   :when '(("SPC" "RET" "<evil-ret>"))
                   :unless '(sp-in-string-p sp-in-comment-p)
                   :actions '(insert navigate)
                   :suffix "")

    (sp-local-pair "if" "fi"
                   :when '(("SPC" "RET" "<evil-ret>"))
                   :unless '(sp-in-string-p sp-in-comment-p)
                   :actions '(insert navigate)
                   :suffix "")))

(use-package fish-mode
  :defer-install t
  :commands (fish_indent-before-save
             fish-mode)
  :mode (("\\.fish\\'"           . fish-mode)
         ("/fish_funced\\..*\\'" . fish-mode))
  :interpreter ("fish" . fish-mode))

;; =============================================================================
;; PowerShell Scripts ==========================================================
;; =============================================================================

(use-package powershell
  :defer-install t
  :commands (powershell-mode
             powershell)
  :mode (("\\.ps[dm]?1\\'" . powershell-mode))
  :config
  (setq powershell-indent 2)
  (use-package smartparens
    :config
    (sp-local-pair 'powershell-mode "`" nil :actions nil)
    (sp-local-pair 'powershell-mode "{" nil :post-handlers
                   '(:add
                     ("||\n[i]" "RET")
                     ("| " "SPC")))))

;; =============================================================================
;; Python ======================================================================
;; =============================================================================

(use-package python
  :ensure nil
  :config
  (add-hook 'python-mode-hook
            (lambda ()
              ;; conflicts with `eldoc-mode'
              (semantic-idle-summary-mode -1)
              (setq mode-name "Py")))

  (use-package company-anaconda
    :defer-install t
    :commands (company-anaconda))

  (use-package anaconda-mode
    :defer-install t
    :commands (anaconda-mode)
    :diminish (anaconda-mode . " ✶")
    :config
    (setq anaconda-mode-installation-directory (locate-user-emacs-file
                                                "data/anaconda-mode"))

    (defun my/anaconda-eldoc-callback-fallback (docstring)
      (setq docstring
            (s-join " " (--map
                         (s-collapse-whitespace
                          (cdr (assoc 'docstring it)))
                         docstring)))
      (eldoc-message
       (substring docstring 0 (min (frame-width) (length docstring)))))

    (defun my/anaconda-eldoc-callback (result)
      (if result
          (eldoc-message (anaconda-mode-eldoc-format result))
        (anaconda-mode-call
         "goto_definitions"
         #'my/anaconda-eldoc-callback-fallback)))

    ;; also show object docstrings
    (defun nadvice/anaconda-mode-eldoc-function ()
      (anaconda-mode-call "eldoc" #'my/anaconda-eldoc-callback)
      nil)

    (advice-add 'anaconda-mode-eldoc-function :override
                #'nadvice/anaconda-mode-eldoc-function))

  (use-package evil
    :config
    (evil-define-key 'normal python-mode-map "gd" #'anaconda-mode-goto))

  (define-key python-mode-map (kbd "M-.") #'anaconda-mode-goto)
  (add-hook 'python-mode-hook #'anaconda-eldoc-mode)
  (add-hook 'python-mode-hook #'anaconda-mode)
  (add-hook 'python-mode-hook #'eldoc-mode)

  (use-package traad
    :defer-install t
    :commands (traad-open
               traad-close
               traad-running?
               traad-display-task-status
               traad-display-full-task-status
               traad-undo
               traad-redo
               traad-display-history
               traad-undo-info
               traad-redo-info
               traad-rename-current-file
               traad-rename
               traad-normalize-arguments
               traad-remove-argument
               traad-extract-method
               traad-extract-variable
               traad-organize-imports
               traad-expand-star-imports
               traad-froms-to-imports
               traad-relatives-to-absolutes
               traad-handle-long-imports
               traad-imports-super-smackdown
               traad-display-occurrences
               traad-display-implementations
               traad-goto-definition
               traad-findit
               traad-code-assist
               traad-display-calltip
               traad-popup-calltip
               traad-display-doc
               traad-popup-doc))

  (use-package live-py-mode
    :defer-install t
    :commands (live-py-mode))

  (use-package py-yapf
    :defer-install t
    :commands (py-yapf-buffer
               py-yapf-enable-on-save)))

(use-package django-html-mode
  :recipe django-mode
  :defer-install t
  :commands (django-html-mode)
  :mode ("\\.djhtml$" . django-html-mode))

(use-package cython-mode
  :defer-install t
  :commands (cython-mode)
  :mode (("\\.pyx\\'" . cython-mode)
         ("\\.pyd\\'" . cython-mode)
         ("\\.pyi\\'" . cython-mode)))

(use-package sage-shell-mode
  :defer-install t
  :commands (run-sage
             run-new-sage
             sage-mode
             sage-shell:run-sage
             sage-shell:run-new-sage
             sage-shell:sage-mode)
  :mode ("\\.sage$" . sage-mode)
  :init
  (progn
    (autoload 'run-sage "sage-shell-mode" nil t)
    (autoload 'run-new-sage "sage-shell-mode" nil t)
    (autoload 'sage-mode "sage-shell-mode" nil t))
  :config
  (setq sage-shell:use-prompt-toolkit t
        sage-shell-view-default-resolution 200)
  (sage-shell:define-alias)

  (use-package evil
    :config
    (evil-set-initial-state 'sage-shell-mode 'insert))

  (add-hook 'sage-shell-mode-hook #'eldoc-mode)
  (add-hook 'sage-mode-hook #'eldoc-mode)

  (add-hook 'sage-shell-mode-hook
            (lambda () (semantic-idle-summary-mode -1)))

  (add-hook 'sage-mode-hook
            (lambda () (semantic-idle-summary-mode -1)))

  (add-hook 'sage-shell-after-prompt-hook #'sage-shell-view)

  (defun nadvice/run-sage (old-fun &optional arg)
    (interactive "P")
    (if (called-interactively-p 'any)
        (cond
         ((consp arg)
          (call-interactively old-fun))
         (t
          (funcall old-fun "sage"))))
    (funcall old-fun arg))
  (advice-add 'run-sage :around #'nadvice/run-sage))

;; =============================================================================
;; Octave/MATLAB ===============================================================
;; =============================================================================

(use-package octave
  :defer-install t
  :commands (octave-mode)
  :mode ("\\.m\\'" . octave-mode)
  :config
  (use-package evil
    :config
    (evil-set-initial-state 'inferior-octave-mode 'insert))

  (use-package smartparens
    :config
    (sp-local-pair 'octave-mode "'" nil :actions nil)))

;; =============================================================================
;; Julia =======================================================================
;; =============================================================================

(use-package julia-mode
  :defer-install t
  :mode (("\\.jl\\'" . julia-mode))
  :commands (julia-mode
             inferior-julia
             run-julia)
  :config
  (use-package evil
    :config
    (evil-set-initial-state 'inferior-julia-mode 'insert))

  (add-hook 'inferior-julia-mode-hook (lambda ()
                                        (auto-indent-mode -1))))

;; =============================================================================
;; Haskell =====================================================================
;; =============================================================================

(use-package haskell
  :recipe haskell-mode
  :defer-install t
  :mode (("\\.hcr\\'" . ghc-core-mode)
         ("\\.dump-simpl\\'" . ghc-core-mode)
         ("\\.ghci\\'" . ghci-script-mode)
         ("\\.cabal\\'" . haskell-cabal-mode)
         ("\\.[gh]s\\'" . haskell-mode)
         ("\\.l[gh]s\\'" . literate-haskell-mode)
         ("\\.hsc\\'" . haskell-mode))
  :commands (ghc-core-create-core
             ghc-core-mode
             ghci-script-mode
             interactive-haskell-mode
             haskell-interactive-mode-return
             haskell-session-kill
             haskell-interactive-kill
             haskell-session
             haskell-interactive-switch
             haskell-session-change
             haskell-kill-session-process
             haskell-interactive-mode-visit-error
             haskell-mode-contextual-space
             haskell-mode-jump-to-tag
             haskell-mode-after-save-handler
             haskell-interactive-bring
             haskell-process-load-file
             haskell-process-reload-file
             haskell-process-load-or-reload
             haskell-process-cabal-build
             haskell-process-cabal
             haskell-process-minimal-imports
             haskell-align-imports
             haskell-cabal-mode
             haskell-cabal-guess-setting
             haskell-cabal-get-dir
             haskell-cabal-visit-file
             haskell-process-restart
             haskell-process-clear
             haskell-process-interrupt
             haskell-process-touch-buffer
             haskell-describe
             haskell-rgrep
             haskell-process-do-info
             haskell-process-do-type
             haskell-mode-jump-to-def-or-tag
             haskell-mode-goto-loc
             haskell-mode-jump-to-def
             haskell-process-cd
             haskell-process-cabal-macros
             haskell-mode-show-type-at
             haskell-process-generate-tags
             haskell-process-unignore
             haskell-session-change-target
             haskell-mode-stylish-buffer
             haskell-mode-find-uses
             haskell-compile
             haskell-ds-create-imenu-index
             turn-on-haskell-decl-scan
             haskell-decl-scan-mode
             haskell-doc-mode
             haskell-doc-current-info
             haskell-doc-show-type
             turn-on-haskell-indent
             haskell-indent-mode
             haskell-indentation-mode
             turn-on-haskell-indentation
             haskell-interactive-mode-reset-error
             haskell-interactive-mode-echo
             haskell-process-show-repl-response
             haskell-process-reload-devel-main
             haskell-menu
             haskell-version
             haskell-mode-view-news
             haskell-mode
             haskell-forward-sexp
             literate-haskell-mode
             haskell-hoogle
             oogle-lookup-from-local
             haskell-hayoo-url
             haskell-session-installed-modules
             haskell-session-all-modules
             haskell-session-project-modules
             haskell-move-nested
             haskell-move-nested-right
             haskell-move-nested-left
             haskell-navigate-imports
             haskell-navigate-imports-go
             haskell-navigate-imports-return
             haskell-session-maybe
             haskell-session-process
             haskell-simple-indent-mode
             turn-on-haskell-simple-indent
             haskell-sort-imports
             turn-on-haskell-unicode-input-method
             highlight-uses-mode
             inferior-haskell-load-file
             inferior-haskell-load-and-run
             inferior-haskell-send-decl
             inferior-haskell-type
             inferior-haskell-kind
             inferior-haskell-info
             inferior-haskell-find-definition
             inferior-haskell-find-haddock
             inf-haskell-mode)
  :interpreter (("runghc" . haskell-mode)
                ("runhaskell" . haskell-mode))
  :init
  (add-to-list 'completion-ignored-extensions ".hi"))

;; =============================================================================
;; Web Development =============================================================
;; =============================================================================

(use-package sgml-mode
  :ensure nil
  :config
  ;; after deleting a tag, indent properly
  (defun nadvice/sgml-delete-tag (&rest _args)
    (indent-region (point-min) (point-max)))

  (advice-add 'sgml-delete-tag :after #'nadvice/sgml-delete-tag))

(use-package css-mode
  :ensure nil
  :config
  (sp-local-pair 'css-mode "{" nil :post-handlers
                 '(:add
                   ("||\n[i]" "RET")
                   ("| " "SPC"))))

(use-package company-web
  :defer-install t
  :commands (company-web-html))

(use-package web-mode
  :defer-install t
  :commands (web-mode)
  :config
  (setq web-mode-auto-close-style 1)
  (add-to-list 'sp-navigate-consider-sgml-tags 'web-mode)
  (sp-local-pair 'web-mode "{" nil :post-handlers
                 '(:add
                   ("||\n[i]" "RET")
                   ("| " "SPC"))))

(use-package less-css-mode
  :defer-install t
  :commands (less-css-mode less-css-compile)
  :mode (("\\.less\\'" . less-css-mode))
  :config
  (sp-local-pair 'less-css-mode "{" nil :post-handlers
                 '(:add
                   ("||\n[i]" "RET")
                   ("| " "SPC"))))

(use-package scss-mode
  :defer-install t
  :commands (scss-mode)
  :mode (("\\.scss\\'" . scss-mode))
  :config
  (use-package smartparens
    :config
    (sp-local-pair 'scss-mode "{" nil :post-handlers
                   '(:add
                     ("||\n[i]" "RET")
                     ("| " "SPC")))))

(use-package sass-mode
  :defer-install t
  :commands (sass-mode)
  :mode (("\\.sass\\'" . sass-mode)))

(use-package coffee-mode
  :defer-install t
  :commands (coffee-mode)
  :mode (("\\.coffee\\'" . coffee-mode)
         ("\\.iced\\'"   . coffee-mode)
         ("Cakefile\\'"  . coffee-mode)
         ("\\.cson\\'"   . coffee-mode))
  :interpreter ("coffee" . coffee-mode))

(use-package literate-coffee-mode
  :defer-install t
  :commands (litcoffee-mode)
  :mode (("\\.litcoffee\\'" . litcoffee-mode)
         ("\\.coffee.md\\'" . litcoffee-mode)))

(use-package livescript-mode
  :defer-install t
  :commands (livescript-mode)
  :mode (("\\.ls\\'"     . livescript-mode)
         ("Slakefile\\'" . livescript-mode)))

(use-package php-mode
  :defer-install t
  :commands (php-mode)
  :mode (("\\.php[s345t]?\\'" . php-mode)
         ("\\.phtml\\'"       . php-mode)
         ("Amkfile"           . php-mode)
         ("\\.amk$"           . php-mode))
  :interpreter ("php" . php-mode))

(use-package dart-mode
  :defer-install t
  :commands (dart-mode)
  :mode ("\\.dart\\'" . dart-mode))

(use-package typescript-mode
  :defer-install t
  :commands (typescript-mode)
  :mode ("\\.ts\\'" . typescript-mode)
  :config
  (with-eval-after-load 'folding
    (when (fboundp 'folding-add-to-marks-list)
      (folding-add-to-marks-list 'typescript-mode "// {{{" "// }}}" )))
  (use-package tide
    :defer-install t
    :commands (tide-setup))

  (add-hook 'typescript-mode-hook #'tide-setup))

(use-package handlebars-mode
  :defer-install t
  :commands (handlebars-mode)
  :mode (("\\.handlebars$" . handlebars-mode)
         ("\\.hbs$"        . handlebars-mode)))

(use-package impatient-mode
  :defer-install t
  :commands (impatient-mode))

;; =============================================================================
;; Dired =======================================================================
;; =============================================================================

(use-package ls-lisp
  :ensure nil
  :config
  (setq ls-lisp-use-insert-directory-program nil
        ls-lisp-support-shell-wildcards t
        ls-lisp-dirs-first t
        ls-lisp-verbosity nil))

(use-package dired
  :ensure nil
  :config
  (require 'ls-lisp)
  (require 'dired-x)

  (setq dired-listing-switches "-alh"
        dired-recursive-copies 'always
        dired-ls-F-marks-symlinks t
        dired-dwim-target t)

  (defun dired-first-file ()
    (interactive)
    (goto-char (point-min))
    (dired-next-line 4))

  (defun dired-last-file ()
    (interactive)
    (goto-char (point-max))
    (dired-next-line -1))

  (defun dired-up-directory ()
    "Take dired up one directory, but behave like dired-find-alternate-file"
    (interactive)
    (let ((old (current-buffer)))
      (dired-up-directory)
      (kill-buffer old)))

  (defun dired-enable-wdired ()
    (interactive)
    (unless (evil-insert-state-p)
      (evil-insert-state))
    (wdired-change-to-wdired-mode))

  (with-eval-after-load 'evil
    (evil-define-key 'normal dired-mode-map "h" #'dired-up-directory)
    (evil-define-key 'normal dired-mode-map "l" #'dired-find-alternate-file)
    (evil-define-key 'normal dired-mode-map "j" #'dired-next-line)
    (evil-define-key 'normal dired-mode-map "k" #'dired-previous-line)

    (evil-define-key 'normal dired-mode-map "I" #'dired-enable-wdired)

    (evil-define-key 'normal dired-mode-map "o" #'dired-sort-toggle-or-edit)
    (evil-define-key 'normal dired-mode-map "m" #'dired-toggle-marks)
    (evil-define-key 'normal dired-mode-map "v" #'dired-mark)
    (evil-define-key 'normal dired-mode-map "V" #'dired-unmark)
    (evil-define-key 'normal dired-mode-map (kbd "C-v") #'dired-unmark-all-marks)
    (evil-define-key 'normal dired-mode-map "u" #'dired-undo)
    (evil-define-key 'normal dired-mode-map "c" #'dired-create-directory)

    (evil-define-key 'normal dired-mode-map "n" #'evil-search-next)
    (evil-define-key 'normal dired-mode-map "N" #'evil-search-previous)
    (evil-define-key 'normal dired-mode-map "q" #'kill-this-buffer)

    (defun my/dired-avy-navigate-down ()
      (interactive)
      (evilem--jump (evilem--collect #'dired-next-line)))

    (defun my/dired-avy-navigate-up ()
      (interactive)
      (evilem--jump (evilem--collect #'dired-previous-line)))

    (defun my/dired-avy-find-file-down ()
      (interactive)
      (my/dired-avy-navigate-down)
      (dired-find-file))

    (defun my/dired-avy-find-file-up ()
      (interactive)
      (my/dired-avy-navigate-up)
      (dired-find-file))

    (evil-define-key 'normal dired-mode-map (kbd "SPC J") #'my/dired-avy-navigate-down)

    (evil-define-key 'normal dired-mode-map (kbd "SPC K") #'my/dired-avy-navigate-up)

    (evil-define-key 'normal dired-mode-map (kbd "SPC j") #'my/dired-avy-find-file-down)

    (evil-define-key 'normal dired-mode-map (kbd "SPC k") #'my/dired-avy-find-file-up))

  (define-key dired-mode-map (kbd "<remap> <beginning-of-buffer>")
    #'dired-first-file)
  (define-key dired-mode-map (kbd "<remap> <end-of-buffer>")
    #'dired-last-file))

(use-package dired-aux
  :ensure nil
  :config
  (add-to-list 'dired-compress-file-suffixes '("\\.zip\\'" ".zip" "unzip")))

;; =============================================================================
;; Comint ======================================================================
;; =============================================================================

(use-package comint
  :ensure nil
  :config
  (setq comint-prompt-read-only t)

  (defun nadvice/comint-previous-matching-input-from-input (old-fun &rest args)
    (condition-case err
        (apply old-fun args)
      (user-error
       (if (string= (cadr err) "Not at command line")
           (cl-destructuring-bind (n &rest ignored) args
             (with-no-warnings
               (if (< n 0)
                   (next-line (- n))
                 (previous-line n))))
         (signal (car err) (cdr err))))))

  (advice-add 'comint-previous-matching-input-from-input
              :around
              #'nadvice/comint-previous-matching-input-from-input)

  (define-key comint-mode-map (kbd "<up>")
    #'comint-previous-matching-input-from-input)
  (define-key comint-mode-map (kbd "<down>")
    #'comint-next-matching-input-from-input))


;; =============================================================================
;; Scheme ======================================================================
;; =============================================================================
(use-package geiser
  :defer-install t
  :commands (geiser-version
             geiser-unload
             geiser-reload
             geiser
             run-geiser
             geiser-connect
             geiser-connect-local
             switch-to-geiser
             run-guile
             switch-to-guile
             connect-to-guile
             run-racket
             switch-to-racket
             connect-to-racket
             run-chicken
             switch-to-chicken
             connect-to-chicken
             geiser-mode
             turn-on-geiser-mode
             turn-off-geiser-mode
             geiser-mode--maybe-activate)
  :mode ("\\.rkt\\'" . scheme-mode)
  :init
  (add-hook 'scheme-mode-hook 'geiser-mode--maybe-activate))

(use-package geiser-debug
  :ensure nil
  :defer t
  :config
  (use-package evil
    :config
    (evil-set-initial-state 'geiser-debug-mode 'insert)))

(use-package geiser-repl
  :ensure nil
  :defer t
  :config
  (use-package evil
    :config
    (evil-set-initial-state 'geiser-repl-mode 'emacs))
  (add-hook 'geiser-repl-mode-hook (lambda ()
                                     (auto-indent-mode -1))))

(use-package hy-mode
  :defer-install t
  :commands (hy-mode)
  :mode ("\\.hy\\'" . hy-mode)
  :interpreter ("hy" . hy-mode))

;; =============================================================================
;; Shell modes =================================================================
;; =============================================================================
(eval-when-compile
  (with-demoted-errors "Load error: %s"
    (require 'hl-line)))

(defun my/generic-term-init ()
  ;; this disables key-chord-mode
  (set (make-local-variable 'input-method-function) nil)
  (adaptive-wrap-prefix-mode -1)
  (visual-line-mode -1)
  (yas-minor-mode -1)
  (setq yas-dont-activate t)

  (setq-local global-hl-line-mode nil)
  (setq-local scroll-margin 0)
  (setq-local smooth-scroll-margin 0))

(add-hook 'term-mode-hook #'my/generic-term-init)
(add-hook 'shell-mode-hook #'my/generic-term-init)
(add-hook 'eshell-mode-hook #'my/generic-term-init)

(use-package term
  :ensure nil
  :config
  (require 'with-editor)
  (add-hook 'term-exec-hook 'with-editor-export-editor)

  (defun nadvice/term-sentinel (old-fun &rest args)
    (cl-destructuring-bind (proc _msg) args
      (if (memq (process-status proc) '(signal exit))
          (let ((buffer (process-buffer proc)))
            (apply old-fun args)
            (kill-buffer buffer)
            (winner-undo)
            (message ""))
        (apply old-fun args))))
  (advice-add 'term-sentinel :around #'nadvice/term-sentinel)

  (define-key term-raw-map (kbd "<f12>") #'term-kill-subjob)
  (define-key term-raw-map (kbd "<remap> <cua-paste>") #'term-paste)

  (defun nadvice/term-exec-1 (name buffer command switches)
    (let* ((environment
            (list
             (format "TERM=%s" term-term-name)
             (format "TERMINFO=%s" data-directory)
             (format term-termcap-format "TERMCAP="
                     term-term-name term-height term-width)
             (format "EMACS=%s (term:%s)" emacs-version term-protocol-version)
             (format "INSIDE_EMACS=%s,term:%s" emacs-version term-protocol-version)
             (format "LINES=%d" term-height)
             (format "COLUMNS=%d" term-width)))
           (process-environment
            (append environment
                    process-environment))
           (tramp-remote-process-environment
            (append environment
                    tramp-remote-process-environment))
           (process-connection-type t)
           (coding-system-for-read 'binary))
      (apply 'start-file-process name buffer
             "/bin/sh" "-c"
             (format "stty -nl echo rows %d columns %d sane 2>/dev/null;\
    if [ $1 = .. ]; then shift; fi; exec \"$@\""
                     term-height term-width)
             ".."
             command switches)))

  (advice-add 'term-exec-1 :override #'nadvice/term-exec-1)

  (defun nadvice/ansi-term (&optional args)
    (interactive "P")
    (cl-destructuring-bind (&optional program new-buffer-name) args
      (let ((default-shell
              (cl-some
               (if (tramp-tramp-file-p default-directory)
                   (lambda (shell)
                     (when shell
                       (with-parsed-tramp-file-name
                           buffer-file-name vec
                         (substring-no-properties
                          (tramp-find-executable
                           vec
                           (file-name-base shell)
                           (tramp-get-remote-path vec)
                           t t)))))
                 (lambda (shell)
                   (when (and shell
                              (file-exists-p shell))
                     shell)))
               (append
                (list (bound-and-true-p explicit-shell-file-name)
                      (getenv "ESHELL")
                      (getenv "SHELL"))
                (when (tramp-tramp-file-p default-directory)
                  (with-parsed-tramp-file-name
                      buffer-file-name vec
                    (or (tramp-find-executable
                         vec "bash" (tramp-get-remote-path vec) t t)
                        (tramp-find-executable
                         vec "ksh" (tramp-get-remote-path vec) t t)
                        (tramp-get-connection-property
                         (tramp-get-connection-process vec) "remote-shell" nil)
                        (tramp-get-method-parameter
                         (tramp-file-name-method vec) 'tramp-remote-shell))))
                (list "/bin/sh")))))
        (if (stringp program)
            (list program new-buffer-name)
          (if (consp program)
              (list (read-from-minibuffer "Run program: "
                                          default-shell)
                    new-buffer-name)
            (list default-shell new-buffer-name))))))

  (advice-add 'ansi-term :filter-args #'nadvice/ansi-term))

(defun eshell-kill-whole-line ()
  (interactive)
  (eshell-bol)
  (kill-line))

(defun my/eshell-onetime-setup ()
  (when (featurep 'evil)
    (evil-define-key 'insert eshell-mode-map (kbd "<tab>") #'company-complete)
    (evil-define-key 'insert eshell-mode-map (kbd "C-a") #'eshell-bol)
    (evil-define-key 'insert eshell-mode-map (kbd "<home>") #'eshell-bol)
    (evil-define-key 'insert eshell-mode-map (kbd "<C-S-backspace>") #'eshell-kill-whole-line)
    (evil-define-key 'insert eshell-mode-map (kbd "C-r") #'eshell-isearch-backward))

  (remove-hook 'eshell-mode-hook #'my/eshell-onetime-setup))

(use-package eshell
  :ensure nil
  :config
  (eval-when-compile
    (with-demoted-errors "Load error: %s"
      (require 'em-smart)
      (require 'em-unix)
      (require 'em-cmpl)
      (require 'company)))

  (add-hook 'eshell-mode-hook
            (lambda ()
              (setq-local company-idle-delay 0.1)
              (my/eshell-onetime-setup)))

  (add-hook 'eshell-directory-change-hook
            (lambda ()
              (setq company-idle-delay
                    (if (file-remote-p default-directory)
                        nil
                      0.1))))

  (setq eshell-cmpl-dir-ignore (rx line-start
                                   (or "." ".." "CVS" ".svn" ".git")
                                   line-end)
        eshell-cmpl-file-ignore (rx (or ".elc" ".zwc" ".pyc" "~" ".swp")
                                    line-end)
        eshell-cmpl-ignore-case t

        eshell-scroll-to-bottom-on-input t
        eshell-scroll-show-maximum-output nil
        eshell-cp-interactive-query t
        eshell-ln-interactive-query t
        eshell-mv-interactive-query t
        eshell-rm-interactive-query t
        eshell-mv-overwrite-files nil))

(defun eshell/clear ()
  (interactive)
  (let ((inhibit-read-only t)) (erase-buffer)))

(defun eshell/emacs (&rest args)
  "Invoke `find-file' on the file.
\"emacs +42 foo\" also goes to line 42 in the buffer."
  (while args
    (if (string-match (rx line-start "+" (group (one-or-more digit)) line-end)
                      (car args))
        (let* ((line (string-to-number (match-string 1 (pop args))))
               (file (pop args)))
          (find-file file)
          (forward-line line))
      (find-file (pop args)))))

(defun my/popup-ansi-term ()
  "Toggle a shell popup buffer with the current file's directory as cwd."
  (interactive)
  (require 's)
  (require 'tramp)
  (let* ((dir (if (derived-mode-p 'dired-mode)
                  (dired-current-directory)
                (file-name-directory (or buffer-file-name "~/"))))
         (shell (or explicit-shell-file-name
                    (when (and buffer-file-name
                               (file-remote-p buffer-file-name))
                      (with-parsed-tramp-file-name buffer-file-name parsed
                        (let* ((login-shell
                                (s-trim
                                 (substring-no-properties
                                  (shell-command-to-string
                                   (format
                                    "getent passwd \"%s\" | cut -f7 -d:"
                                    (shell-quote-argument parsed-user))))))
                               (login-shell-fullpath (tramp-make-tramp-file-name
                                                      parsed-method
                                                      parsed-user
                                                      parsed-host
                                                      login-shell)))
                          (when (file-exists-p login-shell-fullpath)
                            login-shell-fullpath))))
                    (getenv "ESHELL")
                    (getenv "SHELL")
                    "/bin/sh"))
         (popup-buffer (get-buffer "**Popup Shell**"))
         (new-buffer (unless (buffer-live-p popup-buffer)
                       (save-window-excursion
                         (ansi-term shell "*Popup Shell*")
                         (setq popup-buffer (get-buffer "**Popup Shell**")))
                       t)))
    (with-current-buffer popup-buffer
      (comint-send-string nil (concat "cd "
                                      (shell-quote-argument dir)
                                      ";clear\n")))
    (select-window (split-window-sensibly))
    (switch-to-buffer popup-buffer)))

(global-set-key (kbd "<f12>") #'my/popup-ansi-term)

;; =============================================================================
;; Config file modes ===========================================================
;; =============================================================================

(use-package systemd
  :defer-install t
  :commands (systemd-mode)
  :mode (((rx (+? (any "a-zA-Z0-9-_.@\\"))
              "."
              (or "automount"
                  "busname"
                  "mount"
                  "service"
                  "slice"
                  "socket"
                  "swap"
                  "target"
                  "timer"
                  "link"
                  "netdev"
                  "network")
              string-end)
          . systemd-mode)
         ((rx ".#"
              (or (and (+? (any "a-zA-Z0-9-_.@\\"))
                       "."
                       (or "automount"
                           "busname"
                           "mount"
                           "service"
                           "slice"
                           "socket"
                           "swap"
                           "target"
                           "timer"
                           "link"
                           "netdev"
                           "network"))
                  "override.conf")
              (= 16 (char hex-digit))
              string-end)
          . systemd-mode)
         ((rx "/systemd/"
              (+? anything)
              ".d/"
              (+? (not (any ?/)))
              ".conf"
              string-end)
          . systemd-mode)))

(use-package gitattributes-mode
  :defer-install t
  :commands (gitattributes-mode)
  :mode (("/\\.gitattributes\\'"       . gitattributes-mode)
         ("/\\.git/info/attributes\\'" . gitattributes-mode)
         ("/git/attributes\\'"         . gitattributes-mode)))

(use-package gitconfig-mode
  :defer-install t
  :commands (gitconfig-mode)
  :mode (("/\\.gitconfig\\'"  . gitconfig-mode)
         ("/\\.git/config\\'" . gitconfig-mode)
         ("/git/config\\'"    . gitconfig-mode)
         ("/\\.gitmodules\\'" . gitconfig-mode)))

(use-package gitignore-mode
  :defer-install t
  :commands (gitignore-mode)
  :mode (("/\\.gitignore\\'"        . gitignore-mode)
         ("/\\.git/info/exclude\\'" . gitignore-mode)
         ("/git/ignore\\'"          . gitignore-mode)))

(use-package ssh-config-mode
  :defer-install t
  :commands (ssh-config-mode
             ssh-authorized-keys-mode)
  :mode ((".ssh/config\\'"       . ssh-config-mode)
         ("sshd?_config\\'"      . ssh-config-mode)
         ("known_hosts\\'"       . ssh-known-hosts-mode)
         ("authorized_keys2?\\'" . ssh-authorized-keys-mode)))

(use-package pkgbuild-mode
  :defer-install t
  :commands (pkgbuild-mode)
  :mode (("/PKGBUILD\\'" . pkgbuild-mode)))

(use-package dockerfile-mode
  :defer-install t
  :commands (dockerfile-build-buffer
             dockerfile-build-no-cache-buffer
             dockerfile-mode)
  :mode (("Dockerfile.*\\'" . dockerfile-mode)))

(use-package cmake-mode
  :defer-install t
  :commands (cmake-mode
             cmake-command-run
             cmake-help-list-commands
             cmake-help-command
             cmake-help-module
             cmake-help-variable
             cmake-help-property
             cmake-help)
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'"         . cmake-mode)))

;; Qt qmake project files
(add-to-list 'auto-mode-alist '("\\.pro\\'" . makefile-mode))

(use-package hgignore-mode
  :defer-install t
  :commands (hgignore-mode)
  :mode (("\\.hgignore\\'" . hgignore-mode)))

(use-package nginx-mode
  :defer-install t
  :commands (nginx-mode)
  :mode (("nginx\\.conf\\'"     . nginx-mode)
         ("/nginx/.+\\.conf\\'" . nginx-mode)))

;; =============================================================================
;; Markup modes ================================================================
;; =============================================================================

(use-package yaml-mode
  :defer-install t
  :commands (yaml-mode)
  :mode (("\\.e?ya?ml$" . yaml-mode)))

(use-package haml-mode
  :defer-install t
  :commands (haml-mode)
  :mode (("\\.haml\\'" . haml-mode)))

(use-package markdown-mode
  :defer-install t
  :commands (markdown-mode gfm-mode)
  :mode (("\\.text\\'" . markdown-mode)
         ("\\.md\\'"   . markdown-mode))

  :config
  (when (executable-find "marked")
    (setq markdown-command "marked")))

(use-package markdown-preview-mode
  :defer-install t
  :commands (markdown-preview-open-browser
             markdown-preview-cleanup
             markdown-preview-mode))

(use-package bbcode-mode
  :defer-install t
  :commands (bbcode-mode)
  :mode (("\\.bbcode$" . bbcode-mode)))

;; =============================================================================
;; TeX/LaTeX ===================================================================
;; =============================================================================

(use-package company-math
  :defer-install t
  :commands (company-latex-commands
             company-math-symbols-latex
             company-math-symbols-unicode))

(use-package tex
  :recipe auctex
  :commands (bib-cite-minor-mode
             turn-on-bib-cite
             ConTeXt-mode
             context-mode
             context-en-mode
             context-nl-mode
             font-latex-setup
             BibTeX-auto-store
             TeX-latex-mode
             docTeX-mode
             TeX-doctex-mode
             multi-prompt-key-value
             TeX-plain-tex-mode
             ams-tex-mode
             preview-install-styles
             LaTeX-preview-setup
             preview-report-bug
             TeX-assoc-string
             TeX-tex-mode
             TeX-auto-generate
             TeX-auto-generate-global
             TeX-submit-bug-report
             TeX-install-toolbar
             LaTeX-install-toolbar
             TeX-fold-mode
             tex-fold-mode
             tex-font-setup
             Texinfo-mode
             TeX-texinfo-mode
             japanese-plain-tex-mode
             japanese-latex-mode
             texmathp
             texmathp-match-switch
             toolbarx-install-toolbar)

  :mode (("\\.drv\\'" . latex-mode)
         ("\\.hva\\'" . latex-mode)
         ("\\.dtx\\'" . doctex-mode))

  :init
  (el-patch-feature tex auctex)

  (advice-add 'tex-mode :override #'TeX-tex-mode)
  (advice-add 'plain-tex-mode :override #'TeX-plain-tex-mode)
  (advice-add 'texinfo-mode :override #'TeX-texinfo-mode)
  (advice-add 'latex-mode :override #'TeX-latex-mode)
  (advice-add 'doctex-mode :override #'TeX-doctex-mode)

  :config
  (setq TeX-auto-save t
        TeX-save-query nil
        TeX-parse-self t
        TeX-PDF-mode t
        TeX-source-correlate-start-server t
        LaTeX-math-list '(("\'" "dif")
                          ("\"" "Dif")
                          ("$" "int")
                          ("=" "implies")
                          ("!" "neq"))
        TeX-auto-global (locate-user-emacs-file "data/auctex"))

  (add-to-list 'safe-local-variable-values
               '(TeX-command-extra-options . "-shell-escape"))

  (with-eval-after-load 'texmathp
    (add-to-list 'texmathp-tex-commands-default '("tableau" env-on))
    (texmathp-compile))

  (require 'smartparens-latex)

  (el-patch-defun TeX-brace-count-line ()
    "Count number of open/closed braces."
    (save-excursion
      (let ((count 0) (limit (line-end-position)) char)
        (while (progn
                 (skip-chars-forward (el-patch-swap "^{}\\\\"
                                                    "^{}[]\\\\")
                                     limit)
                 (when (and (< (point) limit) (not (TeX-in-comment)))
                   (setq char (char-after))
                   (forward-char)
                   (cond ((eq char ?\{)
                          (setq count (+ count TeX-brace-indent-level)))
                         ((eq char ?\})
                          (setq count (- count TeX-brace-indent-level)))
                         (el-patch-add
                           ((eq char ?\[)
                            (setq count (+ count TeX-brace-indent-level)))
                           ((eq char ?\])
                            (setq count (- count TeX-brace-indent-level))))
                         ((eq char ?\\)
                          (when (< (point) limit)
                            (forward-char)
                            t))))))
        count)))

  (use-package magic-latex-buffer
    :defer-install t
    :commands (magic-latex-buffer)
    :config
    (setq magic-latex-enable-block-align nil
          magic-latex-enable-inline-image nil)

    (set-face-attribute 'ml/llarge nil :height 1.25)
    (set-face-attribute 'ml/xlarge nil :height 1.3)
    (set-face-attribute 'ml/huge nil :height 1.35)
    (set-face-attribute 'ml/hhuge nil :height 1.4))

  (use-package company-auctex
    :recipe (company-auctex :type git
                            :host github
                            :repo "PythonNut/company-auctex")
    :commands (company-auctex-symbols
               company-auctex-environments))

  (add-hook 'TeX-mode-hook
            (lambda ()
              (let ((old-backends company-backends))
                (set (make-local-variable 'company-backends)
                     (append (list (append
                                    '(company-auctex-macros
                                      company-auctex-symbols
                                      company-auctex-environments
                                      company-latex-commands
                                      company-math-symbols-latex
                                      company-dabbrev)
                                    (cdar old-backends)))
                             '((company-ispell))
                             (cdr old-backends))))))

  (use-package evil
    :config
    (use-package evil-latex-textobjects
      :recipe (evil-latex-textobjects :type git
                                      :host github
                                      :repo "hpdeifel/evil-latex-textobjects"
                                      :files ("evil-latex-textobjects.el"))
      :commands (evil-latex-textobjects-mode
                 turn-on-evil-latex-textobjects-mode
                 turn-off-evil-latex-textobjects-mode)

      :config
      (defun my/evil-TeX--expression-range (&optional count exclusive)
        (cond
         ((string= (LaTeX-current-environment) "minted")
          (list (cdr (evil-latex-textobjects-env-beginning))
                (car (evil-latex-textobjects-env-end))))
         (t
          (let* ((terminators (rx (or "&"
                                      "="
                                      "\\\\"
                                      "\\item"
                                      "%"
                                      "\\label"
                                      "\\end"
                                      "\\]"
                                      "\\["
                                      "$"
                                      (and "\\begin{"
                                           (zero-or-more
                                            (or letter "*"))
                                           "}"
                                           (zero-or-more
                                            (or
                                             (and "["
                                                  (zero-or-more char)
                                                  "]")
                                             (and "{"
                                                  (zero-or-more char)
                                                  "}")))))))
                 (beg (save-excursion
                        (while (not (or
                                     (bobp)
                                     (looking-back (if exclusive
                                                       (concat (format "\\(%s\\)" terminators)
                                                               (rx (zero-or-more (or space "\n"))))
                                                     terminators)
                                                   (max (point-min)
                                                        (- (point) 80)))))
                          (forward-char -1))
                        (point)))
                 (end (save-excursion
                        (while (not (or
                                     (eobp)
                                     (looking-at (if exclusive
                                                     (concat (rx (zero-or-more (or space "\n")))
                                                             (format "\\(%s\\)" terminators))
                                                   terminators))))
                          (forward-char 1))
                        (point))))
            (list beg end)))))

      (evil-define-text-object evil-indent-i-TeX-expression (&optional count _beg _end _type)
        "Text object describing the block with the same indentation as the current line."
        (cl-destructuring-bind (begin end)
            (my/evil-TeX--expression-range count t)
          (evil-range begin end 'char)))

      (evil-define-text-object evil-indent-a-TeX-expression (&optional count _beg _end _type)
        "Text object describing the block with the same indentation as the current line."
        (cl-destructuring-bind (begin end)
            (my/evil-TeX--expression-range count nil)
          (evil-range begin end 'char)))

      (define-key evil-latex-textobjects-inner-map "=" #'evil-indent-i-TeX-expression)
      (define-key evil-latex-textobjects-outer-map "=" #'evil-indent-a-TeX-expression))

    (defun my/evil-LaTeX-setup ()
      (require 'evil-surround)
      (evil-latex-textobjects-mode +1)
      (setq evil-surround-pairs-alist
            (append (list '(?\( . ("\\left( " . " \\right)"))
                          '(?\[ . ("\\left[ " . " \\right]"))
                          '(?\{ . ("\\left{ " . " \\right}")))
                    evil-surround-pairs-alist)))

    (add-hook 'LaTeX-mode-hook #'my/evil-LaTeX-setup)
    (evil-set-initial-state 'TeX-error-overview-mode 'insert))

  (add-to-list 'sp-sexp-suffix (list 'latex-mode 'regexp ""))
  (add-hook 'LaTeX-mode-hook (lambda ()
                               (adaptive-wrap-prefix-mode -1)
                               (when (display-graphic-p)
                                 (magic-latex-buffer))))
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
  (add-to-list 'TeX-output-view-style '("^pdf$" "." "evince --page-index=%(outpage) %o"))

  (defun nadvice/TeX-command-master (old-fun arg)
    (interactive "P")
    (if (called-interactively-p 'any)
        (if (consp arg)
            (call-interactively old-fun)
          (cl-letf* (((symbol-function #'TeX-command-query)
                      (lambda (name)
                        (TeX-command-default name)
                        (car-safe (TeX-assoc "LaTeX" TeX-command-list)))))
            (call-interactively old-fun)))
      (apply old-fun args)))

  (advice-add 'TeX-command-master :around #'nadvice/TeX-command-master)

  (defun nadvice/TeX-source-correlate-sync-source (&rest args)
    (recenter)
    (require 'pulse)
    (pulse-momentary-highlight-one-line (point)))

  (advice-add 'TeX-source-correlate-sync-source :after
              #'nadvice/TeX-source-correlate-sync-source)

  (defun nadvice/LaTeX-math-insert (old-fun string dollar)
    (let ((TeX-insert-braces nil))
      (if (texmathp)
          (funcall old-fun string dollar)
        (funcall old-fun string (not dollar)))))

  (advice-add 'LaTeX-math-insert :around #'nadvice/LaTeX-math-insert)

  (defun my/embrace-with-TeX-environment ()
    (let* ((input (read-string "Environment: "))
           (newline (if (= (elt input (1- (length input))) ?\n) "\n" ""))
           (environment (if (or (string-empty-p newline)
                                (string-empty-p input))
                            input
                          (substring input 0 (1- (length input))))))
      (cons (format "\\begin{%s}%s" (or environment "") newline)
            (format "%s\\end{%s}" newline (or environment "")))))

  (defun my/embrace-TeX-setup ()
    (require 'embrace)
    (embrace-add-pair ?= "\\verb|" "|")
    (embrace-add-pair ?~ "\\texttt{" "}")
    (embrace-add-pair ?/ "\\emph{" "}")
    (embrace-add-pair ?* "\\textbf{" "}")
    (embrace-add-pair ?$ "$" "$")
    (embrace-add-pair ?\\ "\\[" "\\]")
    (embrace-add-pair-regexp ?e "\\\\begin{[^\}]*?}" "\\\\end{[^\}]*?}"
                             'my/embrace-with-TeX-environment
                             (embrace-build-help "\\begin{env}" "\\end{env}")))

  (add-hook 'LaTeX-mode-hook #'my/embrace-TeX-setup)

  (defun my/auto-yasnippet-TeX-macro ()
    "Convert the TeX macro around point into a YASnippet snippet"
    (let ((beg (TeX-find-macro-start))
          (end (TeX-find-macro-end)))
      (when (and beg
                 end
                 (looking-at "}"))
        (yas-expand-snippet (replace-regexp-in-string "{}" "{${}}" (substring-no-properties (buffer-substring beg end)))
                            beg
                            end))))

  (add-hook 'TeX-after-insert-macro-hook #'my/auto-yasnippet-TeX-macro)

  (defvar my/TeX-environment-or-macro-default "align*")
  (defun LaTeX-environment-or-macro (arg)
    "TeX-insert-macro and LaTeX-environment merged into one command"
    (interactive "*P")
    (let* ((symbol-list (TeX-symbol-list-filtered))
           (thing (completing-read
                   (concat "Thing: (default "
                           my/TeX-environment-or-macro-default
                           ") ")
                   (append (LaTeX-environment-list-filtered)
                           symbol-list)
                   nil nil nil
                   'LaTeX-environment-and-macro-history
                   my/TeX-environment-or-macro-default)))

      (setq my/TeX-environment-or-macro-default thing)
      (if (not (assoc thing symbol-list))
          (let ((entry (assoc thing (LaTeX-environment-list))))
            (when (interactive-p)
              (setq LaTeX-default-environment thing))
            (if (null entry) (LaTeX-add-environments (list thing)))
            (if arg
                (LaTeX-modify-environment thing)
              (LaTeX-environment-menu thing)))
        (when (interactive-p)
          (setq TeX-default-macro thing))
        (TeX-parse-macro thing (cdr-safe (assoc thing (TeX-symbol-list))))
        (run-hooks 'TeX-after-insert-macro-hook))))

  (defun LaTeX-environment-or-macro-or-self-insert (arg)
    "TeX-insert-macro and LaTeX-environment merged into one command"
    (interactive "*P")
    (if (string-match-p (rx bol
                            (or "tikzpicture"
                                "circuitikz"
                                "lstlisting"
                                "minted"
                                "verbatim")
                            eol)
                        (LaTeX-current-environment))
        (self-insert-command arg)
      (LaTeX-environment-or-macro arg)))

  (define-key TeX-mode-map (kbd ";") #'LaTeX-environment-or-macro-or-self-insert))

(with-eval-after-load 'latex
  (el-patch-defun LaTeX-indent-calculate (&optional force-type)
    "Return the indentation of a line of LaTeX source.
FORCE-TYPE can be used to force the calculation of an inner or
outer indentation in case of a commented line.  The symbols
'inner and 'outer are recognized."
    (save-excursion
      (LaTeX-back-to-indentation force-type)
      (let ((i 0)
            (list-length (safe-length docTeX-indent-inner-fixed))
            (case-fold-search nil)
            entry
            found)
        (cond ((save-excursion (beginning-of-line) (bobp)) 0)
              ((and (eq major-mode 'doctex-mode)
                    fill-prefix
                    (TeX-in-line-comment)
                    (progn
                      (while (and (< i list-length)
                                  (not found))
                        (setq entry (nth i docTeX-indent-inner-fixed))
                        (when (looking-at (nth 0 entry))
                          (setq found t))
                        (setq i (1+ i)))
                      found))
               (if (nth 2 entry)
                   (- (nth 1 entry) (if (integerp comment-padding)
                                        comment-padding
                                      (length comment-padding)))
                 (nth 1 entry)))
              ((looking-at (concat (regexp-quote TeX-esc)
                                   "\\(begin\\|end\\){\\("
                                   (LaTeX-verbatim-regexp)
                                   "\\)}"))
               ;; \end{verbatim} must be flush left, otherwise an unwanted
               ;; empty line appears in LaTeX's output.
               0)
              ((and LaTeX-indent-environment-check
                    ;; Special environments.
                    (let ((entry (assoc (or LaTeX-current-environment
                                            (LaTeX-current-environment))
                                        LaTeX-indent-environment-list)))
                      (and entry
                           (nth 1 entry)
                           (funcall (nth 1 entry))))))
              ((looking-at (concat (regexp-quote TeX-esc)
                                   "\\("
                                   LaTeX-end-regexp
                                   "\\)"))
               ;; Backindent at \end.
               (- (LaTeX-indent-calculate-last force-type) LaTeX-indent-level))
              ((looking-at (concat (regexp-quote TeX-esc) "right\\b"))
               ;; Backindent at \right.
               (- (LaTeX-indent-calculate-last force-type)
                  LaTeX-left-right-indent-level))
              ((looking-at (concat (regexp-quote TeX-esc)
                                   "\\("
                                   LaTeX-item-regexp
                                   "\\)"))
               ;; Items.
               (+ (LaTeX-indent-calculate-last force-type) LaTeX-item-indent))
              ((looking-at (el-patch-swap "}"
                                          (rx (or "}" "]"))))
               ;; End brace in the start of the line.
               (- (LaTeX-indent-calculate-last force-type)
                  TeX-brace-indent-level))
              (t (LaTeX-indent-calculate-last force-type))))))

  (el-patch-defun LaTeX-indent-calculate-last (&optional force-type)
    "Return the correct indentation of a normal line of text.
The point is supposed to be at the beginning of the current line.
FORCE-TYPE can be used to force the calculation of an inner or
outer indentation in case of a commented line.  The symbols
'inner and 'outer are recognized."
    (let (line-comment-current-flag
          line-comment-last-flag
          comment-current-flag
          comment-last-flag)
      (beginning-of-line)
      (setq line-comment-current-flag (TeX-in-line-comment)
            comment-current-flag (TeX-in-commented-line))
      (if comment-current-flag
          (skip-chars-backward "%\n\t ")
        (skip-chars-backward "\n\t "))
      (beginning-of-line)
      ;; If we are called in a non-comment line, skip over comment
      ;; lines.  The computation of indentation should in this case
      ;; rather take the last non-comment line into account.
      ;; Otherwise there might arise problems with e.g. multi-line
      ;; code comments.  This behavior is not enabled in docTeX mode
      ;; where large amounts of line comments may have to be skipped
      ;; and indentation should not be influenced by unrelated code in
      ;; other macrocode environments.
      (while (and (not (eq major-mode 'doctex-mode))
                  (not comment-current-flag)
                  (TeX-in-commented-line)
                  (not (bobp)))
        (skip-chars-backward "\n\t ")
        (beginning-of-line))
      (setq line-comment-last-flag (TeX-in-line-comment)
            comment-last-flag (TeX-in-commented-line))
      (LaTeX-back-to-indentation force-type)
      ;; Separate line comments and other stuff (normal text/code and
      ;; code comments).  Additionally we don't want to compute inner
      ;; indentation when a commented and a non-commented line are
      ;; compared.
      (cond ((or (and (eq major-mode 'doctex-mode)
                      (or (and line-comment-current-flag
                               (not line-comment-last-flag))
                          (and (not line-comment-current-flag)
                               line-comment-last-flag)))
                 (and force-type
                      (eq force-type 'inner)
                      (or (and comment-current-flag
                               (not comment-last-flag))
                          (and (not comment-current-flag)
                               comment-last-flag))))
             0)
            ((looking-at (concat (regexp-quote TeX-esc)
                                 "begin *{\\("
                                 LaTeX-document-regexp
                                 "\\)}"))
             ;; I dislike having all of the document indented...
             (+ (LaTeX-current-indentation force-type)
                ;; Some people have opening braces at the end of the
                ;; line, e.g. in case of `\begin{letter}{%'.
                (TeX-brace-count-line)))
            ((and (eq major-mode 'doctex-mode)
                  (looking-at (concat (regexp-quote TeX-esc)
                                      "end[ \t]*{macrocode\\*?}"))
                  fill-prefix
                  (TeX-in-line-comment))
             ;; Reset indentation to zero after a macrocode
             ;; environment.
             0)
            ((looking-at (concat (regexp-quote TeX-esc)
                                 "begin *{\\("
                                 (LaTeX-verbatim-regexp)
                                 "\\)}"))
             0)
            ((looking-at (concat (regexp-quote TeX-esc)
                                 "end *{\\("
                                 (LaTeX-verbatim-regexp)
                                 "\\)}"))
             ;; If I see an \end{verbatim} in the previous line I skip
             ;; back to the preceding \begin{verbatim}.
             (save-excursion
               (if (re-search-backward (concat (regexp-quote TeX-esc)
                                               "begin *{\\("
                                               (LaTeX-verbatim-regexp)
                                               "\\)}") 0 t)
                   (LaTeX-indent-calculate-last force-type)
                 0)))
            (t (+ (LaTeX-current-indentation force-type)
                  (if (not (and force-type
                                (eq force-type 'outer)
                                (TeX-in-commented-line)))
                      (+ (LaTeX-indent-level-count)
                         (TeX-brace-count-line))
                    0)
                  (cond ((looking-at (concat (regexp-quote TeX-esc)
                                             "\\("
                                             LaTeX-end-regexp
                                             "\\)"))
                         LaTeX-indent-level)
                        ((looking-at
                          (concat (regexp-quote TeX-esc) "right\\b"))
                         LaTeX-left-right-indent-level)
                        ((looking-at (concat (regexp-quote TeX-esc)
                                             "\\("
                                             LaTeX-item-regexp
                                             "\\)"))
                         (- LaTeX-item-indent))
                        ((looking-at (el-patch-swap "}"
                                                    (rx (or "}" "]"))))
                         TeX-brace-indent-level)
                        (t 0))))))))

;; =============================================================================
;; Org mode ====================================================================
;; =============================================================================

(use-package ob-core
  :ensure nil
  :config
  (setq org-confirm-babel-evaluate nil)
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images)

  ;; Load languages when needed
  (defun nadvice/org-babel-execute-src-block (old-fun &rest args)
    (let ((language (org-element-property :language (org-element-at-point))))
      (unless (cdr (assoc (intern language) org-babel-load-languages))
        (add-to-list 'org-babel-load-languages (cons (intern language) t))
        (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))
      (apply old-fun args)))

  (advice-add 'org-babel-execute-src-block :around
              #'nadvice/org-babel-execute-src-block))

(use-package org
  :config
  (setq org-src-fontify-natively t
        org-startup-with-inline-images t)

  (defvar ob-language-file-alist
    (list '(ob-sage . ob-sagemath))
    "An alist that resolves discrepancies between language names and file names in org-babel")

  (defvar ob-deferred-install-languages (list 'ob-axiom
                                              'ob-browser
                                              'ob-coffee
                                              'ob-cypher
                                              'ob-diagrams
                                              'ob-elixir
                                              'ob-go
                                              'ob-http
                                              'ob-hy
                                              'ob-ipython
                                              'ob-kotlin
                                              'ob-lfe
                                              'ob-lua
                                              'ob-mongo
                                              'ob-ml-marklogic
                                              'ob-php
                                              'ob-prolog
                                              'ob-redis
                                              'ob-restclient
                                              'ob-sagemath
                                              'ob-scala
                                              'ob-sly
                                              'ob-sml
                                              'ob-swift
                                              'ob-translate
                                              'ob-typescript)
    "A list of org-babel backends that can be installed with package.el")

  (defun nadvice/org-babel-do-load-languages (old-fun &rest args)
    (cl-letf* ((old-require (symbol-function #'require))
               ((symbol-function #'require)
                (lambda (symbol &rest iargs)
                  (let ((symbol
                         (cdr (or (assoc symbol ob-language-file-alist)
                                  (cons symbol symbol)))))
                    (when (and (not (funcall old-require
                                             symbol
                                             (car-safe iargs)
                                             t))
                               (member symbol
                                       ob-deferred-install-languages))
                      (package-install symbol)
                      (apply old-require symbol iargs))))))
      (apply old-fun args)))

  (advice-add 'org-babel-do-load-languages :around
              #'nadvice/org-babel-do-load-languages))

;; =============================================================================
;; R ===========================================================================
;; =============================================================================

(use-package ess
  :defer-install t
  :commands (R R-mode S)
  :mode (("\\.R$" . R-mode)))

;; =============================================================================
;; Polymodes ===================================================================
;; =============================================================================

(use-package polymode
  :defer-install t
  :mode (("\\.Snw$" . poly-noweb+r-mode)
         ("\\.Rnw$" . poly-noweb+r-mode)
         ("\\.Rmd$" . poly-markdown+r-mode)
         ("\\.rapport$" . poly-rapport-mode)
         ("\\.Rhtml$" . poly-html+r-mode)
         ("\\.Rbrew$" . poly-brew+r-mode)
         ("\\.Rcpp$" . poly-r+c++-mode)
         ("\\.cppR$" . poly-c++r-mode)
         ("\\.js.erb$" . poly-javascript+erb-mode)
         ("\\.coffee.erb$" . poly-coffee+erb-mode)
         ("\\.html.erb$" . poly-html+erb-mode)
         ("\\.slim$" . poly-slim-mode))

    :commands (poly-markdown-mode
                      poly-noweb+r-mode
                      poly-noweb+r-mode
                      poly-markdown+r-mode
                      poly-rapport-mode
                      poly-html+r-mode
                      poly-brew+r-mode
                      poly-r+c++-mode
                      poly-c++r-mode
                      poly-javascript+erb-mode
                      poly-coffee+erb-mode
                      poly-html+erb-mode
                      poly-slim-mode)
    :config
    (require 'poly-R)
    (require 'poly-erb)
    (require 'poly-markdown)
    (require 'poly-noweb)
    (require 'poly-slim))


;; =============================================================================
;; Speculative languages =======================================================
;; =============================================================================

(use-package csharp-mode
  :defer-install t
  :commands (csharp-mode)
  :mode (("\\.cs\\'" . csharp-mode)))

(use-package clojure-mode
  :defer-install t
  :commands (clojure-mode
             clojurescript-mode
             clojurec-mode
             clojurex-mode)
  :mode (("\\.clj\\|dtm\\|edn\\'" . clojure-mode)
         ("\\.cljc\\'" . clojurec-mode)
         ("\\.cljx\\'" . clojurex-mode)
         ("\\.cljs\\'" . clojurescript-mode)
         ("\\(?:build\\|profile\\)\\.boot\\'" . clojure-mode)))

(use-package d-mode
  :defer-install t
  :commands (d-mode)
  :mode ("\\.d[i]?\\'" . d-mode))

(use-package go-mode
  :defer-install t
  :commands (go-mode
             gofmt-before-save
             godoc
             go-download-play)
  :mode (("\\.go\\'" . go-mode))
  :config
  (use-package company-go
    :commands (company-go)))

(use-package swift-mode
  :defer-install t
  :commands (swift-mode
             swift-mode-run-repl)
  :mode ("\\.swift\\'" . swift-mode))

(use-package rust-mode
  :defer-install t
  :commands (rust-mode)
  :mode (("\\.rs\\'" . rust-mode)))

(use-package lua-mode
  :defer-install t
  :commands (lua-mode
             run-lua
             lua-start-process)
  :mode ("\\.lua$" . lua-mode)
  :interpreter ("lua" . lua-mode))

(use-package vimrc-mode
  :defer-install t
  :commands (vimrc-mode)
  :mode (("\\.vim\\'" . vimrc-mode)
         ("[._]?g?vimrc\\'" . vimrc-mode)
         ("\\.exrc\\'" . vimrc-mode)))

(use-package csv-mode
  :defer-install t
  :commands (csv-mode)
  :mode ("\\.[Cc][Ss][Vv]\\'" . csv-mode))

(use-package batch-mode
  :defer-install t
  :commands (batch-mode)
  :mode (("\\.bat\\'" . batch-mode)
         ("\\.cmd\\'" . batch-mode)))

(use-package j-mode
  :defer-install t
  :commands (j-mode)
  :mode ("\\.ij[rstp]$" . j-mode))

(use-package jinja2-mode
  :defer-install t
  :commands (jinja2-mode)
  :mode ("\\.jinja2\\'" . jinja2-mode))

(use-package scala-mode
  :defer-install t
  :commands (scala-mode:set-scala-syntax-mode
             scala-mode:goto-start-of-code
             scala-mode)
  :mode ("\\.\\(scala\\|sbt\\)\\'" . scala-mode))

(use-package vala-mode
  :defer-install t
  :commands (vala-mode)
  :mode ("\\.vala$" . vala-mode))

(use-package fsharp-mode
  :defer-install t
  :commands (fsharp-mode)
  :mode ("\\.fs[iylx]?$" . fsharp-mode))

(use-package elixir-mode
  :defer-install t
  :commands (elixir-mode-open-mode
             elixir-mode-open-elixir-home
             elixir-mode-open-docs-master
             elixir-mode-open-docs-stable
             elixir-mode-version)
  :mode (("\\.elixir\\'" . elixir-mode)
         ("\\.ex\\'"     . elixir-mode)
         ("\\.exs\\'"    . elixir-mode)))

(use-package gnuplot
  :defer-install t
  :commands (gnuplot-mode
             gnuplot-make-buffer
             run-gnuplot)
  :mode ("\\.gp$" . gnuplot-mode))

(use-package dylan-mode
  :defer-install t
  :commands (dylan-mode
             dylanlid-mode)
  :mode (("\\.dylan\\'" . dylan-mode)
         ("\\.lid\\'" . dylanlid-mode)))

(use-package processing-mode
  :defer-install t
  :commands (processing-find-sketch
             processing-mode)
  :mode ("\\.pde$" . processing-mode))

(use-package actionscript-mode
  :defer-install t
  :commands (actionscript-mode)
  :mode ("\\.as\\'" . actionscript-mode))

(use-package puppet-mode
  :defer-install t
  :commands (puppet-mode
             puppetfile-mode)
  :mode (("\\.pp\\'" . puppet-mode)
         ("Puppetfile\\'" . puppetfile-mode)))

(use-package gap-mode
  :defer-install t
  :commands (gap-mode)
  :mode ("\\.\\(g\\(?:ap\\|[di]\\)?\\)\\'" . gap-mode))

(use-package perl6-mode
  :defer-install t
  :commands (perl6-mode)
  :mode ("\\.p[lm]?6\\'" . perl6-mode)
  :interpreter ("perl6" . perl6-mode))

(use-package fstar-mode
  :defer-install t
  :commands (fstar-mode)
  :mode ("\\.fsti?\\'" . fstar-mode))

(use-package sml-mode
  :defer-install t
  :commands (run-sml
             sml-run
             sml-mode
             sml-cm-mode
             sml-lex-mode
             sml-yacc-mode)
  :mode (("\\.s\\(ml\\|ig\\)\\'" . sml-mode)
         ("\\.cm\\'" . sml-cm-mode)
         ("\\.grm\\'" . sml-yacc-mode))

  :init
  (add-to-list 'completion-ignored-extensions ".cm/")
  (add-to-list 'completion-ignored-extensions "CM/"))

(use-package salt-mode
  :defer-install t
  :commands (salt-mode)
  :mode ("\\.sls\\'" . salt-mode))

(use-package ahk-mode
  :defer-install t
  :commands (ahk-mode)
  :mode (("\\.ahk\\'" . ahk-mode)))

(use-package floobits
  :defer-install t
  :commands (floobits-debug
             floobits-summon
             floobits-follow-mode-toggle
             floobits-follow-user
             floobits-leave-workspace
             floobits-complete-signup
             floobits-share-dir-public
             floobits-share-dir-private
             floobits-join-workspace
             floobits-workspace-settings
             floobits-remove-from-workspace
             floobits-open-workspace-in-browser
             floobits-clear-highlights
             floobits-add-to-workspace))

(provide 'config-modes)
