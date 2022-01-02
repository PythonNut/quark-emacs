;; -*- lexical-binding: t -*-
(eval-when-compile
  (require 'config-macros)
  (require 'config-package))
(require 'cl-lib)


(eval-when-compile
  (with-demoted-errors "Load error: %s"
    (require 'evil)))

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
  :init
  (my/onetime-setup auto-compile
    :hook 'before-save-hook
    (auto-compile-on-save-mode +1))

  :config
  (setq auto-compile-display-buffer nil
        auto-compile-mode-line-counter t))

(use-package lisp-mode
  :ensure nil
  :config
  (use-package smartparens
    :config
    (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
    (sp-local-pair 'emacs-lisp-mode "`" nil :when '(sp-in-string-p)))

  (add-hook 'emacs-lisp-mode-hook #'eldoc-mode)
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
  (add-hook
   'emacs-lisp-mode-hook
   (my/defun-as-value my/diminish-elisp-mode ()
     (setq mode-name (if (display-graphic-p) "λ" "EL"))))

  (define-key emacs-lisp-mode-map (kbd "C-c e") #'replace-last-sexp)
  (define-key emacs-lisp-mode-map (kbd "M-.") #'emacs-lisp-goto-definition)
  (define-key emacs-lisp-mode-map (kbd "M-,") #'evil-jump-backward)

  (with-eval-after-load 'evil
    (eval-when-compile
      (with-demoted-errors "Load error: %s"
        (require 'evil)))
    (evil-define-key 'normal emacs-lisp-mode-map "gd"
      #'emacs-lisp-goto-definition)))

(use-package suggest
  :defer-install t
  :commands (suggest suggest-update))

(use-package helpful
  :init
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-h v") #'helpful-variable)

  (el-patch-feature helpful)

  :config
  (define-key helpful-mode-map "J" #'forward-button)
  (define-key helpful-mode-map "K" #'backward-button)

  (el-patch-defun helpful--read-symbol (prompt default-val predicate)
    "Read a symbol from the minibuffer, with completion.
Returns the symbol."
    (when (and default-val
               (not (funcall predicate default-val)))
      (setq default-val nil))
    (when default-val
      ;; `completing-read' expects a string.
      (setq default-val (symbol-name default-val))

      ;; TODO: Only modify the prompt when we don't have ido/ivy/helm,
      ;; because the default is obvious for them.
      (setq prompt
            (replace-regexp-in-string
             (rx ": " eos)
             (format " (default: %s): " default-val)
             prompt)))
    (intern (completing-read prompt
                             (el-patch-swap obarray
                                            #'help--symbol-completion-table)
                             predicate t nil nil
                             default-val))))

(use-package macrostep
  :defer-install t
  :commands (macrostep-mode macrostep-expand)
  :init
  (define-key emacs-lisp-mode-map (kbd "C-c C-e") #'macrostep-expand)
  :config
  (evil-make-intercept-map macrostep-keymap))

;; =============================================================================
;; C-like ======================================================================
;; =============================================================================

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
  (add-hook
   'js2-mode-hook
   (my/defun-as-value my/diminish-js2-mode ()
     (setq mode-name "JS"))))

(use-package rjsx-mode
  :defer-install t
  :commands (rjsx-mode)
  :mode (("\\.jsx\\'" . rjsx-mode)
         ("\\.js\\'" . rjsx-mode)))


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
  (eval-when-compile
    (with-demoted-errors "Load error: %s"
      (require 'sh-script)
      (require 'el-patch)))

  (add-hook
   'sh-mode-hook
   (my/defun-as-value my/sh-mode-detect-zsh ()
     (setq mode-name "sh")
     (if (and buffer-file-name
              (string-match-p (rx ".zsh" line-end) buffer-file-name))
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
                   :suffix ""))

  (el-patch-defun sh-syntax-propertize-function (start end)
    (goto-char start)
    (sh-syntax-propertize-here-doc end)
    (funcall
     (syntax-propertize-rules
      (sh-here-doc-open-re
       (2 (sh-font-lock-open-heredoc
           (match-beginning 0) (match-string 1) (match-beginning 2))))
      ("\\s|" (0 (prog1 nil (sh-syntax-propertize-here-doc end))))
      ;; A `#' begins a comment when it is unquoted and at the
      ;; beginning of a word.  In the shell, words are separated by
      ;; metacharacters.  The list of special chars is taken from
      ;; the single-unix spec of the shell command language (under
      ;; `quoting') but with `$' removed.
      (el-patch-swap
        ("\\(?:[^|&;<>()`\\\"' \t\n]\\|\\${\\)\\(#+\\)" (1 "_"))
        ("\\(?:[^|&;<>(`\\\"' \t\n]\\|\\${\\)\\(#+\\)" (1 "_")))
      ;; In addition, `#' at the beginning of closed parentheses
      ;; does not start a comment if the parentheses are not isolated
      ;; by metacharacters, excluding [()].
      ;; (e.g. `foo(#q/)' and `(#b)foo' in zsh)
      (el-patch-add
        ("[^|&;<>(`\\\"' \t\n](\\(#+\\)" (1 "_"))
        ("(\\(#\\)[^)]+?)[^|&;<>)`\\\"' \t\n]" (1 "_")))
      ;; In a '...' the backslash is not escaping.
      ("\\(\\\\\\)'" (1 (sh-font-lock-backslash-quote)))
      ;; Make sure $@ and $? are correctly recognized as sexps.
      ("\\$\\([?@]\\)" (1 "_"))
      ;; Distinguish the special close-paren in `case'.
      (")" (0 (sh-font-lock-paren (match-beginning 0))))
      ;; Highlight (possibly nested) subshells inside "" quoted
      ;; regions correctly.
      ("\"\\(?:\\(?:[^\\\"]\\|\\\\.\\)*?\\)??\\(\\$(\\|`\\)"
       (1 (ignore
           (if (nth 8 (save-excursion (syntax-ppss (match-beginning 0))))
               (goto-char (1+ (match-beginning 0)))
             ;; Save excursion because we want to also apply other
             ;; syntax-propertize rules within the affected region.
             (save-excursion
               (sh-font-lock-quoted-subshell end)))))))
     (point) end)))

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
  (use-package lsp-julia
    :recipe (lsp-julia :type git
                       :host github
                       :repo "non-Jedi/lsp-julia")

    :init
    (setq lsp-julia-package-dir nil
          lsp-julia-default-environment
          (car (last (file-expand-wildcards
                      (expand-file-name "~/.julia/environments/v*")))))
    (require 'lsp-julia)
    (add-hook 'julia-mode-hook #'lsp-deferred)

    (defun lsp-julia--get-root-remote ()
      "Get the (Julia) project root directory of the current file over TRAMP."
      (concat "\""
              (tramp-file-name-localname
               (tramp-dissect-file-name
                (expand-file-name
                 (or (locate-dominating-file default-directory "Project.toml")
                     (locate-dominating-file default-directory "JuliaProject.toml")
                     (my/tramp-build-name-from-localname
                      (car (last (file-expand-wildcards
                                  (expand-file-name "~/.julia/environments/v*")))))))))
              "\""))

    (defun lsp-julia--symbol-server-store-path-to-jl-no-expand ()
      "Convert the variable `lsp-julia-symbol-server-store-path' to a
    string or \"nothing\" if `nil'"
      (if lsp-julia-symbol-server-store-path
          (let ((sssp lsp-julia-symbol-server-store-path))
            (make-directory sssp t)
            (concat "\"" sssp "\""))
        "nothing"))

    (defun lsp-julia--rls-command-remote ()
      "The command to lauch the Julia Language Server."
      `(,lsp-julia-command
        ,@lsp-julia-flags
        ,(concat "-e "
                 "'"
                 "import Pkg; Pkg.instantiate(); "
                 "using InteractiveUtils, Sockets, SymbolServer, LanguageServer; "
                 "Union{Int64, String}(x::String) = x; "
                 "server = LanguageServer.LanguageServerInstance("
                 "stdin, stdout, "
                 (lsp-julia--get-root-remote) ", "
                 (lsp-julia--get-depot-path) ", "
                 "nothing, "
                 (lsp-julia--symbol-server-store-path-to-jl-no-expand) "); "
                 "run(server);"
                 "'")))

    (lsp-register-client
     (make-lsp-client :new-connection (lsp-tramp-connection 'lsp-julia--rls-command-remote)
                      :major-modes '(julia-mode ess-julia-mode)
                      :server-id 'julia-ls-remote
                      :remote? t
                      :multi-root t
                      :priority -1)))

  (use-package evil
    :config
    (evil-set-initial-state 'inferior-julia-mode 'insert)))

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

(use-package lsp-haskell
  :defer-install t
  :config
  (lsp-register-client
   (make-lsp--client
    :new-connection (lsp-tramp-connection (lambda () (lsp-haskell--hie-command)))
    :major-modes '(haskell-mode)
    :server-id 'hie-remote
    :remote? t)))

(with-eval-after-load 'haskell-mode
  (require 'lsp-haskell)
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode))

(with-eval-after-load 'haskell-interactive-mode
  (evil-set-initial-state 'haskell-interactive-mode 'insert)
  (setq haskell-process-suggest-remove-import-lines t
        haskell-process-auto-import-loaded-modules t))

(use-package alex-mode
  :recipe (alex-mode :host github
                     :repo "sergv/dotemacs"
                     :files ("src/haskell/alex-mode.el")
                     :nonrecursive t)
  :defer-install t
  :commands (alex-mode)
  :init
  (provide 'common)

  (defmacro defparameter (var &optional value doc)
    "Just like CL's defparameter, sets variable value when evaluated."
    (let ((tmp-var '#:store))
      `(progn
         (setf ,tmp-var ,value)
         (if (boundp ',var)
             (setf ,var ,tmp-var)
           (defvar ,var ,tmp-var ,doc))
         nil)))

  :mode "\\.x\\'"
  :config
  (defalias 'alex-indent-line #'indent-relative))

(use-package happy-mode
  :recipe (happy-mode :host github :repo "sergv/happy-mode")
  :defer-install t
  :commands (happy-mode)
  :mode "\\.y\\'"
  :init
  (use-package mmm-mode))

;; =============================================================================
;; Web Development =============================================================
;; =============================================================================

(use-package sgml-mode
  :ensure nil
  :config
  ;; after deleting a tag, indent properly
  (define-advice sgml-delete-tag (:after (&rest _args) auto-indent)
    ;; TODO: This is super slow right?
    (indent-region (point-min) (point-max))))

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

(use-package php-mode
  :defer-install t
  :commands (php-mode)
  :mode (("\\.php[s345t]?\\'" . php-mode)
         ("\\.phtml\\'"       . php-mode)
         ("Amkfile"           . php-mode)
         ("\\.amk$"           . php-mode))
  :interpreter ("php" . php-mode))

(use-package typescript-mode
  :defer-install t
  :commands (typescript-mode)
  :mode ("\\.ts\\'" . typescript-mode)
  :config
  (with-eval-after-load 'folding
    (when (fboundp 'folding-add-to-marks-list)
      (folding-add-to-marks-list 'typescript-mode "// {{{" "// }}}" )))

  (with-eval-after-load 'smartparens
    (sp-local-pair 'typescript-mode "{" nil :post-handlers
                   '(:add
                     ("||\n[i]" "RET")
                     ("| " "SPC"))))

  (use-package tide
    :defer-install t
    :commands (tide-setup)
    :config
    ;; This is because tide is poorly behaved and messes with
    ;; company-backends on load
    (cl-delete 'company-tide company-backends))

  (add-hook 'typescript-mode-hook #'tide-setup)

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
                                '((company-tide
                                   company-yasnippet
                                   company-files)
                                  (company-dabbrev-code)
                                  company-dabbrev))))))))
      (with-no-warnings
        (my/generate-calls-single
            'company-define-specific-modes
          '('typescript-mode-hook))))))

(use-package impatient-mode
  :defer-install t
  :commands (impatient-mode))

(use-package apheleia
  :recipe (apheleia :host github :repo "raxod502/apheleia"))

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

  (define-advice comint-previous-matching-input-from-input
      (:around (old-fun &rest args) maybe-previous-line)
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
    (evil-set-initial-state 'geiser-repl-mode 'emacs)))

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
  :init
  (el-patch-feature term)

  :config
  (add-hook 'term-exec-hook 'with-editor-export-editor)

  (define-advice term-sentinel
      (:around (old-fun &rest args) maybe-cleanup-buffer)
    (cl-destructuring-bind (proc _msg) args
      (if (memq (process-status proc) '(signal exit))
          (let ((buffer (process-buffer proc)))
            (apply old-fun args)
            (kill-buffer buffer)
            (winner-undo)
            (message ""))
        (apply old-fun args))))

  (define-key term-raw-map (kbd "<f12>") #'term-kill-subjob)
  (define-key term-raw-map (kbd "<remap> <cua-paste>") #'term-paste)

  (el-patch-defun term-exec-1 (name buffer command switches)
    ;; We need to do an extra (fork-less) exec to run stty.
    ;; (This would not be needed if we had suitable Emacs primitives.)
    ;; The 'if ...; then shift; fi' hack is because Bourne shell
    ;; loses one arg when called with -c, and newer shells (bash,  ksh) don't.
    ;; Thus we add an extra dummy argument "..", and then remove it.
    (let ((process-environment
           (nconc
            (list
             (format "TERM=%s" term-term-name)
             (format "TERMINFO=%s" data-directory)
             (format term-termcap-format "TERMCAP="
                     term-term-name term-height term-width)

             (format "INSIDE_EMACS=%s,term:%s" emacs-version term-protocol-version)
             (format "LINES=%d" term-height)
             (format "COLUMNS=%d" term-width))
            process-environment))
          (process-connection-type t)
          ;; We should suppress conversion of end-of-line format.
          (inhibit-eol-conversion t)
          ;; The process's output contains not just chars but also binary
          ;; escape codes, so we need to see the raw output.  We will have to
          ;; do the decoding by hand on the parts that are made of chars.
          (coding-system-for-read 'binary))
      (when (term--bash-needs-EMACSp)
        (push (format "EMACS=%s (term:%s)" emacs-version term-protocol-version)
              process-environment))
      (apply (el-patch-swap #'start-process
                            #'start-file-process)
             name buffer
             "/bin/sh" "-c"
             (format "stty -nl echo rows %d columns %d sane 2>/dev/null;\
if [ $1 = .. ]; then shift; fi; exec \"$@\""
                     term-height term-width)
             ".."
             command switches)))

  (define-advice ansi-term
      (:filter-args (&optional args) use-default-shell)
    (interactive "P")
    (cl-destructuring-bind (&optional program new-buffer-name) args
      (let ((default-shell (my/detect-shell)))
        (if (stringp program)
            (list program new-buffer-name)
          (if (consp program)
              (list (read-from-minibuffer "Run program: "
                                          default-shell)
                    new-buffer-name)
            (list default-shell new-buffer-name)))))))

(use-package slurm-script-mode
  :recipe (slurm-script-mode
           :type git
           :host github
           :repo "PythonNut/slurm.el"
           :files ("slurm-script-mode.el"))
  :defer-install t
  :commands (slurm-script-mode))

(use-package slurm-mode
  :recipe (slurm-mode
           :type git
           :host github
           :repo "PythonNut/slurm.el"
           :files ("slurm-mode.el"))
  :defer-install t
  :commands (slurm)
  :config
  (evil-set-initial-state 'slurm-mode 'insert))

(defun eshell-kill-whole-line ()
  (interactive)
  (eshell-bol)
  (kill-line))

(use-package eshell
  :ensure nil
  :config
  (eval-when-compile
    (with-demoted-errors "Load error: %s"
      (require 'em-smart)
      (require 'em-unix)
      (require 'em-cmpl)
      (require 'company)))

  (my/onetime-setup eshell
    :hook 'eshell-mode-hook
    (when (featurep 'evil)
      (evil-define-key 'insert eshell-mode-map (kbd "<tab>") #'company-complete)
      (evil-define-key 'insert eshell-mode-map (kbd "C-a") #'eshell-bol)
      (evil-define-key 'insert eshell-mode-map (kbd "<home>") #'eshell-bol)
      (evil-define-key 'insert eshell-mode-map (kbd "<C-S-backspace>") #'eshell-kill-whole-line)
      (evil-define-key 'insert eshell-mode-map (kbd "C-r") #'eshell-isearch-backward)))

  (add-hook
   'eshell-before-prompt-hook
   (my/defun-as-value my/eshell-slow-company-when-remote ()
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

(use-package vterm
  :commands (vterm)
  :defer-install t
  :config
  (evil-set-initial-state 'vterm-mode 'emacs)
  (set-face-foreground 'vterm-color-black "#586e75"))

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

(use-package pacfiles-mode
  :defer-install t
  :commands (pacfiles pacfiles-start))

(use-package git-modes
  :defer-install t
  :commands (gitattributes-mode gitconfig-mode gitignore-mode)
  :mode (("/\\.gitattributes\\'"       . gitattributes-mode)
         ("/\\.git/info/attributes\\'" . gitattributes-mode)
         ("/git/attributes\\'"         . gitattributes-mode)
         ("/\\.gitconfig\\'"           . gitconfig-mode)
         ("/\\.git/config\\'"          . gitconfig-mode)
         ("/git/config\\'"             . gitconfig-mode)
         ("/\\.gitmodules\\'"          . gitconfig-mode)
         ("/\\.gitignore\\'"           . gitignore-mode)
         ("/\\.git/info/exclude\\'"    . gitignore-mode)
         ("/git/ignore\\'"             . gitignore-mode)))

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
  :mode (("/PKGBUILD\\'" . pkgbuild-mode))
  :config
  (setq pkgbuild-update-sums-on-save nil)

  (el-patch-feature pkgbuild-mode)
  (el-patch-defun pkgbuild-update-sums-line-hook ()
    "Update sum lines if the file was modified"
    (if (and pkgbuild-update-sums-on-save (not pkgbuild-in-hook-recursion))
        (progn
          (setq pkgbuild-in-hook-recursion t)
          (save-buffer)                   ;always save BUFFER 2 times so we get the correct sums in this hook
          (el-patch-add (message "Recalculating sums..."))
          (setq pkgbuild-in-hook-recursion nil)
          (pkgbuild-update-sums-line)))))

(use-package pacfiles-mode
  :defer-install t
  :commands (pacfiles))

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

(use-package nginx-mode
  :defer-install t
  :commands (nginx-mode)
  :mode (("nginx\\.conf\\'"     . nginx-mode)
         ("/nginx/.+\\.conf\\'" . nginx-mode)))

(use-package terraform-mode
  :defer-install t
  :commands (terraform-mode)
  :mode (("\\.tf\\(vars\\)?\\'" . terraform-mode)))

;; =============================================================================
;; Markup modes ================================================================
;; =============================================================================

(use-package yaml-mode
  :recipe (yaml-mode :type git
                     :host github
                     :repo "PythonNut/yaml-mode")
  :defer-install t
  :commands (yaml-mode)
  :mode (("\\.e?ya?ml$" . yaml-mode)))

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

;; =============================================================================
;; Org mode ====================================================================
;; =============================================================================

(use-package ob-core
  :ensure nil
  :config
  (setq org-confirm-babel-evaluate nil)
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images)

  ;; Load languages when needed
  (define-advice org-babel-execute-src-block
      (:around (old-fun &rest args) lazy-load-languages)
    (let ((language (org-element-property :language (org-element-at-point))))
      (unless (cdr (assoc (intern language) org-babel-load-languages))
        (add-to-list 'org-babel-load-languages (cons (intern language) t))
        (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))
      (apply old-fun args))))

(use-package org
  :config
  (setq org-src-fontify-natively t
        org-startup-with-inline-images t)

  (add-hook 'org-mode-hook #'org-indent-mode)

  (defvar ob-language-file-alist
    (list '(ob-sage . ob-sagemath))
    "An alist that resolves discrepancies between language names and file names in org-babel"))

;; =============================================================================
;; R ===========================================================================
;; =============================================================================

(use-package ess
  :defer-install t
  :commands (R R-mode S)
  :mode (("\\.R$" . R-mode)))


;; =============================================================================
;; help-mode ===================================================================
;; =============================================================================
(use-package help
  :ensure nil
  :config
  (setq help-window-select t)
  (define-key help-mode-map "J" #'forward-button)
  (define-key help-mode-map "K" #'backward-button))


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
  :mode (("\\.rs\\'" . rust-mode))
  :config
  (use-package flycheck-rust
    :commands (flycehck-rust-setup))
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

  (with-eval-after-load 'smartparens
    (sp-with-modes
        '(rust-mode)
      (sp-local-pair "/*" "*/" :post-handlers
                     '(:add
                       ("* [i]|\n[i]" newline evil-ret)
                       (" " c-context-line-break c-indent-new-comment-line)))
      (sp-local-pair "{" nil :post-handlers
                     '(:add
                       ("||\n[i]" "RET")
                       ("| " "SPC"))))))

(use-package lua-mode
  :defer-install t
  :commands (lua-mode
             run-lua
             lua-start-process)
  :mode ("\\.lua$" . lua-mode)
  :interpreter ("lua" . lua-mode)
  :config
  (use-package company-lua
    :commands (company-lua)))

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

(use-package gnuplot
  :defer-install t
  :commands (gnuplot-mode
             gnuplot-make-buffer
             run-gnuplot)
  :mode ("\\.gp$" . gnuplot-mode))

(use-package processing-mode
  :defer-install t
  :commands (processing-find-sketch
             processing-mode)
  :mode ("\\.pde$" . processing-mode))

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

(use-package llvm-mode
  :recipe (llvm-mode :host github
                     :repo "llvm-mirror/llvm"
                     :files ("utils/emacs/llvm-mode.el"))
  :defer-install t
  :commands (llvm-mode)
  :mode ("\\.ll\\'" . llvm-mode))

;;; LSP

(use-package lsp-mode
  :commands (lsp-mode)
  :config
  (eval-when-compile
    (with-demoted-errors "Load error: %s"
      (require 'lsp-mode)))
  (require 'yasnippet)
  (setq lsp-prefer-flymake nil
        lsp-auto-guess-root t
        lsp-session-file (expand-file-name (locate-user-emacs-file "data/.lsp-session-v1"))
        lsp-headerline-breadcrumb-enable nil
        lsp-enable-suggest-server-download nil)

  (defun my/lsp-before-initialize/stall-for-remote-connections ()
    (when (lsp--client-remote?
           (lsp--workspace-client lsp--cur-workspace))
      (sit-for 0.1)))

  (add-hook 'lsp-before-initialize-hook
            #'my/lsp-before-initialize/stall-for-remote-connections))

(use-package lsp-ui
  :commands lsp-ui-mode
  :init
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  :config
  (setq lsp-ui-doc-position 'top)

  (when (memq window-system '(mac ns))
    (setq lsp-ui-doc-use-childframe nil)))

(my/require-config-module 'config-tex)
(my/require-config-module 'config-c)
(my/require-config-module 'config-python)

(provide 'config-modes)
