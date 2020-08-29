;; -*- lexical-binding: t -*-
(eval-when-compile (require 'config-macros))

;; =============================================================================
;; TeX/LaTeX ===================================================================
;; =============================================================================

(use-package company-math
  :defer-install t
  :commands (company-latex-commands
             company-math-symbols-latex
             company-math-symbols-unicode))

(with-eval-after-load 'lsp-mode
  (eval-when-compile
    (with-demoted-errors "Load error: %s"
      (require 'lsp-mode)))

  (let* ((local-texlab-debug-path
          (expand-file-name
           (locate-user-emacs-file "data/lsp/texlab/target/debug/texlab")))
         (local-texlab-release-path
          (expand-file-name
           (locate-user-emacs-file "data/lsp/texlab/target/release/texlab")))
         (system-texlab (executable-find "texlab"))
         (texlab-path (cond ((file-executable-p local-texlab-debug-path)
                             local-texlab-debug-path)
                            ((file-executable-p local-texlab-release-path)
                             local-texlab-release-path)
                            (system-texlab system-texlab)))
         (texlab-command (if (and texlab-path (eq system-type 'darwin))
                             (list "env" "PATH=/Library/TeX/texbin/" texlab-path)
                           texlab-path)))
    (when texlab-command
      (setq lsp-clients-texlab-executable texlab-command))))

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
  (el-patch-feature tex)

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
                          ("!" "neq")
                          ("o" "circ"))
        TeX-auto-global (locate-user-emacs-file "data/auctex"))

  (define-key TeX-mode-map "$" #'self-insert-command)

  (when (executable-find "zathura")
    (setf (cadr (assoc 'output-pdf TeX-view-program-selection)) "Zathura"))

  (when (executable-find "evince")
    (add-to-list 'TeX-output-view-style '("^pdf$" "." "evince --page-index=%(outpage) %o")))

  (when (and (eq system-type 'darwin)
             (file-directory-p "/Applications/Skim.app"))
    (setf (cadr (assoc 'output-pdf TeX-view-program-selection)) "Skim")

    ;; Due to the way AUCTeX invokes commands, the programs need to be
    ;; in the PATH, however for some reason on macOS the required
    ;; directories are only added to exec-path and not PATH, so we
    ;; correct this here.
    (advice-add
     'TeX-run-command :around
     (my/defun-as-value my/TeX-run-command/add-exec-path (old-fun &rest args)
       (let ((process-environment process-environment))
         (setenv "PATH"
                 (concat "$PATH" (string-join exec-path path-separator)) t)
         (apply old-fun args)))))

  (defun my/LaTeX-format-name ()
    (save-excursion
      (goto-char (point-min))
      (if (looking-at "%&\\(.*\\)$")
          (substring-no-properties (match-string 1))
        "myformat")))

  (setcar (cdr (assoc "LaTeX" TeX-command-list)) "%`%l%(mode) %t")
  (add-to-list 'TeX-expand-list '("%fmt" my/LaTeX-format-name))
  (add-to-list 'TeX-command-list
               '("mylatexformat"
                 "%(PDF)%(latex) -ini -jobname=%fmt '&%(PDF)latex' mylatexformat.ltx %t"
                 TeX-run-TeX t
                 (latex-mode)
                 :help "Run mylatexformat"))

  (add-to-list 'safe-local-variable-values
               '(TeX-command-extra-options . "-shell-escape"))

  (with-eval-after-load 'texmathp
    (add-to-list 'texmathp-tex-commands-default '("tableau" env-on))
    (texmathp-compile))

  (eval-when-compile
    (use-package smartparens))

  (require 'smartparens-latex)
  (sp-with-modes '(tex-mode plain-tex-mode latex-mode LaTeX-mode)
    (sp-local-pair "\\[" "\\]"
                   :unless '(sp-latex-point-after-backslash)
                   :post-handlers
                     '(:add
                       ("||\n[i]" "RET"))))

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

  (el-patch-defun TeX-process-check (name)
    "Check if a process for the TeX document NAME already exist.
If so, give the user the choice of aborting the process or the current
command."
    (let (process)
      (while (and (setq process (TeX-process name))
                  (eq (process-status process) 'run))
        (el-patch-swap
          (cond
           ((yes-or-no-p (concat "Process `"
                                 (process-name process)
                                 "' for document `"
                                 name
                                 "' running, kill it? "))
            (delete-process process))
           ((eq (process-status process) 'run)
            (error "Cannot have two processes for the same document")))
          (delete-process process)))))

  (use-package magic-latex-buffer
    :defer-install t
    :commands (magic-latex-buffer)
    :init
    (el-patch-feature magic-latex-buffer)
    :config
    (setq magic-latex-enable-block-align nil
          magic-latex-enable-inline-image nil)

    (set-face-attribute 'ml/llarge nil :height 1.25)
    (set-face-attribute 'ml/xlarge nil :height 1.3)
    (set-face-attribute 'ml/huge nil :height 1.35)
    (set-face-attribute 'ml/hhuge nil :height 1.4)

    (el-patch-defun ml/search-regexp (regex &optional bound backward point-safe)
      "Like `search-regexp' but skips escaped chars, comments and
verbish environments. This function raise an error on
failure. When POINT-SAFE is non-nil, the point must not be in the
matching string."
      (ml/safe-excursion
       (let ((case-fold-search nil))
         (if backward
             (search-backward-regexp regex bound)
           (search-forward-regexp regex bound)))
       (or (save-match-data
             (save-excursion
               (and (goto-char (match-beginning 0))
                    (not (and point-safe
                              (< (point) ml/jit-point)
                              (< ml/jit-point (match-end 0))))
                    (looking-back (el-patch-swap
                                    "\\([^\\\\]\\|^\\)\\(\\\\\\\\\\)*"
                                    (rx (or (not-char "\\") bol)
                                        (zero-or-more "\\\\")))
                                  (el-patch-add (line-beginning-position)))
                    (not (ml/skip-comments-and-verbs backward)))))
           (ml/search-regexp regex bound backward point-safe)))))


  (add-hook 'TeX-mode-hook #'lsp-deferred)

  (add-hook
   'TeX-mode-hook
   (my/defun-as-value my/setup-tex-mode-company-backends ()
     (let ((old-backends company-backends))
       (set (make-local-variable 'company-backends)
            (append (list (append
                           '(company-lsp
                             company-dabbrev)
                           (cdar old-backends)))
                    '((company-ispell))
                    (cdr old-backends))))))

  (use-package evil
    :config
    (use-package evil-tex
      :commands (evil-tex-mode))

    (add-hook 'LaTeX-mode-hook #'evil-tex-mode)
    (add-hook 'LaTeX-mode-hook #'my/evil-LaTeX-setup)
    (evil-set-initial-state 'TeX-error-overview-mode 'insert))

  (add-to-list 'sp-sexp-suffix (list 'latex-mode 'regexp ""))
  (add-hook
   'LaTeX-mode-hook
   (my/defun-as-value my/setup-LaTeX-mode ()
     (adaptive-wrap-prefix-mode -1)
     (when (display-graphic-p)
       (magic-latex-buffer))))
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)

  (use-package auctex-latexmk
    :commands (auctex-latexmk-setup)
    :init
    (auctex-latexmk-setup)
    :config
    (setq auctex-latexmk-inherit-TeX-PDF-mode t))

  (advice-add
   'TeX-command-master :around
   (my/defun-as-value nadvice/TeX-command-master (old-fun arg)
     (interactive "P")
     (if (called-interactively-p 'any)
         (if (consp arg)
             (call-interactively old-fun)
           (cl-letf* (((symbol-function #'TeX-command-query)
                       (lambda (name)
                         (TeX-command-default name)
                         (car-safe (TeX-assoc "LatexMk" TeX-command-list)))))
             (call-interactively old-fun)))
       (apply old-fun args))))

  (advice-add
   'TeX-source-correlate-sync-source :after
   (my/defun-as-value nadvice/TeX-source-correlate-sync-source (&rest args)
     (recenter)
     (require 'pulse)
     (pulse-momentary-highlight-one-line (point))))

  (advice-add
   'LaTeX-math-insert :around
   (my/defun-as-value nadvice/LaTeX-math-insert (old-fun string dollar)
     (let ((TeX-insert-braces nil))
       (if (texmathp)
           (funcall old-fun string dollar)
         (funcall old-fun string (not dollar))))))

  (add-hook
   'TeX-after-insert-macro-hook
   (my/defun-as-value my/auto-yasnippet-TeX-macro ()
     "Convert the TeX macro around point into a YASnippet snippet"
     (let ((beg (TeX-find-macro-start))
           (end (TeX-find-macro-end)))
       (when (and beg
                  end
                  (looking-at "}"))
         (yas-expand-snippet
          (replace-regexp-in-string
           "{}"
           "{${}}"
           (substring-no-properties (buffer-substring beg end)))
          beg
          end))))))

(with-eval-after-load 'latex
  (setq TeX-electric-math (cons "\\\(" "\\\)"))

  (define-key LaTeX-mode-map "(" #'self-insert-command)
  (define-key LaTeX-mode-map "[" #'self-insert-command)
  (define-key LaTeX-mode-map "{" #'self-insert-command)

  (defun my/LaTeX-verbatimish-p ()
    (or (LaTeX-verbatim-p)
        (string-match-p (rx bol
                            (or "tikzpicture"
                                "circuitikz"
                                "lstlisting"
                                "minted"
                                "verbatim")
                            eol)
                        (LaTeX-current-environment))))

  (defun my/TeX-escape-from-math (&optional space)
    (let ((why (progn (texmathp)
                      (car texmathp-why))))
      (cond ((string-match-p (rx (or "$" "\\(")) why)
             (while (texmathp) (sp-up-sexp))
             (when space
               (unless (looking-at (rx (or space punct)))
                 (insert " "))))

            ((string= why "\\[")
             (while (texmathp) (sp-up-sexp))
             (when space
               (unless (looking-at (rx (or space punct)))
                 (newline-and-indent))))

            ((string-match-p (rx alpha) why)
             (while (texmathp) (LaTeX-find-matching-end))
             (when space
               (unless (looking-at (rx (or space punct)))
                 (newline-and-indent)))))))

  (defun TeX-math-chord ()
    (interactive)
    (if (texmathp)
        (my/TeX-escape-from-math)
      (TeX-insert-dollar)))

  (defun TeX-math-chord-spaced ()
    (interactive)
    (if (texmathp)
        (my/TeX-escape-from-math t)
      (unless (or (bolp)
                  (looking-back (rx (or space punct)) 1))
        (insert " "))
      (TeX-insert-dollar)))

  (key-chord-define TeX-mode-map (kbd "fj")
                    `(menu-item "" TeX-math-chord
                                :filter ,(lambda (cmd) (unless (my/LaTeX-verbatimish-p) cmd))))
  (key-chord-define TeX-mode-map (kbd "SPC SPC")
                    `(menu-item "" TeX-math-chord-spaced
                                :filter ,(lambda (cmd) (unless (my/LaTeX-verbatimish-p) cmd))))

  (defvar my/LaTeX-environment-or-macro-default "align*")
  (defun LaTeX-environment-or-macro (arg)
    "TeX-insert-macro and LaTeX-environment merged into one command"
    (interactive "*P")
    (let* ((symbol-list (TeX-symbol-list-filtered))
           (thing (completing-read
                   (concat "Thing: (default "
                           my/LaTeX-environment-or-macro-default
                           ") ")
                   (append (LaTeX-environment-list-filtered)
                           symbol-list)
                   nil nil nil
                   'LaTeX-environment-and-macro-history
                   my/LaTeX-environment-or-macro-default)))

      (setq my/LaTeX-environment-or-macro-default thing)
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

  (define-key TeX-mode-map (kbd ";")
    `(menu-item "" LaTeX-environment-or-macro
                :filter ,(lambda (cmd) (unless (my/LaTeX-verbatimish-p) cmd))))

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

(provide 'config-tex)
