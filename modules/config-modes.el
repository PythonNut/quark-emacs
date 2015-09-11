;; -*- lexical-binding: t -*-

(eval-when-compile
  (with-demoted-errors "Load error: %s"
    (require 'evil)
    (require 'flycheck)))

;; =============================================================================
;; Emacs Lisp ==================================================================
;; =============================================================================

(defun emacs-lisp-goto-definition ()
  (interactive)
  (find-function (function-called-at-point)))

(defun replace-last-sexp ()
  (interactive)
  (let ((value (eval (preceding-sexp))))
    (kill-sexp -1)
    (insert (format "%S" value))))

(defun my/auto-compile-onetime-setup ()
  (require 'auto-compile)
  (auto-compile-on-save-mode +1)
  (remove-hook 'before-save-hook #'my/auto-compile-onetime-setup t))

(with-eval-after-load 'eldoc
  (diminish 'eldoc-mode))

(with-eval-after-load 'lisp-mode
  (with-eval-after-load 'smartparens
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

  (evil-define-key 'normal emacs-lisp-mode-map "gd"
    #'emacs-lisp-goto-definition))

;; =============================================================================
;; C-like ======================================================================
;; =============================================================================

(eval-when-compile
  (with-demoted-errors "Load error: %s"
    (require 'cc-mode)))

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(with-eval-after-load 'cc-mode
  (with-eval-after-load 'smartparens
    (setq c-default-style "k&r")

    ;; prefer C++1y
    (add-hook 'c++-mode-hook
              (lambda ()
                (setq flycheck-clang-language-standard "c++1y")))

    ;; prefer C11
    (add-hook 'c-mode-hook
              (lambda ()
                (setq flycheck-clang-language-standard "c11")))

    (let ((my-c-modes
           '('c++-mode
             'java-mode
             'c-mode)))

      (dolist (mode my-c-modes)
        (sp-local-pair mode "/*" "*/" :post-handlers
                       '(:add
                         ("* ||\n[i]" "RET")))
        (sp-local-pair mode "{" nil :post-handlers
                       '(:add
                         ("||\n[i]" "RET")
                         ("| " "SPC")))))))

;; =============================================================================
;; Javascript ==================================================================
;; =============================================================================

(eval-when-compile
  (with-demoted-errors "Load error: %s"
    (require 'js2-mode)
    (require 'js2-refactor)))

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(with-eval-after-load 'js2-mode
  (set-face-foreground 'js2-external-variable
                       (face-foreground 'default))

  (set-face-attribute 'js2-external-variable nil :weight 'extra-bold)
  (set-face-attribute 'js2-external-variable nil :underline t)
  (js2r-add-keybindings-with-prefix "C-c C-r")

  (with-eval-after-load 'smartparens
    (sp-local-pair 'js2-mode "{" nil :post-handlers
                   '(:add
                     ("||\n[i]" "RET")
                     ("| " "SPC"))))

  (setq js2-basic-offset 2))

;; =============================================================================
;; Shell Scripts ===============================================================
;; =============================================================================

;; bind zsh files to sh-mode
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))

;; bind zsh files to the zsh submode of sh-mode
(with-eval-after-load 'sh-script
  (add-hook 'sh-mode-hook
            (lambda ()
              (setq mode-name "sh")
              (if (string-match "\\.zsh$" buffer-file-name)
                  (sh-set-shell "zsh")))))

;; =============================================================================
;; Python ======================================================================
;; =============================================================================

(eval-when-compile
  (with-demoted-errors "Load error: %s"
    (require 'python)
    (require 'anaconda-mode)
    (require 'company-anaconda)))

(add-hook 'python-mode-hook #'anaconda-mode)
(add-hook 'python-mode-hook #'eldoc-mode)

(add-hook 'python-mode-hook
          (lambda ()
            ;; conflicts with `eldoc-mode'
            (semantic-idle-summary-mode -1)
            (setq mode-name "Py")))

(with-eval-after-load 'python
  (evil-define-key 'normal python-mode-map "gd" #'anaconda-mode-goto)
  (define-key python-mode-map (kbd "M-.") #'anaconda-mode-goto))

(with-eval-after-load 'anaconda-mode
  (diminish 'anaconda-mode " ✶"))

(add-to-list 'auto-mode-alist '("\\.pyx\\'" . cython-mode))
(add-to-list 'auto-mode-alist '("\\.pxd\\'" . cython-mode))
(add-to-list 'auto-mode-alist '("\\.pxi\\'" . python-mode))

(autoload 'run-sage "sage-shell-mode" nil t)
(autoload 'run-new-sage "sage-shell-mode" nil t)
(autoload 'sage-mode "sage-shell-mode" nil t)

(with-eval-after-load 'sage-shell-mode
  (sage-shell:define-alias)
  (evil-set-initial-state 'sage-shell-mode 'insert)

  (add-hook 'sage-shell-mode-hook #'eldoc-mode)
  (add-hook 'sage-mode-hook #'eldoc-mode)

  (add-hook 'sage-shell-mode-hook
            (lambda () (semantic-idle-summary-mode -1)))

  (add-hook 'sage-mode-hook
            (lambda () (semantic-idle-summary-mode -1))))

;; =============================================================================
;; Octave ======================================================================
;; =============================================================================

(with-eval-after-load 'octave-mode
  (with-eval-after-load 'smartparens
    (sp-local-pair 'octave-mode "'" nil :actions nil)))

;; =============================================================================
;; Web Development =============================================================
;; =============================================================================

(with-eval-after-load 'css-mode
  (sp-local-pair 'css-mode "{" nil :post-handlers
                 '(:add
                   ("||\n[i]" "RET")
                   ("| " "SPC"))))

(with-eval-after-load 'scss-mode
  (sp-local-pair 'scss-mode "{" nil :post-handlers
                 '(:add
                   ("||\n[i]" "RET")
                   ("| " "SPC"))))

(with-eval-after-load 'web-mode
  (sp-local-pair 'web-mode "{" nil :post-handlers
                 '(:add
                   ("||\n[i]" "RET")
                   ("| " "SPC"))))

;; =============================================================================
;; Dired =======================================================================
;; =============================================================================

(eval-when-compile
  (with-demoted-errors "Load error: %s"
    (require 'dired)
    (require 'dired-x)
    (require 'ls-lisp)))

(with-eval-after-load 'ls-lisp
  (setq ls-lisp-use-insert-directory-program nil
        ls-lisp-support-shell-wildcards t
        ls-lisp-dirs-first t
        ls-lisp-verbosity nil))

(with-eval-after-load 'dired
  (require 'ls-lisp)
  (require 'dired-x)

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
  (evil-define-key 'normal dired-mode-map "q" #'kill-this-buffer))

;; =============================================================================
;; Shell modes =================================================================
;; =============================================================================

(eval-when-compile
  (with-demoted-errors "Load error: %s"
    (require 'hl-line)
    (require 'em-smart)
    (require 'em-unix)))

(defun my/generic-term-init ()
  ;; this disables key-chord-mode
  (set (make-local-variable 'input-method-function) nil)
  (adaptive-wrap-prefix-mode -1)
  (visual-line-mode -1)
  (yas-minor-mode -1)
  (make-variable-buffer-local 'global-hl-line-mode)
  (make-variable-buffer-local 'scroll-margin)
  (make-variable-buffer-local 'smooth-scroll-margin)

  (setq yas-dont-activate t
        global-hl-line-mode nil
        scroll-margin 0
        smooth-scroll-margin 0))

(add-hook 'term-mode-hook #'my/generic-term-init)
(add-hook 'shell-mode-hook #'my/generic-term-init)
(add-hook 'eshell-mode-hook #'my/generic-term-init)

(add-hook 'eshell-mode-hook
          (lambda ()
            (make-variable-buffer-local 'company-idle-delay)))

(defun eshell-kill-whole-line ()
  (interactive)
  (eshell-bol)
  (kill-line))

(defun my/eshell-onetime-setup ()
  (evil-define-key 'insert eshell-mode-map (kbd "<tab>") #'company-complete)
  (evil-define-key 'insert eshell-mode-map (kbd "C-a") #'eshell-bol)
  (evil-define-key 'insert eshell-mode-map (kbd "<home>") #'eshell-bol)
  (evil-define-key 'insert eshell-mode-map (kbd "<C-S-backspace>") #'eshell-kill-whole-line)
  (evil-define-key 'insert eshell-mode-map (kbd "C-r") #'eshell-isearch-backward)

  (remove-hook 'eshell-mode-hook #'my/eshell-onetime-setup))

(with-eval-after-load 'eshell
  (add-hook 'eshell-directory-change-hook
            (lambda ()
              (setq company-idle-delay
                    (if (file-remote-p default-directory)
                        nil
                      0.1))))
  (add-hook 'eshell-mode-hook #'my/eshell-onetime-setup)
  (setq eshell-cmpl-dir-ignore "\\`\\(\\.\\.?\\|CVS\\|\\.svn\\|\\.git\\)/\\'"
        eshell-cmpl-file-ignore "\\(\\.elc\\|\\.zwc\\|\\.pyc\\|~\\|\\.swp\\)\\'"
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
    (if (string-match "\\`\\+\\([0-9]+\\)\\'" (car args))
        (let* ((line (string-to-number (match-string 1 (pop args))))
               (file (pop args)))
          (find-file file)
          (goto-line line))
      (find-file (pop args)))))

(provide 'config-modes)
