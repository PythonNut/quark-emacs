;; -*- lexical-binding: t -*-
(require 'cl-lib)

(eval-when-compile
  (with-demoted-errors "Load error: %s"
    (require 'flx)))

(defun my/company-onetime-setup ()
  (require 'company)
  (run-hooks 'load-theme-hook)
  (remove-hook 'first-change-hook #'my/company-onetime-setup))

(add-hook 'emacs-startup-hook
          (lambda ()
            (add-hook 'first-change-hook #'my/company-onetime-setup)))

(eval-and-compile
  (with-demoted-errors "Load error: %s"
    (require 'config-setq))

  (cl-macrolet
      ((company-define-specific-modes
        (mode backend)
        `(progn
           (add-hook ,mode
                     (lambda ()
                       (require 'company)
                       (let ((old-backends company-backends))
                         (set (make-local-variable 'company-backends)
                              (cons (cons
                                     ,backend
                                     (cdar old-backends))
                                    (cdr old-backends)))))))))

    (with-no-warnings
      (my/generate-calls
          'company-define-specific-modes
        '(('c++-mode-hook     'company-irony)
          ('objc-mode-hook    'company-irony)
          ('c-mode-hook       'company-irony)
          ('arduino-mode-hook 'company-irony)
          ('cmake-mode-hook   'company-cmake)
          ('css-mode-hook     'company-css)
          ('java-mode-hook    'company-eclim)
          ('nxml-mode-hook    'company-nxml)
          ('html-mode-hook    'company-web-html)
          ('web-mode-hook     'company-web-html)
          ('tex-mode-hook     'company-math-symbols-latex)
          ('latex-mode-hook   'company-math-symbols-latex)
          ('scheme-mode-hook  'geiser-company-backend)
          ('texinfo-mode-hook 'company-semantic)
          ('python-mode-hook  'company-anaconda)
          ('text-mode-hook    'company-ispell))))))

(with-eval-after-load 'company
  (eval-when-compile
    (with-demoted-errors "Load error: %s"
      (require 'company)))

  (global-company-mode +1)
  (diminish 'company-mode (if (display-graphic-p) " ❃" " *"))
  (company-flx-mode +1)

  (setq company-idle-delay 0.1
        company-echo-delay 0
        company-auto-complete 'company-explicit-action-p
        company-minimum-prefix-length 2
        company-show-numbers nil
        company-tooltip-flip-when-above t
        company-tooltip-align-annotations t

        company-backends '((company-capf
                            company-yasnippet
                            company-dabbrev-code
                            company-files
                            company-keywords)

                           company-dabbrev))

  (defun company-complete-common-or-complete-full ()
    (interactive)
    (when (company-manual-begin)
      (if (eq last-command #'company-complete-common-or-cycle)
          (let ((company-selection-wrap-around t))
            (call-interactively #'company-complete-selection))
        (let ((buffer-mod-tick (buffer-chars-modified-tick)))
          (call-interactively #'company-complete-common)
          (when (= buffer-mod-tick (buffer-chars-modified-tick))
            (call-interactively #'company-complete-selection)
            (call-interactively #'company-complete))))))

  (define-key company-active-map (kbd "<tab>")
    #'company-complete-common-or-complete-full)
  (define-key company-active-map (kbd "TAB")
    #'company-complete-common-or-complete-full)

  (define-key company-active-map (kbd "<return>") nil)
  (define-key company-active-map (kbd "RET") nil)

  (defun company-select-above (&optional arg)
    (interactive "p")
    (if (let ((ov company-pseudo-tooltip-overlay))
          (and ov (< (overlay-get ov 'company-height) 0)))
        (company-select-next-or-abort arg)
      (company-select-previous-or-abort arg)))

  (defun company-select-below (&optional arg)
    (interactive "p")
    (if (let ((ov company-pseudo-tooltip-overlay))
          (and ov (< (overlay-get ov 'company-height) 0)))
        (company-select-previous-or-abort arg)
      (company-select-next-or-abort arg)))

  (define-key company-active-map (kbd "<up>") #'company-select-above)
  (define-key company-active-map (kbd "<down>") #'company-select-below)

  (defun my/company-setup-tooltip-faces  ()
    (set-face-attribute 'company-tooltip-common-selection nil
                        :background "#839496"
                        :foreground (if (< (display-color-cells) 256)
                                        "black"
                                      nil)
                        :underline nil
                        :inherit 'region)

    (set-face-attribute 'company-tooltip-selection nil
                        :background "#586e75"
                        :foreground nil
                        :inherit 'region)

    (set-face-attribute 'company-tooltip-common nil
                        :background nil
                        :underline nil
                        :inherit 'company-tooltip
                        :foreground "#586e75")

    (set-face-attribute 'company-tooltip-annotation nil
                        :foreground nil
                        :background nil
                        :inherit 'company-tooltip)

    (set-face-attribute 'company-tooltip nil
                        :foreground nil
                        :inherit 'default))

  (add-hook 'load-theme-hook #'my/company-setup-tooltip-faces))

(with-eval-after-load 'company-template
  (defun my/company-setup-template-faces ()
    (set-face-attribute 'company-template-field nil
                        :foreground nil
                        :background nil
                        :inherit 'region))
  (add-hook 'load-theme-hook #'my/company-setup-template-faces))

(with-eval-after-load 'company-dabbrev-code
  (eval-when-compile
    (with-demoted-errors "Load error: %s"
      (require 'company-dabbrev-code)))

  (setq company-dabbrev-code-everywhere t))

;;; ==================================================
;;; Hippie expand - secondary autocompletion framework
;;; ==================================================
(with-eval-after-load 'hippie-exp
  (eval-when-compile
    (with-demoted-errors "Load error: %s"
      (require 'hippie-exp)))

  (defun my/he-try-expand-flx-regexp (str)
    "Generate regexp for flexible matching of str."
    (concat (rx word-boundary)
            (mapconcat (lambda (x)
                         (concat (rx (zero-or-more word) (zero-or-more "-"))
                                 (list x)))
                       str
                       "")
            (rx (zero-or-more word) word-boundary)))

  (defun my/he-try-expand-flx-collect (str)
    "Find and collect all words that flex-match str, and sort by flx score"
    (let ((coll)
          (regexp (my/he-try-expand-flx-regexp str)))
      (save-excursion
        (goto-char (point-min))
        (while (search-forward-regexp regexp nil t)
          (push (thing-at-point 'symbol) coll)))
      (sort coll #'(lambda (a b)
                     (> (car (flx-score a str))
                        (car (flx-score b str)))))))

  (defun my/he-try-expand-flx (old)
    "Try to complete word using flx matching."
    (unless old
      (he-init-string (he-lisp-symbol-beg) (point))
      (unless (he-string-member he-search-string he-tried-table)
        (push he-search-string he-tried-table))
      (setq he-expand-list
            (unless (equal he-search-string "")
              (my/he-try-expand-flx-collect he-search-string))))
    (while (and he-expand-list
                (he-string-member (car he-expand-list) he-tried-table))
      (pop he-expand-list))
    (prog1
        (null he-expand-list)
      (if (null he-expand-list)
          (when old (he-reset-string))
        (he-substitute-string (pop he-expand-list)))))

  (setq hippie-expand-try-functions-list
        '(yas-hippie-try-expand
          try-expand-dabbrev
          try-expand-dabbrev-from-kill
          my/he-try-expand-flx
          try-expand-dabbrev-all-buffers
          try-complete-file-name-partially
          try-complete-file-name
          try-expand-all-abbrevs
          try-expand-list
          try-expand-line
          try-complete-lisp-symbol-partially
          try-complete-lisp-symbol)))

(global-set-key (kbd "M-/") #'hippie-expand)

(provide 'config-company)
