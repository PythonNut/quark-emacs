(eval-when-compile
  (with-demoted-errors
    (require 'company)
    (require 'company-dabbrev-code)
    (require 'config-modes)))

(defun company-onetime-setup ()
  (global-company-mode +1)
  (run-hooks 'load-theme-hook)
  (remove-hook 'prog-mode-hook #'company-onetime-setup-proxy)
  (remove-hook 'text-mode-hook #'company-onetime-setup-proxy))

(defun company-onetime-setup-proxy ()
  (add-hook 'first-change-hook #'company-onetime-setup nil t))

(add-hook 'emacs-startup-hook
  (lambda ()
    (add-hook 'first-change-hook #'company-onetime-setup nil t)
    (add-hook 'prog-mode-hook #'company-onetime-setup-proxy)
    (add-hook 'text-mode-hook #'company-onetime-setup-proxy)))

(with-eval-after-load 'company
  (diminish 'company-mode (if (display-graphic-p) " γ" " Co"))
  (setq
    company-idle-delay 0.1
    company-echo-delay 0
    company-auto-complete 'company-explicit-action-p
    company-minimum-prefix-length 2
    company-show-numbers nil
    company-tooltip-flip-when-above t
    company-tooltip-align-annotations t

    company-backends
    '((company-capf
        company-yasnippet
        company-dabbrev-code
        company-files
        company-keywords)

       company-dabbrev))

  (cl-macrolet
    ((company-define-specific-modes (mode backend)
       `(add-hook ,mode
          (lambda ()
            (let ((old-backends company-backends))
              (set (make-local-variable 'company-backends)
                (cons (cons
                        ,backend
                        (cdar old-backends))
                  (cdr old-backends))))))))

    (with-no-warnings
      (generate-calls company-define-specific-modes
        (
          ('c++-mode-hook  'company-clang)
          ('objc-mode-hook 'company-clang)
          ('c-mode-hook    'company-clang)
          ('css-mode-hook  'company-css)
          ('java-mode-hook 'company-eclim)
          ('nxml-mode-hook 'company-nxml)))))

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

  (defun setup-company-tooltip-faces  ()
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

  (add-hook 'load-theme-hook #'setup-company-tooltip-faces)
  (setup-company-tooltip-faces))

(with-eval-after-load 'company-template
  (defun setup-company-template-faces ()
    (set-face-attribute 'company-template-field nil
      :foreground nil
      :background nil
      :inherit 'region))
  (add-hook 'load-theme-hook #'setup-company-template-faces)
  (setup-company-template-faces))

(with-eval-after-load 'company-dabbrev-code
  (setq company-dabbrev-code-everywhere t))

(provide 'config-auto-complete)
