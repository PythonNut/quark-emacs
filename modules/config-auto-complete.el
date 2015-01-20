(eval-when-compile
  (with-demoted-errors
    (require 'company)
    (require 'company-dabbrev-code)))

(global-company-mode +1)

(with-eval-after-load 'company
  (diminish 'company-mode (if (display-graphic-p) " γ" " Co"))
  (setq
    company-idle-delay 0.1
    company-echo-delay 0
    company-auto-complete 'company-explicit-action-p
    company-minimum-prefix-length 2
    company-show-numbers nil
    company-tooltip-flip-when-above t
    company-tooltip-align-annotations t)

  (add-to-list 'company-backends 'company-yasnippet t)
  
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

  (define-key company-active-map [tab] #'company-complete-common-or-complete-full)
  (define-key company-active-map (kbd "TAB") #'company-complete-common-or-complete-full)

  (add-hook 'load-theme-hook
    (lambda ()
      (set-face-attribute 'company-tooltip-common-selection nil
        :background "#839496"
        :foreground nil
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
        :inherit 'default))))

(with-eval-after-load 'company-dabbrev-code
  (setq company-dabbrev-code-everywhere t))

(provide 'config-auto-complete)
