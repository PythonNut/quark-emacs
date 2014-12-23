(eval-when-compile
  (with-demoted-errors
    (require 'diminish)
    (require 'auto-complete)
    (require 'auto-complete-config)))

(with-eval-after-load 'auto-complete
  (diminish 'auto-complete-mode " α")
  (ac-config-default)
  (ac-flyspell-workaround)
  (ac-linum-workaround)
  (ac-set-trigger-key "C-c <C-tab>")

  (setq-default ac-sources
    (append
      '(
         ac-source-semantic
         ac-source-filename
         ac-source-yasnippet
         )
      ac-sources))

  (setq ac-auto-start t
    ac-auto-show-menu 0.5
    ac-show-menu-immediately-on-auto-complete t
    ac-ignore-case 'smart
    ac-delay 0
    ac-dwim t
    ac-use-fuzzy t
    ac-fuzzy-enable t
    ac-dwim-enable t
    ac-use-comphist t
    popup-use-optimized-column-computation nil))

(eval-when-compile (progn (require 'company)))

(setq
  company-idle-delay 0.1
  company-echo-delay 0
  company-auto-complete 'company-explicit-action-p
  company-minimum-prefix-length 2
  company-show-numbers nil
  company-tooltip-align-annotations t)

(define-key company-active-map (kbd "<tab>") #'company-complete)
(set-face-background 'company-tooltip-common-selection
  (face-background 'company-tooltip-selection))
(set-face-background 'company-tooltip-common
  (face-background 'company-tooltip))

(provide 'config-auto-complete)
