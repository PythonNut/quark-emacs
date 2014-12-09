(eval-when-compile (require 'cl))

(eval-when-compile (require 'auto-complete))
(eval-when-compile (require 'auto-complete-config))

(add-hook 'first-change-hook
  (lambda () (require 'auto-complete-config)))

(with-eval-after-load 'auto-complete
  (diminish 'auto-complete-mode " ⇝")
  (global-auto-complete-mode +1)
  (ac-config-default)
  (ac-flyspell-workaround)
  (ac-linum-workaround)
  (ac-set-trigger-key "C-c <C-tab>")

  (setq ac-sources
    '(ac-source-abbrev
       ac-source-yasnippet
       ac-source-semantic
       ac-source-dictionary
       ac-source-filename
       ac-source-words-in-buffer
       ac-source-words-in-same-mode-buffers))

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
    popup-use-optimized-column-computation nil)

  (add-to-list 'ac-trigger-commands 'backwards-kill-char))
