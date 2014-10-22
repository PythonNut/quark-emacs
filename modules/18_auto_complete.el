;;; ================================
;;; Auto-complete - self explanatory
;;; ================================
(require 'auto-complete-config)

(global-auto-complete-mode)
(add-hook 'text-mode-hook
  '(lambda ()
     (auto-complete-mode +1)))

(ac-set-trigger-key "C-c <C-tab>")

(ac-config-default)
(ac-flyspell-workaround)
(ac-linum-workaround)
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

(add-to-list 'ac-dictionary-directories "~/.emacs.d/elpa/auto-complete-20130724.1750/dict/")
(add-to-list 'ac-trigger-commands 'backwards-kill-char)

;;; =========================================
;;; company-mode alternative to auto-complete
;;; =========================================
(setq
  company-idle-delay 0.1
  company-minimum-prefix-length 2)

