(add-hook 'eshell-mode-hook
  (lambda ()
    (adaptive-wrap-prefix-mode -1)
    (visual-line-mode -1)
    (make-variable-buffer-local 'global-hl-line-mode)
    (eshell-smart-initialize)
    (make-local-variable 'scroll-margin)
    (make-local-variable 'smooth-scroll-margin)
    (setq
      global-hl-line-mode nil
      scroll-margin 0
      smooth-scroll-margin 0)))

(with-eval-after-load 'eshell
  (require 'em-smart)
  (setq
    eshell-where-to-jump 'begin
    eshell-review-quick-commands nil
    eshell-smart-space-goes-to-end t

    eshell-scroll-to-bottom-on-input t
    eshell-scroll-show-maximum-output nil
    eshell-cp-interactive-query t
    eshell-ln-interactive-query t
    eshell-mv-interactive-query t
    eshell-rm-interactive-query t
    eshell-mv-overwrite-files nil))

(add-hook 'term-mode-hook
  (lambda ()
    (adaptive-wrap-prefix-mode -1)
    (visual-line-mode -1)

    (make-variable-buffer-local 'global-hl-line-mode)
    (setq
      global-hl-line-mode nil)))
