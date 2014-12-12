(add-hook 'eshell-mode-hook
  (lambda ()
    (adaptive-wrap-prefix-mode -1)
    (visual-line-mode -1)
    (make-local-variable 'scroll-margin)
    (make-local-variable 'smooth-scroll-margin)
    (setq
      scroll-margin 0
      smooth-scroll-margin 0)))

(with-eval-after-load 'eshell
  (setq
    eshell-scroll-to-bottom-on-input t
    eshell-scroll-show-maximum-output nil
    eshell-cp-interactive-query t
    eshell-ln-interactive-query t
    eshell-mv-interactive-query t
    eshell-rm-interactive-query t
    eshell-mv-overwrite-files nil))
