(eval-when-compile
  (with-demoted-errors
    (require 'hl-line)
    (require 'em-smart)
    (require 'em-unix)))

(defun generic-term-init ()
  ;; this disables key-chord-mode
  (set (make-local-variable 'input-method-function) nil)
  (adaptive-wrap-prefix-mode -1)
  (visual-line-mode -1)
  (yas-minor-mode -1)
  (make-variable-buffer-local 'global-hl-line-mode)
  (make-variable-buffer-local 'scroll-margin)
  (make-variable-buffer-local 'smooth-scroll-margin)
  (setq
    yas-dont-activate t
    global-hl-line-mode nil
    scroll-margin 0
    smooth-scroll-margin 0))

(add-hook 'term-mode-hook #'generic-term-init)
(add-hook 'shell-mode-hook #'generic-term-init)
(add-hook 'eshell-mode-hook #'generic-term-init)

(add-hook 'eshell-mode-hook #'eshell-smart-initialize)

(defun eshell/clear ()
  (interactive)
  (let ((inhibit-read-only t)) (erase-buffer)))

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

