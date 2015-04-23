(with-eval-after-load 'sage-mode
  (require 'sage))

(autoload #'sage-mode "sage-mode")
(add-to-list 'auto-mode-alist '("\\.sage\\'" . sage-mode))
