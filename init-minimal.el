;; load custom settings from external file
(load (setq custom-file (concat user-emacs-directory "custom")))

(icomplete-mode +1)
(show-paren-mode +1)
(xterm-mouse-mode +1)
(visual-line-mode +1)

(set-face-attribute 'show-paren-match nil
  :background nil
  :weight 'extra-bold)

(eval-and-compile
  (add-to-list 'load-path
    (concat user-emacs-directory "modules/")))

(require 'config-setq)

(setq
  lisp-indent-offset 2
  lisp-body-indent 2)

(require 'config-safety)

(load-theme 'tango-dark)

