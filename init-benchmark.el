(setq load-prefer-newer t)
(eval-when-compile (require 'cl-lib))

(eval-and-compile
  (add-to-list 'load-path
    (concat user-emacs-directory "modules/")))

(load (setq custom-file (concat user-emacs-directory "custom.el")))
(load-library "config-setq")
(load-library "config-package")
(load-library "config-modes")
(load-library "config-desktop")
(load-library "config-safety")
(load-library "config-evil")
(load-library "config-ui")
(load-library "config-whitespace")
(load-library "config-paste")
(load-library "config-company")
(load-library "config-vcs")
(load-library "config-minibuffer")
(load-library "config-intel")
(load-library "config-solarized")

(eval-when-compile
  (ignore-errors
    (require 'load-dir)))

(setq
  load-dir-debug nil
  load-dirs (concat
              user-emacs-directory
              "modules/modes/"))
