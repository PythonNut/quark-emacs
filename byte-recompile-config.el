(add-to-list 'load-path (locate-user-emacs-file "modules/"))
(setq load-prefer-newer t
      gc-cons-threshold 402653184)
(require 'config-core)
(let ((my/inhibit-async-byte-recompile-config t))
  (require 'config-package))
(byte-recompile-config)
