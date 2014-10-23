(setq
  split-width-threshold 0
  split-height-threshold 0
  inhibit-startup-screen t
  ring-bell-function 'ignore
  inhibit-startup-echo-area-message t
  inhibit-default-init 1
  ;; sentence-end-double-space nil
  delete-by-moving-to-trash t
  x-stretch-cursor t
  gc-cons-threshold 20000000
  right-margin-width 0
  left-margin-width 0
  cursor-type 'box
  indent-tabs-mode nil
  tab-width 4
  kkc-init-file-name "~/.emacs.d/kkrc"
  jit-lock-defer-time 0.04
  jit-lock-stealth-time 0.2
  jit-lock-stealth-nice 0.1
  jit-lock-stealth-verbose nil)

(provide 'config-setq)
