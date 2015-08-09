(setq
  ad-redefinition-action 'accept
  cursor-type 'box
  delete-by-moving-to-trash t
  echo-keystrokes 0.4
  focus-follows-mouse t
  gc-cons-threshold 20000000
  indent-tabs-mode nil
  inhibit-default-init t
  inhibit-startup-echo-area-message t
  inhibit-startup-screen t
  inhibit-x-resources t
  interprogram-cut-function #'x-select-text
  interprogram-paste-function #'x-selection-value
  jit-lock-defer-time 0.04
  jit-lock-stealth-nice 0.1
  jit-lock-stealth-time 0.2
  jit-lock-stealth-verbose nil
  left-margin-width 0
  max-lisp-eval-depth 5000
  max-specpdl-size 10000
  mode-line-end-spaces nil
  mouse-autoselect-window 0.3
  mouse-drag-copy-region nil
  right-margin-width 0
  ring-bell-function 'ignore
  split-height-threshold 0
  split-width-threshold 0
  tab-width 4
  use-dialog-box nil
  x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)
  x-stretch-cursor t)

(setq-default
  buffer-file-coding-system 'utf-8
  indent-tabs-mode nil)

(put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'erase-buffer 'disabled nil)

(prefer-coding-system 'utf-8)

(defalias 'yes-or-no-p #'y-or-n-p)

(provide 'config-setq)
