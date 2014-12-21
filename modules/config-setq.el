(setq
  ad-redefinition-action 'accept
  cursor-type 'box
  delete-by-moving-to-trash t
  echo-keystrokes 0.4
  focus-follows-mouse 1
  gc-cons-threshold 20000000
  indent-tabs-mode nil
  inhibit-default-init 1
  inhibit-startup-echo-area-message t
  inhibit-startup-screen t
  jit-lock-defer-time 0.04
  jit-lock-stealth-nice 0.1
  jit-lock-stealth-time 0.2
  jit-lock-stealth-verbose nil
  left-margin-width 0
  max-lisp-eval-depth 5000
  max-specpdl-size 10000
  mouse-autoselect-window 0.3
  mouse-drag-copy-region nil
  right-margin-width 0
  ring-bell-function 'ignore
  select-active-regions t
  split-height-threshold 0
  split-width-threshold 0
  tab-width 4
  use-dialog-box nil
  x-select-enable-clipboard t
  x-select-enable-primary t
  x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)
  x-stretch-cursor t)

(setf
  interprogram-cut-function #'x-select-text
  interprogram-paste-function #'x-selection-value)

(setq-default
  buffer-file-coding-system 'utf-8
  indent-tabs-mode nil)

(put 'set-goal-column 'disabled nil)

(defalias 'yes-or-no-p #'y-or-n-p)

(add-hook 'find-file-hook (lambda () (prefer-coding-system 'utf-8)))

(provide 'config-setq)
