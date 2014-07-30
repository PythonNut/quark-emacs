Require this script

  (require 'electric-spacing)

and you can enable electric-spacing-mode with

  (electric-spacing-mode 1)

or "M-x electric-spacing-mode". Enabling the mode with
major-mode-hooks may be useful.

  (add-hook 'org-mode-hook 'electric-spacing-mode)

You can define a pattern where whitespaces should be inserted, by
adding a regexp-pair to "electric-spacing-regexp-pairs". For
example, following setting inserts spaces between Japanese and
English characters like: "foo ほげ bar ふが"

  (add-to-list 'electric-spacing-regexp-pairs
               '("\\cA\\|\\cC\\|\\ck\\|\\cK\\|\\cH" . "[0-9A-Za-z]"))
  (add-to-list 'electric-spacing-regexp-pairs
               '("[0-9A-Za-z]" . "\\cA\\|\\cC\\|\\ck\\|\\cK\\|\\cH"))

If you want to define a buffer-local pattern, make variable
"electric-spacing-regexp-pairs" buffer-local before registering the
pattern.

  (make-local-variable 'electric-spacing-regexp-pairs)

For more informations, see "Readme".
