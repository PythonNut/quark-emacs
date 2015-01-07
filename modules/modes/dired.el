(eval-when-compile
  (with-demoted-errors
    (require 'ls-lisp)))

(with-eval-after-load 'dired
  (require 'ls-lisp)
  (setq ls-lisp-use-insert-directory-program nil
    ls-lisp-support-shell-wildcards t
    ls-lisp-dirs-first t
    ls-lisp-verbosity nil))
