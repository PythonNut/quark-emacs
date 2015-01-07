(eval-when-compile
  (with-demoted-errors
    (require 'tramp)))

(with-eval-after-load 'tramp
  ;; cache passwords for the duration of the session
  ;; note that said cache is _not_ persistent
  (setq
    password-cache-expiry nil
    tramp-default-method "ssh"))
