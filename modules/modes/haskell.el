;;; ============
;;; Haskell mode
;;; ============

(add-hook 'haskell-mode-hook
  '(lambda ()
     (turn-on-haskell-decl-scan)
     (turn-on-haskell-indentation)
     (turn-on-haskell-doc)))

