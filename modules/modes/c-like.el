(setq c-default-style "k&r")

;; prefer C++1y
(add-hook 'c++-mode-hook
  (lambda ()
    (setq flycheck-clang-language-standard "c++1y")))

;; prefer C11
(add-hook 'c-mode-hook
  (lambda ()
    (setq flycheck-clang-language-standard "c11")))
