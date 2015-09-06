;; -*- lexical-binding: t -*-

(eval-when-compile
  (with-demoted-errors "Load error: %s"
    (require 'cc-mode)
    (require 'flycheck)))

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; prefer C++1y
(add-hook 'c++-mode-hook
          (lambda ()
            (setq flycheck-clang-language-standard "c++1y")))

;; prefer C11
(add-hook 'c-mode-hook
          (lambda ()
            (setq flycheck-clang-language-standard "c11")))

(with-eval-after-load 'smartparens
  (with-eval-after-load 'cc-mode
    (setq c-default-style "k&r")
    (let ((my-c-modes
           '('c++-mode
             'java-mode
             'c-mode)))

      (dolist (mode my-c-modes)
        (sp-local-pair mode "/*" "*/" :post-handlers
                       '(:add
                         ("* ||\n[i]" "RET")))
        (sp-local-pair mode "{" nil :post-handlers
                       '(:add
                         ("||\n[i]" "RET")
                         ("| " "SPC")))))))
