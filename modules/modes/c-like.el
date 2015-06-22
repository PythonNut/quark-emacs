(setq c-default-style "k&r")

;; prefer C++1y
(add-hook 'c++-mode-hook
  (lambda ()
    (setq flycheck-clang-language-standard "c++1y")))

;; prefer C11
(add-hook 'c-mode-hook
  (lambda ()
    (setq flycheck-clang-language-standard "c11")))

(with-eval-after-load 'cc-mode
  (let ((my-c-modes
          '('c++-mode
             'java-mode
             'c-mode)))

    (while my-c-modes
      (sp-local-pair (car my-c-modes) "{" nil :post-handlers
        '(:add
           ("||\n[i]" "RET")
           ("| " "SPC")))
      (setq my-c-modes (cdr my-c-modes)))))
