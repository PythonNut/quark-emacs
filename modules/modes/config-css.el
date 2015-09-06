;; -*- lexical-binding: t -*-

(with-eval-after-load 'css-mode
  (sp-local-pair 'css-mode "{" nil :post-handlers
                 '(:add
                   ("||\n[i]" "RET")
                   ("| " "SPC"))))

(with-eval-after-load 'scss-mode
  (sp-local-pair 'scss-mode "{" nil :post-handlers
                 '(:add
                   ("||\n[i]" "RET")
                   ("| " "SPC"))))

(with-eval-after-load 'web-mode
  (sp-local-pair 'web-mode "{" nil :post-handlers
                 '(:add
                   ("||\n[i]" "RET")
                   ("| " "SPC"))))
