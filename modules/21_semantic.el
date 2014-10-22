;;; ============================
;;; Semantic - language analyses
;;; ============================
(add-hook 'prog-mode-hook 'semantic-mode)
(eval-after-load 'semantic
  '(progn
    (global-semanticdb-minor-mode +1)
    (global-semantic-idle-scheduler-mode +1)
    (global-semantic-idle-summary-mode +1)))

