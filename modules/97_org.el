;;; ========
;;; Org mode
;;; ========
(eval-after-load 'org
  '(progn
     (setq
       org-return-follows-link t
       org-return-indent t
       org-completion-use-ido t
       org-descriptive-links t)

     ;; Make windmove work in org-mode:
     (add-hook 'org-shiftup-final-hook 'windmove-up)
     (add-hook 'org-shiftleft-final-hook 'windmove-left)
     (add-hook 'org-shiftdown-final-hook 'windmove-down)
     (add-hook 'org-shiftright-final-hook 'windmove-right)

     (add-to-list 'org-modules 'org-mouse)))

(add-hook 'org-mode-hook
  '(lambda ()
     (yas/minor-mode -1)))

