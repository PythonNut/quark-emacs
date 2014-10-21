;;; ===================================
;;; Ediff emacs diff for git/svn/hg/etc
;;; ===================================
;; side-by-side diffs
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; preserve origional window config
(add-hook 'ediff-load-hook
  (lambda ()
    (add-hook 'ediff-before-setup-hook
      (lambda ()
        ;; (set-face-foreground 'ediff-current-diff-face-A "#6c71c4")
        ;; (set-face-foreground 'ediff-current-diff-face-B "#cb4b16")
        ;; (set-face-foreground 'ediff-current-diff-face-C "#859900")

        (setq ediff-saved-window-configuration (current-window-configuration))))

    (let ((restore-window-configuration
            (lambda ()
              (set-window-configuration ediff-saved-window-configuration))))
      (add-hook 'ediff-quit-hook restore-window-configuration 'append)
      (add-hook 'ediff-suspend-hook restore-window-configuration 'append))))

