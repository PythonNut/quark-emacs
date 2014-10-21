;;; ==================================
;;; icicles - supercharged completions
;;; ==================================
(require 'icicles)
;; (run-with-idle-timer 2 nil
;;   '(lambda ()
;;      (interactive)
;;      (require 'icicles)))

(eval-after-load 'icicles
  '(progn
     ;; (icy-mode +1)
     ;; (icicle-ido-like-mode +1)

     (setq icicle-region-background "grey20")

     (setq icicle-highlight-lighter-flag nil)
     (setq icicle-max-candidates 500)
     (setq icicle-default-cycling-mode 'apropos)
     (setq icicle-show-multi-completion-flag t)
     (setq icicle-search-highlight-all-current-flag t)

     (setq icicle-command-abbrev-match-all-parts-flag nil)
     ;; (setq icicle-add-proxy-candidates-flag t)
     (setq icicle-highlight-input-completion-failure-delay 0)
     (setq icicle-Completions-text-scale-decrease 0.2)
     ;; (add-to-list 'icicle-TAB-completion-methods 'fuzzy)
     (setq icicle-TAB-completion-methods '(vanilla substring basic))
     (setq locate-command "locate")
     (setq icicle-completions-format "vertical")

     ;; (setq icicle-use-C-for-actions-flag nil)
     (setq icicle-incremental-completion t)
     (setq icicle-incremental-completion-delay 0.1)
     (setq icicle-show-Completions-initially-flag t)
     ;; (setq icicle-top-level-when-sole-completion-flag t)
     ;; (setq icicle-top-level-when-sole-completion-delay 0.3)

     (set-face-background 'icicle-candidate-part "grey15")
     (set-face-background 'icicle-completion "grey15")
     (set-face-background 'icicle-mode-line-help "grey30")
     (set-face-foreground 'icicle-mode-line-help "#73d216")
     (set-face-foreground 'icicle-complete-input "#b4fa70")

     (set-face-foreground 'icicle-common-match-highlight-Completions "grey70")
     (set-face-background 'icicle-common-match-highlight-Completions "grey20")

     (set-face-foreground 'icicle-candidate-part "grey70")
     (set-face-background 'icicle-candidate-part "grey20")

     (set-face-foreground 'icicle-current-candidate-highlight "#fce94f")
     (set-face-background 'icicle-current-candidate-highlight "grey20")

     (set-face-background 'icicle-key-complete-menu-local "grey15")
     (set-face-foreground 'icicle-key-complete-menu-local "#e6a8df")

     (set-face-background 'icicle-special-candidate "grey10")
     (set-face-foreground 'icicle-special-candidate "#e6a8df")

     (set-face-background 'icicle-Completions-instruction-1 "grey15")
     (set-face-foreground 'icicle-Completions-instruction-1 "grey70")

     (defadvice icicle-other-window-or-frame (around buffers-too activate)
       (if (and
             (= (length (window-list)) 1)
             (= (length (frame-list)) 1))
         (if (and (elscreen-get-screen-list)
               (> (length (elscreen-get-screen-list)) 1))
           (elscreen-toggle)
           (progn
             (switch-to-buffer (other-buffer))))
         ad-do-it))

     (add-hook 'icicle-minibuffer-setup-hook
       '(lambda ()
          (setq icicle-Completions-window-max-height (/ (* (frame-height) 2) 3))))))

(global-set-key (kbd "C-c C-c o") 'icicle-occur)
(global-set-key (kbd "C-c C-c <tab>") 'icicle-dabbrev-completion)

(define-key evil-normal-state-map (kbd "<backtab>") 'icicle-complete-keys)
(define-key evil-normal-state-map (kbd "C-h <backtab>") 'icicle-complete-keys)
(define-key evil-normal-state-map (kbd "C-c <backtab>") 'icicle-complete-keys)
(define-key evil-normal-state-map (kbd "C-S-x <backtab>") 'icicle-complete-keys)
(define-key evil-normal-state-map (kbd "C-w <backtab>") 'icicle-complete-keys)
(define-key evil-normal-state-map (kbd "C-x <backtab>") 'icicle-complete-keys)
(define-key evil-normal-state-map (kbd "M-g <backtab>") 'icicle-complete-keys)
(define-key evil-normal-state-map (kbd "M-s <backtab>") 'icicle-complete-keys)
(define-key evil-normal-state-map (kbd "M-o <backtab>") 'icicle-complete-keys)
(define-key evil-normal-state-map (kbd "z <backtab>") 'icicle-complete-keys)
(define-key evil-normal-state-map (kbd "Z <backtab>") 'icicle-complete-keys)
(define-key evil-normal-state-map (kbd "g <backtab>") 'icicle-complete-keys)
(define-key evil-normal-state-map (kbd "<f2> <backtab>") 'icicle-complete-keys)
(define-key evil-normal-state-map (kbd "<f1> <backtab>") 'icicle-complete-keys)
(define-key evil-normal-state-map (kbd "C-<menu> <backtab>") 'icicle-complete-keys)
(define-key evil-normal-state-map (kbd "C-l <backtab>") 'icicle-complete-keys)
(define-key evil-normal-state-map (kbd "[ <backtab>") 'icicle-complete-keys)
(define-key evil-normal-state-map (kbd "] <backtab>") 'icicle-complete-keys)
(define-key evil-normal-state-map (kbd ", <backtab>") 'icicle-complete-keys)

(define-key evil-normal-state-map (kbd "C-s") 'icicle-search)
(define-key evil-insert-state-map (kbd "C-S-s") 'icicle-search)

(defun switch-to-minibuffer-window ()
  "switch to minibuffer window (if active)"
  (interactive)
  (when (active-minibuffer-window)
    (select-window (active-minibuffer-window))))

(global-set-key (kbd "C-'") 'switch-to-minibuffer-window)


