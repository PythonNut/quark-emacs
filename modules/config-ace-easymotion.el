(eval-when-compile
  (with-demoted-errors
    (require 'ace-jump-mode)
    (require 'noflet)
    (require 'evil)
    (require 'key-chord)
    (require 'evil-easymotion)))

(defun realign-cursor ()
  (interactive)
  (save-excursion
    (if (> (rest (nth 6 (posn-at-point)))
          (/ (window-body-height) 2))
      (progn
        (call-interactively #'previous-line)
        (call-interactively #'next-line))
      (progn
        (call-interactively #'next-line)
        (call-interactively #'previous-line)))))

(with-eval-after-load 'ace-jump-mode
  ;; use letters, numbers and capitals in that order
  (setq ace-jump-mode-move-keys
    (nconc
      (loop for i from ?a to ?z collect i)
      (loop for i from ?0 to ?9 collect i)
      (loop for i from ?A to ?Z collect i))
    ace-jump-mode-case-fold nil
    ace-jump-mode-scope 'visible)

  (ace-jump-mode-enable-mark-sync)

  (defadvice evil-ace-jump-word-mode
    (after realign activate preactivate compile)
    (ignore-errors (realign-cursor)))

  (defadvice evil-ace-jump-char-mode
    (after realign activate preactivate compile)
    (ignore-errors (realign-cursor)))

  (defadvice evil-ace-jump-line-mode
    (after realign activate preactivate compile)
    (ignore-errors (realign-cursor)))

  (add-hook 'before-make-frame-hook
    (lambda ()
      (when (< (display-color-cells) 256)
        (set-face-foreground 'ace-jump-face-background "white")
        (set-face-background 'ace-jump-face-background "black")
        (set-face-foreground 'ace-jump-face-foreground "black")
        (set-face-background 'ace-jump-face-foreground "white")))))

(key-chord-define evil-insert-state-map "jk" #'evil-ace-jump-word-mode)
(key-chord-define evil-insert-state-map "jc" #'evil-ace-jump-char-mode)
(key-chord-define evil-insert-state-map "jl" #'evil-ace-jump-line-mode)

(key-chord-define evil-emacs-state-map "jk" #'ace-jump-word-mode)
(key-chord-define evil-emacs-state-map "jc" #'ace-jump-char-mode)
(key-chord-define evil-emacs-state-map "jl" #'ace-jump-line-mode)

(evilem-default-keybindings "SPC")
(evilem-define (kbd "SPC s f") 'evil-forward-sexp)
(evilem-define (kbd "SPC s b") 'evil-backward-sexp)
(evilem-define (kbd "SPC s d") 'evil-down-sexp)
(evilem-define (kbd "SPC s D") 'evil-backward-down-sexp)
(evilem-define (kbd "SPC s e") 'evil-up-sexp)
(evilem-define (kbd "SPC s U") 'evil-backward-up-sexp)
(evilem-define (kbd "SPC s n") 'evil-next-sexp)
(evilem-define (kbd "SPC s p") 'evil-previous-sexp)

(evilem-define (kbd "SPC L") 'evil-forward-symbol)
(evilem-define (kbd "SPC H") 'evil-backward-symbol)

(define-key evil-insert-state-map (kbd "M-SPC") (lookup-key evil-motion-state-map (kbd "SPC")))
(define-key evil-insert-state-map (kbd "C-M-SPC") (lookup-key evil-motion-state-map (kbd "SPC")))
(define-key evil-emacs-state-map (kbd "M-SPC") (lookup-key evil-motion-state-map (kbd "SPC")))
(define-key evil-emacs-state-map (kbd "C-M-SPC") (lookup-key evil-motion-state-map (kbd "SPC")))

(provide 'config-ace-easymotion)
