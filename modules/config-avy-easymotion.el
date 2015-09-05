;; -*- lexical-binding: t -*-

(eval-when-compile
  (with-demoted-errors "Load error: %s"
    (require 'avy)
    (require 'evil-states)
    (require 'key-chord)
    (require 'evil-easymotion)))

(key-chord-define evil-insert-state-map "jk" #'avy-goto-word-1)
(key-chord-define evil-insert-state-map "jc" #'avy-goto-char)
(key-chord-define evil-insert-state-map "jl" #'avy-goto-line)

(key-chord-define evil-emacs-state-map "jk" #'avy-goto-word-1)
(key-chord-define evil-emacs-state-map "jc" #'avy-goto-char)
(key-chord-define evil-emacs-state-map "jl" #'avy-goto-line)

(global-set-key (kbd "<remap> <goto-line>") #'avy-goto-line)

;; `C-u a` jumps to `a`.
(defun nadvice/self-insert-command (old-fun &optional arg)
  (interactive "P")
  (cond
    ((consp arg)
      (avy-goto-char last-command-event))
    ((eq '- arg)
      (message "Negative argument not implemented"))
    ((or (numberp arg) (not arg))
      (funcall old-fun (or arg 1)))))

(advice-add 'self-insert-command :around #'nadvice/self-insert-command)

(with-eval-after-load 'avy
  (setq
    avy-background t
    avy-style 'de-bruijn
    avy-keys (string-to-list "jfkdls;aurieowncpqmxzb"))
  (set-face-foreground 'avy-background-face "#586e75")

  (set-face-attribute 'avy-lead-face nil
    :weight 'normal
    :background nil
    :foreground "#b58900")
  (set-face-attribute 'avy-lead-face-0 nil
    :weight 'extra-bold
    :background nil
    :foreground "#dc322f")
  (set-face-attribute 'avy-lead-face-1 nil
    :background nil
    :foreground "#839493"))

(with-eval-after-load 'evil-easymotion
  (setq
    evilem-style 'de-bruijn))

(eval-and-compile (require 'evil-easymotion))

(evilem-default-keybindings "SPC")

(define-key evil-normal-state-map (kbd "SPC l") #'avy-goto-line)
(define-key evil-motion-state-map (kbd "SPC l") #'avy-goto-line)
(define-key evil-normal-state-map (kbd "SPC c") #'avy-goto-char)
(define-key evil-motion-state-map (kbd "SPC c") #'avy-goto-char)

(evilem-define (kbd "SPC g s f") 'evil-forward-sexp)
(evilem-define (kbd "SPC g s b") 'evil-backward-sexp)
(evilem-define (kbd "SPC g s d") 'evil-down-sexp)
(evilem-define (kbd "SPC g s D") 'evil-backward-down-sexp)
(evilem-define (kbd "SPC g s e") 'evil-up-sexp)
(evilem-define (kbd "SPC g s U") 'evil-backward-up-sexp)
(evilem-define (kbd "SPC g s n") 'evil-next-sexp)
(evilem-define (kbd "SPC g s p") 'evil-previous-sexp)

(evilem-define (kbd "SPC s") 'evil-snipe-repeat
  (lambda ()
    (save-excursion
      (ignore-errors
        (call-interactively #'evil-snipe-s))))
  nil
  ((evil-snipe-enable-highlight)
    (evil-snipe-enable-incremental-highlight)))

(evilem-define (kbd "SPC S") 'evil-snipe-repeat-reverse
  (lambda ()
    (save-excursion
      (ignore-errors
        (call-interactively #'evil-snipe-S))))
  nil
  ((evil-snipe-enable-highlight)
    (evil-snipe-enable-incremental-highlight)))

(provide 'config-avy-easymotion)
