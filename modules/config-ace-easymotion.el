(eval-when-compile
  (with-demoted-errors
    (require 'avy)
    (require 'noflet)
    (require 'evil)
    (require 'key-chord)
    (require 'evil-easymotion)))

(key-chord-define evil-insert-state-map "jk" #'avy-goto-word-1)
(key-chord-define evil-insert-state-map "jc" #'avy-goto-char)
(key-chord-define evil-insert-state-map "jl" #'avy-goto-line)

(key-chord-define evil-emacs-state-map "jk" #'avy-goto-word-1)
(key-chord-define evil-emacs-state-map "jc" #'avy-goto-char)
(key-chord-define evil-emacs-state-map "jl" #'avy-goto-line)

(with-eval-after-load 'avy
  (setq
    avy-background t
    avy-style 'de-bruijn
    avy-keys (string-to-list "jfkdls;aurieowncpqmxzb75849392016"))
  (set-face-background 'avy-lead-face-0 nil)
  (set-face-background 'avy-lead-face nil)
  (set-face-foreground 'avy-lead-face-0 "#dc322f")
  (set-face-foreground 'avy-lead-face "#b58900")
  (set-face-attribute 'avy-lead-face nil :weight 'normal)
  (set-face-attribute 'avy-lead-face-0 nil :weight 'extra-bold))

(with-eval-after-load 'evil-easymotion
  (setq
    evilem-style 'de-bruijn))

(evilem-default-keybindings "SPC")

(define-key evil-normal-state-map (kbd "SPC l") #'avy-goto-line)
(define-key evil-motion-state-map (kbd "SPC l") #'avy-goto-line)
(define-key evil-normal-state-map (kbd "SPC c") #'avy-goto-char)
(define-key evil-motion-state-map (kbd "SPC c") #'avy-goto-char)

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
