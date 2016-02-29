;; -*- lexical-binding: t -*-

(eval-when-compile
  (with-demoted-errors "Load error: %s"
    (require 'key-chord)
    (require 'evil)
    (require 'evil-easymotion)))

(key-chord-define evil-insert-state-map "jk" #'avy-goto-char-timer)
(key-chord-define evil-insert-state-map "jw" #'evil-avy-goto-word-1)
(key-chord-define evil-insert-state-map "jl" #'evil-avy-goto-line)

(key-chord-define evil-emacs-state-map "jk" #'avy-goto-char-timer)
(key-chord-define evil-emacs-state-map "jw" #'evil-avy-goto-word-1)
(key-chord-define evil-emacs-state-map "jl" #'evil-avy-goto-line)

(global-set-key (kbd "<remap> <goto-line>") #'evil-avy-goto-line)

(defun nadvice/self-insert-command (old-fun &optional arg)
  (interactive "P")
  (cond
   ;; `C-u a` jumps to `a`.
   ((consp arg)
    (avy-goto-char last-command-event))

   ;; `C-- a` jumps to `a` at the beginning of a (sub)word
   ((eq '- arg)
    (avy-goto-subword-1 last-command-event))

   ((or (numberp arg) (not arg))
    (funcall old-fun (or arg 1)))))

(advice-add 'self-insert-command :around #'nadvice/self-insert-command)

(with-eval-after-load 'avy
  (eval-when-compile
    (with-demoted-errors "Load error: %s"
      (require 'avy)))

  (setq avy-background t
        avy-style 'de-bruijn
        avy-timeout-seconds 0.3
        avy-keys (eval-when-compile (string-to-list "jfkdlsaurieowncpqmxzb")))

  (set-face-foreground 'avy-background-face "#586e75")

  (set-face-attribute 'avy-lead-face nil
                      :weight 'normal
                      :background nil
                      :foreground "#b58900"
                      :inherit nil)
  (set-face-attribute 'avy-lead-face-0 nil
                      :weight 'extra-bold
                      :background nil
                      :foreground "#dc322f"
                      :inherit nil)
  (set-face-attribute 'avy-lead-face-1 nil
                      :background nil
                      :foreground "#839493"
                      :inherit nil))

(with-eval-after-load 'ace-window
  (eval-when-compile
    (with-demoted-errors "Load error: %s"
      (require 'ace-window)))

  (setq aw-keys (eval-when-compile (string-to-list "jfkdlsautnvmircieowpq"))
        aw-ignore-current t
        aw-swap-invert t))

;; bind command to switch to minibuffer
(defun switch-window-dwim (arg)
  "switch to minibuffer window (if active)"
  (interactive "P")
  (let ((num-windows (length (mapcar #'window-buffer (window-list)))))
    (cond ((= num-windows 1)
           (call-interactively #'split-window-right))
          ((minibufferp)
           (other-window (or arg 1)))
          ((active-minibuffer-window)
           (select-window (active-minibuffer-window)))
          ((or (> (length (visible-frame-list)) 1)
               (> num-windows 3)
               (numberp arg))
           (ace-window arg))
          (t
           (other-window (or arg 1))))))

(global-set-key (kbd "C-'") #'switch-window-dwim)
(global-set-key (kbd "C-c '") #'switch-window-dwim)

(require 'evil-easymotion)
(evilem-default-keybindings "SPC")

(define-key evil-normal-state-map (kbd "SPC l") #'evil-avy-goto-line)
(define-key evil-motion-state-map (kbd "SPC l") #'evil-avy-goto-line)
(define-key evil-normal-state-map (kbd "SPC c") #'avy-goto-char-timer)
(define-key evil-motion-state-map (kbd "SPC c") #'avy-goto-char-timer)

(evilem-define (kbd "SPC g s f") #'on-parens-forward-sexp-end)
(evilem-define (kbd "SPC g s b") #'on-parens-backward-sexp)
(evilem-define (kbd "SPC g s d") #'on-parens-down-sexp)
(evilem-define (kbd "SPC g s D") #'on-parens-down-sexp-end)
(evilem-define (kbd "SPC g s u") #'on-parens-up-sexp-end)
(evilem-define (kbd "SPC g s U") #'on-parens-up-sexp)
(evilem-define (kbd "SPC g s n") #'on-parens-forward-sexp)
(evilem-define (kbd "SPC g s p") #'on-parens-backward-sexp-end)

(evilem-define (kbd "SPC s") #'evil-snipe-repeat
               :pre-hook (save-excursion
                           (ignore-errors
                             (call-interactively #'evil-snipe-s)))
               :bind ((evil-snipe-enable-highlight)
                      (evil-snipe-enable-incremental-highlight)))

(evilem-define (kbd "SPC S") #'evil-snipe-repeat-reverse
               :pre-hook (save-excursion
                           (ignore-errors
                             (call-interactively #'evil-snipe-S)))
               :bind ((evil-snipe-enable-highlight)
                      (evil-snipe-enable-incremental-highlight)))

(provide 'config-avy-easymotion)
