(require 'ace-jump-mode)

(eval-when-compile
  (progn
    (require 'ace-jump-mode)
    (require 'noflet)
    (require 'evil)
    (require 'key-chord)))

(with-eval-after-load 'ace-jump-mode
  ;; use letters, numbers and capitals in that order
  (setq ace-jump-mode-move-keys
    (nconc
      (loop for i from ?a to ?z collect i)
      (loop for i from ?0 to ?9 collect i)
      (loop for i from ?A to ?Z collect i))
    ace-jump-mode-case-fold nil)

  ;; enable and use the more powerful ACE jump back feature
  ;; see github.com/winterTTr/ace-jump-mode/wiki/AceJump-FAQ
  (setq ace-jump-mode-scope 'global)

  (defun ace-jump-char-N-lines (&optional n)
    (interactive "p")
    (let* ((N (or n (window-body-height)))
            (query-char (read-char "Query Char:"))
            (start (save-excursion
                     (forward-line (- N))
                     (point)))
            (stop (save-excursion
                    (forward-line (1+ N))
                    (point))))
      (unwind-protect
        (condition-case err
          (progn
            (narrow-to-region start stop)
            (evil-ace-jump-char-mode query-char))
          (error
            (message (error-message-string err))))
        (widen))))

  (defun realign-cursor ()
    (interactive)
    (save-excursion
      (if (> (rest (nth 6 (posn-at-point)))
            (/ (window-body-height) 2))
        (progn
          (call-interactively 'previous-line)
          (call-interactively 'next-line))
        (progn
          (call-interactively 'next-line)
          (call-interactively 'previous-line)))))

  (defadvice evil-ace-jump-word-mode (after cleanup activate)
    (ignore-errors (call-interactively 'realign-cursor)))

  (defadvice evil-ace-jump-char-mode (after cleanup activate)
    (ignore-errors (call-interactively 'realign-cursor)))

  (defadvice evil-ace-jump-line-mode (after realign activate)
    (ignore-errors (call-interactively 'realign-cursor)))

  ;; jump to approximately the same column
  (defadvice evil-ace-jump-line-mode (around restore-pos activate)
    (let ((cursor (first (nth 6 (posn-at-point))))
           (line (- (line-end-position)
                   (line-beginning-position))))
      ad-do-it
      (call-interactively
        (lambda ()
          ;; (back-to-indentation)
          (interactive)
          (forward-char
            (round (* (/ cursor (max (float line) 1))
                     (or (- (line-end-position)
                           (line-beginning-position)) 1))))))))

  (ace-jump-mode-enable-mark-sync)
  (when (and
          (< (display-color-cells) 256)
          (not (display-graphic-p)))
    (set-face-foreground 'ace-jump-face-background "white")
    (set-face-background 'ace-jump-face-background "black")
    (set-face-foreground 'ace-jump-face-foreground "black")
    (set-face-background 'ace-jump-face-foreground "white"))

  (key-chord-define evil-insert-state-map "jl" 'evil-ace-jump-line-mode)
  (key-chord-define evil-insert-state-map "jk" 'evil-ace-jump-word-mode)
  (key-chord-define evil-emacs-state-map "jk" 'ace-jump-word-mode)
  (key-chord-define evil-emacs-state-map "jc" 'ace-jump-char-N-lines)
  (key-chord-define evil-emacs-state-map "jl" 'ace-jump-line-mode)

  (key-chord-define evil-normal-state-map " l" 'evil-ace-jump-line-mode)
  (key-chord-define evil-normal-state-map " n" 'ace-jump-char-N-lines)
  (key-chord-define evil-normal-state-map " c" 'evil-ace-jump-char-mode)
  (key-chord-define evil-normal-state-map " t" 'evil-ace-jump-char-to-mode)

  (key-chord-define evil-insert-state-map "jc" 'ace-jump-char-N-lines))

(require 'evil-easymotion)
(eval-when-compile (require 'evil-easymotion))
(evilem-default-keybindings "SPC")
(evilem-define (kbd "SPC s f") 'evil-forward-sexp)
(evilem-define (kbd "SPC s b") 'evil-backward-sexp)
(evilem-define (kbd "SPC s d") 'evil-down-sexp)
(evilem-define (kbd "SPC s D") 'evil-backward-down-sexp)
(evilem-define (kbd "SPC s e") 'evil-up-sexp)
(evilem-define (kbd "SPC s U") 'evil-backward-up-sexp)
(evilem-define (kbd "SPC s n") 'evil-next-sexp)
(evilem-define (kbd "SPC s p") 'evil-previous-sexp)

;; currently broken
(evilem-define (kbd "SPC L") 'evil-forward-symbol)
(evilem-define (kbd "SPC H") 'evil-backward-symbol)
(provide 'config-ace-easymotion)
