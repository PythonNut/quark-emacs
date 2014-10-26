(require 'noflet)
(require 'ace-jump-mode)
(eval-when-compile (require 'cl))

(eval-after-load 'ace-jump-mode
  '(progn
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

     (defun realign-cursor ()
       (interactive)
       (save-excursion
	 (if (> (rest (nth 6 (posn-at-point)))
	       (/ (window-body-height) 2))
	   (progn (previous-line) (next-line))
	   (progn (next-line) (previous-line)))))

     (defadvice evil-ace-jump-word-mode (after cleanup activate)
       (ignore-errors (call-interactively 'realign-cursor)))

     (defadvice evil-ace-jump-char-mode (after cleanup activate)
       (ignore-errors (call-interactively 'realign-cursor)))

     (defadvice evil-ace-jump-line-mode (after realign activate)
       (ignore-errors (call-interactively 'realign-cursor)))

     ;; jump to appromately the same column
     (defadvice evil-ace-jump-line-mode (around restore-pos activate)
       (let ((cursor (first (nth 6 (posn-at-point))))
	      (line (- (line-end-position)
		      (line-beginning-position))))
	 ad-do-it
	 (call-interactively
	   '(lambda ()
	      ;; (back-to-indentation)
	      (interactive)
	      (forward-char
		(round (* (/ cursor (max (float line) 1))
			 (or (- (line-end-position)
			       (line-beginning-position)) 1))))))))

     (ace-jump-mode-enable-mark-sync)
     (if (< (display-color-cells) 256)
       (progn
         (set-face-foreground 'ace-jump-face-background "white")
         (set-face-background 'ace-jump-face-background "black")
         (set-face-foreground 'ace-jump-face-foreground "black")
         (set-face-background 'ace-jump-face-foreground "white")))))

(key-chord-define evil-insert-state-map "jl" 'evil-ace-jump-line-mode)
(key-chord-define evil-insert-state-map "jk" 'evil-ace-jump-word-mode)
(key-chord-define evil-emacs-state-map "jk" 'ace-jump-word-mode)
(key-chord-define evil-emacs-state-map "jc" 'ace-jump-to-char-within-N-lines)
(key-chord-define evil-emacs-state-map "jl" 'ace-jump-line-mode)

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

(key-chord-define evil-insert-state-map "jc" 'ace-jump-to-char-within-N-lines)

(defmacro ace-generic (collector &rest follower)
  "ace jump to candidates of collector using follower."
  (declare (indent 1))
  `(noflet ((ace-jump-search-candidate (str va-list)
              (mapcar (lambda (x)
                        (make-aj-position
                          :offset (1- x)
                          :visual-area (car va-list)))
                ,collector)))
     (setq ace-jump-mode-end-hook
       (list (lambda ()
               (setq ace-jump-mode-end-hook)
               ,@follower)))
     (ace-jump-do "")))

(defmacro ace-motion-collect (func)
  `(let ((points ())
          (count 0)
          (smooth-scroll-margin 0)
          (scroll-margin 0)
          (win-start (window-start))
          (win-end (window-end)))
     (save-excursion
       (call-interactively (quote ,func))
       (while (when (and
                      (> (1+ (point)) win-start)
                      (< (1+ (point)) win-end)
                      (< count (length ace-jump-mode-move-keys)))
                (push (1+ (point)) points)
                (setq count (1+ count))
                (setq
                  last-command (quote ,func)
                  this-command (quote ,func))
                (call-interactively (quote ,func))
                t))
       (set-window-start (selected-window) win-start)
       (nreverse points))))

(defmacro ace-motion (func)
  `(evil-define-motion ,(make-symbol (concat "ace-" (symbol-name func))) (count)
     :type inclusive
     (evil-without-repeat
       (let ((pnt (point))
              (buf (current-buffer)))
         (evil-enclose-ace-jump-for-motion
           (ace-generic
             (ace-motion-collect ,func)
             ()))
         (when (and (equal buf (current-buffer))
                 (< (point) pnt))
           (setq evil-this-type 'exclusive))))))

(defmacro ease-motion (key motion)
  `(define-key evil-motion-state-map (kbd ,key) (ace-motion ,motion)))

(define-key evil-motion-state-map (kbd "SPC") 'nil)

(ease-motion "SPC w" evil-forward-word-begin)
(ease-motion "SPC W" evil-forward-WORD-begin)
(ease-motion "SPC e" evil-forward-word-end)
(ease-motion "SPC E" evil-forward-WORD-end)
(ease-motion "SPC b" evil-backward-word-begin)
(ease-motion "SPC B" evil-backward-WORD-begin)
(ease-motion "SPC ge" evil-backward-word-end)
(ease-motion "SPC gE" evil-backward-WORD-end)

(ease-motion "SPC h" evil-backward-char)
(ease-motion "SPC j" evil-next-line)
(ease-motion "SPC k" evil-previous-line)
(ease-motion "SPC l" evil-forward-char)

(ease-motion "SPC H" evil-backward-symbol)
(ease-motion "SPC L" evil-forward-symbol)

(ease-motion "SPC g j" evil-next-visual-line)
(ease-motion "SPC g k" evil-previous-visual-line)

(ease-motion "SPC s f" evil-forward-sexp)
(ease-motion "SPC s b" evil-backward-sexp)

(ease-motion "SPC [[" evil-backward-section-begin)
(ease-motion "SPC []" evil-backward-section-end)
(ease-motion "SPC ]]" evil-forward-section-begin)
(ease-motion "SPC ][" evil-forward-section-end)

(ease-motion "SPC L" evil-forward-symbol)
(ease-motion "SPC H" evil-backward-symbol)
(ease-motion "SPC (" evil-forward-sentence)
(ease-motion "SPC )" evil-backward-sentence)

(ease-motion "SPC n" evil-search-next)
(ease-motion "SPC N" evil-search-previous)

(ease-motion "SPC -" evil-previous-line-first-non-blank)
(ease-motion "SPC +" evil-next-line-first-non-blank)

(ease-motion "SPC M-h" evil-backward-sexp)
(ease-motion "SPC M-h" evil-backward-sexp)
(ease-motion "SPC M-j" evil-enter-sexp)
(ease-motion "SPC M-k" evil-backward-up-sexp)
(ease-motion "SPC M-l" evil-forward-sexp)

(ease-motion "SPC s f" evil-forward-sexp)
(ease-motion "SPC s b" evil-backward-sexp)
(ease-motion "SPC s d" evil-down-sexp)
(ease-motion "SPC s D" evil-backward-down-sexp)
(ease-motion "SPC s e" evil-up-sexp)
(ease-motion "SPC s U" evil-backward-up-sexp)
(ease-motion "SPC s n" evil-next-sexp)
(ease-motion "SPC s p" evil-previous-sexp)
