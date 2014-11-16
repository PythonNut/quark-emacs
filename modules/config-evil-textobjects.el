(eval-when-compile
  (progn
    (require 'cl)
    (require 'cl-lib)
    (require 'evil)
    (require 'evil-leader)
    (require 'evil-indent-textobject)
    (require 'evil-nerd-commenter)
    (require 'smartparens)
    (require 'ace-jump-mode)))

;;; === Evil motion section ===

(evil-define-motion evil-forward-symbol (count)
  (sp-forward-symbol count))

(evil-define-motion evil-backward-symbol (count)
  (sp-backward-symbol count))

(define-key evil-motion-state-map "L" 'evil-forward-symbol)
(define-key evil-motion-state-map "H" 'evil-backward-symbol)

;;; === Evil text object section ===
;; evil block indentation textobject for Python
(defun evil-indent--current-indentation ()
  "Return the indentation of the current line. Moves point."
  (buffer-substring-no-properties (point-at-bol)
    (progn (back-to-indentation)
      (point))))

(defun evil-indent--block-range (&optional point)
  "Return the point at the begin and end of the text block "
  ;; there are faster ways to mark the entire file
  ;; so assume the user wants a block and skip to there
  (loop while (and (/= (point) (point-max))
                (= 0
                  (length (evil-indent--current-indentation))))
    do (progn
         (forward-line 1)))

  (cl-flet ((before-end () (/= (point) (point-max)))
             (empty-line-p ()
               (string-match "^[[:space:]]*$"
                 (buffer-substring-no-properties
                   (line-beginning-position)
                   (line-end-position)))))
    (let ((indent (evil-indent--current-indentation)))
      (cl-flet ((line-indent-ok ()
                  (or (>=
                        (length (evil-indent--current-indentation))
                        (length indent))
                    (empty-line-p))))
        ;; now skip ahead to the Nth block with this indentation
        (let ((index (or last-prefix-arg 0)))
          (while (> index 1)
            (while (and (before-end) (line-indent-ok))
              (forward-line 1))
            (while (and (before-end) (not (line-indent-ok)))
              (forward-line 1))
            (setq index (1- index))))
        (save-excursion
          (when point (goto-char point))
          (let ((start (point)) begin end)
            (while (and (/= (point) (point-min)) (line-indent-ok))
              (setq begin (point))
              (forward-line -1))
            (goto-char start)
            (while (and (before-end) (line-indent-ok))
              (setq end (point))
              (forward-line 1))
            (goto-char end)
            (while (empty-line-p)
              (forward-line -1)
              (setq end (point)))
            (list begin end)))))))

(evil-define-text-object evil-indent-i-block (&optional count beg end type)
  "Text object describing the block with the same indentation as the current line."
  (let ((range (evil-indent--block-range)))
    (evil-range (first range) (second range) 'line)))

(evil-define-text-object evil-indent-a-block (&optional count beg end type)
  "Text object describing the block with the same indentation as the current line and the line above."
  :type line
  (let ((range (evil-indent--block-range)))
    (evil-range (save-excursion
                  (goto-char (first (evil-indent--block-range)))
                  (forward-line -1)
                  (point-at-bol))
      (second range) 'line)))

(evil-define-text-object evil-indent-a-block-end (&optional count beg end type)
  "Text object describing the block with the same indentation as the current line and the lines above and below."
  :type line
  (let ((range (evil-indent--block-range)))
    (evil-range (save-excursion
                  (goto-char (first range))
                  (forward-line -1)
                  (point-at-bol))
      (save-excursion
        (goto-char (second range))
        (forward-line 1)
        (point-at-eol)) 'line)))

(define-key evil-inner-text-objects-map "c" 'evil-indent-i-block)
(define-key evil-outer-text-objects-map "c" 'evil-indent-a-block)
(define-key evil-outer-text-objects-map "C" 'evil-indent-a-block-end)

;; evil NERD commenter, commenting awesomeness!
(require 'evil-nerd-commenter)

(global-set-key (kbd "M-;") 'evilnc-comment-or-uncomment-lines)
(global-set-key (kbd "C-M-;") 'evilnc-copy-and-comment-lines)

(evil-leader/set-key
  "ci" 'evilnc-comment-or-uncomment-lines
  "cl" 'evilnc-comment-or-uncomment-to-the-line
  "cc" 'evilnc-copy-and-comment-lines
  "cp" 'evilnc-comment-or-uncomment-paragraphs)

(defun point-in-comment-p (&optional pt)
  "Determine if the point is inside a comment"
  (interactive)
  (let ((syn (syntax-ppss (or pt (point)))))
    (and (nth 8 syn)
      (not (nth 3 syn)))))

(defun evil-indent--comment-range (&optional point)
  "Return the point at the begin and end of the text block with the same indentation.
   If `point' is supplied and non-nil it will return the begin and end of the block surrounding point."

  (when point
    (goto-char point))

  (cl-flet ((before-end () (/= (point) (point-max)))
             (after-beg () (/= (point) (point-min)))
             (comment-anywhere ()
               (or
                 (point-in-comment-p (point-at-bol))
                 (point-in-comment-p (point-at-eol)))))

    (while (and (before-end) (not (comment-anywhere)))
      (forward-line 1))
    (end-of-line)

    (let ((index (or last-prefix-arg 0)))
      (while (> index 1)
        (while (and (before-end) (comment-anywhere))
          (forward-line 1))
        (while (and (before-end) (not (comment-anywhere)))
          (forward-line 1))
        (setq index (1- index))))

    (while (and (before-end) (point-in-comment-p))
      (backward-char))

    (save-excursion
      (if (string-match "^[[:space:]]*$"
            (buffer-substring-no-properties
              (line-beginning-position)
              (save-excursion
                (evil-backward-WORD-end)
                (point))))
        (let ((start (point)) begin end)
          (while (and (/= (point) (point-min))
                   (or (save-excursion
                         (progn
                           (back-to-indentation)
                           (evil-forward-WORD-begin)
                           (point-in-comment-p)))
                     (point-in-comment-p (line-beginning-position))))
            (setq begin (point-at-bol))
            (forward-line -1))

          (goto-char start)

          (while (and (before-end)
                   (or (save-excursion
                         (progn
                           (back-to-indentation)
                           (evil-forward-WORD-begin)
                           (point-in-comment-p)))
                     (point-in-comment-p (line-beginning-position))))
            (setq end (point-at-eol))
            (forward-line 1))
          (list 'line begin end))
        ;; back up the cursor here
        (list 'exclusive
          (progn
            (while (looking-back "[[:space:]]")
              (backward-char))
            (point))
          (line-end-position))))

    (evil-define-text-object evil-indent-i-comment (&optional count beg end type)
      "Text object describing the block with the same indentation as
the current line."
      (let ((range (evil-indent--comment-range)))
        (evil-contract-range (evil-range (second range) (third range) (first range)))))))

(define-key evil-inner-text-objects-map "C" 'evil-indent-i-comment)

(defun evil-make-arbitrary-char-range ()
  (save-excursion
    (sort (list
            (progn
              (deactivate-mark)
              (call-interactively 'evil-ace-jump-char-mode)
              (point))
            (progn
              (deactivate-mark)
              (call-interactively 'evil-ace-jump-char-mode)
              (point)))
      '<)))

(evil-define-text-object evil-i-arbitrary-char-range (&optional _c _b _e _t)
  "Text object describing the range between two arbitrary points"
  :type inclusive
  (let ((range (evil-make-arbitrary-char-range)))
    (evil-range (first range) (second range) 'inclusive)))

(evil-define-text-object evil-a-arbitrary-char-range (&optional _c _b _e _t)
  "Text object describing the range between two arbitrary points"
  :type exclusive
  (let ((range (evil-make-arbitrary-char-range)))
    (evil-range (1- (first range)) (1+ (second range)) 'inclusive)))

(define-key evil-inner-text-objects-map "R" 'evil-i-arbitrary-char-range)
(define-key evil-outer-text-objects-map "R" 'evil-a-arbitrary-char-range)

(defun evil-ace-jump-line-and-revert ()
  (interactive)
  (let ((top (save-excursion
               (evil-window-top)
               (line-number-at-pos))))
    (prog1
      (progn
        (deactivate-mark)
        (evil-ace-jump-line-mode)
        (point))
      (scroll-up-line (- top (save-excursion
                               (evil-window-top)
                               (line-number-at-pos)))))))

(defun evil-make-arbitrary-line-range (&rest _ignored)
  (save-excursion
    (let ((beg (evil-ace-jump-line-and-revert))
           (end (evil-ace-jump-line-and-revert)))
      (evil-range (min beg end) (max beg end)))))

(evil-define-text-object evil-i-arbitrary-line-range (&optional _c _b _e _t)
  "Text object describing the block with the same indentation as the current line."
  :type line
  (save-excursion
    (let ((beg (evil-ace-jump-line-and-revert))
           (end (evil-ace-jump-line-and-revert)))
      (if (> beg end)
        (evil-range beg end 'line)
        (evil-range end beg 'line)))))

(evil-define-text-object evil-a-arbitrary-line-range (&optional _c _b _e _t)
  :type line
  "Text object describing the block with the same indentation as the current line."
  (save-excursion
    (let ((beg (progn
                 (deactivate-mark)
                 (evil-ace-jump-line-mode)
                 (point)))
           (end (progn
                  (deactivate-mark)
                  (evil-ace-jump-line-mode)
                  (point))))
      (evil-range
        (progn
          (goto-char (min beg end))
          (loop do (forward-line -1) while
            (string-match "^[[:space:]]*$"
              (buffer-substring-no-properties
                (line-beginning-position)
                (line-end-position))))
          (forward-line 1)
          (point))
        (progn
          (goto-char (max beg end))
          (loop do (forward-line 1) while
            (string-match "^[[:space:]]*$"
              (buffer-substring-no-properties
                (line-beginning-position)
                (line-end-position))))
          (forward-line -1)
          (point)) 'line))))

(define-key evil-inner-text-objects-map "r" 'evil-i-arbitrary-line-range)
(define-key evil-outer-text-objects-map "r" 'evil-a-arbitrary-line-range)

;; (require 'evil-args)
(autoload 'evil-inner-arg "evil-args")
(autoload 'evil-outer-arg "evil-args")
(autoload 'evil-forward-arg "evil-args")
(autoload 'evil-backward-arg "evil-args")

(define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
(define-key evil-outer-text-objects-map "a" 'evil-outer-arg)

;; (define-key evil-normal-state-map "L" 'evil-forward-arg)
;; (define-key evil-normal-state-map "H" 'evil-backward-arg)
;; (define-key evil-motion-state-map "L" 'evil-forward-arg)
;; (define-key evil-motion-state-map "H" 'evil-backward-arg)

(define-key evil-normal-state-map "K" 'evil-jump-out-args)

(define-key evil-inner-text-objects-map "s" 'evil-inner-symbol)
(define-key evil-outer-text-objects-map "s" 'evil-a-symbol)

(define-key evil-inner-text-objects-map "." 'evil-inner-sentence)
(define-key evil-outer-text-objects-map "." 'evil-a-sentence)

;;; === evil operators ===
(autoload 'evilnc-comment-operator "evil-nerd-commenter")
(autoload 'evilnc-hotkey-comment-operator "evil-nerd-commenter")
(define-key evil-operator-state-map "gc" 'evilnc-comment-operator)
(define-key evil-normal-state-map "gc" 'evilnc-comment-operator)

(evil-define-operator evil-macro-on-all-lines (beg end &optional arg)
  (evil-normal-state)
  (goto-char end)
  (evil-visual-state)
  (goto-char beg)
  (evil-ex-normal (region-beginning) (region-end)
    (concat "@"
      (single-key-description
        (read-char "What macro?")))))

(define-key evil-operator-state-map "gl" 'evil-macro-on-all-lines)
(define-key evil-normal-state-map "gl" 'evil-macro-on-all-lines)

(require 'wide-n)

(defun narrow-to-region-indirect (start end)
  "Restrict editing in this buffer to the current region, indirectly."
  (interactive "r")
  (deactivate-mark)
  (let ((buf (clone-indirect-buffer nil nil)))
    (with-current-buffer buf
      (narrow-to-region start end))
    (switch-to-buffer buf)))

(global-set-key (kbd "C-x n i") 'narrow-to-region-indirect)

(evil-define-operator evil-narrow-indirect (beg end type)
  "Indirectly narrow the region from BEG to END."
  (interactive "<R>")
  (evil-normal-state)
  (narrow-to-region-indirect beg end))

(evil-define-operator evil-narrow-region (beg end type)
  "Indirectly narrow the region from BEG to END."
  (interactive "<R>")
  (evil-normal-state)
  (narrow-to-region beg end))

(define-key evil-operator-state-map "gn" 'evil-narrow-region)
(define-key evil-normal-state-map "gn" 'evil-narrow-region)
(define-key evil-operator-state-map "gN" 'evil-narrow-indirect)
(define-key evil-normal-state-map "gN" 'evil-narrow-indirect)

(evil-define-operator evil-eval-region (beg end type)
  (save-excursion
    (evil-normal-state)
    (goto-char beg)
    (evil-visual-state)
    (goto-char end)
    (eval-region beg end)
    (evil-normal-state)))

(define-key evil-operator-state-map "gV" 'evil-eval-region)
(define-key evil-normal-state-map "gV" 'evil-eval-region)

(evil-define-operator evil-align-regexp (beg end type)
  (save-excursion
    (evil-normal-state)
    (goto-char beg)
    (evil-visual-state)
    (goto-char end)
    (call-interactively 'align-regexp)
    (evil-normal-state)))

(define-key evil-operator-state-map "g|" 'evil-align-regexp)
(define-key evil-normal-state-map "g|" 'evil-align-regexp)
