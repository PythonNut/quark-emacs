(eval-when-compile
  (with-demoted-errors
    (require 'cl-lib)
    (require 'evil)
    (require 'evil-indent-textobject)
    (require 'smartparens)
    (require 'ace-jump-mode)))

;;; === Evil motion section ===

(defun evil-smart-visual-line ()
  (unless (fboundp #'evil-visual-line-hydra/body)
    (require 'hydra)
    (defhydra evil-visual-line-hydra
      (:pre (setq hydra-is-helpful nil)
        :post (setq hydra-is-helpful t))
      ("j" evil-next-visual-line)
      ("k" evil-previous-visual-line)))
  (evil-visual-line-hydra/body))

(evil-define-motion evil-smart-next-visual-line (count)
  (evil-next-visual-line count)
  (evil-smart-visual-line))

(evil-define-motion evil-smart-previous-visual-line (count)
  (evil-previous-visual-line count)
  (evil-smart-visual-line))

(define-key evil-motion-state-map "gj" #'evil-smart-next-visual-line)
(define-key evil-motion-state-map "gk" #'evil-smart-previous-visual-line)

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
  (while (and (string-empty-p
                (evil-indent--current-indentation))
           (not (eobp)))
    (forward-line))
  (cl-flet* ((empty-line-p ()
               (string-match "^[[:space:]]*$"
                 (buffer-substring-no-properties
                   (line-beginning-position)
                   (line-end-position))))
              (line-indent-ok (indent)
                (or (<= (length indent)
                      (length (evil-indent--current-indentation)))
                  (empty-line-p))))
    (let ((indent (evil-indent--current-indentation)) start begin end)
      ;; now skip ahead to the Nth block with this indentation
      (dotimes (index (or last-prefix-arg 0))
        (while (and (line-indent-ok) (not (eobp))) (forward-line))
        (while (or (line-indent-ok indent) (eobp)) (forward-line)))
      (save-excursion
        (setq start (goto-char (or point (point))))
        (while (and (line-indent-ok indent) (not (bobp)))
          (setq begin (point))
          (forward-line -1))
        (goto-char start)
        (while (and (line-indent-ok indent) (not (eobp)))
          (setq end (point))
          (forward-line))
        (goto-char end)
        (while (empty-line-p)
          (forward-line -1)
          (setq end (point)))
        (list begin end)))))

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

(evil-define-text-object evil-indent-a-block-end (count &optional beg end type)
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
        (point-at-eol))
      'line)))

(define-key evil-inner-text-objects-map "c" #'evil-indent-i-block)
(define-key evil-outer-text-objects-map "c" #'evil-indent-a-block)
(define-key evil-outer-text-objects-map "C" #'evil-indent-a-block-end)

(defun evil-make-char-range ()
  (save-excursion
    (sort (list
            (progn
              (deactivate-mark)
              (evil-ace-jump-char-mode)
              (point))
            (progn
              (deactivate-mark)
              (evil-ace-jump-char-mode)
              (point)))
      '<)))

(evil-define-text-object evil-i-char-range (count &optional beg end type)
  "Text object describing the range between two arbitrary points"
  :type inclusive
  (let ((range (evil-make-char-range)))
    (evil-range (first range) (second range) 'inclusive)))

(evil-define-text-object evil-a-char-range (count &optional beg end type)
  "Text object describing the range between two arbitrary points"
  :type exclusive
  (let ((range (evil-make-char-range)))
    (evil-range (1- (first range)) (1+ (second range)) 'inclusive)))

(define-key evil-inner-text-objects-map "R" #'evil-i-char-range)
(define-key evil-outer-text-objects-map "R" #'evil-a-char-range)

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

(evil-define-text-object evil-i-line-range (count &optional beg end type)
  "Text object describing the block with the same indentation as the current line."
  :type line
  (save-excursion
    (let ((beg (evil-ace-jump-line-and-revert))
           (end (evil-ace-jump-line-and-revert)))
      (if (> beg end)
        (evil-range beg end #'line)
        (evil-range end beg #'line)))))

(define-key evil-inner-text-objects-map "r" #'evil-i-line-range)

(define-key evil-inner-text-objects-map "." #'evil-inner-sentence)
(define-key evil-outer-text-objects-map "." #'evil-a-sentence)

(evil-define-text-object evil-i-entire-buffer (count &optional ben end type)
  "Text object describing the entire buffer excluding empty lines at the end"
  :type line
  (evil-range (point-min) (save-excursion
                            (goto-char (point-max))
                            (skip-chars-backward " \n\t")
                            (point)) 'line))

(evil-define-text-object evil-an-entire-buffer (count &optional beg end type)
  "Text object describing the entire buffer"
  :type line
  (evil-range (point-min) (point-max) 'line))

(define-key evil-inner-text-objects-map "e" #'evil-i-entire-buffer)
(define-key evil-outer-text-objects-map "e" #'evil-an-entire-buffer)

(evil-define-text-object evil-inner-last-paste (count &optional beg end type)
  :type char
  (evil-range (evil-get-marker ?\[) (evil-get-marker ?\]) 'char))

(define-key evil-inner-text-objects-map "P" #'evil-inner-last-paste)

;;; === evil operators ===
(autoload #'evilnc-comment-operator "evil-nerd-commenter")
(autoload #'evilnc-hotkey-comment-operator "evil-nerd-commenter")
(define-key evil-operator-state-map "gc" #'evilnc-comment-operator)
(define-key evil-normal-state-map "gc" #'evilnc-comment-operator)

(evil-define-operator evil-macro-on-all-lines (beg end &optional arg)
  (evil-with-state
    (evil-normal-state)
    (goto-char end)
    (evil-visual-state)
    (goto-char beg)
    (evil-ex-normal (region-beginning) (region-end)
      (concat "@"
        (single-key-description
          (read-char "What macro?"))))))

(define-key evil-operator-state-map "g@" #'evil-macro-on-all-lines)
(define-key evil-normal-state-map "g@" #'evil-macro-on-all-lines)

(defun narrow-to-region-indirect (start end)
  "Restrict editing in this buffer to the current region, indirectly."
  (interactive "r")
  (deactivate-mark)
  (let ((buf (clone-indirect-buffer nil nil)))
    (with-current-buffer buf
      (narrow-to-region start end))
    (switch-to-buffer buf)))

(global-set-key (kbd "C-x n i") #'narrow-to-region-indirect)

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

(define-key evil-operator-state-map "gn" #'evil-narrow-region)
(define-key evil-normal-state-map "gn" #'evil-narrow-region)
(define-key evil-operator-state-map "gN" #'evil-narrow-indirect)
(define-key evil-normal-state-map "gN" #'evil-narrow-indirect)

(evil-define-operator evil-eval-region (beg end type)
  (save-excursion
    (evil-with-state
      (evil-normal-state)
      (goto-char beg)
      (evil-visual-state)
      (goto-char end)
      (eval-region beg end))))

(define-key evil-operator-state-map "gV" #'evil-eval-region)
(define-key evil-normal-state-map "gV" #'evil-eval-region)

(evil-define-operator evil-align-regexp (beg end type)
  (save-excursion
    (evil-with-state
      (evil-normal-state)
      (goto-char beg)
      (evil-visual-state)
      (goto-char end)
      (call-interactively #'align-regexp))))

(define-key evil-operator-state-map "g|" #'evil-align-regexp)
(define-key evil-normal-state-map "g|" #'evil-align-regexp)

(provide 'config-evil-textobjects)
