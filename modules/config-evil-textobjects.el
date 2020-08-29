;; -*- lexical-binding: t -*-
(require 'cl-lib)
(eval-when-compile
  (use-package evil)
  (require 'evil))

;;; === Evil motion section ===
(use-package hydra
  :init
  (eval-when-compile
    (with-demoted-errors "Load error: %s"
      (require 'hydra)))

  (defhydra evil-visual-line-hydra
    (:pre (setq hydra-is-helpful nil)
          :post (setq hydra-is-helpful t))
    ("j" evil-next-visual-line)
    ("k" evil-previous-visual-line))

  (defhydra my/smart-evil-scroll-page-hydra
    (:pre (setq hydra-is-helpful nil)
          :post (setq hydra-is-helpful t))
    "Scroll by page"
    ("f" evil-scroll-page-down)
    ("b" evil-scroll-page-up)))

(evil-define-motion evil-smart-next-visual-line (count)
  (evil-next-visual-line (or count 1))
  (evil-visual-line-hydra/body))

(evil-define-motion evil-smart-previous-visual-line (count)
  (evil-previous-visual-line (or count 1))
  (evil-visual-line-hydra/body))

(evil-define-motion evil-smart-scroll-page-down (count)
  (evil-scroll-page-down (or count 1))
  (my/smart-evil-scroll-page-hydra/body))

(evil-define-motion evil-smart-scroll-page-up (count)
  (evil-scroll-page-up (or count 1))
  (my/smart-evil-scroll-page-hydra/body))

(define-key evil-motion-state-map "gj" #'evil-smart-next-visual-line)
(define-key evil-motion-state-map "gk" #'evil-smart-previous-visual-line)

(define-key evil-motion-state-map (kbd "C-f") #'evil-smart-scroll-page-down)
(define-key evil-motion-state-map (kbd "C-b") #'evil-smart-scroll-page-up)

;;; === Evil text object section ===
;; evil block indentation textobject for Python
(defun my/evil-indent--current-indentation ()
  "Return the indentation of the current line. Moves point."
  (buffer-substring-no-properties (point-at-bol)
    (progn (back-to-indentation)
           (point))))

(defun my/evil-indent--block-range (&optional count point)
  "Return the point at the begin and end of the text block "
  ;; there are faster ways to mark the entire file
  ;; so assume the user wants a block and skip to there
  (while (and (string-empty-p
               (my/evil-indent--current-indentation))
              (not (eobp)))
    (forward-line))
  (cl-flet* ((empty-line-p ()
                           (string-match-p (rx line-start
                                               (zero-or-more whitespace)
                                               line-end)
                                           (buffer-substring-no-properties
                                            (line-beginning-position)
                                            (line-end-position))))
             (line-indent-ok (indent)
                             (or (<= (length indent)
                                     (length
                                      (my/evil-indent--current-indentation)))
                                 (empty-line-p))))
    (let ((indent (my/evil-indent--current-indentation)) start begin end)
      ;; now skip ahead to the Nth block with this indentation
      (dotimes (_ (or count 0))
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

(evil-define-text-object evil-indent-i-block (&optional count _beg _end _type)
  "Text object describing the block with the same indentation as the current line."
  (cl-destructuring-bind (begin end)
      (my/evil-indent--block-range count)
    (evil-range begin end 'line)))

(evil-define-text-object evil-indent-a-block (&optional count _beg _end _type)
  "Text object describing the block with the same indentation as the current line and the line above."
  :type line
  (cl-destructuring-bind (begin end)
      (my/evil-indent--block-range count)
    (evil-range (save-excursion
                  (goto-char begin)
                  (forward-line -1)
                  (point-at-bol))
                end
                'line)))

(evil-define-text-object evil-indent-a-block-end (count &optional _beg _end _type)
  "Text object describing the block with the same indentation as the current line and the lines above and below."
  :type line
  (cl-destructuring-bind (begin end)
      (my/evil-indent--block-range count)
    (evil-range (save-excursion
                  (goto-char begin)
                  (forward-line -1)
                  (point-at-bol))
                (save-excursion
                  (goto-char end)
                  (forward-line 1)
                  (point-at-eol))
                'line)))

(define-key evil-inner-text-objects-map "i" #'evil-indent-i-block)
(define-key evil-outer-text-objects-map "i" #'evil-indent-a-block)
(define-key evil-inner-text-objects-map "I" #'evil-indent-a-block-end)
(define-key evil-outer-text-objects-map "I" #'evil-indent-a-block-end)

(defun evil-avy-jump-line-and-revert ()
  (interactive)
  (let ((top (save-excursion
               (evil-window-top)
               (line-number-at-pos))))
    (prog1
        (progn
          (deactivate-mark)
          (avy-goto-line)
          (point))
      (scroll-up-line (- top (save-excursion
                               (evil-window-top)
                               (line-number-at-pos)))))))

(evil-define-text-object evil-i-line-range (count &optional _beg _end _type)
  "Text object describing the block with the same indentation as the current line."
  :type line
  (save-excursion
    (let ((beg (evil-avy-jump-line-and-revert))
          (end (evil-avy-jump-line-and-revert)))
      (if (> beg end)
          (evil-range beg end #'line)
        (evil-range end beg #'line)))))

(define-key evil-inner-text-objects-map "r" #'evil-i-line-range)

(define-key evil-inner-text-objects-map "." #'evil-inner-sentence)
(define-key evil-outer-text-objects-map "." #'evil-a-sentence)

(evil-define-text-object evil-i-entire-buffer (count &optional _beg _end _type)
  "Text object describing the entire buffer excluding empty lines at the end"
  :type line
  (evil-range (point-min) (save-excursion
                            (goto-char (point-max))
                            (skip-chars-backward " \n\t")
                            (point)) 'line))

(evil-define-text-object evil-an-entire-buffer (count &optional _beg _end _type)
  "Text object describing the entire buffer"
  :type line
  (evil-range (point-min) (point-max) 'line))

(define-key evil-inner-text-objects-map "e" #'evil-i-entire-buffer)
(define-key evil-outer-text-objects-map "e" #'evil-an-entire-buffer)

(evil-define-text-object evil-inner-last-paste (count &optional _beg _end _type)
  :type char
  (evil-range (evil-get-marker ?\[) (evil-get-marker ?\]) 'char))

(define-key evil-inner-text-objects-map "P" #'evil-inner-last-paste)

;;; === evil operators ===
(autoload #'evilnc-comment-operator "evil-nerd-commenter")
(autoload #'evilnc-hotkey-comment-operator "evil-nerd-commenter")
(define-key evil-operator-state-map "gc" #'evilnc-comment-operator)
(define-key evil-normal-state-map "gc" #'evilnc-comment-operator)

(evil-define-operator evil-macro-on-all-lines (beg end &optional _arg)
  (evil-with-state 'normal
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

(evil-define-operator evil-narrow-indirect (beg end _type)
  "Indirectly narrow the region from BEG to END."
  (interactive "<R>")
  (evil-with-state 'normal
    (narrow-to-region-indirect beg end)))

(evil-define-operator evil-narrow-region (beg end _type)
  "Indirectly narrow the region from BEG to END."
  (interactive "<R>")
  (evil-with-state 'normal
    (narrow-to-region beg end)))

(define-key evil-operator-state-map "gn" #'evil-narrow-region)
(define-key evil-normal-state-map "gn" #'evil-narrow-region)
(define-key evil-operator-state-map "gN" #'evil-narrow-indirect)
(define-key evil-normal-state-map "gN" #'evil-narrow-indirect)

(evil-define-operator evil-eval-region (beg end _type)
  (eval-region beg end))

(define-key evil-operator-state-map "gV" #'evil-eval-region)
(define-key evil-normal-state-map "gV" #'evil-eval-region)

(evil-define-operator evil-align-regexp (beg end _type)
  (save-excursion
    (evil-with-state 'normal
      (goto-char beg)
      (evil-visual-state)
      (goto-char end)
      (call-interactively #'align-regexp))))

(define-key evil-operator-state-map "g|" #'evil-align-regexp)
(define-key evil-normal-state-map "g|" #'evil-align-regexp)

(evil-define-operator evil-count-words-region (beg end _type)
  (save-excursion
    (evil-with-state 'normal
      (goto-char beg)
      (evil-visual-state)
      (goto-char end)
      (call-interactively #'count-words-region))))

(define-key evil-operator-state-map (kbd "M-=") #'evil-count-words-region)
(define-key evil-normal-state-map (kbd "M-=") #'evil-count-words-region)

(use-package evil-textobj-syntax
  :init
  (eval-when-compile
    (with-demoted-errors "Load error: %s"
      (require 'el-patch)
      (require 'evil-textobj-syntax)))

  (el-patch-feature evil-textobj-syntax)

  (el-patch-defcustom evil-textobj-syntax-i-key "h"
    "Keys for evil-i-syntax"
    :type 'string
    :group 'evil-textobj-syntax)

  (el-patch-defcustom evil-textobj-syntax-a-key "h"
    "Keys for evil-a-syntax"
    :type 'string
    :group 'evil-textobj-syntax)

  (define-key evil-outer-text-objects-map
    evil-textobj-syntax-a-key #'evil-a-syntax)
  (define-key evil-inner-text-objects-map
    evil-textobj-syntax-i-key #'evil-i-syntax))

(provide 'config-evil-textobjects)
