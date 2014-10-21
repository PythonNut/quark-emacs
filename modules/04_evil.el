
;;; ===================================
;;; Evil mode - Emacs + Vim keybindings
;;; ===================================
(require 'evil)
(require 'evil-leader)
(require 'evil-indent-textobject-autoloads)

(setq evil-want-C-w-delete 'nil
  evil-want-C-w-in-emacs-state 'nil
  evil-ex-complete-emacs-commands 't
  evil-want-fine-undo 't
  evil-search-module 'evil-search
  evil-magic 'very-magic)

(setq-default
  evil-symbol-word-search 't)

(evil-mode +1)
(global-evil-leader-mode +1)
(setq evil-leader/leader "," evil-leader/in-all-states t)

(add-hook 'org-mode-hook
  '(lambda ()
     (require 'evil-org)))

(evil-set-initial-state 'diff-mode 'motion)
(evil-set-initial-state 'backups-mode 'insert)
(evil-set-initial-state 'erc-mode 'emacs)
(evil-set-initial-state 'git-commit-mode 'insert)
(evil-set-initial-state 'backup-walker-mode 'motion)
(evil-set-initial-state 'package-menu-mode 'motion)

(define-key evil-normal-state-map (kbd "<down>") 'evil-next-visual-line)
(define-key evil-motion-state-map (kbd "<up>") 'evil-previous-visual-line)
(define-key evil-motion-state-map (kbd "<down>") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "<up>") 'evil-previous-visual-line)

(key-chord-define evil-insert-state-map  "jj" 'evil-normal-state)
(key-chord-define evil-replace-state-map "jj" 'evil-normal-state)
(key-chord-define evil-emacs-state-map   "jj" 'evil-normal-state)

(key-chord-define evil-insert-state-map  "kk" 'evil-normal-state)
(key-chord-define evil-replace-state-map "kk" 'evil-normal-state)
(key-chord-define evil-emacs-state-map   "kk" 'evil-normal-state)

(key-chord-define evil-insert-state-map ";'" 'evil-ex)
(key-chord-define evil-emacs-state-map ";'" 'evil-ex)

(global-set-key (kbd "C-<backspace>") 'evil-delete-backward-word)

(key-chord-define evil-normal-state-map " l" 'evil-ace-jump-line-mode)
(key-chord-define evil-normal-state-map " n" 'ace-jump-char-N-lines)
(key-chord-define evil-normal-state-map " b" 'ace-jump-buffer)
(key-chord-define evil-normal-state-map " c" 'evil-ace-jump-char-mode)
(key-chord-define evil-normal-state-map " t" 'evil-ace-jump-char-to-mode)


(defun isearch-exit-chord-worker (&optional arg)
  (interactive "p")
  (execute-kbd-macro (kbd "<backspace> <return>")))

(defun isearch-exit-chord (arg)
  (interactive "p")
  (isearch-printing-char)
  (unless (fboundp 'smartrep-read-event-loop)
    (require 'smartrep))
  (run-at-time 0.3 nil 'keyboard-quit)
  (condition-case e
    (smartrep-read-event-loop
      '(("j" . isearch-exit-chord-worker)
         ("k" . isearch-exit-chord-worker)))
    (quit nil)))

(define-key isearch-mode-map "j" 'isearch-exit-chord)
(define-key isearch-mode-map "k" 'isearch-exit-chord)

;; This function builds a repeatable version of its argument COMMAND.
(defun repeat-command (command)
  "Repeat COMMAND."
  (interactive)
  (let ((repeat-previous-repeated-command command)
         (last-repeatable-command 'repeat))
    (repeat nil)))


(define-key evil-normal-state-map (kbd "C-SPC")
  '(lambda () (interactive)
     (evil-insert-state)
     (execute-kbd-macro (kbd "C-SPC"))))

(define-key evil-normal-state-map (kbd "C-RET")
  '(lambda ()
     (interactive)
     (evil-insert-state)
     (cua-set-rectangle-mark)))

(define-key evil-insert-state-map (kbd "C-e") 'evil-end-of-visual-line)
(setq evil-replace-state-cursor '("#884444" box))

(defun evil-open-below-normal (arg)
  (interactive "p")
  (evil-open-below arg)
  (evil-normal-state)
  (message ""))

(defun evil-open-above-normal (arg)
  (interactive "p")
  (evil-open-above arg)
  (evil-normal-state)
  (message ""))

(define-key evil-normal-state-map (kbd "[ <SPC>") 'evil-open-above-normal)
(define-key evil-normal-state-map (kbd "] <SPC>") 'evil-open-below-normal)

(define-key evil-normal-state-map (kbd "[ e") 'drag-stuff-up)
(define-key evil-normal-state-map (kbd "] e") 'drag-stuff-down)

(define-key evil-normal-state-map (kbd "[ w") 'drag-stuff-left)
(define-key evil-normal-state-map (kbd "] w") 'drag-stuff-right)

(define-key evil-insert-state-map (kbd "C-j") 'evil-normal-state)
(define-key evil-emacs-state-map (kbd "C-j") 'evil-normal-state)
(define-key evil-visual-state-map (kbd "C-j") 'evil-normal-state)
(define-key evil-replace-state-map (kbd "C-j") 'evil-normal-state)

(defun evil-open-paragraph-full (arg)
  (interactive "p")
  (evil-open-above-normal arg)
  (evil-open-below arg)
  (keyboard-quit))

(defun evil-open-paragraph-empty (arg)
  (interactive "p")
  (evil-open-below arg)
  (evil-previous-line arg)
  (indent-according-to-mode)
  (keyboard-quit))

(defun evil-open-paragraph (arg)
  (interactive "p")
  (unless (fboundp 'smartrep-read-event-loop)
    (require 'smartrep))
  (run-at-time 0.3 nil 'keyboard-quit)
  (let ((blank-line (string-match "^[[:space:]]*$"
                      (buffer-substring-no-properties
                        (line-beginning-position)
                        (line-end-position)))))
    (evil-open-below arg)
    (run-hooks 'post-command-hook)
    (if blank-line
      (condition-case e
        (smartrep-read-event-loop
          '(("o" . evil-open-paragraph-empty)))
        (quit nil))
      (condition-case e
        (smartrep-read-event-loop
          '(("o" . evil-open-paragraph-full)))
        (quit nil)))))

(define-key evil-normal-state-map "o" 'evil-open-paragraph)

(defun evil-yank-to-end-of-line ()
  (interactive)
  (evil-yank (point) (point-at-eol)))

(define-key evil-normal-state-map "Y" 'evil-yank-to-end-of-line)

(define-key evil-normal-state-map "U" 'undo-tree-visualize)
(define-key evil-normal-state-map (kbd "C-z")
  '(lambda ()
     (interactive)
     (message "use u.")))

(defun my-evil-smart-undo (&rest args)
  (interactive)
  (undo-tree-undo)
  (unless (fboundp 'smartrep-read-event-loop)
    (require 'smartrep))
  (condition-case e
    (smartrep-read-event-loop
      '(("r" . undo-tree-redo)
         ("u" . undo-tree-undo)))
    (quit nil)))

(defun my-evil-smart-redo (&rest args)
  (interactive)
  (undo-tree-redo)
  (unless (fboundp 'smartrep-read-event-loop)
    (require 'smartrep))
  (condition-case e
    (smartrep-read-event-loop
      '(("r" . undo-tree-redo)
         ("u" . undo-tree-undo)))
    (quit nil)))

(define-key evil-normal-state-map (kbd "u")   'my-evil-smart-undo)
(define-key evil-normal-state-map (kbd "C-r") 'my-evil-smart-redo)

;; ignore deleted blanks in evil
(defun purge-kill-ring ()
  (interactive)
  (let ((last-kill (substring-no-properties (first kill-ring)
                     0 (length (first kill-ring)))))
    (when (= (or (string-match "^[[:space:]]*\n$" last-kill) (point-max)) 0)
      (setq kill-ring (rest kill-ring)))))

(defadvice evil-delete (after ignore-blanks activate)
  (purge-kill-ring)
  (when (eq (first (get-char-property 0 'yank-handler (first kill-ring)))
          'evil-yank-line-handler)
    (put-text-property 0 1 'whole-line-or-region t (first kill-ring))))

(autoload 'evil-exchange        "evil-exchange")
(autoload 'evil-exchange-cancel "evil-exchange")

(define-key evil-normal-state-map "gx" 'evil-exchange)
(define-key evil-visual-state-map "gx" 'evil-exchange)
(define-key evil-normal-state-map "gX" 'evil-exchange-cancel)
(define-key evil-visual-state-map "gX" 'evil-exchange-cancel)

;;; === Evil motion section ===

(evil-define-motion evil-forward-symbol (count)
  (sp-forward-symbol count))

(evil-define-motion evil-backward-symbol (count)
  (sp-backward-symbol count))

(define-key evil-motion-state-map "L" 'evil-forward-symbol)
(define-key evil-motion-state-map "H" 'evil-backward-symbol)

;;; === Evil text object section ===
(evil-define-text-object evil-a-sexp (count &optional beg end type)
  (evil-an-object-range count beg end #'sp-forward-sexp #'sp-backward-sexp))

(evil-define-text-object evil-inner-sexp (count &optional beg end type)
  (evil-inner-object-range count beg end #'sp-forward-sexp #'sp-backward-sexp))

(define-key evil-outer-text-objects-map "e" 'evil-a-sexp)
(define-key evil-inner-text-objects-map "e" 'evil-inner-sexp)

;; do delimited selection
(defun evil-between-range (count beg end type &optional inclusive)
  (ignore-errors
    (let ((count (abs (or count 1)))
           (beg (and beg end (min beg end)))
           (end (and beg end (max beg end)))
           (ch (evil-read-key))
           beg-inc end-inc)
      (save-excursion
        (when beg (goto-char beg))
        (evil-find-char (- count) ch)
        (setq beg-inc (point)))
      (save-excursion
        (when end (goto-char end))
        (backward-char)
        (evil-find-char count ch)
        (setq end-inc (1+ (point))))
      (if inclusive
        (evil-range beg-inc end-inc)
        (if (and beg end (= (1+ beg-inc) beg) (= (1- end-inc) end))
          (evil-range beg-inc end-inc)
          (evil-range (1+ beg-inc) (1- end-inc)))))))

(evil-define-text-object evil-a-between (count &optional beg end type)
  "Select range between a character by which the command is followed."
  (evil-between-range count beg end type t))
(evil-define-text-object evil-i-between (count &optional beg end type)
  "Select inner range between a character by which the command is followed."
  (evil-between-range count beg end type))

(define-key evil-outer-text-objects-map "f" 'evil-a-between)
(define-key evil-inner-text-objects-map "f" 'evil-i-between)

;; evil block indentation textobject for Python
(defun evil-indent--current-indentation ()
  "Return the indentation of the current line. Moves point."
  (buffer-substring-no-properties (point-at-bol)
    (progn (back-to-indentation)
      (point))))

(defun evil-indent--block-range (&optional point)
  "Return the point at the begin and end of the text block with the same indentation."
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
        (setq index (or last-prefix-arg 0))
        (while (> index 1)
          (while (and (before-end) (line-indent-ok))
            (forward-line 1))
          (while (and (before-end) (not (line-indent-ok)))
            (forward-line 1))
          (setq index (1- index)))
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
(autoload 'evilnc-comment-or-uncomment-lines "evil-nerd-commenter")
(autoload 'evilnc-comment-or-uncomment-to-the-line "evil-nerd-commenter")
(autoload 'evilnc-copy-and-comment-lines "evil-nerd-commenter")

(global-set-key (kbd "M-;") 'evilnc-comment-or-uncomment-lines)
(global-set-key (kbd "C-M-;") 'evilnc-copy-and-comment-lines)

(defun evilnc-comment-or-uncomment-object ()
  (interactive)
  (call-interactively 'evil-visual-state)
  (call-interactively 'evil-indent-i-comment)
  (call-interactively 'evilnc-comment-or-uncomment-lines))

(defun evilnc-auto-comment ()
  (interactive)
  (save-excursion
    (call-interactively 'evilnc-comment-or-uncomment-lines))
  (save-excursion
    (call-interactively 'evilnc-comment-or-uncomment-lines)))

(evil-leader/set-key
  "ci" 'evilnc-comment-or-uncomment-lines
  "cl" 'evilnc-comment-or-uncomment-to-the-line
  "cc" 'evilnc-copy-and-comment-lines
  "cp" 'evilnc-comment-or-uncomment-paragraphs
  "ca" 'evilnc-auto-comment
  "co" 'evilnc-comment-or-uncomment-object)

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
    (setq index (or last-prefix-arg 0))

    (while (> index 1)
      (while (and (before-end) (comment-anywhere))
        (forward-line 1))
      (while (and (before-end) (not (comment-anywhere)))
        (forward-line 1))
      (setq index (1- index)))

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
          (line-end-position))))))

(evil-define-text-object evil-indent-i-comment (&optional count beg end type)
  "Text object describing the block with the same indentation as
the current line."
  (let ((range (evil-indent--comment-range)))
    (evil-contract-range (evil-range (second range) (third range) (first range)))))

(define-key evil-inner-text-objects-map "C" 'evil-indent-i-comment)

(defun evil-make-arbitrary-char-range ()
  (save-excursion
    (let ((beg (progn (deactivate-mark) (ace-jump-char-mode) (point)))
           (end (progn (deactivate-mark) (ace-jump-char-mode) (point))))
      (if (> beg end)
        (list beg end)
        (list end beg)))))

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

(define-key evil-inner-text-objects-map "r" 'evil-i-arbitrary-char-range)
(define-key evil-outer-text-objects-map "r" 'evil-a-arbitrary-char-range)

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
          (loop do (previous-line) while
            (string-match "^[[:space:]]*$"
              (buffer-substring-no-properties
                (line-beginning-position)
                (line-end-position))))
          (next-line)
          (point))
        (progn
          (goto-char (max beg end))
          (loop do (next-line) while
            (string-match "^[[:space:]]*$"
              (buffer-substring-no-properties
                (line-beginning-position)
                (line-end-position))))
          (previous-line)
          (point)) 'line))))

(define-key evil-inner-text-objects-map "R" 'evil-i-arbitrary-line-range)
(define-key evil-outer-text-objects-map "R" 'evil-a-arbitrary-line-range)

(evil-define-text-object my-evil-next-match (count &optional beg end type)
  "Select next match."
  (save-excursion
    (evil-search-previous 1)
    (evil-search-next count)
    (list evil-ex-search-match-beg evil-ex-search-match-end)))

(evil-define-text-object my-evil-previous-match (count &optional beg end type)
  "Select previous match."
  (evil-search-next 1)
  (evil-search-previous count)
  (list evil-search-match-beg evil-search-match-end))

(define-key evil-outer-text-objects-map "m" 'my-evil-previous-match)
(define-key evil-inner-text-objects-map "m" 'my-evil-next-match)

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
(evil-define-operator evil-comment-region (beg end type)
  (evil-normal-state)
  (goto-char beg)
  (evil-visual-state)
  (goto-char end)
  (call-interactively 'evilnc-comment-or-uncomment-lines))

(define-key evil-operator-state-map "gc" 'evil-comment-region)
(define-key evil-normal-state-map "gc" 'evil-comment-region)

(evil-define-operator evil-comment-and-copy-region (beg end type)
  (evil-normal-state)
  (goto-char beg)
  (evil-visual-state)
  (goto-char end)
  (call-interactively evilnc-copy-and-comment-lines))

(define-key evil-operator-state-map "gC" 'evil-comment-and-copy-region)
(define-key evil-normal-state-map "gC" 'evil-comment-and-copy-region)

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

;;; === other customizations ===
;; smarter f and t
(evil-define-motion evil-fastnav-forward-to (count)
  (fastnav-jump-to-char-forward (or count 1)))

(evil-define-motion evil-fastnav-backward-to (count)
  (fastnav-jump-to-char-backward (or count 1)))

(evil-define-motion evil-fastnav-forward-til (count)
  (fastnav-jump-to-char-forward (or count 1))
  (backward-char))

(evil-define-motion evil-fastnav-backward-til (count)
  (fastnav-jump-to-char-backward (or count 1))
  (forward-char))

(define-key evil-normal-state-map "t" 'evil-fastnav-forward-til)
(define-key evil-normal-state-map "T" 'evil-fastnav-backward-til)
(define-key evil-normal-state-map "f" 'evil-fastnav-forward-to)
(define-key evil-normal-state-map "F" 'evil-fastnav-backward-to)

(define-key evil-motion-state-map "t" 'evil-fastnav-forward-til)
(define-key evil-motion-state-map "T" 'evil-fastnav-backward-til)
(define-key evil-motion-state-map "f" 'evil-fastnav-forward-to)
(define-key evil-motion-state-map "F" 'evil-fastnav-backward-to)

(define-key evil-visual-state-map "t" 'evil-fastnav-forward-til)
(define-key evil-visual-state-map "T" 'evil-fastnav-backward-til)
(define-key evil-visual-state-map "f" 'evil-fastnav-forward-to)
(define-key evil-visual-state-map "F" 'evil-fastnav-backward-to)

;;; Change modeline color by Evil state
(lexical-let ((default-color (cons (face-background 'mode-line)
                               (face-foreground 'mode-line))))
  (add-hook 'post-command-hook
    (lambda ()
      (let ((color (cond ((minibufferp) default-color)
                     ((evil-normal-state-p) '("#586e75" . "#eee8d5"))
                     ((evil-emacs-state-p)  '("#859900" . "#eee8d5"))
                     ((evil-insert-state-p)  '("#93a1a1" . "#073642"))
                     ((evil-visual-state-p) '("#268bd2" . "#eee8d5"))
                     ((evil-replace-state-p) '("#dc322f" . "#eee8d5"))
                     (t '("grey70" . "black")))))
        (set-face-background 'mode-line (first color))
        (set-face-foreground 'mode-line (rest color))
        (set-face-foreground 'mode-line-buffer-id (rest color))))))

;; Evil surround, easily change surrounding chars
(require 'surround)
(global-surround-mode +1)

;; Esc quits from everything
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key evil-motion-state-map [escape] 'evil-normal-state)
(define-key evil-operator-state-map [escape] 'evil-normal-state)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

(define-key evil-insert-state-map (kbd "C-M-z") 'evil-emacs-state)
(define-key evil-emacs-state-map (kbd "C-M-z") 'evil-insert-state)
(define-key evil-normal-state-map (kbd "C-M-z") 'evil-insert-state)
(define-key evil-motion-state-map (kbd "C-M-z") 'evil-insert-state)
(define-key evil-visual-state-map (kbd "C-M-z") 'evil-insert-state)

(defun my-normal-smart-undo (&rest args)
  (interactive)
  (undo-tree-undo)
  (unless (fboundp 'smartrep-read-event-loop)
    (require 'smartrep))
  (condition-case e
    (smartrep-read-event-loop
      '(("y" . undo-tree-redo)
         ("z" . undo-tree-undo)))
    (quit nil)))

(defun my-normal-smart-redo (&rest args)
  (interactive)
  (undo-tree-redo)
  (unless (fboundp 'smartrep-read-event-loop)
    (require 'smartrep))
  (condition-case e
    (smartrep-read-event-loop
      '(("y" . undo-tree-redo)
         ("z" . undo-tree-undo)))
    (quit nil)))

(define-key evil-insert-state-map (kbd "C-z") 'my-normal-smart-undo)
(define-key evil-emacs-state-map (kbd "C-z") 'my-normal-smart-undo)

(global-set-key (kbd "<remap> <undo-tree-redo>") 'my-normal-smart-redo)
(global-set-key (kbd "C-S-z") 'my-normal-smart-redo)

(defadvice evil-paste-before (around auto-indent activate)
  (evil-indent (point) (+ (point) (length ad-do-it))))

(defadvice evil-paste-after (around auto-indent activate)
  (evil-indent (point) (+ (point) (length ad-do-it))))

(defun evil-rename-symbol (arg)
  "mark the region in defun (definition of function) and use string replacing UI in evil-mode
to replace the symbol under cursor"
  (interactive "P")
  (let ((old (thing-at-point 'symbol)))
    (mark-defun)
    (unless (evil-visual-state-p)
      (evil-visual-state))
    (evil-ex (concat "'<,'>s/" (if (= 0 (length old)) "" "\\<\\(") old (if (= 0 (length old)) "" "\\)\\>/")))))

(global-set-key (kbd "C-c C-c r") 'evil-rename-symbol)


