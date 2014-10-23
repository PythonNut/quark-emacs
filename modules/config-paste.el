(require 'whole-line-or-region)

(cua-mode +1)

;; easy-kill the line if no region
(defun my-wlr-easy-kill (&optional prefix)
  (interactive "*p")
  (whole-line-or-region-call-with-region 'easy-kill prefix))

;; cua-cut the line if no region
(defun my-wlr-cua-cut-region (&optional prefix)
  (interactive "*p")
  (whole-line-or-region-call-with-region
    '(lambda (&optional beg end)
       (goto-char beg)
       (set-mark-command prefix)
       (cua-set-mark)
       (goto-char end)
       (cua-cut-region current-prefix-arg)) prefix))

;; cua-yank a line if cut as a line
(defun whole-line-or-region-yank-cua (raw-prefix &optional string-in)
  "Yank (paste) previously killed text.

If the text to be yanked was killed with a whole-line-or-region
function *as* a whole-line, then paste it as a whole line (i.e. do not
break up the current line, and do not force the user to move point).

RAW-PREFIX is used to determine which string to yank, just as `yank'
would normally use it.

Optionally, pass in string to be \"yanked\" via STRING-IN."
  (interactive "*P")

  ;; figure out what yank would do normally
  (let ((string-to-yank
          (or string-in
            (current-kill
              (cond ((listp raw-prefix) 0)
                ((eq raw-prefix '-) -1)
                (t (1- raw-prefix))) t)))
         (saved-column (current-column)))

    ;; check for whole-line prop in yanked text
    (if (get-text-property 0 'whole-line-or-region string-to-yank)
      (let ((beg (line-beginning-position)))
        ;; goto beg of line and yank
        (beginning-of-line)
        (if string-in
          ;; insert "manually"
          (insert string-in)
          ;; just yank as normal
          (cua-paste raw-prefix))

        ;; a whole-line killed from end of file may not have a
        ;; trailing newline -- add one, in these cases
        (when (not (string-match "\n$" string-to-yank))
          (insert "\n")
          (previous-line 1))

        ;; restore state of being....
        (move-to-column saved-column)
        (remove-text-properties beg (+ beg 1) '(whole-line-or-region nil)))

      ;; no whole-line-or-region mark
      (if string-in
        ;; insert "manually"
        (progn
          (when (and delete-selection-mode
                  mark-active)
            (delete-active-region))
          (insert string-in))
        ;; just yank as normal
        (cua-paste raw-prefix)))))

(defun easy-kill-on-my-line (_n)
  "Get current line, but mark as a whole line for whole-line-or-region"
  (let ((str (thing-at-point 'line))
         (beg (line-beginning-position)))
    (save-excursion
      (put-text-property 0 1 'whole-line-or-region t str)
      (easy-kill-adjust-candidate 'my-line str))))

(setq easy-kill-try-things '(url email my-line))

(define-key evil-insert-state-map (kbd "C-w") nil)
(define-key evil-insert-state-map (kbd "<remap> <kill-region>") 'my-wlr-cua-cut-region)
(define-key evil-insert-state-map (kbd "<remap> <kill-ring-save>") 'easy-kill)
(define-key evil-normal-state-map (kbd "<remap> <kill-ring-save>") 'easy-kill)
(define-key evil-insert-state-map (kbd "C-y") 'whole-line-or-region-yank-cua)

(define-key evil-emacs-state-map (kbd "<remap> <kill-region>") 'my-wlr-cua-cut-region)
(define-key evil-emacs-state-map (kbd "<remap> <kill-ring-save>") 'easy-kill)
(define-key evil-emacs-state-map (kbd "C-y") 'whole-line-or-region-yank-cua)
