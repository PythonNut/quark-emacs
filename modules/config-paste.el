(eval-when-compile
  (with-demoted-errors
    (require 'evil)
    (require 'cua-base)
    (require 'easy-kill)
    (require 'iso-transl)
    (require 'whole-line-or-region)))

(autoload 'whole-line-or-region-call-with-region "whole-line-or-region")
(autoload 'whole-line-or-region-call-with-prefix "whole-line-or-region")

(setq
  cua-paste-pop-rotate-temporarily t
  cua-enable-cua-keys nil
  cua-virtual-rectangle-edges t
  cua-auto-tabify-rectangles nil)

(when (display-graphic-p)
  (define-key evil-insert-state-map (kbd "C-x SPC") #'cua-set-rectangle-mark)
  (define-key evil-emacs-state-map (kbd "C-x SPC") #'cua-set-rectangle-mark)
  (setq cua-rectangle-mark-key (kbd "C-x SPC")))

(cua-mode +1)

(put 'evil-forward-char             'CUA 'move)
(put 'evil-backward-char            'CUA 'move)
(put 'evil-next-visual-line         'CUA 'move)
(put 'evil-previous-visual-line     'CUA 'move)
(put 'evil-end-of-visual-line       'CUA 'move)
(put 'evil-beginning-of-visual-line 'CUA 'move)

;; cua-cut the line if no region
(defadvice cua-cut-region
  (around whole-line-or-region
    (&optional prefix)
    activate preactivate compile)
  (interactive "*p")
  (whole-line-or-region-call-with-region
    (lambda (beg end &optional prefix)
      (interactive "rP")
      (call-interactively
        (ad-get-orig-definition 'cua-cut-region)
        current-prefix-arg)) prefix t t prefix))

;; cua-yank a line if cut as a line
(defadvice cua-paste
  (around whole-line-or-region
    (raw-prefix &optional string-in)
    activate preactivate compile)
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
          (call-interactively (ad-get-orig-definition 'cua-paste) raw-prefix))

        ;; a whole-line killed from end of file may not have a
        ;; trailing newline -- add one, in these cases
        (when (not (string-match "\n$" string-to-yank))
          (insert "\n")
          (forward-line -1))

        ;; restore state of being....
        (move-to-column saved-column)
        (remove-text-properties beg (1+ beg) '(whole-line-or-region nil)))

      ;; no whole-line-or-region mark
      (if string-in
        ;; insert "manually"
        (progn
          (when (and delete-selection-mode mark-active)
            (delete-active-region))
          (insert string-in))
        ;; just yank as normal
        (if (eq (car (get-text-property 0 'yank-handler
                       string-to-yank))
              'evil-yank-line-handler)
          (evil-paste-before raw-prefix)
          (call-interactively (ad-get-orig-definition 'cua-paste) raw-prefix))))))

(defun easy-kill-on-my-line (_n)
  "Get current line, but mark as a whole line for whole-line-or-region"
  (let ((str (thing-at-point 'line))
         (beg (line-beginning-position)))
    (save-excursion
      (put-text-property 0 1 'whole-line-or-region t str)
      (easy-kill-adjust-candidate 'my-line str))))

(with-eval-after-load 'easy-kill
  (setq easy-kill-try-things '(url email my-line)))

;; make evil respect whole-line-or-region
(defadvice evil-paste-after
  (before whole-line-or-region
    activate preactivate compile)
  (when (get-text-property 0 'whole-line-or-region (car kill-ring))
    (setf (car kill-ring)
      (propertize (car kill-ring) 'yank-handler (list 'evil-yank-line-handler)))))

(defadvice evil-paste-after
  (before whole-line-or-region
    activate preactivate compile)
  (when (get-text-property 0 'whole-line-or-region (car kill-ring))
    (setf (car kill-ring)
      (propertize (car kill-ring) 'yank-handler (list 'evil-yank-line-handler)))))

;; unify evil-paste with cua rectangles
(defadvice evil-paste-after
  (around cua-rectangles
    activate preactivate compile)
  (if (eq (car (get-text-property 0 'yank-handler (car kill-ring)))
        'rectangle--insert-for-yank)
    (evil-with-state
      (call-interactively #'evil-append)
      (call-interactively #'cua-paste))
    ad-do-it))

(defadvice evil-paste-before
  (around cua-rectangles
    activate preactivate compile)
  (if (eq (car (get-text-property 0 'yank-handler (car kill-ring)))
        'rectangle--insert-for-yank)
    (evil-with-state
      (call-interactively #'evil-insert)
      (call-interactively #'cua-paste))
    ad-do-it))

(define-key evil-insert-state-map (kbd "C-w") nil)

(define-key evil-insert-state-map
  (kbd "<remap> <kill-region>") #'cua-cut-region)
(define-key evil-insert-state-map
  (kbd "<remap> <kill-ring-save>") #'easy-kill)
(define-key evil-normal-state-map
  (kbd "<remap> <kill-ring-save>") #'easy-kill)
(define-key evil-insert-state-map (kbd "C-y") #'cua-paste)

(define-key evil-emacs-state-map
  (kbd "<remap> <kill-region>") #'cua-cut-region)
(define-key evil-emacs-state-map
  (kbd "<remap> <kill-ring-save>") #'easy-kill)
(define-key evil-emacs-state-map (kbd "C-y") #'cua-paste)

(defun setup-paste ()
  (unless (display-graphic-p)
    (when (and (not xclip-mode)
            (or
              (executable-find "xclip")
              (executable-find "pbcopy")))
      (xclip-mode +1))
    (xterm-mouse-mode +1)
    (require 'bracketed-paste)
    (bracketed-paste-enable)
    (bracketed-paste-setup)

    ;; fix display corruption in certain terminals when using isearch
    (defadvice isearch-printing-char
      (after redisplay activate preactivate compile)
      (redraw-display))

    (when (getenv "TMUX")
      (run-hooks 'terminal-init-xterm-hook))
    (add-hook 'kill-emacs-hook
      (lambda ()
        (xterm-mouse-mode -1)))))

(add-hook 'after-make-frame-hook #'setup-paste)
(add-hook 'emacs-startup-hook #'setup-paste)

(with-eval-after-load 'bracketed-paste
  (add-hook 'bracketed-paste--pasting-mode-hook
    (lambda ()
      (smartparens-mode -1))))

(with-eval-after-load 'iso-transl
  (define-prefix-command 'arrow-thin-map)
  (define-key iso-transl-ctl-x-8-map "-" 'arrow-thin-map)
  (define-key iso-transl-ctl-x-8-map "->" "→")
  (define-key iso-transl-ctl-x-8-map "-->" "→")
  (define-key iso-transl-ctl-x-8-map "-<" "←")
  (define-key iso-transl-ctl-x-8-map "--<" "←")

  (define-prefix-command 'arrow-thick-map)
  (define-key iso-transl-ctl-x-8-map "=" 'arrow-thick-map)
  (define-key iso-transl-ctl-x-8-map "=>" "⇒")
  (define-key iso-transl-ctl-x-8-map "==>" "⇒")
  (define-key iso-transl-ctl-x-8-map "=<" "⇐")
  (define-key iso-transl-ctl-x-8-map "==<" "⇐"))

(provide 'config-paste)
