;; -*- lexical-binding: t -*-

(eval-when-compile
  (with-demoted-errors "Load error: %s"
    (require 'evil)
    (require 'cua-base)))

;; In GUI Emacs, save the clipboard to the kill ring on focus-in
;; so we can copy things into Emacs even if we don't yank it first
;; or we add new items to the kill-ring before we yank.
(add-hook 'focus-in-hook
          (lambda ()
            (let ((gui-selection (gui-selection-value)))
              (when (and gui-selection
                         (not (string= gui-selection
                                       (substring-no-properties (car-safe kill-ring)))))
                (kill-new gui-selection)))))

(autoload #'whole-line-or-region-call-with-region "whole-line-or-region")
(autoload #'whole-line-or-region-call-with-prefix "whole-line-or-region")

(setq cua-paste-pop-rotate-temporarily t
      cua-enable-cua-keys nil
      cua-virtual-rectangle-edges t
      cua-auto-tabify-rectangles nil
      cua-rectangle-mark-key (kbd "C-x SPC"))

(define-key evil-insert-state-map (kbd "C-x SPC") #'cua-set-rectangle-mark)
(define-key evil-emacs-state-map (kbd "C-x SPC") #'cua-set-rectangle-mark)

(cua-mode +1)

(put 'evil-forward-char             'CUA 'move)
(put 'evil-backward-char            'CUA 'move)
(put 'evil-next-visual-line         'CUA 'move)
(put 'evil-previous-visual-line     'CUA 'move)
(put 'evil-end-of-visual-line       'CUA 'move)
(put 'evil-beginning-of-visual-line 'CUA 'move)

;; cua-cut the line if no region
(defun nadvice/cua-cut-region (old-fun &optional prefix)
  (interactive "*p")
  (whole-line-or-region-call-with-region
   (lambda (_beg _end &optional _prefix)
     (interactive "rP")
     (call-interactively
      old-fun
      current-prefix-arg))
   prefix t t prefix))

(advice-add 'cua-cut-region :around #'nadvice/cua-cut-region)

;; cua-yank a line if cut as a line
(defun nadvice/cua-paste (old-fun raw-prefix &optional string-in)
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
            (call-interactively old-fun raw-prefix))

          ;; a whole-line killed from end of file may not have a
          ;; trailing newline -- add one, in these cases
          (when (not (string-match-p "\n$" string-to-yank))
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
          (call-interactively old-fun raw-prefix))))))

(advice-add 'cua-paste :around #'nadvice/cua-paste)

(defun easy-kill-on-my-line (_n)
  "Get current line, but mark as a whole line for whole-line-or-region"
  (let ((str (thing-at-point 'line)))
    (save-excursion
      (put-text-property 0 1 'whole-line-or-region t str)
      (easy-kill-adjust-candidate 'my-line str))))

(with-eval-after-load 'easy-kill
  (eval-when-compile
    (with-demoted-errors "Load error: %s"
      (require 'easy-kill)))

  (setq easy-kill-try-things '(url email my-line)))

;; make evil respect whole-line-or-region
(defun nadvice/evil-paste-line (&rest _args)
  (when (with-demoted-errors "Failed to check text properties for paste. %s"
          (get-text-property 0 'whole-line-or-region (car kill-ring)))
    (setf (car kill-ring)
          (propertize (car kill-ring) 'yank-handler (list 'evil-yank-line-handler)))))

(advice-add 'evil-paste-before :before #'nadvice/evil-paste-line)
(advice-add 'evil-paste-after  :before #'nadvice/evil-paste-line)


;; unify evil-paste with cua rectangles
(defun nadvice/evil-paste-after (old-fun &rest args)
  (if (eq (with-demoted-errors "Failed to check text properties for paste. %s"
            (car (get-text-property 0 'yank-handler (car kill-ring))))
          'rectangle--insert-for-yank)
      (evil-with-state 'normal
        (call-interactively #'evil-append)
        (call-interactively #'cua-paste))
    (apply old-fun args)))

(defun nadvice/evil-paste-before (old-fun &rest args)
  (if (eq (with-demoted-errors "Failed to check text properties for paste. %s"
            (car (get-text-property 0 'yank-handler (car kill-ring))))
          'rectangle--insert-for-yank)
      (evil-with-state 'normal
        (call-interactively #'evil-insert)
        (call-interactively #'cua-paste))
    (apply old-fun args)))

(advice-add 'evil-paste-after  :around #'nadvice/evil-paste-after)
(advice-add 'evil-paste-before :around #'nadvice/evil-paste-before)


(define-key evil-insert-state-map (kbd "C-w") nil)


(define-key evil-insert-state-map
  (kbd "<remap> <kill-ring-save>") #'easy-kill)
(define-key evil-emacs-state-map
  (kbd "<remap> <kill-ring-save>") #'easy-kill)
(define-key evil-normal-state-map
  (kbd "<remap> <kill-ring-save>") #'easy-kill)

(define-key evil-emacs-state-map
  (kbd "<remap> <kill-region>") #'cua-cut-region)
(define-key evil-insert-state-map
  (kbd "<remap> <kill-region>") #'cua-cut-region)

(define-key evil-emacs-state-map
  (kbd "<remap> <yank-pop>") #'cua-paste-pop)
(define-key evil-insert-state-map
  (kbd "<remap> <yank-pop>") #'cua-paste-pop)

(define-key evil-emacs-state-map (kbd "C-y") #'cua-paste)
(define-key evil-insert-state-map (kbd "C-y") #'cua-paste)



(with-eval-after-load 'xt-mouse
  (add-hook 'kill-emacs-hook
                (lambda ()
                  (xterm-mouse-mode -1))))

(defun my/setup-paste (&optional frame)
  (with-selected-frame (or frame (selected-frame))
    (unless (display-graphic-p)
      (when (and (not xclip-mode)
                 (or (executable-find "xclip")
                     (executable-find "pbcopy")))
        (xclip-mode +1))

      ;; just in case the terminal is a failure
      (defvar arrow-keys-brace-map (make-sparse-keymap)
        "Keymap for untranslated brace arrow keys")
      (define-key esc-map "[" arrow-keys-brace-map)
      (define-key arrow-keys-brace-map "A" (kbd "<up>"))
      (define-key arrow-keys-brace-map "B" (kbd "<down>"))
      (define-key arrow-keys-brace-map "C" (kbd "<right>"))
      (define-key arrow-keys-brace-map "D" (kbd "<left>"))

      (defvar arrow-keys-O-map (make-sparse-keymap)
        "Keymap for untranslated O arrow keys")
      (define-key esc-map "O" arrow-keys-O-map)
      (define-key arrow-keys-O-map "A" (kbd "<up>"))
      (define-key arrow-keys-O-map "B" (kbd "<down>"))
      (define-key arrow-keys-O-map "C" (kbd "<right>"))
      (define-key arrow-keys-O-map "D" (kbd "<left>"))

      ;; interpreted as C-<Arrow> in a terminal
      (define-key key-translation-map (kbd "M-[ d") (kbd "<C-left>"))
      (define-key key-translation-map (kbd "M-[ c") (kbd "<C-right>"))
      (define-key key-translation-map (kbd "M-[ a") (kbd "<C-up>"))
      (define-key key-translation-map (kbd "M-[ b") (kbd "<C-down"))

      (define-key key-translation-map (kbd "M-[ D") (kbd "<C-left>"))
      (define-key key-translation-map (kbd "M-[ C") (kbd "<C-right>"))
      (define-key key-translation-map (kbd "M-[ A") (kbd "<C-up>"))
      (define-key key-translation-map (kbd "M-[ B") (kbd "<C-down>"))

      ;; interpret M-<arrow> when terminal fails to compose ESC
      (define-key key-translation-map (kbd "ESC <left>") (kbd "<M-left>"))
      (define-key key-translation-map (kbd "ESC <right>") (kbd "<M-right>"))
      (define-key key-translation-map (kbd "ESC <up>") (kbd "<M-up>"))
      (define-key key-translation-map (kbd "ESC <down>") (kbd "<M-down>"))

      (xterm-mouse-mode +1)

      (bracketed-paste-enable)
      (bracketed-paste-setup)

      ;; fix display corruption in certain terminals
      (add-hook 'isearch-update-post-hook #'redraw-display)

      (when (getenv "TMUX")
        (run-hooks 'terminal-init-xterm-hook)))))

(my/setup-paste)
(add-hook 'after-make-frame-functions #'my/setup-paste)

(with-eval-after-load 'bracketed-paste
  (add-hook 'bracketed-paste--pasting-mode-hook
            (lambda ()
              (smartparens-mode -1))))

(with-eval-after-load 'iso-transl
  (eval-when-compile
    (with-demoted-errors "Load error: %s"
      (require 'iso-transl)))

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
  (define-key iso-transl-ctl-x-8-map "==<" "⇐")

  (let* ((keys (eval-when-compile
                 (append
                  (mapcar #'string
                          (string-to-list
                           "',-./0123456789;=[\\]`abcdefghijklmnopqrstuvwxyz"))
                  '("<left>" "<right>" "<up>" "<down>"
                    "<return>" "<tab>" "RET" "TAB")))))

    (define-prefix-command 'iso-cm-map)
    (define-prefix-command 'iso-cs-map)
    (define-prefix-command 'iso-ms-map)
    (define-prefix-command 'iso-cms-map)
    (define-key iso-transl-ctl-x-8-map (kbd ";") 'iso-cm-map)
    (define-key iso-transl-ctl-x-8-map (kbd ":") 'iso-cs-map)
    (define-key iso-transl-ctl-x-8-map (kbd "M-;") 'iso-ms-map)
    (define-key iso-transl-ctl-x-8-map (kbd "M-:") 'iso-cms-map)

    (dolist (key keys)
      (define-key iso-transl-ctl-x-8-map
        (kbd (concat "; " key))
        (kbd (concat "C-M-" key)))

      (define-key iso-transl-ctl-x-8-map
        (kbd (concat ": " key))
        (kbd (concat "C-S-" key)))

      (define-key iso-transl-ctl-x-8-map
        (kbd (concat "M-; " key))
        (kbd (concat "M-S-" key)))

      (define-key iso-transl-ctl-x-8-map
        (kbd (concat "M-: " key))
        (kbd (concat "C-M-S-" key))))))

(require 'config-package)

(package-deferred-install 'legalese
    :autoload-names '('legalese))

(with-eval-after-load 'xclip
  (defun nadvice/xclip-set-selection (old-fun &rest args)
    (let ((default-directory "/"))
      (apply old-fun args)))
  (advice-add 'xclip-set-selection :around #'nadvice/xclip-set-selection)
  (advice-add 'xclip-selection-value :around #'nadvice/xclip-set-selection))

(defun remove-clipboard-formatting ()
  "A quick command to drop clipboard formatting"
  (interactive)
  (with-temp-buffer
    (cua-paste nil)
    (mark-whole-buffer)
    (cua-cut-region nil)))

(provide 'config-paste)
