;; -*- lexical-binding: t -*-
(eval-when-compile (require 'config-macros))

(eval-when-compile
  (with-demoted-errors "Load error: %s"
    (require 'cua-base)
    (require 'el-patch)
    (use-package evil)
    (require 'evil)))

;; Bring most recently yank-popped entry to the front of the kill-ring

(defvar kill-ring-yank-index 0
  "Index into kill-ring of last yank-pop. The item yank-popped
  will be at the head of the kill ring, but if the next command
  is also yank-pop, it will be returned here first before this
  variable is incremented.")

(el-patch-defun current-kill (n &optional do-not-move)
  "Rotate the yanking point by N places, and then return that kill.
If N is zero and `interprogram-paste-function' is set to a
function that returns a string or a list of strings, and if that
function doesn't return nil, then that string (or list) is added
to the front of the kill ring and the string (or first string in
the list) is returned as the latest kill.

If N is not zero, and if `yank-pop-change-selection' is
non-nil, use `interprogram-cut-function' to transfer the
kill at the new yank point into the window system selection.

If optional arg DO-NOT-MOVE is non-nil, then don't actually
move the yanking point; just return the Nth kill forward."
  (el-patch-add (require 'dash))
  (let ((interprogram-paste (and (= n 0)
                                 interprogram-paste-function
                                 (funcall interprogram-paste-function))))
    (if interprogram-paste
        (progn
          ;; Disable the interprogram cut function when we add the new
          ;; text to the kill ring, so Emacs doesn't try to own the
          ;; selection, with identical text.
          (let ((interprogram-cut-function nil))
            (if (listp interprogram-paste)
                (mapc 'kill-new (nreverse interprogram-paste))
              (kill-new interprogram-paste)))
          (el-patch-add (setq kill-ring-yank-index 0))
          (car kill-ring))
      (or kill-ring (error "Kill ring is empty"))
      (el-patch-add
        ;; Put the head of kill-ring back where we had
        ;; previously found it, and fetch the next element
        (if (or (eq 0 n) (eq this-command 'evil-visual-paste))
            (setq kill-ring-yank-index 0)
          (setq kill-ring
                (-insert-at kill-ring-yank-index
                            (car kill-ring)
                            (cdr kill-ring))
                kill-ring-yank-index n)
          (when (>= kill-ring-yank-index (- (length kill-ring) 1))
            (setq kill-ring-yank-index (- (length kill-ring) 1))
            (user-error "Reached end of kill-ring"))
          (when (< kill-ring-yank-index 0)
            (setq kill-ring-yank-index 0)
            (user-error "Reached beginning of kill-ring"))
          (setq kill-ring (let ((new (nth kill-ring-yank-index
                                          kill-ring)))
                            (cons new (-remove-at kill-ring-yank-index
                                                  kill-ring))))))
      (let ((ARGth-kill-element
             (el-patch-swap
               (nthcdr (mod (- n (length kill-ring-yank-pointer))
                            (length kill-ring))
                       kill-ring)
               kill-ring)))
        (unless do-not-move
          (setq kill-ring-yank-pointer ARGth-kill-element)
          (when (and yank-pop-change-selection
                     (> n 0)
                     interprogram-cut-function)
            (funcall interprogram-cut-function (car ARGth-kill-element))))
        (car ARGth-kill-element)))))

(use-package xclip
  :config
  (advice-add
   'xclip-set-selection :around
   (my/defun-as-value nadvice/xclip-set-selection (old-fun &rest args)
     (let ((default-directory "/"))
       (apply old-fun args))))

  (advice-add
   'xclip-selection-value :around
   (my/defun-as-value nadvice/xclip-selection-value (old-fun &rest args)
     (let ((default-directory "/"))
       (unless (string-match-p
                (rx bol "Error: Can't open display: ")
                (cdr (my/process-file-to-string "xclip" nil t nil "-o")))
         (apply old-fun args))))))

(use-package bracketed-paste
  :config
  (add-hook
   'bracketed-paste--pasting-mode-hook
   (my/defun-as-value my/disable-smartparens-during-bracketed-paste ()
     (smartparens-mode -1))))

(use-package whole-line-or-region
  :commands (whole-line-or-region-call-with-region
             whole-line-or-region-call-with-prefix))

(use-package cua-base
  :ensure nil
  :init
  (setq kill-do-not-save-duplicates t
        cua-paste-pop-rotate-temporarily t
        cua-enable-cua-keys t
        cua-virtual-rectangle-edges t
        cua-auto-tabify-rectangles nil
        cua-rectangle-mark-key (kbd "C-x SPC"))
  (cua-mode +1)
  :config
  (evil-define-key 'normal cua-global-keymap (kbd "C-v") nil))

;; cua-cut the line if no region
(advice-add
 'cua-cut-region :around
 (my/defun-as-value nadvice/cua-cut-region (old-fun &optional prefix)
   (interactive "*p")
   (whole-line-or-region-call-with-region
    (lambda (_beg _end &optional _prefix)
      (interactive "rP")
      (call-interactively
       old-fun
       current-prefix-arg))
    prefix t t prefix)))

;; cua-yank a line if cut as a line
(advice-add
 'cua-paste :around
 (my/defun-as-value nadvice/cua-paste (old-fun raw-prefix)
   ;; figure out what yank would do normally
   (let ((string-to-yank (current-kill
                          (cond ((listp raw-prefix) 0)
                                ((eq raw-prefix '-) -1)
                                (t (1- raw-prefix))) t))
         (saved-column (current-column)))

     ;; check for whole-line prop in yanked text
     (if (get-text-property 0 'whole-line-or-region string-to-yank)
         (save-excursion
           (end-of-line)
           (insert "\n")
           (let ((beg (line-beginning-position)))
             (insert (string-remove-suffix "\n" string-to-yank))
             (remove-text-properties beg (1+ beg) '(whole-line-or-region nil))))

       ;; no whole-line-or-region mark
       (if (eq (car (get-text-property 0 'yank-handler
                                       string-to-yank))
               'evil-yank-line-handler)
           (evil-paste-after raw-prefix)
         (funcall old-fun raw-prefix))))))

(defun easy-kill-on-my-line (_n)
  "Get current line, but mark as a whole line for whole-line-or-region"
  (let ((str (thing-at-point 'line)))
    (save-excursion
      (put-text-property 0 1 'whole-line-or-region t str)
      (easy-kill-adjust-candidate 'my-line str))))

(use-package easy-kill
  :config
  (setq easy-kill-try-things '(url email my-line)))

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

(with-eval-after-load 'evil
  (eval-when-compile
    (with-demoted-errors "Load error: %s"
      (require 'evil)))

  (define-key evil-insert-state-map (kbd "C-x SPC") #'cua-set-rectangle-mark)
  (define-key evil-emacs-state-map (kbd "C-x SPC") #'cua-set-rectangle-mark)
  (define-key evil-insert-state-map (kbd "C-v") #'cua-paste)

  (put 'evil-forward-char             'CUA 'move)
  (put 'evil-backward-char            'CUA 'move)
  (put 'evil-next-visual-line         'CUA 'move)
  (put 'evil-previous-visual-line     'CUA 'move)
  (put 'evil-end-of-visual-line       'CUA 'move)
  (put 'evil-beginning-of-visual-line 'CUA 'move)

  ;; make evil respect whole-line-or-region
  (defun nadvice/evil-paste-line (&rest _args)
    (when (with-demoted-errors "Failed to check text properties for paste. %s"
            (get-text-property 0 'whole-line-or-region (car kill-ring)))
      (setf (car kill-ring)
            (propertize (car kill-ring) 'yank-handler (list 'evil-yank-line-handler)))))

  (advice-add 'evil-paste-before :before #'nadvice/evil-paste-line)
  (advice-add 'evil-paste-after  :before #'nadvice/evil-paste-line)


  ;; unify evil-paste with cua rectangles
  (advice-add
   'evil-paste-after :around
   (my/defun-as-value nadvice/evil-paste-after (old-fun &rest args)
     (if (eq (with-demoted-errors "Failed to check text properties for paste. %s"
               (car (get-text-property 0 'yank-handler (car kill-ring))))
             'rectangle--insert-for-yank)
         (evil-with-state 'normal
           (call-interactively #'evil-append)
           (call-interactively #'cua-paste))
       (apply old-fun args))))

  (advice-add
   'evil-paste-before :around
   (my/defun-as-value nadvice/evil-paste-before (old-fun &rest args)
     (if (eq (with-demoted-errors "Failed to check text properties for paste. %s"
               (car (get-text-property 0 'yank-handler (car kill-ring))))
             'rectangle--insert-for-yank)
         (evil-with-state 'normal
           (call-interactively #'evil-insert)
           (call-interactively #'cua-paste))
       (apply old-fun args))))

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

  ;; ensure cua-mode doesn't interfere with evil visual state
  (let ((my/cua-mode-was-on))
    (add-hook 'evil-visual-state-entry-hook
              (my/defun-as-value my/entering-visual-state-disable-cua ()
                (setq my/cua-mode-was-on cua-mode)
                (when cua-mode
                  (cua-mode -1))))
    (add-hook 'evil-visual-state-exit-hook
              (my/defun-as-value my/leaving-visual-state-enable-cua ()
                (when my/cua-mode-was-on
                  (cua-mode +1))))))

(use-package legalese
  :defer-install t
  :commands (legalese))

(defun remove-clipboard-formatting ()
  "A quick command to drop clipboard formatting"
  (interactive)
  (with-temp-buffer
    (cua-paste nil)
    (push-mark (point))
    (push-mark (point-max) nil t)
    (goto-char (point-min))
    (cua-cut-region nil)))

(el-patch-evil-define-command evil-visual-paste (count &optional register)
  "Paste over Visual selection."
  :suppress-operator t
  (interactive "*P<x>")
  (setq count (prefix-numeric-value count))
  ;; evil-visual-paste is typically called from evil-paste-before or
  ;; evil-paste-after, but we have to mark that the paste was from
  ;; visual state
  (setq this-command 'evil-visual-paste)
  (el-patch-let (($paste (if paste-eob
                             (evil-paste-after count register)
                           (evil-paste-before count register))))
    (let* ((text (if register
                     (evil-get-register register)
                   (current-kill 0)))
           (yank-handler (car-safe (get-text-property
                                    0 'yank-handler text)))
           new-kill
           paste-eob)
      (evil-with-undo
        (let* (el-patch-swap
                ((kill-ring (list (current-kill 0)))
                 (kill-ring-yank-pointer kill-ring))
                ((kill-ring kill-ring)
                 (kill-ring-yank-pointer kill-ring-yank-pointer)
                 (kill-ring-yank-index kill-ring-yank-index)))
          (when (evil-visual-state-p)
            (evil-visual-rotate 'upper-left)
            ;; if we replace the last buffer line that does not end in a
            ;; newline, we use `evil-paste-after' because `evil-delete'
            ;; will move point to the line above
            (when (and (= evil-visual-end (point-max))
                       (/= (char-before (point-max)) ?\n))
              (setq paste-eob t))
            (evil-delete evil-visual-beginning evil-visual-end
                         (evil-visual-type))
            (when (and (eq yank-handler #'evil-yank-line-handler)
                       (not (eq (evil-visual-type) 'line))
                       (not (= evil-visual-end (point-max))))
              (insert "\n"))
            (evil-normal-state)
            (setq new-kill (current-kill 0))
            (current-kill 1))
          (el-patch-remove $paste))
        (el-patch-add $paste)
        (when evil-kill-on-visual-paste
          (kill-new new-kill))
        ;; mark the last paste as visual-paste
        (setq evil-last-paste
              (list (nth 0 evil-last-paste)
                    (nth 1 evil-last-paste)
                    (nth 2 evil-last-paste)
                    (nth 3 evil-last-paste)
                    (nth 4 evil-last-paste)
                    t))))))

(provide 'config-paste)
