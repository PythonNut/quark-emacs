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
          ;; Also disable the interprogram paste function, so that
          ;; `kill-new' doesn't call it repeatedly.
          (let ((interprogram-cut-function nil)
                (interprogram-paste-function nil))
	    (if (listp interprogram-paste)
                ;; Use `reverse' to avoid modifying external data.
                (mapc #'kill-new (reverse interprogram-paste))
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
  :init
  (define-key evil-emacs-state-map (kbd "C-w") nil)
  (define-key evil-insert-state-map (kbd "C-w") nil)
  (global-set-key (kbd "<remap> <kill-region>")
                  #'whole-line-or-region-kill-region))

(use-package easy-kill
  :init
  (global-set-key (kbd "<remap> <kill-ring-save>")
                  #'easy-kill)

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

  ;; make evil respect whole-line-or-region
  (defun nadvice/evil-paste-line (&rest _args)
    (when (with-demoted-errors "Failed to check text properties for paste. %s"
            (get-text-property 0 'whole-line-or-region (car kill-ring)))
      (setf (car kill-ring)
            (propertize (car kill-ring) 'yank-handler (list 'evil-yank-line-handler)))))

  (advice-add 'evil-paste-before :before #'nadvice/evil-paste-line)
  (advice-add 'evil-paste-after  :before #'nadvice/evil-paste-line)

  (define-key evil-emacs-state-map (kbd "C-y") nil)
  (define-key evil-insert-state-map (kbd "C-y") nil))

(use-package legalese
  :defer-install t
  :commands (legalese))

(defun remove-clipboard-formatting ()
  "A quick command to drop clipboard formatting"
  (interactive)
  (with-temp-buffer
    (yank nil)
    (kill-region (point-min) (point-max))))

(provide 'config-paste)
