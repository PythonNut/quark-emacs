(require 'evil)

(eval-when-compile
  (with-demoted-errors
    (require 'cl)
    (require 'evil)
    (require 'evil-surround)))

(setq
  evil-toggle-key "C-M-z"
  evil-want-C-w-delete nil
  evil-want-C-w-in-emacs-state nil
  evil-ex-complete-emacs-commands t
  evil-want-fine-undo t
  evil-search-module 'evil-search
  evil-magic 'very-magic
  evil-shift-width 2)

(setq-default
  evil-symbol-word-search t)

(evil-mode +1)

(define-key evil-visual-state-map "v" #'er/expand-region)

;; define C-<arrow> for terminals
(global-set-key (kbd "M-[ d") #'left-word)
(global-set-key (kbd "M-[ c") #'right-word)
(global-set-key (kbd "M-[ a") #'backward-paragraph)
(global-set-key (kbd "M-[ b") #'forward-paragraph)

(define-key evil-normal-state-map (kbd "<down>") #'evil-next-visual-line)
(define-key evil-motion-state-map (kbd "<up>") #'evil-previous-visual-line)
(define-key evil-motion-state-map (kbd "<down>") #'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "<up>") #'evil-previous-visual-line)

(key-chord-define evil-insert-state-map  "jj" #'evil-normal-state)
(key-chord-define evil-replace-state-map "jj" #'evil-normal-state)
(key-chord-define evil-emacs-state-map   "jj" #'evil-normal-state)

(key-chord-define evil-insert-state-map  "kk" #'evil-normal-state)
(key-chord-define evil-replace-state-map "kk" #'evil-normal-state)
(key-chord-define evil-emacs-state-map   "kk" #'evil-normal-state)

(key-chord-define evil-insert-state-map ";'" #'evil-ex)
(key-chord-define evil-emacs-state-map ";'" #'evil-ex)

(global-set-key (kbd "C-<backspace>") #'evil-delete-backward-word)

;; Esc quits from everything
(define-key evil-normal-state-map [escape] #'keyboard-quit)
(define-key evil-emacs-state-map [escape] #'evil-normal-state)
(define-key evil-visual-state-map [escape] #'keyboard-quit)
(define-key evil-motion-state-map [escape] #'evil-normal-state)
(define-key evil-operator-state-map [escape] #'evil-normal-state)
(define-key minibuffer-local-map [escape] #'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] #'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] #'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] #'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] #'minibuffer-keyboard-quit)

;; define a key to switch to emacs state
(define-key evil-insert-state-map (kbd "C-M-z") #'evil-emacs-state)
(define-key evil-emacs-state-map (kbd "C-M-z") #'evil-normal-state)
(define-key evil-normal-state-map (kbd "C-M-z") #'evil-emacs-state)
(define-key evil-motion-state-map (kbd "C-M-z") #'evil-emacs-state)
(define-key evil-visual-state-map (kbd "C-M-z") #'evil-emacs-state)
(define-key evil-motion-state-map (kbd "C-z") nil)

;; indent pasted regions in evil
(defadvice evil-paste-before
  (around auto-indent activate preactivate compile)
  (indent-region (point) (+ (point) (length ad-do-it))))

(defadvice evil-paste-after
  (around auto-indent activate preactivate compile)
  (indent-region (point) (+ (point) (length ad-do-it))))

;;; Change modeline color by Evil state
(lexical-let ((default-color (cons (face-background 'mode-line)
                               (face-foreground 'mode-line))))
  (add-hook 'post-command-hook
    (lambda ()
      (let ((color (if (/= (display-color-cells) 8)
                     (cond ((minibufferp) default-color)
                       ((evil-normal-state-p) '("#586e75" . "#eee8d5"))
                       ((evil-emacs-state-p)  '("#859900" . "#eee8d5"))
                       ((evil-insert-state-p)  '("#93a1a1" . "#073642"))
                       ((evil-visual-state-p) '("#268bd2" . "#eee8d5"))
                       ((evil-replace-state-p) '("#dc322f" . "#eee8d5"))
                       (t '("grey70" . "black")))
                     (cond ((minibufferp) default-color)
                       ((evil-normal-state-p) '("white" . "blue"))
                       ((evil-emacs-state-p)  '("white" . "green"))
                       ((evil-insert-state-p)  '("black" . "grey"))
                       ((evil-visual-state-p) '("white" . "cyan"))
                       ((evil-replace-state-p) '("white" . "red"))
                       (t '("grey" . "black"))))))
        (set-face-background 'mode-line (first color))
        (set-face-foreground 'mode-line (rest color))
        (set-face-foreground 'mode-line-buffer-id (rest color))))))

;; switch to insert state if I set an emacs-style mark
(define-key evil-normal-state-map (kbd "C-SPC")
  (lambda () (interactive)
    (evil-insert-state)
    (execute-kbd-macro (kbd "C-SPC"))))

(define-key evil-normal-state-map (kbd "C-RET")
  (lambda ()
    (interactive)
    (evil-insert-state)
    (cua-set-rectangle-mark)))

(define-key evil-insert-state-map (kbd "C-s")
  (lambda ()
    (interactive)
    (evil-normal-state)
    (call-interactively 'evil-search-forward)))

(define-key evil-insert-state-map (kbd "C-e") #'evil-end-of-visual-line)
(setq evil-replace-state-cursor '("#884444" box))

;; open line and stay in normal mode
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

(define-key evil-normal-state-map (kbd "[ <SPC>") #'evil-open-above-normal)
(define-key evil-normal-state-map (kbd "] <SPC>") #'evil-open-below-normal)

(define-key evil-normal-state-map "U" #'undo-tree-visualize)

(defun evil-window-hydra-wrapper ()
  (unless (fboundp 'evil-window-hydra/body)
    (require 'hydra)
    (with-no-warnings
      (defhydra evil-window-hydra ()
        "switch window"
        ("h" evil-window-left-smart "left")
        ("j" evil-window-down-smart "down")
        ("k" evil-window-up-smart "up")
        ("l" evil-window-right-smart "right")
        ("RET" nil "quit"))))
  (with-no-warnings
    (evil-window-hydra/body)))

(evil-define-command evil-window-left-smart ()
  "A `hydra' enabled `evil-window-left'"
  (interactive)
  (with-demoted-errors (call-interactively #'evil-window-left))
  (evil-window-hydra-wrapper))

(evil-define-command evil-window-down-smart ()
  (interactive)
  (with-demoted-errors (call-interactively #'evil-window-down))
  (evil-window-hydra-wrapper))

(evil-define-command evil-window-up-smart ()
  "A `hydra' enabled `evil-window-left'"
  (interactive)
  (with-demoted-errors (call-interactively #'evil-window-up))
  (evil-window-hydra-wrapper))

(evil-define-command evil-window-right-smart ()
  "A `hydra' enabled `evil-window-left'"
  (interactive)
  (with-demoted-errors (call-interactively #'evil-window-right))
  (evil-window-hydra-wrapper))

(define-key evil-normal-state-map (kbd "C-w h") 'evil-window-left-smart)
(define-key evil-normal-state-map (kbd "C-w j") 'evil-window-down-smart)
(define-key evil-normal-state-map (kbd "C-w k") 'evil-window-up-smart)
(define-key evil-normal-state-map (kbd "C-w l") 'evil-window-right-smart)

(require 'config-evil-modules)
(require 'config-evil-textobjects)


;; cross lines on (list f F t T ; ,)
;; consider setting evil-cross-lines to t for laughs
(defadvice evil-repeat-find-char
  (around cross-lines activate preactivate compile)
  (let ((evil-cross-lines t))
    ad-do-it))

(defadvice evil-repeat-find-char-reverse
  (around cross-lines activate preactivate compile)
  (let ((evil-cross-lines t))
    ad-do-it))

(defadvice evil-find-char
  (around cross-lines activate preactivate compile)
  (let ((evil-cross-lines t))
    ad-do-it))

(defadvice evil-find-char-to
  (around cross-lines activate preactivate compile)
  (let ((evil-cross-lines t))
    ad-do-it))

(defadvice evil-find-char-backward
  (around cross-lines activate preactivate compile)
  (let ((evil-cross-lines t))
    ad-do-it))

(defadvice evil-find-char-to-backward
  (around cross-lines activate preactivate compile)
  (let ((evil-cross-lines t))
    ad-do-it))

(provide 'config-evil)
