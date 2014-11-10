(require 'evil)

(eval-when-compile
  (progn
    (require 'cl)
    (require 'evil)
    (require 'evil-leader)))

(setq 
  evil-want-C-w-delete nil
  evil-want-C-w-in-emacs-state nil
  evil-ex-complete-emacs-commands t
  evil-want-fine-undo t
  evil-search-module 'evil-search

  evil-magic 'very-magic)

(setq-default
  evil-symbol-word-search t)

(evil-mode +1)

(require 'evil-leader)
(global-evil-leader-mode +1)
(setq 
  evil-leader/leader "," 
  evil-leader/in-all-states t)

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

;; define a key to switch to emacs state
(define-key evil-insert-state-map (kbd "C-M-z") 'evil-emacs-state)
(define-key evil-emacs-state-map (kbd "C-M-z") 'evil-insert-state)
(define-key evil-normal-state-map (kbd "C-M-z") 'evil-insert-state)
(define-key evil-motion-state-map (kbd "C-M-z") 'evil-insert-state)
(define-key evil-visual-state-map (kbd "C-M-z") 'evil-insert-state)

;; indent pasted regions in evil
(defadvice evil-paste-before (around auto-indent activate)
  (evil-indent (point) (+ (point) (length ad-do-it))))

(defadvice evil-paste-after (around auto-indent activate)
  (evil-indent (point) (+ (point) (length ad-do-it))))

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

;; exit isearch with jj and kk 
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

(define-key evil-insert-state-map (kbd "C-e") 'evil-end-of-visual-line)
(setq evil-replace-state-cursor '("#884444" box))

;; escape out of help mode buffers
(add-hook 'help-mode-hook
  (lambda ()
    (key-chord-define help-mode-map "jj" 'quit-window)))

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

(define-key evil-normal-state-map (kbd "[ <SPC>") 'evil-open-above-normal)
(define-key evil-normal-state-map (kbd "] <SPC>") 'evil-open-below-normal)

;; bind a fallback keybind to goto normal state
(define-key evil-insert-state-map (kbd "C-j") 'evil-normal-state)
(define-key evil-emacs-state-map (kbd "C-j") 'evil-normal-state)
(define-key evil-visual-state-map (kbd "C-j") 'evil-normal-state)
(define-key evil-replace-state-map (kbd "C-j") 'evil-normal-state)

;; let oo open a new paragraph
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

;; let Y == y$, similar to D = d$ 
(defun evil-yank-to-end-of-line ()
  (interactive)
  (evil-yank (point) (point-at-eol)))

(define-key evil-normal-state-map "Y" 'evil-yank-to-end-of-line)

;; break bad undo habits
(define-key evil-normal-state-map "U" 'undo-tree-visualize)
(define-key evil-normal-state-map (kbd "C-z")
  (lambda ()
    (interactive)
    (message "use u.")))

(load-library "config-evil-modules")
(load-library "config-evil-textobjects")
