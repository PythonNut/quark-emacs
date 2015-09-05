(require 'evil)

(eval-when-compile
  (with-demoted-errors
    (require 'cl)
    (require 'evil)
    (require 'evil-surround)))

(setq
  evil-auto-indent t
  evil-ex-complete-emacs-commands t
  evil-magic 'very-magic
  evil-search-module 'evil-search
  evil-shift-width 2
  evil-toggle-key "C-M-z"
  evil-want-C-w-delete nil
  evil-want-C-w-in-emacs-state nil
  evil-want-fine-undo t)

(fset 'evil-visual-update-x-selection 'ignore)

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
(defun nadvice/evil-paste-indent (old-fun &rest args)
  (indent-region (point) (+ (point) (length (apply old-fun args)))))

(advice-add 'evil-paste-before :around #'nadvice/evil-paste-indent)
(advice-add 'evil-paste-after :around #'nadvice/evil-paste-indent)

(lexical-let ((my/evil-mode-line-face-cookies nil))
  (defun my/evil-set-mode-line-face ()
    (let ((color
            (if (< (display-color-cells) 256)
              (pcase evil-state
                (`normal  '("white" . "blue"))
                (`emacs   '("white" . "green"))
                (`insert  '("black" . "grey"))
                (`visual  '("white" . "cyan"))
                (`replace '("white" . "red"))
                (other    '("grey" . "black")))
              (pcase evil-state
                (`normal  '("#586e75" . "#eee8d5"))
                (`emacs   '("#859900" . "#eee8d5"))
                (`insert  '("#93a1a1" . "#073642"))
                (`visual  '("#268bd2" . "#eee8d5"))
                (`replace '("#dc322f" . "#eee8d5"))
                (other    '("grey70" . "black"))))))

      (mapc #'face-remap-remove-relative my/evil-mode-line-face-cookies)
      (setq my/evil-mode-line-face-cookies
        (list
          (face-remap-add-relative 'mode-line
            `((:foreground ,(cdr color) :background ,(car color)) mode-line))

          (face-remap-add-relative 'mode-line-buffer-id
            `((:foreground ,(cdr color)) mode-line-buffer-id)))))))

;; Change modeline color by Evil state
(add-hook 'post-command-hook #'my/evil-set-mode-line-face)

(define-key evil-insert-state-map (kbd "C-s")
  (lambda ()
    (interactive)
    (evil-normal-state)
    (call-interactively 'evil-search-forward)))

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

(defun my/evil-window-hydra-wrapper ()
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
  (my/evil-window-hydra-wrapper))

(evil-define-command evil-window-down-smart ()
  (interactive)
  (with-demoted-errors (call-interactively #'evil-window-down))
  (my/evil-window-hydra-wrapper))

(evil-define-command evil-window-up-smart ()
  "A `hydra' enabled `evil-window-left'"
  (interactive)
  (with-demoted-errors (call-interactively #'evil-window-up))
  (my/evil-window-hydra-wrapper))

(evil-define-command evil-window-right-smart ()
  "A `hydra' enabled `evil-window-left'"
  (interactive)
  (with-demoted-errors (call-interactively #'evil-window-right))
  (my/evil-window-hydra-wrapper))

(define-key evil-normal-state-map (kbd "C-w h") 'evil-window-left-smart)
(define-key evil-normal-state-map (kbd "C-w j") 'evil-window-down-smart)
(define-key evil-normal-state-map (kbd "C-w k") 'evil-window-up-smart)
(define-key evil-normal-state-map (kbd "C-w l") 'evil-window-right-smart)

(require 'config-evil-modules)
(require 'config-evil-textobjects)

(provide 'config-evil)
