;;; =======================================
;;; ansi-color - process ANSI color escapes
;;; =======================================
;;(require 'ansi-color)
(autoload 'ansi-color-apply-on-region "ansi-color")
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)
(setq comint-prompt-read-only 't)

;;; =================================
;;; ansi-term mode - a shell in emacs
;;; =================================

(autoload 'multi-term "multi-term")
(setq multi-term-program "/bin/zsh")

(defvar my-term-shell "/bin/zsh")
(defadvice ansi-term (before force-bash activate)
  (interactive (list my-term-shell)))

;; term
(defface term-color-black
  '((t (:foreground "#555555" :background "#272822"))) "ansi-term black")
(defface term-color-red
  '((t (:foreground "#ff5555" :background "#272822"))) "ansi-term red")
(defface term-color-green
  '((t (:foreground "#55ff55" :background "#272822"))) "ansi-term green")
(defface term-color-yellow
  '((t (:foreground "#ffff55" :background "#272822"))) "ansi-term yellow")
(defface term-color-blue
  '((t (:foreground "#5555ff" :background "#272822"))) "ansi-term blue")
(defface term-color-magenta
  '((t (:foreground "#ff55ff" :background "#272822"))) "ansi-term magenta")
(defface term-color-cyan
  '((t (:foreground "#55ffff" :background "#272822"))) "ansi-term cyan")
(defface term-color-white
  '((t (:foreground "#ffffff" :background "#272822"))) "ansi-term white")
'(term-default-fg-color ((t (:inherit term-color-white))))
'(term-default-bg-color ((t (:inherit term-color-black))))

;; ansi-term colors
(setq ansi-term-color-vector
  [term
    term-color-black
    term-color-red
    term-color-green
    term-color-yellow
    term-color-blue
    term-color-magenta
    term-color-cyan
    term-color-white])

(defun my-term-hook ()
  ;; (add-to-list 'term-bind-key-alist '("\C-c <right>" . multi-term-next))
  ;; (add-to-list 'term-bind-key-alist '("\C-c <left>" . multi-term-prev))
  (define-key term-mode-map (kbd "C-c <right>") 'multi-term-next)
  (define-key term-mode-map (kbd "C-c <left>") 'multi-term-prev)
  (set-face-foreground 'term-bold "green")
  (goto-address-mode)
  (evil-emacs-state)
  (highlight-current-line-on "n")
  (setq yas-dont-activate t)
  (rainbow-delimiters-mode -1)
  (global-unset-key (kbd "<tab>"))
  ;; (global-set-key (kbd "<tab>")
  ;;   '(lambda () (interactive)
  ;;      (term-send-raw-string "\t")))
  (defun term-send-tab ()
    (interactive)
    (term-send-raw-string "\t"))

  (defun term-send-kill-word ()
    (interactive)
    (term-send-raw-string "^[^H]"))

  (define-key term-mode-map (kbd "<tab>") 'term-send-tab)
  (define-key term-mode-map (kbd "<C-backspace>") 'term-send-tab))

(add-hook 'term-mode-hook 'my-term-hook)
(setq default-terminal-coding-system 'utf-8)

