;; -*- lexical-binding: t -*-

(use-package key-chord
  :init
  (defun fix-key-chords ()
    (interactive)
    (key-chord-mode -1)
    (key-chord-mode +1))
  
  :config
  (cl-letf (((symbol-function #'message)
             (lambda (&rest args)
               (when args
                 (apply #'format args)))))
    (key-chord-mode +1)))

(use-package evil
  :init (evil-mode +1)
  :config
  (setq evil-auto-indent t
        evil-ex-complete-emacs-commands t
        evil-magic 'very-magic
        evil-search-module 'evil-search
        evil-shift-width 2
        evil-toggle-key "C-M-z"
        evil-want-C-w-delete nil
        evil-want-C-w-in-emacs-state nil
        evil-want-fine-undo t
        evil-normal-state-cursor '("#8a8a8a" box)
        evil-operator-state-cursor '("#8a8a8a" evil-half-cursor)
        evil-replace-state-cursor '("#884444" box)
        evil-insert-state-cursor `("#8a8a8a" bar)
        evil-emacs-state-cursor `("#5f8700" bar))

  (fset 'evil-visual-update-x-selection 'ignore)

  (setq-default evil-symbol-word-search t)

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

  (let ((my/evil-mode-line-face-cookies))
    (defun my/evil-set-mode-line-face (&rest _args)
      (cl-destructuring-bind (bg-color fg-color)
          (if (< (display-color-cells) 256)
              (pcase evil-state
                (`normal  '("white" "blue"))
                (`emacs   '("white" "green"))
                (`insert  '("black" "grey"))
                (`visual  '("white" "cyan"))
                (`replace '("white" "red"))
                (_         '("grey"  "black")))
            (pcase evil-state
              (`normal  '("#586e75" "#eee8d5"))
              (`emacs   '("#859900" "#eee8d5"))
              (`insert  '("#93a1a1" "#073642"))
              (`visual  '("#268bd2" "#eee8d5"))
              (`replace '("#dc322f" "#eee8d5"))
              (_        '("grey70"  "black"))))

        (mapc #'face-remap-remove-relative my/evil-mode-line-face-cookies)
        (setq my/evil-mode-line-face-cookies
              (list (face-remap-add-relative
                     'mode-line
                     `((:foreground ,fg-color :background ,bg-color)
                       mode-line))
                    (face-remap-add-relative
                     'mode-line-buffer-id
                     `((:foreground ,fg-color)
                       mode-line-buffer-id)))))))

  ;; Change modeline color by Evil state
  (advice-add 'evil-generate-mode-line-tag
              :after
              #'my/evil-set-mode-line-face)

  ;; open line and stay in normal mode
  (evil-define-command evil-open-below-normal (arg)
    (interactive "p")
    (let ((evil-echo-state))
      (evil-with-state 'normal
        (evil-open-below arg))))

  (evil-define-command evil-open-above-normal (arg)
    (interactive "p")
    (let ((evil-echo-state))
      (evil-with-state 'normal
        (evil-open-above arg))))

  (define-key evil-normal-state-map (kbd "[ <SPC>") #'evil-open-above-normal)
  (define-key evil-normal-state-map (kbd "] <SPC>") #'evil-open-below-normal)

  (define-key evil-normal-state-map "U" #'undo-tree-visualize)

  (use-package hydra
    :demand t
    :config
    (defhydra evil-window-hydra ()
      "switch window"
      ("h" evil-window-left-smart "left")
      ("j" evil-window-down-smart "down")
      ("k" evil-window-up-smart "up")
      ("l" evil-window-right-smart "right")
      ("RET" nil "quit")))

  (evil-define-command evil-window-left-smart ()
    "A `hydra' enabled `evil-window-left'"
    (with-demoted-errors "%s"
      (call-interactively #'evil-window-left))
    (evil-window-hydra/body))

  (evil-define-command evil-window-down-smart ()
    (with-demoted-errors "%s"
      (call-interactively #'evil-window-down))
    (evil-window-hydra/body))

  (evil-define-command evil-window-up-smart ()
    "A `hydra' enabled `evil-window-left'"
    (with-demoted-errors "%s"
      (call-interactively #'evil-window-up))
    (evil-window-hydra/body))

  (evil-define-command evil-window-right-smart ()
    "A `hydra' enabled `evil-window-left'"
    (with-demoted-errors "%s"
      (call-interactively #'evil-window-right))
    (evil-window-hydra/body))

  (define-key evil-normal-state-map (kbd "C-w h") #'evil-window-left-smart)
  (define-key evil-normal-state-map (kbd "C-w j") #'evil-window-down-smart)
  (define-key evil-normal-state-map (kbd "C-w k") #'evil-window-up-smart)
  (define-key evil-normal-state-map (kbd "C-w l") #'evil-window-right-smart)

  (evil-define-command evil-delete-backward-word-smart ()
    "Delete previous word."
    (require 'subword)
    (if (and (bolp) (not (bobp)))
        (progn
          (unless evil-backspace-join-lines (user-error "Beginning of line"))
          (delete-char -1))
      (evil-delete (max
                    (let ((word-point (save-excursion
                                        (subword-backward)
                                        (point))))
                      (if (= word-point (save-excursion
                                          (backward-char)
                                          (point)))
                          (save-excursion
                            (backward-char)
                            (subword-backward)
                            (point))
                        word-point))
                    (line-beginning-position))
                   (point)
                   'exclusive
                   nil)))

  (global-set-key (kbd "<C-backspace>") #'evil-delete-backward-word-smart)
  (define-key evil-insert-state-map (kbd "C-t") #'transpose-chars)
  (define-key evil-insert-state-map (kbd "C-d") #'evil-delete)
  (global-set-key (kbd "<remap> <kill-whole-line>") #'evil-delete-whole-line)

  (define-key evil-insert-state-map (kbd "<insert>") #'evil-replace-state)
  (define-key evil-replace-state-map (kbd "<insert>") #'evil-insert-state)

  (evil-define-command evil-cycle-spacing (&optional count)
    (cycle-spacing (or count 1)))

  (global-set-key (kbd "<remap> <just-one-space>") #'evil-cycle-spacing)
  (global-set-key (kbd "<remap> <delete-horizontal-space>") #'evil-cycle-spacing)

  (with-eval-after-load 'session
    (defun my/evil--jumps-savehist-load ()
      (let ((ring (make-ring evil-jumps-max-length)))
        (cl-loop for jump in (reverse evil-jumps-history)
                 do (ring-insert ring jump))
        (setf (evil-jumps-struct-ring (evil--jumps-get-current)) ring)))
    (add-hook 'session-after-load-save-file-hook #'my/evil--jumps-savehist-load)
    (defun nadvice/session-save-session/evil--jumps (&rest _args)
      (evil--jumps-savehist-sync))
    (advice-add 'session-save-session :before
                #'nadvice/session-save-session/evil--jumps)
    (add-to-list 'session-globals-include 'evil-jumps-history))

  (require 'config-evil-modules)
  (require 'config-evil-textobjects))

(provide 'config-evil)
