;; -*- lexical-binding: t -*-
(eval-when-compile (require 'config-macros))

(use-package avy
  :init
  (advice-add
   'self-insert-command :around
   (my/defun-as-value nadvice/self-insert-command (old-fun &optional arg)
     (interactive "P")
     (cond
      ;; `C-u a` jumps to `a`.
      ((consp arg)
       (avy-goto-char last-command-event))

      ;; `C-- a` jumps to `a` at the beginning of a (sub)word
      ((eq '- arg)
       (avy-goto-subword-1 last-command-event))

      ((or (numberp arg) (not arg))
       (funcall old-fun (or arg 1))))))

  (global-set-key (kbd "<remap> <goto-line>") #'evil-avy-goto-line)
  :config
  (eval-when-compile
    (with-demoted-errors "Load error: %s"
      (require 'avy)))

  (setq avy-background t
        avy-style 'de-bruijn
        avy-timeout-seconds 0.3
        avy-keys (eval-when-compile (string-to-list "jfkdlsaurieowncpqmxzb")))

  (defun my/avy-setup-faces ()
    (set-face-foreground 'avy-background-face "#586e75")

    (set-face-attribute 'avy-lead-face nil
                        :weight 'normal
                        :background nil
                        :foreground "#b58900"
                        :inherit nil)
    (set-face-attribute 'avy-lead-face-0 nil
                        :weight 'extra-bold
                        :background nil
                        :foreground "#dc322f"
                        :inherit nil)
    (set-face-attribute 'avy-lead-face-1 nil
                        :background nil
                        :foreground "#839493"
                        :inherit nil))

  (my/avy-setup-faces)
  (add-hook 'load-theme-hook #'my/avy-setup-faces)

  (use-package evil
    ;; Note: evil is ensured in config-evil
    :ensure nil
    :config
    (eval-when-compile
      (with-demoted-errors "Load error: %s"
        (use-package evil)
        (use-package key-chord)
        (require 'key-chord)
        (require 'evil)))
    (key-chord-define evil-insert-state-map "jk" #'avy-goto-char)
    (key-chord-define evil-insert-state-map "jw" #'evil-avy-goto-word-1)
    (key-chord-define evil-insert-state-map "jl" #'evil-avy-goto-line)

    (key-chord-define evil-emacs-state-map "jk" #'avy-goto-char)
    (key-chord-define evil-emacs-state-map "jw" #'evil-avy-goto-word-1)
    (key-chord-define evil-emacs-state-map "jl" #'evil-avy-goto-line)))

(use-package ace-window
  :init
  ;; bind command to switch to minibuffer
  (defun switch-window-dwim (arg)
    "switch to minibuffer window (if active)"
    (interactive "P")
    (let ((num-windows (length (mapcar #'window-buffer (window-list)))))
      (cond ((= num-windows 1)
             (call-interactively #'split-window-right))
            ((minibufferp)
             (other-window (or arg 1)))
            ((active-minibuffer-window)
             (select-window (active-minibuffer-window)))
            ((or (> (length (visible-frame-list)) 1)
                 (> num-windows 3)
                 (numberp arg))
             (ace-window arg))
            (t
             (other-window (or arg 1))))))

  (global-set-key (kbd "C-'") #'switch-window-dwim)
  (global-set-key (kbd "C-c '") #'switch-window-dwim)

  :config
  (setq aw-keys (eval-when-compile (string-to-list "jfkdlsautnvmircieowpq"))
        aw-ignore-current t
        aw-swap-invert t))

(use-package evil-easymotion
  :init
  (eval-when-compile
    (use-package evil)
    (require 'el-patch)
    (require 'evil-easymotion))

  (el-patch-feature evil-easymotion)

  (el-patch-defvar evilem-map (make-sparse-keymap)
    "Keymap used for the default bindings")

  (el-patch-defun evilem-default-keybindings (prefix)
    "Define easymotions for all motions evil defines by default"
    (define-key evil-motion-state-map (kbd prefix) evilem-map))

  (define-key evilem-map "w" #'evilem-motion-forward-word-begin)
  (define-key evilem-map "W" #'evilem-motion-forward-WORD-begin)
  (define-key evilem-map "e" #'evilem-motion-forward-word-end)
  (define-key evilem-map "E" #'evilem-motion-forward-WORD-end)
  (define-key evilem-map "b" #'evilem-motion-backward-word-begin)
  (define-key evilem-map "B" #'evilem-motion-backward-WORD-begin)
  (define-key evilem-map "ge" #'evilem-motion-backward-word-end)
  (define-key evilem-map "gE" #'evilem-motion-backward-WORD-end)
  (define-key evilem-map "j" #'evilem-motion-next-line)
  (define-key evilem-map "k" #'evilem-motion-previous-line)
  (define-key evilem-map "gj" #'evilem-motion-next-visual-line)
  (define-key evilem-map "gk" #'evilem-motion-previous-visual-line)
  (define-key evilem-map "t" #'evilem-motion-find-char-to)
  (define-key evilem-map "T" #'evilem-motion-find-char-to-backward)
  (define-key evilem-map "f" #'evilem-motion-find-char)
  (define-key evilem-map "F" #'evilem-motion-find-char-backward)
  (define-key evilem-map "[[" #'evilem-motion-backward-section-begin)
  (define-key evilem-map "[]" #'evilem-motion-backward-section-end)
  (define-key evilem-map "]]" #'evilem-motion-forward-section-begin)
  (define-key evilem-map "][" #'evilem-motion-forward-section-end)
  (define-key evilem-map "(" #'evilem-motion-backward-sentence-begin)
  (define-key evilem-map ")" #'evilem-motion-forward-sentence-begin)
  (define-key evilem-map "n" #'evilem-motion-search-next)
  (define-key evilem-map "N" #'evilem-motion-search-previous)
  (define-key evilem-map "*" #'evilem-motion-search-word-forward)
  (define-key evilem-map "#" #'evilem-motion-search-word-backward)
  (define-key evilem-map "-" #'evilem-motion-previous-line-first-non-blank)
  (define-key evilem-map "+" #'evilem-motion-next-line-first-non-blank)

  (define-key evilem-map (kbd "g s f") (evilem-create #'on-parens-forward-sexp-end))
  (define-key evilem-map (kbd "g s b") (evilem-create #'on-parens-backward-sexp))
  (define-key evilem-map (kbd "g s d") (evilem-create #'on-parens-down-sexp))
  (define-key evilem-map (kbd "g s D") (evilem-create #'on-parens-down-sexp-end))
  (define-key evilem-map (kbd "g s u") (evilem-create #'on-parens-up-sexp-end))
  (define-key evilem-map (kbd "g s U") (evilem-create #'on-parens-up-sexp))
  (define-key evilem-map (kbd "g s n") (evilem-create #'on-parens-forward-sexp))
  (define-key evilem-map (kbd "g s p") (evilem-create #'on-parens-backward-sexp-end))

  (define-key evilem-map (kbd "s")
    (evilem-create #'evil-snipe-repeat
                   :pre-hook (save-excursion
                               (ignore-errors
                                 (call-interactively #'evil-snipe-s)))
                   :bind ((evil-snipe-enable-highlight)
                          (evil-snipe-enable-incremental-highlight))))

  (define-key evilem-map (kbd "S")
    (evilem-create #'evil-snipe-repeat-reverse
                   :pre-hook (save-excursion
                               (ignore-errors
                                 (call-interactively #'evil-snipe-S)))
                   :bind ((evil-snipe-enable-highlight)
                          (evil-snipe-enable-incremental-highlight))))

  ;; These are easymotions, but whatever....
  (define-key evilem-map (kbd "l") #'evil-avy-goto-line)
  (define-key evilem-map (kbd "c") #'avy-goto-char-timer)

  (evilem-default-keybindings "SPC")

  (use-package evil-snipe
    ;; Note: evil-snipe is ensured in config-evil-modules
    :ensure nil
    :config
    (cl-macrolet
        ((snipe-repeat-easymotion-forward
          (key)
          `(define-key evil-snipe-parent-transient-map (kbd ,(concat "SPC " key))
             (evilem-create 'evil-snipe-repeat
                            :bind ((evil-snipe-scope 'buffer)
                                   (evil-snipe-enable-highlight)
                                   (evil-snipe-enable-incremental-highlight))))))
      (my/generate-calls-single 'snipe-repeat-easymotion-forward
        '("s" "f" "t")))

    (cl-macrolet
        ((snipe-repeat-easymotion-backward
          (key)
          `(define-key evil-snipe-parent-transient-map (kbd ,(concat "SPC " key))
             (evilem-create 'evil-snipe-repeat-reverse
                            :bind ((evil-snipe-scope 'buffer)
                                   (evil-snipe-enable-highlight)
                                   (evil-snipe-enable-incremental-highlight))))))
      (my/generate-calls-single 'snipe-repeat-easymotion-backward
        '("S" "F" "T")))))

(provide 'config-avy-easymotion)
