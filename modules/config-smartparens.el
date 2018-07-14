;; -*- lexical-binding: t -*-
(eval-when-compile (require 'config-macros))

(use-package on-parens
  :commands (;; motion
             on-parens-forward-sexp
             on-parens-forward-sexp-end
             on-parens-backward-sexp
             on-parens-backward-sexp-end
             on-parens-up-sexp
             on-parens-up-sexp-end
             on-parens-down-sexp
             on-parens-down-sexp-end
             on-parens-forward-sexp-in-supersexp
             on-parens-backward-sexp-in-supersexp
             ;; editing
             on-parens-forward-slurp
             on-parens-forward-barf
             on-parens-backward-slurp
             on-parens-backward-barf
             on-parens-splice
             on-parens-split-supersexp
             on-parens-join-neighbor-sexp
             on-parens-kill-sexp))

(use-package smartparens
  :init
  (smartparens-global-mode +1)

  :config
  (eval-when-compile
    (with-demoted-errors "Load error: %s"
      (require 'el-patch)))

  (setq sp-cancel-autoskip-on-backward-movement nil)

  (defun my/sp-pair-function (id action context)
    (if (eq action 'insert)
        (or (looking-at (rx (any space punct)))
            (sp-point-before-eol-p id action context))
      t))

  (diminish 'smartparens-mode " σ")

  (sp-pair "(" ")" :when '(my/sp-pair-function) :wrap "C-)")
  (sp-pair "{" "}" :when '(my/sp-pair-function) :wrap "C-}")
  (sp-pair "[" "]" :when '(my/sp-pair-function) :wrap "C-]")
  (sp-pair "\"" "\"" :when '(my/sp-pair-function) :wrap "C-\"")
  (sp-pair "'" "'" :when '(my/sp-pair-function) :unless '(sp-in-comment-p))

  ;; disable "'" pairing in text mode, as it's often an apostrophe
  (add-hook 'text-mode-hook
            (lambda ()
              (sp-local-pair major-mode "'" nil :actions nil)))

  (el-patch-defun sp-show--pair-echo-match (start end olen clen)
    "Print the line of the matching paren in the echo area if not
visible on screen. Needs to be called after the show-pair overlay
has been created."
    (let ((match-positions (list start end olen clen)))
      (when (not (and (equal sp-show-pair-previous-match-positions match-positions)
                      (equal sp-show-pair-previous-point (point))))
        (setq sp-show-pair-previous-match-positions match-positions)
        (setq sp-show-pair-previous-point (point))
        (let* ((visible-start (pos-visible-in-window-p start))
               (visible-end (pos-visible-in-window-p end))
               (where (cond
                       ((not visible-start) start)
                       ((not visible-end) end)))
               (el-patch-add (start-point (point))))
          (when where
            (save-excursion
              (let* ((from (progn (goto-char where) (beginning-of-line) (point)))
                     (to (progn (end-of-line) (point)))
                     (line (buffer-substring from to))
                     (message-log-max)) ;; don't log in messages
                ;; Add smartparens overlay for opening parens
                (let* ((i1 (- start from))
                       (i2 (+ i1 olen)))
                  (when (and (< i1 (length line)) (>= i2 0))
                    (add-face-text-property (max i1 0) (min i2 (length line))
                                            'sp-show-pair-match-face nil line)))
                ;; Add smartparens overlay for closing parens
                (let* ((i1 (- end from 1))
                       (i2 (+ i1 clen)))
                  (when (and (< i1 (length line)) (>= i2 0))
                    (add-face-text-property (max i1 0) (min i2 (length line))
                                            'sp-show-pair-match-face nil line)))
                ;; echo line of match
                (el-patch-swap
                  (message "Matches: %s" (string-trim line))
                  (let ((current-line (line-number-at-pos start-point))
                        (matching-line (line-number-at-pos)))
                    (message "Matches %s%d: %s"
                             (if (> matching-line current-line) "↓" "↑")
                             (abs (- current-line matching-line))
                             (string-trim line)))))))))))

  (show-smartparens-global-mode +1)

  (with-eval-after-load 'evil
    (define-key evil-insert-state-map (kbd "C-]") nil)
    (define-key evil-normal-state-map (kbd "C-]") nil)
    (define-key evil-motion-state-map (kbd "C-]") nil)
    (define-key evil-emacs-state-map (kbd "C-]") nil)

    ;; textobject for the sexp immediately after point
    (defun my/evil-next-thing (count &optional _beg _end _type inclusive)
      (ignore-errors
        (save-excursion
          (call-interactively 'sp-select-next-thing count)
          (when (> (point) (mark))
            (exchange-point-and-mark))
          ;; check, it doesn't make sense to take the "inside" of a symbol
          (if (or inclusive
                  (not (and (string-match-p
                             (rx (not (any punct "([{")))
                             (string (char-after (point))))
                            (string-match-p
                             (rx (not (any punct "}])")))
                             (string (char-before (mark)))))))
              (evil-range (point) (mark))
            (evil-range (1+ (point)) (1- (mark)))))))

    (evil-define-text-object evil-a-next-thing (count &optional beg end _type)
      "Select the range defined by sp-select-next-thing."
      (my/evil-next-thing count beg end type t))
    (evil-define-text-object evil-i-next-thing (count &optional beg end _type)
      "Select the range defined by sp-select-next-thing."
      (my/evil-next-thing count beg end type))

    (define-key evil-outer-text-objects-map "n" #'evil-a-next-thing)
    (define-key evil-inner-text-objects-map "n" #'evil-i-next-thing)

    (defun my/evil-previous-thing (count &optional _beg _end _type inclusive)
      (ignore-errors
        (save-excursion
          (call-interactively 'sp-select-previous-thing count)
          (when (> (point) (mark))
            (exchange-point-and-mark))
          ;; check, it doesn't make sense to take the "inside" of a symbol
          (if (or inclusive
                  (not (and (string-match-p
                             (rx (not (any punct "([{")))
                             (string (char-after (point))))
                            (string-match-p
                             (rx (not (any punct "}])")))
                             (string (char-before (mark)))))))
              (evil-range (point) (mark))
            (evil-range (1+ (point)) (1- (mark)))))))

    (evil-define-text-object evil-a-previous-thing (count &optional beg end _type)
      "Select the range defined by sp-select-previous-thing."
      (interactive "<c>")
      (my/evil-previous-thing count beg end type t))
    (evil-define-text-object evil-i-previous-thing (count &optional beg end _type)
      "Select the range defined by sp-select-previous-thing."
      (interactive "<c>")
      (my/evil-previous-thing count beg end type))

    (define-key evil-outer-text-objects-map "N" #'evil-a-previous-thing)
    (define-key evil-inner-text-objects-map "N" #'evil-i-previous-thing)

    ;; define top level motions bindings
    (cl-macrolet
        ((sp-define-bindings
          (key func)
          `(evil-define-key 'motion sp-keymap ,key ,func)))

      (with-no-warnings
        (my/generate-calls
            'sp-define-bindings
          '(((kbd "C-M-f") #'on-parens-forward-sexp-end)
            ((kbd "C-M-b") #'on-parens-backward-sexp)

            ((kbd "C-M-d") #'on-parens-down-sexp)
            ((kbd "C-M-S-d") #'on-parens-down-sexp-end)

            ((kbd "C-M-u") #'on-parens-up-sexp-end)
            ((kbd "C-M-S-u") #'on-parens-up-sexp)

            ((kbd "C-M-n") #'on-parens-forward-sexp)
            ((kbd "C-M-p") #'on-parens-backward-sexp-end)))))

    (cl-macrolet
        ((sp-define-bindings
          (key func)
          `(progn
             (evil-define-key 'insert sp-keymap ,key ,func)
             (evil-define-key 'emacs sp-keymap ,key ,func)

             (define-key minibuffer-local-map ,key ,func)
             (define-key minibuffer-local-ns-map ,key ,func)
             (define-key minibuffer-local-completion-map ,key ,func)
             (define-key minibuffer-local-must-match-map ,key ,func))))

      (with-no-warnings
        (my/generate-calls
            'sp-define-bindings
          '(((kbd "C-M-f") #'sp-forward-sexp)
            ((kbd "C-M-b") #'sp-backward-sexp)

            ((kbd "C-M-d") #'sp-down-sexp)
            ((kbd "C-M-S-d") #'sp-backward-down-sexp)

            ((kbd "C-M-u") #'sp-up-sexp)
            ((kbd "C-M-S-u") #'sp-backward-up-sexp)

            ((kbd "C-M-n") #'sp-next-sexp)
            ((kbd "C-M-p") #'sp-previous-sexp)))))

    (cl-macrolet
        ((sp-define-bindings
          (key func)
          `(evil-define-key 'normal sp-keymap ,key ,func)))

      (with-no-warnings
        (my/generate-calls
            'sp-define-bindings
          '(((kbd "C-M-k") #'on-parens-kill-sexp)

            ((kbd "C-M-t") #'sp-transpose-sexp)

            ((kbd "M-(") #'sp-select-previous-thing)
            ((kbd "M-)") #'sp-select-next-thing)

            ((kbd "C-+") #'sp-rewrap-sexp)
            ((kbd "M-<delete>") #'on-parens-kill-sexp)
            ((kbd "M-<backspace>") #'sp-backward-kill-sexp)
            ((kbd "S-<backspace>") #'sp-backward-unwrap-sexp)

            ((kbd "C-M-a") #'sp-absorb-sexp)
            ((kbd "C-M-e") #'sp-emit-sexp)

            ((kbd "C-M-,") #'on-parens-forward-slurp)
            ((kbd "C-M-.") #'on-parens-forward-barf)

            ((kbd "M-<") #'on-parens-backward-slurp)
            ((kbd "M->") #'on-parens-backward-barf)))))

    (cl-macrolet
        ((sp-define-bindings
          (key func)
          `(progn
             (evil-define-key 'insert sp-keymap ,key ,func)
             (evil-define-key 'emacs sp-keymap ,key ,func)

             (define-key minibuffer-local-map ,key ,func)
             (define-key minibuffer-local-ns-map ,key ,func)
             (define-key minibuffer-local-completion-map ,key ,func)
             (define-key minibuffer-local-must-match-map ,key ,func))))

      (with-no-warnings
        (my/generate-calls
            'sp-define-bindings
          '(((kbd "C-M-k") #'sp-kill-sexp)

            ((kbd "C-M-t") #'sp-transpose-sexp)

            ((kbd "M-(") #'sp-select-previous-thing)
            ((kbd "M-)") #'sp-select-next-thing)

            ((kbd "C-+") #'sp-rewrap-sexp)
            ((kbd "M-<delete>") #'sp-kill-sexp)
            ((kbd "M-<backspace>") #'sp-backward-kill-sexp)
            ((kbd "S-<backspace>") #'sp-backward-unwrap-sexp)

            ((kbd "C-M-a") #'sp-absorb-sexp)
            ((kbd "C-M-e") #'sp-emit-sexp)

            ((kbd "C-M-,") #'sp-forward-slurp-sexp)
            ((kbd "C-M-.") #'sp-forward-barf-sexp)

            ((kbd "M-<") #'sp-backward-slurp-sexp)
            ((kbd "M->") #'sp-backward-barf-sexp)))))

    ;; allow quick repetition since normal state key chains are awkward
    (defhydra evil-sp-move-hydra (:hint nil
                                        :idle 0.3
                                        :pre (setq hydra-is-helpful nil)
                                        :post (setq hydra-is-helpful t))
      ("f" on-parens-forward-sexp-end)
      ("b" on-parens-backward-sexp)
      ("n" on-parens-forward-sexp)
      ("p" on-parens-backward-sexp-end)
      ("d" on-parens-down-sexp)
      ("D" on-parens-down-sexp-end)
      ("u" on-parens-up-sexp-end)
      ("U" on-parens-up-sexp))

    (defhydra evil-sp-barfslurp-hydra (:hint nil
                                             :idle 0.3
                                             :pre (setq hydra-is-helpful nil)
                                             :post (setq hydra-is-helpful t))
      "[_<_] ← barf  → [_._]  [_>_] ← slurp → [_,_]  [_a_] ← emit  → [_e_]"
      ("," on-parens-forward-slurp)
      ("." on-parens-forward-barf)
      ("<" on-parens-backward-slurp)
      (">" on-parens-backward-barf)
      ("a" sp-absorb-sexp)
      ("e" sp-emit-sexp))

    (evil-define-motion evil-sp-move ()
      (evil-sp-move-hydra/body))

    (evil-define-command evil-sp-barfslurp ()
      (evil-sp-barfslurp-hydra/body))

    (evil-define-motion evil-sp-forward-sexp (&optional arg &rest _args)
      (on-parens-forward-sexp-end (or arg 1))
      (evil-sp-move))

    (evil-define-motion evil-sp-backward-sexp (&optional arg &rest _args)
      (on-parens-backward-sexp (or arg 1))
      (evil-sp-move))

    (evil-define-motion evil-sp-next-sexp (&optional arg &rest _args)
      (on-parens-forward-sexp (or arg 1))
      (evil-sp-move))

    (evil-define-motion evil-sp-previous-sexp (&optional arg &rest _args)
      (on-parens-backward-sexp-end (or arg 1))
      (evil-sp-move))

    (evil-define-motion evil-sp-down-sexp (&optional arg &rest _args)
      (on-parens-down-sexp (or arg 1))
      (evil-sp-move))

    (evil-define-motion evil-sp-backward-down-sexp (&optional arg &rest _args)
      (on-parens-down-sexp-end (or arg 1))
      (evil-sp-move))

    (evil-define-motion evil-sp-up-sexp (&optional arg &rest _args)
      (on-parens-up-sexp-end (or arg 1))
      (evil-sp-move))

    (evil-define-motion evil-sp-backward-up-sexp (&optional arg &rest _args)
      (on-parens-up-sexp (or arg 1))
      (evil-sp-move))

    (evil-define-command evil-sp-forward-slurp-sexp (&optional arg &rest _args)
      (on-parens-forward-slurp (or arg 1))
      (evil-sp-barfslurp))

    (evil-define-command evil-sp-forward-barf-sexp (&optional arg &rest _args)
      (on-parens-forward-barf (or arg 1))
      (evil-sp-barfslurp))

    (evil-define-command evil-sp-backward-slurp-sexp (&optional arg &rest _args)
      (on-parens-backward-slurp (or arg 1))
      (evil-sp-barfslurp))

    (evil-define-command evil-sp-backward-barf-sexp (&optional arg &rest _args)
      (on-parens-backward-barf (or arg 1))
      (evil-sp-barfslurp))

    (defhydra hydra/smartparens-tools (:color blue :hint nil :idle 0.3)
      "
[_U_] ↰↱ [_u_]  [_K_] ←  kill  → [_k_]  [_<_] ← barf  → [_._]   [_s_] split
[_b_] ←→ [_f_]  [_p_] ←  next  → [_n_]  [_>_] ← slurp → [_,_]   [_j_] join
[_D_] ↲↳ [_d_]  [_W_] ← unwrap → [_w_]  [_a_] ← emit  → [_e_]   [_t_] trans"
      ("f" evil-sp-forward-sexp)
      ("b" evil-sp-backward-sexp)
      ("d" evil-sp-down-sexp)
      ("D" evil-sp-backward-down-sexp)
      ("u" evil-sp-up-sexp)
      ("U" evil-sp-backward-up-sexp)
      ("n" evil-sp-next-sexp)
      ("p" evil-sp-previous-sexp)
      ("k" on-parens-kill-sexp)
      ("K" sp-backward-kill-sexp)
      ("w" sp-unwrap-sexp)
      ("W" sp-backward-unwrap-sexp)
      ("s" on-parens-split-supersexp)
      ("j" sp-join-sexp)
      ("a" sp-absorb-sexp)
      ("e" sp-emit-sexp)
      ("t" sp-transpose-sexp)
      ("," evil-sp-forward-slurp-sexp)
      ("." evil-sp-forward-barf-sexp)
      ("<" evil-sp-backward-barf-sexp)
      (">" evil-sp-backward-slurp-sexp))

    ;; evil normal mode bindings
    (with-no-warnings
      (evil-define-motion my/smart-smartparens-tools ()
        (hydra/smartparens-tools/body)))

    (define-key evil-normal-state-map "gs" #'my/smart-smartparens-tools)))

(use-package paren
  :init
  (add-hook 'minibuffer-setup-hook (lambda ()
                                     (show-smartparens-mode -1)
                                     (show-paren-mode +1)))

  :config
  (defun nadvice/show-paren-mode (old-fun &rest args)
    ;; http://emacs.stackexchange.com/questions/12532/buffer-local-idle-timer
    (cl-letf* ((old-run-with-idle-timer (symbol-function #'run-with-idle-timer))
               ((symbol-function #'run-with-idle-timer)
                (lambda (&rest args)
                  (cl-destructuring-bind (_secs _repeat function &rest rest)
                      args
                    (let* (;; Chicken and egg problem.
                           (fns (make-symbol "local-idle-timer"))
                           (timer (apply old-run-with-idle-timer args))
                           (fn `(lambda (&rest args)
                                  (if (active-minibuffer-window)
                                      (with-current-buffer ,(current-buffer)
                                        (apply (function ,function) args))
                                    (cancel-timer ,timer)))))
                      (fset fns fn)
                      timer)))))
      (apply old-fun args)))

  (advice-add 'show-paren-mode :around #'nadvice/show-paren-mode))

(provide 'config-smartparens)
