;; -*- lexical-binding: t -*-

(require 'smartparens-config)

(eval-when-compile
  (with-demoted-errors "Load error: %s"
    (require 's)
    (require 'evil)
    (require 'diminish)
    (require 'on-parens)
    (require 'config-setq)))

(with-eval-after-load 'smartparens
  (eval-when-compile (require 'smartparens))
  (setq sp-autoinsert-quote-if-followed-by-closing-pair nil
        sp-cancel-autoskip-on-backward-movement nil))

(smartparens-global-mode +1)

(defun my/sp-on-delimiter-p ()
  (ignore-errors (cl-letf (((symbol-function #'message) #'format))
                   (save-excursion
                     (when (or (evil-normal-state-p)
                               (evil-motion-state-p)
                               (evil-operator-state-p))
                       (let ((evil-cross-lines nil))
                         (evil-forward-char)))
                     (if (and (sp-get (sp-get-sexp nil) :beg)
                              (= (point) (sp-get (sp-get-sexp nil) :beg)))
                         (cons (sp-get (sp-get-sexp nil) :beg)
                               (sp-get (sp-get-sexp nil) :end))
                       (if (and (sp-get (sp-get-sexp t) :end)
                                (= (point) (sp-get (sp-get-sexp t) :end)))
                           (cons (sp-get (sp-get-sexp t) :beg)
                                 (sp-get (sp-get-sexp t) :end))
                         nil))))))

(defun nadvice/sp-show--pair-function (&rest _args)
  "If the matching paren is offscreen, show the matching line in the
echo area. Has no effect if the character before point is not of
the syntax class ')'."
  (let ((matching-sexp (my/sp-on-delimiter-p)))
    (when (and matching-sexp
               (not (minibufferp))
               (save-excursion
                 (or (< (progn (move-to-window-line -1) (line-end-position))
                        (cdr matching-sexp))
                     (> (progn (move-to-window-line 0) (point))
                        (car matching-sexp)))))

      (cl-destructuring-bind (current-line matching-line text)
          (cons (line-number-at-pos (point))
                (save-excursion
                  (goto-char (car matching-sexp))
                  (let ((line (thing-at-point 'line)))
                    (when (string-match "\\`[ \t\n\r]+" line)
                        (setq line (replace-match "" t t line)))
                    (when (string-match "[ \t\n\r]+\\'" line)
                        (setq line (replace-match "" t t line)))
                    (list (line-number-at-pos (point))
                          line))))
        (message "Matches %s (%d %s)" text
                 (abs (- current-line matching-line))
                 (if (> matching-line current-line)
                     "below"
                   "above"))))))

(advice-add 'sp-show--pair-function :after #'nadvice/sp-show--pair-function)

(show-smartparens-global-mode +1)
;; (make-variable-buffer-local 'show-paren-mode)
;; (setq-default show-paren-mode nil)

(with-eval-after-load 'paren
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

(add-hook 'minibuffer-setup-hook (lambda ()
                                   (show-smartparens-mode -1)
                                   (show-paren-mode +1)))

;; textobject for the sexp immediately after point
(defun my/evil-next-thing (count &optional _beg _end _type inclusive)
  (ignore-errors
    (save-excursion
      (call-interactively 'sp-select-next-thing count)
      (when (> (point) (mark))
        (exchange-point-and-mark))
      ;; check, it doesn't make sense to take the "inside" of a symbol
      (if (or inclusive
              (not (and (string-match
                         (string (char-after (point))) "[^[:punct:]([{]")
                        (string-match
                         (string (char-before (mark))) "[^[:punct:])\]}]"))))
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
              (not (and
                    (string-match
                     (string (char-after (point)))
                     "[^[:punct:]([{]")
                    (string-match
                     (string (char-before (mark)))
                     "[^[:punct:])\]}]"))))
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
        ((kbd "C-M-S-u") #'on-parens--up-sexp)

        ((kbd "C-M-n") #'on-parens-forward-sexp)
        ((kbd "C-M-p") #'on-parens--backward-sexp-end)))))

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
(with-no-warnings
  (evil-define-motion evil-sp-move ()
    (unless (fboundp 'evil-sp-move-hydra/body)
      (require 'hydra)
      (defhydra evil-sp-move-hydra (nil nil
                                        :hint nil
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
        ("U" on-parens-up-sexp)))
    (evil-sp-move-hydra/body))

  (evil-define-command evil-sp-barfslurp ()
    (unless (fboundp 'evil-sp-barfslurp-hydra/body)
      (require 'hydra)
      (defhydra evil-sp-barfslurp-hydra (nil nil
                                             :hint nil
                                             :idle 0.3
                                             :pre (setq hydra-is-helpful nil)
                                             :post (setq hydra-is-helpful t))
        "[_<_] ← barf  → [_._]  [_>_] ← slurp → [_,_]  [_a_] ← emit  → [_e_]"
        ("," on-parens-forward-slurp)
        ("." on-parens-forward-barf)
        ("<" on-parens-backward-slurp)
        (">" on-parens-backward-barf)
        ("a" sp-absorb-sexp)
        ("e" sp-emit-sexp)))
    (evil-sp-barfslurp-hydra/body)))

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

;; evil normal mode bindings
(with-no-warnings
  (evil-define-motion my/smart-smartparens-tools ()
    (interactive)
    (unless (fboundp 'hydra/smartparens-tools/body)
      (require 'hydra)
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
       (">" evil-sp-backward-slurp-sexp)))

  (hydra/smartparens-tools/body)))

(define-key evil-normal-state-map "gs" #'my/smart-smartparens-tools)

(with-eval-after-load 'smartparens
  (defun my/my-sp-pair-function (id action context)
    (if (eq action 'insert)
        (or (looking-at "[[:space:][:punct:]]")
            (sp-point-before-eol-p id action context))
      t))

  (diminish 'smartparens-mode " σ")

  (sp-pair "(" ")" :when '(my/my-sp-pair-function) :wrap "C-)")
  (sp-pair "{" "}" :when '(my/my-sp-pair-function) :wrap "C-}")
  (sp-pair "[" "]" :when '(my/my-sp-pair-function) :wrap "C-]")
  (sp-pair "\"" "\"" :when '(my/my-sp-pair-function) :wrap "C-\"")
  (sp-pair "'" "'" :when '(my/my-sp-pair-function))

  (define-key evil-insert-state-map (kbd "C-]") nil)
  (define-key evil-normal-state-map (kbd "C-]") nil)
  (define-key evil-motion-state-map (kbd "C-]") nil)
  (define-key evil-emacs-state-map (kbd "C-]") nil)

  ;; disable "'" pairing in text mode, as it's often an apostrophe
  (add-hook 'text-mode-hook
            (lambda ()
              (sp-local-pair major-mode "'" nil :actions nil))))

(provide 'config-smartparens)
