(require 'smartparens-config)

(eval-when-compile
  (with-demoted-errors
    (require 'evil)
    (require 'key-chord)
    (require 'hydra)
    (require 'smartparens)
    (require 'diminish)
    (require 'config-modes)))

(setq
  sp-autoinsert-quote-if-followed-by-closing-pair nil
  sp-autoescape-string-quote nil
  sp-autoescape-string-quote-if-empty nil
  sp-cancel-autoskip-on-backward-movement nil)

(smartparens-global-mode +1)
(raise-minor-mode-map-alist 'smartparens-mode-map)

;; define smartparens motions as evil motions
(evil-define-motion evil-forward-sexp (count)
  (sp-forward-sexp count))
(evil-define-motion evil-backward-sexp (count)
  (sp-backward-sexp count))
(evil-define-motion evil-up-sexp (count)
  (sp-up-sexp count))
(evil-define-motion evil-backward-up-sexp (count)
  (sp-backward-up-sexp count))
(evil-define-motion evil-down-sexp (count)
  (sp-down-sexp count))
(evil-define-motion evil-backward-down-sexp (count)
  (sp-backward-down-sexp count))
(evil-define-motion evil-next-sexp (count)
  (sp-next-sexp count))
(evil-define-motion evil-previous-sexp (count)
  (sp-previous-sexp count))

;; textobject for the sexp immediately after point
(defun evil-next-thing (count &optional beg end type inclusive)
  (ignore-errors
    (save-excursion
      (call-interactively 'sp-select-next-thing count)
      (if (> (point) (mark))
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

(evil-define-text-object evil-a-next-thing (count &optional beg end type)
  "Select the range defined by sp-select-next-thing."
  (evil-next-thing count beg end type t))
(evil-define-text-object evil-i-next-thing (count &optional beg end type)
  "Select the range defined by sp-select-next-thing."
  (evil-next-thing count beg end type))

(define-key evil-outer-text-objects-map "n" #'evil-a-next-thing)
(define-key evil-inner-text-objects-map "n" #'evil-i-next-thing)

(defun evil-previous-thing (count &optional beg end type inclusive)
  (ignore-errors
    (save-excursion
      (call-interactively 'sp-select-previous-thing count)
      (if (> (point) (mark))
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

(evil-define-text-object evil-a-previous-thing (count &optional beg end type)
  "Select the range defined by sp-select-previous-thing."
  (interactive "<c>")
  (evil-previous-thing count beg end type t))
(evil-define-text-object evil-i-previous-thing (count &optional beg end type)
  "Select the range defined by sp-select-previous-thing."
  (interactive "<c>")
  (evil-previous-thing count beg end type))

(define-key evil-outer-text-objects-map "N" #'evil-a-previous-thing)
(define-key evil-inner-text-objects-map "N" #'evil-i-previous-thing)

;; define top level motions bindings
(cl-macrolet
  ((sp-define-bindings (key func)
     `(progn
        (evil-define-key 'motion sp-keymap ,key ,func))))

  (with-no-warnings
    (generate-calls sp-define-bindings
      (
        ((kbd "C-M-f") #'evil-forward-sexp)
        ((kbd "C-M-b") #'evil-backward-sexp)

        ((kbd "C-M-d") #'evil-down-sexp)
        ((kbd "C-M-S-d") #'evil-backward-down-sexp)

        ((kbd "C-M-u") #'evil-up-sexp)
        ((kbd "C-M-S-u") #'evil-backward-up-sexp)

        ((kbd "C-M-n") #'evil-next-sexp)
        ((kbd "C-M-p") #'evil-previous-sexp)))))

(cl-macrolet
  ((sp-define-bindings (key func)
     `(progn
        (evil-define-key 'insert sp-keymap ,key ,func)
        (evil-define-key 'emacs sp-keymap ,key ,func)

        (define-key minibuffer-local-map ,key ,func)
        (define-key minibuffer-local-ns-map ,key ,func)
        (define-key minibuffer-local-completion-map ,key ,func)
        (define-key minibuffer-local-must-match-map ,key ,func))))

  (with-no-warnings
    (generate-calls sp-define-bindings
      (
        ((kbd "C-M-f") #'sp-forward-sexp)
        ((kbd "C-M-b") #'sp-backward-sexp)

        ((kbd "C-M-d") #'sp-down-sexp)
        ((kbd "C-M-S-d") #'sp-backward-down-sexp)

        ((kbd "C-M-u") #'sp-up-sexp)
        ((kbd "C-M-S-u") #'sp-backward-up-sexp)

        ((kbd "C-M-n") #'sp-next-sexp)
        ((kbd "C-M-p") #'sp-previous-sexp)))))

(cl-macrolet
  ((sp-define-bindings (key func)
     `(progn
        (evil-define-key 'normal sp-keymap ,key ,func)
        (evil-define-key 'insert sp-keymap ,key ,func)
        (evil-define-key 'emacs sp-keymap ,key ,func)

        (define-key minibuffer-local-map ,key ,func)
        (define-key minibuffer-local-ns-map ,key ,func)
        (define-key minibuffer-local-completion-map ,key ,func)
        (define-key minibuffer-local-must-match-map ,key ,func))))

  (with-no-warnings
    (generate-calls sp-define-bindings
      (
        ((kbd "C-M-k") #'sp-kill-sexp)

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
(defun evil-sp-move ()
  (unless (fboundp 'evil-sp-move-hydra/body)
    (require 'hydra)
    (defhydra evil-sp-move-hydra ()
      "sexp"
      ("f" evil-forward-sexp "→")
      ("b" evil-backward-sexp "←")
      ("d" evil-down-sexp "↘")
      ("D" evil-backward-down-sexp "↙")
      ("u" evil-up-sexp "↗")
      ("U" evil-backward-up-sexp "↖")
      ("n" evil-next-sexp "⇒")
      ("p" evil-previous-sexp "⇐")))
  (evil-sp-move-hydra/body))

(defun evil-sp-barfslurp ()
  (unless (fboundp 'evil-sp-barfslurp-hydra/body)
    (require 'hydra)
    (defhydra evil-sp-barfslurp-hydra ()
      "sexp"
      ("," sp-forward-slurp-sexp "← slurp")
      ("." sp-forward-barf-sexp "→ barf")
      ("<" sp-backward-slurp-sexp "→ slurp")
      (">" sp-backward-barf-sexp "← barf")))
  (evil-sp-barfslurp-hydra/body))

(evil-define-command evil-sp-forward-sexp (&rest args)
  (call-interactively #'evil-forward-sexp args)
  (evil-sp-move))

(evil-define-command evil-sp-backward-sexp (&rest args)
  (call-interactively #'evil-backward-sexp args)
  (evil-sp-move))

(evil-define-command evil-sp-down-sexp (&rest args)
  (call-interactively #'evil-down-sexp args)
  (evil-sp-move))

(evil-define-command evil-sp-backward-down-sexp (&rest args)
  (call-interactively #'evil-backward-down-sexp args)
  (evil-sp-move))

(evil-define-command evil-sp-up-sexp (&rest args)
  (call-interactively #'evil-up-sexp args)
  (evil-sp-move))

(evil-define-command evil-sp-backward-up-sexp (&rest args)
  (call-interactively #'evil-backward-up-sexp args)
  (evil-sp-move))

(evil-define-command evil-sp-next-sexp (&rest args)
  (call-interactively #'evil-next-sexp args)
  (evil-sp-move))

(evil-define-command evil-sp-previous-sexp (&rest args)
  (call-interactively #'evil-previous-sexp args)
  (evil-sp-move))


(evil-define-command evil-sp-forward-slurp-sexp ()
  (call-interactively #'sp-forward-slurp-sexp)
  (evil-sp-barfslurp))

(evil-define-command evil-sp-forward-barf-sexp ()
  (call-interactively #'sp-forward-barf-sexp)
  (evil-sp-barfslurp))

(evil-define-command evil-sp-backward-slurp-sexp ()
  (call-interactively #'sp-backward-slurp-sexp)
  (evil-sp-barfslurp))

(evil-define-command evil-sp-backward-barf-sexp ()
  (call-interactively #'sp-backward-barf-sexp)
  (evil-sp-barfslurp))

;; evil normal mode bindings
(define-prefix-command 'sp-sexp-ops)
(cl-macrolet
  ((sp-define-bindings (key func)
     `(progn
        (define-key evil-normal-state-map ,key ,func))))
  (generate-calls sp-define-bindings
    (
      ("gs" #'sp-sexp-ops)
      ("gsf" #'evil-sp-forward-sexp)
      ("gsb" #'evil-sp-backward-sexp)
      ("gsd" #'evil-sp-down-sexp)
      ("gsD" #'evil-sp-backward-down-sexp)
      ("gsu" #'evil-sp-up-sexp)
      ("gsU" #'evil-sp-backward-up-sexp)
      ("gsn" #'evil-sp-next-sexp)
      ("gsp" #'evil-sp-previous-exp)
      ("gsk" #'sp-kill-sexp)
      ("gsK" #'sp-backward-kill-sexp)
      ("gsw" #'sp-unwrap-sexp)
      ("gsW" #'sp-backward-unwrap-sexp)
      ("gss" #'sp-split-sexp)
      ("gsj" #'sp-join-sexp)
      ("gsa" #'sp-absorb-sexp)
      ("gse" #'sp-emit-sexp)
      ("gs," #'evil-sp-forward-slurp-sexp)
      ("gs." #'evil-sp-forward-barf-sexp)
      ("gs<" #'evil-sp-backward-barf-sexp)
      ("gs>" #'evil-sp-backward-slurp-sexp))))

(defun my-sp-pair-function (id action context)
  (if (eq action 'insert)
    (or (looking-at "[[:space:][:punct:]]")
      (sp-point-before-eol-p id action context))
    t))

(with-eval-after-load 'smartparens
  (diminish 'smartparens-mode " σ")
  (set-face-background 'sp-pair-overlay-face "grey20")
  (set-face-foreground 'sp-pair-overlay-face "default")

  (sp-pair "(" ")" :when '(my-sp-pair-function) :wrap "C-)")
  (sp-pair "{" "}" :when '(my-sp-pair-function) :wrap "C-}")
  (sp-pair "[" "]" :when '(my-sp-pair-function) :wrap "C-]")
  (sp-pair "\"" "\"" :when '(my-sp-pair-function) :wrap "C-\"")
  (sp-pair "'" "'" :when '(my-sp-pair-function))

  (define-key evil-insert-state-map (kbd "C-]") nil)
  (define-key evil-normal-state-map (kbd "C-]") nil)
  (define-key evil-motion-state-map (kbd "C-]") nil)
  (define-key evil-emacs-state-map (kbd "C-]") nil)

  ;; disable "'" pairing in text mode, as it's often an apostrophe
  (add-hook 'text-mode-hook
    (lambda ()
      (sp-local-pair major-mode "'" nil :actions nil))))

(provide 'config-smartparens)
