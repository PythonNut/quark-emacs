(require 'smartparens-config)
(eval-when-compile (require 'cl))
(eval-when-compile (require 'evil))

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

;; register smartparens motions as CUA motions too
(put 'evil-forward-sexp       'CUA 'move)
(put 'evil-backward-sexp      'CUA 'move)
(put 'evil-up-sexp            'CUA 'move)
(put 'evil-backward-up-sexp   'CUA 'move)
(put 'evil-down-sexp          'CUA 'move)
(put 'evil-forward-sexp       'CUA 'move)
(put 'evil-backward-sexp      'CUA 'move)
(put 'evil-up-sexp            'CUA 'move)
(put 'evil-backward-up-sexp   'CUA 'move)
(put 'evil-down-sexp          'CUA 'move)
(put 'evil-backward-down-sexp 'CUA 'move)
(put 'evil-next-sexp          'CUA 'move)
(put 'evil-previous-sexp      'CUA 'move)

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

(define-key evil-outer-text-objects-map "n" 'evil-a-next-thing)
(define-key evil-inner-text-objects-map "n" 'evil-i-next-thing)

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
  (evil-previous-thing count beg end type t))
(evil-define-text-object evil-i-previous-thing (count &optional beg end type)
  "Select the range defined by sp-select-previous-thing."
  (evil-previous-thing count beg end type))

(define-key evil-outer-text-objects-map "N" 'evil-a-previous-thing)
(define-key evil-inner-text-objects-map "N" 'evil-i-previous-thing)

;; define top level motions bindings
(define-key evil-motion-state-map (kbd "M-h") 'evil-backward-sexp)
(define-key evil-motion-state-map (kbd "M-j") 'evil-enter-sexp)
(define-key evil-motion-state-map (kbd "M-k") 'evil-backward-up-sexp)
(define-key evil-motion-state-map (kbd "M-l") 'evil-forward-sexp)

(define-key sp-keymap (kbd "C-M-f") 'evil-forward-sexp)
(define-key sp-keymap (kbd "C-M-b") 'evil-backward-sexp)
(define-key sp-keymap (kbd "C-M-k") 'sp-kill-sexp)

(define-key sp-keymap (kbd "C-M-d") 'evil-down-sexp)
(define-key sp-keymap (kbd "C-M-a") 'evil-backward-down-sexp)

(define-key sp-keymap (kbd "C-M-e") 'evil-up-sexp)
(define-key sp-keymap (kbd "C-M-u") 'evil-backward-up-sexp)

(define-key sp-keymap (kbd "C-M-n") 'evil-next-sexp)
(define-key sp-keymap (kbd "C-M-p") 'evil-previous-sexp)

(define-key sp-keymap (kbd "C-M-t") 'sp-transpose-sexp)

(define-key sp-keymap (kbd "M-(") 'sp-select-previous-thing)
(define-key sp-keymap (kbd "M-)") 'sp-select-next-thing)

(define-key sp-keymap (kbd "C-+") 'sp-rewrap-sexp)
(define-key sp-keymap (kbd "M-<delete>") 'sp-kill-sexp)
(define-key sp-keymap (kbd "M-<backspace>") 'sp-backward-kill-sexp)
(define-key sp-keymap (kbd "S-<backspace>") 'sp-backward-unwrap-sexp)

(define-key sp-keymap (kbd "C-M-,") 'sp-forward-slurp-sexp)
(define-key sp-keymap (kbd "C-M-.") 'sp-forward-barf-sexp)

(define-key sp-keymap (kbd "M-<") 'sp-backward-slurp-sexp)
(define-key sp-keymap (kbd "M->") 'sp-backward-barf-sexp)

;; allow quick repetition since normal state key chains are awkward
(defun evil-smart-smartparens-move ()
  (unless (fboundp 'smartrep-read-event-loop)
    (require 'smartrep))
  (condition-case e
    (smartrep-read-event-loop
      '(("f" . 'evil-smart-forward-sexp)
         ("b" . 'evil-smart-backward-sexp)
         ("d" . 'evil-smart-down-sexp)
         ("D" . 'evil-smart-backward-down-sexp)
         ("e" . 'evil-smart-up-sexp)
         ("U" . 'evil-smart-backward-up-sexp)
         ("n" . 'evil-smart-next-sexp)
         ("p" . 'evil-smart-previous-sexp)
         ("<return>" . 'keyboard-quit)))
    (quit nil)))

(defun evil-smart-smartparens-barfslurp ()
  (unless (fboundp 'smartrep-read-event-loop)
    (require 'smartrep))
  (condition-case e
    (smartrep-read-event-loop
      '(("," . 'sp-smart-forward-slurp-sexp)
         ("." . 'sp-smart-forward-barf-sexp)
         ("<" . 'sp-smart-backward-slurp-sexp)
         (">" . 'sp-smart-backward-barf-sexp)
         ("<return>" . 'keyboard-quit)))
    (quit nil)))

(evil-define-command evil-smart-forward-sexp (&rest args)
  (call-interactively 'evil-forward-sexp args)
  (evil-smart-smartparens-move))

(evil-define-command evil-smart-backward-sexp (&rest args)
  (call-interactively 'evil-backward-sexp args)
  (evil-smart-smartparens-move))

(evil-define-command evil-smart-down-sexp (&rest args)
  (call-interactively 'evil-down-sexp args)
  (evil-smart-smartparens-move))

(evil-define-command evil-smart-backwards-down-sexp (&rest args)
  (call-interactively 'evil-backwards-down-sexp args)
  (evil-smart-smartparens-move))

(evil-define-command evil-smart-up-sexp (&rest args)
  (call-interactively 'evil-up-sexp args)
  (evil-smart-smartparens-move))

(evil-define-command evil-smart-backwards-up-sexp (&rest args)
  (call-interactively 'evil-backwards-up-sexp args)
  (evil-smart-smartparens-move))

(evil-define-command evil-smart-next-sexp (&rest args)
  (call-interactively 'evil-next-sexp args)
  (evil-smart-smartparens-move))

(evil-define-command evil-smart-previous-sexp (&rest args)
  (call-interactively 'evil-previous-sexp args)
  (evil-smart-smartparens-move))


(evil-define-command sp-smart-forward-slurp-sexp ()
  (call-interactively 'sp-forward-slurp-sexp)
  (evil-smart-smartparens-barfslurp))

(evil-define-command sp-smart-forward-barf-sexp ()
  (call-interactively 'sp-forward-barf-sexp)
  (evil-smart-smartparens-barfslurp))

(evil-define-command sp-smart-backward-slurp-sexp ()
  (call-interactively 'sp-backward-slurp-sexp)
  (evil-smart-smartparens-barfslurp))

(evil-define-command sp-smart-backward-barf-sexp ()
  (call-interactively 'sp-backward-barf-sexp)
  (evil-smart-smartparens-barfslurp))

;; evil normal mode bindings
(define-prefix-command 'sp-sexp-ops)
(let ((nmap evil-normal-state-map))
  (define-key nmap (kbd "g s") 'sp-sexp-ops)
  (define-key nmap (kbd "g s f") 'evil-smart-forward-sexp)
  (define-key nmap (kbd "g s b") 'evil-smart-backward-sexp)
  (define-key nmap (kbd "g s d") 'evil-smart-down-sexp)
  (define-key nmap (kbd "g s D") 'evil-smart-backward-down-sexp)
  (define-key nmap (kbd "g s e") 'evil-smart-up-sexp)
  (define-key nmap (kbd "g s U") 'evil-smart-backward-up-sexp)
  (define-key nmap (kbd "g s n") 'evil-smart-next-sexp)
  (define-key nmap (kbd "g s p") 'evil-smart-previous-sexp)
  (define-key nmap (kbd "g s k") 'sp-kill-sexp)
  (define-key nmap (kbd "g s K") 'sp-backward-kill-sexp)
  (define-key nmap (kbd "g s w") 'sp-unwrap-sexp)
  (define-key nmap (kbd "g s W") 'sp-backward-unwrap-sexp)
  (define-key nmap (kbd "g s ,") 'sp-smart-forward-slurp-sexp)
  (define-key nmap (kbd "g s .") 'sp-smart-forward-barf-sexp)
  (define-key nmap (kbd "g s <") 'sp-smart-backward-slurp-sexp)
  (define-key nmap (kbd "g s >") 'sp-smart-backward-barf-sexp))

(defun my-sp-pair-function (id action context)
  (if (eq action 'insert)
    (or (looking-at "[[:space:][:punct:]]")
      (sp-point-before-eol-p id action context))
    t))

(with-eval-after-load 'smartparens
  (progn
    (diminish 'smartparens-mode " σ")
    (set-face-background 'sp-pair-overlay-face "grey20")
    (set-face-foreground 'sp-pair-overlay-face "default")

    (sp-pair "(" ")" :when '(my-sp-pair-function) :wrap "C-)")
    (sp-pair "{" "}" :when '(my-sp-pair-function) :wrap "C-}")
    (sp-pair "[" "]" :when '(my-sp-pair-function) :wrap "C-]")
    (sp-pair "\"" "\"" :when '(my-sp-pair-function) :wrap "C-\"")
    (sp-pair "'" "'" :when '(my-sp-pair-function))

    (let ((my-c-modes
	    '('c++-mode
	       'java-mode
	       'c-mode
	       'css-mode
	       'scss-mode
	       'web-mode
	       'js2-mode
	       'js3-mode)))

      (while my-c-modes
	(sp-local-pair (car my-c-modes) "{" nil :post-handlers
	  '(:add
	     ("||\n[i]" "RET")
	     ("| " "SPC")))
	(setq my-c-modes (cdr my-c-modes))))

    (define-key evil-insert-state-map (kbd "C-]") 'nil)
    (define-key evil-normal-state-map (kbd "C-]") 'nil)
    (define-key evil-motion-state-map (kbd "C-]") 'nil)
    (define-key evil-emacs-state-map (kbd "C-]") 'nil)

    ;; disable "'" pairing in text mode, as it's often an apostrophe
    (add-hook 'text-mode-hook
      '(lambda ()
	 (sp-local-pair major-mode "'" nil :actions nil)))

    (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
    (sp-local-pair 'emacs-lisp-mode "`" nil :when '(sp-in-string-p))))

(add-hook 'LaTeX-mode-hook
  '(lambda ()
     (require 'smartparens-latex)))

(add-hook 'sgml-mode-hook
  '(lambda ()
     (require 'smartparens-html)))
