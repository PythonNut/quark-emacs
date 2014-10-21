;;; ===========
;;; Parentheses
;;; ===========
;; Smart parens
(set 'lisp-indent-offset 2)
(setq sp-autoinsert-quote-if-followed-by-closing-pair nil)
(setq sp-autoescape-string-quote nil)
(setq sp-autoescape-string-quote-if-empty nil)
(setq sp-cancel-autoskip-on-backward-movement nil)

(require 'smartparens-config)
(smartparens-global-mode +1)
(raise-minor-mode-map-alist 'smartparens-mode-map)

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

(defun evil-smart-forward-sexp (&rest args)
  (interactive)
  (call-interactively 'evil-forward-sexp args)
  (evil-smart-smartparens-move))

(defun evil-smart-backward-sexp (&rest args)
  (interactive)
  (call-interactively 'evil-backward-sexp args)
  (evil-smart-smartparens-move))

(defun evil-smart-down-sexp (&rest args)
  (interactive)
  (call-interactively 'evil-down-sexp args)
  (evil-smart-smartparens-move))

(defun evil-smart-backwards-down-sexp (&rest args)
  (interactive)
  (call-interactively 'evil-backwards-down-sexp args)
  (evil-smart-smartparens-move))

(defun evil-smart-up-sexp (&rest args)
  (interactive)
  (call-interactively 'evil-up-sexp args)
  (evil-smart-smartparens-move))

(defun evil-smart-backwards-up-sexp (&rest args)
  (interactive)
  (call-interactively 'evil-backwards-up-sexp args)
  (evil-smart-smartparens-move))

(defun evil-smart-next-sexp (&rest args)
  (interactive)
  (call-interactively 'evil-next-sexp args)
  (evil-smart-smartparens-move))

(defun evil-smart-previous-sexp (&rest args)
  (interactive)
  (call-interactively 'evil-previous-sexp args)
  (evil-smart-smartparens-move))

(defun sp-smart-forward-slurp-sexp ()
  (interactive)
  (call-interactively 'sp-forward-slurp-sexp)
  (evil-smart-smartparens-barfslurp))
(defun sp-smart-forward-barf-sexp ()
  (interactive)
  (call-interactively 'sp-forward-barf-sexp)
  (evil-smart-smartparens-barfslurp))
(defun sp-smart-backward-slurp-sexp ()
  (interactive)
  (call-interactively 'sp-backward-slurp-sexp)
  (evil-smart-smartparens-barfslurp))
(defun sp-smart-backward-barf-sexp ()
  (interactive)
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

(eval-after-load 'smartparens
  '(progn
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

;; show matching
(show-paren-mode +1)
(set 'show-paren-delay 0.05)
(set-face-background 'show-paren-match-face "default")
(set-face-foreground 'show-paren-match-face "default")

;; show matching line off screen
(defadvice show-paren-function
  (after show-matching-paren-offscreen activate)
  "If the matching paren is offscreen, show the matching line in the
        echo area. Has no effect if the character before point is not of
        the syntax class ')'."
  (let* ((cb (char-before (point)))
          (matching-text (and cb
                           (char-equal (char-syntax cb) ?\))
                           (blink-matching-open))))
    (when matching-text (message matching-text))))

;; color all parentheses at all times
(require 'rainbow-delimiters)

;; color parentheses by level
;; if cursor is on a paren, make the parens stick out more

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'text-mode-hook 'rainbow-delimiters-mode)

(set-face-foreground 'rainbow-delimiters-depth-1-face "#eee8d5")
(set-face-foreground 'rainbow-delimiters-depth-2-face "#6c71c4")
(set-face-foreground 'rainbow-delimiters-depth-3-face "#cb4b16")
(set-face-foreground 'rainbow-delimiters-depth-4-face "#859900")
(set-face-foreground 'rainbow-delimiters-depth-5-face "#93a1a1")
(set-face-foreground 'rainbow-delimiters-depth-6-face "#258bd2")
(set-face-foreground 'rainbow-delimiters-depth-7-face "#d33682")
(set-face-foreground 'rainbow-delimiters-depth-8-face "#6c71c4")
(set-face-foreground 'rainbow-delimiters-depth-9-face "#2aa198")


(defun rainbow-wash-out-colour (colour &optional degree)
  "Return a colour string specifying a washed-out version of COLOUR."
  (let ((basec (color-values
                 (face-attribute 'default :foreground)))
         (col (color-values colour))
         (list nil))
    (unless degree (setq degree 2))
    (while col
      (push (/ (/ (+ (or (pop col) 128)
                    (* degree (or (pop basec) 128)))
                 (1+ degree))
              256)
        list))
    (apply 'format "#%02x%02x%02x" (nreverse list))))

(defun rainbow-wash-out-face (face &optional degree)
  "Make the foreground colour of FACE appear a bit more pale."
  (let ((colour (face-attribute face :foreground)))
    (unless (eq colour 'unspecified)
      (set-face-attribute face nil
        :foreground (rainbow-wash-out-colour colour degree)))))

(defun rainbow-delimiters-wash (arg)
  (rainbow-wash-out-face 'rainbow-delimiters-depth-1-face arg)
  (rainbow-wash-out-face 'rainbow-delimiters-depth-2-face arg)
  (rainbow-wash-out-face 'rainbow-delimiters-depth-3-face arg)
  (rainbow-wash-out-face 'rainbow-delimiters-depth-4-face arg)
  (rainbow-wash-out-face 'rainbow-delimiters-depth-5-face arg)
  (rainbow-wash-out-face 'rainbow-delimiters-depth-6-face arg)
  (rainbow-wash-out-face 'rainbow-delimiters-depth-7-face arg)
  (rainbow-wash-out-face 'rainbow-delimiters-depth-8-face arg)
  (rainbow-wash-out-face 'rainbow-delimiters-depth-9-face arg))

(add-hook 'emacs-startup-hook
  '(lambda ()
     (rainbow-delimiters-wash 1.5)))

(defun rainbow-delimiters-saturate (face &optional degree)
  (require 'hexrgb)
  "Adjust the saturation of the given face by the given degree"
  (set-face-foreground face
    (hexrgb-increment-saturation
      (face-attribute face :foreground) 0.5)))

(defvar rainbow-delimiters-face-delta 0.1)
(defun rainbow-delimiters-focus (arg)
  (rainbow-delimiters-saturate 'rainbow-delimiters-depth-1-face arg)
  (rainbow-delimiters-saturate 'rainbow-delimiters-depth-2-face arg)
  (rainbow-delimiters-saturate 'rainbow-delimiters-depth-3-face arg)
  (rainbow-delimiters-saturate 'rainbow-delimiters-depth-4-face arg)
  (rainbow-delimiters-saturate 'rainbow-delimiters-depth-5-face arg)
  (rainbow-delimiters-saturate 'rainbow-delimiters-depth-6-face arg)
  (rainbow-delimiters-saturate 'rainbow-delimiters-depth-7-face arg)
  (rainbow-delimiters-saturate 'rainbow-delimiters-depth-8-face arg)
  (rainbow-delimiters-saturate 'rainbow-delimiters-depth-9-face arg))

(defvar rainbow-delimiters-switch nil)

(defun rainbow-delimiters-on-maybe ()
  (unless (or rainbow-delimiters-switch (minibufferp))
    (rainbow-delimiters-focus rainbow-delimiters-face-delta)
    (setq rainbow-delimiters-switch t)))

(defun rainbow-delimiters-off-maybe ()
  (when rainbow-delimiters-switch
    (rainbow-delimiters-focus (- rainbow-delimiters-face-delta))
    (setq rainbow-delimiters-switch nil)))

(defun rainbow-delimiters-focus-on-maybe ()
  "Display the show pair overlays."
  (let* ((pair-list (sp--get-allowed-pair-list))
          (opening (sp--get-opening-regexp pair-list))
          (closing (sp--get-closing-regexp pair-list))
          (allowed (and sp-show-pair-from-inside (sp--get-allowed-regexp))))
    (when (or
            (or (sp--looking-at (if sp-show-pair-from-inside allowed opening))
              (and (memq major-mode sp-navigate-consider-stringlike-sexp)
                (looking-at (sp--get-stringlike-regexp))))

            (or (sp--looking-back (if sp-show-pair-from-inside allowed closing))
              (and (memq major-mode sp-navigate-consider-stringlike-sexp)
                (sp--looking-back (sp--get-stringlike-regexp)))))
      (rainbow-delimiters-on-maybe))))

(run-with-idle-timer 0.6 t 'rainbow-delimiters-focus-on-maybe)

(defun rainbow-delimiters-focus-off-maybe ()
  "Display the show pair overlays."
  (let* ((pair-list (sp--get-allowed-pair-list))
          (opening (sp--get-opening-regexp pair-list))
          (closing (sp--get-closing-regexp pair-list))
          (allowed (and sp-show-pair-from-inside (sp--get-allowed-regexp))))
    (unless (or
              (or (sp--looking-at (if sp-show-pair-from-inside allowed opening))
                (and (memq major-mode sp-navigate-consider-stringlike-sexp)
                  (looking-at (sp--get-stringlike-regexp))))

              (or (sp--looking-back (if sp-show-pair-from-inside allowed closing))
                (and (memq major-mode sp-navigate-consider-stringlike-sexp)
                  (sp--looking-back (sp--get-stringlike-regexp)))))
      (rainbow-delimiters-off-maybe))))

(run-with-idle-timer 0.1 t 'rainbow-delimiters-focus-off-maybe)


