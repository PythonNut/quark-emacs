(eval-when-compile
  (with-demoted-errors
    (require 'cl-lib)
    (require 'key-chord)
    (require 'evil)
    (require 'diminish)
    (require 'evil-easymotion)
    (require 'config-modes)))

(require 'config-scroll)
(require 'config-line-numbers)
(require 'config-ace-easymotion)
(require 'config-undo)

(require 'config-smartparens)
(require 'config-rainbow-delimiters)

(show-paren-mode +1)

(cl-macrolet
  ((autoload-multiple-cursors (func)
     `(autoload ,func "multiple-cursors")))
  (generate-calls-single autoload-multiple-cursors
    (
      #'mc/mark-lines
      #'mc/mark-next-lines
      #'mc/mark-previous-lines
      #'mc/unmark-next-like-this
      #'mc/unmark-previous-like-this
      #'mc/skip-to-previous-like-this
      #'mc/mark-all-like-this
      #'mc/mark-all-words-like-this
      #'mc/mark-all-symbols-like-this
      #'mc/mark-all-in-region
      #'mc/mark-all-in-region-regexp
      #'mc/mark-more-like-this-extended
      #'mc/mmlte--up
      #'mc/mmlte--down
      #'mc/mmlte--left
      #'mc/mmlte--right
      #'mc/mark-all-like-this-dwim
      #'mc/mark-all-dwim
      #'mc/mark-all-like-this-in-defun
      #'mc/mark-all-words-like-this-in-defun
      #'mc/mark-all-symbols-like-this-in-defun
      #'mc/add-cursor-on-click
      #'mc/mark-sgml-tag-pair
      #'mc/mark-pop
      #'set-rectangular-region-anchor
      #'rrm/switch-to-multiple-cursors
      #'mc/insert-numbers
      #'mc/reverse-regions
      #'mc/sort-regions
      #'hum/keyboard-quit
      #'mc-hide-unmatched-lines-mode
      )))

(global-set-key (kbd "C-c l") #'mc/edit-lines)
(global-set-key (kbd "C-c a") #'mc/mark-all-like-this-dwim)
(global-set-key (kbd "C-c s") #'mc/mark-all-in-region)
(global-set-key (kbd "C-c M") #'mc/mark-pop)
(global-set-key (kbd "C->") #'mc/mark-next-like-this)
(global-set-key (kbd "C-<") #'mc/mark-previous-like-this)

;; directional window movement
(add-hook 'window-configuration-change-hook
  (lambda ()
    (windmove-default-keybindings 'meta)))

;; directional frame movement too
(add-hook 'before-make-frame-hook
  (lambda ()
    (windmove-default-keybindings 'meta)
    (unless (featurep 'framemove)
      (require 'framemove))))

(with-eval-after-load 'framemove
  (setq framemove-hook-into-windmove t))

(global-set-key (kbd "C-.") #'er/expand-region)

;;; ====================================
;;; iflib - switch buffers alt-tab style
;;; ====================================
(defvar iflipb-running-p nil)

(with-eval-after-load 'iflipb
  (setq
    iflipb-ignore-buffers '("^ " "^*helm" "^*Compile" "^*Quail")
    iflipb-wrap-around 't)

  (defun iflipb-first-iflipb-buffer-switch-command ()
    "Determines whether this is the first invocation of
  iflipb-next-buffer or iflipb-previous-buffer this round."
    (or (not (or (eq last-command 'iflipb-next-buffer)
               (eq last-command 'iflipb-previous-buffer)))
      iflipb-running-p)))

(defun iflipb-next-buffer-smart ()
  "A `smartrep' enabled next-buffer"
  (interactive)
  (unless (featurep 'smartrep) (require 'smartrep))
  (call-interactively #'iflipb-next-buffer)
  (lexical-let ((iflipb-running-p t))
    (condition-case e
      (smartrep-read-event-loop
        '(("C-S-<iso-lefttab>" . #'iflipb-previous-buffer-smart)
           ("S-<iso-lefttab>"  . #'iflipb-previous-buffer-smart)
           ("<C-tab>"          . #'iflipb-next-buffer-smart)
           ("<tab>"            . #'iflipb-next-buffer-smart)
           ("<return>"         . #'keyboard-quit)))
      (quit nil))))

(defun iflipb-previous-buffer-smart ()
  "A `smartrep' enabled previous-buffer"
  (interactive)
  (unless (featurep 'smartrep) (require 'smartrep))
  (call-interactively #'iflipb-previous-buffer)
  (lexical-let ((iflipb-running-p t))
    (condition-case e
      (smartrep-read-event-loop
        '(("C-S-<iso-lefttab>" . #'iflipb-previous-buffer-smart)
           ("S-<iso-lefttab>"  . #'iflipb-previous-buffer-smart)
           ("<C-tab>"          . #'iflipb-next-buffer-smart)
           ("<tab>"            . #'iflipb-next-buffer-smart)
           ("<return>"         . #'keyboard-quit)))
      (quit nil))))

(global-set-key (kbd "<C-tab>") 'iflipb-next-buffer-smart)
(global-set-key (kbd "C-S-<iso-lefttab>") 'iflipb-previous-buffer-smart)

(provide 'config-ui)
