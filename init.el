(setq
  split-width-threshold 0
  split-height-threshold 0
  inhibit-startup-screen t
  inhibit-startup-echo-area-message t
  inhibit-default-init 1
  ;; sentence-end-double-space nil
  delete-by-moving-to-trash t
  x-stretch-cursor t
  gc-cons-threshold 20000000
  right-margin-width 0
  left-margin-width 0
  cursor-type 'box
  indent-tabs-mode nil
  tab-width 4
  kkc-init-file-name "~/.emacs.d/kkrc"
  jit-lock-defer-time 0.04
  jit-lock-stealth-time 0.5
  jit-lock-stealth-nice 0.1
  jit-lock-stealth-verbose nil)

(tool-bar-mode -1)

(setq-default
  indent-tabs-mode nil
  tab-width 4
  comment-start "# "
  cursor-type 'box)

(setq frame-title-format '(buffer-file-name "%f" (dired-directory dired-directory "%b")))

(require 'package)
(require 'uniquify)
(setq
  uniquify-buffer-name-style 'post-forward
  uniquify-separator ":")
(eval-when-compile (require 'cl))

(set-fringe-mode '(1 . 0))
(if (display-graphic-p)
  (menu-bar-mode +1)
  (menu-bar-mode -1))

(defun toggle-fullscreen ()
  "Toggle full screen on X11"
  (interactive)
  (when (eq window-system 'x)
    (if (file-exists-p "/usr/bin/wmctrl")
      (shell-command "wmctrl -r :ACTIVE: -btoggle,fullscreen")
      (set-frame-parameter
        nil 'fullscreen
        (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))))

(global-set-key (kbd "<f11>") 'toggle-fullscreen)

(defun toggle-ui ()
  (interactive)
  (if (and (boundp 'menu-bar-mode) menu-bar-mode)
    (menu-bar-mode -1)
    (menu-bar-mode +1))
  (if (and (boundp 'scroll-bar-mode) scroll-bar-mode)
    (scroll-bar-mode -1)
    (scroll-bar-mode +1)))

(global-set-key (kbd "<f9>") 'toggle-ui)

(defun toggle-mode-line ()
  "toggles the modeline on and off"
  (interactive)
  (setq mode-line-format
    (if (equal mode-line-format nil)
      (default-value 'mode-line-format)) )
  (redraw-display))

(defun enable-debugging ()
  (interactive)
  (setq debug-on-error t))

(global-set-key (kbd "<M-f9>") 'toggle-mode-line)
(global-set-key (kbd "<right-fringe> <mouse-1>") 'scroll-bar-mode)

(auto-compression-mode 1)
(blink-cursor-mode 0)
(delete-selection-mode 1)

(setq epa-file-name-regexp "\\.\\(gpg\\|asc\\)$")
(epa-file-name-regexp-update)
(setenv "GPG_AGENT_INFO" nil)

(global-set-key (kbd "<mouse-6>") (kbd "<left>"))
(global-set-key (kbd "<mouse-7>") (kbd "<right>"))

(global-set-key (kbd "<f6>") 'multi-term)

;;; ===========
;;; Theme setup
;;; ===========

(defalias 'yes-or-no-p 'y-or-n-p)
(add-hook 'emacs-startup-hook '(lambda ()
  ;; bold keywords please
  (set-face-attribute 'font-lock-keyword-face 'nil :weight 'extra-bold)
  (set-face-attribute 'font-lock-comment-face 'nil :slant 'italic)
  ;; (set-face-foreground 'font-lock-comment-face "#28490d")
  (egoge-wash-out-fontlock-faces 0.5)))

(defun raise-minor-mode-map-alist (mode-symbol)
  "Raise `minor-mode-map-alist' priority of MODE-SYMBOL."
  (let ((x (assq mode-symbol minor-mode-map-alist)))
    (and x (setq minor-mode-map-alist (cons x (delq x minor-mode-map-alist))))))

(defun lower-minor-mode-map-alist (mode-symbol)
  "Lower `minor-mode-map-alist' priority of MODE-SYMBOL."
  (let ((rel (assq mode-symbol minor-mode-map-alist)))
    (setq minor-mode-map-alist (append (delete rel minor-mode-map-alist) (list rel)))))

(defun egoge-wash-out-colour (colour &optional degree)
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

(defun egoge-wash-out-face (face &optional degree)
  "Make the foreground colour of FACE appear a bit more pale."
  (let ((colour (face-attribute face :foreground)))
    (unless (eq colour 'unspecified)
      (set-face-attribute face nil
        :foreground (egoge-wash-out-colour colour degree)))))

(defun egoge-find-faces (regexp)
  "Return a list of all faces whose names match REGEXP."
  (delq nil
    (mapcar (lambda (face)
              (and (string-match regexp
                     (symbol-name face))
                face))
      (face-list))))

(defun egoge-wash-out-fontlock-faces (&optional degree)
  (mapc (lambda (elt)
          (egoge-wash-out-face elt degree))
    (delq 'font-lock-warning-face
      (egoge-find-faces "^font-lock"))))

;;; ===========
;;; basic setup
;;; ===========

(defun display-startup-echo-area-message ()
  "If it wasn't for this you'd be GNU/Spammed by now"
  (message ""))

(add-to-list 'load-path "~/.emacs.d/personal/")

(let* ((dir (expand-file-name "~/.emacs.d/elpa"))
       (default-directory dir))
  (when (file-directory-p dir)
    (add-to-list 'load-path dir)
    (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
        (normal-top-level-add-subdirs-to-load-path))))

;; env tweaks
(setq default-major-mode 'text-mode)
(setq-default indent-tabs-mode nil)

(setq mouse-drag-copy-region nil)
(setq x-select-enable-primary t)
(setq x-select-enable-clipboard t)
(setq select-active-regions t)
(setf interprogram-cut-function 'x-select-text)
(setf interprogram-paste-function 'x-cut-buffer-or-selection-value)

(global-set-key (kbd "<mouse-8>")
  '(lambda ()
     (interactive)
     (scroll-down-line)
     (scroll-down-line)
     (scroll-down-line)))

(global-set-key (kbd "<mouse-9>")
  '(lambda ()
     (interactive)
     (scroll-up-line)
     (scroll-up-line)
     (scroll-up-line)))

(unless (display-graphic-p)
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))))

(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse 't)

(global-set-key (kbd "M-DEL") 'evil-delete-backward-word)

(defadvice kill-whole-line (around delete-empty-lines activate)
  (if (string-match "^[[:space:]]*$"
        (buffer-substring-no-properties
          (line-beginning-position)
          (line-end-position)))
    (delete-blank-lines)
    ad-do-it))

;;; ==================
;;; save last position
;;; ==================
(setq save-place-file "~/.emacs.d/saveplace")
(setq-default save-place t)
(require 'saveplace)

;;; =========================
;;; auto save - protect me!!!
;;; =========================
(defvar backup-location "~/.emacs.d/data/backups")
(defvar autosave-location "~/.emacs.d/data/autosave")
(defvar backup-directory "~/.emacs.d/data/backups")
(defvar tramp-backup-directory "~/.emacs.d/data/tramp-backups")

(setq backup-directory-alist
  `((".*" . ,backup-location)
     (".*\.emacs\.d.*" . "/dev/null")))

(setq auto-save-file-name-transforms
  `((".*" ,autosave-location t)))

(global-set-key (kbd "C-c B") 'backup-walker-start)

(require 'backups-mode)
(backups-mode-start)
(auto-save-mode +1)

;; automatically save buffers associated with files on buffer switch
;; and on windows switch
(defadvice switch-to-buffer (before save-buffer-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice other-window (before other-window-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice windmove-up (before other-window-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice windmove-down (before other-window-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice windmove-left (before other-window-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice windmove-right (before other-window-now activate)
  (when buffer-file-name (save-buffer)))

;; save backups too
(setq version-control t ;; Use version numbers for backups
  kept-new-versions 30  ;; Number of newest versions to keep
  kept-old-versions 0   ;; Number of oldest versions to keep
  delete-old-versions t ;; Don't Ask to delete excess backup versions
  backup-by-copying t   ;; Copy linked files, don't rename.
  backup-by-copying-when-linked t ;; copy links too
  auto-save-default t    ;; also auto-save
  auto-save-timeout 10   ;; auto-save after 10s of idle time
  auto-save-interval 200 ;; auto-save after 200 chars
  vc-make-backup-files t ;; because we don't commit every save
  )

(defun force-backup-of-buffer ()
  (setq buffer-backed-up nil))

(add-hook 'before-save-hook  'force-backup-of-buffer)

;; also save when frame focus lost
;; TODO: 24.4 focus hooks

;;; ========================
;;; Elscreen - tabs in emacs
;;; ========================
(require 'elscreen)
(setq elscreen-prefix-key "\C-l")
(setq elscreen-display-tab             nil)
(setq elscreen-tab-display-kill-screen nil)
(setq elscreen-display-screen-number   nil)
(setq elscreen-tab-display-control     nil)

;; automatically start elscreen if needed
(setq started-elscreen nil)
(defun load-elscreen ()
  (interactive)
  (elscreen-start)
  (setq started-elscreen t))

(defun elscreen-frame-title-update ()
  (when (elscreen-screen-modified-p 'elscreen-frame-title-update)
    (let* ((screen-list (sort (elscreen-get-screen-list) '<))
            (screen-to-name-alist (elscreen-get-screen-to-name-alist))
            (title (mapconcat
                     (lambda (screen)
                       (format "%s"
                         (if (string= (elscreen-status-label screen) "+")
                           (concat "[" (get-alist screen screen-to-name-alist) "]")
                           (get-alist screen screen-to-name-alist))))
                     screen-list " - ")))
      (if (fboundp 'set-frame-name)
        (set-frame-name title)
        (setq frame-title-format title)))))

(eval-after-load "elscreen"
  '(progn
     (add-hook 'elscreen-screen-update-hook 'elscreen-frame-title-update)
     (setq elscreen-prefix-key "\C-l")
     
     (set-face-background 'elscreen-tab-background-face "grey65")
     (set-face-attribute 'elscreen-tab-background-face nil :underline nil)
     (set-face-foreground 'elscreen-tab-current-screen-face "white")
     (set-face-background 'elscreen-tab-current-screen-face "grey50")
     (set-face-attribute 'elscreen-tab-current-screen-face nil :underline 'nil)
     (set-face-foreground 'elscreen-tab-other-screen-face "black")
     (set-face-background 'elscreen-tab-other-screen-face "grey65")
     (set-face-attribute 'elscreen-tab-other-screen-face nil :underline 'nil)
     
     ;; automatically create new if switching to blank screen
     (defmacro elscreen-create-automatically-open (ad-do-it)
       `(if (not (= (length (elscreen-get-screen-list)) (+ (elscreen-get-current-screen) 1)))
            , ad-do-it
          (elscreen-create)
       (elscreen-notify-screen-modification 'force-immediately)
       (elscreen-message "New screen is automatically created")))
     
     (defmacro elscreen-create-automatically (ad-do-it)
       `(if (not (elscreen-one-screen-p))
            , ad-do-it
          (elscreen-create)
          (elscreen-notify-screen-modification 'force-immediately)
          (elscreen-message "New screen is automatically created")))
     
     (defadvice elscreen-next (around elscreen-create-automatically activate)
       (elscreen-create-automatically-open ad-do-it))
     
     (defadvice elscreen-previous (around elscreen-create-automatically activate)
    (elscreen-create-automatically ad-do-it))
     
     (defadvice elscreen-toggle (around elscreen-create-automatically activate)
       (elscreen-create-automatically ad-do-it))))

(defun al-elscreen-next ()
  (interactive)
  (unless started-elscreen
    (load-elscreen))
  (elscreen-next))

(defun al-elscreen-previous ()
  (interactive)
  (unless started-elscreen
    (load-elscreen))
  (elscreen-previous))

;; (defun al-elscreen-create ()
;;   (interactive)
;;   (unless started-elscreen
;;     (load-elscreen))
;;   (elscreen-create))

;; (defun al-elscreen-clone ()
;;   (interactive)
;;   (unless started-elscreen
;;     (load-elscreen))
;;   (elscreen-clone))

;; (define-prefix-command 'elscreen-map)
;; (global-set-key (kbd "C-l") 'elscreen-map)
;; (global-set-key (kbd "C-l c") 'al-elscreen-create)
;; (global-set-key (kbd "C-l C") 'al-elscreen-clone)

;; Alt+(PgUp|PgDown) switches between elscreens
(global-set-key (kbd "M-<prior>") 'al-elscreen-previous)
(global-set-key (kbd "M-<next>") 'al-elscreen-next)

;;; =====================================
;;; Windmove - effortless window movement
;;; =====================================
(windmove-default-keybindings)

(defadvice make-frame-command (before load-framemove activate)
  (progn
    (require 'framemove)
    (setq framemove-hook-into-windmove t)))

(defadvice windmove-left (around try-elscreen activate)
  (if (and
        (= (length (window-list)) 1)
        (= (length (frame-list)) 1))
    (if (and (elscreen-get-screen-list)
      (> (length (elscreen-get-screen-list)) 1))
      (al-elscreen-previous)
        ad-do-it)
    ad-do-it))

(defadvice windmove-right (around try-elscreen activate)
  (if (and
        (= (length (window-list)) 1)
        (= (length (frame-list)) 1))
    (if (and (elscreen-get-screen-list)
          (> (length (elscreen-get-screen-list)) 1))
      (al-elscreen-next)
      (if (y-or-n-p-with-timeout "Start elscreen? " 2 nil)
        (al-elscreen-next)
        ad-do-it))
    ad-do-it))

;;; =========================================
;;; Key Chords - commands for unmodified keys
;;; =========================================
(require 'key-chord)
(key-chord-mode 1)

;;; ==========================
;;; Kill ring - cut and paste+
;;; ==========================
(autoload 'kill-ring-search "kill-ring-search")
(global-set-key (kbd "M-C-y") 'kill-ring-search)

;;; ====================
;;; Clean greek modeline
;;; ====================

(defvar mode-line-cleaner-alist
  '((auto-complete-mode . " α")
     (yas-minor-mode . " ¥")
     (undo-tree-mode . " μ")
     (flyspell-mode . " ⎂")
     (helm-mode . " ⛭")
     (emmet-mode . "")
     (hs-minor-mode . " ±")
     (auto-highlight-symbol-mode . "")
     (projectile-global-mode . " →")
     (projectile-mode . " →")
     (icy-mode . " I")
     (icicle-mode . " I")
     (adaptive-wrap-prefix-mode . " ⬎")
     (highlight-changes-mode . " Δ")
     (visual-line-mode . "")
     (smartparens-mode . " ⒮")
     (auto-indent-mode . " ⤷")
     (ws-butler-mode . "")

     ;; Major modes
     (js2-mode . "js")
     (lisp-interaction-mode . "λI")
     (python-mode . "Py")
     (html-mode . "<>")
     (c++-mode . "C+")
     (css-mode . ":;")
     (livescript-mode . "->")
     (emacs-lisp-mode . "λ")))

(defun clean-mode-line ()
  (interactive)
  (loop for cleaner in mode-line-cleaner-alist
    do (let* ((mode (first cleaner))
               (mode-str (rest cleaner))
               (old-mode-str (rest (assq mode minor-mode-alist))))
         (when old-mode-str
           (setcar old-mode-str mode-str))
         ;; major mode
         (when (eq mode major-mode)
           (setq mode-name mode-str)))))

(add-hook 'after-change-major-mode-hook 'clean-mode-line)

;;; ======================
;;; Text mode improvements
;;; ======================
(add-hook 'text-mode-hook 'auto-fill-mode)

;;; =====================================
;;; Make scripts executable automatically
;;; =====================================
(defvar my-buffer-should-be-executable nil)
(make-variable-buffer-local 'my-file-should-be-executable)

(defun my-check-buffer-executable ()
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (setq my-file-should-be-executable
        (or (and (looking-at "^#!")
              (not (file-exists-p buffer-file-name)))
          (file-executable-p buffer-file-name))))))

(defun my-make-script-executable ()
  "If file starts with a shebang, make `buffer-file-name' executable"
  (when my-file-should-be-executable
    (set-file-modes buffer-file-name
      (logior (file-modes buffer-file-name) #o100))
    (message (concat "Made " buffer-file-name " executable"))))

(add-hook 'before-save-hook 'my-check-buffer-executable)
(add-hook 'after-save-hook 'my-make-script-executable)

;;; ========
;;; CUA mode
;;; ========
(xterm-mouse-mode +1)
(add-hook 'kill-emacs-hook
  '(lambda (&rest args)
     (xterm-mouse-mode -1)))
(cua-selection-mode +1)

(add-hook 'minibuffer-setup-hook
  '(lambda ()
     (interactive)
     (cua-selection-mode -1)))

(add-hook 'minibuffer-exit-hook
  '(lambda ()
     (interactive)
     (cua-selection-mode +1)))

;;; ===================================
;;; Evil mode - Emacs + Vim keybindings
;;; ===================================
(require 'evil)
(require 'evil-leader)
(require 'evil-indent-textobject-autoloads)

(setq evil-want-C-w-delete 'nil
  evil-want-C-w-in-emacs-state 'nil
  evil-ex-complete-emacs-commands 't
  evil-want-fine-undo 't
  evil-search-module 'evil-search
  evil-magic 'very-magic)

(setq-default
  evil-symbol-word-search 't)

(evil-mode +1)
(global-evil-leader-mode +1)
(setq evil-leader/leader "," evil-leader/in-all-states t)

(evil-set-initial-state 'diff-mode 'motion)
(evil-set-initial-state 'backups-mode 'insert)
(evil-set-initial-state 'erc-mode 'emacs)
(evil-set-initial-state 'git-commit-mode 'insert)
(evil-set-initial-state 'backup-walker-mode 'motion)

(add-hook 'package-menu-mode-hook
  '(lambda ()
     (interactive)
     (call-interactively 'evil-motion-state)
     (run-at-time 0.2 nil
       '(lambda ()
          (interactive)
          (call-interactively 'evil-motion-state)
          (run-hooks 'post-command-hook)))))

(define-key evil-normal-state-map (kbd "<down>") 'evil-next-visual-line)
(define-key evil-motion-state-map (kbd "<up>") 'evil-previous-visual-line)
(define-key evil-motion-state-map (kbd "<down>") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "<up>") 'evil-previous-visual-line)

(key-chord-define evil-insert-state-map  "jj" 'evil-normal-state)
(key-chord-define evil-replace-state-map "jj" 'evil-normal-state)
(key-chord-define evil-emacs-state-map   "jj" 'evil-normal-state)

(key-chord-define evil-insert-state-map  "kk" 'evil-normal-state)
(key-chord-define evil-replace-state-map "kk" 'evil-normal-state)
(key-chord-define evil-emacs-state-map   "kk" 'evil-normal-state)

(key-chord-define evil-insert-state-map ";'" 'evil-ex)
(key-chord-define evil-emacs-state-map ";'" 'evil-ex)

(global-set-key (kbd "C-<backspace>") 'evil-delete-backward-word)

(key-chord-define evil-normal-state-map " l" 'evil-ace-jump-line-mode)
(key-chord-define evil-normal-state-map " n" 'ace-jump-char-N-lines)
(key-chord-define evil-normal-state-map " b" 'ace-jump-buffer)
(key-chord-define evil-normal-state-map " c" 'evil-ace-jump-char-mode)
(key-chord-define evil-normal-state-map " t" 'evil-ace-jump-char-to-mode)


(defun isearch-exit-chord-worker (&optional arg)
  (interactive "p")
  (execute-kbd-macro (kbd "<backspace> <return>")))

(defun isearch-exit-chord (arg)
  (interactive "p")
  (isearch-printing-char)
  (unless (fboundp 'smartrep-read-event-loop)
    (require 'smartrep))
  (run-at-time 0.3 nil 'keyboard-quit)
  (condition-case e
    (smartrep-read-event-loop
      '(("j" . isearch-exit-chord-worker)
         ("k" . isearch-exit-chord-worker)))
    (quit nil)))

(define-key isearch-mode-map "j" 'isearch-exit-chord)
(define-key isearch-mode-map "k" 'isearch-exit-chord)

;; This function builds a repeatable version of its argument COMMAND.
(defun repeat-command (command)
  "Repeat COMMAND."
  (interactive)
  (let ((repeat-previous-repeated-command command)
         (last-repeatable-command 'repeat))
    (repeat nil)))


(define-key evil-normal-state-map (kbd "C-SPC")
  '(lambda () (interactive)
     (evil-insert-state)
     (execute-kbd-macro (kbd "C-SPC"))))

(define-key evil-normal-state-map (kbd "C-RET")
  '(lambda ()
     (interactive)
     (evil-insert-state)
     (cua-set-rectangle-mark)))

(define-key evil-insert-state-map (kbd "C-e") 'end-of-visual-line)
(setq evil-replace-state-cursor '("#884444" box))

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

(define-key evil-normal-state-map (kbd "[ <SPC>") 'evil-open-above-normal)
(define-key evil-normal-state-map (kbd "] <SPC>") 'evil-open-below-normal)

(define-key evil-normal-state-map (kbd "[ e") 'drag-stuff-up)
(define-key evil-normal-state-map (kbd "] e") 'drag-stuff-down)

(define-key evil-normal-state-map (kbd "[ w") 'drag-stuff-left)
(define-key evil-normal-state-map (kbd "] w") 'drag-stuff-right)

(define-key evil-insert-state-map (kbd "C-j") 'evil-normal-state)
(define-key evil-emacs-state-map (kbd "C-j") 'evil-normal-state)
(define-key evil-visual-state-map (kbd "C-j") 'evil-normal-state)
(define-key evil-replace-state-map (kbd "C-j") 'evil-normal-state)

(defun evil-open-paragraph-full (arg)
  (interactive "p")
  (evil-open-above-normal arg)
  (evil-open-below arg)
  (keyboard-quit))

(defun evil-open-paragraph-empty (arg)
  (interactive "p")
  (evil-open-below arg)
  (evil-previous-line arg)
  (indent-according-to-mode)
  (keyboard-quit))

(defun evil-open-paragraph (arg)
  (interactive "p")
  (unless (fboundp 'smartrep-read-event-loop)
    (require 'smartrep))
  (run-at-time 0.3 nil 'keyboard-quit)
  (let ((blank-line (string-match "^[[:space:]]*$"
                      (buffer-substring-no-properties
                        (line-beginning-position)
                        (line-end-position)))))
    (evil-open-below arg)
    (run-hooks 'post-command-hook)
    (if blank-line
      (condition-case e
        (smartrep-read-event-loop
          '(("o" . evil-open-paragraph-empty)))
      (quit nil))
      (condition-case e
        (smartrep-read-event-loop
          '(("o" . evil-open-paragraph-full)))
        (quit nil)))))

(define-key evil-normal-state-map "o" 'evil-open-paragraph)

(defun evil-yank-to-end-of-line ()
  (interactive)
  (evil-yank (point) (point-at-eol)))

(define-key evil-normal-state-map "Y" 'evil-yank-to-end-of-line)

(define-key evil-normal-state-map "U" 'undo-tree-visualize)
(define-key evil-normal-state-map (kbd "C-z")
  '(lambda ()
     (interactive)
     (message "use u.")))

(defun my-evil-smart-undo (&rest args)
  (interactive)
  (undo-tree-undo)
  (unless (fboundp 'smartrep-read-event-loop)
    (require 'smartrep))
  (condition-case e
    (smartrep-read-event-loop
      '(("r" . undo-tree-redo)
         ("u" . undo-tree-undo)))
    (quit nil)))

(defun my-evil-smart-redo (&rest args)
  (interactive)
  (undo-tree-redo)
  (unless (fboundp 'smartrep-read-event-loop)
    (require 'smartrep))
  (condition-case e
    (smartrep-read-event-loop
      '(("r" . undo-tree-redo)
         ("u" . undo-tree-undo)))
    (quit nil)))

(define-key evil-normal-state-map (kbd "u")   'my-evil-smart-undo)
(define-key evil-normal-state-map (kbd "C-r") 'my-evil-smart-redo)

;; ignore deleted blanks in evil
(defun purge-kill-ring ()
  (interactive)
  (let ((last-kill (substring-no-properties (first kill-ring)
                     0 (length (first kill-ring)))))
    (when (= (or (string-match "^[[:space:]]*\n$" last-kill) (point-max)) 0)
      (setq kill-ring (rest kill-ring)))))

(defadvice evil-delete (after ignore-blanks activate)
  (purge-kill-ring)
  (when (eq (first (get-char-property 0 'yank-handler (first kill-ring)))
          'evil-yank-line-handler)
    (put-text-property 0 1 'whole-line-or-region t (first kill-ring))))

(autoload 'evil-exchange        "evil-exchange")
(autoload 'evil-exchange-cancel "evil-exchange")

(defun auto-evil-exchange ()
  (interactive)
  (unless (boundp 'evil-exchange)
    (require 'evil-exchange))
  (call-interactively 'evil-exchange))

(defun auto-evil-exchange-cancel ()
  (interactive)
  (unless (boundp 'evil-exchange)
    (require 'evil-exchange))
  (call-interactively 'evil-exchange-cancel))

(define-key evil-normal-state-map "gx" 'auto-evil-exchange)
(define-key evil-visual-state-map "gx" 'auto-evil-exchange)
(define-key evil-normal-state-map "gX" 'auto-evil-exchange-cancel)
(define-key evil-visual-state-map "gX" 'auto-evil-exchange-cancel)

;;; === Evil motion section ===
(defun beginning-and-end-of-sexp ()
  (destructuring-bind (b . e)
    (save-excursion
      (forward-char)
      (bounds-of-thing-at-point 'sexp))
    (cons b e)))

(evil-define-motion evil-forward-sexp (count)
  :type inclusive
  (dotimes (i (or count 1))
    (let ((lookahead-1 (char-syntax (char-after (point))))
           (lookahead-2 (char-syntax (char-after (1+ (point)))))
           (new-point (point)))
      (condition-case nil
        (progn (save-excursion
                 (message "lookahead1 = %S, lookahead-2 = %S"
                   (string lookahead-1) (string lookahead-2))
                 (cond ((or (memq lookahead-2 '(?\ ?>))
                          (member lookahead-1 '(?\ ?>)))
                         (forward-char)
                         (skip-syntax-forward "->")
                         (setq new-point (point)))
                   (t (unless (memq lookahead-1 '(?\" ?\())
                        (forward-char))
                     (sp-forward-sexp)
                     (backward-char)
                     (setq new-point (point)))))
          (goto-char new-point))
        (error (error "End of sexp"))))))

(evil-define-motion evil-backward-sexp (count)
  :type inclusive
  (dotimes (i (or count 1))
    (let ((lookahead (char-syntax (char-after (point))))
           (new-point (point)))
      (condition-case nil
        (progn (save-excursion
                 (when (memq lookahead '(?\) ?\"))
                   (forward-char))
                 (sp-backward-sexp)
                 (setq new-point (point)))
          (goto-char new-point))
        (error (error "Beginning of sexp"))))))

(evil-define-motion evil-enter-sexp (count)
  :type inclusive
  (dotimes (i (or count 1))
    (let ((lookahead-1 (char-syntax (char-after (point))))
           (lookahead-2 (char-syntax (char-after (1+ (point)))))
           (lookbehind-1 (char-syntax (char-before (point))))
           (lookbehind-2 (char-syntax (char-before (1- (point))))))
      (cond ((and (= lookahead-1 ?\()
               (/= lookbehind-1 ?\\)
               (= (char-after (1+ (point))) ?\n))
              (forward-char)
              (skip-syntax-forward "-"))
        ((and (= lookahead-1 ?\()
           (/= lookbehind-1 ?\\)
           (/= lookahead-2 ?\)))
          ;; do not move the cursor if it's on the opening paren of ()
          (forward-char)
          (skip-syntax-forward "-"))
        ((and (= lookahead-1 ?\))
           (or (/= lookbehind-1 ?\( )
             (= lookbehind-2 ?\\)))
          ;; do not move the cursor if it's on the closing paren of ()
          (skip-syntax-backward "-")
          (backward-char))
        (t (error "Already at the deepest level"))))))

(evil-define-motion evil-exit-sexp (count)
  :type inclusive
  (dotimes (i (or count 1))
    (let (op-pos cl-pos)
      (condition-case nil
        (progn (save-excursion
                 (sp-backward-up-sexp)
                 (setq op-pos (point))
                 (sp-forward-sexp)
                 (setq cl-pos (point)))
          (let ((lookahead (char-syntax (char-after (point)))))
            (case lookahead
              (?\( (goto-char op-pos))
              (?\) (goto-char cl-pos))
              (otherwise (goto-char (if (> (abs (- (point) cl-pos))
                                          (abs (- (point) op-pos)))
                                      op-pos
                                      cl-pos))))))
        (error (error "Already at top-level."))))) )

(define-key evil-motion-state-map (kbd "M-h") 'evil-backward-sexp)
(define-key evil-motion-state-map (kbd "M-j") 'evil-enter-sexp)
(define-key evil-motion-state-map (kbd "M-k") 'evil-exit-sexp)
(define-key evil-motion-state-map (kbd "M-l") 'evil-forward-sexp)

(evil-define-motion evil-forward-symbol (count)
  (call-interactively 'sp-forward-symbol count))

(evil-define-motion evil-backward-symbol (count)
  (call-interactively 'sp-backward-symbol count))

(define-key evil-motion-state-map "L" 'evil-forward-symbol)
(define-key evil-motion-state-map "H" 'evil-backward-symbol)

;;; === Evil text object section ===
(evil-define-text-object evil-a-sexp (count &optional beg end type)
  (evil-an-object-range count beg end #'sp-forward-sexp #'sp-backward-sexp))

(evil-define-text-object evil-inner-sexp (count &optional beg end type)
  (evil-inner-object-range count beg end #'sp-forward-sexp #'sp-backward-sexp))

(define-key evil-outer-text-objects-map "e" 'evil-a-sexp)
(define-key evil-inner-text-objects-map "e" 'evil-inner-sexp)

;; do delimited selection
(defun evil-between-range (count beg end type &optional inclusive)
  (ignore-errors
    (let ((count (abs (or count 1)))
           (beg (and beg end (min beg end)))
           (end (and beg end (max beg end)))
          (ch (evil-read-key))
           beg-inc end-inc)
      (save-excursion
        (when beg (goto-char beg))
        (evil-find-char (- count) ch)
        (setq beg-inc (point)))
      (save-excursion
        (when end (goto-char end))
        (backward-char)
        (evil-find-char count ch)
        (setq end-inc (1+ (point))))
      (if inclusive
          (evil-range beg-inc end-inc)
        (if (and beg end (= (1+ beg-inc) beg) (= (1- end-inc) end))
            (evil-range beg-inc end-inc)
          (evil-range (1+ beg-inc) (1- end-inc)))))))

(evil-define-text-object evil-a-between (count &optional beg end type)
  "Select range between a character by which the command is followed."
  (evil-between-range count beg end type t))
(evil-define-text-object evil-i-between (count &optional beg end type)
  "Select inner range between a character by which the command is followed."
  (evil-between-range count beg end type))

(define-key evil-outer-text-objects-map "f" 'evil-a-between)
(define-key evil-inner-text-objects-map "f" 'evil-i-between)

;; evil block indentation textobject for Python
(defun evil-indent--current-indentation ()
  "Return the indentation of the current line. Moves point."
  (buffer-substring-no-properties (point-at-bol)
    (progn (back-to-indentation)
      (point))))

(defun evil-indent--block-range (&optional point)
  "Return the point at the begin and end of the text block with the same indentation."
  ;; there are faster ways to mark the entire file
  ;; so assume the user wants a block and skip to there
  (loop while (and (/= (point) (point-max))
                (= 0
                  (length (evil-indent--current-indentation))))
    do (progn
         (forward-line 1)))

  (cl-flet ((before-end () (/= (point) (point-max)))
          (empty-line-p ()
           (string-match "^[[:space:]]*$"
             (buffer-substring-no-properties
               (line-beginning-position)
               (line-end-position)))))
    (let ((indent (evil-indent--current-indentation)))
      (cl-flet ((line-indent-ok ()
               (or (>=
                     (length (evil-indent--current-indentation))
                     (length indent))
                 (empty-line-p))))
        ;; now skip ahead to the Nth block with this indentation
        (setq index (or last-prefix-arg 0))
        (while (> index 1)
          (while (and (before-end) (line-indent-ok))
                 (forward-line 1))
          (while (and (before-end) (not (line-indent-ok)))
                 (forward-line 1))
          (setq index (1- index)))
        (save-excursion
          (when point (goto-char point))
          (let ((start (point)) begin end)
            (while (and (/= (point) (point-min)) (line-indent-ok))
              (setq begin (point))
              (forward-line -1))
            (goto-char start)
            (while (and (before-end) (line-indent-ok))
              (setq end (point))
              (forward-line 1))
            (goto-char end)
            (while (empty-line-p)
              (forward-line -1)
              (setq end (point)))
            (list begin end)))))))

(evil-define-text-object evil-indent-i-block (&optional count beg end type)
  "Text object describing the block with the same indentation as the current line."
  (let ((range (evil-indent--block-range)))
    (evil-range (first range) (second range) 'line)))

(evil-define-text-object evil-indent-a-block (&optional count beg end type)
  "Text object describing the block with the same indentation as the current line and the line above."
  :type line
  (let ((range (evil-indent--block-range)))
    (evil-range (save-excursion
                  (goto-char (first (evil-indent--block-range)))
                  (forward-line -1)
                  (point-at-bol))
                (second range) 'line)))

(evil-define-text-object evil-indent-a-block-end (&optional count beg end type)
  "Text object describing the block with the same indentation as the current line and the lines above and below."
  :type line
  (let ((range (evil-indent--block-range)))
    (evil-range (save-excursion
                  (goto-char (first range))
                  (forward-line -1)
                  (point-at-bol))
                (save-excursion
                  (goto-char (second range))
                  (forward-line 1)
                  (point-at-eol)) 'line)))

(define-key evil-inner-text-objects-map "c" 'evil-indent-i-block)
(define-key evil-outer-text-objects-map "c" 'evil-indent-a-block)
(define-key evil-outer-text-objects-map "C" 'evil-indent-a-block-end)

;; evil NERD commenter, commenting awesomeness!
(autoload 'evilnc-comment-or-uncomment-lines "evil-nerd-commenter")
(autoload 'evilnc-comment-or-uncomment-to-the-line "evil-nerd-commenter")
(autoload 'evilnc-copy-and-comment-lines "evil-nerd-commenter")

(global-set-key (kbd "M-;") 'evilnc-comment-or-uncomment-lines)
(global-set-key (kbd "M-:") 'evilnc-comment-or-uncomment-to-the-line)
(global-set-key (kbd "C-M-;") 'evilnc-copy-and-comment-lines)

(defun evilnc-comment-or-uncomment-object ()
  (interactive)
  (call-interactively 'evil-visual-state)
  (call-interactively 'evil-indent-i-comment)
  (call-interactively 'evilnc-comment-or-uncomment-lines))

(defun evilnc-auto-comment ()
  (interactive)
  (save-excursion
    (call-interactively 'evilnc-comment-or-uncomment-lines))
  (save-excursion
    (call-interactively 'evilnc-comment-or-uncomment-lines)))

(evil-leader/set-key
  "ci" 'evilnc-comment-or-uncomment-lines
  "cl" 'evilnc-comment-or-uncomment-to-the-line
  "cc" 'evilnc-copy-and-comment-lines
  "cp" 'evilnc-comment-or-uncomment-paragraphs
  "ca" 'evilnc-auto-comment
  "co" 'evilnc-comment-or-uncomment-object)

(defun point-in-comment-p (&optional pt)
  "Determine if the point is inside a comment"
  (interactive)
  (let ((syn (syntax-ppss (or pt (point)))))
      (and (nth 8 syn)
        (not (nth 3 syn)))))

(defun evil-indent--comment-range (&optional point)
  "Return the point at the begin and end of the text block with the same indentation.
   If `point' is supplied and non-nil it will return the begin and end of the block surrounding point."

  (when point
    (goto-char point))

  (cl-flet ((before-end () (/= (point) (point-max)))
          (after-beg () (/= (point) (point-min)))
          (comment-anywhere ()
            (or
              (point-in-comment-p (point-at-bol))
              (point-in-comment-p (point-at-eol)))))

    (while (and (before-end) (not (comment-anywhere)))
      (forward-line 1))
    (end-of-line)
    (setq index (or last-prefix-arg 0))

    (while (> index 1)
      (while (and (before-end) (comment-anywhere))
          (forward-line 1))
      (while (and (before-end) (not (comment-anywhere)))
        (forward-line 1))
      (setq index (1- index)))

    (while (and (before-end) (point-in-comment-p))
      (backward-char))

    (save-excursion
      (if (string-match "^[[:space:]]*$"
            (buffer-substring-no-properties
              (line-beginning-position)
              (save-excursion
                (evil-backward-WORD-end)
                (point))))
        (let ((start (point)) begin end)
          (while (and (/= (point) (point-min))
                   (or (save-excursion
                     (progn
                       (back-to-indentation)
                       (evil-forward-WORD-begin)
                       (point-in-comment-p)))
                     (point-in-comment-p (line-beginning-position))))
            (setq begin (point-at-bol))
            (forward-line -1))

          (goto-char start)

          (while (and (before-end)
                   (or (save-excursion
                     (progn
                       (back-to-indentation)
                       (evil-forward-WORD-begin)
                       (point-in-comment-p)))
                     (point-in-comment-p (line-beginning-position))))
            (setq end (point-at-eol))
            (forward-line 1))
          (list 'line begin end))
        ;; back up the cursor here
        (list 'exclusive
          (progn
            (while (looking-back "[[:space:]]")
              (backward-char))
            (point))
          (line-end-position))))))

(evil-define-text-object evil-indent-i-comment (&optional count beg end type)
  "Text object describing the block with the same indentation as
the current line."
  (let ((range (evil-indent--comment-range)))
    (evil-contract-range (evil-range (second range) (third range) (first range)))))

(define-key evil-inner-text-objects-map "C" 'evil-indent-i-comment)

(defun evil-make-arbitrary-char-range (&rest _ignored)
  (save-excursion
    (let ((beg (progn (deactivate-mark) (evil-ace-jump-char-mode) (point)))
           (end (progn (deactivate-mark) (evil-ace-jump-char-mode) (point))))
      (if (> beg end)
        (list beg end)
        (list end beg)))))

(evil-define-text-object evil-i-arbitrary-char-range (&optional _c _b _e _t)
  "Text object describing the block with the same indentation as the current line."
  :type inclusive
  (let ((range (evil-make-arbitrary-char-range)))
    (evil-range (first range) (second range) 'inclusive)))

(evil-define-text-object evil-a-arbitrary-char-range (&optional _c _b _e _t)
  :type exclusive
  "Text object describing the block with the same indentation as the current line."
  (let ((range (evil-make-arbitrary-char-range)))
    (evil-range (1- (first range)) (1+ (second range)) 'inclusive)))

(define-key evil-inner-text-objects-map "r" 'evil-i-arbitrary-char-range)
(define-key evil-outer-text-objects-map "r" 'evil-a-arbitrary-char-range)

(defun evil-ace-jump-line-and-revert ()
  (interactive)
  (let ((top (save-excursion
               (evil-window-top)
               (line-number-at-pos))))
    (prog1
      (progn
        (deactivate-mark)
        (evil-ace-jump-line-mode)
        (point))
      (scroll-up-line (- top (save-excursion
                               (evil-window-top)
                               (line-number-at-pos)))))))

(defun evil-make-arbitrary-line-range (&rest _ignored)
  (save-excursion
    (let ((beg (evil-ace-jump-line-and-revert))
           (end (evil-ace-jump-line-and-revert)))
      (evil-range (min beg end) (max beg end)))))

(evil-define-text-object evil-i-arbitrary-line-range (&optional _c _b _e _t)
  "Text object describing the block with the same indentation as the current line."
  :type line
  (save-excursion
    (let ((beg (evil-ace-jump-line-and-revert))
           (end (evil-ace-jump-line-and-revert)))
      (if (> beg end)
        (evil-range beg end 'line)
        (evil-range end beg 'line)))))

(evil-define-text-object evil-a-arbitrary-line-range (&optional _c _b _e _t)
  :type line
  "Text object describing the block with the same indentation as the current line."
  (save-excursion
    (let ((beg (progn
                 (deactivate-mark)
                 (evil-ace-jump-line-mode)
                 (point)))
           (end (progn
                  (deactivate-mark)
                  (evil-ace-jump-line-mode)
                  (point))))
      (evil-range
        (progn
          (goto-char (min beg end))
          (loop do (previous-line) while
            (string-match "^[[:space:]]*$"
              (buffer-substring-no-properties
                (line-beginning-position)
                (line-end-position))))
          (next-line)
          (point))
        (progn
          (goto-char (max beg end))
          (loop do (next-line) while
            (string-match "^[[:space:]]*$"
              (buffer-substring-no-properties
                (line-beginning-position)
                (line-end-position))))
          (previous-line)
          (point)) 'line))))

(define-key evil-inner-text-objects-map "R" 'evil-i-arbitrary-line-range)
(define-key evil-outer-text-objects-map "R" 'evil-a-arbitrary-line-range)

(evil-define-text-object my-evil-next-match (count &optional beg end type)
  "Select next match."
  (save-excursion
    (evil-search-previous 1)
    (evil-search-next count)
    (list evil-ex-search-match-beg evil-ex-search-match-end)))

(evil-define-text-object my-evil-previous-match (count &optional beg end type)
  "Select previous match."
  (evil-search-next 1)
  (evil-search-previous count)
  (list evil-search-match-beg evil-search-match-end))

(define-key evil-outer-text-objects-map "m" 'my-evil-previous-match)
(define-key evil-inner-text-objects-map "m" 'my-evil-next-match)

;; (require 'evil-args)
(autoload 'evil-inner-arg "evil-args")
(autoload 'evil-outer-arg "evil-args")
(autoload 'evil-forward-arg "evil-args")
(autoload 'evil-backward-arg "evil-args")

(define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
(define-key evil-outer-text-objects-map "a" 'evil-outer-arg)

;; (define-key evil-normal-state-map "L" 'evil-forward-arg)
;; (define-key evil-normal-state-map "H" 'evil-backward-arg)
;; (define-key evil-motion-state-map "L" 'evil-forward-arg)
;; (define-key evil-motion-state-map "H" 'evil-backward-arg)

(define-key evil-normal-state-map "K" 'evil-jump-out-args)

(define-key evil-inner-text-objects-map "s" 'evil-inner-symbol)
(define-key evil-outer-text-objects-map "s" 'evil-a-symbol)

(define-key evil-inner-text-objects-map "." 'evil-inner-sentence)
(define-key evil-outer-text-objects-map "." 'evil-a-sentence)

;;; === evil operators ===
(evil-define-operator evil-comment-region (beg end type)
  (evil-normal-state)
  (goto-char beg)
  (evil-visual-state)
  (goto-char end)
  (call-interactively 'evilnc-comment-or-uncomment-lines))

(define-key evil-operator-state-map "gc" 'evil-comment-region)
(define-key evil-normal-state-map "gc" 'evil-comment-region)

(evil-define-operator evil-comment-and-copy-region (beg end type)
  (evil-normal-state)
  (goto-char beg)
  (evil-visual-state)
  (goto-char end)
  (call-interactively evilnc-copy-and-comment-lines))

(define-key evil-operator-state-map "gC" 'evil-comment-and-copy-region)
(define-key evil-normal-state-map "gC" 'evil-comment-and-copy-region)

(evil-define-operator evil-macro-on-all-lines (beg end &optional arg)
  (evil-normal-state)
  (goto-char end)
  (evil-visual-state)
  (goto-char beg)
  (evil-ex-normal (region-beginning) (region-end)
    (concat "@"
      (single-key-description
        (read-char "What macro?")))))

(define-key evil-operator-state-map "gl" 'evil-macro-on-all-lines)
(define-key evil-normal-state-map "gl" 'evil-macro-on-all-lines)

(require 'wide-n)

(defun narrow-to-region-indirect (start end)
  "Restrict editing in this buffer to the current region, indirectly."
  (interactive "r")
  (deactivate-mark)
  (let ((buf (clone-indirect-buffer nil nil)))
    (with-current-buffer buf
      (narrow-to-region start end))
    (switch-to-buffer buf)))

(global-set-key (kbd "C-x n i") 'narrow-to-region-indirect)

(evil-define-operator evil-narrow-indirect (beg end type)
  "Indirectly narrow the region from BEG to END."
  (interactive "<R>")
  (evil-normal-state)
  (narrow-to-region-indirect beg end))

(evil-define-operator evil-narrow-region (beg end type)
  "Indirectly narrow the region from BEG to END."
  (interactive "<R>")
  (evil-normal-state)
  (narrow-to-region beg end))

(define-key evil-operator-state-map "gn" 'evil-narrow-region)
(define-key evil-normal-state-map "gn" 'evil-narrow-region)
(define-key evil-operator-state-map "gN" 'evil-narrow-indirect)
(define-key evil-normal-state-map "gN" 'evil-narrow-indirect)

(evil-define-operator evil-eval-region (beg end type)
  (save-excursion
    (evil-normal-state)
    (goto-char beg)
    (evil-visual-state)
    (goto-char end)
    (eval-region beg end)
    (evil-normal-state)))
  
(define-key evil-operator-state-map "gV" 'evil-eval-region)
(define-key evil-normal-state-map "gV" 'evil-eval-region)


(evil-define-operator evil-align-regexp (beg end type)
  (save-excursion
    (evil-normal-state)
    (goto-char beg)
    (evil-visual-state)
    (goto-char end)
    (call-interactively 'align-regexp)
    (evil-normal-state)))

(define-key evil-operator-state-map "g|" 'evil-align-regexp)
(define-key evil-normal-state-map "g|" 'evil-align-regexp)

;;; === other customizations ===
;; smarter f and t
(evil-define-motion evil-fastnav-forward-to (count)
  (fastnav-jump-to-char-forward (or count 1)))

(evil-define-motion evil-fastnav-backward-to (count)
  (fastnav-jump-to-char-backward (or count 1)))

(evil-define-motion evil-fastnav-forward-til (count)
  (fastnav-jump-to-char-forward (or count 1))
  (backward-char))

(evil-define-motion evil-fastnav-backward-til (count)
  (fastnav-jump-to-char-backward (or count 1))
  (forward-char))

(define-key evil-normal-state-map "t" 'evil-fastnav-forward-til) 
(define-key evil-normal-state-map "T" 'evil-fastnav-backward-til)
(define-key evil-normal-state-map "f" 'evil-fastnav-forward-to)
(define-key evil-normal-state-map "F" 'evil-fastnav-backward-to)

(define-key evil-motion-state-map "t" 'evil-fastnav-forward-til) 
(define-key evil-motion-state-map "T" 'evil-fastnav-backward-til)
(define-key evil-motion-state-map "f" 'evil-fastnav-forward-to)
(define-key evil-motion-state-map "F" 'evil-fastnav-backward-to)

(define-key evil-visual-state-map "t" 'evil-fastnav-forward-til) 
(define-key evil-visual-state-map "T" 'evil-fastnav-backward-til)
(define-key evil-visual-state-map "f" 'evil-fastnav-forward-to)
(define-key evil-visual-state-map "F" 'evil-fastnav-backward-to)

;;; Change modeline color by Evil state
(lexical-let ((default-color (cons (face-background 'mode-line)
                               (face-foreground 'mode-line))))
  (add-hook 'post-command-hook
    (lambda ()
      (let ((color (cond ((minibufferp) default-color)
                     ((evil-normal-state-p) '("#586e75" . "#eee8d5"))
                     ((evil-emacs-state-p)  '("#859900" . "#eee8d5"))
                     ((evil-insert-state-p)  '("#93a1a1" . "#073642"))
                     ((evil-visual-state-p) '("#268bd2" . "#eee8d5"))
                     ((evil-replace-state-p) '("#dc322f" . "#eee8d5"))
                     (t '("grey70" . "black")))))
        (set-face-background 'mode-line (first color))
        (set-face-foreground 'mode-line (rest color))
        (set-face-foreground 'mode-line-buffer-id (rest color))))))

;; Evil surround, easily change surrounding chars
(require 'surround)
(global-surround-mode +1)

;; Esc quits from everything
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key evil-motion-state-map [escape] 'evil-normal-state)
(define-key evil-operator-state-map [escape] 'evil-normal-state)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

(define-key evil-insert-state-map (kbd "C-M-z") 'evil-emacs-state)
(define-key evil-emacs-state-map (kbd "C-M-z") 'evil-insert-state)
(define-key evil-normal-state-map (kbd "C-M-z") 'evil-insert-state)
(define-key evil-motion-state-map (kbd "C-M-z") 'evil-insert-state)
(define-key evil-visual-state-map (kbd "C-M-z") 'evil-insert-state)

(defun my-normal-smart-undo (&rest args)
  (interactive)
  (undo-tree-undo)
  (unless (fboundp 'smartrep-read-event-loop)
    (require 'smartrep))
  (condition-case e
    (smartrep-read-event-loop
      '(("y" . undo-tree-redo)
         ("z" . undo-tree-undo)))
    (quit nil)))

(defun my-normal-smart-redo (&rest args)
  (interactive)
  (undo-tree-redo)
  (unless (fboundp 'smartrep-read-event-loop)
    (require 'smartrep))
  (condition-case e
    (smartrep-read-event-loop
      '(("y" . undo-tree-redo)
         ("z" . undo-tree-undo)))
    (quit nil)))

(define-key evil-insert-state-map (kbd "C-z") 'my-normal-smart-undo)
(define-key evil-emacs-state-map (kbd "C-z") 'my-normal-smart-undo)

(global-set-key (kbd "<remap> <undo-tree-redo>") 'my-normal-smart-redo)
(global-set-key (kbd "C-S-z") 'my-normal-smart-redo)

(defadvice evil-paste-before (around auto-indent activate)
  (evil-indent (point) (+ (point) (length ad-do-it))))

(defadvice evil-paste-after (around auto-indent activate)
  (evil-indent (point) (+ (point) (length ad-do-it))))

(defun evil-rename-symbol (arg)
  "mark the region in defun (definition of function) and use string replacing UI in evil-mode
to replace the symbol under cursor"
  (interactive "P")
  (let ((old (thing-at-point 'symbol)))
    (mark-defun)
    (unless (evil-visual-state-p)
      (evil-visual-state))
    (evil-ex (concat "'<,'>s/" (if (= 0 (length old)) "" "\\<\\(") old (if (= 0 (length old)) "" "\\)\\>/")))))

(global-set-key (kbd "C-c C-c r") 'evil-rename-symbol)


;;; ==========
;;; Phi search
;;; ==========
(autoload 'phi-search "phi-search")
(autoload 'phi-search-backward "phi-search")
(autoload 'phi-replace-query "phi-replace")

(setq
  phi-search-case-sensitive  'guess
  phi-replace-case-sensitive 'guess)

(global-set-key (kbd "C-s") 'phi-search)
(global-set-key (kbd "C-r") 'phi-search-backward)
(global-set-key (kbd "M-%") 'phi-replace-query)

(defadvice phi-search (after kill-gratio activate)
  (progn
    (evil-insert-state)
    (define-key evil-insert-state-map (kbd "RET") 'phi-search-complete)
    (shrink-window (window-total-height))))

(defadvice phi-replace--initialize (after kill-gratio activate)
  (progn
    (evil-emacs-state)
    (define-key evil-insert-state-map (kbd "RET") 'phi-search-complete)
    (shrink-window (window-total-height))))

(defadvice phi-search-complete (after restart-gratio activate)
  (progn
    (define-key evil-insert-state-map (kbd "RET") 'smart-newline)))

(defadvice phi-replace-complete (after restart-gratio activate)
  (progn
    (define-key evil-insert-state-map (kbd "RET") 'smart-newline)))

(eval-after-load 'phi-search
  '(progn
     (define-key phi-search-default-map (kbd "RET") 'phi-search-complete)))

;;; ===============================
;;; Abbrev mode - text substitution
;;; ===============================
(setq abbrev-file-name "~/.emacs.d/abbrev_defs")
(abbrev-mode +1)
(setq save-abbrevs t)

;;; ================
;;; Smooth scrolling
;;; ================
(setq smooth-scroll-margin 5)

(setq scroll-margin 1
  scroll-conservatively 0
  scroll-up-aggressively 0.01
  scroll-down-aggressively 0.01)
(setq-default scroll-up-aggressively 0.01
  scroll-down-aggressively 0.01)

;; Package archives
(setq package-archives
  '(("gnu" . "http://elpa.gnu.org/packages/")
     ;; ("elpa" . "http://tromey.com/elpa/")
     ;; ("marmalade" . "http://marmalade-repo.org/packages/")
     ("melpa" . "http://melpa.milkbox.net/packages/")))

;;; ===========================================
;;; Undo tree mode - the ultimate undo function
;;; ===========================================
(add-hook 'first-change-hook
  (lambda () (require 'undo-tree)))

(defalias 'redo 'undo-tree-redo)
(defalias 'undo 'undo-tree-undo)

(add-hook 'emacs-startup-hook
  '(lambda ()
     (run-at-time 0.1 nil
       '(lambda ()
          (message "")))))

(add-hook 'find-file-hook
  '(lambda ()
     (run-at-time 1 nil
       '(lambda ()
          (message "")))))

(key-chord-define evil-emacs-state-map "uu" 'undo-tree-visualize)

(eval-after-load 'undo-tree
  '(progn
     (global-set-key (kbd "M-_") 'undo-tree-redo)
     (setq undo-tree-auto-save-history t)

     (define-key undo-tree-visualizer-mode-map "C-g" 'undo-tree-visualizer-quit)
     (define-key undo-tree-visualizer-mode-map (kbd "<escape>") 'undo-tree-visualizer-quit)
     (define-key undo-tree-visualizer-mode-map (kbd "<return>") 'undo-tree-visualizer-quit)
     (define-key undo-tree-visualizer-mode-map (kbd "<up>") 'undo-tree-visualize-undo)
     (define-key undo-tree-visualizer-mode-map (kbd "<down>") 'undo-tree-visualize-redo)
     (evil-define-key 'motion undo-tree-visualizer-mode-map (kbd "t")
       'undo-tree-visualizer-toggle-timestamps)
     (evil-define-key 'motion  undo-tree-visualizer-mode-map (kbd "d")
       'undo-tree-visualizer-toggle-diff)
     
     (add-hook 'undo-tree-visualizer-mode-hook
       '(lambda ()
          (evil-motion-state)))
     
     ;; compress undo with xz
     (when (locate-file "xz" exec-path)
       (defadvice undo-tree-make-history-save-file-name
         (after undo-tree activate)
         (setq ad-return-value (concat (make-auto-save-file-name) ".undo.xz"))))

     ;; Keep region when undoing in region
     (defadvice undo-tree-undo (around keep-region activate)
       (if (use-region-p)
         (let ((m (set-marker (make-marker) (mark)))
                (p (set-marker (make-marker) (point))))
           ad-do-it
           (goto-char p)
           (set-mark m)
           (set-marker p nil)
           (set-marker m nil))
         ad-do-it))


     (message "")))

;;; ================
;;; better registers
;;; ================
;; (require 'better-registers)
(autoload 'jump-to-register "better-registers")
;; (better-registers-install-save-registers-hook)
;; (setq better-registers-save-file "~/.emacs.d/saveregisters.el")
;; (load better-registers-save-file)
;; (better-registers -1)
(define-key evil-insert-state-map (kbd "C-r") 'jump-to-register)

;;; ================================
;;; ACE jump mode and ACE jump stuff
;;; ================================
;;(load "~/.emacs.d/elpa/ace-jump-mode-20121104.1157/ace-jump-mode.el")
(add-to-list 'load-path "~/.emacs.d/elpa/ace-jump-mode-20121104.1157/")
;; use autoloads
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)

;; use letters, numbers and capitals in that order
(setq ace-jump-mode-move-keys
  (nconc
    (loop for i from ?a to ?z collect i)
    (loop for i from ?0 to ?9 collect i)
    (loop for i from ?A to ?Z collect i))
  ace-jump-mode-case-fold nil)

;; enable and use the more powerful ACE jump back feature
;; see github.com/winterTTr/ace-jump-mode/wiki/AceJump-FAQ
(autoload
  'ace-jump-mode-pop-mark
  "ace-jump-mode"
  "Ace jump back:-)"
  t)

(setq ace-jump-mode-scope 'global)

;; (defadvice pop-global-mark (around ace-pop-mark activate)
;;   (ace-jump-mode-pop-mark))


(defun realign-cursor ()
  (save-excursion
    (if (> (rest (nth 6 (posn-at-point)))
          (/ (window-body-height) 2))
      (progn (previous-line) (next-line))
      (progn (next-line) (previous-line)))))

(defadvice evil-ace-jump-word-mode (after cleanup activate)
  (progn
    (call-interactively
      '(lambda ()
         (interactive)
         (realign-cursor)))))

(defadvice evil-ace-jump-char-mode (after cleanup activate)
  (progn
    (call-interactively
      '(lambda ()
         (interactive)
         (realign-cursor)))))

(defadvice evil-ace-jump-line-mode (after realign activate)
  (progn
    (call-interactively
      '(lambda ()
         (interactive)
         (realign-cursor)))))

(defadvice evil-ace-jump-line-mode (around restore-pos activate)
  (let ((cursor (first (nth 6 (posn-at-point))))
         (line (- (line-end-position)
                 (line-beginning-position))))
    ad-do-it
    (call-interactively
      '(lambda ()
         ;; (back-to-indentation)
         (interactive)
         (forward-char
           (round (* (/ cursor (max (float line) 1))
                    (or (- (line-end-position)
                          (line-beginning-position)) 1))))))))

(eval-after-load "ace-jump-mode" '(progn
  (ace-jump-mode-enable-mark-sync)
  (if (< (display-color-cells) 256)
    (progn
      (set-face-foreground 'ace-jump-face-background "white")
      (set-face-background 'ace-jump-face-background "black")
      (set-face-foreground 'ace-jump-face-foreground "black")
      (set-face-background 'ace-jump-face-foreground "white")))))

(key-chord-define evil-insert-state-map "jl" 'evil-ace-jump-line-mode)
(key-chord-define evil-insert-state-map "jk" 'evil-ace-jump-word-mode)
(key-chord-define evil-insert-state-map "uu" 'undo-tree-visualize)
(key-chord-define evil-emacs-state-map "jk" 'ace-jump-word-mode)
(key-chord-define evil-emacs-state-map "jc" 'ace-jump-to-char-within-N-lines)
(key-chord-define evil-emacs-state-map "jl" 'ace-jump-line-mode)
(key-chord-define evil-emacs-state-map "JJ" 'ace-jump-buffer)
;; (define-key evil-normal-state-map (kbd "SPC") 'evil-ace-jump-word-mode)

(defun ace-jump-char-N-lines (&optional n)
  (interactive "p")
  (let* ((N (or n (window-body-height)))
          (query-char (read-char "Query Char:"))
               (start (save-excursion
                        (forward-line (- N))
                        (point)))
               (stop (save-excursion
                       (forward-line (1+ N))
                       (point))))
        (unwind-protect
              (condition-case err
                (progn
                  (narrow-to-region start stop)
                          (evil-ace-jump-char-mode query-char))
                      (error
                        (message (error-message-string err))))
          (widen))))

(key-chord-define evil-insert-state-map "jc" 'ace-jump-to-char-within-N-lines)

(require 'noflet)
(require 'ace-jump-mode)

(defmacro ace-generic (collector &rest follower)
  "ace jump to candidates of collector using follower."
  (declare (indent 1))
  `(noflet ((ace-jump-search-candidate (str va-list)
              (mapcar (lambda (x)
                        (make-aj-position
                          :offset (1- x)
                          :visual-area (car va-list)))
                ,collector)))
     (setq ace-jump-mode-end-hook
       (list (lambda ()
               (setq ace-jump-mode-end-hook)
               ,@follower)))
     (ace-jump-do "")))

(defmacro ace-motion-collect (func)
  `(let ((points ())
          (count 0)
          (smooth-scroll-window-margin 0)
          (win-start (window-start))
          (win-end (window-end)))
     (save-excursion
       (call-interactively (quote ,func))
       (while (when (and
                      (> (1+ (point)) win-start)
                      (< (1+ (point)) win-end)
                      (< count (length ace-jump-mode-move-keys)))
                (push (1+ (point)) points)
                (setq count (1+ count))
                (call-interactively (quote ,func))
                t))
       (set-window-start (selected-window) win-start)
       (nreverse points))))

(defmacro ace-motion (func)
  `(evil-define-motion ,(make-symbol (concat "ace-" (symbol-name func))) (count)
     :type inclusive
     (evil-without-repeat
       (let ((pnt (point))
              (buf (current-buffer)))
         (evil-enclose-ace-jump-for-motion
           (ace-generic
             (ace-motion-collect ,func)
             ()))
         (when (and (equal buf (current-buffer))
                 (< (point) pnt))
           (setq evil-this-type 'exclusive))))))

(defmacro ease-motion (key motion)
  `(define-key evil-motion-state-map (kbd ,key) (ace-motion ,motion)))
(define-key evil-motion-state-map (kbd "SPC") 'nil)

(ease-motion "SPC w" evil-forward-word-begin)
(ease-motion "SPC W" evil-forward-WORD-begin)
(ease-motion "SPC e" evil-forward-word-end)
(ease-motion "SPC E" evil-forward-WORD-end)
(ease-motion "SPC b" evil-backward-word-begin)
(ease-motion "SPC B" evil-backward-WORD-begin)
(ease-motion "SPC ge" evil-backward-word-end)
(ease-motion "SPC gE" evil-backward-WORD-end)

(ease-motion "SPC h" evil-backward-char)
(ease-motion "SPC j" evil-next-line)
(ease-motion "SPC k" evil-previous-line)
(ease-motion "SPC l" evil-forward-char)

(ease-motion "SPC H" evil-backward-symbol)
(ease-motion "SPC L" evil-forward-symbol)

(ease-motion "SPC g j" evil-next-visual-line)
(ease-motion "SPC g k" evil-previous-visual-line)

(ease-motion "SPC s f" evil-forward-sexp)
(ease-motion "SPC s b" evil-backward-sexp)

(ease-motion "SPC [[" evil-backward-section-begin)
(ease-motion "SPC []" evil-backward-section-end)
(ease-motion "SPC ]]" evil-forward-section-begin)
(ease-motion "SPC ][" evil-forward-section-end)

(ease-motion "SPC L" evil-forward-symbol)
(ease-motion "SPC H" evil-backward-symbol)
(ease-motion "SPC (" evil-forward-sentence)
(ease-motion "SPC )" evil-backward-sentence)

(ease-motion "SPC n" evil-search-next)
(ease-motion "SPC N" evil-search-previous)

(ease-motion "SPC -" evil-previous-line-first-non-blank)
(ease-motion "SPC +" evil-next-line-first-non-blank)

;;; ======================================
;;; Whitespace mode - visualize whitespace
;;; ======================================
;;(require 'whitespace)
;;(global-whitespace-mode +1)
;; (add-hook 'prog-mode-hook '(lambda ()
;;   (whitespace-mode +1)))

(set-face-background 'trailing-whitespace "#744")
(setq show-trailing-whitespace t)

(eval-after-load 'whitespace
  '(progn
     (setq whitespace-style
       '(
          face
          ;; tabs
          ;; spaces
          ;; trailing
          trailing
          ;; space-mark
          ;; tab-mark
          ))

     ;; dim grey dots are good for guiding the eye
     (set-face-background 'whitespace-space 'nil)
     (set-face-foreground 'whitespace-space "grey30")
     (set-face-attribute 'whitespace-space nil :weight 'light)

     ;; may trailing whitespace blaze like the noonday sun
     (set-face-foreground 'whitespace-trailing "grey60")
     (set-face-background 'whitespace-trailing 'nil)
     (set-face-attribute 'whitespace-trailing 'nil :weight 'extra-bold)

     (set-face-foreground 'whitespace-newline "grey10")
     (set-face-background 'whitespace-newline 'nil)
     (set-face-attribute 'whitespace-newline 'nil :weight 'light)))

(autoload 'indent-guide-mode "indent-guide")
;; (require 'indent-guide)
;; (setq indent-guide-char "│")
;; (set-face-foreground 'indent-guide-face "grey30")
;; (set-face-background 'indent-guide-face 'nil)

;; (indent-guide-global-mode)
;; (require 'indent-vline)

(setq require-final-newline t)
(add-hook 'find-file-hooks
  (lambda ()
    (require 'ws-butler)
    (ws-butler-global-mode +1)))

;; (add-hook 'prog-mode-hook 'indent-hint-mode)

(defun cleanup-buffer-safe ()
  (interactive)
  (untabify (point-min) (point-max))
  (set-buffer-file-coding-system 'utf-8))

(add-hook 'before-save-hook 'cleanup-buffer-safe)

;;; =====================
;;; Intelligent wrap mode
;;; =====================
(require 'adaptive-wrap)
(add-hook 'visual-line-mode-hook 'adaptive-wrap-prefix-mode)
(global-visual-line-mode +1)
(add-hook 'term-mode
  '(lambda ()
     (adaptive-wrap-prefix-mode -1)))

;;; ==============================================
;;; The Minibuffer - command and minibuf goes here
;;; ==============================================
;;; SMEX - interactive command interface
(setq smex-save-file "~/.emacs.d/smex-items")
(setq smex-history-length 10000)
;; SMEX M-x async load

(global-set-key (kbd "C-x M-x") 'execute-extended-command)

(defun auto-smex ()
  (interactive)
  (and (fboundp 'smex-initialize)
    (or (boundp 'smex-cache)
      (smex-initialize)))
  (global-set-key (kbd "M-x") 'smex)
  (smex))

(global-set-key (kbd "M-x") 'auto-smex)

(defadvice smex (around space-inserts-hyphen activate compile)
  (let ((ido-cannot-complete-command
    `(lambda ()
      (interactive)
      (if (string= " " (this-command-keys))
        (insert ?-)
        (funcall ,ido-cannot-complete-command)))))
    ad-do-it))

;;; recursive minibuffers
(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode t)

(define-key evil-normal-state-map (kbd "SPC SPC") 'auto-smex)

;;; =======================================
;;; Flyspell - inline real time spell check
;;; =======================================
;; text mode
(eval-after-load 'flyspell-mode
  '(progn
     (when (locate-file "hunspell" exec-path)
       (setq ispell-program-name "hunspell")
       (add-to-list 'ispell-extra-args "--sug-mode=ultra"))
     (set 'flyspell-issue-message-flag nil)))

(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;;; ============================
;;; Semantic - language analyses
;;; ============================
(add-hook 'prog-mode-hook 'semantic-mode)

;;; =======================================
;;; Line numbers - intelligent line numbers
;;; =======================================
(require 'linum)
(add-hook 'prog-mode-hook
  '(lambda ()
     (when window-system
       (linum-mode +1))))

(eval-after-load 'linum
  '(progn
     (set-face-background 'linum 'nil)
     (set-face-foreground 'linum "grey51")

     (require 'linum-relative)

     ;; truncate current line to four digits
     (defun linum-relative (line-number)
       (let* ((diff1 (abs (- line-number linum-relative-last-pos)))
               (diff (if (minusp diff1)
                       diff1
                       (+ diff1 linum-relative-plusp-offset)))
               (current-p (= diff linum-relative-plusp-offset))
               (current-symbol (if (and linum-relative-current-symbol current-p)
                                 (if (string= "" linum-relative-current-symbol)
                                   (number-to-string (% line-number 1000))
                                   linum-relative-current-symbol)
                                 (number-to-string diff)))
               (face (if current-p 'linum-relative-current-face 'linum)))
         (propertize (format linum-relative-format current-symbol) 'face face)))

     
     (setq linum-relative-current-symbol "")
     (setq linum-relative-format "%3s ")

     (set-face-background 'linum-relative-current-face "grey15")
     (set-face-foreground 'linum-relative-current-face "grey70")

     (set-face-background 'linum-relative-current-face "#073642")
     (set-face-foreground 'linum-relative-current-face "#839496")

     (set-face-attribute 'linum-relative-current-face nil :weight 'extra-bold)

     (global-set-key (kbd "C-c L")
       '(lambda ()
          (interactive)
          (if linum-mode (linum-relative-toggle)
            (linum-mode +1))))))

;;; ================
;;; Auto indent mode
;;; ================
(require 'auto-indent-mode)
(setq auto-indent-key-for-end-of-line-then-newline "M-RET")
(setq auto-indent-key-for-end-of-line-insert-char-then-newline "<M-S-return>")

(defadvice auto-indent-eol-newline (after indent-too activate)
 (indent-for-tab-command))
(defadvice auto-indent-eol-char-newline (after indent-too activate)
 (indent-for-tab-command))

(global-set-key (kbd "M-RET") 'auto-indent-eol-newline)
(global-set-key (kbd "C-M-j") 'auto-indent-eol-newline)
(global-set-key (kbd "<M-S-return>") 'auto-indent-eol-char-newline)

(setq auto-indent-newline-function 'newline-and-indent)
(setq auto-indent-assign-indent-level 4)
(setq auto-indent-backward-delete-char-behavior 'hungry)
(setq auto-indent-indent-style 'conservative)
(auto-indent-global-mode +1)

(defun back-to-indentation-or-beginning ()
   (interactive "^")
   (if (= (point) (progn (back-to-indentation) (point)))
     (beginning-of-line)))

(put 'back-to-indentation-or-beginning 'CUA 'move)
(global-set-key (kbd "<home>") 'back-to-indentation-or-beginning)

(global-set-key (kbd "C-S-y") 'yank)

(defun sfp-page-down (&optional arg)
  (interactive "^P")
  (setq this-command 'next-line)
  (next-line
    (- (window-text-height)
      next-screen-context-lines)))

(put 'sfp-page-down 'isearch-scroll t)
(put 'sfp-page-down 'CUA 'move)
(global-set-key (kbd "<next>") 'sfp-page-down)


(defun sfp-page-up (&optional arg)
  (interactive "^P")
  (setq this-command 'previous-line)
  (previous-line
    (- (window-text-height)
      next-screen-context-lines)))

(put 'sfp-page-up 'isearch-scroll t)
(put 'sfp-page-up 'CUA 'move)
(global-set-key (kbd "<prior>") 'sfp-page-up)

;;; ====================================
;;; snippets - prebaked code just for me
;;; ====================================
(require 'yasnippet)
(setq yas-snippet-dirs "~/.emacs.d/data/snippets/")
(setq yas-trigger-key "C-c TAB")
(yas-global-mode 1)

;; automatic yasnippets
(autoload 'aya-create "auto-yasnippet")
(autoload 'aya-expand "auto-yasnippet")
(global-set-key (kbd "C-c C") 'aya-create)
(global-set-key (kbd "C-c E") 'aya-expand)

;; emmet mode - extensible html/css snippets
(defun autoload-emmet ()
  (interactive)
  (require 'emmet-mode)
  (add-hook 'emmet-mode-hook
    '(lambda ()
       (defun try-expand-emmet (arg)
         (emmet-expand-yas))
       (add-to-list 'hippie-expand-try-functions-list 'try-expand-emmet)))
  (emmet-mode +1))

;; Auto-start on any markup modes
(add-hook 'sgml-mode-hook 'autoload-emmet)
(add-hook 'css-mode-hook  'autoload-emmet)

;;; ============
;;; Emacs tables
;;; ============
(setq max-lisp-eval-depth '40000)
(setq max-specpdl-size '10000)

;;; ============================================
;;; AcuTeX - the most powerful LaTeX editor ever
;;; ============================================
(defun ac-latex-mode-setup ()
  (require 'tex-site)
  (require 'auto-complete-auctex)
  (require 'ac-math)

  (setq-default TeX-master nil)
  (setq TeX-parse-self t)
  (setq TeX-auto-save t)

  (add-to-list 'ac-modes 'latex-mode)
  (setq ac-sources
    (append '(
      ac-source-math-unicode
      ac-source-math-latex
      ac-source-latex-commands)
    ac-sources))
  (ac-auctex-setup))

(add-hook 'LaTeX-mode-hook 'ac-latex-mode-setup)

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

(define-key sp-keymap (kbd "C-M-f") 'sp-forward-sexp)
(define-key sp-keymap (kbd "C-M-b") 'sp-backward-sexp)

(define-key sp-keymap (kbd "C-M-d") 'sp-down-sexp)
(define-key sp-keymap (kbd "C-M-a") 'sp-backward-down-sexp)

(define-key sp-keymap (kbd "C-M-e") 'sp-up-sexp)
(define-key sp-keymap (kbd "C-M-u") 'sp-backward-up-sexp)

(define-key sp-keymap (kbd "C-M-n") 'sp-next-sexp)
(define-key sp-keymap (kbd "C-M-p") 'sp-previous-sexp)

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


;; evil normal mode bindings
(define-prefix-command 'sp-sexp-ops)
(define-key evil-normal-state-map (kbd "g s") 'sp-sexp-ops)
(define-key evil-normal-state-map (kbd "g s f") 'sp-forward-sexp)
(define-key evil-normal-state-map (kbd "g s b") 'sp-backward-sexp)
(define-key evil-normal-state-map (kbd "g s d") 'sp-down-sexp)
(define-key evil-normal-state-map (kbd "g s D") 'sp-backward-down-sexp)
(define-key evil-normal-state-map (kbd "g s e") 'sp-up-sexp)
(define-key evil-normal-state-map (kbd "g s u") 'sp-backward-up-sexp)
(define-key evil-normal-state-map (kbd "g s n") 'sp-next-sexp)
(define-key evil-normal-state-map (kbd "g s p") 'sp-previous-sexp)
(define-key evil-normal-state-map (kbd "g s N") 'sp-select-next-thing)
(define-key evil-normal-state-map (kbd "g s P") 'sp-select-previous-thing)
(define-key evil-normal-state-map (kbd "g s r") 'sp-rewrap-sexp)
(define-key evil-normal-state-map (kbd "g s k") 'sp-kill-sexp)
(define-key evil-normal-state-map (kbd "g s K") 'sp-backwards-kill-sexp)
(define-key evil-normal-state-map (kbd "g s w") 'sp-unwrap-sexp)
(define-key evil-normal-state-map (kbd "g s s") 'sp-forward-slurp-sexp)
(define-key evil-normal-state-map (kbd "g s b") 'sp-forward-barf-sexp)
(define-key evil-normal-state-map (kbd "g s S") 'sp-backward-slurp-sexp)
(define-key evil-normal-state-map (kbd "g s B") 'sp-backward-barf-sexp)

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

     (sp-local-pair 'haskell-mode "'" nil :actions nil)
     (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
     (sp-local-pair 'emacs-lisp-mode "`" nil :when '(sp-in-string-p))))

(add-hook 'LaTeX-mode-hook '(lambda ()
  (require 'smartparens-latex)))

(add-hook 'sgml-mode-hook '(lambda ()
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
  (interactive)
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

(set-face-foreground 'rainbow-delimiters-depth-1-face "#dddddd")
(set-face-foreground 'rainbow-delimiters-depth-2-face "#fce94f")
(set-face-foreground 'rainbow-delimiters-depth-3-face "#8cc4ff")
(set-face-foreground 'rainbow-delimiters-depth-4-face "#73d216")
(set-face-foreground 'rainbow-delimiters-depth-5-face "#fcaf3e")
(set-face-foreground 'rainbow-delimiters-depth-6-face "#e6a8df")
(set-face-foreground 'rainbow-delimiters-depth-7-face "#b4fa70")
(set-face-foreground 'rainbow-delimiters-depth-8-face "#acd2f8")
(set-face-foreground 'rainbow-delimiters-depth-9-face "#6a8057")

(defun rainbow-delimiters-wash (arg)
  (egoge-wash-out-face 'rainbow-delimiters-depth-1-face arg)
  (egoge-wash-out-face 'rainbow-delimiters-depth-2-face arg)
  (egoge-wash-out-face 'rainbow-delimiters-depth-3-face arg)
  (egoge-wash-out-face 'rainbow-delimiters-depth-4-face arg)
  (egoge-wash-out-face 'rainbow-delimiters-depth-5-face arg)
  (egoge-wash-out-face 'rainbow-delimiters-depth-6-face arg)
  (egoge-wash-out-face 'rainbow-delimiters-depth-7-face arg)
  (egoge-wash-out-face 'rainbow-delimiters-depth-8-face arg)
  (egoge-wash-out-face 'rainbow-delimiters-depth-9-face arg))

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

;;; ======================
;;; Highlight current line
;;; ======================
(require 'highlight-current-line)
(highlight-current-line-on "y")

(highlight-current-line-set-bg-color "grey20")
(when (display-graphic-p)
  (highlight-current-line-set-bg-color "#073642"))

(column-number-mode +1)
;;(size-indication-mode +1)

;; highlight-current-symbol
(require 'auto-highlight-symbol)
(define-key auto-highlight-symbol-mode-map (kbd "M-<left>") nil)
(define-key auto-highlight-symbol-mode-map (kbd "M-<right>") nil)
(define-key auto-highlight-symbol-mode-map (kbd "C-c <left>") 'ahs-backward)
(define-key auto-highlight-symbol-mode-map (kbd "C-c <right>") 'ahs-forward)
(global-auto-highlight-symbol-mode +1)

(set-face-background 'ahs-definition-face "grey10")
(set-face-foreground 'ahs-definition-face 'nil)

(set-face-background 'ahs-plugin-defalt-face "grey20")
(set-face-foreground 'ahs-plugin-defalt-face 'nil)
(set-face-background 'ahs-face 'nil)
(set-face-foreground 'ahs-face 'nil)
(set-face-attribute 'ahs-face nil :weight 'semi-bold)

(global-set-key (kbd "C-c <left>") 'auto-ahs-backward)
(global-set-key (kbd "C-c <right>") 'auto-ahs-forward)
(define-key auto-highlight-symbol-mode-map (kbd "M-S-<left>") nil)
(define-key auto-highlight-symbol-mode-map (kbd "M-S-<right>") nil)

;;; ====================================
;;; flycheck - real-time syntax checking
;;; ====================================

(add-hook 'prog-mode-hook
  '(lambda ()
     (require 'flycheck)
     (add-hook 'flycheck-mode-hook #'flycheck-haskell-setup)
     (setq flycheck-mode-line-lighter " ✓")))

(defun my-display-error-messages-condensed (errors)
  (-when-let (messages (-keep #'flycheck-error-message errors))
    (when (flycheck-may-use-echo-area-p)
      (display-message-or-buffer (s-join "\n" messages)
        flycheck-error-message-buffer))))

(eval-after-load 'flycheck
  '(progn
     '(setq flycheck-display-errors-function #'my-display-error-messages-condensed)
     (global-flycheck-mode +1)))

;;; ================================
;;; Auto-complete - self explanatory
;;; ================================
(require 'auto-complete-config)

(global-auto-complete-mode)
(add-hook 'text-mode-hook '(lambda ()
  (auto-complete-mode +1)))
(ac-set-trigger-key "C-c <C-tab>")

(ac-config-default)
(ac-flyspell-workaround)
(ac-linum-workaround)
(setq ac-sources
  '(ac-source-abbrev
     ac-source-yasnippet
     ac-source-semantic
     ac-source-dictionary
     ac-source-filename
     ac-source-words-in-buffer
     ac-source-words-in-same-mode-buffers))

(setq ac-auto-start t
  ac-auto-show-menu 0.5
  ac-show-menu-immediately-on-auto-complete t
  ac-ignore-case 'smart
  ac-delay 0
  ac-dwim t
  ac-use-fuzzy t
  ac-fuzzy-enable t
  ac-dwim-enable t
  ac-use-comphist t
  popup-use-optimized-column-computation nil)

(add-to-list 'ac-dictionary-directories "~/.emacs.d/elpa/auto-complete-20130724.1750/dict/")
(add-to-list 'ac-trigger-commands 'backwards-kill-char)

;;; =========================================
;;; company-mode alternative to auto-complete
;;; =========================================
(setq
  company-idle-delay 0.1
  company-minimum-prefix-length 2)

;;; ==================================================
;;; Hippie expand - secondary autocompletion framework
;;; ==================================================
(defun try-expand-flx (old)
  "Try to complete word using flx matching."
  (if (not old)
    (progn
      (he-init-string (he-lisp-symbol-beg) (point))
      (if (not (he-string-member he-search-string he-tried-table))
        (setq he-tried-table (cons he-search-string he-tried-table)))
      (setq he-expand-list
        (and (not (equal he-search-string ""))
          (try-expand-flx-collect he-search-string)))))
  (while (and he-expand-list
           (he-string-member (first he-expand-list) he-tried-table))
    (setq he-expand-list (rest he-expand-list)))
  (if (null he-expand-list)
    (progn
      (if old (he-reset-string)) ())
    (progn
      (he-substitute-string (first he-expand-list))
      (setq he-expand-list (rest he-expand-list))
      t)))

(defun try-expand-flx-collect (str)
  "Find and collect all words that flex-match str, and sort by flx score"
  (let ((coll '())
         (regexp (try-expand-flx-regexp str)))
    (save-excursion
      (goto-char (point-min))
      (while (search-forward-regexp regexp nil t)
        (setq coll (cons (thing-at-point 'symbol) coll))))
    (setq coll (sort coll #'(lambda (a b)
                              (> (first (flx-score a str))
                                (first (flx-score b str))))))
    coll))

(defun try-expand-flx-regexp (str)
  "Generate regexp for flexible matching of str."
  (concat "\\b" (mapconcat (lambda (x) (concat "\\w*-*" (list x))) str "")
          "\\w*-*" "\\b"))

(global-set-key (kbd "M-/") 'hippie-expand)
(setq hippie-expand-try-functions-list
  '(yas-hippie-try-expand
     try-expand-dabbrev
     try-expand-dabbrev-from-kill
     try-expand-flx
     try-expand-dabbrev-all-buffers
     try-complete-file-name-partially
     try-complete-file-name
     try-expand-all-abbrevs
     try-expand-list
     try-expand-line
     try-complete-lisp-symbol-partially
     try-complete-lisp-symbol))

;;; ==============================================
;;; Region bindings - bindings when regions are on
;;; ==============================================
(add-hook 'activate-mark-hook
  '(lambda (&optional arg)
     (unless (and (boundp 'region-bindings-mode) region-bindings-mode)
       (require 'region-bindings-mode))))

(eval-after-load 'region-bindings-mode
  '(progn
     (region-bindings-mode-enable)
     (raise-minor-mode-map-alist region-bindings-mode)
     (define-key region-bindings-mode-map "a" 'mc/mark-all-like-this)
     (define-key region-bindings-mode-map "p" 'mc/mark-previous-like-this)
     (define-key region-bindings-mode-map "n" 'mc/mark-next-like-this)
     (define-key region-bindings-mode-map "k" 'kill-region)))

;;; ================================
;;; Smart indent/unindent with <tab>
;;; ================================
(setq my-tab-width 2)
(defun indent-block ()
  (interactive)
  (shift-region my-tab-width)
  (setq deactivate-mark nil))

(defun unindent-block ()
  (interactive)
  (shift-region (- my-tab-width))
  (setq deactivate-mark nil))

(defun shift-region (numcols)
  " my trick to expand the region to the beginning and end of the area selected
 much in the handy way I liked in the Dreamweaver editor."
  (if (< (point) (mark))
    (if (not (bolp))
      (progn
        (beginning-of-line)
        (exchange-point-and-mark)
        (end-of-line)))
    (progn
      (end-of-line)
      (exchange-point-and-mark)
      (beginning-of-line)))

  (setq region-start (region-beginning))
  (setq region-finish (region-end))

  (save-excursion
    (if (< (point) (mark))
      (exchange-point-and-mark))
    (let ((save-mark (mark)))
      (indent-rigidly
        region-start
        region-finish
        numcols))))

(defun my-unindent ()
  "Unindent line, or block if it's a region selected.
When pressing Shift+tab, erase words backward (one at a time) up to the beginning of line."

  (interactive)
  (if mark-active
    (unindent-block)
    (progn
      (unless (bolp)
        (if (looking-back "^[ \t]*")
          (progn
            ;;"a" holds how many spaces are
            ;; there to the beginning of the line
            (let ((a (length
                       (buffer-substring-no-properties
                         (point-at-bol)
                         (point)))))
              (progn
                ;; delete backwards progressively
                ;; in my-tab-width steps, but without
                ;; going further of the beginning of line.
                (if (> a my-tab-width)
                  (delete-backward-char my-tab-width)
                  (backward-delete-char a)))))
          ;; delete tab and spaces first,
          ;; if at least 2 exist, before removing words
          (progn
            (if (looking-back "[ \t]\\{2,\\}")
              (delete-horizontal-space)
              (backward-kill-word 1))))))))

(defun indent-or-complete (arg)
  "Indent region selected as a block; if no selection present either indent according to mode,
or expand the word preceding point. Multiple tabs cycle indentation level."
  (interactive "P")
  (if mark-active
    (progn
      (setq this-command 'indent-block)
      (indent-block)
      (message "indent block"))

    (if (minibufferp)
      (unless (minibuffer-complete)
        (hippie-expand))

      (if (looking-at "\\>")
        (if (and (fboundp 'auto-complete-mode) auto-complete-mode)
          (progn
            (setq this-command 'ac-trigger-key-command)
            (ac-trigger-key-command)
            (message "auto-complete"))

          (if (and (fboundp 'company-complete) company-mode)
            (progn
              (setq this-command 'company-complete)
              (company-complete)
              (message "company-complete"))
            
            (progn
              (setq this-command 'hippie-expand)
              (hippie-expand arg)
              (message "hippie-expand"))))

        (progn
          (setq this-command 'indent-for-tab-command)
          (indent-for-tab-command arg)
          (message "indent"))))))

;;; =============================
;;; Magit - fast, interactive git
;;; =============================
(global-set-key (kbd "C-c C-g") 'magit-status)

(evil-set-initial-state 'svn-status-mode 'insert)
(evil-set-initial-state 'magit-status-mode 'insert)

(setq magit-completing-read-function
  'magit-ido-completing-read)

(eval-after-load 'magit
  '(progn
     (raise-minor-mode-map-alist 'magit-status-mode-map)
     (define-key magit-log-mode-map (kbd "k") 'previous-line)
     (define-key magit-log-mode-map (kbd "j") 'next-line)
     (define-key magit-status-mode-map (kbd "k") 'previous-line)
     (define-key magit-status-mode-map (kbd "K") 'magit-discard-item)
     (define-key magit-status-mode-map (kbd "j") 'next-line)))

(add-hook 'magit-status-mode-hook
  '(lambda ()
     (set-input-method "TeX")))

;; and psvn for svn not-so-awesomeness
(autoload 'svn-status "psvn")

(eval-after-load 'psvn
  '(progn
     (setq svn-status-verbose nil)))

(global-set-key (kbd "C-c C-c s") 'svn-status)

;;; ==============================
;;; Ido - interactively do things
;;; ==============================
(ido-mode +1)
(require 'flx-ido)
(require 'ido-ubiquitous)
(ido-ubiquitous +1)
(flx-ido-mode +1)

(set-face-foreground 'flx-highlight-face "grey70")
(set-face-background 'flx-highlight-face "grey20")
(set-face-attribute 'flx-highlight-face nil :underline nil)

(setq ido-save-directory-list-file "~/.emacs.d/ido.last")

(defun ido-goto-symbol (&optional symbol-list)
  "Refresh imenu and jump to a place in the buffer using Ido."
  (interactive)
  (unless (featurep 'imenu)
    (require 'imenu nil t))
  (cond
    ((not symbol-list)
      (let ((ido-mode ido-mode)
             (ido-enable-flex-matching
               (if (boundp 'ido-enable-flex-matching)
                 ido-enable-flex-matching t))
             name-and-pos symbol-names position)
        (unless ido-mode
          (ido-mode 1)
          (setq ido-enable-flex-matching t))
        (while (progn
                 (imenu--cleanup)
                 (setq imenu--index-alist nil)
                 (ido-goto-symbol (imenu--make-index-alist))
                 (setq selected-symbol
                   (ido-completing-read "Symbol? " symbol-names))
                 (string= (first imenu--rescan-item) selected-symbol)))
        (unless (and (boundp 'mark-active) mark-active)
          (push-mark nil t nil))
        (setq position (rest (assoc selected-symbol name-and-pos)))
        (cond
          ((overlayp position)
            (goto-char (overlay-start position)))
          (t
            (goto-char position)))))
    ((listp symbol-list)
      (dolist (symbol symbol-list)
        (let (name position)
          (cond
            ((and (listp symbol) (imenu--subalist-p symbol))
              (ido-goto-symbol symbol))
            ((listp symbol)
              (setq name (first symbol))
              (setq position (rest symbol)))
            ((stringp symbol)
              (setq name symbol)
              (setq position
                (get-text-property 1 'org-imenu-marker symbol))))
          (unless (or (null position) (null name)
                    (string= (first imenu--rescan-item) name))
            (add-to-list 'symbol-names name)
            (add-to-list 'name-and-pos (cons name position))))))))

(global-set-key (kbd "M-=") 'ido-goto-symbol)

;; hippie expand with ido
(defun my-hippie-expand-completions (&optional hippie-expand-function)
  "Return the full list of possible completions generated by `hippie-expand'.
The optional argument can be generated with `make-hippie-expand-function'."
  (let ((this-command 'my-hippie-expand-completions)
        (last-command last-command)
        (buffer-modified (buffer-modified-p))
        (hippie-expand-function (or hippie-expand-function 'hippie-expand)))
    (cl-flet ((ding))
      ;; avoid the (ding) when hippie-expand exhausts its options.
      (while (progn
               (funcall hippie-expand-function nil)
               (setq last-command 'my-hippie-expand-completions)
               (not (equal he-num -1)))))
    ;; Evaluating the completions modifies the buffer,
    ;; however we will finish up in the same state that we
    ;; began, and (save-current-buffer) seems a
    ;; bit heavyweight in the circumstances.
    (set-buffer-modified-p buffer-modified)
    ;; Provide the options in the order in which they are normally generated.
    (delete he-search-string (reverse he-tried-table))))

(defmacro my-ido-hippie-expand-with (hippie-expand-function)
  "Generate an interactively-callable function that offers ido-based completion
using the specified hippie-expand function."
  `(call-interactively
    (lambda (&optional selection)
      (interactive
       (let ((options (my-hippie-expand-completions ,hippie-expand-function)))
         (if options
             (list (ido-completing-read "Completions: " options)))))
      (if selection
        (he-substitute-string selection t)
        (message "No expansion found")))))

(defun my-ido-hippie-expand ()
  "Offer ido-based completion for the word at point."
  (interactive)
  (my-ido-hippie-expand-with 'hippie-expand))

(global-set-key (kbd "C-c /") 'my-ido-hippie-expand)

;;; ==========
;;; Projectile
;;; ==========
;; (require 'projectile)
(autoload 'projectile-find-file "projectile")
(autoload 'projectile-find-dir "projectile")
(autoload 'projectile-switch-project "projectile")
(autoload 'projectile-switch-to-buffer "projectile")
(autoload 'projectile-project-p "projectile")
(autoload 'projectile-mode "projectile")

(setq projectile-completion-system 'ido)
(setq projectile-enable-caching t)
(setq projectile-require-project-root t)

(global-set-key (kbd "C-c C-f") 'projectile-find-file)
(global-set-key (kbd "C-c C-d") 'projectile-find-dir)
(global-set-key (kbd "C-c C-p") 'projectile-switch-project)
(global-set-key (kbd "C-c C-b") 'projectile-switch-to-buffer)
(global-set-key (kbd "C-c p f") 'projectile-find-file)
(global-set-key (kbd "C-c p d") 'projectile-find-dir)
(global-set-key (kbd "C-c p p") 'projectile-switch-project)
(global-set-key (kbd "C-c p b") 'projectile-switch-to-buffer)

(autoload '->> "dash")
(setq my-projectile-project-root-files
  '(".projectile"       ; projectile project marker
     ".git"             ; Git VCS root dir
     ".hg"              ; Mercurial VCS root dir
     ".fslckout"        ; Fossil VCS root dir
     ".bzr"             ; Bazaar VCS root dir
     "_darcs"           ; Darcs VCS root dir
     "rebar.config"     ; Rebar project file
     "project.clj"      ; Leiningen project file
     "pom.xml"          ; Maven project file
     "build.sbt"        ; SBT project file
     "build.gradle"     ; Gradle project file
     "Gemfile"          ; Bundler file
     "requirements.txt" ; Pip file
     "Makefile"         ; Make project file
     ".svn"             ; SVN project file
     ))

(defun my-projectile-file-truename (file-name)
  "A thin wrapper around `expand-file-name' that handles nil.
Expand FILE-NAME using `default-directory'."
  (when file-name
    (file-truename file-name)))

(defun my-projectile-project-root ()
  (interactive)
  "Retrieves the root directory of a project if available.
The current directory is assumed to be the project's root otherwise."
  (or (->> my-projectile-project-root-files
        (--map (locate-dominating-file default-directory it))
        (-remove #'null)
        (car)
        (my-projectile-file-truename))
    ()))

;;; ==============================================
;;; Helm - the ultimate interactive command system
;;; ==============================================
(require 'helm-config)
(require 'helm-imenu)
(require 'helm-files)

(run-with-idle-timer 5 nil
  '(lambda ()
     (interactive)
     (require 'helm-ring)
     (run-with-idle-timer 5 nil
       '(lambda ()
          (interactive)
          (require 'helm-misc)))))

(eval-after-load 'helm
  '(progn
     (setq helm-locate-command "locate %s -r %s -be -l 999")
     (setq helm-ff-transformer-show-only-basename nil)
     (setq helm-buffers-fuzzy-matching t)
     
     ;; (set-face-background 'helm-source-header "grey15")
     ;; (set-face-foreground 'helm-source-header "#e6a8df")
     ;; (set-face-background 'helm-selection     "grey40")
     ;; (set-face-background 'helm-candidate-number "#fcaf3e")
     ;; (set-face-foreground 'helm-selection     'nil);"#fcaf3e")
     (set-face-attribute 'helm-selection nil :underline 'nil)
     (add-to-list 'helm-boring-file-regexp-list "\\.undo.xz$")
     (add-to-list 'helm-boring-file-regexp-list "\\#$")
     (setq helm-ff-newfile-prompt-p 'nil)

     (global-set-key (kbd "C-x f") 'ido-find-file)
     (global-set-key (kbd "C-S-x C-S-f") 'icicle-find-file)
     (global-set-key (kbd "C-S-X C-S-B") 'icicle-buffer)

     (global-set-key (kbd "C-c f") 'fiplr-find-file)
     (global-set-key (kbd "C-c C-e") 'helm-eval-expression)
     (global-set-key (kbd "C-c C-s") 'helm-swoop)

     (setq helm-locate
       `((name . "Locate")
          (init . helm-locate-set-command)
          (candidates-process . helm-locate-init)
          (type . file)
          (requires-pattern . 3)
          (history . ,'helm-file-name-history)
          (keymap . ,helm-generic-files-map)
          (help-message . helm-generic-file-help-message)
          (candidate-number-limit . 999)
          (mode-line . helm-generic-file-mode-line-string)))

     (defun my-helm-buffers (&rest arg)
       (interactive)
       (helm-other-buffer
         '(helm-source-buffers-list
            helm-source-recentf
            helm-source-files-in-current-dir
            helm-source-files-in-all-dired
            helm-source-buffer-not-found)
         "*helm-find-files"))

     (global-set-key (kbd "C-x C-b") 'my-helm-buffers)
     (global-set-key (kbd "C-x b") 'ido-switch-buffer)

     (defun my-helm-find-files (&rest arg)
       (interactive)
       (helm-other-buffer
         '(helm-source-recentf
            helm-source-buffers-list
            helm-source-files-in-current-dir
            helm-source-find-files
            helm-source-findutils
            helm-source-locate
            ;; helm-source-tracker-search
            )
         "*helm-find-files"))

     (global-set-key (kbd "C-x C-f") 'my-helm-find-files)
     
     (defvar my-helm-source-evaluation-result
       '((name . "Evaluation Result")
          (init . (lambda () (require 'edebug)))
          (dummy)
          (multiline)
          (mode-line . "C-RET: nl-and-indent, tab: reindent, C-tab:complete, C-p/n: next/prec-line.")
          (filtered-candidate-transformer .
            (lambda (candidates source)
              (list
                (condition-case nil
                  (with-helm-current-buffer
                    (pp-to-string
                      (if edebug-active
                        (edebug-eval-expression
                          (read helm-pattern))
                        (eval (read helm-pattern)))))
                  (error "")))))
          (action . (("Copy result to kill-ring" .
                       (lambda (candidate)
                         (with-current-buffer helm-buffer
                           (let ((end (save-excursion
                                        (goto-char (point-max))
                                        (search-backward "\n")
                                        (point))))
                             (kill-region (point) end)))))
                      ("copy sexp to kill-ring" .
                        (lambda (candidate)
                          (kill-new helm-input)))))))

     (defun my-helm-omni (&rest arg)
       (interactive)
       (helm-occur-init-source)
       (unless (fboundp 'helm-source-kill-ring)
         (require 'helm-ring))
       (unless (fboundp 'helm-source-lacarte)
         (require 'helm-misc))

       (let ((bufs (list (buffer-name (current-buffer)))))
         (helm-attrset 'moccur-buffers bufs helm-source-occur)
         (helm-set-local-variable 'helm-multi-occur-buffer-list bufs)
         (helm-set-local-variable
           'helm-multi-occur-buffer-tick
           (cl-loop for b in bufs
             collect (buffer-chars-modified-tick (get-buffer b))))
         (helm-other-buffer
           (append '(helm-source-buffers-list)

             ;; projectile explodes when not in project
             (when (my-projectile-project-root)
               (unless (fboundp 'helm-source-projectile-files-list)
                 (require 'helm-projectile))
               '(helm-source-projectile-recentf-list
                  helm-source-projectile-files-list
                  helm-source-projectile-buffers-list))

             '( ;; files
                helm-source-file-cache
                helm-source-recentf
                helm-source-files-in-current-dir
                helm-source-bookmarks
                ;; code search
                helm-source-imenu
                helm-source-occur
                ;; internal
                helm-source-kill-ring
                helm-source-mark-ring
                helm-source-register
                ;; code construction
                ;; helm-source-regexp
                helm-source-lacarte
                my-helm-source-evaluation-result
                ;; file location
                helm-source-findutils
                helm-source-locate
                ;; helm-source-tracker-search
                ;; fallback
                ;; helm-source-buffer-not-found
                ))
           "*helm-omni*")))

     (global-set-key (kbd "C-c C-o") 'my-helm-omni)
     (define-key evil-normal-state-map (kbd "C-c C-o") 'my-helm-omni)
     (define-key evil-insert-state-map (kbd "C-c C-o") 'my-helm-omni)
     (define-key evil-emacs-state-map (kbd "C-c C-o") 'my-helm-omni)
     (define-key evil-motion-state-map (kbd "C-c C-o") 'my-helm-omni)
     (define-key evil-replace-state-map (kbd "C-c C-o") 'my-helm-omni)))

;;; ==================================
;;; icicles - supercharged completions
;;; ==================================
(require 'icicles)
;; (run-with-idle-timer 2 nil
;;   '(lambda ()
;;      (interactive)
;;      (require 'icicles)))

(eval-after-load 'icicles
  '(progn
     ;; (icy-mode +1)
     ;; (icicle-ido-like-mode +1)

     (setq icicle-region-background "grey20")

     (setq icicle-highlight-lighter-flag nil)
     (setq icicle-max-candidates 500)
     (setq icicle-default-cycling-mode 'apropos)
     (setq icicle-show-multi-completion-flag t)
     (setq icicle-search-highlight-all-current-flag t)
     
     (setq icicle-command-abbrev-match-all-parts-flag nil)
     ;; (setq icicle-add-proxy-candidates-flag t)
     (setq icicle-highlight-input-completion-failure-delay 0)
     (setq icicle-Completions-text-scale-decrease 0.2)
     ;; (add-to-list 'icicle-TAB-completion-methods 'fuzzy)
     (setq icicle-TAB-completion-methods '(vanilla substring basic))
     (setq locate-command "locate")
     (setq icicle-completions-format "vertical")

     ;; (setq icicle-use-C-for-actions-flag nil)
     (setq icicle-incremental-completion t)
     (setq icicle-incremental-completion-delay 0.1)
     (setq icicle-show-Completions-initially-flag t)
     ;; (setq icicle-top-level-when-sole-completion-flag t)
     ;; (setq icicle-top-level-when-sole-completion-delay 0.3)
     
     (set-face-background 'icicle-candidate-part "grey15")
     (set-face-background 'icicle-completion "grey15")
     (set-face-background 'icicle-mode-line-help "grey30")
     (set-face-foreground 'icicle-mode-line-help "#73d216")
     (set-face-foreground 'icicle-complete-input "#b4fa70")

     (set-face-foreground 'icicle-common-match-highlight-Completions "grey70")
     (set-face-background 'icicle-common-match-highlight-Completions "grey20")

     (set-face-foreground 'icicle-candidate-part "grey70")
     (set-face-background 'icicle-candidate-part "grey20")

     (set-face-foreground 'icicle-current-candidate-highlight "#fce94f")
     (set-face-background 'icicle-current-candidate-highlight "grey20")

     (set-face-background 'icicle-key-complete-menu-local "grey15")
     (set-face-foreground 'icicle-key-complete-menu-local "#e6a8df")

     (set-face-background 'icicle-special-candidate "grey10")
     (set-face-foreground 'icicle-special-candidate "#e6a8df")

     (set-face-background 'icicle-Completions-instruction-1 "grey15")
     (set-face-foreground 'icicle-Completions-instruction-1 "grey70")
     
     (defadvice icicle-other-window-or-frame (around buffers-too activate)
       (if (and
             (= (length (window-list)) 1)
             (= (length (frame-list)) 1))
         (if (and (elscreen-get-screen-list)
               (> (length (elscreen-get-screen-list)) 1))
           (elscreen-toggle)
           (progn
             (switch-to-buffer (other-buffer))))
         ad-do-it))

     (add-hook 'icicle-minibuffer-setup-hook
       '(lambda ()
          (interactive)
          (setq icicle-Completions-window-max-height (/ (* (frame-height) 2) 3))))))

(global-set-key (kbd "C-c C-c o") 'icicle-occur)
(global-set-key (kbd "C-c C-c <tab>") 'icicle-dabbrev-completion)

(define-key evil-normal-state-map (kbd "<backtab>") 'icicle-complete-keys)
(define-key evil-normal-state-map (kbd "C-h <backtab>") 'icicle-complete-keys)
(define-key evil-normal-state-map (kbd "C-c <backtab>") 'icicle-complete-keys)
(define-key evil-normal-state-map (kbd "C-S-x <backtab>") 'icicle-complete-keys)
(define-key evil-normal-state-map (kbd "C-w <backtab>") 'icicle-complete-keys)
(define-key evil-normal-state-map (kbd "C-x <backtab>") 'icicle-complete-keys)
(define-key evil-normal-state-map (kbd "M-g <backtab>") 'icicle-complete-keys)
(define-key evil-normal-state-map (kbd "M-s <backtab>") 'icicle-complete-keys)
(define-key evil-normal-state-map (kbd "M-o <backtab>") 'icicle-complete-keys)
(define-key evil-normal-state-map (kbd "z <backtab>") 'icicle-complete-keys)
(define-key evil-normal-state-map (kbd "Z <backtab>") 'icicle-complete-keys)
(define-key evil-normal-state-map (kbd "g <backtab>") 'icicle-complete-keys)
(define-key evil-normal-state-map (kbd "<f2> <backtab>") 'icicle-complete-keys)
(define-key evil-normal-state-map (kbd "<f1> <backtab>") 'icicle-complete-keys)
(define-key evil-normal-state-map (kbd "C-<menu> <backtab>") 'icicle-complete-keys)
(define-key evil-normal-state-map (kbd "C-l <backtab>") 'icicle-complete-keys)
(define-key evil-normal-state-map (kbd "[ <backtab>") 'icicle-complete-keys)
(define-key evil-normal-state-map (kbd "] <backtab>") 'icicle-complete-keys)
(define-key evil-normal-state-map (kbd ", <backtab>") 'icicle-complete-keys)

(define-key evil-normal-state-map (kbd "C-s") 'icicle-search)
(define-key evil-insert-state-map (kbd "C-S-s") 'icicle-search)

(defun switch-to-minibuffer-window ()
  "switch to minibuffer window (if active)"
  (interactive)
  (when (active-minibuffer-window)
    (select-window (active-minibuffer-window))))

(global-set-key (kbd "C-'") 'switch-to-minibuffer-window)


;;; ==================================
;;; Tramp -- transparent remote access
;;; ==================================
(setq tramp-chunksize 500)
(setq tramp-default-method "ssh")
(eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))

;;; ====================================
;;; emacsclient - instantaneous startup!
;;; ====================================
(defun emacsclient-setup (&rest args)
  (interactive)

  (global-set-key (kbd "<mouse-4>")
    '(lambda ()
       (interactive)
       (scroll-down-line)
       (scroll-down-line)
       (scroll-down-line)))

  (global-set-key (kbd "<mouse-5>")
    '(lambda ()
       (interactive)
       (scroll-up-line)
       (scroll-up-line)
       (scroll-up-line)))

  (set-face-foreground 'rainbow-delimiters-depth-1-face "#dddddd")
  (set-face-foreground 'rainbow-delimiters-depth-2-face "#fce94f")
  (set-face-foreground 'rainbow-delimiters-depth-3-face "#b4fa70")
  (set-face-foreground 'rainbow-delimiters-depth-4-face "#8cc4ff")
  (set-face-foreground 'rainbow-delimiters-depth-5-face "#fcaf3e")
  (set-face-foreground 'rainbow-delimiters-depth-6-face "#73d216")
  (set-face-foreground 'rainbow-delimiters-depth-7-face "#e6a8df")
  (set-face-foreground 'rainbow-delimiters-depth-8-face "#acd2f8")
  (set-face-foreground 'rainbow-delimiters-depth-9-face "#6a8057")
  (rainbow-delimiters-wash 1.5)
  
  (set-face-background 'fringe "#022F3A"))

(add-hook 'after-make-frame-functions
  '(lambda (&rest args)
     (message "make frame")
     (load-elscreen)
     (when (and (fboundp 'server-running-p) (server-running-p))
       (run-at-time 0.0 'nil 'emacsclient-setup))))

(add-hook 'server-visit-hook
  '(lambda ()
     (message "server visit hook")
     (run-at-time 0.0 'nil 'emacsclient-setup)))

(add-hook 'server-switch-hook
  '(lambda ()
     (message "server switch hook")
     (run-at-time 0.0 'nil 'emacsclient-setup)))

;;; =================================
;;; Multiple cursors - blow your mind
;;; =================================
(require 'multiple-cursors)
(global-set-key (kbd "C-c l") 'mc/edit-lines)
(global-set-key (kbd "C-c a") 'mc/mark-all-like-this-dwim)
(global-set-key (kbd "C-c m") 'mc/mark-more-like-this-extended)
(global-set-key (kbd "C-c s") 'mc/mark-all-in-region)
(global-set-key (kbd "C-c M") 'mc/mark-pop)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)

(global-set-key (kbd "<C-S-mouse-1>") 'mc/add-cursor-on-click)
(global-set-key (kbd "<C-M-mouse-1>") 'mc/add-cursor-on-click)
(global-set-key (kbd "C-c C-SPC") 'set-rectangular-region-anchor)

;; (eval-after-load 'mc/edit-lines
;;   '(require 'multiple-cursors))
;; (eval-after-load 'mc/mark-next-like-this
;;   '(require 'multiple-cursors))
;; (eval-after-load 'mc/mark-previous-like-this
;;   '(require 'multiple-cursors))
;; (eval-after-load 'mc/mark-all-like-this-dwim
;;   '(require 'multiple-cursors))
;; (eval-after-load 'mc/mark-more-like-this-extended
;;   '(require 'multiple-cursors))
;; (eval-after-load 'mc/mark-all-in-region
;;   '(require 'multiple-cursors))
;; (eval-after-load 'mc/mark-pop
;;   '(require 'multiple-cursors))

;; alignment keys
(global-set-key (kbd "C-|") 'align-regexp)


(require 'whole-line-or-region)
(defun my-wlr-easy-kill (&optional prefix)
  (interactive "*p")
  (whole-line-or-region-call-with-region 'easy-kill prefix))

(defun my-wlr-cua-cut-region (&optional prefix)
  (interactive "p")
  (whole-line-or-region-call-with-region
    '(lambda (&optional beg end)
       (goto-char beg)
       (set-mark-command prefix)
       (cua-set-mark)
       (goto-char end)
       (cua-cut-region current-prefix-arg)
       (message ""))))

(defun whole-line-or-region-yank (raw-prefix &optional string-in)
  "Yank (paste) previously killed text.

If the text to be yanked was killed with a whole-line-or-region
function *as* a whole-line, then paste it as a whole line (i.e. do not
break up the current line, and do not force the user to move point).

RAW-PREFIX is used to determine which string to yank, just as `yank'
would normally use it.

Optionally, pass in string to be \"yanked\" via STRING-IN."
  (interactive "*P")

  ;; figure out what yank would do normally
  (let ((string-to-yank
          (or string-in
            (current-kill
              (cond ((listp raw-prefix) 0)
                ((eq raw-prefix '-) -1)
                (t (1- raw-prefix))) t)))
         (saved-column (current-column)))

    ;; check for whole-line prop in yanked text
    (if (get-text-property 0 'whole-line-or-region string-to-yank)
      (let ((beg (line-beginning-position)))
        ;; goto beg of line and yank
        (beginning-of-line)
        (if string-in
          ;; insert "manually"
          (insert string-in)
          ;; just yank as normal
          (cua-paste raw-prefix))

        ;; a whole-line killed from end of file may not have a
        ;; trailing newline -- add one, in these cases
        (when (not (string-match "\n$" string-to-yank))
          (insert "\n")
          (previous-line 1))

        ;; restore state of being....
        (move-to-column saved-column)
        (remove-text-properties beg (+ beg 1) '(whole-line-or-region nil)))

      ;; no whole-line-or-region mark
      (if string-in
        ;; insert "manually"
        (progn
          (when (and delete-selection-mode
                  mark-active)
            (delete-active-region))
          (insert string-in))
        ;; just yank as normal
        (cua-paste raw-prefix)))))

(defun easy-kill-on-my-line (_n)
  "Get current line, but mark as a whole line for whole-line-or-region"
  (let ((str (thing-at-point 'line))
         (beg (line-beginning-position)))
    (save-excursion
      (put-text-property 0 1 'whole-line-or-region t str)
      (easy-kill-adjust-candidate 'my-line str))))

(setq easy-kill-try-things '(url email my-line))

(define-key evil-insert-state-map (kbd "C-w") nil)
(define-key evil-insert-state-map (kbd "<remap> <kill-region>") 'my-wlr-cua-cut-region)
(define-key evil-insert-state-map (kbd "<remap> <kill-ring-save>") 'easy-kill)
(define-key evil-normal-state-map (kbd "<remap> <kill-ring-save>") 'easy-kill)
(define-key evil-insert-state-map (kbd "C-y") 'whole-line-or-region-yank)

(define-key evil-emacs-state-map (kbd "<remap> <kill-region>") 'my-wlr-cua-cut-region)
(define-key evil-emacs-state-map (kbd "<remap> <kill-ring-save>") 'easy-kill)
(define-key evil-emacs-state-map (kbd "C-y") 'whole-line-or-region-yank)

;; fast cursor placement
(evil-leader/set-key
  "n" 'mc/mark-next-like-this
  "p" 'mc/mark-previous-like-this)

;;; ==============================
;;; Teleport handlers - move fast!
;;; ==============================
;;; expand-region - expand selection by semantic units
(define-key evil-insert-state-map (kbd "M-n") 'er/expand-region)
(define-key evil-normal-state-map (kbd "M-n") 'er/expand-region)
(define-key evil-visual-state-map (kbd "M-n") 'er/expand-region)
(define-key evil-emacs-state-map (kbd "M-n") 'er/expand-region)

;;; fastnav - modal ace-style navigation
(global-set-key (kbd "M-z") 'fastnav-zap-up-to-char-forward)
(global-set-key (kbd "M-Z") 'fastnav-zap-up-to-char-backward)

;; redundant with fastnav-sprint
(global-set-key (kbd "M-r") 'fastnav-replace-char-forward)
(global-set-key (kbd "M-R") 'fastnav-replace-char-backward)

(global-set-key (kbd "M-i") 'fastnav-insert-at-char-forward)
(global-set-key (kbd "M-I") 'fastnav-insert-at-char-backward)

(global-set-key (kbd "M-j") 'fastnav-execute-at-char-forward)
(global-set-key (kbd "M-J") 'fastnav-execute-at-char-backward)

(global-set-key (kbd "M-k") 'fastnav-delete-char-forward)
(global-set-key (kbd "M-K") 'fastnav-delete-char-backward)
(global-set-key (kbd "M-m") 'fastnav-mark-to-char-forward)
(global-set-key (kbd "M-M") 'fastnav-mark-to-char-backward)

(global-set-key (kbd "M-p") 'fastnav-sprint-forward)
(global-set-key (kbd "M-P") 'fastnav-sprint-backward)

;;; Smart forward - navigate by semantic sections
(autoload 'smart-up "smart-forward")
(autoload 'smart-down "smart-forward")
(autoload 'smart-forward "smart-forward")
(autoload 'smart-backward "smart-forward")

(put 'smart-up 'CUA 'move)
(put 'smart-down 'CUA 'move)
(put 'smart-forward 'CUA 'move)
(put 'smart-backward 'CUA 'move)

(global-set-key (kbd "M-<up>") 'smart-up)
(global-set-key (kbd "M-<down>") 'smart-down)
(global-set-key (kbd "M-<left>") 'smart-backward)
(global-set-key (kbd "M-<right>") 'smart-forward)

(eval-after-load 'smartrep
  '(progn
     (setq smartrep-mode-line-string-activated "[...]")
     (setq smartrep-mode-line-active-bg "#444444")))

(eval-after-load 'nav-flash
  '(progn
     (set-face-foreground 'nav-flash-face 'nil)
     (set-face-background 'nav-flash-face "grey40")
     (setq nav-flash-use-pulse 't)))

;; make newline super smart
(define-key evil-normal-state-map (kbd "RET") 'smart-newline)

(define-key evil-insert-state-map (kbd "C-m") 'newline)
(define-key evil-normal-state-map (kbd "C-m") 'newline)

;;; ====================================
;;; iflib - switch buffers alt-tab style
;;; ====================================
(autoload 'iflipb-next-buffer "iflipb")
(autoload 'iflipb-previous-buffer "iflipb")

(defvar iflipb-auto-off-timeout-sec 1)

(setq
  iflipb-ignore-buffers "^ "
  iflipb-wrap-around 't)

(eval-after-load 'iflipb
  '(progn
     (defvar iflipb-timeout 'nil)
     (defadvice iflipb-next-buffer (after flip-timeout activate)
       (progn
         (setq iflipb-timeout 'nil)
         (run-with-idle-timer iflipb-auto-off-timeout-sec 'nil
           '(lambda ()
              (setq iflipb-timeout 't)))))

     (defadvice iflipb-previous-buffer (after flip-timeout activate)
       (progn
         (setq iflipb-timeout 'nil)
         (run-with-idle-timer iflipb-auto-off-timeout-sec 'nil
           '(lambda ()
              (setq iflipb-timeout 't)))))

     (defun iflipb-first-iflipb-buffer-switch-command ()
       "Determines whether this is the first invocation of
  iflipb-next-buffer or iflipb-previous-buffer this round."
       (or (not (or (eq last-command 'iflipb-next-buffer)
                  (eq last-command 'iflipb-previous-buffer)))
         iflipb-timeout))))

(global-set-key (kbd "C-<tab>") 'iflipb-next-buffer)
(global-set-key (kbd "C-<backtab>") 'iflipb-previous-buffer)
(global-set-key (kbd "C-S-<iso-lefttab>") 'iflipb-previous-buffer)

;;; ===============================
;;; Do Re Mi - incremental commands
;;; ===============================

;;; =================================================
;;; God mode - a VI command mode using emacs bindings
;;; =================================================
(eval-after-load 'god-mode
  '(progn
     (key-chord-define evil-normal-state-map "JK" 'god-mode)
     (key-chord-define evil-insert-state-map "JK" 'god-mode)
     (key-chord-define evil-emacs-state-map "JK" 'god-local-mode)

     (add-hook 'god-mode-enabled-hook
       '(lambda ()
          (interactive)
          (evil-emacs-state)))

     (add-hook 'god-mode-disabled-hook
       '(lambda ()
          (interactive)
          (evil-insert-state)))))

;;; ====================================================
;;; dtrt-indent - guess indentation offset of alien code
;;; ====================================================
(require 'dtrt-indent)
(dtrt-indent-mode +1)

(defun eval-and-replace (value)
  "Evaluate the sexp at point and replace it with its value"
  (interactive (list (eval-last-sexp nil)))
  (kill-sexp -1)
  (insert (format "%S" value)))

(global-set-key (kbd "C-x C-e") 'eval-and-replace)

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

;;; ===========
;;; Python mode
;;; ===========
(add-hook 'inferior-python-mode-hook
  '(lambda ()
     (auto-complete-mode +1)
     (flycheck-mode +1)
     (jedi:ac-setup)))

(defun annotate-pdb ()
  (interactive)
  (highlight-lines-matching-regexp "pdb.set_trace()"))

(add-hook 'python-mode-hook 'annotate-pdb)
;;(require 'ipython)
(add-hook 'python-mode-hook
  '(lambda ()
     (require 'comint)
     (setq
       python-shell-interpreter "ipython"
       python-shell-interpreter-args ""
       python-shell-prompt-regexp "In \\[[0-9]+\\]: "
       python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
       python-shell-completion-setup-code
       "from IPython.core.completerlib import module_completion"
       python-shell-completion-module-string-code
       "';'.join(module_completion('''%s'''))\n"
       python-shell-completion-string-code
       "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")
     (defun my-python-compile ()
       "Use compile to run python programs"
       (interactive)
       (compile (concat "/bin/ipython " (buffer-file-name)) t))
     (setq compilation-scroll-output t)
     (setq auto-indent-eol-char ":")
     (global-set-key (kbd "<f5>") 'my-python-compile)

     ;; guess indentation level from first block
     (run-at-time "0.1 sec" nil 'python-indent-guess-indent-offset)
     (setq jedi:complete-on-dot t)
     (jedi:ac-setup)))

;;; ===============
;;; Javascript mode
;;; ===============
(add-hook 'js2-mode-hook 'ac-js2-mode)
;; (add-hook 'js3-mode-hook 'ac-js2-mode)
;; (autoload 'tern-mode "tern.el" nil t)
;; (add-hook 'js2-mode-hook 'tern-mode)
;; (add-hook 'js3-mode-hook 'tern-mode)
;; (add-to-list 'ac-modes 'js3-mode)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(setq tern-ac-on-dot t)
(setq ac-js2-evaluate-calls t)

(eval-after-load 'js2-mode
  '(progn
     (js2r-add-keybindings-with-prefix "C-c C-r")
     (setq
       js2-basic-offset 2
       js2-bounce-indent-p t)))

(eval-after-load 'tern
  '(progn
     (require 'tern-auto-complete)
     (tern-ac-setup)))

;;; ==========
;;; C(++) mode
;;; ==========
(setq
  c-basic-offset 4
  c-default-style
  '((java-mode . "java")
     (awk-mode . "awk")
     (other . "linux")))


(add-hook 'c++-mode-hook
  '(lambda ()
     (defun my-c++-compile ()
       (interactive)
       (let ((executable (file-name-sans-extension (buffer-file-name))))
         (compile (concat "g++ -o " executable " " (buffer-file-name) "; " executable))))

     (require 'disaster)
     (define-key c-mode-base-map (kbd "C-c d") 'disaster)

     (global-set-key (kbd "<f5>") 'my-c++-compile)
     (define-key evil-insert-state-map (kbd "<f5>") 'my-c++-compile)
     (define-key evil-normal-state-map (kbd "<f5>") 'my-c++-compile)
     (define-key evil-emacs-state-map (kbd "<f5>") 'my-c++-compile)))

(add-hook 'c-mode-hook
  '(lambda ()
     (defun my-c-compile ()
       (interactive)
       (let ((executable (file-name-sans-extension (buffer-file-name))))
         (compile (concat "gcc -o " executable " " (buffer-file-name) "; " executable))))

     (require 'disaster)
     (define-key c-mode-base-map (kbd "C-c d") 'disaster)

     (global-set-key (kbd "<f5>") 'my-c-compile)
     (define-key evil-insert-state-map (kbd "<f5>") 'my-c-compile)
     (define-key evil-normal-state-map (kbd "<f5>") 'my-c-compile)
     (define-key evil-emacs-state-map (kbd "<f5>") 'my-c-compile)))

;;; ============
;;; Haskell mode
;;; ============

(add-hook 'haskell-mode-hook
  '(lambda ()
     (turn-on-haskell-decl-scan)
     (turn-on-haskell-indentation)
     (turn-on-haskell-doc)))

;;; ========
;;; CSS mode
;;; ========
(add-hook 'css-mode-hook 'rainbow-mode)
(sp-local-pair 'css-mode ":" ";")
(add-hook 'scss-mode-hook 'auto-complete-mode)
(add-hook 'scss-mode-hook 'flyspell-prog-mode)

;;; ===========
;;; Cython mode
;;; ===========
;; (require 'cython-mode)
(autoload 'cython-mode "cython-mode")
(autoload 'sage-mode "sage-mode")
(add-hook 'sage-mode-hook 'jedi:setup)
(add-to-list 'auto-mode-alist '("\\.pyx\\'" . cython-mode))
(add-to-list 'auto-mode-alist '("\\.pxd\\'" . cython-mode))
(add-to-list 'auto-mode-alist '("\\.pxi\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.sage\\'" . sage-mode))
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))
(add-hook 'sh-mode-hook
  (lambda ()
    (if (string-match "\\.zsh$" buffer-file-name)
      (sh-set-shell "zsh"))))

;;; ==============
;;; *scratch* mode
;;; ==============
(define-key lisp-interaction-mode-map (kbd "C-x C-s") 'eval-buffer)

;;; ====================
;;; ERC -- IRC for Emacs
;;; ====================
(setq erc-autojoin-channels-alist
  '(("esper.net" "#aops")))

(setq erc-track-enable-keybindings 't)
(setq erc-default-server "irc.esper.net")
(setq erc-default-port "5555")

(eval-after-load 'erc
  '(progn
     (add-to-list 'erc-modules 'spelling)
     (add-to-list 'erc-modules 'track)
     (add-to-list 'erc-modules 'match)
     (add-to-list 'erc-modules 'ring)
     (add-to-list 'erc-modules 'button)
     (add-to-list 'erc-modules 'notify)
     (add-to-list 'erc-modules 'notifications)
     (add-to-list 'erc-modules 'completion)
     (evil-define-key 'emacs erc-mode-map (kbd "M-n") 'erc-previous-command)
     (erc-update-modules)))

(defun aops-irc ()
  "Connect to the AoPS IRC channel"
  (interactive)
  (erc :server "irc.esper.net"
    :port 5555
    :nick "PythonNut"
    :password nil))

;;; ========
;;; Org mode
;;; ========
(defun yas/org-very-safe-expand ()
  (let ((yas/fallback-behavior 'return-nil)) (yas/expand)))

(add-hook 'org-mode-hook
  '(lambda ()
     (yas/minor-mode -1)
     (ws-butler-mode +1)
     ;; try and fix yasnippet
     (global-set-key (kbd "<f7>") 'org-cycle)

     (make-variable-buffer-local 'yas/trigger-key)
     (setq yas/trigger-key "<tab>")
     (add-to-list 'org-tab-first-hook 'yas/org-very-safe-expand)
     (define-key yas/keymap "<tab>" 'yas/next-field)
     ;; Make windmove work in org-mode:
     (add-hook 'org-shiftup-final-hook 'windmove-up)
     (add-hook 'org-shiftleft-final-hook 'windmove-left)
     (add-hook 'org-shiftdown-final-hook 'windmove-down)
     (add-hook 'org-shiftright-final-hook 'windmove-right)))

(autoload 'bongo "bongo")
(autoload 'asy-mode "asy-mode")
;;; =========
;;; Customize
;;; =========
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cua-rectangle ((t (:inherit region :background "#93a1a1" :foreground "#002b36"))))
 '(flycheck-error ((t (:underline "Red1"))))
 '(flycheck-info ((t (:underline "#353"))))
 '(flycheck-warning ((t (:underline "gray35"))))
 '(flyspell-duplicate ((t (:underline "#7e6d3a"))))
 '(flyspell-incorrect ((t (:underline "#772a2f"))))
 '(mode-line ((t (:background "grey70" :foreground "black" :box nil :underline nil))))
 '(mode-line-emphasis ((t (:background "gray70" :weight bold))))
 '(mode-line-inactive ((t (:inherit mode-line :background "#555753" :foreground "#eeeeec" :box nil :weight light)))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(adaptive-wrap-extra-indent 2)
 '(ansi-color-names-vector ["#002b36" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#839496"])
 '(compilation-message-face (quote default))
 '(custom-safe-themes (quote ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(ergoemacs-mode-used "5.7.5")
 '(ergoemacs-theme "5.7.5")
 '(fci-rule-color "#073642")
 '(flycheck-indication-mode nil)
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-tail-colors (quote (("#073642" . 0) ("#546E00" . 20) ("#00736F" . 30) ("#00629D" . 50) ("#7B6000" . 60) ("#8B2C02" . 70) ("#93115C" . 85) ("#073642" . 100))))
 '(magit-diff-use-overlays nil)
 '(magit-use-overlays nil)
 '(projectile-globally-ignored-directories (quote (".idea" ".eunit" ".git" ".hg" ".fslckout" ".bzr" "_darcs" "venv" "build" ".svn")))
 '(projectile-project-root-files (quote (".projectile" ".git" ".hg" ".fslckout" ".bzr" "_darcs" "rebar.config" "project.clj" "pom.xml" "build.sbt" "build.gradle" "Gemfile" "Makefile" ".svn")))
 '(recentf-exclude (quote ("\\\\.emacs.d/elpa\\\\")) t)
 '(recentf-max-menu-items 20)
 '(recentf-max-saved-items 100)
 '(send-mail-function (quote smtpmail-send-it))
 '(syslog-debug-face (quote ((t :background unspecified :foreground "#2aa198" :weight bold))))
 '(syslog-error-face (quote ((t :background unspecified :foreground "#dc322f" :weight bold))))
 '(syslog-hour-face (quote ((t :background unspecified :foreground "#859900"))))
 '(syslog-info-face (quote ((t :background unspecified :foreground "#268bd2" :weight bold))))
 '(syslog-ip-face (quote ((t :background unspecified :foreground "#b58900"))))
 '(syslog-su-face (quote ((t :background unspecified :foreground "#d33682"))))
 '(syslog-warn-face (quote ((t :background unspecified :foreground "#cb4b16" :weight bold))))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map (quote ((20 . "#dc322f") (40 . "#CF4F1F") (60 . "#C26C0F") (80 . "#b58900") (100 . "#AB8C00") (120 . "#A18F00") (140 . "#989200") (160 . "#8E9500") (180 . "#859900") (200 . "#729A1E") (220 . "#609C3C") (240 . "#4E9D5B") (260 . "#3C9F79") (280 . "#2aa198") (300 . "#299BA6") (320 . "#2896B5") (340 . "#2790C3") (360 . "#268bd2"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list (quote (unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83"))))

(require 'solarized)
(load-theme 'solarized-dark)

(scroll-bar-mode -1)
(set-face-attribute 'show-paren-match-face nil :weight 'extra-bold)
(set-face-background 'fringe "#022F3A")
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
(put 'scroll-left 'disabled nil)
(put 'downcase-region 'disabled nil)

;; (fringe-mode '(1 . 1))
(put 'narrow-to-region 'disabled nil)
(when (daemonp)
  (message "running daemon startup")
  (semantic-mode +1)
  (message "loading icicles")
  (require 'icicles)
  (message "loading projectile")
  (require 'projectile)
  (message "loading helm-buffers")
  (require 'helm-buffers)
  (message "loading helm-ring")
  (require 'helm-ring)
  (message "loading helm-misc")
  (require 'helm-misc)
  (message "loading semantic")
  (require 'semantic)
  (message "loading magit")
  (require 'magit)
  (message "loading smex")
  (require 'smex)
  (smex-initialize)
  (message "loading god-mode")
  (require 'god-mode)
  (message "loading framemove")
  (require 'framemove)
  (message "loading undo-tree")
  (require 'undo-tree)
  (message "loading region-bindings-mode")
  (require 'region-bindings-mode)
  (message "loading flycheck")
  (require 'flycheck)
  (message "loading flyspell")
  (require 'flyspell)
  (message "loading phi-search")
  (require 'phi-search)
  (message "loading better-registers")
  (require 'better-registers)
  (message "loading ace-jump-mode")
  (require 'ace-jump-mode)
  (message "loading back-button")
  (require 'back-button))

;;; ==========================
;;; Dem preprocessor languages
;;; ==========================
(add-to-list 'auto-mode-alist '("\\.less\\'" . less-css-mode))
(autoload 'livescript-mode
  "~/.emacs.d/personal/livescript-mode.el"
  "A major mode for LiveScript" t)

;;; ===================================
;;; Ediff emacs diff for git/svn/hg/etc
;;; ===================================
;; side-by-side diffs
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; preserve origional window config
(add-hook 'ediff-load-hook
  (lambda ()
    (add-hook 'ediff-before-setup-hook
      (lambda ()
        ;; (set-face-foreground 'ediff-current-diff-face-A "#6c71c4")
        ;; (set-face-foreground 'ediff-current-diff-face-B "#cb4b16")
        ;; (set-face-foreground 'ediff-current-diff-face-C "#859900")
        
        (setq ediff-saved-window-configuration (current-window-configuration))))
    
    (let ((restore-window-configuration
            (lambda ()
              (set-window-configuration ediff-saved-window-configuration))))
      (add-hook 'ediff-quit-hook restore-window-configuration 'append)
      (add-hook 'ediff-suspend-hook restore-window-configuration 'append))))

;;; ==========
;;; Julia mode
;;; ==========
(require 'ess-site)
(add-hook 'julia-mode 'inferior-ess-mode)

(add-hook 'julia-mode-hook
  '(lambda ()
     (require 'comint)

     (defun my-julia-compile ()
       "Use compile to run python programs"
       (interactive)
       (compile (concat "julia -L " (buffer-file-name)) t))

     (setq compilation-scroll-output t)
     (global-set-key (kbd "<f5>") 'my-julia-compile)
     (define-key evil-insert-state-map (kbd "<f5>") 'my-julia-compile)
     (define-key evil-normal-state-map (kbd "<f5>") 'my-julia-compile)
     (define-key evil-emacs-state-map (kbd "<f5>") 'my-julia-compile)))

(add-hook 'comint-exec-hook
  (lambda () (set-process-query-on-exit-flag (get-buffer-process (current-buffer)) nil)))
