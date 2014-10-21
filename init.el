(setq
  split-width-threshold 0
  split-height-threshold 0
  inhibit-startup-screen t
  ring-bell-function 'ignore
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
  jit-lock-stealth-time 0.2
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

(auto-compression-mode 1)
(blink-cursor-mode 0)
(delete-selection-mode 1)

(setq epa-file-name-regexp "\\.\\(gpg\\|asc\\)$")
(epa-file-name-regexp-update)
(setenv "GPG_AGENT_INFO" nil)

(global-set-key (kbd "<f6>") 'multi-term)

;;; ===========
;;; Theme setup
;;; ===========

(defalias 'yes-or-no-p 'y-or-n-p)
(add-hook 'emacs-startup-hook
  '(lambda ()
     ;; bold keywords please
     (set-face-attribute 'font-lock-keyword-face 'nil :weight 'extra-bold)
     (set-face-attribute 'font-lock-comment-face 'nil :slant 'italic)))

(defun raise-minor-mode-map-alist (mode-symbol)
  "Raise `minor-mode-map-alist' priority of MODE-SYMBOL."
  (let ((x (assq mode-symbol minor-mode-map-alist)))
    (and x (setq minor-mode-map-alist (cons x (delq x minor-mode-map-alist))))))

(defun lower-minor-mode-map-alist (mode-symbol)
  "Lower `minor-mode-map-alist' priority of MODE-SYMBOL."
  (let ((rel (assq mode-symbol minor-mode-map-alist)))
    (setq minor-mode-map-alist (append (delete rel minor-mode-map-alist) (list rel)))))

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
(setq-default major-mode 'text-mode)
(setq-default indent-tabs-mode nil)

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
  `((".*" . ,backup-location)))

(setq auto-save-file-name-transforms
  `((".*" ,autosave-location t)))

(global-set-key (kbd "C-c B") 'backup-walker-start)

(add-hook 'first-change-hook (lambda () (require 'backups-mode)))
(eval-after-load 'backups-mode '(backups-mode-start))

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

;; (defun elscreen-frame-title-update ()
;;   (when (elscreen-screen-modified-p 'elscreen-frame-title-update)
;;     (let* ((screen-list (sort (elscreen-get-screen-list) '<))
;;             (screen-to-name-alist (elscreen-get-screen-to-name-alist))
;;             (title (mapconcat
;;                      (lambda (screen)
;;                        (format "%s"
;;                          (if (string= (elscreen-status-label screen) "+")
;;                            (concat "[" (get-alist screen screen-to-name-alist) "]")
;;                            (get-alist screen screen-to-name-alist))))
;;                      screen-list " - ")))
;;       (if (fboundp 'set-frame-name)
;;         (set-frame-name title)
;;         (setq frame-title-format title)))))

(eval-after-load "elscreen"
  '(progn
     (elscreen-start)
     ;; (add-hook 'elscreen-screen-update-hook 'elscreen-frame-title-update)
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

(autoload 'elscreen-next     "elscreen")
(autoload 'elscreen-previous "elscreen")

;; Alt+(PgUp|PgDown) switches between elscreens
(global-set-key (kbd "M-<prior>") 'elscreen-previous)
(global-set-key (kbd "M-<next>") 'elscreen-next)

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
      (elscreen-previous)
      ad-do-it)
    ad-do-it))

(defadvice windmove-right (around try-elscreen activate)
  (if (and
        (= (length (window-list)) 1)
        (= (length (frame-list)) 1))
    (if (and (elscreen-get-screen-list)
          (> (length (elscreen-get-screen-list)) 1))
      (elscreen-next)
      (if (y-or-n-p-with-timeout "Start elscreen? " 2 nil)
        (elscreen-next)
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

(add-hook 'after-change-major-mode-hook
  '(lambda ()
     (clean-mode-line)
     (run-at-time 0.3 nil 'clean-mode-line)))

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
     (cua-selection-mode -1)))

(add-hook 'minibuffer-exit-hook
  '(lambda ()
     (cua-selection-mode +1)))

(strokes-mode +1)
(setq strokes-file "~/.emacs.d/.strokes")
(strokes-load-user-strokes)
(global-set-key (kbd "<down-mouse-3>") 'strokes-do-stroke)

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

(setq
  mouse-wheel-scroll-amount '(3 ((shift) . 1))
  redisplay-dont-pause t
  mouse-wheel-progressive-speed nil
  mouse-wheel-follow-mouse t
  smooth-scroll-margin 5
  scroll-margin 5
  scroll-conservatively 0
  scroll-up-aggressively 0.01
  scroll-down-aggressively 0.01)

(setq-default
  scroll-up-aggressively 0.01
  scroll-down-aggressively 0.01)

;; scroll-margin is reset at start for some reason
(add-hook 'emacs-startup-hook
  '(lambda ()
     (setq scroll-margin 5)))

;; Package archives
(setq package-archives
  '(("gnu" . "http://elpa.gnu.org/packages/")
     ;; ("elpa" . "http://tromey.com/elpa/")
     ;; ("marmalade" . "http://marmalade-repo.org/packages/")
     ("melpa" . "http://melpa.milkbox.net/packages/")))

;;; ================
;;; better registers
;;; ================
(autoload 'jump-to-register "better-registers")
(define-key evil-insert-state-map (kbd "C-r") 'jump-to-register)



;;; ==============================================
;;; The Minibuffer - command and minibuf goes here
;;; ==============================================
;;; SMEX - interactive command interface
(setq smex-save-file "~/.emacs.d/smex-items")
(setq smex-history-length 1000)
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

;; don't let the cursor go into minibuffer prompt
(setq minibuffer-prompt-properties
  '(read-only t
     point-entered
     minibuffer-avoid-prompt
     face
     minibuffer-prompt))

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
(eval-after-load 'semantic
  '(progn
    (global-semanticdb-minor-mode +1)
    (global-semantic-idle-scheduler-mode +1)
    (global-semantic-idle-summary-mode +1)))

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

(global-set-key (kbd "<mouse-6>") 'evil-scroll-page-down)
(global-set-key (kbd "<mouse-7>") 'evil-scroll-page-up)

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
(eval-after-load 'emmet-mode
  '(progn
     (defun try-expand-emmet (arg)
       (emmet-expand-yas))
     (add-to-list 'hippie-expand-try-functions-list
       'try-expand-emmet)
     (emmet-mode +1)))

;; Auto-start on any markup modes
(add-hook 'sgml-mode-hook '(lambda () (require 'emmet-mode)))
(add-hook 'css-mode-hook  '(lambda () (require 'emmet-mode)))

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
    (append
      '(ac-source-math-unicode
         ac-source-math-latex
         ac-source-latex-commands)
      ac-sources))
  (ac-auctex-setup))

(add-hook 'LaTeX-mode-hook 'ac-latex-mode-setup)


;;; ====================================
;;; flycheck - real-time syntax checking
;;; ====================================

(add-hook 'prog-mode-hook '(lambda () (require 'flycheck)))

(defun my-display-error-messages-condensed (errors)
  (-when-let (messages (-keep #'flycheck-error-message errors))
    (when (flycheck-may-use-echo-area-p)
      (display-message-or-buffer (s-join "\n" messages)
        flycheck-error-message-buffer))))

(eval-after-load 'flycheck
  '(progn
     (setq flycheck-display-errors-function #'my-display-error-messages-condensed)
     (defun flycheck-mode-line-status-text (&optional status)
       (let ((text (pcase (or status flycheck-last-status-change)
                     (`not-checked "")
                     (`no-checker "-")
                     (`running "*")
                     (`errored "!")
                     (`finished
                       (if flycheck-current-errors
                         (let ((error-counts (flycheck-count-errors
                                               flycheck-current-errors)))
                           (format ":%s/%s"
                             (or (cdr (assq 'error error-counts)) 0)
                             (or (cdr (assq 'warning error-counts)) 0)))
                         ""))
                     (`interrupted "-")
                     (`suspicious "?"))))
         (concat " ✓" text)))
     ;; (add-hook 'flycheck-mode-hook #'flycheck-haskell-setup)
     (global-flycheck-mode +1)))

;;; ================================
;;; Auto-complete - self explanatory
;;; ================================
(require 'auto-complete-config)

(global-auto-complete-mode)
(add-hook 'text-mode-hook
  '(lambda ()
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
    (icicle-complete-keys)))

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

        (if (eq major-mode 'org-mode)
          (progn
            (setq this-command 'org-cycle)
            (call-interactively 'org-cycle))
          (progn
            (setq this-command 'indent-for-tab-command)
            (indent-for-tab-command arg)
            (message "indent")))))))

(define-key evil-insert-state-map (kbd "<tab>") 'indent-or-complete)
(define-key evil-insert-state-map (kbd "<backtab>") 'my-unindent)

;;; =============================
;;; Magit - fast, interactive git
;;; =============================
(global-set-key (kbd "C-c C-m") 'projectile-vc)

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

;; disable regular key chords by switching input methods
(add-hook 'magit-status-mode-hook
  '(lambda ()
     (set-input-method "TeX")))

;; and psvn for svn not-so-awesomeness
(autoload 'svn-status "psvn")

(eval-after-load 'psvn
  '(progn
     (setq svn-status-verbose nil)))

(global-set-key (kbd "C-c C-c s") 'svn-status)

;;; =============================
;;; Dired, the emacs file manager
;;; =============================
(eval-after-load 'dired+
  '(progn
     (define-key dired-mode-map (kbd "<down-mouse-3>") 'strokes-do-stroke)
     (toggle-diredp-find-file-reuse-dir 1)))

(add-hook 'dired-load-hook
  '(lambda ()
     (require 'dired-x)
     (require 'dired+)))

(defun dired-here ()
  (interactive)
  (dired (file-name-directory (buffer-file-name))))

;;; ==============================
;;; Ido - interactively do things
;;; ==============================
(ido-mode +1)
(require 'flx-ido)
(require 'ido-ubiquitous)
(require 'ido-vertical-mode)
(ido-ubiquitous +1)
(flx-ido-mode +1)
(ido-vertical-mode +1)

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

;;; ==========
;;; Projectile
;;; ==========
;; (require 'projectile)
(require 'projectile-autoloads)
(projectile-global-mode +1)

(eval-after-load 'projectile
  '(progn
     (projectile-global-mode +1)))

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
(evil-define-key 'normal prog-mode-map (kbd "RET") 'smart-newline)

(define-key evil-insert-state-map (kbd "C-m") 'newline)
(define-key evil-normal-state-map (kbd "C-m") 'newline)

;;; ====================================
;;; iflib - switch buffers alt-tab style
;;; ====================================
(autoload 'iflipb-next-buffer "iflipb")
(autoload 'iflipb-previous-buffer "iflipb")

(defvar iflipb-auto-off-timeout-sec 3)

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
(eval-after-load 'org
  '(progn
     (setq
       org-return-follows-link t
       org-return-indent t
       org-completion-use-ido t
       org-descriptive-links t)

     ;; Make windmove work in org-mode:
     (add-hook 'org-shiftup-final-hook 'windmove-up)
     (add-hook 'org-shiftleft-final-hook 'windmove-left)
     (add-hook 'org-shiftdown-final-hook 'windmove-down)
     (add-hook 'org-shiftright-final-hook 'windmove-right)

     (add-to-list 'org-modules 'org-mouse)))

(add-hook 'org-mode-hook
  '(lambda ()
     (yas/minor-mode -1)))

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
;; (require 'ess-site)
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
