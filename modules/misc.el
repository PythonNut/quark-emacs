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

(defun enable-debugging ()
  (interactive)
  (setq debug-on-error t))

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

;;; ============
;;; Emacs tables
;;; ============
(setq max-lisp-eval-depth 40000)
(setq max-specpdl-size 10000)






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



;;; ==========
;;; C(++) mode
;;; ==========
(setq
  c-basic-offset 4
  c-default-style
  '((java-mode . "java")
     (awk-mode . "awk")
     (other . "linux")))

(add-hook 'sh-mode-hook
  (lambda ()
    (if (string-match "\\.zsh$" buffer-file-name)
      (sh-set-shell "zsh"))))

;;; ==============
;;; *scratch* mode
;;; ==============
(define-key lisp-interaction-mode-map (kbd "C-x C-s") 'eval-buffer)

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
(put 'narrow-to-region 'disabled nil)



