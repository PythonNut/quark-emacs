(require 'package)

(eval-when-compile (require 'cl-lib))

;; Package archives
(setq
  package-enable-at-startup nil
  package-archives
  '(("gnu" . "http://elpa.gnu.org/packages/")
     ("elpa" . "http://tromey.com/elpa/")
     ("melpa" . "http://melpa.org/packages/")))

(package-initialize)

;; Guarantee all packages are installed on start
(defvar packages-list
  '(
     ;; ido based packages
     flx-ido
     ido-ubiquitous
     ido-vertical-mode
     smex

     ;; evil based modes
     ;; evil
     evil-args
     evil-easymotion
     evil-exchange
     evil-indent-textobject
     evil-matchit
     evil-nerd-commenter
     evil-org
     evil-snipe
     evil-surround
     evil-visualstar

     ;; major modes
     coffee-mode
     cython-mode
     gitattributes-mode
     gitconfig-mode
     gitignore-mode
     haskell-mode
     ;; js2-mode
     js2-refactor
     json-mode
     julia-mode
     less-css-mode
     livescript-mode
     markdown-mode
     matlab-mode
     php-mode
     pkgbuild-mode
     sass-mode
     scss-mode
     web-mode
     yaml-mode

     ;; ace-jump-mode
     adaptive-wrap
     aggressive-indent
     anaconda-mode
     auto-compile
     ;; auto-complete
     auto-indent-mode
     bracketed-paste
     company
     company-anaconda
     diff-hl
     dired-avfs
     dired-filter
     dired-subtree
     diminish
     dtrt-indent
     easy-kill
     expand-region
     flx-isearch
     flycheck
     framemove
     helm-ag
     helm-git-grep
     helm-projectile
     hexrgb
     hydra
     icicles
     idomenu
     iflipb
     impatient-mode
     idle-require
     key-chord
     lacarte
     linum-relative
     load-dir
     ;; magit
     magit-filenotify
     multiple-cursors
     ;; noflet
     popwin
     psvn
     rainbow-delimiters
     smartparens
     smooth-scrolling
     solarized-theme
     traad
     whole-line-or-region
     ws-butler
     xclip)
  "List of packages needs to be installed at launch")

(defun has-package-not-installed ()
  (cl-loop for p in packages-list
    when (not (package-installed-p p)) do (cl-return t)
    finally (cl-return nil)))

(defun install-all-packages ()
  (interactive)
  (when (has-package-not-installed)
    ;; Check for new packages (package versions)
    (message "%s" "Get latest versions of all packages...")
    (package-refresh-contents)
    (message "%s" " done.")
    ;; Install the missing packages
    (dolist (p packages-list)
      (when (not (package-installed-p p))
	(package-install p)))
    (package-initialize)))

(install-all-packages)

(add-to-list 'load-path (concat user-emacs-directory "personal/"))

(eval-when-compile
  (with-demoted-errors
    (require 'idle-require)
    (require 'diminish)))

(with-eval-after-load 'idle-require
  (add-hook 'idle-require-mode-hook
    (lambda ()
      (diminish 'idle-require-mode (if (display-graphic-p) " ⋯" " IR"))))

  (setq
    idle-require-idle-delay 1
    idle-require-load-break 0
    idle-require-symbols
    '(
       helm-files
       helm-ring
       helm-projectile
       helm-semantic
       helm-ag
       yasnippet
       company

       magit
       magit-filenotify
       multiple-cursors
       easy-kill
       flx-isearch
       ace-jump-mode
       whole-line-or-region
       smex
       psvn

       icicles)))

(if (daemonp)
  (progn
    (require 'idle-require)
    (message "loading symbols for server")
    (idle-require-mode +1)
    (dolist (sym idle-require-symbols)
      (let ((name (symbol-name sym)))
        (unless (string= name "idle-require")
          (message (format "Loading %s..." name))
          (with-demoted-errors (require sym nil t))))))

  (add-hook 'emacs-startup-hook
    (lambda ()
      (run-with-idle-timer 0.5 nil
        (lambda ()
          (defadvice idle-require-load-next
            (around quiet activate preactivate compile)
            (ad-enable-advice 'load 'before 'quiet-loading)
            (ad-activate 'load)
            (cl-letf (((symbol-function 'message) #'format))
              ad-do-it)
            (ad-disable-advice 'load 'before 'quiet-loading)
            (ad-activate 'load)
            (when (null idle-require-symbols)
              (message "")))

          (idle-require-mode +1))))))

(provide 'config-package)
