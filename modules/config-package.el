(require 'package)

(eval-when-compile
  (with-demoted-errors
    (require 'cl-lib)
    (require 'idle-require)
    (require 'diminish)))

;; Package archives
(setq
  package-enable-at-startup nil
  package-archives
  '(("gnu" . "http://elpa.gnu.org/packages/")
     ("elpa" . "http://tromey.com/elpa/")
     ("melpa" . "http://melpa.org/packages/")))

(package-initialize)

;; Guarantee all packages are installed on start
(defun has-package-not-installed (package-list)
  (cl-loop for p in package-list
    when (not (package-installed-p p)) do (cl-return t)
    finally (cl-return nil)))

(defun ensure-packages-are-installed (package-list)
  (interactive)
  (when (has-package-not-installed package-list)
    (package-refresh-contents)
    (dolist (p package-list)
      (when (not (package-installed-p p))
	(package-install p)))
    (package-initialize)))

(ensure-packages-are-installed
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
     markdown-mode
     matlab-mode
     php-mode
     pkgbuild-mode
     sass-mode
     scss-mode
     web-mode
     yaml-mode

     adaptive-wrap
     aggressive-indent
     anaconda-mode
     auto-compile
     auto-indent-mode
     avy
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
     linum-relative
     load-dir
     ;; magit
     magit-filenotify
     multiple-cursors
     rainbow-delimiters
     smartparens
     smooth-scrolling
     solarized-theme
     traad
     volatile-highlights
     whole-line-or-region
     ws-butler
     xclip))

(add-to-list 'load-path (concat user-emacs-directory "personal/"))

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
       company)))

(if (not (daemonp))
  (add-hook 'emacs-startup-hook
    (lambda ()
      (run-with-idle-timer 0.5 nil
        (lambda ()
          (defadvice idle-require-load-next
            (around quiet activate preactivate compile)
            (ad-enable-advice 'load 'before 'quiet-loading)
            (ad-activate 'load)
            (with-demoted-errors
              (cl-letf (((symbol-function 'message) #'format))
                ad-do-it))
            (ad-disable-advice 'load 'before 'quiet-loading)
            (ad-activate 'load)
            (when (null idle-require-symbols)
              (message "")))

          (idle-require-mode +1)))))

  (message "loading symbols for server")
  (idle-require-mode +1)
  (dolist (sym idle-require-symbols)
    (let ((name (symbol-name sym)))
      (unless (string= name "idle-require")
        (message (format "Loading %s..." name))
        (with-demoted-errors (require sym nil t))))))

(provide 'config-package)
