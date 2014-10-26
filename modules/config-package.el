(require 'package)
(eval-when-compile (require 'cl))
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
  '(icicles
     solarized-theme

     ;; ido based packages
     smex
     flx-ido
     ido-vertical-mode
     ido-ubiquitous

     ;; evil based modes
     evil
     evil-leader
     evil-nerd-commenter
     evil-indent-textobject
     evil-surround
     evil-org
     evil-exchange
     evil-terminal-cursor-changer
     evil-visualstar

     ;; major modes
     idris-mode
     haskell-mode
     livescript-mode
     scss-mode
     sass-mode
     less-css-mode
     markdown-mode
     js2-mode
     cython-mode
     coffee-mode
     dart-mode
     julia-mode
     matlab-mode
     web-mode

     framemove
     aggressive-indent
     auto-async-byte-compile
     gitattributes-mode
     gitconfig-mode
     gitignore-mode
     idle-require
     multiple-cursors
     ace-jump-mode
     noflet
     hexrgb
     rainbow-delimiters
     smartparens
     magit
     magit-filenotify
     psvn
     linum-relative
     ws-butler
     dtrt-indent
     adaptive-wrap
     xclip
     easy-kill
     load-dir
     auto-indent-mode
     lacarte
     smartrep
     whole-line-or-region
     wide-n
     key-chord
     auto-complete
     flycheck
     flyspell
     helm-projectile
     iflipb)
  "List of packages needs to be installed at launch")

(defun has-package-not-installed ()
  (loop for p in packages-list
    when (not (package-installed-p p)) do (return t)
    finally (return nil)))

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

(add-to-list 'load-path "~/.emacs.d/personal/")
