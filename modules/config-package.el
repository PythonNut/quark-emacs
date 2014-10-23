(require 'package)

;; Package archives
(setq package-archives
  '(("gnu" . "http://elpa.gnu.org/packages/")
     ("elpa" . "http://tromey.com/elpa/")
     ("melpa" . "http://melpa.milkbox.net/packages/")))

(package-initialize)
(provide 'config-package)

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
	(package-install p)))))

(install-all-packages)
