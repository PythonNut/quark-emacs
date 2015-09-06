;; -*- lexical-binding: t -*-

(require 'package)

(eval-when-compile
  (with-demoted-errors "Load error: %s"
    (require 'cl-lib)))

;; Package archives
(setq
 package-enable-at-startup nil
 package-archives
 '(("gnu" . "http://elpa.gnu.org/packages/")
   ("melpa" . "http://melpa.org/packages/")))

(package-initialize)

;; Guarantee all packages are installed on start
(defun my/has-package-not-installed (package-list)
  (cl-loop for p in package-list
           when (not (package-installed-p p)) do (cl-return t)
           finally (cl-return nil)))

(defun my/ensure-packages-are-installed (package-list)
  (interactive)
  (when (my/has-package-not-installed package-list)
    (package-refresh-contents)
    (dolist (p package-list)
      (when (not (package-installed-p p))
        (package-install p)))
    (package-initialize)))

(defvar my/required-packages
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
    evil-quickscope

    ;; major modes
    coffee-mode
    cython-mode
    gitattributes-mode
    gitconfig-mode
    gitignore-mode
    ;; js2-mode
    js2-refactor
    json-mode
    markdown-mode
    php-mode
    web-mode
    yaml-mode

    adaptive-wrap
    aggressive-indent
    anaconda-mode
    auto-compile
    auto-highlight-symbol
    auto-indent-mode
    avy
    bracketed-paste
    company
    company-anaconda
    diff-hl
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
    hydra
    icicles
    iflipb
    impatient-mode
    idle-require
    key-chord
    linum-relative
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

(my/ensure-packages-are-installed my/required-packages)

(eval-when-compile
  (with-demoted-errors "Load error: %s"
    (require 'idle-require)))
(add-to-list 'load-path (expand-file-name "personal/" user-emacs-directory))

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

(defun nadvice/idle-require-quiet (old-fun &rest args)
  (advice-add 'load :filter-args #'nadvice/load-quiet)
  (with-demoted-errors
      (cl-letf (((symbol-function 'message) #'format))
        (apply old-fun args)))
  (advice-remove #'load #'nadvice/load-quiet)
  (when (null idle-require-symbols)
    (message "")))

(with-eval-after-load 'idle-require
  (advice-add 'idle-require-load-next :around #'nadvice/idle-require-quiet))

(if (not (daemonp))
    (add-hook 'emacs-startup-hook
              (lambda ()
                (run-with-idle-timer 0.5 nil
                                     (lambda ()
                                       (idle-require-mode +1)))))

  (message "loading symbols for server")
  (idle-require-mode +1)
  (dolist (sym idle-require-symbols)
    (let ((name (symbol-name sym)))
      (unless (string= name "idle-require")
        (message (format "Loading %s..." name))
        (with-demoted-errors "Idle load error: %s"
          (require sym nil t))))))

(provide 'config-package)
