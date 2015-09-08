;; -*- lexical-binding: t -*-

(require 'package)

(eval-when-compile
  (with-demoted-errors "Load error: %s"
    (require 'cl-lib)))

;; Package archives
(setq package-enable-at-startup nil
      package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")))

(package-initialize)

;; Guarantee all packages are installed on start
(defun my/has-package-not-installed (package-list)
  (catch 'package-return
    (dolist (p package-list)
      (unless (package-installed-p p)
        (throw 'package-return t)))
    (throw 'package-return nil)))

(defun my/ensure-packages-are-installed (package-list)
  (interactive)
  (save-window-excursion
    (when (my/has-package-not-installed package-list)
      (package-refresh-contents)
      (dolist (p package-list)
        (when (not (package-installed-p p))
          (package-install p)))
      (byte-recompile-config)
      (package-initialize))))

(defvar my/required-packages
  '(;; ido based packages
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
              (diminish 'idle-require-mode)))

  (setq idle-require-idle-delay 3
        idle-require-load-break 1
        idle-require-symbols
        '(magit
          hydra
          evil-snipe
          multiple-cursors
          avy)))

(with-eval-after-load 'idle-require
  (defun nadvice/idle-require-quiet (old-fun &rest args)
    (advice-add 'load :filter-args #'nadvice/load-quiet)
    (with-demoted-errors "Idle require error: %s"
      (cl-letf (((symbol-function 'message) #'format))
        (apply old-fun args)))
    (advice-remove #'load #'nadvice/load-quiet))

  (advice-add 'idle-require-load-next :around #'nadvice/idle-require-quiet))

(add-hook 'emacs-startup-hook
          (lambda ()
            (run-with-idle-timer 0.1 nil
                                 (lambda ()
                                   (mapc #'require
                                         '(helm-files
                                           helm-ring
                                           helm-projectile
                                           helm-semantic))
                                   (idle-require-mode +1)))))

(defun package-upgrade-all (&optional automatic)
  "Upgrade all packages automatically without showing *Packages* buffer."
  (interactive)
  (package-refresh-contents)
  (let (upgrades)
    (cl-flet ((get-version (name where)
                           (let ((pkg (cadr (assq name where))))
                             (when pkg
                               (package-desc-version pkg)))))
      (dolist (package (mapcar #'car package-alist))
        (let ((in-archive (get-version package package-archive-contents)))
          (when (and in-archive
                     (version-list-< (get-version package package-alist)
                                     in-archive))
            (push (cadr (assq package package-archive-contents))
                  upgrades)))))
    (if upgrades
        (when (or automatic
                  (yes-or-no-p
                   (message "Upgrade %d package%s (%s)? "
                            (length upgrades)
                            (if (= (length upgrades) 1) "" "s")
                            (mapconcat #'package-desc-full-name upgrades ", "))))
          (save-window-excursion
            (dolist (package-desc upgrades)
              (let ((old-package (cadr (assq (package-desc-name package-desc)
                                             package-alist))))
                (package-install package-desc)
                (package-delete  old-package)))))
      (message "All packages are up to date"))))

(provide 'config-package)

