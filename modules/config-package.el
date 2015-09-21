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

(defvar my/package-cached-autoloads nil)
(defvar my/package-cache-last-build-time nil)

(defvar package-autoload-file (expand-file-name "data/autoload-cache.el"
                                                user-emacs-directory))

(defun my/package-rebuild-autoloads (&rest _args)
  (interactive)
  (let ((autoloads (file-expand-wildcards
                    (expand-file-name "elpa/*/*-autoloads.el"
                                      user-emacs-directory))))
    (with-temp-buffer
      (dolist (file autoloads)
        (insert-file-contents file)

        ;; detect custom themes
        (when (with-temp-buffer
                (insert-file-contents file)
                (search-forward "'custom-theme-load-path" nil t))
          (when (boundp 'custom-theme-load-path)
            (insert (format "(add-to-list 'custom-theme-load-path \"%s\")"
                            (file-name-as-directory
                             (file-name-directory file)))))))

      (insert (format "(setq my/package-cached-autoloads '%S)"
                      (mapcar #'file-name-sans-extension
                              (file-expand-wildcards
                               (expand-file-name "elpa/*/*-autoloads.el"
                                                 user-emacs-directory)))))

      (let ((mtime (nth 6 (file-attributes
                           (expand-file-name "elpa"
                                             user-emacs-directory)))))
        (insert (format "(setq my/package-cache-last-build-time '%S)" mtime)))
      (write-file package-autoload-file nil)
      (cl-letf ((load-path))
        (load package-autoload-file)))))

(unwind-protect (progn
                  (unless (file-exists-p package-autoload-file)
                    (my/package-rebuild-autoloads))
                  (cl-letf ((load-path))
                    (load package-autoload-file))
                  (unless (equal (nth 6 (file-attributes
                                         (expand-file-name
                                          "elpa" user-emacs-directory)))
                                 my/package-cache-last-build-time)
                    (my/package-rebuild-autoloads)))

  (dolist (dir (file-expand-wildcards
                (expand-file-name "elpa/*" user-emacs-directory)))
    (when (file-directory-p dir)
      (add-to-list 'load-path dir))))

(defun nadvice/package-initialize (old-fun &rest args)
  (cl-letf* ((orig-load (symbol-function 'load))
             ((symbol-function 'load)
              (lambda (&rest args)
                (cl-destructuring-bind
                    (file &optional _noerror _nomessage _nosuffix _must-suffix)
                    args
                  (unless (member file my/package-cached-autoloads)
                    (message "Package cache miss: %s" file)
                    (my/package-rebuild-autoloads)
                    (apply orig-load args))))))
    (apply old-fun args)))

(advice-add 'package-initialize :around #'nadvice/package-initialize)
(package-initialize)

;; Guarantee all packages are installed on start
(defun my/has-package-not-installed (packages)
  (catch 'package-return
    (dolist (package packages)
      (unless (package-installed-p package)
        (throw 'package-return t)))
    (throw 'package-return nil)))

(defun my/ensure-packages-are-installed (packages)
  (interactive)
  (save-window-excursion
    (when (my/has-package-not-installed packages)
      (package-refresh-contents)
      (dolist (package packages)
        (unless (package-installed-p package)
          (package-install package)))
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
    evil-quickscope

    ace-window
    adaptive-wrap
    aggressive-indent
    auto-compile
    auto-highlight-symbol
    auto-indent-mode
    ;; avy
    bracketed-paste
    company
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
    magit
    multiple-cursors
    rainbow-delimiters
    smartparens
    smooth-scrolling
    solarized-theme
    volatile-highlights
    which-key
    whole-line-or-region
    ws-butler
    xclip
    yasnippet))

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
          which-key
          hydra
          evil-snipe
          multiple-cursors
          avy)))

(with-eval-after-load 'idle-require
  (defun nadvice/idle-require-quiet (old-fun &rest args)
    (with-demoted-errors "Idle require error: %s"
      (cl-letf* ((old-load (symbol-function 'load))
                 ((symbol-function 'message) #'format)
                 ((symbol-function 'load)
                  (lambda (file &optional noerror _nomessage &rest args)
                    (apply old-load file noerror t args))))
        (apply old-fun args))))

  (advice-add 'idle-require-load-next :around #'nadvice/idle-require-quiet))

(add-hook 'emacs-startup-hook
          (lambda ()
            (run-with-idle-timer 0.1 nil
                                 (lambda ()
                                   (require 'helm-files)
                                   (require 'helm-ring)
                                   (require 'helm-projectile)
                                   (require 'helm-semantic)
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
                   (format "Upgrade %d package%s (%s)? "
                           (length upgrades)
                           (if (= (length upgrades) 1) "" "s")
                           (mapconcat #'package-desc-full-name upgrades ", "))))
          (save-window-excursion
            (dolist (package-desc upgrades)
              (let ((old-package (cadr (assq (package-desc-name package-desc)
                                             package-alist))))
                (package-install package-desc)
                (package-delete  old-package)))
            (message "All package upgrades completed.")))
      (message "All packages are up to date"))))

(eval-and-compile
  (defun my/remove-keyword-params (seq)
    (and seq
         (cl-destructuring-bind (head . tail) seq
           (if (keywordp head) (my/remove-keyword-params (cdr tail))
             (cons head (my/remove-keyword-params tail)))))))

(cl-defmacro package-deferred-install (package-name
                                       &rest forms
                                       &key feature-name
                                       mode-entries
                                       autoload-names
                                       manual-setup
                                       &allow-other-keys)
  (declare (indent 4))
  `(with-no-warnings
     (unless (package-installed-p ,package-name)
       ,@(when manual-setup
           (list manual-setup))
       ,@(mapcar (lambda (item)
                   `(add-to-list 'auto-mode-alist ,item))
                 (cadr mode-entries))
       ,@(mapcar (lambda (name)
                   `(defun ,(cadr name) (&rest args)
                      (interactive)
                      (save-window-excursion
                        (package-install ,package-name))
                      (require ,(or feature-name package-name))
                      (if (called-interactively-p)
                          (call-interactively ,name)
                        (apply ,name args))))
                 (cadr autoload-names)))
     ,@(let ((forms (my/remove-keyword-params forms)))
         (when forms
           (list `(with-eval-after-load ,(or feature-name package-name)
                    ,@forms))))))

(provide 'config-package)
