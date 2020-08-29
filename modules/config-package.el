;; -*- lexical-binding: t -*-
(eval-when-compile (require 'config-macros))
(require 'cl-lib)

;; =============================================
;; Setup straight.el
;; =============================================

(setq straight-repository-branch "develop"
      straight-check-for-modifications 'live
      straight-use-package-version 'ensure
      straight-use-package-by-default t
      straight-recipes-gnu-elpa-use-mirror t)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; ================================================
;; Require packages in the background after startup
;; ================================================

(add-to-list 'load-path (locate-user-emacs-file "personal/"))

(defvar idle-jobs nil
  "Symbols which need to be autoloaded.")

(defvar idle-job-timer (run-with-idle-timer 0.1 t 'idle-job-run-next))

(defun idle-job-run-next ()
  "Load symbols from `idle-require-symbols' until input occurs."
  (while (and idle-jobs
              (not (input-pending-p)))
    (cl-letf* ((old-load (symbol-function #'load))
               ((symbol-function #'load)
                (lambda (file &optional noerror _nomessage &rest args)
                  (apply old-load
                         file
                         noerror
                         (not (eq debug-on-error 'startup))
                         args))))
      (with-demoted-errors "Idle job error: %s"
        (funcall (pop idle-jobs))))))

(defun idle-job-add-require (sym &optional append)
  (cl-letf ((fun (lambda ()
                   (let ((start-time (current-time))
                         (verbose my/flag-debug-init))
                     (unless (require sym nil t)
                       (message "failed to load %s" sym))

                     (when verbose
                       (message "%.3f loaded %s"
                                (float-time (time-subtract
                                             (current-time)
                                             start-time))
                                sym))))))
    (if append
        (setq idle-jobs (append idle-jobs (list fun)))
      (push fun idle-jobs))))

(defun idle-job-add-function (sym &optional append)
  (cl-letf ((fun (lambda ()
                   (let ((start-time (current-time))
                         (verbose my/flag-debug-init))
                     (funcall sym)
                     (when verbose
                       (message "%.3f ran %S"
                                (float-time (time-subtract
                                             (current-time)
                                             start-time))
                                sym))))))
    (if append
        (setq idle-jobs (append idle-jobs (list fun)))
      (push fun idle-jobs))))

(idle-job-add-function (my/defun-as-value my/ws-butler-init ()
                         (ws-butler-global-mode +1)))

(idle-job-add-require 'magit)

(defmacro my/load-magit-submodule (sym)
  `(idle-job-add-function
    (my/defun-as-value
        ,(intern (format "my/lazy-load-%s" (symbol-name (cadr sym)))) ()
      (cl-letf* ((old-require (symbol-function #'require))
                 ((symbol-function #'require)
                  (lambda (feature &optional filename noerror)
                    (unless (eq feature 'magit)
                      (funcall old-require
                               feature
                               filename
                               noerror)))))
        (unless (require ,sym nil t)
          (message "failed to load %s" ,sym))))))

(my/load-magit-submodule 'magit-bookmark)
(my/load-magit-submodule 'magit-submodule)
(my/load-magit-submodule 'magit-obsolete)
(my/load-magit-submodule 'magit-blame)
(my/load-magit-submodule 'magit-stash)
(my/load-magit-submodule 'magit-bisect)
(my/load-magit-submodule 'magit-push)
(my/load-magit-submodule 'magit-pull)
(my/load-magit-submodule 'magit-fetch)
(my/load-magit-submodule 'magit-clone)
(my/load-magit-submodule 'magit-remote)
(my/load-magit-submodule 'magit-commit)
(my/load-magit-submodule 'magit-sequence)
(my/load-magit-submodule 'magit-notes)
(my/load-magit-submodule 'magit-worktree)
(my/load-magit-submodule 'magit-tag)
(my/load-magit-submodule 'magit-merge)
(my/load-magit-submodule 'magit-branch)
(my/load-magit-submodule 'magit-reset)
(my/load-magit-submodule 'magit-files)
(my/load-magit-submodule 'magit-refs)
(my/load-magit-submodule 'magit-status)

(idle-job-add-require 'package)
(idle-job-add-require 'magit-repos)
(idle-job-add-require 'magit-apply)
(idle-job-add-require 'magit-wip)
(idle-job-add-require 'magit-log)
(idle-job-add-require 'magit-diff)
(idle-job-add-require 'magit-core)
(idle-job-add-require 'magit-autorevert)
(idle-job-add-require 'magit-margin)
(idle-job-add-require 'magit-mode)
(idle-job-add-require 'transient)
(idle-job-add-require 'git-commit)
(idle-job-add-require 'log-edit)
(idle-job-add-require 'message)
(idle-job-add-require 'rmc)
(idle-job-add-require 'puny)
(idle-job-add-require 'rfc822)
(idle-job-add-require 'mml)
(idle-job-add-require 'mml-sec)
(idle-job-add-require 'epa)
(idle-job-add-require 'epg)
(idle-job-add-require 'gnus-util)
(idle-job-add-require 'rmail)
(idle-job-add-require 'rmail-loaddefs)
(idle-job-add-require 'mm-decode)
(idle-job-add-require 'mm-bodies)
(idle-job-add-require 'mail-parse)
(idle-job-add-require 'mailabbrev)
(idle-job-add-require 'mail-utils)
(idle-job-add-require 'gmm-utils)
(idle-job-add-require 'mailheader)
(idle-job-add-require 'add-log)
(idle-job-add-require 'pcvs-util)
(idle-job-add-require 'with-editor)

(idle-job-add-require 'volatile-highlights)

(idle-job-add-function #'my/yas-init)
(idle-job-add-require 'yasnippet)

(idle-job-add-require 'company)

(idle-job-add-require 'multiple-cursors)
(idle-job-add-require 'mc-hide-unmatched-lines-mode)
(idle-job-add-require 'mc-separate-operations)
(idle-job-add-require 'rectangular-region-mode)
(idle-job-add-require 'mc-mark-pop)
(idle-job-add-require 'mc-mark-more)
(idle-job-add-require 'mc-cycle-cursors)
(idle-job-add-require 'mc-edit-lines)
(idle-job-add-require 'multiple-cursors-core)

(idle-job-add-require 'ace-jump-helm-line)
(idle-job-add-require 'avy)

(idle-job-add-require 'expand-region)
(idle-job-add-require 'er-basic-expansions)
(idle-job-add-require 'expand-region-core)
(idle-job-add-require 'expand-region-custom)
(idle-job-add-require 'evil-snipe)

(idle-job-add-require 'counsel)
(idle-job-add-require 'swiper)
(idle-job-add-require 'ivy)
(idle-job-add-require 'colir)
(idle-job-add-require 'ivy-overlay)
(idle-job-add-require 'ffap)
(idle-job-add-require 'dired)
(idle-job-add-require 'dired-loaddefs)

(idle-job-add-require 'helm-projectile)
(idle-job-add-require 'projectile)
(idle-job-add-require 'grep)
(idle-job-add-require 'compile)
(idle-job-add-require 'ibuf-ext)
(idle-job-add-require 'ibuffer)

(idle-job-add-require 'helm-for-files)
(idle-job-add-require 'recentf)
(idle-job-add-require 'helm-bookmark)
(idle-job-add-require 'bookmark)
(idle-job-add-require 'browse-url)
(idle-job-add-require 'xml)

(idle-job-add-require 'url)
(idle-job-add-require 'url-privacy)
(idle-job-add-require 'url-expand)
(idle-job-add-require 'url-history)
(idle-job-add-require 'mailcap)

(idle-job-add-require 'helm-ring)
(idle-job-add-require 'helm-elisp)
(idle-job-add-require 'helm-eval)
(idle-job-add-require 'edebug)
(idle-job-add-require 'helm-info)

(idle-job-add-require 'helm-files)
(idle-job-add-require 'helm-buffers)
(idle-job-add-require 'helm-occur)
(idle-job-add-require 'helm-tags)
(idle-job-add-require 'helm-locate)
(idle-job-add-require 'helm-grep)
(idle-job-add-require 'helm-types)
(idle-job-add-require 'helm-utils)
(idle-job-add-require 'helm-help)

(idle-job-add-require 'helm)
(idle-job-add-require 'helm-source)
(idle-job-add-require 'helm-lib)

(idle-job-add-require 'winner)

(straight-use-package '(use-package
                         :type git
                         :host github
                         :repo "raxod502/use-package"))

(straight-use-package '(el-patch
                        :type git
                        :host github
                        :repo "raxod502/el-patch"))

(eval-when-compile (require 'el-patch))
(setq el-patch-use-aggressive-defvar t)
(defvar el-patch--patches (make-hash-table))

(with-eval-after-load 'el-patch
  (el-patch-deftype evil-define-command
    :classify el-patch-classify-function
    :locate el-patch-locate-function
    :declare ((doc-string 3)
              (indent defun)))
  (el-patch-deftype evil-define-motion
    :classify el-patch-classify-function
    :locate el-patch-locate-function
    :declare ((doc-string 3)
              (indent defun)))
  (el-patch-deftype evil-define-text-object
    :classify el-patch-classify-function
    :locate el-patch-locate-function
    :declare ((doc-string 3)
              (indent defun)))
  (el-patch-deftype evil-define-operator
    :classify el-patch-classify-function
    :locate el-patch-locate-function
    :declare ((doc-string 3)
              (indent defun))))

(el-patch-feature use-package)

(with-eval-after-load 'use-package
  (setq use-package-always-ensure t
        use-package-always-defer t)

  (advice-add
   'straight-use-package-ensure-function :around
   (my/defun-as-value nadvice/straight-use-package-ensure-function (old-fun &rest args)
     (cl-letf* (((symbol-function #'y-or-n-p) (lambda (_prompt) t)))
       (apply old-fun args))))

  (el-patch-defun use-package-handler/:ensure (name keyword ensure rest state)
    (let* ((body (use-package-process-keywords name rest
                   ;; Here we are conditionally updating the marker
                   ;; value for deferred installation; this will be
                   ;; checked later by `:config'. For more information
                   ;; see `use-package-handler/:defer-install'.
                   (if (eq (plist-get state :defer-install)
                           :defer-install)
                       (plist-put state :defer-install :ensure)
                     state))))
      ;; We want to avoid installing packages when the `use-package'
      ;; macro is being macro-expanded by elisp completion (see
      ;; `lisp--local-variables'), but still do install packages when
      ;; byte-compiling to avoid requiring `package' at runtime.
      (el-patch-add
        (when (and (bound-and-true-p byte-compile-current-file)
                   (not (plist-get state :defer-install)))
          ;; Eval when byte-compiling,
          (funcall use-package-ensure-function
                   name ensure state :byte-compile)))
      (cond
       ((plist-get state :defer-install)
        (push
         `(puthash ',name '(,ensure . ,state)
                   use-package--deferred-packages)
         body)
        (push `(,use-package-pre-ensure-function
                ',name ',ensure ',state)
              body))
       (el-patch-remove
         ((bound-and-true-p byte-compile-current-file)
          ;; Eval when byte-compiling,
          (funcall use-package-ensure-function
                   name ensure state :byte-compile)))
       ;;  or else wait until runtime.
       (t (el-patch-wrap 2
            (unless (and (not ensure)
                         (eq use-package-ensure-function
                             'straight-use-package-ensure-function))
              (push `(,use-package-ensure-function
                      ',name ',ensure ',state :ensure)
                    body)))))
      body)))

(autoload 'use-package-install-deferred-package "use-package"
  "Install a package whose installation has been deferred.
NAME should be a symbol naming a package (actually, a feature).
This is done by calling `use-package-ensure-function' is called
with four arguments: the key (NAME) and the two elements of the
cons in `use-package--deferred-packages' (the value passed to
`:ensure', and the `state' plist), and a keyword providing
information about the context in which the installation is
happening. (This defaults to `:unknown' but can be overridden by
providing CONTEXT.)

Return t if the package is installed, nil otherwise. (This is
determined by the return value of `use-package-ensure-function'.)
If the package is installed, its entry is removed from
`use-package--deferred-packages'. If the package has no entry in
`use-package--deferred-packages', do nothing and return t.")

(el-patch-defvar use-package--deferred-packages (make-hash-table)
  "Hash mapping packages to data about their installation.

The keys are not actually symbols naming packages, but rather
symbols naming the features which are the names of \"packages\"
required by `use-package' forms. Since
`use-package-ensure-function' could be set to anything, it is
actually impossible for `use-package' to determine what package
is supposed to provide the feature being ensured just based on
the value of `:ensure'.

Each value is a cons, with the car being the the value passed to
`:ensure' and the cdr being the `state' plist. See
`use-package-install-deferred-package' for information about how
these values are used to call `use-package-ensure-function'.")

(use-package hydra
  :init
  (autoload 'hydra-default-pre "hydra"))

(use-package s)
(use-package restart-emacs)

(use-package exec-path-from-shell
  :config
  (setq exec-path-from-shell-check-startup-files nil)

  :init
  (eval-when-compile
    (with-demoted-errors "Load error: %s"
      (require 'exec-path-from-shell)))

  (when (memq window-system '(mac ns))
    (setq exec-path
          (or (eval-when-compile
                (require 'cl-lib)
                (exec-path-from-shell-initialize)
                (cl-remove-duplicates exec-path :test #'string=))
              exec-path))))

(provide 'config-package)
