;; -*- lexical-binding: t -*-
(eval-when-compile
  (require 'config-macros)
  (require 'config-package))

(use-package auth-source
  :ensure nil
  :config
  (add-to-list 'auth-sources (locate-user-emacs-file "data/authinfo.gpg")))

(use-package vc-hooks
  :ensure nil
  :config
  (setq vc-follow-symlinks t)

  (remove-hook 'find-file-hook #'vc-refresh-state)
  (add-hook
   'find-file-hook
   (my/defun-as-value my/maybe/vc-refresh-state ()
     (unless (my/slow-fs default-directory)
       (vc-refresh-state)))))

(use-package vc-git
  :ensure nil
  :config
  (setq vc-git-diff-switches '("--histogram")))

(use-package diff-hl
  :init
  (add-hook
   'find-file-hook
   (my/defun-as-value my/maybe-diff-hl-mode ()
     (when (and (display-graphic-p)
                (not (my/slow-fs buffer-file-name)))
       (diff-hl-mode +1)
       (diff-hl-update))))

  :config
  (setq diff-hl-draw-borders nil)
  (diff-hl-flydiff-mode +1)
  ;; Automatically refresh when magit state changes
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(with-eval-after-load 'transient
  (setq transient-levels-file (locate-user-emacs-file "data/transient/levels.el")
        transient-history-file (locate-user-emacs-file "data/transient/history.el")
        transient-values-file (locate-user-emacs-file "data/transient/values.el"))

  (transient-bind-q-to-quit))

(use-package magit
  :init
  (use-package compat)
  (defvar magit-no-message (list "Turning on magit-auto-revert-mode"))
  :config
  (eval-when-compile
    (with-demoted-errors "Load error: %s"
      (require 'magit)
      (require 'el-patch)))

  ;; Don't use graphical password prompt in terminal
  (unless (display-graphic-p)
    (setenv "SSH_ASKPASS" "magit-askpass"))

  (setq magit-push-always-verify nil
        magit-log-format-graph-function #'magit-log-format-unicode-graph
        magit-completing-read-function #'ivy-completing-read
        magit-diff-refine-hunk t
        magit-pull-or-fetch t
        magit-diff-visit-prefer-worktree t)

  (with-eval-after-load 'evil
    (evil-collection-init 'magit))
  (evil-define-key 'visual magit-mode-map (kbd "o") #'exchange-point-and-mark)
  (evil-set-initial-state 'git-commit-mode 'insert)

  (transient-suffix-put 'magit-fetch "u" :key "f")
  (transient-suffix-put 'magit-pull "u" :key "F")
  (transient-suffix-put 'magit-rebase "e" :key "r")
  (transient-suffix-put 'magit-push "p" :key "P")
  (transient-append-suffix 'magit-pull "m"
    '("-a" "Autostash" "--autostash"))

  ;; disable regular key chords by switching input methods
  (defun my/setup-magit-mode ()
    (setq-local input-method-function nil)
    (setq-local global-hl-line-mode nil))

  (add-hook 'magit-log-mode-hook #'my/setup-magit-mode)
  (add-hook 'magit-refs-mode-hook #'my/setup-magit-mode)
  (add-hook 'magit-status-mode-hook #'my/setup-magit-mode)
  (add-hook 'magit-stash-mode-hook #'my/setup-magit-mode)
  (add-hook 'git-rebase-mode-hook #'my/setup-magit-mode)

  (add-hook 'with-editor-mode-hook #'evil-insert-state)
  (add-hook 'with-editor-mode-hook #'jit-lock-fontify-now)

  (define-advice magit-revert-buffers
      (:after (&rest _args) clear-echo-area)
    (run-with-timer 1 nil #'message ""))

  ;; If there is only one stash, operate on it immediately
  (define-advice magit-read-stash (:around (old-fun &rest args) auto-select-single)
    (cl-letf* ((old-magit-completing-read (symbol-function #'magit-completing-read))
               ((symbol-function #'magit-completing-read)
                (lambda (prompt choices &rest args)
                  (if (= (length choices) 1)
                      (car choices)
                    (apply old-magit-completing-read prompt choices args)))))
      (apply old-fun args)))

  (define-advice magit-process-username-prompt
      (:around (old-fun &rest args) read-username-as-passwd)
    (cl-letf* (((symbol-function #'read-string)
                (lambda (prompt &optional _init _hist default inherit)
                  (read-passwd prompt nil default))))
      (apply old-fun args))))

(use-package magit-diff
  :ensure nil
  :config
  (defun magit-stage-smallest-hunk ()
    (interactive)
    (let ((prev nil))
      (magit-diff-set-context
       (lambda (cur)
         (setq prev cur)
         0))
      (unwind-protect
          (magit-apply)
        (magit-diff-set-context (lambda (cur) prev)))))

  (define-key magit-diff-mode-map (kbd "=") #'magit-stage-smallest-hunk))

(with-eval-after-load 'git-rebase
  (eval-when-compile
    (with-demoted-errors "Load error: %s"
      (require 'git-rebase)))
  (define-key git-rebase-mode-map "j" #'next-line)
  (define-key git-rebase-mode-map "k" #'previous-line)
  (define-key git-rebase-mode-map "K" #'git-rebase-kill-line))

(use-package magit-svn
  :after magit
  :config
  (add-hook 'magit-mode-hook 'magit-svn-mode))

(use-package smerge-mode
  :ensure nil
  :init
  (with-eval-after-load 'vc-git
    (define-advice vc-message-unresolved-conflicts
        (:around (old-fun filename) mention-smerge)
      (if (not(when buffer-file-name
                (eq (vc-backend buffer-file-name) 'Git)))
          (funcall old-fun filename)
        (require 'smerge-mode)
        (message
         "There are unresolved conflicts in %s. Use %s to resolve."
         filename
         (key-description smerge-command-prefix)))))

  :config
  (eval-when-compile
    (with-demoted-errors "Load error: %s"
      (require 'hydra)))

  (diminish 'smerge-mode)

  (defhydra hydra/smerge-tools (:color blue :hint nil :idle 0.3)
    "
_p_ ← → _n_ Keep _a_ll _b_ase _m_ine _o_ther _u_ndo | _C_ombine _E_diff _R_efine _r_esolve
Diff _=<_ base/mine  _==_ mine/other  _=>_ base/other
"
    ("C" smerge-combine-with-next)
    ("E" smerge-ediff)
    ("R" smerge-refine)
    ("r" smerge-resolve)

    ("RET" smerge-keep-current :color red)
    ("a" smerge-keep-all :color red)
    ("b" smerge-keep-base :color red)
    ("m" smerge-keep-mine :color red)
    ("o" smerge-keep-other :color red)

    ("n" smerge-next :color red)
    ("p" smerge-prev :color red)

    ("u" undo :color red)

    ("=<" smerge-diff-base-mine)
    ("==" smerge-diff-mine-other)
    ("=>" smerge-diff-base-other))

  (define-key smerge-mode-map smerge-command-prefix #'hydra/smerge-tools/body)

  (set-face-attribute 'smerge-refined-added nil
                      :background 'unspecified
                      :inherit 'magit-diff-added-highlight)
  (set-face-attribute 'smerge-refined-removed nil
                      :background 'unspecified
                      :inherit 'magit-diff-removed-highlight))

(use-package projectile
  :init
  (eval-when-compile
    (with-demoted-errors "Load error: %s"
      (require 'el-patch)))

  (autoload #'projectile-find-file-hook-function "projectile"
    "Called by `find-file-hook' when `projectile-mode' is on.

The function does pretty much nothing when triggered on remote files
as all the operations it normally performs are extremely slow over
tramp.")

  (autoload #'projectile-track-known-projects-find-file-hook "projectile"
    "Function for caching projects with `find-file-hook'.")

  (autoload #'delete-file-projectile-remove-from-cache "projectile")

  (autoload #'compilation-find-file-projectile-find-compilation-buffer "projectile"
    "Try to find a buffer for FILENAME, if we cannot find it,
fallback to the original function.")

  (el-patch-feature projectile)
  (defgroup projectile nil
    "Manage and navigate projects easily."
    :group 'tools
    :group 'convenience
    :link '(url-link :tag "Github" "https://github.com/bbatsov/projectile")
    :link '(url-link :tag "Online Manual" "https://projectile.readthedocs.io/")
    :link '(emacs-commentary-link :tag "Commentary" "projectile"))

  (el-patch-defcustom projectile-keymap-prefix nil
    "Projectile keymap prefix."
    :group 'projectile
    :type 'string)

  (el-patch-defvar projectile-command-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "4 a") #'projectile-find-other-file-other-window)
      (define-key map (kbd "4 b") #'projectile-switch-to-buffer-other-window)
      (define-key map (kbd "4 C-o") #'projectile-display-buffer)
      (define-key map (kbd "4 d") #'projectile-find-dir-other-window)
      (define-key map (kbd "4 D") #'projectile-dired-other-window)
      (define-key map (kbd "4 f") #'projectile-find-file-other-window)
      (define-key map (kbd "4 g") #'projectile-find-file-dwim-other-window)
      (define-key map (kbd "4 t") #'projectile-find-implementation-or-test-other-window)
      (define-key map (kbd "5 a") #'projectile-find-other-file-other-frame)
      (define-key map (kbd "5 b") #'projectile-switch-to-buffer-other-frame)
      (define-key map (kbd "5 d") #'projectile-find-dir-other-frame)
      (define-key map (kbd "5 D") #'projectile-dired-other-frame)
      (define-key map (kbd "5 f") #'projectile-find-file-other-frame)
      (define-key map (kbd "5 g") #'projectile-find-file-dwim-other-frame)
      (define-key map (kbd "5 t") #'projectile-find-implementation-or-test-other-frame)
      (define-key map (kbd "!") #'projectile-run-shell-command-in-root)
      (define-key map (kbd "&") #'projectile-run-async-shell-command-in-root)
      (define-key map (kbd "a") #'projectile-find-other-file)
      (define-key map (kbd "b") #'projectile-switch-to-buffer)
      (define-key map (kbd "C") #'projectile-configure-project)
      (define-key map (kbd "c") #'projectile-compile-project)
      (define-key map (kbd "d") #'projectile-find-dir)
      (define-key map (kbd "D") #'projectile-dired)
      (define-key map (kbd "e") #'projectile-recentf)
      (define-key map (kbd "E") #'projectile-edit-dir-locals)
      (define-key map (kbd "f") #'projectile-find-file)
      (define-key map (kbd "g") #'projectile-find-file-dwim)
      (define-key map (kbd "F") #'projectile-find-file-in-known-projects)
      (define-key map (kbd "i") #'projectile-invalidate-cache)
      (define-key map (kbd "I") #'projectile-ibuffer)
      (define-key map (kbd "j") #'projectile-find-tag)
      (define-key map (kbd "k") #'projectile-kill-buffers)
      (define-key map (kbd "l") #'projectile-find-file-in-directory)
      (define-key map (kbd "m") #'projectile-commander)
      (define-key map (kbd "o") #'projectile-multi-occur)
      (define-key map (kbd "p") #'projectile-switch-project)
      (define-key map (kbd "q") #'projectile-switch-open-project)
      (define-key map (kbd "P") #'projectile-test-project)
      (define-key map (kbd "r") #'projectile-replace)
      (define-key map (kbd "R") #'projectile-regenerate-tags)
      (define-key map (kbd "s g") #'projectile-grep)
      (define-key map (kbd "s r") #'projectile-ripgrep)
      (define-key map (kbd "s s") #'projectile-ag)
      (define-key map (kbd "S") #'projectile-save-project-buffers)
      (define-key map (kbd "t") #'projectile-toggle-between-implementation-and-test)
      (define-key map (kbd "T") #'projectile-find-test-file)
      (define-key map (kbd "u") #'projectile-run-project)
      (define-key map (kbd "v") #'projectile-vc)
      (define-key map (kbd "V") #'projectile-browse-dirty-projects)
      (define-key map (kbd "x e") #'projectile-run-eshell)
      (define-key map (kbd "x i") #'projectile-run-ielm)
      (define-key map (kbd "x t") #'projectile-run-term)
      (define-key map (kbd "x s") #'projectile-run-shell)
      (define-key map (kbd "x g") #'projectile-run-gdb)
      (define-key map (kbd "x v") #'projectile-run-vterm)
      (define-key map (kbd "z") #'projectile-cache-current-file)
      (define-key map (kbd "<left>") #'projectile-previous-project-buffer)
      (define-key map (kbd "<right>") #'projectile-next-project-buffer)
      (define-key map (kbd "ESC") #'projectile-project-buffers-other-buffer)
      map)
    "Keymap for Projectile commands after `projectile-keymap-prefix'.")
  (fset 'projectile-command-map projectile-command-map)

  (el-patch-defvar projectile-mode-map
    (let ((map (make-sparse-keymap)))
      (when projectile-keymap-prefix
        (define-key map projectile-keymap-prefix 'projectile-command-map))
      (easy-menu-define projectile-mode-menu map
        "Menu for Projectile"
        '("Projectile"
          ["Find file" projectile-find-file]
          ["Find file in known projects" projectile-find-file-in-known-projects]
          ["Find test file" projectile-find-test-file]
          ["Find directory" projectile-find-dir]
          ["Find file in directory" projectile-find-file-in-directory]
          ["Find other file" projectile-find-other-file]
          ["Switch to buffer" projectile-switch-to-buffer]
          ["Jump between implementation file and test file" projectile-toggle-between-implementation-and-test]
          ["Kill project buffers" projectile-kill-buffers]
          ["Save project buffers" projectile-save-project-buffers]
          ["Recent files" projectile-recentf]
          ["Previous buffer" projectile-previous-project-buffer]
          ["Next buffer" projectile-next-project-buffer]
          "--"
          ["Toggle project wide read-only" projectile-toggle-project-read-only]
          ["Edit .dir-locals.el" projectile-edit-dir-locals]
          "--"
          ["Switch to project" projectile-switch-project]
          ["Switch to open project" projectile-switch-open-project]
          ["Discover projects in directory" projectile-discover-projects-in-directory]
          ["Browse dirty projects" projectile-browse-dirty-projects]
          ["Open project in dired" projectile-dired]
          "--"
          ["Search in project (grep)" projectile-grep]
          ["Search in project (ag)" projectile-ag]
          ["Replace in project" projectile-replace]
          ["Multi-occur in project" projectile-multi-occur]
          "--"
          ["Run GDB" projectile-run-gdb]
          "--"
          ["Run shell" projectile-run-shell]
          ["Run eshell" projectile-run-eshell]
          ["Run ielm" projectile-run-ielm]
          ["Run term" projectile-run-term]
          "--"
          ["Cache current file" projectile-cache-current-file]
          ["Invalidate cache" projectile-invalidate-cache]
          ["Regenerate [e|g]tags" projectile-regenerate-tags]
          "--"
          ["Configure project" projectile-configure-project]
          ["Compile project" projectile-compile-project]
          ["Test project" projectile-test-project]
          ["Run project" projectile-run-project]
          ["Repeat last external command" projectile-repeat-last-command]
          "--"
          ["Project info" projectile-project-info]
          ["About" projectile-version]))
      map)
    "Keymap for Projectile mode.")

  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

  (el-patch-define-minor-mode projectile-mode
    "Minor mode to assist project management and navigation.

When called interactively, toggle `projectile-mode'.  With prefix
ARG, enable `projectile-mode' if ARG is positive, otherwise disable
it.

When called from Lisp, enable `projectile-mode' if ARG is omitted,
nil or positive.  If ARG is `toggle', toggle `projectile-mode'.
Otherwise behave as if called interactively.

\\{projectile-mode-map}"
    :lighter projectile--mode-line
    :keymap projectile-mode-map
    :group 'projectile
    :require 'projectile
    :global t
    (cond
     (projectile-mode
      (el-patch-remove
        ;; setup the commander bindings
        (projectile-commander-bindings)
        ;; initialize the projects cache if needed
        (unless projectile-projects-cache
          (setq projectile-projects-cache
                (or (projectile-unserialize projectile-cache-file)
                    (make-hash-table :test 'equal))))
        (unless projectile-projects-cache-time
          (setq projectile-projects-cache-time
                (make-hash-table :test 'equal)))
        ;; load the known projects
        (projectile-load-known-projects)
        ;; update the list of known projects
        (projectile--cleanup-known-projects)
        (projectile-discover-projects-in-search-path))
      (add-hook 'find-file-hook 'projectile-find-file-hook-function)
      (add-hook 'projectile-find-dir-hook #'projectile-track-known-projects-find-file-hook t)
      (add-hook 'dired-before-readin-hook #'projectile-track-known-projects-find-file-hook t t)
      (advice-add 'compilation-find-file :around #'compilation-find-file-projectile-find-compilation-buffer)
      (advice-add 'delete-file :before #'delete-file-projectile-remove-from-cache))
     (t
      (remove-hook 'find-file-hook #'projectile-find-file-hook-function)
      (remove-hook 'dired-before-readin-hook #'projectile-track-known-projects-find-file-hook t)
      (advice-remove 'compilation-find-file #'compilation-find-file-projectile-find-compilation-buffer)
      (advice-remove 'delete-file #'delete-file-projectile-remove-from-cache))))

  (setq projectile-mode-line-prefix (if (display-graphic-p) " ↠" " /"))
  (projectile-mode +1)

  :config
  (define-advice projectile-cleanup-known-projects
      (:around (orig-func &rest args) inhibit-message)
    (let ((inhibit-message t))
      (apply orig-func args)))

  (setq projectile-known-projects-file
        (locate-user-emacs-file "data/.projectile-bookmarks.eld")
        projectile-cache-file
        (locate-user-emacs-file "data/.projectile.cache")
        projectile-completion-system 'ivy
        projectile-dynamic-mode-line nil))

(use-package diff
  :ensure nil
  :config
  (setq diff-switches "-u"))

(use-package ediff
  :ensure nil
  :init
  (defun my/command-line-ediff (_switch)
    (let ((file1 (pop command-line-args-left))
          (file2 (pop command-line-args-left)))
      (ediff file1 file2)))

  (add-to-list 'command-switch-alist '("diff" . my/command-line-ediff))
  :config
  (eval-when-compile
    (with-demoted-errors "Load error: %s"
      (require 'ediff)))

  (defvar ediff-saved-window-configuration)

  (add-hook
   'ediff-before-setup-hook
   (my/defun-as-value my/ediff-save-window-config ()
     (setq ediff-saved-window-configuration (current-window-configuration))))

  (let ((restore-window-configuration
         (lambda ()
           (set-window-configuration ediff-saved-window-configuration))))
    (add-hook 'ediff-quit-hook restore-window-configuration 'append)
    (add-hook 'ediff-suspend-hook restore-window-configuration 'append))

  (define-advice ediff-setup-keymap
      (:after (&rest _args) vi-keys)
    (define-key ediff-mode-map "j" #'ediff-next-difference)
    (define-key ediff-mode-map "k" #'ediff-previous-difference))

  ;; don't start another frame
  (setq ediff-window-setup-function #'ediff-setup-windows-plain))

(use-package git-timemachine
  :defer-install t
  :commands (git-timemachine-toggle
             git-timemachine
             git-timemachine-switch-branch))

(use-package git-undo
  :defer-install t
  :commands (git-undo))

(use-package git-link
  :defer-install t
  :commands (git-link
             git-link-commit
             git-link-homepage))

(use-package deadgrep
  :defer-install t
  :commands (deadgrep))

(provide 'config-vcs)
