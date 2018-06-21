;; -*- lexical-binding: t -*-

(use-package auth-source
  :ensure nil
  :config
  (add-to-list 'auth-sources (locate-user-emacs-file "data/authinfo.gpg")))

(use-package vc-hooks
  :ensure nil
  :config
  (setq vc-follow-symlinks t)

  (defun my/maybe/vc-refresh-state ()
    (unless (my/slow-fs default-directory)
      (vc-refresh-state)))

  (remove-hook 'find-file-hook #'vc-refresh-state)
  (add-hook 'find-file-hook #'my/maybe/vc-refresh-state))

(use-package vc-git
  :ensure nil
  :config
  (setq vc-git-diff-switches '("--histogram")))

(use-package diff-hl
  :init
  (defun my/maybe-diff-hl-mode ()
    (when (and (display-graphic-p)
               (not (my/slow-fs buffer-file-name)))
      (diff-hl-mode +1)
      (diff-hl-update)))

  (add-hook 'find-file-hook #'my/maybe-diff-hl-mode)

  :config
  (setq diff-hl-draw-borders nil)
  (diff-hl-flydiff-mode +1)
  ;; Automatically refresh when magit state changes
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(use-package magit
  :init (defvar magit-no-message (list "Turning on magit-auto-revert-mode"))
  :config
  (eval-when-compile
    (with-demoted-errors "Load error: %s"
      (require 'magit)
      (require 'el-patch)))

  ;; Don't use graphical password prompt in terminal
  (unless (display-graphic-p)
    (setenv "SSH_ASKPASS" "magit-askpass"))

  (setq magit-push-always-verify nil
        magit-completing-read-function #'magit-ido-completing-read
        magit-log-format-graph-function #'magit-log-format-unicode-graph
        magit-completing-read-function 'ivy-completing-read
        magit-diff-refine-hunk t)

  (evil-set-initial-state 'magit-status-mode 'insert)
  (evil-set-initial-state 'magit-log-mode 'insert)
  (evil-set-initial-state 'magit-popup-mode 'insert)
  (evil-set-initial-state 'magit-refs-mode 'insert)
  (evil-set-initial-state 'magit-stash-mode 'insert)
  (evil-set-initial-state 'magit-revision-mode 'motion)
  (evil-set-initial-state 'magit-process-mode 'motion)
  (evil-set-initial-state 'git-rebase-mode 'emacs)

  (define-key magit-log-mode-map "j" #'next-line)
  (define-key magit-refs-mode-map "j" #'next-line)
  (define-key magit-status-mode-map "j" #'next-line)

  (magit-change-popup-key 'magit-fetch-popup  :action ?u ?f)
  (magit-change-popup-key 'magit-pull-popup   :action ?u ?F)
  (magit-change-popup-key 'magit-rebase-popup :action ?e ?r)
  (magit-change-popup-key 'magit-push-popup   :action ?p ?P)

  (cl-macrolet
      ((magit-setup-section-k
        (mode &optional command)
        `(with-demoted-errors "magit setup error: %s"
           (define-key ,mode (kbd "<C-tab>") nil)
           (define-key ,mode (kbd "<S-tab>") #'magit-section-cycle)
           (define-key ,mode (kbd "<backtab>") #'magit-section-cycle)
           (define-key ,mode (kbd "k") #'previous-line)
           ,(when command
              `(define-key ,mode (kbd "K") ,command)))))
    (with-no-warnings
      (my/generate-calls
          'magit-setup-section-k
        '((magit-branch-section-map #'magit-branch-delete)
          (magit-commit-section-map)
          (magit-file-section-map #'magit-discard)
          (magit-hunk-section-map #'magit-discard)
          (magit-log-mode-map)
          (magit-module-commit-section-map)
          (magit-remote-section-map)
          (magit-staged-section-map #'magit-discard)
          (magit-stash-section-map #'magit-stash-drop)
          (magit-stashes-section-map)
          (magit-status-mode-map)
          (magit-tag-section-map #'magit-tag-delete)
          (magit-unpulled-section-map)
          (magit-unpushed-section-map)
          (magit-unstaged-section-map #'magit-discard)
          (magit-untracked-section-map #'magit-discard)))))

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

  (defun nadvice/magit-revert-buffers (&rest _args)
    (run-with-timer 1 nil #'message ""))

  (advice-add 'magit-revert-buffers :after #'nadvice/magit-revert-buffers)

  ;; If there is only one stash, operate on it immediately
  (el-patch-defun magit-read-stash (prompt)
    (let ((stashes (magit-list-stashes)))
      (el-patch-wrap 2 1
        (if (> (length stashes) 1)
            (magit-completing-read prompt stashes nil t nil nil
                                   (magit-stash-at-point)
                                   (car stashes))
          (car stashes)))))

  (el-patch-defun magit-process-username-prompt (process string)
    "Forward username prompts to the user."
    (--when-let (magit-process-match-prompt
                 magit-process-username-prompt-regexps string)
      (process-send-string
       process (magit-process-kill-on-abort
                   process
                 (concat (el-patch-swap
                           (read-string it nil nil (user-login-name))
                           (read-passwd it nil (user-login-name)))
                         "\n"))))))

(with-eval-after-load 'git-rebase
  (eval-when-compile
    (with-demoted-errors "Load error: %s"
      (require 'git-rebase)))
  (define-key git-rebase-mode-map "j" #'next-line)
  (define-key git-rebase-mode-map "k" #'previous-line)
  (define-key git-rebase-mode-map "K" #'git-rebase-kill-line))

(use-package magithub
  :init
  ;; Magithub is not well-behaved, so this needs to be set early
  (setq magithub-dir (locate-user-emacs-file "data/magithub"))
  :config
  (evil-set-initial-state 'magithub-dash-mode 'motion)
  (evil-set-initial-state 'magithub-issue-view-mode 'motion)
  (magithub-feature-autoinject t))

(use-package magit-svn
  :after magit
  :config
  (add-hook 'magit-mode-hook 'magit-svn-mode))

(use-package smerge-mode
  :ensure nil
  :init
  (eval-when-compile
    (with-demoted-errors "Load error: %s"
      (require 'hydra)))

  (autoload #'smerge-remove-props "smerge-mode" nil nil)
  (autoload #'smerge-match-conflict "smerge-mode" nil nil)

  (el-patch-feature smerge-mode)

  (defhydra hydra/smerge-tools (:color blue :hint nil :idle 0.3)
    "
_p_ ← → _n_ Keep _a_ll _b_ase _m_ine _o_ther | _C_ombine _E_diff _R_efine _r_esolve
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

    ("=<" smerge-diff-base-mine)
    ("==" smerge-diff-mine-other)
    ("=>" smerge-diff-base-other))

  (el-patch-defcustom smerge-command-prefix "\C-c^"
    "Prefix for `smerge-mode' commands."
    :type '(choice (const :tag "ESC"   "\e")
		   (const :tag "C-c ^" "\C-c^" )
		   (const :tag "none"  "")
		   string))

  (el-patch-defconst smerge-font-lock-keywords
    '((smerge-find-conflict
       (1 smerge-upper-face prepend t)
       (2 smerge-base-face prepend t)
       (3 smerge-lower-face prepend t)
       ;; FIXME: `keep' doesn't work right with syntactic fontification.
       (0 smerge-markers-face keep)
       (4 nil t t)
       (5 nil t t)))
    "Font lock patterns for `smerge-mode'.")

  ;; TODO: This isn't exactly correct, but the map is defined using a
  ;; macro that el-patch doesn't understand.
  (defvar smerge-mode-map (make-sparse-keymap))
  (define-key smerge-mode-map smerge-command-prefix #'hydra/smerge-tools/body)

  (el-patch-defconst smerge-begin-re "^<<<<<<< \\(.*\\)\n")
  (el-patch-defconst smerge-end-re "^>>>>>>> \\(.*\\)\n")
  (el-patch-defconst smerge-base-re "^||||||| \\(.*\\)\n")
  (el-patch-defconst smerge-lower-re "^=======\n")
  (el-patch-defconst smerge-parsep-re
    (concat smerge-begin-re "\\|" smerge-end-re "\\|"
            smerge-base-re "\\|" smerge-lower-re "\\|"))

  (el-patch-defun smerge-conflict-overlay (pos)
    "Return the conflict overlay at POS if any."
    (let ((ols (overlays-at pos))
          conflict)
      (dolist (ol ols)
        (if (and (eq (overlay-get ol 'smerge) 'conflict)
                 (> (overlay-end ol) pos))
            (setq conflict ol)))
      conflict))

  (el-patch-defun smerge-find-conflict (&optional limit)
    "Find and match a conflict region.  Intended as a font-lock MATCHER.
The submatches are the same as in `smerge-match-conflict'.
Returns non-nil if a match is found between point and LIMIT.
Point is moved to the end of the conflict."
    (let ((found nil)
          (pos (point))
          conflict)
      ;; First check to see if point is already inside a conflict, using
      ;; the conflict overlays.
      (while (and (not found) (setq conflict (smerge-conflict-overlay pos)))
        ;; Check the overlay's validity and kill it if it's out of date.
        (condition-case nil
            (progn
              (goto-char (overlay-start conflict))
              (smerge-match-conflict)
              (goto-char (match-end 0))
              (if (<= (point) pos)
                  (error "Matching backward!")
                (setq found t)))
          (error (smerge-remove-props
                  (overlay-start conflict) (overlay-end conflict))
                 (goto-char pos))))
      ;; If we're not already inside a conflict, look for the next conflict
      ;; and add/update its overlay.
      (while (and (not found) (re-search-forward smerge-begin-re limit t))
        (condition-case nil
            (progn
              (smerge-match-conflict)
              (goto-char (match-end 0))
              (let ((conflict (smerge-conflict-overlay (1- (point)))))
                (if conflict
                    ;; Update its location, just in case it got messed up.
                    (move-overlay conflict (match-beginning 0) (match-end 0))
                  (setq conflict (make-overlay (match-beginning 0) (match-end 0)
                                               nil 'front-advance nil))
                  (overlay-put conflict 'evaporate t)
                  (overlay-put conflict 'smerge 'conflict)
                  (let ((props smerge-text-properties))
                    (while props
                      (overlay-put conflict (pop props) (pop props))))))
              (setq found t))
          (error nil)))
      found))

  (el-patch-define-minor-mode smerge-mode
    "Minor mode to simplify editing output from the diff3 program.
With a prefix argument ARG, enable the mode if ARG is positive,
and disable it otherwise.  If called from Lisp, enable the mode
if ARG is omitted or nil.
\\{smerge-mode-map}"
    :group 'smerge :lighter (el-patch-swap " SMerge" nil)
    (when (and (boundp 'font-lock-mode) font-lock-mode)
      (save-excursion
        (if smerge-mode
	    (font-lock-add-keywords nil smerge-font-lock-keywords 'append)
	  (font-lock-remove-keywords nil smerge-font-lock-keywords))
        (goto-char (point-min))
        (while (smerge-find-conflict)
	  (save-excursion
	    (font-lock-fontify-region (match-beginning 0) (match-end 0) nil)))))
    (if (string-match (regexp-quote smerge-parsep-re) paragraph-separate)
        (unless smerge-mode
          (set (make-local-variable 'paragraph-separate)
               (replace-match "" t t paragraph-separate)))
      (when smerge-mode
        (set (make-local-variable 'paragraph-separate)
             (concat smerge-parsep-re paragraph-separate))))
    (unless smerge-mode
      (smerge-remove-props (point-min) (point-max))))


  (add-hook 'find-file-hook #'smerge-mode)

  :config
  (diminish 'smerge-mode))

(with-eval-after-load 'projectile
  (setq projectile-known-projects-file
        (locate-user-emacs-file "data/.projectile-bookmarks.eld")
        projectile-cache-file
        (locate-user-emacs-file "data/.projectile.cache"))

  (projectile-global-mode +1)

  (setq projectile-completion-system 'ivy
        projectile-mode-line
        '(:eval (format (if (display-graphic-p) " ↠" " /"))))
  (define-key projectile-mode-map (kbd "C-c p") #'hydra/projectile-tools/body))

(defhydra hydra/projectile-tools (global-map "C-c p"
                                             :color blue :hint nil :idle 0.3)
  "
Find^^             Operate on project^^      Other window
_f_ file           _c_ compile project       _O f_ file
_d_ dir            _R_ regenerate tags       _O d_ dir
_g_ file dwim      _S_ save project          _O g_ file dwim
_a_ other file     ^!^ command in /          _O a_ other file
_l_ file in dir    ^&^ async command in /    _O b_ switch buffer
_T_ test file      _z_ cache current file    _O t_ implementation←→test
_j_ tag            _i_ invalidate cache      _O C-o_ display buffer

Special^^       Buffers^^                    Search and replace
_D_ dired       _b_ switch buffer            _o_ multi-occur
_e_ recentf     _k_ kill buffers             _r_ replace
_I_ ibuffer     _p_ switch project           _s g_ grep
_v_ vc          _ESC_ project other buffer   _s s_ ag
_m_ commander   _F_ file any project         _s a_ ack

Tests   _P_ test-project    _t_ toggle implementation←→test"
  ("O a" projectile-find-other-file-other-window)
  ("O b" projectile-switch-to-buffer-other-window)
  ("O C-o" projectile-display-buffer)
  ("O d" projectile-find-dir-other-window)
  ("O f" projectile-find-file-other-window)
  ("O g" projectile-find-file-dwim-other-window)
  ("O t" projectile-find-implementation-or-test-other-window)
  ("!" projectile-run-shell-command-in-root)
  ("&" projectile-run-async-shell-command-in-root)
  ("a" projectile-find-other-file)
  ("b" projectile-switch-to-buffer)
  ("c" projectile-compile-project)
  ("d" projectile-find-dir)
  ("D" projectile-dired)
  ("e" projectile-recentf)
  ("f" projectile-find-file)
  ("g" projectile-find-file-dwim)
  ("F" projectile-find-file-in-known-projects)
  ("i" projectile-invalidate-cache)
  ("I" projectile-ibuffer)
  ("j" projectile-find-tag)
  ("k" projectile-kill-buffers)
  ("l" projectile-find-file-in-directory)
  ("m" projectile-commander)
  ("o" projectile-multi-occur)
  ("p" projectile-switch-project)
  ("P" projectile-test-project)
  ("r" projectile-replace)
  ("R" projectile-regenerate-tags)
  ("s a" helm-projectile-ack)
  ("s g" projectile-grep)
  ("s s" helm-projectile-ag)
  ("S" projectile-save-project-buffers)
  ("t" projectile-toggle-between-implementation-and-test)
  ("T" projectile-find-test-file)
  ("v" projectile-vc)
  ("z" projectile-cache-current-file)
  ("ESC" projectile-project-buffers-other-buffer))

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

  (add-hook 'ediff-before-setup-hook
            (lambda ()
              (setq ediff-saved-window-configuration (current-window-configuration))))

  (let ((restore-window-configuration
         (lambda ()
           (set-window-configuration ediff-saved-window-configuration))))
    (add-hook 'ediff-quit-hook restore-window-configuration 'append)
    (add-hook 'ediff-suspend-hook restore-window-configuration 'append))

  (defun nadvice/ediff-setup-keymap (&rest _args)
    (define-key ediff-mode-map "j" #'ediff-next-difference)
    (define-key ediff-mode-map "k" #'ediff-previous-difference))

  (advice-add 'ediff-setup-keymap :after #'nadvice/ediff-setup-keymap)

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

(provide 'config-vcs)
