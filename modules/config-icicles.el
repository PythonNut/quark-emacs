(eval-when-compile
  (progn
    (require 'cl)
    (require 'cl-lib)
    (require 'key-chord)
    (require 'icicles)
    (require 'config-modes)))

;; custom hook run when icicles in initialized
;; (defvar icicle-init-hook nil)

(add-hook 'icicle-init-hook
  (lambda ()
    (setq
      icicle-highlight-lighter-flag nil
      icicle-max-candidates 300
      icicle-default-cycling-mode 'apropos
      icicle-show-multi-completion-flag t
      icicle-search-highlight-all-current-flag t
      icicle-command-abbrev-match-all-parts-flag nil
      icicle-highlight-input-completion-failure-delay 0
      icicle-Completions-text-scale-decrease 0.2
      icicle-TAB-completion-methods '(vanilla substring basic)
      icicle-expand-input-to-common-match 2
      icicle-apropos-complete-keys (list (kbd "C-i") (kbd "<tab>"))
      icicle-prefix-complete-keys (list (kbd "<backtab>"))
      icicle-yank-function 'cua-paste
      locate-command "locate"
      completions-format 'vertical
      icicle-incremental-completion t
      icicle-incremental-completion-delay 0.1
      icicle-show-Completions-initially-flag nil)))

(defmacro auto-icicle (func)
  `(defadvice ,func (around icy-mode (&rest args) activate)
     (interactive)
     (if (called-interactively-p 'any)
       (if icicle-mode
         (call-interactively (ad-get-orig-definition ',func) args)
         (unwind-protect
           (progn
             (call-interactively 'icicle-mode +1)
             (run-hooks 'icicle-init-hook)
             (call-interactively (ad-get-orig-definition ',func) args))
           (progn
             (call-interactively 'icicle-mode -1)
             (message ""))))
       ad-do-it)))

(defmacro autoload-icicle (func)
  `(autoload ',func "icicles" "autoloaded icicle function" t))

(cl-macrolet
  ((setup-icicles (commands)
     `(progn
        (generate-calls-single autoload-icicle ,commands)
        (with-eval-after-load 'icicles
          (run-hooks 'icicle-init-hook)
          (generate-calls-single auto-icicle ,commands)))))

  (setup-icicles
    (
      icicle-add-buffer-candidate
      icicle-add-buffer-config
      icicle-add-entry-to-saved-completion-set
      icicle-apropos
      icicle-apropos-command
      icicle-apropos-function
      icicle-apropos-option
      icicle-apropos-options-of-type
      icicle-apropos-value
      icicle-apropos-variable
      icicle-apropos-vars-w-val-satisfying
      icicle-apropos-zippy
      ;; icicle-bbdb-complete-mail
      ;; icicle-bbdb-complete-name
      ;; icicle-bookmark
      ;; icicle-bookmark-all-tags
      ;; icicle-bookmark-all-tags-other-window
      ;; icicle-bookmark-all-tags-regexp
      ;; icicle-bookmark-all-tags-regexp-other-window
      ;; icicle-bookmark-annotated-narrow
      ;; icicle-bookmark-autofile
      ;; icicle-bookmark-autofile-all-tags
      ;; icicle-bookmark-autofile-all-tags-other-window
      ;; icicle-bookmark-autofile-all-tags-regexp
      ;; icicle-bookmark-autofile-all-tags-regexp-other-window
      ;; icicle-bookmark-autofile-narrow
      ;; icicle-bookmark-autofile-other-window
      ;; icicle-bookmark-autofile-some-tags
      ;; icicle-bookmark-autofile-some-tags-other-window
      ;; icicle-bookmark-autofile-some-tags-regexp
      ;; icicle-bookmark-autofile-some-tags-regexp-other-window
      ;; icicle-bookmark-autonamed
      ;; icicle-bookmark-autonamed-narrow
      ;; icicle-bookmark-autonamed-other-window
      ;; icicle-bookmark-autonamed-this-buffer
      ;; icicle-bookmark-autonamed-this-buffer-narrow
      ;; icicle-bookmark-autonamed-this-buffer-other-window
      ;; icicle-bookmark-bookmark-file
      ;; icicle-bookmark-bookmark-file-narrow
      ;; icicle-bookmark-bookmark-list
      ;; icicle-bookmark-bookmark-list-narrow
      ;; icicle-bookmark-cmd
      ;; icicle-bookmark-desktop
      ;; icicle-bookmark-desktop-narrow
      ;; icicle-bookmark-dired
      ;; icicle-bookmark-dired-narrow
      ;; icicle-bookmark-dired-this-dir-narrow
      ;; icicle-bookmark-dired-wildcards-narrow
      ;; icicle-bookmark-dired-other-window
      ;; icicle-bookmarked-buffer-list
      ;; icicle-bookmarked-file-list
      ;; icicle-bookmark-file
      ;; icicle-bookmark-file-all-tags
      ;; icicle-bookmark-file-all-tags-other-window
      ;; icicle-bookmark-file-all-tags-regexp
      ;; icicle-bookmark-file-all-tags-regexp-other-window
      ;; icicle-bookmark-file-other-window
      ;; icicle-bookmark-file-narrow
      ;; icicle-bookmark-file-some-tags
      ;; icicle-bookmark-file-some-tags-other-window
      ;; icicle-bookmark-file-some-tags-regexp
      ;; icicle-bookmark-file-some-tags-regexp-other-window
      ;; icicle-bookmark-file-this-dir
      ;; icicle-bookmark-file-this-dir-other-window
      ;; icicle-bookmark-file-this-dir-all-tags
      ;; icicle-bookmark-file-this-dir-all-tags-other-window
      ;; icicle-bookmark-file-this-dir-all-tags-regexp
      ;; icicle-bookmark-file-this-dir-all-tags-regexp-other-window
      ;; icicle-bookmark-file-this-dir-narrow
      ;; icicle-bookmark-file-this-dir-some-tags
      ;; icicle-bookmark-file-this-dir-some-tags-other-window
      ;; icicle-bookmark-file-this-dir-some-tags-regexp
      ;; icicle-bookmark-file-this-dir-some-tags-regexp-other-window
      ;; icicle-bookmark-gnus
      ;; icicle-bookmark-gnus-narrow
      ;; icicle-bookmark-gnus-other-window
      ;; icicle-bookmark-image
      ;; icicle-bookmark-image-narrow
      ;; icicle-bookmark-image-other-window
      ;; icicle-bookmark-info
      ;; icicle-bookmark-info-narrow
      ;; icicle-bookmark-info-other-window
      ;; icicle-bookmark-jump
      ;; icicle-bookmark-jump-other-window
      ;; icicle-bookmark-list
      ;; icicle-bookmark-local-file
      ;; icicle-bookmark-local-file-narrow
      ;; icicle-bookmark-local-file-other-window
      ;; icicle-bookmark-man
      ;; icicle-bookmark-man-narrow
      ;; icicle-bookmark-man-other-window
      ;; icicle-bookmark-navlist-narrow
      ;; icicle-bookmark-non-file
      ;; icicle-bookmark-non-file-narrow
      ;; icicle-bookmark-non-file-other-window
      ;; icicle-bookmark-other-window
      ;; icicle-bookmark-region
      ;; icicle-bookmark-region-narrow
      ;; icicle-bookmark-region-other-window
      ;; icicle-bookmark-remote-file
      ;; icicle-bookmark-remote-file-narrow
      ;; icicle-bookmark-remote-file-other-window
      ;; icicle-bookmark-save-marked-files
      ;; icicle-bookmark-save-marked-files-as-project
      ;; icicle-bookmark-save-marked-files-more
      ;; icicle-bookmark-save-marked-files-persistently
      ;; icicle-bookmark-save-marked-files-to-variable
      ;; icicle-bookmark-set
      ;; icicle-bookmark-some-tags
      ;; icicle-bookmark-some-tags-other-window
      ;; icicle-bookmark-some-tags-regexp
      ;; icicle-bookmark-some-tags-regexp-other-window
      ;; icicle-bookmark-specific-buffers
      ;; icicle-bookmark-specific-buffers-narrow
      ;; icicle-bookmark-specific-buffers-other-window
      ;; icicle-bookmark-specific-files
      ;; icicle-bookmark-specific-files-narrow
      ;; icicle-bookmark-specific-files-other-window
      ;; icicle-bookmark-temporary
      ;; icicle-bookmark-temporary-narrow
      ;; icicle-bookmark-temporary-other-window
      ;; icicle-bookmark-this-buffer
      ;; icicle-bookmark-this-buffer-narrow
      ;; icicle-bookmark-this-buffer-other-window
      ;; icicle-bookmark-url
      ;; icicle-bookmark-url-narrow
      ;; icicle-bookmark-url-other-window
      ;; icicle-bookmark-w3m
      ;; icicle-bookmark-w3m-narrow
      ;; icicle-bookmark-w3m-other-window
      icicle-buffer
      icicle-buffer-config
      icicle-buffer-list
      icicle-buffer-no-search
      icicle-buffer-no-search-other-window
      icicle-buffer-other-window
      icicle-cd-for-abs-files
      icicle-cd-for-loc-files
      icicle-choose-window-by-name
      icicle-choose-window-for-buffer-display
      icicle-clear-history
      icicle-clear-current-history
      icicle-color-theme
      icicle-comint-dynamic-complete
      icicle-comint-dynamic-complete-filename
      icicle-comint-replace-by-expanded-filename
      icicle-command-abbrev
      icicle-command-abbrev-command
      ;; icicle-completing-yank
      ;; icicle-customize-apropos
      ;; icicle-customize-apropos-faces
      ;; icicle-customize-apropos-groups
      ;; icicle-customize-apropos-options
      ;; icicle-customize-apropos-options-of-type
      ;; icicle-customize-apropos-opts-w-val-satisfying
      ;; icicle-customize-face
      ;; icicle-customize-face-other-window
      ;; icicle-customize-icicles-group
      ;; icicle-custom-theme
      ;; icicle-dabbrev-completion
      icicle-delete-file
      icicle-delete-window
      icicle-describe-option-of-type
      icicle-describe-process
      icicle-describe-var-w-val-satisfying
      icicle-delete-windows
      icicle-directory-list
      icicle-dired
      icicle-dired-chosen-files
      icicle-dired-chosen-files-other-window
      icicle-dired-insert-as-subdir
      icicle-dired-other-window
      icicle-dired-project
      icicle-dired-project-other-window
      icicle-dired-saved-file-candidates
      icicle-dired-saved-file-candidates-other-window
      icicle-dired-save-marked
      icicle-dired-save-marked-as-project
      icicle-dired-save-marked-more
      icicle-dired-save-marked-more-recursive
      icicle-dired-save-marked-persistently
      icicle-dired-save-marked-recursive
      icicle-dired-save-marked-to-cache-file-recursive
      icicle-dired-save-marked-to-fileset-recursive
      icicle-dired-save-marked-to-variable
      icicle-dired-save-marked-to-variable-recursive
      icicle-execute-extended-command
      icicle-execute-named-keyboard-macro
      icicle-face-list
      icicle-file
      icicle-file-list
      icicle-file-other-window
      icicle-find-file
      icicle-find-file-abs-no-search
      icicle-find-file-abs-no-search-other-window
      icicle-find-file-abs-of-content
      icicle-find-file-abs-of-content-other-window
      icicle-find-file-absolute
      icicle-find-file-absolute-other-window
      icicle-find-file-abs-read-only
      icicle-find-file-abs-read-only-other-window
      icicle-find-file-in-tags-table
      icicle-find-file-in-tags-table-other-window
      icicle-find-file-of-content
      icicle-find-file-of-content-in-tags-table
      icicle-find-file-of-content-in-tags-table-other-window
      icicle-find-file-of-content-other-window
      icicle-find-file-other-window
      icicle-find-file-no-search
      icicle-find-file-no-search-in-tags-table
      icicle-find-file-no-search-in-tags-table-other-window
      icicle-find-file-no-search-other-window
      icicle-find-file-read-only
      icicle-find-file-read-only-other-window
      icicle-find-first-tag
      icicle-find-first-tag-other-window
      icicle-find-tag
      icicle-grep-saved-file-candidates
      icicle-gud-gdb-complete-command
      icicle-increment-option
      icicle-increment-variable
      icicle-insert-buffer
      icicle-keyword-list
      icicle-kill-buffer
      icicle-kmacro
      icicle-lisp-complete-symbol
      icicle-locate
      icicle-locate-file
      icicle-locate-file-no-search
      icicle-locate-file-no-search-no-symlinks
      icicle-locate-file-no-search-no-symlinks-other-window
      icicle-locate-file-no-search-other-window
      icicle-locate-file-no-symlinks
      icicle-locate-file-no-symlinks-other-window
      icicle-locate-file-of-content
      icicle-locate-file-of-content-no-symlinks
      icicle-locate-file-of-content-no-symlinks-other-window
      icicle-locate-file-of-content-other-window
      icicle-locate-file-other-window
      icicle-locate-other-window
      icicle-locate-no-search
      icicle-locate-no-search-other-window
      icicle-locate-of-content
      icicle-locate-of-content-other-window
      ;; icicle-ORIG-customize-face
      ;; icicle-ORIG-customize-face-other-window
      ;; icicle-ORIG-dabbrev-completion
      ;; icicle-ORIG-lisp-complete-symbol
      ;; icicle-ORIG-lisp-completion-at-point
      ;; icicle-ORIG-repeat-complex-command
      icicle-other-window-or-frame
      icicle-pop-tag-mark
      icicle-pp-eval-expression
      icicle-recent-file
      icicle-recent-file-no-search
      icicle-recent-file-no-search-other-window
      icicle-recent-file-of-content
      icicle-recent-file-of-content-other-window
      icicle-recent-file-other-window
      ;; icicle-recompute-shell-command-candidates
      icicle-regexp-list
      icicle-remove-buffer-candidate
      icicle-remove-buffer-config
      icicle-remove-entry-from-saved-completion-set
      icicle-remove-file-from-recentf-list
      icicle-remove-saved-completion-set
      icicle-repeat-complex-command
      icicle-reset-option-to-nil
      ;; icicle-select-bookmarked-region
      icicle-select-frame
      icicle-select-frame-by-name
      icicle-select-window
      icicle-select-window-by-name
      icicle-send-bug-report
      icicle-send-signal-to-process
      icicle-set-option-to-t
      icicle-sexp-list
      ;; icicle-shell-dynamic-complete-command
      ;; icicle-shell-dynamic-complete-environment-variable
      ;; icicle-shell-dynamic-complete-filename
      icicle-string-list
      icicle-toggle-option
      icicle-visit-marked-file-of-content
      icicle-visit-marked-file-of-content-other-window
      icicle-visit-marked-file-of-content-recursive
      icicle-visit-marked-file-of-content-recursive-other-window
      icicle-widget-file-complete
      icicle-yank-maybe-completing
      icicle-yank-pop-commands
      ;; icicle-zap-to-char
      icicle-anything
      icicle-apply
      ;; icicle-bookmark-a-file
      ;; icicle-bookmark-tagged
      ;; icicle-bookmark-tagged-other-window
      icicle-choose-faces
      icicle-choose-invisible-faces
      icicle-choose-visible-faces
      icicle-comint-command
      icicle-comint-search
      icicle-compilation-search
      icicle-complete
      icicle-complete-keys
      icicle-complete-menu-bar
      icicle-complete-thesaurus-entry
      icicle-describe-package
      icicle-doc
      icicle-exchange-point-and-mark
      icicle-find-file-all-tags
      icicle-find-file-all-tags-other-window
      icicle-find-file-all-tags-regexp
      icicle-find-file-all-tags-regexp-other-window
      ;; icicle-find-file-handle-bookmark
      ;; icicle-find-file-handle-bookmark-other-window
      icicle-find-file-some-tags
      icicle-find-file-some-tags-other-window
      icicle-find-file-some-tags-regexp
      icicle-find-file-some-tags-regexp-other-window
      icicle-find-file-tagged
      icicle-find-file-tagged-other-window
      icicle-font
      icicle-font-lock-keyword
      icicle-frame-bg
      icicle-frame-fg
      icicle-fundoc
      icicle-goto-global-marker
      icicle-goto-global-marker-or-pop-global-mark
      icicle-goto-marker
      icicle-goto-marker-or-set-mark-command
      icicle-hide-faces
      icicle-hide-only-faces
      icicle-hide/show-comments
      icicle-imenu
      icicle-imenu-command
      icicle-imenu-command-full
      icicle-imenu-face
      icicle-imenu-face-full
      icicle-imenu-full
      icicle-imenu-key-explicit-map
      icicle-imenu-key-explicit-map-full
      icicle-imenu-key-implicit-map
      icicle-imenu-key-implicit-map-full
      icicle-imenu-macro
      icicle-imenu-macro-full
      icicle-imenu-non-interactive-function
      icicle-imenu-non-interactive-function-full
      icicle-imenu-user-option
      icicle-imenu-user-option-full
      icicle-imenu-variable
      icicle-imenu-variable-full
      icicle-ido-like-mode
      ;; icicle-Info-goto-node
      ;; icicle-Info-goto-node-no-search
      ;; icicle-Info-goto-node-of-content
      ;; icicle-Info-index
      ;; icicle-Info-index-20
      ;; icicle-Info-menu
      ;; icicle-Info-menu-cmd
      ;; icicle-Info-virtual-book
      icicle-insert-thesaurus-entry
      icicle-load-library
      icicle-map
      icicle-next-font-lock-keywords
      icicle-next-font-lock-keywords-repeat
      icicle-next-visible-thing
      icicle-non-whitespace-string-p
      icicle-object-action
      icicle-occur
      icicle-occur-dired-marked
      icicle-occur-dired-marked-recursive
      icicle-pick-color-by-name
      icicle-plist
      icicle-previous-visible-thing
      icicle-read-color
      icicle-read-color-WYSIWYG
      icicle-save-string-to-variable
      icicle-search
      ;; icicle-search-all-tags-bookmark
      ;; icicle-search-all-tags-regexp-bookmark
      ;; icicle-search-autofile-bookmark
      ;; icicle-search-autonamed-bookmark
      ;; icicle-search-bookmark
      ;; icicle-search-bookmark-list-bookmark
      ;; icicle-search-bookmark-list-marked
      ;; icicle-search-bookmarks-together
      icicle-search-buffer
      icicle-search-buff-menu-marked
      icicle-search-char-property
      icicle-search-defs
      icicle-search-defs-full
      ;; icicle-search-dired-bookmark
      icicle-search-dired-marked
      icicle-search-dired-marked-recursive
      icicle-search-file
      ;; icicle-search-file-bookmark
      icicle-search-generic
      ;; icicle-search-gnus-bookmark
      icicle-search-highlight-cleanup
      icicle-search-ibuffer-marked
      ;; icicle-search-info-bookmark
      icicle-search-keywords
      icicle-search-lines
      ;; icicle-search-local-file-bookmark
      ;; icicle-search-man-bookmark
      ;; icicle-search-non-file-bookmark
      icicle-search-overlay-property
      icicle-search-paragraphs
      icicle-search-pages
      ;; icicle-search-region-bookmark
      ;; icicle-search-remote-file-bookmark
      icicle-search-sentences
      ;; icicle-search-some-tags-bookmark
      ;; icicle-search-some-tags-regexp-bookmark
      ;; icicle-search-specific-buffers-bookmark
      ;; icicle-search-specific-files-bookmark
      ;; icicle-search-temporary-bookmark
      icicle-search-text-property
      icicle-search-thing
      ;; icicle-search-this-buffer-bookmark
      ;; icicle-search-url-bookmark
      icicle-search-w-isearch-string
      ;; icicle-search-w3m-bookmark
      icicle-search-word
      icicle-search-xml-element
      icicle-search-xml-element-text-node
      icicle-select-frame
      icicle-select-frame-by-name
      icicle-select-text-at-point
      icicle-set-S-TAB-methods-for-command
      icicle-set-TAB-methods-for-command
      icicle-show-faces
      icicle-show-only-faces
      ;; icicle-synonyms
      icicle-tag-a-file
      icicle-tags-search
      icicle-untag-a-file
      icicle-vardoc
      ;; icicle-where-is
      ;; icicle-wide-n
      )))

(global-set-key (kbd "C-:") 'icicle-pp-eval-expression)
(global-set-key (kbd "<backtab>") 'icicle-complete-keys)
(define-key evil-normal-state-map (kbd "<backtab>") 'icicle-complete-keys)
(define-key evil-insert-state-map (kbd "<backtab>") 'icicle-complete-keys)

(provide 'config-icicles)
