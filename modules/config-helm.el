;; -*- lexical-binding: t -*-
(eval-when-compile (require 'config-macros))

(use-package helm-flx
  :config
  (setq helm-flx-for-helm-locate t))

(use-package helm-flx-historian
  :recipe (helm-flx-historian :type git :host github :repo "PythonNut/historian.el"))

(use-package ace-jump-helm-line)

(use-package helm
  :config
  (helm-flx-mode +1)
  (helm-flx-historian-mode +1)

  ;; swap C-z (i.e. accept-and-complete) with tab (i.e. select action)
  (define-key helm-map (kbd "<tab>")     #'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-i")       #'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-z")       #'helm-select-action)
  (define-key helm-map (kbd "C-r" )      #'isearch-backward-regexp)
  (define-key helm-map (kbd "C-'")       #'ace-jump-helm-line-execute-action)
  (define-key helm-map (kbd "<left>")    #'backward-char)
  (define-key helm-map (kbd "<right>")   #'forward-char)
  (define-key helm-map (kbd "M-<left>")  #'helm-previous-source)
  (define-key helm-map (kbd "M-<right>") #'helm-next-source)

  (set-face-attribute 'helm-selection nil :underline nil)
  (setq helm-case-fold-search 'smart
        helm-candidate-separator (make-string 20 ?─)
        helm-inherit-input-method nil))

(use-package helm-mode
  :ensure nil
  :config
  (setq helm-mode-fuzzy-match t
        helm-completion-in-region-fuzzy-match t))

(setq helm-recentf-fuzzy-match t)
(use-package helm-files
  :ensure nil
  :config
  (let (fasd-env-cache)
    (defun my/fasd-execute (&rest args)
      (require 's)
      (require 'exec-path-from-shell)
      (s-trim
       (let* ((env (or fasd-env-cache
                       (setq fasd-env-cache
                             (exec-path-from-shell-getenvs
                              (list "PATH" "_FASD_DATA" "_FASD_FUZZY")))))
              (exec-path (parse-colon-path (cdr (assoc "PATH" env))))
              (fasd-path (executable-find "fasd"))
              (process-environment process-environment))
         (cl-assert fasd-path nil "fasd not found!")
         (add-to-list 'process-environment
                      (format "_FASD_DATA=%s" (cdr (assoc "_FASD_DATA" env))))
         (add-to-list 'process-environment
                      (format "_FASD_FUZZY=%s" (cdr (assoc "_FASD_FUZZY" env))))
         (with-output-to-string
           (with-current-buffer
               standard-output
             (apply #'process-file fasd-path nil t nil args))))))

    (defun helm-fasd-init ()
      (require 'exec-path-from-shell)
      "Initialize async process for `helm-source-fasd'."
      (let* ((env (or fasd-env-cache
                      (setq fasd-env-cache
                            (exec-path-from-shell-getenvs
                             (list "PATH" "_FASD_DATA" "_FASD_FUZZY")))))
             (exec-path (parse-colon-path (cdr (assoc "PATH" env))))
             (fasd-path (executable-find "fasd"))
             (process-environment process-environment)
             (cmd (concat fasd-path " -R -l -d " (shell-quote-argument helm-pattern))))
        (cl-assert fasd-path nil "fasd not found!")
        (add-to-list 'process-environment
                     (format "_FASD_DATA=%s" (cdr (assoc "_FASD_DATA" env))))
        (add-to-list 'process-environment
                     (format "_FASD_FUZZY=%s" (cdr (assoc "_FASD_FUZZY" env))))
        (helm-log "Starting helm-fasd process")
        (helm-log "Command line used was:\n\n%s"
                  (concat ">>> " (propertize cmd 'face 'font-lock-comment-face) "\n\n"))
        (prog1
            (start-process
             "fasd-process" helm-buffer
             fasd-path "-R" "-l" "-d" helm-pattern)
          (set-process-sentinel
           (get-buffer-process helm-buffer)
           #'(lambda (_process event)
               (if (string= event "finished\n")
                   (with-helm-window
                     (setq mode-line-format
                           '(" " mode-line-buffer-identification " "
                             (:eval (format "L%s" (helm-candidate-number-at-point))) " "
                             (:eval (propertize
                                     (format "[fasd process finished - (%s results)]"
                                             (max (1- (count-lines
                                                       (point-min) (point-max)))
                                                  0))
                                     'face 'helm-locate-finish))))
                     (force-mode-line-update))
                 (helm-log "Error: Fasd %s"
                           (replace-regexp-in-string "\n" "" event)))))))))

  (defun my/helm-find-files-slash (arg)
    (interactive "p")
    (eval-and-compile (require 's))
    (if (looking-back "~\\([a-zA-Z0-9 ]+\\)" (- (point) 10))
        (let* ((target (match-string 1))
               (result (apply #'my/fasd-execute "-ld1" (s-split " " target))))
          (if (string-empty-p result)
              (self-insert-command arg)
            (beginning-of-line)
            (kill-line)
            (insert (concat result "/"))))
      (if (looking-back "[/~:]" (1- (point)))
          (self-insert-command arg)
        (helm-execute-persistent-action))))

  (define-key helm-find-files-map (kbd "/") #'my/helm-find-files-slash)

  (defclass helm-fasd-source (helm-source-async helm-type-file)
    ((candidates-process :initform 'helm-fasd-init)
     (history :initform 'helm-file-name-history)
     (keymap :initform helm-generic-files-map)
     (help-message :initform helm-generic-file-help-message)
     (candidate-number-limit :initform 9999)
     (mode-line :initform helm-read-file-name-mode-line-string)))

  (defvar helm-source-fasd (helm-make-source "fasd" 'helm-fasd-source))

  (defun helm-fasd ()
    "Helm source for fasd."
    (interactive)
    (require 'helm-mode)
    (require 'helm-for-files)
    (let ((helm-ff-transformer-show-only-basename nil))
      (helm :sources 'helm-source-fasd
            :buffer "*helm fasd*")))

  (setq helm-ff-transformer-show-only-basename nil
        helm-ff-newfile-prompt-p nil
        helm-ff-skip-boring-files t
        helm-recentf-fuzzy-match t

        helm-boring-file-regexp-list (append helm-boring-file-regexp-list
                                             (list (rx ".synctex.gz" line-end)
                                                   (rx ".undo.xz" line-end)
                                                   (rx ".elc" line-end)
                                                   (rx "#" line-end)
                                                   (rx "~" line-end)
                                                   (rx ".zwc.old" line-end)
                                                   (rx ".zwc" line-end)
                                                   (rx "/.#"))))

  (advice-add
   'helm-ff-filter-candidate-one-by-one :around
   (my/defun-as-value nadvice/helm-ff-filter-candidate-one-by-one (old-fun file)
     (when (or (not (string-match-p (rx (or "." "..") line-end) file))
               (string-match-p (rx ".")
                               (helm-basename (or (bound-and-true-p helm-input)
                                                  ""))))
       (funcall old-fun file)))))

(use-package helm-buffers
  :ensure nil
  :config
  (setq helm-buffers-fuzzy-matching t
        helm-boring-buffer-regexp-list (list (rx " ")
                                             (rx "*helm")
                                             (rx "*Compile")
                                             (rx "*Quail"))))

(use-package helm-imenu
  :ensure nil
  :init
  (setq helm-imenu-fuzzy-match t))

(use-package helm-semantic
  :ensure nil
  :init
  (setq helm-semantic-fuzzy-match t))

(use-package helm-command
  :ensure nil
  :config
  (setq helm-M-x-fuzzy-match t))

(use-package helm-projectile
  :init
  (el-patch-feature helm-projectile)

  :config
  (eval-when-compile
    (with-demoted-errors "Load error: %s"
      (require 'el-patch)))

  (setq helm-projectile-fuzzy-match t)

  (defvar my/helm-non-projectile-buffers-list-cache nil)
  (defclass helm-source-non-projectile-buffer (helm-source-sync helm-type-buffer)
    ((init :initform
           (lambda ()
             (setq my/helm-non-projectile-buffers-list-cache
                   (condition-case nil
                       (mapcar #'buffer-name
                               (let* ((project-root (projectile-project-root)))
                                 (cl-remove-if
                                  (lambda (buffer)
                                    (projectile-project-buffer-p buffer
                                                                 project-root))
                                  (buffer-list))))
                     (error nil)))
             (let ((result
                    (cl-loop for b in my/helm-non-projectile-buffers-list-cache
                             maximize (length b) into len-buf
                             maximize (length (with-current-buffer b
                                                (symbol-name major-mode)))
                             into len-mode
                             finally return (cons len-buf len-mode))))
               (unless helm-buffer-max-length
                 (setq helm-buffer-max-length (car result)))
               (unless helm-buffer-max-len-mode
                 ;; If a new buffer is longer that this value
                 ;; this value will be updated
                 (setq helm-buffer-max-len-mode (cdr result))))))
     (candidates :initform my/helm-non-projectile-buffers-list-cache)
     (matchplugin :initform nil)
     (match :initform 'helm-buffers-match-function)
     (persistent-action :initform 'helm-buffers-list-persistent-action)
     (keymap :initform helm-buffer-map)
     (volatile :initform t)
     (persistent-help
      :initform
      "Show this buffer / C-u \\[helm-execute-persistent-action]: Kill this buffer")))

  (defvar helm-source-non-projectile-buffers-list (helm-make-source "Non project buffers" 'helm-source-non-projectile-buffer))

  (defvar helm-source-non-projectile-recentf-list
    (helm-build-in-buffer-source "Other recent files"
      :data (lambda ()
              (condition-case nil
                  (when (bound-and-true-p recentf-list)
                    (if (projectile-project-p)
                        (let ((project-root (projectile-project-root)))
                          (cl-remove-if
                           (lambda (f) (string-prefix-p project-root f))
                           recentf-list))
                      recentf-list))
                (error nil)))
      :fuzzy-match helm-recentf-fuzzy-match
      :pattern-transformer #'helm-recentf-pattern-transformer
      :help-message 'helm-ff-help-message
      :mode-line helm-read-file-name-mode-line-string
      :action helm-projectile-file-actions
      :persistent-action #'helm-ff-kill-or-find-buffer-fname)
    "Helm source definition for recent files not in current project.")

  (el-patch-defvar helm-source-projectile-files-list
    (helm-build-sync-source "Projectile files"
      :before-init-hook (lambda ()
                          (add-hook 'helm-after-update-hook #'helm-projectile--move-to-real)
                          (add-hook 'helm-cleanup-hook #'helm-projectile--remove-move-to-real))
      :candidates (lambda ()
                    (when (projectile-project-p)
                      (with-helm-current-buffer
                        (cl-loop with root = (projectile-project-root)
                                 for display in (projectile-current-project-files)
                                 collect (cons display (expand-file-name display root))))))
      :filtered-candidate-transformer
      (lambda (files _source)
        (with-helm-current-buffer
          (let* ((root (projectile-project-root))
                 (file-at-root (file-relative-name (expand-file-name helm-pattern root))))
            (if (or (string-empty-p helm-pattern)
                    (assoc helm-pattern files))
                files
              (el-patch-swap
                (if (equal helm-pattern file-at-root)
                    (cl-acons (helm-ff-prefix-filename helm-pattern nil t)
                              (expand-file-name helm-pattern)
                              files)
                  (cl-pairlis (list (helm-ff-prefix-filename helm-pattern nil t)
                                    (helm-ff-prefix-filename file-at-root nil t))
                              (list (expand-file-name helm-pattern)
                                    (expand-file-name helm-pattern root))
                              files))
                files)))))
      (el-patch-add :candidate-number-limit 100)
      :fuzzy-match helm-projectile-fuzzy-match
      :keymap helm-projectile-find-file-map
      :help-message 'helm-ff-help-message
      :mode-line helm-read-file-name-mode-line-string
      :action helm-projectile-file-actions
      :persistent-action #'helm-projectile-file-persistent-action
      :persistent-help "Preview file")
    "Helm source definition for Projectile files."))

(use-package helm-locate
  :ensure nil
  :init
  (el-patch-feature helm-locate)

  :config
  (eval-when-compile
    (with-demoted-errors "Load error: %s"
      (require 'el-patch)))

  (cond ((eq system-type 'gnu/linux)
         (setq helm-locate-fuzzy-match t
               helm-locate-command "locate %s -r %s -e -l 100"))
        ((eq system-type 'darwin)
         (setq helm-locate-fuzzy-match nil
               helm-locate-command "mdfind -name %s %s")))

  (el-patch-defvar helm-source-locate
    (helm-make-source "Locate" 'helm-locate-source
      :pattern-transformer 'helm-locate-pattern-transformer
      ;; :match-part is only used here to tell helm which part
      ;; of candidate to highlight.
      :match-part (lambda (candidate)
                    (if (or (string-match-p " -b\\'" helm-pattern)
                            (and helm-locate-fuzzy-match
                                 (not (string-match "\\s-" helm-pattern))))
                        (helm-basename candidate)
                      candidate))
      (el-patch-add :candidate-number-limit 1000))))

(use-package helm-regexp
  :ensure nil
  :config
  (helm-occur-init-source))

(use-package helm-ag
  :init
  ;; adaptively fallback to ack and ack-grep
  (defvar my/ag-available nil)

  :config
  (cond ((executable-find "ag")
         (setq my/ag-available t))
        ((executable-find "ack")
         (setq helm-ag-base-command "ack --nocolor --nogroup"
               my/ag-available t))
        ((executable-find "ack-grep")
         (setq helm-ag-base-command "ack-grep --nocolor --nogroup"
               my/ag-available t))))

(use-package helm-git-grep)

(use-package helm-rg
  :init
  (defvar my/rg-available nil)

  :config
  (when (executable-find "rg")
    (setq my/rg-available t)))

(defun my/helm-interfile-omni (&rest _args)
  (interactive)
  (require 'helm-files)
  (require 'helm-for-files)
  (require 'helm-find)
  (require 'helm-ring)
  (require 'helm-misc)
  (require 'projectile)
  (require 'recentf)
  (require 'dired)

  (require 'helm-rg)
  (unless my/rg-available
    (require 'helm-ag))

  (when helm-turn-on-recentf (recentf-mode 1))

  (let ((helm-sources-using-default-as-input)
        (projectile-root (ignore-errors (projectile-project-p)))
        (slow-fs (my/slow-fs default-directory))
        (helm-rg--current-dir
         (helm-rg--interpret-starting-dir helm-rg-default-directory))
        (helm-rg--glob-string helm-rg-default-glob-string)
        (helm-rg--case-sensitivity helm-rg-default-case-sensitivity)
        my/rg-used)
    (when projectile-root
      (projectile-maybe-invalidate-cache nil))

    (unwind-protect
        (helm :sources
              (append
               ;; projectile explodes when not in project
               (if projectile-root
                   (when (require 'helm-projectile nil t)
                     '(helm-source-projectile-buffers-list
                       helm-source-non-projectile-buffers-list))
                 '(helm-source-buffers-list))

               (if projectile-root
                   (append
                    '(helm-source-projectile-recentf-list
                      helm-source-non-projectile-recentf-list)
                    (unless slow-fs
                      '(helm-source-projectile-files-list)))
                 '(helm-source-recentf
                   helm-source-files-in-current-dir))

               ;; disable expensive helm sources when using TRAMP
               (unless slow-fs
                 (append
                  ;; code search
                  (if my/rg-available
                      (progn
                        (setq my/rg-used t)
                        '(helm-rg-process-source))
                    (if (and projectile-root
                             (featurep 'vc-git)
                             (vc-git-responsible-p projectile-root)
                             (require 'helm-git-grep))
                        '(helm-source-git-grep)
                      (when my/ag-available
                        '(helm-source-do-ag))))

                  ;; file location, of which projectile is a faster subset
                  (unless projectile-root
                    '(helm-source-findutils))

                  ;; '(helm-source-locate)
                  )))

              :fuzzy-match t
              :prompt (if projectile-root
                          (format "[%s] > " (projectile-project-name))
                        "> ")
              :buffer "*helm-omni*")
      (when my/rg-used
        (helm-rg--unwind-cleanup)))))

(defun my/helm-intrafile-omni (&rest _args)
  (interactive)
  (require 'helm-files)
  (require 'helm-ring)
  (require 'helm-misc)

  (let ((helm-sources-using-default-as-input)
        (bufs (list (buffer-name (current-buffer))))
        (projectile-root (ignore-errors (projectile-project-p))))

    (helm-attrset 'moccur-buffers bufs helm-source-occur)
    (helm-set-local-variable 'helm-multi-occur-buffer-list bufs)
    (helm-set-local-variable 'helm-multi-occur-buffer-tick
                             (mapcar (lambda (buf)
                                       (buffer-chars-modified-tick
                                        (get-buffer buf)))
                                     bufs))

    (helm :sources
          (append
           (if (and (featurep 'semantic)
                    (semantic-active-p)
                    (require 'helm-semantic nil t))
               '(helm-source-semantic)
             (when (require 'helm-imenu nil t)
               '(helm-source-imenu)))

           '(;; files
             helm-source-occur

             ;; internal sources
             helm-source-register
             helm-source-kill-ring
             helm-source-mark-ring
             helm-source-global-mark-ring
             helm-source-regexp
             helm-source-calculation-result))

          :fuzzy-match t
          :prompt (if projectile-root
                      (format "[%s] > " (projectile-project-name))
                    "> ")
          :buffer "*helm-omni*")))

(global-set-key (kbd "C-c C-o") #'my/helm-interfile-omni)

(global-set-key (kbd "M-:") #'helm-eval-expression)
(global-set-key (kbd "C-:") #'pp-eval-expression)
(global-set-key (kbd "M-p") #'my/helm-intrafile-omni)
(global-set-key (kbd "C-=") #'my/helm-intrafile-omni)

(global-set-key (kbd "C-x C-b") #'helm-buffers-list)
(global-set-key (kbd "C-x C-f") #'helm-find-files)

(use-package evil
  :config
  (advice-add
   'evil-paste-pop :around
   (my/defun-as-value nadvice/evil-paste-pop (old-fun &rest args)
     (if (memq last-command '(evil-paste-after
                              evil-paste-before
                              evil-visual-paste))
         (apply old-fun args)
       (call-interactively #'my/helm-interfile-omni))))

  (define-key evil-insert-state-map (kbd "C-p") #'my/helm-interfile-omni)
  (define-key evil-motion-state-map (kbd "C-p") #'my/helm-interfile-omni))

(global-set-key (kbd "C-S-p") #'helm-locate)
(global-set-key (kbd "M-P") #'helm-locate)

(use-package helm-gtags
  :defer-install t
  :commands (helm-gtags-clear-all-cache
             helm-gtags-clear-cache
             helm-gtags-next-history
             helm-gtags-previous-history
             helm-gtags-select
             helm-gtags-select-path
             helm-gtags-tags-in-this-function
             helm-gtags-create-tags
             helm-gtags-delete-tags
             helm-gtags-find-tag
             helm-gtags-find-tag-other-window
             helm-gtags-find-rtag
             helm-gtags-find-symbol
             helm-gtags-find-pattern
             helm-gtags-find-files
             helm-gtags-find-tag-from-here
             helm-gtags-dwim
             helm-gtags-parse-file
             helm-gtags-pop-stack
             helm-gtags-show-stack
             helm-gtags-clear-stack
             helm-gtags-clear-all-stacks
             helm-gtags-update-tags
             helm-gtags-resume
             helm-gtags-mode)

  :init
  ;; Unfortunately, this must be declared at toplevel.
  (setq helm-gtags-fuzzy-match t)

  :config
  (require 'el-patch)
  (setq helm-gtags-auto-update t
        helm-gtags-ignore-case t
        helm-gtags-direct-helm-completing t)

  (el-patch-feature helm-gtags)
  (el-patch-defun helm-gtags--read-tagname (type &optional default-tagname)
    (let ((tagname (helm-gtags--token-at-point type))
          (prompt (assoc-default type helm-gtags--prompt-alist))
          (comp-func (assoc-default type helm-gtags-comp-func-alist)))
      (if (and tagname helm-gtags-use-input-at-cursor)
          tagname
        (when (and (not tagname) default-tagname)
          (setq tagname default-tagname))
        (when tagname
          (setq prompt (format "%s(default \"%s\") " prompt tagname)))
        (let ((completion-ignore-case helm-gtags-ignore-case)
              (completing-read-function 'completing-read-default))
          (if (and helm-gtags-direct-helm-completing (memq type '(tag rtag symbol find-file)))
              (helm-comp-read prompt comp-func
                              (el-patch-remove :history 'helm-gtags--completing-history)
                              :exec-when-only-one t
                              (el-patch-add :fuzzy t)
                              :default tagname)
            (completing-read prompt comp-func nil nil nil
                             'helm-gtags--completing-history tagname)))))))

(use-package helm-systemd
  :defer-install t
  :commands (helm-systemd))

(provide 'config-helm)
