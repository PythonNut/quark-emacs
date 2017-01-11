;; -*- lexical-binding: t -*-
(require 'cl-lib)

(eval-when-compile
  (with-demoted-errors "Load error: %s"
    (require 'helm)))

(with-eval-after-load 'helm-flx
  (eval-when-compile
    (require 'helm-flx))
  (setq helm-flx-for-helm-locate t))

(with-eval-after-load 'helm
  (helm-flx-mode +1)

  ;; swap C-z (i.e. accept-and-complete) with tab (i.e. select action)
  (define-key helm-map (kbd "<tab>") #'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-i")   #'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-z")   #'helm-select-action)
  (define-key helm-map (kbd "C-r" )  #'isearch-backward-regexp)
  (define-key helm-map (kbd "C-'")   #'ace-jump-helm-line-execute-action)

  (set-face-attribute 'helm-selection nil :underline nil)
  (setq helm-case-fold-search 'smart
        helm-candidate-separator (make-string 20 ?─)
        helm-inherit-input-method nil))

(with-eval-after-load 'helm-mode
  (eval-when-compile
    (require 'helm-mode))
  (setq helm-mode-fuzzy-match t
        helm-completion-in-region-fuzzy-match t))

(with-eval-after-load 'helm-files
  (eval-when-compile
    (require 'helm-files))
  (setq helm-ff-transformer-show-only-basename nil
        helm-ff-newfile-prompt-p nil
        helm-ff-skip-boring-files t
        helm-recentf-fuzzy-match t

        helm-source-recentf (helm-make-source "Recentf"
                                'helm-recentf-source
                              :fuzzy-match helm-recentf-fuzzy-match)

        helm-boring-file-regexp-list (append helm-boring-file-regexp-list
                                             (list (rx "/." line-end)
                                                   (rx "/.." line-end)
                                                   (rx ".undo.xz" line-end)
                                                   (rx ".elc" line-end)
                                                   (rx "#" line-end)
                                                   (rx "~" line-end)
                                                   (rx ".zwc.old" line-end)
                                                   (rx ".zwc" line-end)))))

(with-eval-after-load 'helm-buffers
  (eval-when-compile
    (require 'helm-buffers))
  (setq helm-buffers-fuzzy-matching t
        helm-boring-buffer-regexp-list (list (rx " ")
                                             (rx "*helm")
                                             (rx "*Compile")
                                             (rx "*Quail"))))

(with-eval-after-load 'helm-semantic
  (eval-when-compile
    (require 'helm-semantic))
  (setq helm-semantic-fuzzy-match t
        helm-source-semantic (helm-make-source "Semantic Tags"
                                 'helm-semantic-source
                               :fuzzy-match helm-semantic-fuzzy-match)))

(with-eval-after-load 'helm-imenu
  (eval-when-compile
    (require 'helm-imenu))
  (setq helm-imenu-fuzzy-match t
        helm-source-imenu (helm-make-source "Imenu"
                              'helm-imenu-source
                            :fuzzy-match helm-imenu-fuzzy-match)))

(with-eval-after-load 'helm-command
  (eval-when-compile
    (require 'helm-command))
  (setq helm-M-x-fuzzy-match t))

(with-eval-after-load 'helm-projectile
  (eval-when-compile
    (require 'helm-projectile))
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
                  (when (boundp 'recentf-list)
                    (let ((project-root (projectile-project-root)))
                      (cl-remove-if
                       (lambda (f) (string-prefix-p project-root f))
                       recentf-list)))
                (error nil)))
      :fuzzy-match helm-projectile-fuzzy-match
      :coerce 'helm-projectile-coerce-file
      :keymap helm-projectile-find-file-map
      :help-message 'helm-ff-help-message
      :mode-line helm-read-file-name-mode-line-string
      :action helm-projectile-file-actions
      :persistent-action #'helm-projectile-file-persistent-action)
    "Helm source definition for recent files not in current project."))

(with-eval-after-load 'helm-locate
  (eval-when-compile
    (require 'helm-locate))
  (setq helm-locate-fuzzy-match t
        helm-source-locate
        (helm-make-source "Locate" 'helm-locate-source
          :pattern-transformer 'helm-locate-pattern-transformer
          :candidate-number-limit 100)
        helm-locate-command "locate %s -r %s -e -l 100"))

;; adaptively fallback to ack and ack-grep
(with-eval-after-load 'helm-ag
  (eval-when-compile
    (require 'helm-ag))

  (unless (executable-find "ag")
    (if (executable-find "ack")
        (setq helm-ag-base-command "ack --nocolor --nogroup")
      (when (executable-find "ack-grep")
        (setq helm-ag-base-command "ack-grep --nocolor --nogroup")))))

(with-eval-after-load 'helm-regexp
  (helm-occur-init-source))

(defun my/helm-interfile-omni (&rest _args)
  (interactive)
  (require 'helm-files)
  (require 'helm-ring)
  (require 'helm-misc)
  (require 'projectile)

  (unless (featurep 'helm-ag)
    (when (or (executable-find "ag")
              (executable-find "ack")
              (executable-find "ack-grep"))
      (require 'helm-ag)))

  (require 'recentf)
  (when helm-turn-on-recentf (recentf-mode 1))

  (let ((helm-sources-using-default-as-input)
        (projectile-root (ignore-errors (projectile-project-p)))
        (file-remote (and buffer-file-name
                          (file-remote-p default-directory))))

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
                (unless file-remote
                  '(helm-source-projectile-files-list)))
             '(helm-source-recentf))

           '(;; files
             helm-source-files-in-current-dir
             helm-source-find-files)

           ;; disable expensve helm sources when using TRAMP
           (unless file-remote
             (append
              ;; code search
              (if (and projectile-root
                       (featurep 'vc-git)
                       (vc-git-responsible-p projectile-root)
                       (require 'helm-git-grep nil t))
                  '(helm-source-git-grep)
                (when (featurep 'helm-ag)
                  '(helm-source-do-ag)))

              ;; file location, of which projectile is a faster subset
              (unless projectile-root
                '(helm-source-findutils))

              '(helm-source-locate))))

          :fuzzy-match t
          :prompt (if projectile-root
                      (format "[%s] > " (projectile-project-name))
                    "> ")
          :buffer "*helm-omni*")))

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

(eval-when-compile
  (with-demoted-errors "Load error: %s"
    (require 'evil)))

(defun nadvice/evil-paste-pop (old-fun &rest args)
  (if (memq last-command '(evil-paste-after
                           evil-paste-before
                           evil-visual-paste))
      (apply old-fun args)
    (call-interactively #'my/helm-interfile-omni)))

(advice-add 'evil-paste-pop :around #'nadvice/evil-paste-pop)

(define-key evil-insert-state-map (kbd "C-p") #'my/helm-interfile-omni)
(define-key evil-motion-state-map (kbd "C-p") #'my/helm-interfile-omni)

(global-set-key (kbd "C-c C-o") #'my/helm-interfile-omni)

(global-set-key (kbd "M-:") #'helm-eval-expression)
(global-set-key (kbd "M-p") #'my/helm-intrafile-omni)

(global-set-key (kbd "C-x C-b") #'helm-buffers-list)
(global-set-key (kbd "C-x C-f") #'helm-find-files)

(provide 'config-helm)
