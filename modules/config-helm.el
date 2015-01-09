(eval-when-compile
  (with-demoted-errors
    (require 'cl-lib)
    (require 'evil)
    (require 'helm)
    (require 'helm-ag)
    (require 'helm-config)
    (require 'helm-files)
    (require 'helm-grep)
    (require 'key-chord)
    (require 'projectile)
    (require 'semantic)
    (require 'config-modes)))

(with-eval-after-load 'helm-files
  (setq
    helm-recentf-fuzzy-match t
    helm-locate-fuzzy-match t)

  (setq helm-boring-file-regexp-list
    (append helm-boring-file-regexp-list
      '("\\.undo.xz$"
         "\\.elc$"
         "\\#$"
         "\\~$"
         "\\.zwc.old$"
         "\\.zwc$"))))

(with-eval-after-load 'helm
  (setq
    helm-case-fold-search 'smart
    helm-locate-command "locate %s -r %s -be -l 500"
    helm-ff-transformer-show-only-basename nil
    helm-buffers-fuzzy-matching t
    helm-ff-newfile-prompt-p nil)

  (set-face-attribute 'helm-selection nil :underline nil))

(with-eval-after-load 'helm-buffers
  (unless helm-source-buffers-list
    (setq helm-source-buffers-list
      (helm-make-source "Buffers" 'helm-source-buffers)))
  (setq helm-boring-buffer-regexp-list
    '("\\ "
       "\\*helm"
       "\\*Compile"
       "\\*Quail")))

;; adaptively fallback to ack and ack-grep
(with-eval-after-load 'helm-ag
  (unless (executable-find "ag")
    (if (executable-find "ack")
      (setq helm-ag-base-command "ack --nocolor --nogroup")
      (when (executable-find "ack-grep")
        (setq helm-ag-base-command "ack-grep --nocolor --nogroup")))))

(global-set-key (kbd "M-:") #'helm-eval-expression)

(defun my-helm-buffers (&rest arg)
  (interactive)
  (unless (featurep 'helm-buffers) (require 'helm-buffers))
  (unless (featurep 'helm-files) (require 'helm-files))
  (helm
    :sources
    '(helm-source-buffers-list
       helm-source-recentf
       helm-source-files-in-current-dir
       helm-source-files-in-all-dired
       helm-source-buffer-not-found)
    :fuzzy-match t
    :prompt "> "
    :buffer "*helm-find-buffers"))

(global-set-key (kbd "C-x C-b") #'my-helm-buffers)

(defun my-helm-find-files (&rest arg)
  (interactive)
  (unless (featurep 'helm-buffers) (require 'helm-buffers))
  (unless (featurep 'helm-files) (require 'helm-files))
  (helm
    :sources
    '(helm-source-recentf
       helm-source-buffers-list
       helm-source-files-in-current-dir
       helm-source-find-files
       helm-source-findutils
       helm-source-locate)
    :fuzzy-match t
    :prompt "> "
    :buffer "*helm-find-files"))

(global-set-key (kbd "C-x C-f") 'my-helm-find-files)

(defun my-helm-omni (&rest arg)
  (interactive)
  (unless (featurep 'helm-files) (require 'helm-files))
  (unless (featurep 'helm-ring) (require 'helm-ring))
  (unless (featurep 'helm-misc) (require 'helm-misc))
  (unless (featurep 'helm-semantic) (require 'helm-semantic))
  (unless (featurep 'helm-ag)
    (when (or
            (executable-find "ag")
            (executable-find "ack")
            (executable-find "ack-grep"))
      (require 'helm-ag)))
  (unless (featurep 'helm-projectile)
    (require 'helm-projectile)
    (unless projectile-global-mode-buffers
      (projectile-global-mode +1)))

  (helm-occur-init-source)
  (let ((bufs (list (buffer-name (current-buffer))))
         (projectile-root (ignore-errors (projectile-project-p))))

    (helm-attrset 'moccur-buffers bufs helm-source-occur)
    (helm-set-local-variable 'helm-multi-occur-buffer-list bufs)
    (helm-set-local-variable 'helm-multi-occur-buffer-tick
      (cl-loop for b in bufs
        collect (buffer-chars-modified-tick (get-buffer b))))
    (helm
      :sources
      (append
        ;; projectile explodes when not in project
        (if projectile-root
          '(helm-source-projectile-buffers-list)
          '(helm-source-buffers-list))

        (if (semantic-active-p)
          '(helm-source-semantic)
          '(helm-source-imenu))

        (if projectile-root
          '(helm-source-projectile-recentf-list
             helm-source-recentf
             helm-source-projectile-files-list)
          '(helm-source-recentf))

        '(;; files
           helm-source-files-in-current-dir
           helm-source-find-files
           helm-source-occur

           ;; internal sources
           helm-source-kill-ring
           helm-source-mark-ring
           helm-source-global-mark-ring)

        ;; code search
        (when (featurep 'helm-ag)
          '(helm-source-do-ag))

        ;; file location, of which projectile can be a superset
        (unless projectile-root
          '(helm-source-findutils))

        '(helm-source-locate))

      :fuzzy-match t
      :prompt "> "
      :buffer "*helm-omni*")))

(defadvice evil-paste-pop
  (around auto-helm-omni activate preactivate compile)
  (if (memq last-command
        '(evil-paste-after
           evil-paste-before
           evil-visual-paste))
    ad-do-it
    (call-interactively #'my-helm-omni)))

(define-key evil-insert-state-map (kbd "C-p") #'my-helm-omni)

(defun helm-helm-commands ()
  (interactive)
  (minibuffer-with-setup-hook
    (lambda ()
      (insert "helm-"))
    (call-interactively #'helm-M-x)))


(evil-leader/set-key
  "h" #'helm-helm-commands)

(global-set-key (kbd "C-c C-o") #'my-helm-omni)
(define-key evil-normal-state-map (kbd "C-c C-o") #'my-helm-omni)
(define-key evil-insert-state-map (kbd "C-c C-o") #'my-helm-omni)
(define-key evil-emacs-state-map (kbd "C-c C-o") #'my-helm-omni)
(define-key evil-motion-state-map (kbd "C-c C-o") #'my-helm-omni)
(define-key evil-replace-state-map (kbd "C-c C-o") #'my-helm-omni)

(provide 'config-helm)
