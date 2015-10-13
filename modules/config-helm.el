;; -*- lexical-binding: t -*-

(eval-when-compile
  (with-demoted-errors "Load error: %s"
    (require 'cl-lib)
    (require 'helm)
    (require 'helm-semantic)
    (require 'helm-imenu)
    (require 'helm-command)
    (require 'helm-mode)))

(defvar helm-flx-cache nil)

(with-eval-after-load 'helm-files
  (eval-when-compile
    (require 'helm-files))

  (setq helm-ff-transformer-show-only-basename nil
        helm-ff-newfile-prompt-p nil
        helm-ff-skip-boring-files t
        helm-boring-file-regexp-list (append helm-boring-file-regexp-list
                                             '("/\\.$"
                                               "/\\.\\.$"
                                               "\\.undo\\.xz$"
                                               "\\.elc$"
                                               "\\#$"
                                               "\\~$"
                                               "\\.zwc\\.old$"
                                               "\\.zwc$"))))

(defun my/helm-fuzzy-matching-sort-fn (candidates _source &optional use-real)
  (require 'flx)
  (if (string= helm-pattern "")
      candidates
    (mapcar #'car
            (sort (mapcar
                   (lambda (cand)
                     (cons cand (or
                                 (car (flx-score
                                       (if (consp cand)
                                           (if use-real
                                               (cdr cand)
                                             (car cand))
                                         cand)
                                       helm-pattern helm-flx-cache))
                                 0)))
                   candidates)
                  (lambda (s1 s2)
                    (> (cdr s1)
                       (cdr s2)))))))

(defun my/helm-fuzzy-highlight-match (candidate)
  (require 'flx)
  (let* ((pair (and (consp candidate) candidate))
         (display (if pair (car pair) candidate))
         (real (cdr pair)))
    (with-temp-buffer
      (insert display)
      (goto-char (point-min))
      (if (string-match-p " " helm-pattern)
          (dolist (p (split-string helm-pattern))
            (when (search-forward p nil t)
              (add-text-properties
               (match-beginning 0) (match-end 0) '(face helm-match))))
        (dolist (index (cdr (flx-score
                             (substring-no-properties display)
                             helm-pattern helm-flx-cache)))
          (with-demoted-errors
              (add-text-properties
               (1+ index) (+ 2 index) '(face helm-match)))))
      (setq display (buffer-string)))
    (if real (cons display real) display)))

(setq helm-fuzzy-sort-fn #'my/helm-fuzzy-matching-sort-fn
      helm-fuzzy-matching-highlight-fn #'my/helm-fuzzy-highlight-match
      helm-semantic-fuzzy-match t
      helm-imenu-fuzzy-match t
      helm-M-x-fuzzy-match t
      helm-mode-fuzzy-match t
      helm-buffers-fuzzy-matching t
      helm-projectile-fuzzy-match t
      helm-recentf-fuzzy-match t
      helm-completion-in-region-fuzzy-match t
      helm-case-fold-search 'smart)

(with-eval-after-load 'helm
  (with-eval-after-load 'flx
    (setq helm-flx-cache (flx-make-string-cache #'flx-get-heatmap-file)))

  ;; swap C-z (i.e. accept-and-complete) with tab (i.e. select action)
  (define-key helm-map (kbd "<tab>") #'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-i")   #'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-z")   #'helm-select-action)
  (define-key helm-map (kbd "C-r" )  #'isearch-backward-regexp)
  (define-key helm-map (kbd "C-'")   #'ace-jump-helm-line-execute-action)

  (set-face-attribute 'helm-selection nil :underline nil))

(with-eval-after-load 'helm-buffers
  (setq helm-boring-buffer-regexp-list '("\\ "
                                         "\\*helm"
                                         "\\*Compile"
                                         "\\*Quail")))

(with-eval-after-load 'helm-locate
  (setq helm-locate-fuzzy-match nil
        helm-source-locate
        (helm-make-source "Locate" 'helm-locate-source
          :pattern-transformer 'helm-locate-pattern-transformer
          :candidate-number-limit 100)
        helm-locate-command "locate %s -r %s -be -l 100"))

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

(defun my/helm-omni (&rest _args)
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

  (let ((helm-sources-using-default-as-input)
        (bufs (list (buffer-name (current-buffer))))
        (projectile-root (ignore-errors (projectile-project-p)))
        (file-remote (and buffer-file-name
                          (file-remote-p default-directory))))

    (helm-attrset 'moccur-buffers bufs helm-source-occur)
    (helm-set-local-variable 'helm-multi-occur-buffer-list bufs)
    (helm-set-local-variable 'helm-multi-occur-buffer-tick
                             (mapcar (lambda (buf)
                                       (buffer-chars-modified-tick
                                        (get-buffer buf)))
                                     bufs))
    (helm :sources
          (append
           ;; projectile explodes when not in project
           (if projectile-root
               (when (require 'helm-projectile nil t)
                 '(helm-source-projectile-buffers-list))
             '(helm-source-buffers-list))

           (if (and (featurep 'semantic)
                    (semantic-active-p)
                    (require 'helm-semantic nil t))
               '(helm-source-semantic)
             (when (require 'helm-imenu nil t)
               '(helm-source-imenu)))

           (if projectile-root
               (append
                '(helm-source-projectile-recentf-list
                  helm-source-recentf)
                (unless file-remote
                  '(helm-source-projectile-files-list)))
             '(helm-source-recentf))

           '(;; files
             helm-source-files-in-current-dir
             helm-source-find-files
             helm-source-occur

             ;; internal sources
             helm-source-kill-ring
             helm-source-mark-ring
             helm-source-global-mark-ring)

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

(eval-when-compile
  (with-demoted-errors "Load error: %s"
    (require 'evil)))

(defun nadvice/evil-paste-pop (old-fun &rest args)
  (if (memq last-command '(evil-paste-after
                           evil-paste-before
                           evil-visual-paste))
      (apply old-fun args)
    (call-interactively #'my/helm-omni)))

(advice-add 'evil-paste-pop :around #'nadvice/evil-paste-pop)

(define-key evil-insert-state-map (kbd "C-p") #'my/helm-omni)
(define-key evil-motion-state-map (kbd "C-p") #'my/helm-omni)

(global-set-key (kbd "C-c C-o") #'my/helm-omni)
(define-key evil-normal-state-map (kbd "C-c C-o") #'my/helm-omni)
(define-key evil-insert-state-map (kbd "C-c C-o") #'my/helm-omni)
(define-key evil-emacs-state-map (kbd "C-c C-o") #'my/helm-omni)
(define-key evil-motion-state-map (kbd "C-c C-o") #'my/helm-omni)
(define-key evil-replace-state-map (kbd "C-c C-o") #'my/helm-omni)

(global-set-key (kbd "M-=") #'helm-semantic-or-imenu)
(global-set-key (kbd "M-:") #'helm-eval-expression)

(global-set-key (kbd "C-x C-b") #'helm-buffers-list)
(global-set-key (kbd "C-x C-f") #'helm-find-files)

(provide 'config-helm)
