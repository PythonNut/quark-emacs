;; -*- lexical-binding: t -*-

(eval-when-compile
  (with-demoted-errors "Load error: %s"
    (require 'cl-lib)
    (require 'evil)
    (require 'helm)
    (require 'helm-ag)
    (require 'helm-config)
    (require 'helm-files)
    (require 'helm-grep)
    (require 'helm-command)
    (require 'helm-imenu)
    (require 'helm-locate)
    (require 'helm-semantic)
    (require 'key-chord)
    (require 'projectile)
    (require 'semantic)))

(defvar helm-flx-cache)

(with-eval-after-load 'helm-files
  (setq helm-ff-skip-boring-files t
        helm-recentf-fuzzy-match t
        helm-boring-file-regexp-list
        (append helm-boring-file-regexp-list
                '(
                  "/\\.$"
                  "/\\.\\.$"
                  "\\.undo\\.xz$"
                  "\\.elc$"
                  "\\#$"
                  "\\~$"
                  "\\.zwc\\.old$"
                  "\\.zwc$"))))

(defun my/helm-fuzzy-matching-sort-fn (candidates _source &optional use-real)
  (if (string= helm-pattern "")
      candidates
    (cl-letf* ((table-scr (make-hash-table :test 'equal))
               ((symbol-function 'score-cand)
                (lambda  (cand)
                  (setq cand
                        (if (consp cand)
                            (if use-real (cdr cand) (car cand))
                          cand))
                  (or
                   (gethash cand table-scr)
                   (puthash cand
                            (or
                             (car (flx-score
                                   (substring-no-properties cand)
                                   (substring-no-properties helm-pattern)
                                   helm-flx-cache))
                             0)
                            table-scr)))))
      (sort candidates
            (lambda (s1 s2)
              (>
               (score-cand s1)
               (score-cand s2)))))))

(defun my/helm-fuzzy-highlight-match (candidate)
  (let* ((pair (and (consp candidate) candidate))
         (display (if pair (car pair) candidate))
         (real (cdr pair)))
    (with-temp-buffer
      (insert display)
      (goto-char (point-min))
      (if (string-match-p " " helm-pattern)
          (cl-loop with pattern = (split-string helm-pattern)
                   for p in pattern
                   do (when (search-forward (substring-no-properties p) nil t)
                        (add-text-properties
                         (match-beginning 0) (match-end 0) '(face helm-match))))
        (cl-loop with pattern = (cdr (flx-score
                                      (substring-no-properties display)
                                      helm-pattern helm-flx-cache))
                 for index in pattern
                 do (add-text-properties
                     (1+ index) (+ 2 index) '(face helm-match))))
      (setq display (buffer-string)))
    (if real (cons display real) display)))

(setq helm-fuzzy-sort-fn #'my/helm-fuzzy-matching-sort-fn
      helm-fuzzy-matching-highlight-fn #'my/helm-fuzzy-highlight-match

      helm-M-x-fuzzy-match t
      helm-recentf-fuzzy-match t
      helm-buffers-fuzzy-matching t
      helm-completion-in-region-fuzzy-match t
      helm-imenu-fuzzy-match t
      helm-locate-fuzzy-match nil
      helm-mode-fuzzy-match t
      helm-semantic-fuzzy-match t

      helm-case-fold-search 'smart
      helm-ff-transformer-show-only-basename nil
      helm-ff-newfile-prompt-p nil)

(with-eval-after-load 'helm
  ;; swap C-z (i.e. accept-and-complete) with tab (i.e. select action)
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-z")  'helm-select-action)

  (require 'flx)

  (setq helm-flx-cache (flx-make-string-cache #'flx-get-heatmap-file))
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

(with-eval-after-load 'helm-locate
  (setq helm-source-locate
        (helm-make-source "Locate" 'helm-locate-source
          :pattern-transformer 'helm-locate-pattern-transformer
          :candidate-number-limit 100)
        helm-locate-command "locate %s -r %s -be -l 100"))

;; adaptively fallback to ack and ack-grep
(with-eval-after-load 'helm-ag
  (unless (executable-find "ag")
    (if (executable-find "ack")
        (setq helm-ag-base-command "ack --nocolor --nogroup")
      (when (executable-find "ack-grep")
        (setq helm-ag-base-command "ack-grep --nocolor --nogroup")))))

(with-eval-after-load 'helm-projectile
  (setq helm-projectile-fuzzy-match t))

(with-eval-after-load 'helm-regexp
  (helm-occur-init-source))

(global-set-key (kbd "M-:") #'helm-eval-expression)

(defun my/helm-buffers (&rest arg)
  (interactive)
  (require 'helm-buffers)
  (require 'helm-files)
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

(global-set-key (kbd "C-x C-b") #'my/helm-buffers)
(global-set-key (kbd "C-x C-f") #'helm-find-files)

(defun my/helm-omni (&rest arg)
  (interactive)
  (require 'helm-files)
  (require 'helm-ring)
  (require 'helm-misc)
  (require 'projectile)

  (let ((helm-sources-using-default-as-input)
        (bufs (list (buffer-name (current-buffer))))
        (projectile-root (ignore-errors (projectile-project-p)))
        (file-remote (and buffer-file-name
                          (file-remote-p default-directory))))

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
          (progn
            (require 'helm-projectile)
            '(helm-source-projectile-buffers-list))
        '(helm-source-buffers-list))

      (if (and (featurep 'semantic)
               (semantic-active-p))
          (progn
            (require 'helm-semantic)
            '(helm-source-semantic))
        (require 'helm-imenu)
        '(helm-source-imenu))

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
           (unless (featurep 'helm-ag)
             (when (or
                    (executable-find "ag")
                    (executable-find "ack")
                    (executable-find "ack-grep"))
               (require 'helm-ag)))
           (when (featurep 'helm-ag)
             '(helm-source-do-ag)))

         ;; file location, of which projectile can be a superset
         (unless projectile-root
           '(helm-source-findutils))

         '(helm-source-locate))))

     :fuzzy-match t
     :prompt (if projectile-root
                 (format "[%s] > " (projectile-project-name))
               "> ")
     :buffer "*helm-omni*")))

(defun nadvice/evil-paste-pop (old-fun &rest args)
  (if (memq last-command
            '(evil-paste-after
              evil-paste-before
              evil-visual-paste))
      (apply old-fun args)
    (call-interactively #'my/helm-omni)))

(advice-add 'evil-paste-pop :around #'nadvice/evil-paste-pop)

(define-key evil-insert-state-map (kbd "C-p") #'my/helm-omni)

(global-set-key (kbd "C-c C-o") #'my/helm-omni)
(define-key evil-normal-state-map (kbd "C-c C-o") #'my/helm-omni)
(define-key evil-insert-state-map (kbd "C-c C-o") #'my/helm-omni)
(define-key evil-emacs-state-map (kbd "C-c C-o") #'my/helm-omni)
(define-key evil-motion-state-map (kbd "C-c C-o") #'my/helm-omni)
(define-key evil-replace-state-map (kbd "C-c C-o") #'my/helm-omni)

(global-set-key (kbd "M-=") #'helm-semantic-or-imenu)

(provide 'config-helm)
