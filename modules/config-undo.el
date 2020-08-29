;; -*- lexical-binding: t -*-
(eval-when-compile (require 'config-macros))

(use-package undo-tree
  :config
  (eval-when-compile
    (require 'undo-tree)
    (use-package evil)
    (require 'evil))

  (evil-set-initial-state 'undo-tree-visualizer-mode 'motion)

  (setq undo-tree-mode-lighter nil
        undo-tree-auto-save-history t
        ;; Explanation from raxod502/radian: Disable undo-in-region.
        ;; It sounds like an interesting feature, but unfortunately
        ;; the implementation is very buggy and regularly causes you
        ;; to lose your undo history.
        undo-tree-enable-undo-in-region t
        undo-tree-history-directory-alist
        `((,(rx (zero-or-more anything))
           . ,(expand-file-name (locate-user-emacs-file "data/undo-backups/")))))

  (defalias 'redo #'undo-tree-redo)
  (defalias 'undo #'undo-tree-undo)

  ;; keep undo-tree from overriding C-x r
  (define-key undo-tree-map (kbd "C-x r u") nil)
  (define-key undo-tree-map (kbd "C-x r U") nil)
  (define-key undo-tree-map (kbd "C-x r") nil)
  (define-key undo-tree-map (kbd "C-?") nil)

  (key-chord-define evil-emacs-state-map "uu" #'undo-tree-visualize)
  (key-chord-define evil-insert-state-map "uu" #'undo-tree-visualize)

  (define-key evil-visual-state-map "u" #'undo-tree-undo)

  (global-set-key (kbd "M-_") #'undo-tree-redo)

  ;; visual line wrapping breaks the
  (add-hook 'undo-tree-visualizer-mode-hook
            (lambda ()
              (setq-local input-method-function nil)
              (setq-local global-hl-line-mode nil)
              (visual-line-mode -1)))

  (evil-set-initial-state 'undo-tree-visualizer-mode 'emacs)

  (define-key undo-tree-visualizer-mode-map
    (kbd "C-g") #'undo-tree-visualizer-quit)
  (define-key undo-tree-visualizer-mode-map
    (kbd "<escape>") #'undo-tree-visualizer-quit)
  (define-key undo-tree-visualizer-mode-map
    (kbd "RET") #'undo-tree-visualizer-quit)

  (define-key undo-tree-visualizer-mode-map
    (kbd "j") #'undo-tree-visualize-redo)
  (define-key undo-tree-visualizer-mode-map
    (kbd "k") #'undo-tree-visualize-undo)
  (define-key undo-tree-visualizer-mode-map
    (kbd "h") #'undo-tree-visualize-switch-branch-left)
  (define-key undo-tree-visualizer-mode-map
    (kbd "l") #'undo-tree-visualize-switch-branch-right)

  ;; compress undo with xz
  (cond ((executable-find "zstd")
         (advice-add
          'undo-tree-make-history-save-file-name :filter-return
          (my/defun-as-value nadvice/undo-tree/zstd-compress (ret)
            (concat ret ".zst"))))
        ((executable-find "gzip")
         (advice-add
          'undo-tree-make-history-save-file-name :filter-return
          (my/defun-as-value nadvice/undo-tree/gzip-compress (ret)
            (concat ret ".gz")))))

  (advice-add
   'undo-list-transfer-to-tree :around
   (my/defun-as-value nadvice/undo-tree-ignore-text-properties (old-fun &rest args)
     (dolist (item buffer-undo-list)
       (and (consp item)
            (stringp (car item))
            (setcar item (substring-no-properties (car item)))))
     (apply old-fun args)))

  (advice-add
   'undo-tree-load-history :around
   (my/defun-as-value nadvice/undo-tree-load-history/quiet (old-fun &rest args)
     (let ((inhibit-message t)
           (jka-compr-verbose))
       (apply old-fun args))))

  (advice-add
   'undo-tree-save-history :around
   (my/defun-as-value nadvice/undo-tree-save-history/quiet (old-fun &rest args)
     (cl-letf* ((jka-compr-verbose nil)
                (old-write-region (symbol-function #'write-region))
                ((symbol-function #'write-region)
                 (lambda (start end filename &optional append _visit &rest args)
                   (apply old-write-region
                          start
                          end
                          filename
                          append
                          0
                          args))))
       (apply old-fun args)))))

(provide 'config-undo)
