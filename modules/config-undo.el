;; -*- lexical-binding: t -*-
(eval-when-compile (require 'config-macros))

(use-package undo-tree
  :init
  (autoload 'undo-tree-undo "undo-tree")
  (autoload 'undo-tree-redo "undo-tree")

  (defalias 'redo #'undo-tree-redo)
  (defalias 'undo #'undo-tree-undo)

  (with-eval-after-load 'evil
    (advice-add 'evil--check-undo-system :override #'ignore))

  :config
  (setq undo-tree-mode-lighter nil
        undo-tree-auto-save-history t
        undo-tree-enable-undo-in-region t
        undo-tree-history-directory-alist
        `((,(rx (zero-or-more anything))
           . ,(expand-file-name (locate-user-emacs-file "data/undo-backups/")))))

  (global-undo-tree-mode +1)

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

  (with-eval-after-load 'evil
    (evil-set-initial-state 'undo-tree-visualizer-mode 'emacs))

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
         (define-advice undo-tree-make-history-save-file-name
             (:filter-return (ret) zstd-compress)
           (concat ret ".zst")))
        ((executable-find "gzip")
         (define-advice undo-tree-make-history-save-file-name
             (:filter-return (ret) gzip-compress)
           (concat ret ".gz"))))

  (define-advice undo-list-transfer-to-tree
      (:around (old-fun &rest args) ignore-text-properties)
    (dolist (item buffer-undo-list)
      (and (consp item)
           (stringp (car item))
           (setcar item (substring-no-properties (car item)))))
    (apply old-fun args))

  (define-advice undo-tree-load-history
      (:around (old-fun &rest args) inhibit-message)
    (let ((inhibit-message t)
          (jka-compr-verbose))
       (apply old-fun args)))

  (define-advice undo-tree-save-history
      (:around (old-fun &rest args) inhibit-message)
    (cl-letf* ((jka-compr-verbose)
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
      (apply old-fun args))))

(provide 'config-undo)
