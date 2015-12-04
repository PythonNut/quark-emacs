;; -*- lexical-binding: t -*-


(with-eval-after-load 'undo-tree
  (eval-when-compile
    (require 'undo-tree)
    (require 'evil))

  (diminish 'undo-tree-mode " μ")
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
  (setq undo-tree-auto-save-history t)

  ;; visual line wrapping breaks the
  (add-hook 'undo-tree-visualizer-mode-hook
            (lambda ()
              (set (make-local-variable 'input-method-function) nil)
              (set (make-variable-buffer-local 'global-hl-line-mode) nil)
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
  (when (executable-find "xz")
    (defun nadvice/undo-tree-make-history-save-file-name (_ret)
      (let ((auto-save-file-name-transforms
             '((".*" "/home/pythonnut/.emacs.d/data/undo-backups/" t))))
        (concat (make-auto-save-file-name) ".undo.xz")))

    (defun nadvice/undo-tree-load-history (old-fun &rest args)
      (let ((jka-compr-verbose))
        (apply old-fun args)))

    (advice-add 'undo-tree-make-history-save-file-name
                :filter-return
                #'nadvice/undo-tree-make-history-save-file-name)
    (advice-add 'undo-tree-load-history
                :around
                #'nadvice/undo-tree-load-history))

  ;; Keep region when undoing in region
  (defun nadvice/undo-tree-undo (old-fun &rest args)
    (if (use-region-p)
        (let ((m (set-marker (make-marker) (mark)))
              (p (set-marker (make-marker) (point))))
          (apply old-fun args)
          (goto-char p)
          (set-mark m)
          (set-marker p nil)
          (set-marker m nil))
      (apply old-fun args)))

  (advice-add 'undo-tree-undo :around #'nadvice/undo-tree-undo))

(provide 'config-undo)
