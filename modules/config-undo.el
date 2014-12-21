(eval-when-compile
  (progn
    (require 'evil)
    (require 'undo-tree)))

(add-hook 'first-change-hook
  (lambda () (require 'undo-tree)))

(with-eval-after-load 'evil
  (evil-define-key 'motion undo-tree-visualizer-mode-map (kbd "t")
    #'undo-tree-visualizer-toggle-timestamps)
  (evil-define-key 'motion  undo-tree-visualizer-mode-map (kbd "d")
    #'undo-tree-visualizer-toggle-diff))

(with-eval-after-load 'undo-tree
  (diminish 'undo-tree-mode " μ")
  (defalias 'redo #'undo-tree-redo)
  (defalias 'undo #'undo-tree-undo)

  (key-chord-define evil-emacs-state-map "uu" #'undo-tree-visualize)
  (key-chord-define evil-insert-state-map "uu" #'undo-tree-visualize)

  (define-key evil-visual-state-map "u" #'undo-tree-undo)

  (global-set-key (kbd "M-_") #'undo-tree-redo)
  (setq undo-tree-auto-save-history t)

  (add-to-list 'evil-overriding-maps 'undo-tree-visualizer-mode-map)

  ;; visual line wrapping breaks the 
  (add-hook 'undo-tree-visualizer-mode-hook
    (lambda ()
      (visual-line-mode -1)))

  (evil-define-key 'motion undo-tree-visualizer-mode-map
    "C-g" #'undo-tree-visualizer-quit)
  (evil-define-key 'motion undo-tree-visualizer-mode-map
    (kbd "<escape>") #'undo-tree-visualizer-quit)
  (evil-define-key 'motion undo-tree-visualizer-mode-map
    (kbd "<return>") #'undo-tree-visualizer-quit)
  (evil-define-key 'motion undo-tree-visualizer-mode-map
    (kbd "<up>") #'undo-tree-visualize-undo)
  (evil-define-key 'motion undo-tree-visualizer-mode-map
    (kbd "<down>") #'undo-tree-visualize-redo)

  ;; compress undo with xz
  (when (locate-file "xz" exec-path)
    (defadvice undo-tree-make-history-save-file-name
      (after undo-tree activate)
      (setq ad-return-value (concat (make-auto-save-file-name) ".undo.xz")))

    (defadvice undo-tree-load-history (around quiet-compress activate)
      (let ((jka-compr-verbose nil))
        ad-do-it)))

  ;; Keep region when undoing in region
  (defadvice undo-tree-undo (around keep-region activate)
    (if (use-region-p)
      (let ((m (set-marker (make-marker) (mark)))
             (p (set-marker (make-marker) (point))))
        ad-do-it
        (goto-char p)
        (set-mark m)
        (set-marker p nil)
        (set-marker m nil))
      ad-do-it)))

(provide 'config-undo)
