;;; ===========================================
;;; Undo tree mode - the ultimate undo function
;;; ===========================================
(add-hook 'first-change-hook
  (lambda () (require 'undo-tree)))

(defalias 'redo 'undo-tree-redo)
(defalias 'undo 'undo-tree-undo)

(add-hook 'emacs-startup-hook
  '(lambda ()
     (run-at-time 0.1 nil
       '(lambda ()
          (message "")))))

(add-hook 'find-file-hook
  '(lambda ()
     (run-at-time 1 nil
       '(lambda ()
          (message "")))))

(key-chord-define evil-emacs-state-map "uu" 'undo-tree-visualize)

(eval-after-load 'undo-tree
  '(progn
     (global-set-key (kbd "M-_") 'undo-tree-redo)
     (setq undo-tree-auto-save-history t)

     (define-key undo-tree-visualizer-mode-map "C-g" 'undo-tree-visualizer-quit)
     (define-key undo-tree-visualizer-mode-map (kbd "<escape>") 'undo-tree-visualizer-quit)
     (define-key undo-tree-visualizer-mode-map (kbd "<return>") 'undo-tree-visualizer-quit)
     (define-key undo-tree-visualizer-mode-map (kbd "<up>") 'undo-tree-visualize-undo)
     (define-key undo-tree-visualizer-mode-map (kbd "<down>") 'undo-tree-visualize-redo)
     (evil-define-key 'motion undo-tree-visualizer-mode-map (kbd "t")
       'undo-tree-visualizer-toggle-timestamps)
     (evil-define-key 'motion  undo-tree-visualizer-mode-map (kbd "d")
       'undo-tree-visualizer-toggle-diff)

     (add-hook 'undo-tree-visualizer-mode-hook
       '(lambda ()
          (evil-motion-state)))

     ;; compress undo with xz
     (when (locate-file "xz" exec-path)
       (defadvice undo-tree-make-history-save-file-name
         (after undo-tree activate)
         (setq ad-return-value (concat (make-auto-save-file-name) ".undo.xz"))))

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
         ad-do-it))


     (message "")))


