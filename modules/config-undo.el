;; -*- lexical-binding: t -*-

(eval-when-compile (require'cl-lib))

(with-eval-after-load 'undo-tree
  (eval-when-compile
    (require 'undo-tree)
    (require 'evil))

  (evil-set-initial-state 'undo-tree-visualizer-mode 'motion)

  (diminish 'undo-tree-mode)
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
  (when (executable-find "xz")
    (defun nadvice/undo-tree-make-history-save-file-name (_ret)
      (let ((auto-save-file-name-transforms
             `((,(rx (zero-or-more not-newline))
                ,(locate-user-emacs-file "data/undo-backups") t))))
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

  ;; Strip text-properties from undo history
  (require 'seq)
  (require 'cl-lib)

  (cl-defstruct (copy-tree*
                 (:constructor copy-tree*-mem (&optional stack stack-new (hash (make-hash-table)))))
    stack stack-new hash)

  (defmacro copy-tree*--push (el el-new mem &optional hash)
    (let ((my-el (make-symbol "my-el"))
          (my-el-new (make-symbol "my-el-new"))) ; makes sure `el' is only evaluated once
      (append `(let ((,my-el ,el)
                     (,my-el-new ,el-new))
                 (push ,my-el (copy-tree*-stack ,mem))
                 (push ,my-el-new (copy-tree*-stack-new ,mem)))
              (and hash
                   `((puthash ,my-el ,my-el-new (copy-tree*-hash ,mem))))
              (list my-el-new))))

  (defmacro copy-tree*--pop (el el-new mem)
    `(setq ,el (pop (copy-tree*-stack ,mem))
           ,el-new (pop (copy-tree*-stack-new mem))))

  (defun copy-tree*--copy-node (node mem vecp)
    (if (or (consp node)
            (and vecp (vectorp node)))
        (let ((existing-node (gethash node (copy-tree*-hash mem))))
          (if existing-node
              existing-node
            (copy-tree*--push node (if (consp node)
                                       (cons nil nil)
                                     (make-vector (length node) nil))
                              mem t)))
      node))

  (defun copy-tree* (tree &optional vecp)
    "Structure preserving version of `cl-copy-tree'."
    (if (or (consp tree)
            (and vecp (vectorp tree)))
        (let* ((tree-new (if (consp tree) (cons nil nil)
                           (make-vector (length tree) nil)))
               (mem (copy-tree*-mem))
               next
               next-new)
          (copy-tree*--push tree tree-new mem t)
          (while (copy-tree*--pop next next-new mem)
            (cond
             ((consp next)
              (setcar next-new (copy-tree*--copy-node (car next) mem vecp))
              (setcdr next-new (copy-tree*--copy-node (cdr next) mem vecp)))
             ((and vecp (vectorp next))
              (cl-loop for i from 0 below (length next) do
                       (aset next-new i (copy-tree*--copy-node (aref next i) mem vecp))))))
          tree-new)
      tree))

  (defun undo-tree-seq-unprop (x)
    "Remove text properties on all strings of seq."
    (cond
     ((stringp x)
      (substring-no-properties x))
     ((consp x)
      (cons (undo-tree-seq-unprop (car x)) (undo-tree-seq-unprop (cdr x))))
     ((null x)
      nil)
     ((seqp x)
      (apply (type-of x) (seq-map (lambda (x) (undo-tree-seq-unprop x)) x)))
     (t x)))

  (defun nadvice/undo-tree-save-history (&optional filename overwrite)
    "Store undo-tree history to file.

If optional argument FILENAME is omitted, default save file is
\".<buffer-file-name>.~undo-tree\" if buffer is visiting a file.
Otherwise, prompt for one.

If OVERWRITE is non-nil, any existing file will be overwritten
without asking for confirmation."
    (interactive)
    (when (eq buffer-undo-list t)
      (user-error "No undo information in this buffer"))
    (undo-list-transfer-to-tree)
    (when (and buffer-undo-tree (not (eq buffer-undo-tree t)))
      (condition-case nil
          (undo-tree-kill-visualizer)
        (error (undo-tree-clear-visualizer-data buffer-undo-tree)))
      (let ((buff (current-buffer))
            tree)
        ;; get filename
        (unless filename
          (setq filename
                (if buffer-file-name
                    (undo-tree-make-history-save-file-name buffer-file-name)
                  (expand-file-name (read-file-name "File to save in: ") nil))))
        (when (or (not (file-exists-p filename))
                  overwrite
                  (yes-or-no-p (format "Overwrite \"%s\"? " filename)))
          (unwind-protect
              (progn
                ;; transform undo-tree into non-circular structure, and make
                ;; temporary copy
                (undo-tree-decircle buffer-undo-tree)
                (setq tree (copy-tree* buffer-undo-tree))
                ;; discard undo-tree object pool before saving
                (setf (undo-tree-object-pool tree) nil)
                (undo-tree-mapc
                 (lambda (node)
                   (setf (undo-tree-node-undo node) (undo-tree-seq-unprop (undo-tree-node-undo node)))
                   (setf (undo-tree-node-redo node) (undo-tree-seq-unprop (undo-tree-node-redo node))))
                 (undo-tree-root tree))
                ;; print undo-tree to file
                ;; NOTE: We use `with-temp-buffer' instead of `with-temp-file'
                ;;       to allow `auto-compression-mode' to take effect, in
                ;;       case user has overridden or advised the default
                ;;       `undo-tree-make-history-save-file-name' to add a
                ;;       compressed file extension.
                (with-auto-compression-mode
                  (with-temp-buffer
                    (prin1 (sha1 buff) (current-buffer))
                    (terpri (current-buffer))
                    (let ((print-circle t)) (prin1 tree (current-buffer)))
                    (write-region nil nil filename))))
            ;; restore circular undo-tree data structure
            (undo-tree-recircle buffer-undo-tree))))))
  
  (advice-add 'undo-tree-save-history
              :override
              #'nadvice/undo-tree-save-history))

(provide 'config-undo)
