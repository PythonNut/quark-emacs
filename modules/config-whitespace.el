;; -*- lexical-binding: t -*-
(eval-when-compile (require 'config-macros))

(eval-when-compile
  (with-demoted-errors "Load error: %s"
    (use-package evil)
    (require 'evil)
    (require 'smie)))

(setq require-final-newline t
      line-move-visual t)

(use-package adaptive-wrap
  :init
  (eval-when-compile
    (with-demoted-errors "Load error: %s"
      (require 'el-patch)))

  (el-patch-feature adaptive-wrap)

  (el-patch-defcustom adaptive-wrap-extra-indent (el-patch-swap 0 2)
    "Number of extra spaces to indent in `adaptive-wrap-prefix-mode'.

`adaptive-wrap-prefix-mode' indents the visual lines to
the level of the actual line plus `adaptive-wrap-extra-indent'.
A negative value will do a relative de-indent.

Examples:

actual indent = 2
extra indent = -1

  Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do
 eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut
 enim ad minim veniam, quis nostrud exercitation ullamco laboris
 nisi ut aliquip ex ea commodo consequat.

actual indent = 2
extra indent = 2

  Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do
    eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut
    enim ad minim veniam, quis nostrud exercitation ullamco laboris
    nisi ut aliquip ex ea commodo consequat."
    :type 'integer
    :safe 'integerp
    :group 'visual-line)

  (make-variable-buffer-local 'adaptive-wrap-extra-indent)

  (el-patch-defun adaptive-wrap--face-extends (face)
    (if (fboundp 'face-extend-p)
        (face-extend-p face nil t)
      ;; Before Emacs 27, faces always extended beyond EOL.  Check for a
      ;; non-default background.
      (face-background face nil t)))

  (el-patch-defun adaptive-wrap--prefix-face (fcp beg end)
    (cond ((get-text-property 0 'face fcp))
          ;; If the last character is a newline and has a face that
          ;; extends beyond EOL, assume that this face spans the whole
          ;; line and apply it to the prefix to preserve the "block"
          ;; visual effect.
          ;; NB: the face might not actually span the whole line: see for
          ;; example removed lines in diff-mode, where the first character
          ;; has the diff-indicator-removed face, while the rest of the
          ;; line has the diff-removed face.
          ((= (char-before end) ?\n)
           (let ((eol-face (get-text-property (1- end) 'face)))
             (and eol-face (adaptive-wrap--face-extends eol-face) eol-face)))))

  (el-patch-defun adaptive-wrap--prefix (fcp)
    (let ((fcp-len (string-width fcp)))
      (cond
       ((= 0 adaptive-wrap-extra-indent)
        fcp)
       ((< 0 adaptive-wrap-extra-indent)
        (concat fcp (make-string adaptive-wrap-extra-indent ?\s)))
       ((< 0 (+ adaptive-wrap-extra-indent fcp-len))
        (substring fcp
                   0
                   (+ adaptive-wrap-extra-indent fcp-len)))
       (t
        ""))))

  (el-patch-defun adaptive-wrap-fill-context-prefix (beg end)
    "Like `fill-context-prefix', but with length adjusted by `adaptive-wrap-extra-indent'."
    (let* ((fcp
            ;; `fill-context-prefix' ignores prefixes that look like paragraph
            ;; starts, in order to avoid inadvertently creating a new paragraph
            ;; while filling, but here we're only dealing with single-line
            ;; "paragraphs" and we don't actually modify the buffer, so this
            ;; restriction doesn't make much sense (and is positively harmful in
            ;; taskpaper-mode where paragraph-start matches everything).
            (or (let ((paragraph-start "\\`\\'a"))
                  (fill-context-prefix beg end))
                ;; Note: fill-context-prefix may return nil; See:
                ;; http://article.gmane.org/gmane.emacs.devel/156285
                ""))
           (prefix (adaptive-wrap--prefix fcp))
           (face (adaptive-wrap--prefix-face fcp beg end)))
      (if face
          (propertize prefix 'face face)
        prefix)))

  (el-patch-defun adaptive-wrap-prefix-function (beg end)
    "Indent the region between BEG and END with adaptive filling."
    ;; Any change at the beginning of a line might change its wrap prefix, which
    ;; affects the whole line.  So we need to "round-up" `end' to the nearest end
    ;; of line.  We do the same with `beg' although it's probably not needed.
    (goto-char end)
    (unless (bolp) (forward-line 1))
    (setq end (point))
    (goto-char beg)
    (forward-line 0)
    (setq beg (point))
    (while (< (point) end)
      (let ((lbp (point)))
        (put-text-property
         (point) (progn (search-forward "\n" end 'move) (point))
         'wrap-prefix
         (let ((pfx (adaptive-wrap-fill-context-prefix
                     lbp (point))))
           ;; Remove any `wrap-prefix' property that
           ;; might have been added earlier.
           ;; Otherwise, we end up with a string
           ;; containing a `wrap-prefix' string
           ;; containing a `wrap-prefix' string ...
           (remove-text-properties
            0 (length pfx) '(wrap-prefix) pfx)
           (let ((dp (get-text-property 0 'display pfx)))
             (when (and dp (eq dp (get-text-property (1- lbp) 'display)))
               ;; There's a `display' property which covers not just the
               ;; prefix but also the previous newline.  So it's not just making
               ;; the prefix more pretty and could interfere or even defeat our
               ;; efforts (e.g. it comes from `visual-fill-mode').
               (remove-text-properties
                0 (length pfx) '(display) pfx)))
           pfx))))
    `(jit-lock-bounds ,beg . ,end))

  (el-patch-define-minor-mode adaptive-wrap-prefix-mode
    "Wrap the buffer text with adaptive filling."
    :lighter ""
    :group 'visual-line
    (if adaptive-wrap-prefix-mode
        (progn
          ;; HACK ATTACK!  We want to run after font-lock (so our
          ;; wrap-prefix includes the faces applied by font-lock), but
          ;; jit-lock-register doesn't accept an `append' argument, so
          ;; we add ourselves beforehand, to make sure we're at the end
          ;; of the hook (bug#15155).
          (add-hook 'jit-lock-functions
                    #'adaptive-wrap-prefix-function 'append t)
          (jit-lock-register #'adaptive-wrap-prefix-function))
      (jit-lock-unregister #'adaptive-wrap-prefix-function)
      (with-silent-modifications
        (save-restriction
          (widen)
          (remove-text-properties (point-min) (point-max) '(wrap-prefix nil)))))))

(add-hook 'visual-line-mode-hook #'adaptive-wrap-prefix-mode)

(diminish 'visual-line-mode)
(global-visual-line-mode +1)

;; always ensure UTF-8
(add-hook
 'before-save-hook
 (my/defun-as-value cleanup-buffer-safe ()
   (interactive)
   (set-buffer-file-coding-system 'utf-8)))

(use-package ws-butler
  :diminish (ws-butler-mode ." β")
  :init
  ;; autoload ws-butler on file open
  (my/onetime-setup ws-butler
    :hook 'find-file-hook
    (ws-butler-global-mode +1)))

(use-package dtrt-indent
  :init (add-hook 'find-file-hook #'dtrt-indent-mode)
  :config (diminish 'dtrt-indent-mode))

;; ws-butler also loads highlight-changes-mode
(add-hook
 'highlight-changes-mode-hook
 (my/defun-as-value my/diminish-highlight-changes-mode ()
   (diminish 'highlight-changes-mode)))

(use-package aggressive-indent
  :config
  (diminish 'aggressive-indent-mode (if (display-graphic-p) " ⇒" " *→")))

(add-hook
 'after-change-major-mode-hook
 (my/defun-as-value my/smie-auto-guess ()
   (when (and
          (featurep 'smie)
          (not (eq smie-grammar 'unset)))
     (let ((smie-config--buffer-local nil))
       (smie-config-guess)))))

(defun back-to-indentation-visual-or-beginning (&optional n)
  (interactive "^p")
  (or n (setq n 1))
  (if (/= n 1)
      (let ((line-move-visual t))
        (line-move (1- n) t)))
  (let ((bol (save-excursion (beginning-of-line) (point)))
        (bti (save-excursion (back-to-indentation) (point)))
        (bovl (save-excursion (beginning-of-visual-line) (point))))
    (if (bound-and-true-p multiple-cursors-mode)
        (cond
         ((= (point) bti)
          (beginning-of-line))
         ((/= (point) bol)
          (back-to-indentation)))
      (cond
       ((= (point) bol)
        (back-to-indentation))
       ((= (point) bti)
        (beginning-of-visual-line))
       ((= (point) bovl)
        (back-to-indentation))
       ((/= bol bovl)
        (beginning-of-visual-line))
       (t
        (back-to-indentation))))))

(defun end-of-visual-line-or-end (&optional n)
  (interactive "^p")
  (or n (setq n 1))
  (if (/= n 1)
      (let ((line-move-visual t))
        (line-move (1- n) t)))
  (let ((eovl (save-excursion (end-of-visual-line) (point))))
    (cond
     ((bound-and-true-p multiple-cursors-mode)
      (end-of-line))
     ((= (point) eovl)
      (end-of-line))
     (t
      (end-of-visual-line)))))

(with-eval-after-load 'evil
  (eval-when-compile
    (with-demoted-errors "Load error: %s"
      (require 'evil)))

  (define-key evil-insert-state-map (kbd "C-a")
    #'back-to-indentation-visual-or-beginning)
  (define-key evil-motion-state-map (kbd "C-a")
    #'back-to-indentation-visual-or-beginning)

  (define-key evil-insert-state-map (kbd "<home>")
    #'back-to-indentation-visual-or-beginning)
  (define-key evil-motion-state-map (kbd "<home>")
    #'back-to-indentation-visual-or-beginning)
  (define-key evil-insert-state-map (kbd "C-e") #'end-of-visual-line-or-end)
  (define-key evil-insert-state-map (kbd "<end>") #'end-of-visual-line-or-end))

(provide 'config-whitespace)
