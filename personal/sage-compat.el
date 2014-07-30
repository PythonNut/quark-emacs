;;;_* sage-compat.el --- Compatibility with new python.el

;; Hack around new python.el.  Eventually all of this should go away,
;; but for now it's the easiest way to get things working.

;;; Commentary:

;;; Code:

(require 'python)
(require 'rx)

;; From org-called-interactively-p
(defmacro sage-called-interactively-p (&optional kind)
  (if (featurep 'xemacs)
      `(interactive-p)
    (if (or (> emacs-major-version 23)
            (and (>= emacs-major-version 23)
                 (>= emacs-minor-version 2)))
        `(with-no-warnings (called-interactively-p ,kind)) ;; defined with no argument in <=23.1
      `(interactive-p))))

(unless (fboundp 'help-print-return-message)
  (defalias 'help-print-return-message 'print-help-return-message))

(if (fboundp 'python-beginning-of-string)
    (defalias 'sage-beginning-of-string 'python-beginning-of-string)
  (with-no-warnings
    (defun sage-beginning-of-string ()
      "Go to beginning of string around point.
Do nothing if not in string."
      (let ((bos (python-info-ppss-context 'string)))
        (when bos
          (goto-char bos))))))

(if (fboundp 'python-in-string/comment)
    (defalias 'sage-in-string/comment 'python-in-string/comment)
  (defalias 'sage-in-string/comment 'python-info-ppss-comment-or-string-p))

(unless (boundp 'python-prev-dir/file)
  (defvar python-prev-dir/file nil))

(unless (fboundp 'python-comment-line-p)
  (defalias 'python-comment-line-p 'python-info-current-line-comment-p))

(unless (fboundp 'python-beginning-of-statement)
  (defalias 'python-beginning-of-statement 'python-nav-beginning-of-statement))

(unless (fboundp 'python-end-of-statement)
  (defalias 'python-end-of-statement 'python-nav-end-of-statement))

(unless (fboundp 'python-comment-line-p)
  (defalias 'python-comment-line-p 'python-info-current-line-comment-p))

(unless (fboundp 'python-open-block-statement-p)
  (defalias 'python-open-block-statement-p 'python-info-beginning-of-block-p))

(unless (fboundp 'python-previous-statement)
  (defalias 'python-previous-statement #'python-nav-backward-sentence))

(unless (fboundp 'python-beginning-of-block)
  (defalias 'python-previous-statement #'python-nav-beginning-of-block))

(unless (fboundp 'python-end-of-block)
  (defalias 'python-end-of-block 'python-nav-end-of-block))

;; Changed pyrex to cython
(define-obsolete-function-alias 'pyrex-mode 'cython-mode "0.10")
(define-obsolete-function-alias 'pyrex-mode-p 'cython-mode-p "0.10")
(define-obsolete-function-alias 'pyrex-open-block-statement-p 'cython-open-block-statement-p "0.10")
(define-obsolete-function-alias 'pyrex-beginning-of-defun 'cython-beginning-of-defun "0.10")
(define-obsolete-function-alias 'pyrex-end-of-defun 'cython-end-of-defun "0.10")
(define-obsolete-function-alias 'pyrex-current-defun 'cython-current-defun "0.10")

(provide 'sage-compat)

;;; sage-compat.el ends here
