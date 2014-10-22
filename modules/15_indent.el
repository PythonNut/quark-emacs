;;; ================
;;; Auto indent mode
;;; ================
(require 'auto-indent-mode)
(setq auto-indent-key-for-end-of-line-then-newline "M-RET")
(setq auto-indent-key-for-end-of-line-insert-char-then-newline "<M-S-return>")

(defadvice auto-indent-eol-newline (after indent-too activate)
  (indent-for-tab-command))
(defadvice auto-indent-eol-char-newline (after indent-too activate)
  (indent-for-tab-command))

(global-set-key (kbd "M-RET") 'auto-indent-eol-newline)
(global-set-key (kbd "C-M-j") 'auto-indent-eol-newline)
(global-set-key (kbd "<M-S-return>") 'auto-indent-eol-char-newline)

(setq auto-indent-newline-function 'newline-and-indent)
(setq auto-indent-assign-indent-level 4)
(setq auto-indent-backward-delete-char-behavior 'hungry)
(setq auto-indent-indent-style 'conservative)
(auto-indent-global-mode +1)

(defun back-to-indentation-or-beginning ()
  (interactive "^")
  (if (= (point) (progn (back-to-indentation) (point)))
    (beginning-of-line)))

(put 'back-to-indentation-or-beginning 'CUA 'move)
(global-set-key (kbd "<home>") 'back-to-indentation-or-beginning)

;;; ================================
;;; Smart indent/unindent with <tab>
;;; ================================
(setq my-tab-width 2)
(defun indent-block ()
  (interactive)
  (shift-region my-tab-width)
  (setq deactivate-mark nil))

(defun unindent-block ()
  (interactive)
  (shift-region (- my-tab-width))
  (setq deactivate-mark nil))

(defun shift-region (numcols)
  " my trick to expand the region to the beginning and end of the area selected
 much in the handy way I liked in the Dreamweaver editor."
  (if (< (point) (mark))
    (if (not (bolp))
      (progn
        (beginning-of-line)
        (exchange-point-and-mark)
        (end-of-line)))
    (progn
      (end-of-line)
      (exchange-point-and-mark)
      (beginning-of-line)))

  (setq region-start (region-beginning))
  (setq region-finish (region-end))

  (save-excursion
    (if (< (point) (mark))
      (exchange-point-and-mark))
    (let ((save-mark (mark)))
      (indent-rigidly
        region-start
        region-finish
        numcols))))

(defun my-unindent ()
  "Unindent line, or block if it's a region selected.
When pressing Shift+tab, erase words backward (one at a time) up to the beginning of line."

  (interactive)
  (if mark-active
    (unindent-block)
    (icicle-complete-keys)))

(defun indent-or-complete (arg)
  "Indent region selected as a block; if no selection present either indent according to mode,
or expand the word preceding point. Multiple tabs cycle indentation level."
  (interactive "P")
  (if mark-active
    (progn
      (setq this-command 'indent-block)
      (indent-block)
      (message "indent block"))

    (if (minibufferp)
      (unless (minibuffer-complete)
        (hippie-expand))

      (if (looking-at "\\>")
        (if (and (fboundp 'auto-complete-mode) auto-complete-mode)
          (progn
            (setq this-command 'ac-trigger-key-command)
            (ac-trigger-key-command)
            (message "auto-complete"))

          (if (and (fboundp 'company-complete) company-mode)
            (progn
              (setq this-command 'company-complete)
              (company-complete)
              (message "company-complete"))

            (progn
              (setq this-command 'hippie-expand)
              (hippie-expand arg)
              (message "hippie-expand"))))

        (if (eq major-mode 'org-mode)
          (progn
            (setq this-command 'org-cycle)
            (call-interactively 'org-cycle))
          (progn
            (setq this-command 'indent-for-tab-command)
            (indent-for-tab-command arg)
            (message "indent")))))))

(define-key evil-insert-state-map (kbd "<tab>") 'indent-or-complete)
(define-key evil-insert-state-map (kbd "<backtab>") 'my-unindent)

;;; ====================================================
;;; dtrt-indent - guess indentation offset of alien code
;;; ====================================================
(require 'dtrt-indent)
(dtrt-indent-mode +1)

(defun eval-and-replace (value)
  "Evaluate the sexp at point and replace it with its value"
  (interactive (list (eval-last-sexp nil)))
  (kill-sexp -1)
  (insert (format "%S" value)))

(global-set-key (kbd "C-x C-e") 'eval-and-replace)

