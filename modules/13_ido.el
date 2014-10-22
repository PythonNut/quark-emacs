;;; ==============================
;;; Ido - interactively do things
;;; ==============================
(ido-mode +1)
(require 'flx-ido)
(require 'ido-ubiquitous)
(require 'ido-vertical-mode)
(ido-ubiquitous +1)
(flx-ido-mode +1)
(ido-vertical-mode +1)

(set-face-foreground 'flx-highlight-face "grey70")
(set-face-background 'flx-highlight-face "grey20")
(set-face-attribute 'flx-highlight-face nil :underline nil)

(setq ido-save-directory-list-file "~/.emacs.d/ido.last")

(defun ido-goto-symbol (&optional symbol-list)
  "Refresh imenu and jump to a place in the buffer using Ido."
  (interactive)
  (unless (featurep 'imenu)
    (require 'imenu nil t))
  (cond
    ((not symbol-list)
      (let ((ido-mode ido-mode)
             (ido-enable-flex-matching
               (if (boundp 'ido-enable-flex-matching)
                 ido-enable-flex-matching t))
             name-and-pos symbol-names position)
        (unless ido-mode
          (ido-mode 1)
          (setq ido-enable-flex-matching t))
        (while (progn
                 (imenu--cleanup)
                 (setq imenu--index-alist nil)
                 (ido-goto-symbol (imenu--make-index-alist))
                 (setq selected-symbol
                   (ido-completing-read "Symbol? " symbol-names))
                 (string= (first imenu--rescan-item) selected-symbol)))
        (unless (and (boundp 'mark-active) mark-active)
          (push-mark nil t nil))
        (setq position (rest (assoc selected-symbol name-and-pos)))
        (cond
          ((overlayp position)
            (goto-char (overlay-start position)))
          (t
            (goto-char position)))))
    ((listp symbol-list)
      (dolist (symbol symbol-list)
        (let (name position)
          (cond
            ((and (listp symbol) (imenu--subalist-p symbol))
              (ido-goto-symbol symbol))
            ((listp symbol)
              (setq name (first symbol))
              (setq position (rest symbol)))
            ((stringp symbol)
              (setq name symbol)
              (setq position
                (get-text-property 1 'org-imenu-marker symbol))))
          (unless (or (null position) (null name)
                    (string= (first imenu--rescan-item) name))
            (add-to-list 'symbol-names name)
            (add-to-list 'name-and-pos (cons name position))))))))

(global-set-key (kbd "M-=") 'ido-goto-symbol)

