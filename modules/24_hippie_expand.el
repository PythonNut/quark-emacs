;;; ==================================================
;;; Hippie expand - secondary autocompletion framework
;;; ==================================================
(defun try-expand-flx (old)
  "Try to complete word using flx matching."
  (if (not old)
    (progn
      (he-init-string (he-lisp-symbol-beg) (point))
      (if (not (he-string-member he-search-string he-tried-table))
        (setq he-tried-table (cons he-search-string he-tried-table)))
      (setq he-expand-list
        (and (not (equal he-search-string ""))
          (try-expand-flx-collect he-search-string)))))
  (while (and he-expand-list
           (he-string-member (first he-expand-list) he-tried-table))
    (setq he-expand-list (rest he-expand-list)))
  (if (null he-expand-list)
    (progn
      (if old (he-reset-string)) ())
    (progn
      (he-substitute-string (first he-expand-list))
      (setq he-expand-list (rest he-expand-list))
      t)))

(defun try-expand-flx-collect (str)
  "Find and collect all words that flex-match str, and sort by flx score"
  (let ((coll '())
         (regexp (try-expand-flx-regexp str)))
    (save-excursion
      (goto-char (point-min))
      (while (search-forward-regexp regexp nil t)
        (setq coll (cons (thing-at-point 'symbol) coll))))
    (setq coll (sort coll #'(lambda (a b)
                              (> (first (flx-score a str))
                                (first (flx-score b str))))))
    coll))

(defun try-expand-flx-regexp (str)
  "Generate regexp for flexible matching of str."
  (concat "\\b" (mapconcat (lambda (x) (concat "\\w*-*" (list x))) str "")
    "\\w*-*" "\\b"))

(global-set-key (kbd "M-/") 'hippie-expand)
(setq hippie-expand-try-functions-list
  '(yas-hippie-try-expand
     try-expand-dabbrev
     try-expand-dabbrev-from-kill
     try-expand-flx
     try-expand-dabbrev-all-buffers
     try-complete-file-name-partially
     try-complete-file-name
     try-expand-all-abbrevs
     try-expand-list
     try-expand-line
     try-complete-lisp-symbol-partially
     try-complete-lisp-symbol))
