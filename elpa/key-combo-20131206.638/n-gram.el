(defun flatten2(l)
  (if l (append (car l) (flatten2 (cdr l))) nil))

(defun print-hash (hash)
  (princ "{")
  (maphash (lambda (key value)
              (princ (format "%s => %s, "
                       (my-pp-to-string key)
                       (if (hash-table-p value) (print-hash value) (pp value))
                       ))
              ) hash)
  (princ "}")
  nil
  )

(defun traverse-tree (tree function &optional prefix)
  (let ((hoge
         (mapcar
          (lambda (x)
            (let ((pre (vconcat prefix (vector (car x)))))
              ;;(char-to-string (car x)))))
              (if (keymapp (cdr x))
                  (traverse-tree (cdr x) function pre)
                (car (cdr x)))
              ))
          (cdr tree))))
    (apply function prefix hoge)))

(defun traverse-tree-node (tree function &optional prefix)
  ;;(if (keymapp tree)
  (let ((hoge
         (mapcar
          (lambda (x)
            (let ((pre (vconcat prefix (vector (car x)))))
              ;;(char-to-string (car x))))
              (if (keymapp (cdr x))
                  (flatten2 (traverse-tree-node (cdr x) function pre))
                (list (funcall function prefix (cons (car x) (cadr x))))
                )))
          (cdr tree))))
    (if prefix hoge (apply 'append hoge))))

(defun my-lookup-key (keymap key)
  (let ((found (lookup-key keymap key)))
    (if (numberp found) nil (car found))
    ))

(defun my-define-key (keymap key def)
  (define-key keymap key (list def))
  )

(defun n-gram-internal (list n tree)
  (dotimes (i (1+ (- (length list) n)))
    (let* ((sub (substring (vconcat list) i (+ i n)))
           (count (or (my-lookup-key tree sub) 0)))
      (my-define-key tree sub (1+ count))))
  tree)

(defun split-word (&optional reversep)
  (let ((list))
    (save-excursion
      (goto-char (point-min))
      (while (< (point) (point-max))
        (let ((pre (point)))
          (skip-syntax-forward (char-to-string (char-syntax (char-after))))
          (cond ((and (eq ?- (char-after))
                      (eq ?_ (char-syntax (char-after))))
                 (forward-symbol 1)))
          (push
           (cond ((eq ?  (char-syntax (char-after pre)))
                  (intern " "))
                 ((eq ?w (char-syntax (char-after pre)))
                  (push
                   (intern (buffer-substring-no-properties pre (point)))
                   words)
                  (intern (buffer-substring-no-properties pre (point))))
                 ((and (eq ?\n (char-after pre))
                       (eq ?  (char-syntax (or (char-after (1+ pre)) ?\n ))))
                  (skip-syntax-forward
                   (char-to-string (char-syntax (char-after))))
                  (intern "\n"))
                 (t
                  (intern (buffer-substring-no-properties pre (point)))))
           list)))
      (if reversep list (nreverse list)))))

;; (let ((words))
;;   (pp-to-string (split-word))
;;   ;;(message "%S" (nreverse words))
;;   )

(defun my-memq (a b)
  (flatten2
   (delete-if
    'null
    (mapcar (lambda (x)
              (if (memq x b) (list x) nil))
            a))))

;; (my-memq '(a b d e) '(a c))
;; (my-memq '(d) '(a c))
;; (my-memq '(nil a) '(nil a))
;;(my-memq '(a nil) '(nil a b))
;;(my-memq '(nil) '(a nil b))

;;(memq nil '(a nil b))
;;(not (my-memq (car x) tmp-list))

(defun my-pp-to-string (string)
  (let((string1 (pp-to-string string)))
    (substring string1 0 (1- (length string1)))
    ))

(defun n-gram-print (list &optional reversep)
  (mapcar
   (lambda (elements)
     (let ((sequence
            (mapcar (lambda (x) (substring-no-properties(symbol-name x )))
                    (car elements))))
       (princ (if reversep
           (apply 'format "%s<-%s %3.1f%% %d/%d\n"
                  (pp-to-string (symbol-name (nth 1 elements)))
                  (my-pp-to-string
                   (vconcat (nreverse sequence)))
                  (cdr (cdr elements))
                  )
         (apply 'format "%s->%s %3.1f%% %d/%d\n"
                (my-pp-to-string
                 (vconcat sequence))
                (pp-to-string (symbol-name (nth 1 elements)))
                (cdr(cdr elements)))))
       ))
   list))

(defun make-element (prefix x)
  (list
   prefix ;;0 pre
   ;;(pp-to-string (symbol-name (car x)))
   (car x)
   (if (eq 0 (cdr x)) 0
     (/ (* (cdr x) 100) (gethash prefix my-hash))) ;;2 %
   (cdr x);; 3 n/
   (gethash prefix my-hash));;4/n
  )

;;(n-gram 3)
;;(n-gram 5)
;;(reverse '[a b])
;;(or nil 1)
;;(last '[a b c])
;;(substring '[a b c] 0 0)
;;(setq a 1)
;;(append nil a)
(defun n-gram1 (n &optional reversep skip-symbolp)
  (let ((tree (make-sparse-keymap))
        (my-hash (make-hash-table :test 'equal))
        (my-list nil)
        (words nil)
        (max-lisp-eval-depth 1000))
    ;;treem
    (n-gram-internal (split-word reversep) n tree)
    (traverse-tree
     tree
     (lambda (prefix &rest list)
       (let ((ret (apply '+ list)))
         (puthash prefix ret my-hash)
         ret)))
    (traverse-tree
     tree
     (lambda (prefix &rest list)
       (unless (eq prefix nil)
         (let ((pre (if (eq (length prefix) 1) nil
                      (substring prefix 0
                                 (1- (length prefix)))))
               (last (aref prefix (1- (length prefix)))))
           (push (make-element pre (cons last (gethash prefix my-hash)))
                 my-list)
           ))))
    (setq my-list
          (append
           (traverse-tree-node
            tree
            'make-element
            ) my-list))
    (setq my-list
          (delete-if (lambda (x)
                       (or (< (nth 3 x) 3);;count
                           (< (nth 2 x) 50);;%
                           ;;(eq ?w (char-syntax (nth 1 x)))
                           )) my-list);;count
          );;filter
    (setq my-list
          (sort my-list
                (lambda (x y)(> (nth 3 x) (nth 3 y)))));;count
    (setq my-list
          (sort my-list
                (lambda (x y)(> (nth 2 x) (nth 2 y)))));;%
    ;;branch cut filter
    (let ((tmp-list nil)
          (list))
      (setq my-list
            (delete-if
             'null (mapcar
                    (lambda(x)
                      (let ((all (vconcat (car x) (vector (nth 1 x)))))
                        (cond
                         ((and (my-memq all words)
                               (not (my-memq all tmp-list)))
                          (setq tmp-list
                                (append tmp-list (my-memq all words)))
                          (if skip-symbolp nil x))
                         ((and (my-memq all words)) nil)
                         (t x)
                         )))
                    my-list
                    )))
      );;end let
    (setq my-list (nreverse my-list))
    (let((temp-buffer-show-function 'switch-to-buffer))
      (with-output-to-temp-buffer "*n-gram*"
        (n-gram-print my-list reversep)
        )
      )
    )
  )

(defun n-gram (n &optional reversep skip-symbolp)
  (interactive "nInput n of n-gram: ")
  (run-with-idle-timer 0 nil 'n-gram1 n reversep skip-symbolp)
  ;;(n-gram1 n reversep skip-symbolp)
  )
;;(vconcat '[a b] (vector 'c))
;;(n-gram 3)
;;(n-gram 3 t)
;;(n-gram 5)
;;(n-gram 5 nil t);;symbol-skip
;;'((a . b) (d . e) (d . a) (d . c) (d . c) (d . e) (e . c) )
;;(memq nil '(nil b))

;;(my-memq '(nil a) '(nil a))

;;(append '(a b) '(c d))
(provide 'n-gram)
;;; n-gram.el ends here