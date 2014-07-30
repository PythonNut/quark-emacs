;; -*- encoding: utf-8-unix; -*-
;; File-name:    <indent-hint.el>
;; Create:       <2012-09-10 12:04:07 ran9er>
;; Time-stamp:   <2013-02-17 00:35:27 ran9er>
;; Mail:         <2999am@gmail.com>

;; *init
(setq ih-key 'ih
      ih-bg 'indent-hint-bg
      ih-overlay-pool nil
      ih-table nil
      )

(defun ih-init(&optional l)
  (mapc
   (lambda(x) (or (local-variable-p x)
              (make-local-variable x)))
   '(ih-table ih-overlay-pool))
  (setq ih-table (make-hash-table :size 100 :test 'equal))
  (ih-bgo-init)
  (add-hook 'post-command-hook 'ih-bgo-mv t t)
  (font-lock-fontify-buffer))

;; *xpm
(defun ih-make-xpm (width height color &optional lor)
  (let* ((w width)
         (h height)
         (s1 (concat "\"" (make-string w (string-to-char " ")) "\""))
         (s2 (cond
              ((eq lor 0)
               (concat "\"." (make-string (1- w) (string-to-char " ")) "\""))
              ((eq lor 1)
               (concat "\"" (make-string (1- w) (string-to-char " ")) ".\""))
              ((null lor)
               (concat "\"" (make-string (- (1- w)(/ (1- w) 2))(string-to-char " "))
                       "." (make-string (/ (1- w) 2)(string-to-char " ")) "\""))))
         (sa (concat s1 ",\n" s2 ",\n")))
    (eval `(concat "/* XPM */
static char * dot_vline_xpm[] = {
\"" (number-to-string w) " " (number-to-string h) " 2 1\",
\"  c None\",
\". c " color "\",\n"
,@(mapcar (lambda(x) sa)
          (make-list (1- (/ h 2)) 0))
s1 ",\n" s2 "};"
))))

(defvar ih-line-height (or (car (window-line-height)) 20))
(defvar ih-img (ih-make-xpm 9 ih-line-height "#4D4D4D"))
(defvar ih-img-lgc (ih-make-xpm 9 ih-line-height "#5d478b"))
(defvar ih-img-mtd (ih-make-xpm 9 ih-line-height "khaki"))
(defvar ih-img-dat (ih-make-xpm 9 ih-line-height "#008b45"))


;; *overlay
;; (defun ih-make-overlay (b e)
;;   (let* ((p 'ih-overlay-pool)
;;          (q (eval p))
;;          (ov (or (car (prog1 q (set p (cdr q))))
;;                  (make-overlay b e))))
;;     (move-overlay ov b e)
;;     ov))
(defun ih-make-overlay (b e)
  (let* ((p 'ih-overlay-pool)
         (q (eval p))
         (ov (car q)))
    (if ov
        (progn
          (set p (cdr-safe q))
          (move-overlay ov b e))
      (setq ov (make-overlay b e)))
    ov))

(defun ih-delete-overlay (o)
  (let ((ov o)
        (p 'ih-overlay-pool))
    ;; (overlay-put ov ih-key nil)
    (delete-overlay ov)
    (set p (cons ov (eval p)))))
(defun ih-overlay-exist (k p q)
  (let (r o (l (overlays-in p q)))
    (while (and l
                (null
                 (if (overlay-get (setq o (car l)) k)
                     (setq r t)
                   nil)))
      (setq l (cdr l)))
    (if r o)))
(defun ih-make-head()
  (make-temp-name ""))
;; (defun ih-make-head1()
;;   (let* ((p (point))
;;          (q (1+ p))
;;          o)
;;     (or
;;      (setq o (ih-overlay-exist ih-head p q))
;;      (progn
;;        (setq o (ih-make-overlay p p))
;;        (overlay-put o ih-head t)))
;;     o))

;; *table
(defun ih-put (k v)
  (let ((h ih-table))
    (puthash k v h)))
(defun ih-get (k)
  (let ((h ih-table))
    (gethash k h)))
(defun ih-rem (k)
  (let ((h ih-table))
    (remhash k h)
    ;; debug
    ;; (ih-table-length)
    ))

;; *count-line
(defun ih-count-line(&optional pos)
  (let* ((p (or pos (point)))
         (c (save-excursion
              (goto-char p)
              (current-column)))
         (x 0)(r 0) w)
    (save-excursion
      (while
          (and (> (point-max)(line-end-position))
               (or
                (and (ih-white-line)
                     (setq x (1+ x)
                           w (cons r w)))
                (and (< c (current-indentation))
                     (setq x 0))))
        (forward-line)
        (move-to-column c)
        (setq r (1+ r))))
    (cons (- r x) (nthcdr x w))))

;; *white line
(defun ih-white-line()
  (save-excursion
    (move-to-column (current-indentation))
    (eolp)))

(defun ihwl-create()
  )
(defun ihwl-destroy()
  )
(defun ihwl-insert(col k &optional img color)
  )
(defun ihwl-delete()
  )
;; *draw-indent-hint-line
(defun draw-indent-hint-line (&optional column img color)
  (interactive "P")
  (save-excursion
    (let* ((i (or column (current-indentation)))
           (h (ih-make-head))
           (m (progn (forward-line)
                     (move-to-column i)
                     (ih-count-line)))
           (x (car m))(y (cdr m)) lst)
      (if (> x 0)
          (progn
            (kill-indent-hint (point))
            (dotimes (n x)
              (if (memq n y)
                  (ihwl-insert i h img color)
                (setq lst (cons (draw-indent-hint (point) h img color) lst)))
              (forward-line)
              (move-to-column i))
            (ih-put h (cons y lst)))))))

;; *draw-indent-hint
(defun draw-indent-hint (pos id &optional img color)
  (let* ((img (or img ih-img))
         (color (or color "#4D4D4D"))
         (ov (ih-make-overlay pos (1+ pos))))
    (overlay-put ov ih-key id)
    ;; (overlay-put ov evaporate t)
    (funcall draw-indent-hint-func ov img color)
    ov))

(setq draw-indent-hint-func
      (if (display-images-p)
          (lambda(o img color)
            (overlay-put o 'display
                         `(display (image
                                    :type xpm
                                    :data ,img
                                    :pointer text
                                    :ascent center
                                    :mask (heuristic t))
                                   rear-nonsticky (display)
                                   fontified t)))
        (lambda(o img color)
          (overlay-put o 'display
                       "|"))))

;; *erase-indent-hint
(defun kill-indent-hint (m &optional n)
  (let ((n (or n (1+ m))))
    (mapc
     (lambda(x)(let ((i (overlay-get x ih-key)))
             (if i
                 (progn
                   (mapc
                    (lambda(y)(ih-delete-overlay y))
                    (cdr (ih-get i)))
                   ;; (mapc
                   ;;  (lambda(x)(ihwl-delete x))
                   ;;  (car (ih-get i)))
                   (ih-rem i)
                   ))))
     (overlays-in m n))))
(defun erase-indent-hint (overlay after? beg end &optional length)
  (let ((inhibit-modification-hooks t)
        p1 p2)
    (if after?
        (save-excursion
          (forward-line)
          ;; (setq p1 (point))
          (setq p1 (line-beginning-position)
                p2 (+ p1 (current-indentation)))
          (kill-indent-hint p1 p2)
          (font-lock-fontify-block))
      (setq p1 (line-beginning-position) ;; (point)
            p2 (+ p1 (current-indentation)))
      (kill-indent-hint p1 p2))))

;; *background overlay
(defun ih-bgo-init (&optional r)
  (let* ((b (line-beginning-position))
         (e (+ b (current-indentation)))
         o)
    (setq r (or r 'ih-background-overlay))
    (make-local-variable r)
    (setq o (make-overlay b e))
    (overlay-put o ih-bg t)
    ;; debug
    ;; (overlay-put o 'face '((t (:background "grey40"))))
    (overlay-put o 'modification-hooks '(erase-indent-hint))
    (overlay-put o 'insert-in-front-hooks '(erase-indent-hint))
    (overlay-put o 'insert-behind-hooks '(erase-indent-hint))
    (set r o)))
(defun ih-bgo-mv(&optional o)
  (let* ((o (or o ih-background-overlay))
         (b (line-beginning-position))
         (e (+ b (current-indentation))))
    (move-overlay o b e)))
;; *interface
(defun indent-hint-current-column ()
  (save-excursion
    (goto-char (match-beginning 1))
    (current-column)))

(defun indent-hint (&optional regexp column img color)
  (interactive)
  (let ((x (or regexp "^")))
    (font-lock-add-keywords
     nil `((,x
            (0 (draw-indent-hint-line ,column ,img ,color)))))))

(defun indent-hint-mode (&optional lst l)
  (interactive)
  (let* ((c '(indent-hint-current-column))
         (lst (or lst '(("^[ \t]*\\([^ \t]\\)"))))
         (lst (if l lst (reverse lst))))
    (ih-init l)
    (dolist (x lst)
      (indent-hint (car x) c (cadr x)))))

;;;###autoload
(defun indent-hint-lisp ()
  (interactive)
  (indent-hint-mode
   '(("^[ \t]*\\((\\)")
     ("\\((lambda\\|(defun\\|(defmacro\\)" ih-img-mtd)
     ("\\((let\\*?\\|(if\\|(while\\|(cond\\|(and\\|(or\\|(map.*\\|(save-excursion\\)" ih-img-lgc)
     ("\\((setq\\|(defvar\\)" ih-img-dat)
     ("[,`#']+\\((\\)" ih-img-dat))))

;;;###autoload
(defun indent-hint-fixed(&optional img)
  (interactive)
  (indent-hint-mode
   `(( "^[ \t]*\\([^ \t]\\)"
       ,img))))

;;;###autoload
(defun indent-hint-js ()
  (interactive)
  (indent-hint-mode
   '(("^[ \t]*\\([^ \t}(]\\)")
     ("\\(function\\|var\\)" ih-img-mtd)
     ("\\(if\\|for\\|else\\|switch\\)" ih-img-lgc)
     ("^[ \t]*\\((\\)" ih-img-dat))
   #@2:t))

;;;###autoload
(defun indent-hint-test (&optional regexp)
  (interactive)
  (indent-hint (or regexp "\\(def\\|class\\|if\\)")
               '(indent-hint-current-column))
  (in-init))

;; **old
(defun indent-vline-lisp ()
  (interactive)
  (in-init)
  (let ((c '(indent-hint-current-column))
        (blk "\\((let\\*?\\|(if\\|(while\\|(cond\\|(map.*\\|(defun\\|(save-excursion\\)"))
    (if indent-hint-lazy
        (progn
          (indent-hint "^[ \t]*\\((\\)" c)
          (indent-hint "\\((lambda\\|(setq\\|(defvar\\)" c 'ih-img-lst)
          (indent-hint blk c 'ih-img-blk)
          (indent-hint "[,`#']+\\((\\)" c 'ih-img-lst))
      (indent-hint "[,`#']+\\((\\)" c 'ih-img-lst)
      (indent-hint blk c 'ih-img-blk)
      (indent-hint "\\((lambda\\|(setq\\|(defvar\\)" c 'ih-img-lst)
      (indent-hint "^[ \t]*\\((\\)" c))))

;; *debug
(defun ih-table-length()
  (interactive)
  (let ((l 0)
        (h ih-table))
    (maphash
     (lambda(x y)
       (setq l (1+ l)))
     h)
    (message (number-to-string l))))

(defun what-overlays (&optional p)
  (interactive)
  (print
   (let ((pt (or p (point))))
     (cons (cons pt (current-column))
           (mapcar
            (lambda(x) (remove-if
                    nil
                    `(,x
                      ,(overlay-get x ih-key)
                      ;; ,(if (overlay-get x ih-head) 'head)
                      ,(if (overlay-get x ih-bg) 'bg)
                      ,(if (eq (overlay-get x 'face) 'hl-line) 'hl-line))))
            (overlays-in pt (1+ pt)))))))

(when
nil
(what-overlays)
(length indent-hint-list)
(dolist (x indent-hint-list)
  (if (null (eval x))
      (and (unintern x)
           (setq indent-hint-list
                 (delq x indent-hint-list)))))
(setq overlay-no-buffer nil)
(dolist (x indent-hint-list)
  (dolist (y (eval x))
    (if (null (overlay-buffer y))
        (setq overlay-no-buffer
              (cons y overlay-no-buffer))))))
(provide 'indent-hint)
