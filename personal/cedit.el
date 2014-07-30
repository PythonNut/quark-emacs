;;; cedit.el --- paredit-like commands for c-like languages

;; Copyright (C) 2013 zk_phi

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA

;; Author: zk_phi
;; URL: http://hins11.yu-yake.com/
;; Version: 0.0.1

;;; Commentary:

;; Following commands are defined. Call them with "M-x foo", or bind some keys.

;; o cedit-forward-char / cedit-backward-char
;;   (in following examples, "|" are cursors)
;;
;;       fo|o; {bar;} baz;
;;   =>  foo|; {bar;} baz;
;;   =>  foo;| {bar;} baz;
;;   =>  foo; {bar;}| baz;
;;   =>  foo; {bar;} b|az;

;; o cedit-beginning-of-statement / cedit-end-of-statement
;;
;;       else{f|oo;}
;;   =>  else{|foo;} / else{foo;|}
;;
;;       els|e{bar;}
;;   =>  |else{bar;} / else{bar;}|

;; o cedit-down-block
;;
;;       wh|ile(cond){foo;}
;;   =>  while(cond){|foo;}

;; o cedit-up-block-forward / cedit-up-block-backward
;;
;;       if(cond){fo|o;}
;;   =>  |if(cond){foo;} / if(cond){foo;}|

;; o cedit-slurp
;;
;;       fo|o; bar;
;;   =>  fo|o, bar;
;;
;;       {fo|o;} bar;
;;   =>  {fo|o; bar;}

;; o cedit-wrap-brace
;;
;;       fo|o;
;;   =>  {fo|o;}

;; o cedit-barf
;;
;;       fo|o, bar;
;;   =>  fo|o; bar;
;;
;;       {fo|o; bar;}
;;   =>  {fo|o;} bar;

;; o cedit-splice-killing-backward
;;
;;       foo, ba|r, baz;
;;   =>  |bar, baz;
;;
;;       {foo; ba|r; baz;}
;;   =>  |bar; baz;

;; o cedit-raise
;;
;;       foo, ba|r, baz;
;;   =>  |bar;
;;
;;       {foo; ba|r; baz;}
;;   =>  |bar;

;; In addition, if "paredit.el" is installed on your emacs, following
;; commands are also defined.

;; o cedit-or-paredit-slurp
;; o cedit-or-paredit-barf
;; o cedit-or-paredit-splice-killing-backward
;; o cedit-or-paredit-raise

;; They are "dwim" commands that call one of cedit-xxx or paredit-xxx.

;;; Change Log:

;; 0.0.0 test release
;; 0.0.1 use require instead of autoload

;;; Code:

(eval-when-compile (require 'cl))

;; * constants

(defconst cedit-version "0.0.1")

;; * utilities

(defmacro cedit--move-iff-possible (&rest sexps)
  "try to eval sexps. point is moved only if succeeded."
  `(let ((old-point (point)))
     (condition-case err (progn ,@sexps)
       (error (goto-char old-point) (error (cadr err))))))

(defmacro cedit--save-excursion (&rest sexps)
  "eval sexps. point is not moved even when an error occurs."
  `(cedit--move-iff-possible
    (let ((val (progn ,@sexps)))
      (goto-char old-point)
      val)))

(defmacro cedit--orelse (first second)
  "try to eval the first sexp. if failed, the second sexp is evaled."
  `(condition-case err ,first (error ,second)))

(defmacro cedit--dowhile (prop &rest sexps)
  "simple do-while loop"
  `(progn ,@sexps
          (while ,prop (progn ,@sexps))))

(defun cedit--count-statements (beg end)
  "return number of statements in the region"
  (cedit--save-excursion
   (goto-char beg)
   (let ((cnt 0))
     (while (ignore-errors (cedit-end-of-statement))
       (setq cnt (1+ cnt)))
     cnt)))

(defun cedit--search-char-forward (chars)
  "* moves point even when fail
\(search ?r)
fo|o; (bar;) foobar;  =>  foo; (bar;) foobar|;
foo; (bar;) foobar|;  =>  ERROR
bar|; foobar;  =>  bar; foobar|;"
  (when (not (listp chars)) (setq chars (list chars)))
  (cedit--dowhile (not (member (char-before) chars))
                  (cedit--orelse (cedit-forward-char)
                                 (error "not found %s" chars)))
  (point))

(defun cedit--search-char-backward (chars)
  "* moves point even when fail
\(search ?f)
foo; (bar;) |foobar;  =>  |foo; (bar;) foobar;
|foo; (bar;) foobar;  =>  ERROR
foo; |foobar;  =>  |foo; foobar;"
  (when (not (listp chars)) (setq chars (list chars)))
  (cedit--dowhile (not (member (char-after) chars))
                  (cedit--orelse (cedit-backward-char)
                                 (error "not found %s" chars)))
  (point))

(defun cedit--this-statement-type ()
  (cedit--save-excursion
   (cedit-end-of-statement 'this)
   (case (char-before) (?\; 'statement) (?\} 'block))))

;; * motion commands

(defconst cedit--opening-parens '(?\{ ?\( ?\[))
(defconst cedit--closing-parens '(?\} ?\) ?\]))

;;;###autoload
(defun cedit-forward-char (&optional nest)
  "balanced forward-char / returns point
foo|; {bar;} baz;  =>  foo;| {bar;} baz;
foo;| {bar;} baz;  =>  foo; {bar;}| baz;
foo; {bar;|} baz;  =>  ERROR
foo; {bar;} baz;|  =>  ERROR"
  (interactive)
  (if (null nest) (setq nest 0))
  (cedit--move-iff-possible
   (skip-chars-forward "\s\t\n")
   (cond ((member (char-after) cedit--opening-parens)
          (setq nest (1+ nest)))
         ((member (char-after) cedit--closing-parens)
          (setq nest (1- nest))))
   (cond ((= (point) (point-max))
          (error "reached to EOF"))
         ((< nest 0)
          (error "reached to closing paren")))
   (forward-char)
   (when (> nest 0) (cedit-forward-char nest))
   (point)))

;;;###autoload
(defun cedit-backward-char (&optional nest)
  "balanced backward-char / returns point
foo; {bar;}| baz;  =>  foo; |{bar;} baz;
foo;| {bar;} baz;  =>  foo|; {bar;} baz;
foo; {|bar;} baz;  =>  ERROR
|foo; {bar;} baz;  =>  ERROR"
  (interactive)
  (if (null nest) (setq nest 0))
  (cedit--move-iff-possible
   (skip-chars-backward "\s\t\n")
   (cond ((member (char-before) cedit--closing-parens)
          (setq nest (1+ nest)))
         ((member (char-before) cedit--opening-parens)
          (setq nest (1- nest))))
   (cond ((= (point) (point-min))
          (error "reached to BOF"))
         ((< nest 0)
          (error "reached to opening paren")))
   (backward-char)
   (when (> nest 0) (cedit-backward-char nest))
   (point)))

;;;###autoload
(defun cedit-end-of-statement (&optional this)
  "goto end of statement
when THIS is non-nil, do not move to next statement
when fail, point is never moved
foo;| {bar;} baz;  =>  foo; {bar;}| baz;
foo; {bar;}| baz;  =>  foo; {bar;} baz;|
foo; {bar;} baz;|  =>  ERROR
foo; {bar;|} baz;  =>  ERROR"
  (interactive)
  (if (and this (member (char-before) '(?\; ?\})))
      ;; if THIS, and the point is EOS, just return point
      (point)
    ;; otherwise, search for next EOS
    (cedit--move-iff-possible
     (cedit--search-char-forward '(?\; ?\})))))

;;;###autoload
(defun cedit-beginning-of-statement (&optional this)
  "goto beginning of statement
when THIS is non-nil, do not move to previous statement
when fail, point is never moved
foo; {bar;} |baz;  =>  foo; |{bar;} baz;
foo; |{bar;} baz;  =>  |foo; {bar;} baz;
|foo; {bar;} baz;  =>  ERROR
foo; {|bar;} baz;  =>  ERROR"
  (interactive)
  (cedit--move-iff-possible
   ;; if THIS, goto end of this statement
   (when this (cedit-end-of-statement 'this))
   ;; goto previous BOS
   (cedit-backward-char)          ; fail if no statements are backward
   (when (ignore-errors
           (cedit--search-char-backward '(?\; ?\{)))
     (cedit-forward-char))
   (skip-chars-forward "\s\t\n"))
  (point))

;;;###autoload
(defun cedit-down-block ()
  "go down into block
|else{foo; bar;}  =>  else{|foo; bar;}
|foo;  =>  ERROR"
  (interactive)
  (cedit--move-iff-possible
   (when (not (eq (cedit--this-statement-type) 'block))
     (error "this statement is not a block"))
   (cedit-beginning-of-statement 'this)
   (search-forward "{")
   (skip-chars-forward "\s\t\n")))

;;;###autoload
(defun cedit-up-block-backward ()
  "go backward out of block.
if called at top-level, goto beginning of the first statement.
do{foo; bar; b|az;}  =>  |do{foo; bar; baz;}
 foo; bar; b|az;   =>   |foo; bar; baz;"
  (interactive)
  ;; goto beginning of the first statement
  (ignore-errors
    (while t (cedit-beginning-of-statement)))
  ;; go backward out of block if possible
  (ignore-errors
    (skip-chars-backward "\s\t\n")
    (backward-char)
    (cedit-beginning-of-statement 'this))
  (point))

;;;###autoload
(defun cedit-up-block-forward ()
  "go forward out of block.
if called at top-level, goto end of the last statement.
do{foo; bar; b|az;}  =>  do{foo; bar; baz;}|
 foo; bar; b|az;   =>   foo; bar; baz;|"
  (interactive)
  ;; goto end of the last statement
  (ignore-errors
    (while t (cedit-end-of-statement)))
  ;; go forward out of block if possible
  (ignore-errors
    (skip-chars-forward "\s\t\n")
    (forward-char)
    (cedit-end-of-statement 'this))
  (point))

;; * slurp command

(defun cedit--slurp-semi ()
  (cedit--save-excursion
   ;; foo;| bar;
   (cedit-end-of-statement 'this)
   (assert (= (char-before) ?\;))
   ;; foo|; bar;
   (let ((beg (1- (point))))
     ;; foo; bar;|
     (cedit-end-of-statement)
     (assert (= (char-before) ?\;))
     ;; foo; |bar;
     (cedit-beginning-of-statement 'this)
     ;; foo|bar;
     (delete-region beg (point))
     ;; foo, |bar;
     (insert ", "))))

(defun cedit--slurp-brace ()
  (cedit--save-excursion
   ;; foo; }| bar;
   (case (cedit--this-statement-type)
     (block (cedit-end-of-statement 'this))
     (statement (cedit-up-block-forward)))
   (assert (= (char-before) ?\}))
   ;; foo; |} bar;
   (let* ((beg (1- (point)))
          ;; foo; } |bar;
          (end (progn (cedit-end-of-statement) ; existence of next stmt is asserted here
                      (cedit-beginning-of-statement 'this))))
     ;; foo; |bar;
     (delete-region beg end)
     ;; foo; bar;|
     (cedit-end-of-statement)
     ;; foo; bar; }|
     (insert "\n}")
     (indent-region beg (point)))))

;;;###autoload
(defun cedit-slurp ()
  "slurp statement
{fo|o; bar;} baz;  =>  {fo|o, bar;} baz;
                   =>  {fo|o, bar; baz;}
                   =>  {fo|o, bar, baz;}"
  (interactive)
  (if (eq (cedit--this-statement-type) 'block)
      (cedit--slurp-brace)
    (cedit--orelse (cedit--slurp-semi)
                   (cedit--slurp-brace))))

;; * wrap command

;;;###autoload
(defun cedit-wrap-brace ()
  "wrap statement with brace
to wrap two or more statements, mark them"
  (interactive)
  (cedit--save-excursion
   (if (and transient-mark-mode mark-active)
       (let ((beg (region-beginning))
             (end (region-end)))
         (deactivate-mark)
         (goto-char beg)
         (insert "{\n")
         (goto-char (+ 2 end))
         (insert "\n}")
         (indent-region beg (point)))
     (cedit-beginning-of-statement 'this)
     (let ((beg (point)))
       (insert "{\n")
       (cedit-end-of-statement 'this)
       (insert "\n}")
       (indent-region beg (point))))))

;; * barf command

(defun cedit--barf-semi ()
  (cedit--save-excursion
   ;; f|oo, bar;
   (let ((beg (cedit-beginning-of-statement 'this))
         ;; foo, bar;|
         (end (cedit-end-of-statement 'this)))
     (assert (= (char-before) ?\;))
     ;; foo|, bar;
     (cedit--search-char-backward ?,)
     (when (< (point) beg)
       (error "no expressions to barf"))
     ;; foo| bar;
     (delete-char 1)
     ;; foo| bar;
     (delete-region (point)
                    (save-excursion (skip-chars-forward "\s\t\n")
                                    (point)))
     ;; foo|bar;
     (insert ";\n")
     ;; foo; bar;
     (indent-region beg (cedit-end-of-statement)))))

(defun cedit--barf-brace ()
  (cedit--save-excursion
   (when (eq (cedit--this-statement-type) 'block)
     (cedit-down-block))
   ;; fo|o; bar; }
   (let* ((beg (point))
          ;; foo; bar; }|
          (end (cedit-up-block-forward))
          ;; foo; bar;| }
          (stmt-end (progn (assert (= (char-before) ?\}))
                           (backward-char)
                           (1+ (cedit--search-char-backward ?\;))))
          ;; foo; |bar; }
          (stmt-beg (cedit-beginning-of-statement 'this)))
     ;; foo; |bar;
     (delete-region stmt-end end)
     ;; foo; }|bar;
     (insert "}\n")
     (indent-region beg (cedit-end-of-statement)))))

;;;###autoload
(defun cedit-barf ()
  "barf statement
{fo|o, bar; baz;}  =>  {fo|o; bar; baz;}
                   =>  {fo|o; bar;} baz;
                   =>  {fo|o;} bar; baz;"
  (interactive)
  (if (eq (cedit--this-statement-type) 'block.)
      (cedit--barf-brace)
    (cedit--orelse (cedit--barf-semi)
                   (cedit--barf-brace))))

;; * splice command

(defun cedit--splice-killing-backward-semi ()
  (let* ((beg (save-excursion
                (when (> (save-excursion (cedit-beginning-of-statement 'this))
                         (cedit--search-char-backward ?,)) ; may fail
                  (error "this is the first expression"))
                (forward-char)
                (skip-chars-forward "\s\t\n")
                (point)))
         (end (save-excursion
                (cedit-end-of-statement 'this)
                (assert (= (char-before) ?\;))
                (point))))
    (delete-region (cedit-beginning-of-statement 'this) beg)))

(defun cedit--splice-killing-backward-brace ()
  (let* ((beg (save-excursion
                (cedit-beginning-of-statement 'this)))
         (end (save-excursion
                ;; end of the last statement in this block
                (ignore-errors (while t (cedit-end-of-statement)))
                (point)))
         (str (buffer-substring beg end))
         (count (cedit--count-statements beg end)))
    (delete-region (save-excursion (cedit-up-block-backward))
                   (save-excursion (cedit-up-block-forward)))
    (indent-region (point)
                   (save-excursion (insert str) (point)))))

;;;###autoload
(defun cedit-splice-killing-backward ()
  "splice statements killing preceding statements
{foo; bar, b|az, foobar;}  =>  {foo; |baz, foobar;}
                           =>  {|baz, foobar;}
                           =>  baz, foobar;"
  (interactive)
  (cedit--orelse (cedit--splice-killing-backward-semi)
                 (cedit--splice-killing-backward-brace)))

;; * raise command

(defun cedit--raise-semi ()
  (let* ((beg (save-excursion
                (when (ignore-errors (cedit--search-char-backward '(?, ?\; ?\})))
                  (forward-char))
                (skip-chars-forward "\s\t\n")
                (point)))
         (end (save-excursion
                (cedit--search-char-forward '(?\; ?,))
                (1- (point))))
         (str (buffer-substring beg end)))
    (when (and (= beg (save-excursion (cedit-beginning-of-statement 'this)))
               (= end (1- (save-excursion (cedit-end-of-statement 'this)))))
      (error "cannot raise single expression"))
    (delete-region (save-excursion (cedit-end-of-statement 'this))
                   (cedit-beginning-of-statement 'this))
    (save-excursion (insert str ";"))))

(defun cedit--raise-brace (&optional beg end)
  (let* ((beg (or beg
                  (save-excursion (cedit-beginning-of-statement 'this))))
         (end (or end
                  (save-excursion (cedit-end-of-statement 'this))))
         (str (buffer-substring beg end)))
    (delete-region (save-excursion (cedit-up-block-backward))
                   (save-excursion (cedit-up-block-forward)
                                   (assert (= (char-before) ?\}))
                                   (point)))
    (indent-region (point)
                   (save-excursion (insert str) (point)))))

;;;###autoload
(defun cedit-raise ()
  "raise statement
{foo; bar, b|az, foobar;}  =>  {foo; |baz;}
                           =>  baz;
to raise statement, in case comma-expr is also able to be raise, mark it."
  (interactive)
  (if (and (interactive-p) transient-mark-mode mark-active)
      (let ((beg (region-beginning))
            (end (region-end)))
        (deactivate-mark)
        (cedit--raise-brace beg end))
    (cedit--orelse (cedit--raise-semi)
                   (cedit--raise-brace))))

;; * paredit

(when (require 'paredit nil t)

;;;###autoload
  (defun cedit-or-paredit-slurp ()
    "call cedit-slurp or paredit-forward-slurp-sexp"
    (interactive)
    (let ((pare (save-excursion
                  (ignore-errors (paredit-forward-up) (point))))
          (c (save-excursion
               (ignore-errors (cedit-end-of-statement 'this)))))
      (cond ((null c) (paredit-forward-slurp-sexp))
            ((null pare) (cedit-slurp))
            ((< pare c) (cedit--orelse (paredit-forward-slurp-sexp)
                                       (cedit-slurp)))
            (t (cedit--orelse (cedit-slurp)
                              (paredit-forward-slurp-sexp))))))

;;;###autoload
  (defun cedit-or-paredit-barf ()
    "call cedit-barf or paredit-backward-barf-sexp"
    (interactive)
    (let ((pare (save-excursion
                  (ignore-errors (paredit-forward-up) (point))))
          (c (save-excursion
               (ignore-errors (cedit-end-of-statement 'this)))))
      (cond ((null c) (paredit-backward-barf-sexp))
            ((null pare) (cedit-barf))
            ((< pare c) (cedit--orelse (paredit-backward-barf-sexp)
                                       (cedit-barf)))
            (t (cedit--orelse (cedit-barf)
                              (paredit-backward-barf-sexp))))))

;;;###autoload
  (defun cedit-or-paredit-splice-killing-backward ()
    "call cedit-splice-killing or paredit-splice-sexp-killing-backward"
    (interactive)
    (let ((pare (save-excursion
                  (ignore-errors (paredit-forward-up) (point))))
          (c (save-excursion
               (ignore-errors (cedit-end-of-statement 'this)))))
      (cond ((null c) (paredit-splice-sexp-killing-backward))
            ((null pare) (cedit-splice-killing-backward))
            ((< pare c) (cedit--orelse
                         (paredit-splice-sexp-killing-backward)
                         (cedit-splice-killing-backward)))
            (t (cedit--orelse
                (cedit-splice-killing-backward)
                (paredit-splice-sexp-killing-backward))))))

;;;###autoload
  (defun cedit-or-paredit-raise ()
    "call cedit-raise or paredit-raise-sexp"
    (interactive)
    (let ((pare (save-excursion
                  (ignore-errors (paredit-forward-up) (point))))
          (c (save-excursion
               (ignore-errors (cedit-end-of-statement 'this)))))
      (cond ((null c) (paredit-raise-sexp))
            ((null pare) (cedit-raise))
            ((< pare c) (cedit--orelse (paredit-raise-sexp)
                                       (cedit-raise)))
            (t (cedit--orelse (cedit-raise)
                              (paredit-raise-sexp))))))
  )

;; * provide

(provide 'cedit)

;;; cedit.el ends here
