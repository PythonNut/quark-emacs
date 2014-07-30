;;; livescript-mode.el --- Major mode for editing LiveScript files
;; Version: 20140612.2121

;; Copyright (C) 2012-2014 Hisamatsu Yasuyuki

;; Author  : Hisamatsu Yasuyuki <yas@null.net>
;; URL     : https://github.com/yhisamatsu/livescript-mode
;; Keywords: languages livescript
;; Version : 0.0.3

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A major mode for editing LiveScript code.

;;; Code:

(require 'font-lock)

(eval-when-compile (require 'cl))

;;
;; Group
;;

(defgroup livescript nil
  "Major mode for editing LiveScript code."
  :prefix "livescript-"
  :group 'languages)

(defgroup livescript-faces nil
  "Fontification colors."
  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
  :prefix "livescript-"
  :group 'livescript)

;;
;; Syntax table
;;

(defvar livescript-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry '(0 . 127) "_   " st)
    (dolist (range '((?0 . ?9) (?A . ?Z) (?a . ?z)))
      (modify-syntax-entry range "@   " st))
    (dolist (ch '(?\t ?\f ?\r ?\s))
      (modify-syntax-entry ch "-   " st))
    (modify-syntax-entry ?\( "()  " st)
    (modify-syntax-entry ?\) ")(  " st)
    (modify-syntax-entry ?\[ "(]  " st)
    (modify-syntax-entry ?\] ")[  " st)
    (modify-syntax-entry ?{  "(}  " st)
    (modify-syntax-entry ?}  "){  " st)
    (modify-syntax-entry ?#  "<   " st)
    (modify-syntax-entry ?\n ">   " st)
    (modify-syntax-entry ?*  "_ 23b" st)
    (modify-syntax-entry ?/ "$ 14" st)
    (dolist (ch '(?, ?: ?! ??))
      (modify-syntax-entry ch ".   " st))
    (modify-syntax-entry ?\" "\"\"   " st)
    (modify-syntax-entry ?'  "\"'   " st)
    (modify-syntax-entry ?`  "$   " st)
    (modify-syntax-entry ?@  "'   " st)
    (modify-syntax-entry ?\\ "\\   " st)
    st)
  "Syntax table in use in `livescript-mode' buffers.")

(defvar livescript-mode-abbrev-table nil)
(define-abbrev-table 'livescript-mode-abbrev-table ())

;;
;; Face
;;
(defface livescript-font-lock-bold-face
  '((t :inherit bold))
  "Font Lock mode face used to highlight interpolation in LiveScript regexps."
  :group 'livescript-faces)


(defface livescript-font-lock-shadow-face
  '((((class color grayscale) (min-colors 88) (background light))
     :foreground "grey30"
     :weight semi-bold)
    (((class color grayscale) (min-colors 88) (background dark))
     :foreground "grey70"
     :weight semi-bold)
    (t :inherit shadow))
  "Font Lock mode face used to highlight this/it/that in LiveScript regexps."
  :group 'livescript-faces)


;; Utility

(defun livescript--regexp-from-symbols (sequence)
  (concat "\\_<" (regexp-opt (mapcar #'symbol-name sequence) t) "\\>"))

(defun livescript--join-string (strings separator)
  (mapconcat #'identity strings separator))

;;
;; Search based highlighting
;;

(defvar livescript-keywords-regexp
  (let* ((js-keywords    [break catch class continue delete else extends
                                finally for if loop new return
                                super switch throw try until while])
         (cs-keywords    [by of own then unless when])
         (ls-keywords    [fallthrough otherwise from til to])
         (js/cs-reserved [case const debugger default do enum export
                               function import let native var void with
                               __extends __hasProp])
         (keywords (vconcat js-keywords cs-keywords ls-keywords js/cs-reserved)))
    (livescript--regexp-from-symbols keywords))
  "Regular expression to match ordinary keywords of LiveScript.")

(defvar livescript-boolean-operators-regexp
  (livescript--regexp-from-symbols [and in is isnt not or])
  "Regular expression to match boolean operators.")

(defvar livescript-builtins-regexp
  (livescript--regexp-from-symbols [instanceof typeof])
  "Regular expression to match 'instanceof' or 'typeof'.")

(defvar livescript-context-regexp
  (livescript--regexp-from-symbols [this it that])
  "Regular expression to match 'this', 'it' or 'that'.")

(defvar livescript-boolean-regexp
  (livescript--regexp-from-symbols [true false yes no on off null undefined])
  "Regular expression to match booleans themselves.")

(defvar livescript-property-regexp "\\(\\w+\\)\\s-*:"
  "Regular expression to match property names.")

(defvar livescript-negate-regexp "\\(\\w+\\s-*:[:=]\\|@@\\w*\\)"
  "Regular expression to negate highlighting.")

(defvar livescript-instance-regexp "\\(@\\w+\\)"
  "Regular expression to match instance variables.")

(defvar livescript-function-name-regexp
  (let* ((param     "\\s-*\\(?:\\w\\|\\.\\)+\\s-*")
         (default   "\\(?:\\(?:[:=?]\\|||\\)\\s-*\\|\\s-+or\\s-+\\)[^,\)]+?")
         (arg       (concat param "\\(?:" default "\\)?"))
         (args      (concat arg "\\(?:," arg "\\)*"))
         (arrow     "\\(?:--?\\|~~?\\)>")
         (anon-func (concat "!?\\s-*\\(?:(" args ")\\)?\\s-*" arrow))
         (func-name "\\(?1:\\w+\\)")
         ;; func = [(args)] ->
         (func-def1 (concat "\\_<" func-name "\\s-*[:=]\\s-*" anon-func))
         ;; function func [args] [(then|=>) ...]
         (func-def2 (concat "^\\s-*[!~]?\\s-*function\\s-+" func-name)))
    (format "\\(?:%s\\|%s\\)" func-def1 func-def2))
  "Regular expression to match function names.")

(defvar livescript-class-name-regexp
  "\\_<class\\s-+\\(?:exports\.\\)?\\(\\w+\\)"
  "Regular expression to match class names.")

(defun livescript-interpolation-matcher (bound)
  "Function to match interpolation."
  (catch 'found
    (while (re-search-forward
			"\\(#\\(?:{\\(?2:.*?\\)\\}\\|\\w+\\)\\)"
			bound t)
      (let ((face         (livescript--get-face   (1- (point))))
            (syntax-class (livescript--get-syntax (match-beginning 1))))
        (when (livescript--interpolatable-p face syntax-class)
          (throw 'found t))))))

(defvar livescript-heregex-face 'font-lock-constant-face)

(defun livescript-comment-inside-heregex-matcher (bound)
  "Function to match comment inside heregex."
  (catch 'found
    (while (re-search-forward "\\(#\\s-.*$\\)" bound t)
      (let ((face (livescript--get-face (1- (point)))))
        (when (memq livescript-heregex-face face)
          (throw 'found t))))))

(defun livescript--interpolatable-p (face syntax-class)
  (and (livescript--interpolatable-face-p         face)
       (livescript--interpolatable-syntax-class-p syntax-class)))

(defvar livescript-interpolatable-faces
  '(font-lock-string-face font-lock-constant-face))

(defun livescript--interpolatable-face-p (face)
  (catch 'found
    (dolist (f face)
      (when (memq f livescript-interpolatable-faces) (throw 'found t)))))

(defvar livescript-interpolatable-syntax-classes '(6))

(defun livescript--interpolatable-syntax-class-p (syntax-class)
  (not (and syntax-class
            (memq syntax-class livescript-interpolatable-syntax-classes))))

(defun livescript--get-face (point)
  (let ((face (get-text-property point 'face)))
    (if (listp face) face (list face))))

(defun livescript--get-syntax (point)
  (syntax-class (get-text-property point 'syntax-table)))

;;
;; Font lock keywords
;;

(defconst livescript-font-lock-keywords-1
  `((,livescript-negate-regexp         1 font-lock-negation-char-face)
    (,livescript-instance-regexp       1 font-lock-variable-name-face)
    (,livescript-function-name-regexp  1 font-lock-function-name-face)
    (,livescript-class-name-regexp     1 font-lock-type-face)
    (,livescript-property-regexp       1 font-lock-type-face)
    (,livescript-boolean-regexp        1 font-lock-constant-face)
    (,livescript-keywords-regexp       1 font-lock-keyword-face)))

(defconst livescript-font-lock-keywords-2
  (append livescript-font-lock-keywords-1
          `((,livescript-boolean-operators-regexp 1 font-lock-builtin-face)
            (,livescript-builtins-regexp          1 font-lock-builtin-face)
            (,livescript-context-regexp
             (1 'livescript-font-lock-shadow-face))
            (livescript-interpolation-matcher
             (1 'livescript-font-lock-bold-face prepend)
             (2 (livescript--font-lock-pop-bold-face) t t))
            (livescript-comment-inside-heregex-matcher
             (1 font-lock-comment-face prepend t)))))

(defun livescript--font-lock-pop-bold-face ()
  (let* ((pop-beginning-position (match-beginning 2))
         (face (get-text-property pop-beginning-position 'face)))
    (when face (remove 'livescript-font-lock-bold-face face))))

(defvar livescript-font-lock-keywords livescript-font-lock-keywords-1
  "Default `font-lock-keywords' of LiveScript mode.")

;;
;; Imenu support
;;

(defvar livescript-imenu-generic-expression
  `((nil ,livescript-function-name-regexp 1))
  "Imenu generic expression for LiveScript mode.")

;;
;; Commands
;;

(defvar livescript-mode-map
  (let ((map (make-sparse-keymap)))

    map)
  "Keymap used in LiveScript mode.")

;;
;; Syntax propertize
;;

(defconst livescript--conflicting-syntax-classes
  (let ((string-quote   7)
        (escape         9)
        (comment-start 11))
    (list comment-start string-quote escape))
  "List of syntax classes which can conflict with syntax-table property.")

(defun livescript--put-syntax (beg end syntax)
  "Set the syntax property on the current buffer to SYNTAX between BEG and END.
SYNTAX is a string which `string-to-syntax' accepts."
  (put-text-property beg end 'syntax-table (string-to-syntax syntax)))

(defun livescript--escape-syntax (beg end subst)
  (loop for i from beg to end
        do (let ((class (syntax-class (syntax-after i))))
             (when (memq class livescript--conflicting-syntax-classes)
               (livescript--put-syntax i (1+ i) subst)))))

(defun livescript--put-enclosing-syntax (beg end syntax &optional subst)
  (livescript--put-syntax beg (1+ beg) syntax)
  (when subst
    (livescript--escape-syntax (1+ beg) (1- end) subst))
  (livescript--put-syntax (1- end) end syntax))

(defun livescript--put-syntax-multiline (beg end syntax &optional subst)
  (put-text-property beg end 'syntax-multiline    t)
  (put-text-property beg end 'font-lock-multiline t)
  (livescript--put-enclosing-syntax beg end syntax subst))

(defun livescript--multiline-rule (open close syntax &optional subst)
  `(,(format "\\(%s[[:ascii:]]*?%s\\)" open (or close open))
    (1 (ignore
        (livescript--put-syntax-multiline
         (match-beginning 1) (match-end 1) ,syntax ,subst)))))

(defvar livescript--unclosed-positions nil
  "Unclosed literals and their positions.")

(defconst livescript-complex-syntax '(\'\'\' \"\"\" <\\\[ //)
  "List of symbols whose names are complex syntax elements.
Complex syntax elements are heredocument, string list and heregexp.")

(defun livescript--make-unclosed-positions ()
  (let ((tbl (make-hash-table :size 11)))
    (dolist (syntax livescript-complex-syntax)
      (puthash syntax nil tbl))
    tbl))

(defun livescript--make-syntax-propertize-function ()
  "Return a function used for highlighting codes syntactically."
  ;; prepare rules
  (let ((skip-comment '("\\(\\s<.*\\)$" (1 (ignore))))
        (heredoc1     (livescript--multiline-rule "'''"    nil "\"" "'"))
        (heredoc2     (livescript--multiline-rule "\"\"\"" nil "\"" "_"))
        (skip-string  '("\\([^\"'\\]\\s\"\\S\"+?\\s\"\\)" (1 (ignore))))
        (string-list  (livescript--multiline-rule "<\\[" "\\]>" "|"))
        (heregex      (livescript--multiline-rule "//"     nil "\"/")))
    ;; embed rules before calling macro
    (eval `(syntax-propertize-rules
            ,skip-comment
            ,heredoc1
            ,heredoc2
            ,skip-string
            ,string-list
            ,heregex

            ;; /regular-expression/
            ((concat "\\(?:^\\|\\W\\)"
                     "\\(/\\)"
                     "\\(?:\\\\.\\|\\[\\(?:\\\\.\\|.\\)+\\]\\|[^*/\n]\\)"
                     "\\(?:\\\\.\\|\\[\\(?:\\\\.\\|.\\)+\\]\\|[^/\n]\\)*"
                     "\\(/\\)"
                     "[gimy$?]\\{0,4\\}")
             (1 "\"/") (2 "\"/"))

            ;; \string
            ("\\(\\\\[^[:space:]\n][^]}\),;[:space:]\n]*\\)"
             (1 (ignore
                 (livescript--put-enclosing-syntax
                  (match-beginning 1) (match-end 1) "|" "'"))))

            ;; unclosed multiline literals
            ((let ((complex (mapcar #'symbol-name livescript-complex-syntax)))
               (concat "\\(" (livescript--join-string complex "\\|") "\\)"))
             (1 (ignore
                 (puthash (intern-soft (match-string 1)) (match-beginning 1)
                          livescript--unclosed-positions))))
            ))))

(defconst livescript-syntax-propertize-function
  (livescript--make-syntax-propertize-function))

(defvar livescript-syntax-propertize-extend-region-functions
  (append
   '(livescript-syntax-propertize-extend-region-function-1)
   '(livescript-syntax-propertize-extend-region-function-2)
   syntax-propertize-extend-region-functions))

(defun livescript-syntax-propertize-extend-region-function-1 (start end)
  "Fix the range of syntax propertization from START to END."
  (let* ((new-start start)
         (new-end end)
         (min-unclosed (livescript-minimum-unclosed)))
    (when min-unclosed
      (save-excursion
        (goto-char (cdr min-unclosed))
        (setq new-start (line-beginning-position)))
      (livescript--clear-unclosed-positions))
    (cons new-start new-end)))

(defun livescript-syntax-propertize-extend-region-function-2 (start end)
  "Fix the range of syntax propertization from START to END."
  (let ((new-start start)
        (new-end end))
    (save-excursion
      (goto-char start)
      (when (and (prog1 (zerop (forward-line -1)) (end-of-line))
                 (get-text-property (point) 'syntax-multiline))
        (setq new-start (point))))
    (cons new-start new-end)))

(defun livescript-minimum-unclosed ()
  "Return the position where the first unclosed syntax appears."
  (let (kv-alist)
    (maphash #'(lambda (k v) (when v (push (cons k v) kv-alist)))
             livescript--unclosed-positions)
    (when kv-alist
      (car (sort kv-alist #'(lambda (a b) (< (cdr a) (cdr b))))))))

(defun livescript--clear-unclosed-positions ()
  (maphash #'(lambda (k v) (puthash k nil livescript--unclosed-positions))
           livescript--unclosed-positions))

(defvar font-lock-beg)
(defun livescript-font-lock-extend-region-function ()
  (let ((min-unclosed (livescript-minimum-unclosed)))
    (when (and min-unclosed (< (cdr min-unclosed) font-lock-beg))
      (setq font-lock-beg (cdr min-unclosed)))))

(defvar livescript-font-lock-extend-region-functions
  (append
   '(livescript-font-lock-extend-region-function)
   font-lock-extend-region-functions))

(defun livescript-syntactic-face-function (state)
  "Return one of font-lock's basic face according to the parser's STATE.
STATE is a return value of `syntax-ppss'."
  (case (livescript--string-state state)
    ((nil) 'font-lock-comment-face)
    ((?/)  'font-lock-constant-face)
    (t     'font-lock-string-face)))

(defun livescript--string-state (state) (nth 3 state))

;;
;; Setup
;;

(defun livescript-mode-variables ()
  "Setup buffer-local variables for `livescript-mode'."
  ;; Syntax table
  (set-syntax-table livescript-mode-syntax-table)
  ;; Abbrev
  (setq local-abbrev-table livescript-mode-abbrev-table)
  ;; Comment
  (set (make-local-variable 'comment-start) "#")
  (set (make-local-variable 'comment-add) 1)
  ;; Imenu
  (setq imenu-case-fold-search t)
  (setq imenu-syntax-alist '(("-_$" . "w")))
  (setq imenu-generic-expression livescript-imenu-generic-expression)
  ;; Font-lock
  (set (make-local-variable 'font-lock-defaults)
       '((livescript-font-lock-keywords
          livescript-font-lock-keywords-1 livescript-font-lock-keywords-2)
         nil nil (("-_$" . "w")) nil
         (font-lock-syntactic-face-function
          . livescript-syntactic-face-function)
         (parse-sexp-lookup-properties . t)))
  ;; Syntactic fontification
  (set (make-local-variable 'livescript--unclosed-positions)
       (livescript--make-unclosed-positions))

  (set (make-local-variable 'syntax-propertize-extend-region-functions)
       livescript-syntax-propertize-extend-region-functions)
  (set (make-local-variable 'font-lock-extend-region-functions)
       livescript-font-lock-extend-region-functions)
  (set (make-local-variable 'syntax-propertize-function)
       livescript-syntax-propertize-function))

;;;###autoload
(define-derived-mode livescript-mode prog-mode "LiveScript"
  "Major mode for editing LiveScript code.

Commands:

\\{livescript-mode-map}"
  (livescript-mode-variables))

;;
;; Customize variables
;;

(defcustom livescript-mode-hook nil
  "Normal hook run when entering `livescript-mode'.
See `run-hooks'."
  :type 'hook
  :group 'livescript)

(defcustom livescript-program-name "livescript"
  "The command to evaluate LiveScript code."
  :type 'string
  :group 'livescript)


;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ls\\'" . livescript-mode))

(provide 'livescript-mode)


;;;###compile in emacs
;;; all this section is copy/paste awesome feature from
;;; https://github.com/defunkt/coffee-mode

(defcustom livescript-args-compile '("-c")
  "The arguments to pass to `livescript-command' to compile a file."
  :type 'list
  :group 'livescript)

(defcustom livescript-command "lsc"
  "The LivescriptScript command used for evaluating code."
  :type 'string
  :group 'livescript)

(defcustom livescript-compiled-buffer-name "*livescript-compiled*"
  "The name of the scratch buffer used for compiled LivescriptScript."
  :type 'string
  :group 'livescript)

(defun livescript-compile-region (start end)
  "Compiles a region and displays the JavaScript in a buffer called
`livescript-compiled-buffer-name'."
  (interactive "r")

  (let ((buffer (get-buffer livescript-compiled-buffer-name)))
    (when buffer
      (with-current-buffer buffer
        (erase-buffer))))

  (apply (apply-partially 'call-process-region start end
                          livescript-command nil
                          (get-buffer-create livescript-compiled-buffer-name)
                          nil)
         (append livescript-args-compile (list "-s" "-p" "-b")))

  (let ((buffer (get-buffer livescript-compiled-buffer-name)))
    (display-buffer buffer)
    (with-current-buffer buffer
      (let ((buffer-file-name "tmp.js")) (set-auto-mode)))))

(defun livescript-compile-buffer ()
  "Compiles the current buffer and displays the JavaScript in a buffer
called `livescript-compiled-buffer-name'."
  (interactive)
  (save-excursion
    (livescript-compile-region (point-min) (point-max))))

;;; livescript-mode.el ends here
