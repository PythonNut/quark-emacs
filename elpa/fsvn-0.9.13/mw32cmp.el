;;; -*- Coding: utf-8 -*-
;;; mw32cmp.el --- Meadow compatibility for NTEmacs.
;;
;; Run following command in expanded directory.
;; svn export http://svn.meadowy.org/Meadow/trunk/lisp/international/mw32script.el ./mw32script.el
;;

;;; History:
;; 

;;; Commentary:
;;   
;;  Implemented by Elisp + reg.exe

;; TODO
;;  code cleaning
;;  Investigate Meadow's all extended function.

;;; Code:



(require 'mw32script)
(mw32script-make-pathext-regexp)



(defconst mw32cmp-function-alist
  '(
    (mw32-registry-get . mw32cmp-pseudo-registry-get)
    (mw32-registry-create-key . mw32cmp-pseudo-registry-create-key)
    (mw32-registry-delete-key . mw32cmp-pseudo-registry-delete-key)
    (mw32-registry-delete-value . mw32cmp-pseudo-registry-delete-value)
    (mw32-registry-list-keys . mw32cmp-pseudo-registry-list-keys)
    (mw32-registry-list-values . mw32cmp-pseudo-registry-list-values)
    (mw32-registry-set . mw32cmp-pseudo-registry-set)
    (mw32-registry-words-to-integer . mw32cmp-pseudo-registry-words-to-integer)
    (dos-to-unix-filename . mw32cmp-pseudo-dos-to-unix-filename)
    (unix-to-dos-filename . mw32cmp-pseudo-unix-to-dos-filename)
    (unix-to-dos-argument . mw32cmp-pseudo-unix-to-dos-argument)))

;; non `defun' for testing in meadow
(unless (featurep 'meadow)
  (mapc
   (lambda (x)
     (defalias (car x) (cdr x)))
   mw32cmp-function-alist))



;; external definitions
(defvar default-terminal-coding-system)

(defconst mw32cmp-registry-type-alist
  '(
    (registry-binary                     "REG_BINARY"                     mw32cmp-registry-parse-binary)
    (registry-dword                      "REG_DWORD"                      mw32cmp-registry-parse-dword)
    (registry-dword-little-endian        "REG_DWORD"                      mw32cmp-registry-parse-dword)
    (registry-expand-sz                  "REG_EXPAND_SZ"                  mw32cmp-registry-parse-expand-sz)

    (registry-multi-sz                   "REG_MULTI_SZ"                   mw32cmp-registry-parse-multi-sz)
    ;;todo reg.exe return reg_none but regedit display this key as reg_dword
    (registry-qword                       "REG_NONE"                       mw32cmp-registry-parse-qword)
;;    (registry-none                       "REG_NONE"                       mw32cmp-registry-parse-none)
    (registry-qword                      "REG_QWORD"                      mw32cmp-registry-parse-qword)
    (registry-qword-little-endian        "REG_QWORD"                      mw32cmp-registry-parse-qword)
    (registry-sz                         "REG_SZ"                         mw32cmp-registry-parse-sz)

    (registry-link                       "REG_LINK"                       mw32cmp-registry-parse-link)
    (registry-dword-big-endian           "REG_DWORD_BIG_ENDIAN"           mw32cmp-registry-parse-dword-big-endian)
    (registry-resource-list              "REG_RESOURCE_LIST"              mw32cmp-registry-parse-resource-list)
    (registry-full-resource-descriptor   "REG_FULL_RESOURCE_DESCRIPTOR"   mw32cmp-registry-parse-full-resource-descriptor)
    (registry-resource-requirements-list "REG_RESOURCE_REQUIREMENTS_LIST" mw32cmp-registry-parse-resource-requirements-list)
    ))

(defvar mw32cmp-pseudo-registry-default-name
  (let ((locale (w32-get-current-locale-id)))
    (cond
     ((= locale 1041)
      "<名前なし>")
     (t
      (error "Not supported locale"))))
  "reg.exe specification cannot determine key default value.")

(defmacro mw32cmp-pseudo-registry-narrow (key &rest form)
  (declare (indent 1))
  `(let ((MW32CMP-KEY-REGEX (concat "^" (regexp-quote ,key)))
         MW32CMP-START MW32CMP-END)
     (goto-char (point-min))
     (unless (re-search-forward (concat MW32CMP-KEY-REGEX "$") nil t)
       (error "failed searcy key %s" key))
     (setq MW32CMP-START (1+ (point)))
     (setq MW32CMP-END
           (or (and (re-search-forward MW32CMP-KEY-REGEX nil t)
                    (line-beginning-position))
               (point-max)))
     (narrow-to-region MW32CMP-START MW32CMP-END)
     (goto-char (point-min))
     ,@form))

;;todo registry key and value contains space?
;;todo REG_SZ like string must be contains in this regexp.
(defconst mw32cmp-pseudo-registry-value-regexp
  "^[ \t]+\\([^ \t]+\\)[ \t]+\\([^ \t]+\\)[ \t]+\\(.*\\)")

(defun mw32cmp-pseudo-registry-check ()
  (unless (executable-find "reg")
    (error "Unable get regisotry.")))

(defun mw32cmp-pseudo-registry-get (key &optional name)
  (mw32cmp-pseudo-registry-check)
  (mw32cmp-pseudo-check-string key)
  (if name
      (let ((args (list "query" key))
            (name-regex mw32cmp-pseudo-registry-value-regexp)
            ret reg)
        (setq args (append args (list "/v" name)))
        (with-temp-buffer
          (unless (= (apply 'call-process "reg" nil t nil args) 0)
            (signal 'error (list "Cannot open registry key" key)))
          (mw32cmp-pseudo-registry-narrow key
            (while (null ret)
              (when (looking-at name-regex)
                (setq reg (mw32cmp-pseudo-registry-matched-name-object))
                (when (string= (nth 0 reg) name)
                  (setq ret (nthcdr 1 reg))))
              (forward-line 1)
              (when (eobp)
                (setq ret t)))))
        (if (eq ret t) nil ret))
    ;; get default value using another process
    (cdr (assoc "" (mw32cmp-pseudo-registry-list-values key t)))))

(defun mw32cmp-pseudo-registry-list-keys (key &optional with-data)
  (mw32cmp-pseudo-registry-check)
  (mw32cmp-pseudo-check-string key)
  (let (args regexp ret matcher)
    (setq args (list "query" key))
    (setq matcher
          (if with-data
              (lambda () 
                (let ((key (match-string 0))
                      (subkey (match-string 1)))
                  (cons subkey (mw32cmp-pseudo-registry-get key))))
            (lambda () (match-string 1))))
    (with-temp-buffer
      (unless (= (apply 'call-process "reg" nil t nil args) 0)
        (signal 'error (list "Cannot open registry key" key)))
      (setq regexp (concat "^" (regexp-quote key) "\\\\?\\([^\\\\\n]+\\)$"))
      (goto-char (point-min))
      (while (not (eobp))
        (when (looking-at regexp)
          (setq ret (cons (funcall matcher) ret)))
        (forward-line 1)))
    (nreverse ret)))

(defun mw32cmp-pseudo-registry-list-values (key &optional with-data)
  (mw32cmp-pseudo-registry-check)
  (mw32cmp-pseudo-check-string key)
  (let (args matcher)
    (setq args (list "query" key))
    (with-temp-buffer
      (unless (= (apply 'call-process "reg" nil t nil args) 0)
        (signal 'error (list "Cannot open registry key" key)))
      (setq matcher
            (if with-data
                'mw32cmp-pseudo-registry-matched-name-object
              'mw32cmp-pseudo-registry-matched-name))
      (mw32cmp-pseudo-registry-narrow key
        (let ((name-regex mw32cmp-pseudo-registry-value-regexp)
              ret)
          (while (not (eobp))
            (when (looking-at name-regex)
              (setq ret (cons (funcall matcher) ret)))
            (forward-line 1))
          (nreverse ret))))))

(defun mw32cmp-pseudo-registry-words-to-integer (lword hword &optional hhword hhhword)
  (error "Not implemented yet"))

(defun mw32cmp-pseudo-registry-create-key (key)
  (mw32cmp-pseudo-registry-check)
  (let (args)
    (setq args (list "add" key "/f"))
    (mw32cmp-call-reg-update-process args)))

(defun mw32cmp-pseudo-registry-delete-key (key)
  (mw32cmp-pseudo-registry-check)
  (mw32cmp-pseudo-check-string key)
  (let (args)
    (setq args (list "delete" key "/f"))
    (or (mw32cmp-call-reg-update-process args)
        (signal 'error 
                (cons "Invalid registry key" 
                      (mw32cmp-registry-pseudo-parse-key key))))))

(defun mw32cmp-pseudo-registry-delete-value (key name)
  (mw32cmp-pseudo-registry-check)
  (mw32cmp-pseudo-check-string key)
  (mw32cmp-pseudo-check-string name)
  (let (args)
    (setq args (list "delete" key "/f" "/v" name))
    (or (mw32cmp-call-reg-update-process args)
        (let ((parsed (mw32cmp-registry-pseudo-parse-key key)))
          (signal 'error (cons "Invalid registry name" 
                               (cons (car parsed) 
                                     (cons (cdr parsed) name))))))))

(defun mw32cmp-pseudo-registry-set (key name data)
  (mw32cmp-pseudo-registry-check)
  (mw32cmp-pseudo-check-string key)
  (mw32cmp-pseudo-check-string name)
  (mw32cmp-pseudo-check-list data)
  (let ((parsed (mw32cmp-registry-pseudo-parse-data data))
         args)
    (setq args (list "add" key "/f"
                     "/v" name 
                     "/t" (car parsed)
                     "/d" (cdr parsed)))
    ;; BUG Create key under non-existent key meadow version throw error.
    ;;     but reg.exe create full path to key.
    ;;TODO only works string
    (mw32cmp-call-reg-update-process args)))

(defun mw32cmp-pseudo-check-string (data)
  (unless (stringp data)
    (signal 'wrong-type-argument (list 'stringp data))))

(defun mw32cmp-pseudo-check-list (data)
  (unless (listp data)
    (signal 'wrong-type-argument (list 'listp data))))

(defun mw32cmp-call-reg-update-process (args)
  (= (apply 'call-process "reg" nil (current-buffer) nil args) 0))

(defun mw32cmp-registry-pseudo-symbol-to-type (symbol)
  (or (cadr (assq symbol mw32cmp-registry-type-alist))
      "REG_SZ"))

(defun mw32cmp-registry-pseudo-parse-key (key)
  "return cons cell (rootkey . subkey)"
  (when (stringp key)
    (when (string-match "^\\([^\\\\]+\\)\\(?:\\\\\\(.*\\)\\)?" key)
      (cons (match-string 1 key) (match-string 2 key)))))

(defun mw32cmp-registry-pseudo-parse-data (data)
  (let* ((sym (cdr data))
         (type (mw32cmp-registry-pseudo-symbol-to-type sym)))
    (cond
     ((memq sym '(registry-sz registry-expand-sz))
      (unless (stringp (car data))
        (signal 'wrong-type-argument (car data)))
      (cons type (car data)))
     ((eq sym 'registry-multi-sz)
      (unless (listp (car data))
        (signal 'wrong-type-argument (car data)))
      ;;FIXME see reg add /? about \0. Consider data contains that string.
      (cons type (mapconcat 'identity (car data) "\\0")))
     (t
      ;;TODO
      (cons type (car data))))))

(defsubst mw32cmp-pseudo-registry-matched-name-object ()
  (let ((reg-name (match-string 1))
        (reg-type (match-string 2))
        (reg-value (match-string 3))
        name type value reg-define)
    (setq name (mw32cmp-pseudo-registry-key-name reg-name))
    (setq reg-define (mw32cmp-registry-type-converter reg-type))
    (setq type (nth 0 reg-define))
    (setq value (funcall (nth 2 reg-define) reg-value))
    (cons name (when value (cons value type)))))
  
(defsubst mw32cmp-pseudo-registry-matched-name ()
  (let ((reg-name (match-string 1))
        name)
    (setq name (mw32cmp-pseudo-registry-key-name reg-name))
    name))

(defsubst mw32cmp-pseudo-registry-key-name (reg-name)
  (if (string= reg-name mw32cmp-pseudo-registry-default-name)
      "" 
    reg-name))

(defun mw32cmp-registry-parse-binary (value) 
  (let ((i 0)
        (len (length value))
        hex char list)
    (while (< i len)
      (setq hex (substring value i (+ i 2)))
      (setq char (string-to-number hex 16))
      (setq list (cons char list))
      (setq i (+ i 2)))
    (concat (nreverse list))))

(defun mw32cmp-registry-parse-dword (value) 
  (unless (string-match "^0x\\([0-9a-z]+\\)$" value)
    (signal 'error (list "Not a hex value" value)))
  (let ((hex (match-string 1 value))
        num val)
    (setq num (string-to-number hex 16))
    (cons (floor num 65536) (floor (mod num 65536)))))

(defun mw32cmp-registry-parse-dword-big-endian (value) value)
(defun mw32cmp-registry-parse-expand-sz (value) value)
(defun mw32cmp-registry-parse-link (value) value)
(defun mw32cmp-registry-parse-multi-sz (value) 
  (split-string value "\\\\0" t))
(defun mw32cmp-registry-parse-none (value) value)
(defun mw32cmp-registry-parse-qword (value) 
  (let ((i 0)
        (len (length value))
        hhex lhex list)
    (while (< i len)
      ;;todo big-endian/little-endian system dependent value?
      (setq hhex (substring value i (+ i 2)))
      (setq lhex (substring value (+ i 2) (+ i 4)))
      (setq list (cons (string-to-number (concat lhex hhex) 16) list))
      (setq i (+ i 4)))
    list))

(defun mw32cmp-registry-parse-resource-list (value) value)
(defun mw32cmp-registry-parse-sz (value)
  ;;FIXME correct variable?
  (encode-coding-string value default-terminal-coding-system))
(defun mw32cmp-registry-parse-full-resource-descriptor (value) value)
(defun mw32cmp-registry-parse-resource-requirements-list (value) value)

(defun mw32cmp-registry-type-converter (type)
  (let* ((alist mw32cmp-registry-type-alist)
         (defined
           (catch 'found
             (mapc
              (lambda (cell)
                (when (string= (nth 1 cell) type)
                  (throw 'found cell)))
              alist)
             nil)))
    (unless defined
      (error "No matched registry type"))
    defined))

(defun mw32cmp-pseudo-dos-to-unix-filename (filename)
  (replace-regexp-in-string "\\\\" "/" filename))

(defun mw32cmp-pseudo-unix-to-dos-filename (file)
  (replace-regexp-in-string "/" "\\\\" file))

(defun mw32cmp-pseudo-unix-to-dos-argument (filename ep h2sp qp s2isp)
  ""
  (let (ret qf)
    (mapc
     (lambda (c)
       (if (and (eq c ?\\) ep)
           (setq ret (cons c ret))
         (if qf
             (if (eq c qf)
                 (setq qf nil)
               (setq ret (cons c ret)))
           (cond
            ((eq c ?/)
             (setq ret (cons (if s2isp ?\\ c) ret)))
            ((eq c ?=)
             (setq ret (cons (if h2sp ?/ c) ret)))
            ((memq c '(?/ ?\"))
             (if qp
                 (setq qf c)
               (setq ret (cons c ret))))
            (t
             (setq ret (cons c ret)))))))
     (string-to-list filename))
    (concat (nreverse ret))))



(provide 'mw32cmp)

;;; mw32cmp.el ends here
