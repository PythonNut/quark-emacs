;; -*- lexical-binding: t -*-
(eval-when-compile (require 'cl-lib))

(defmacro my/defun-as-value (name arglist &optional docstring &rest body)
  (declare (doc-string 3) (indent 2))
  `(progn
     (defun ,name ,arglist ,docstring ,@body)
     #',name))

;; basically, a mapcar for macros
(defmacro my/generate-calls (operator arglists)
  (declare (indent 1))
  `(progn
     ,@(mapcar (lambda (arglist) `(,(cadr operator) ,@arglist)) (cadr arglists))))

(defmacro my/generate-calls-single (operator arglist)
  (declare (indent 1))
  `(progn
     ,@(mapcar (lambda (arg) `(,(cadr operator) (,@arg))) (cadr arglist))))

(defmacro my/onetime-setup (name &optional docstring &rest body)
  (declare (doc-string 3) (indent 1))
  (let ((func-name (intern (concat "my/"
                                   (symbol-name name)
                                   "-onetime-setup")))
        keyw hook condition after-hook)
    (unless (stringp docstring)
      (push docstring body)
      (setq docstring nil))
    (while (keywordp (setq keyw (car body)))
      (setq body (cdr body))
      (pcase keyw
        (`:hook (setq hook (pop body)))
        (`:after-hook (setq after-hook (pop body)))
        (`:condition (setq condition (pop body)))
        (_  (error (format "Unrecognized keyword")))))
    (cl-assert hook nil ":hook not specified!")
    `(progn
       (defun ,func-name nil ,docstring
              ,@(if condition
                    `((when ,condition
                        ,@body
                        (remove-hook ,hook #',func-name)))
                  `(,@body
                    (remove-hook ,hook #',func-name)))
              )
       ,@(if after-hook
             (let ((setup-func-name
                    (intern (concat "my/setup-"
                                    (symbol-name name)
                                    "-onetime-setup"))))
               `((defun ,setup-func-name
                     ()
                   ,(concat "setup my/"
                            (symbol-name name)
                            "-onetime-setup")
                   (add-hook ,hook #',func-name))
                 (add-hook ,after-hook #',setup-func-name)))
           `((add-hook ,hook #',func-name))))))

(provide 'config-macros)
