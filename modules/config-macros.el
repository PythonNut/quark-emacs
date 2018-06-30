;; -*- lexical-binding: t -*-
(defmacro my/defun-as-value (name arglist &optional docstring &rest body)
  (declare (doc-string 3) (indent 2))
  `(progn
     (defun ,name ,arglist ,docstring ,@body)
     #',name))

(provide 'config-macros)
