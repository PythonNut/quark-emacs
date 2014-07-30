;;; Compiled snippets and support files for `snippet-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'snippet-mode
  '(("field" "\\${${1:${2:n}:}$3${4:\\$(${5:lisp-fn})}\\}$0" "${ ...  } field" nil nil nil nil nil nil)
     ("mirror" "\\${${2:n}:${4:\\$(${5:reflection-fn})}\\}$0" "${n:$(...)} mirror" nil nil nil nil nil nil)
     ("vars" "# name: $1${2:\n# key: ${3:trigger-key}}${4:\n# keybinding: ${5:keybinding}}${6:\n# expand-env: (${7:})}\n# contributor: $6\n# --\n$0" "Snippet header" nil nil nil nil nil nil)))


;;; Do not edit! File generated at Tue May  6 17:54:00 2014
