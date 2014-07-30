;;; Compiled snippets and support files for `c++-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'c++-mode
  '(("beginend" "${1:v}.begin(), $1.end" "v.begin(), v.end()" nil nil nil nil nil nil)
     ("class" "class ${1:Name}\n{\npublic:\n    ${1:$(yas-substr text \"[^: ]*\")}($2);\n    virtual ~${1:$(yas-substr text \"[^: ]*\")}();\n};" "class ... { ... }" nil nil nil nil nil nil)
     ("ns" "namespace" "namespace ..." nil nil nil nil nil nil)
     ("template" "template <typename ${T}>" "template <typename ...>" nil nil nil nil nil nil)
     ("using" "using namespace ${std};\n$0" "using namespace ..." nil nil nil nil nil nil)))


;;; Do not edit! File generated at Tue May  6 17:54:00 2014
