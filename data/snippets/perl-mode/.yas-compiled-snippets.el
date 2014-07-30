;;; Compiled snippets and support files for `perl-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'perl-mode
  '(("eval" "eval {\n    ${1:# do something risky...}\n};\nif (\\$@) {\n    ${2:# handle failure...}\n}" "eval { ... } if ($@) { ... }" nil nil nil nil nil nil)
     ("for" "for (my \\$${1:var} = 0; \\$$1 < ${2:expression}; \\$$1++) {\n    ${3:# body...}\n}" "for (...) { ... }" nil nil nil nil nil nil)
     ("fore" "foreach my \\$${1:x} (@${2:array}) {\n    ${3:# body...}\n}" "foreach ... { ... }" nil nil nil nil nil nil)
     ("if" "if ($1) {\n    $0\n}" "if (...) { ... }" nil nil nil nil nil nil)
     ("ife" "if ($1) {\n    $2\n} else {\n    $3\n}" "if (...) { ... } else { ... }" nil nil nil nil nil nil)
     ("ifee" "if ($1) {\n	${2:# body...}\n} elsif ($3) {\n	${4:# elsif...}\n} else {\n	${5:# else...}\n}" "if, elsif, else ..." nil nil nil nil nil nil)
     ("sub" "sub ${1:function_name} {\n    $0\n}" "sub ... { ... }" nil nil nil nil nil nil)
     ("unless" "unless ($1) {\n    $0\n}" "unless (...) { ... }" nil nil nil nil nil nil)
     ("while" "while ($1) {\n    $0\n}" "while (...) { ... }" nil nil nil nil nil nil)
     ("xfore" "${1:expression} foreach @${2:array};" "... foreach ..." nil nil nil nil nil nil)
     ("xif" "${1:expression} if ${2:condition}" "... if ..." nil nil nil nil nil nil)
     ("xunless" "${1:expression} unless ${2:condition}" "... unless ..." nil nil nil nil nil nil)
     ("xwhile" "${1:expression} while ${2:condition};" "... while ..." nil nil nil nil nil nil)))


;;; Do not edit! File generated at Tue May  6 17:54:00 2014
