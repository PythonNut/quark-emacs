;;; Compiled snippets and support files for `erlang-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'erlang-mode
  '(("after" "after\n    $1 -> $0" "after ... ->" nil nil nil nil nil nil)
     ("begin" "begin\n    $0\nend" "begin ... end" nil nil nil nil nil nil)
     ("beh" "-behaviour(${1:gen_server}).\n$0" "-behaviour(...)." nil nil nil nil nil nil)
     ("case" "case $1 of\n    $0\nend" "case ... of ... end" nil nil nil nil nil nil)
     ("compile" "-compile([${1:export_all}]).\n$0" "-compile(...)." nil nil nil nil nil nil)
     ("def" "-define($1,$2).\n$0" "-define(...,...)." nil nil nil nil nil nil)
     ("exp" "-export([${1:start/0}]).\n$0" "-export([])." nil nil nil nil nil nil)
     ("fun" "fun ($1) -> $0 end" "fun (...) -> ... end" nil nil nil nil nil nil)
     ("if" "if\n    $1 -> $2;\n    true -> $0\nend" "if ... -> ... ; true -> ... end" nil nil nil nil nil nil)
     ("ifdef" "-ifdef($1).\n$0\n-endif." "-ifdef(...). ... -endif." nil nil nil nil nil nil)
     ("ifndef" "-ifndef($1).\n$0\n-endif." "-ifndef(...). ... -endif." nil nil nil nil nil nil)
     ("imp" "-import(${1:lists}, [${2:map/2, sum/1}]).\n$0" "-import([])." nil nil nil nil nil nil)
     ("inc" "-include(\"$1\").\n$0" "-include(\"...\")." nil nil nil nil nil nil)
     ("inc" "-include_lib(\"$1\").\n$0" "-include_lib(\"...\")." nil nil nil nil nil nil)
     ("loop" "${1:loop}($2) ->\n    receive\n	${3:_} ->\n	    $1($2)\n    end.\n$0" "loop(...) -> receive _ -> loop(...) end." nil nil nil nil nil nil)
     ("mod" "-module(${1:`(file-name-nondirectory\n              (file-name-sans-extension (or (buffer-file-name) (buffer-name))))`}).\n$0" "-module()." nil nil nil nil nil nil)
     ("rcv" "receive\n    $1 -> $0\nend" "receive ... -> ... end" nil nil nil nil nil nil)
     ("rcv" "receive\nafter\n    $1 -> $0\nend" "receive after ... -> ... end" nil nil nil nil nil nil)
     ("rec" "-record($1,{$2}).\n$0" "-record(...,{...})." nil nil nil nil nil nil)
     ("try" "try $1 of\n    $0\ncatch\nafter\nend" "try ... of ... catch after end" nil nil nil nil nil nil)
     ("undef" "-undef($1).\n$0" "-undef(...)." nil nil nil nil nil nil)))


;;; Do not edit! File generated at Tue May  6 17:54:00 2014
