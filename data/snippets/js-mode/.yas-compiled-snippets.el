;;; Compiled snippets and support files for `js-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'js-mode
  '(("assert" "assert.${1:equal}($0);\n" "assert" nil nil nil nil nil nil)
     ("cb" "function(error, $1){\n$0\n}\n" "cb" nil nil nil nil nil nil)
     ("com" "\n/**\n * $0\n * @param {${1:String}} $2\n * @return {${3:String}}\n */\n" "com" nil nil nil nil nil nil)
     ("desc" "describe('$1', function(){\n$2\n});\n" "desc" nil nil nil nil nil nil)
     ("err" "if(${1:error}){\n  callback($1);\n  return;\n}\n$2\n" "err" nil nil nil nil nil nil)
     ("error" "if(${1:error}){\n  callback($1);\n  return;\n}\n$2\n" "error" nil nil nil nil nil nil)
     ("expect" "expect($1).to.${2:equal}($3);\n$0" "expect" nil nil nil nil nil nil)
     ("exports" "module.exports = {\n$0\n};\n" "exports" nil nil nil nil nil nil)
     ("fn" "function$1($2){\n$0\n}\n" "fn" nil nil nil nil nil nil)
     ("for" "var ${1:i} = ${2:-1}, len = $3.length;\n\nfor(; ++$1 < len;){\n  $4\n}\n" "for" nil nil nil nil nil nil)
     ("id" "document.getElementById('$0');\n" "id" nil nil nil nil nil nil)
     ("if" "if( ${1} ){\n$0\n}\n" "if" nil nil nil nil nil nil)
     ("invoc" "(function($1){\n$0\n})($2);\n" "invoc" nil nil nil nil nil nil)
     ("it" "it('$1', function(done){\n$2\n});\n" "it" nil nil nil nil nil nil)
     ("log" "console.${1:log}( $0 );\n" "log" nil nil nil nil nil nil)
     ("method" "$1.prototype.$2 = function($3){\n${0}\n};\n" "method" nil nil nil nil nil nil)
     ("mod" "var $1 = require(\"${2:$1}\")$3\n\nmodule.exports = {\n  $4: $4$5\n};\n\n$0\n" "mod" nil nil nil nil nil nil)
     ("prop" "\nvar $1 = (function(){\n\n  var value = undefined;\n\n  return function $1(newValue){\n\n    if( $1.arguments.length > 0 ){\n      value = newValue;\n    }\n\n    return value;\n  };\n\n})();\n" "prop" nil nil nil nil nil nil)
     ("proto" "$1.prototype.$2 = $0\n" "proto" nil nil nil nil nil nil)
     ("rq" "$1 = require(\"${2:$1}\")$0\n" "rq" nil nil nil nil nil nil)
     ("rt" "return$0;\n" "rt" nil nil nil nil nil nil)
     ("slice" "Array.prototype.slice.${1:call}($0);\n" "slice" nil nil nil nil nil nil)
     ("super" "$1.prototype.${2:constructor}.${3:call}($0);\n" "super" nil nil nil nil nil nil)
     ("switch" "switch($1){\n$0\n};\n" "switch" nil nil nil nil nil nil)
     ("throw" "throw new Error(\"$1\");\n" "throw" nil nil nil nil nil nil)
     ("try" "try {\n  $1\n} catch(error) {\n  $0\n}\n" "try" nil nil nil nil nil nil)
     ("var" "var $1 = $0\n" "var" nil nil nil nil nil nil)
     ("wh" "while($1){\n$0\n}\n" "wh" nil nil nil nil nil nil)
     ("while" "\nvar i = $1.length;\n\nwhile( i -- ){\n  $0\n}" "while" nil nil nil nil nil nil)))


;;; Do not edit! File generated at Tue May  6 17:54:00 2014
