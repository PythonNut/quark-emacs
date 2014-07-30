;;; Compiled snippets and support files for `ruby-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'ruby-mode
  '(("all" "all? { |${e}| $0 }" "all? { |...| ... }" nil
      ("collections")
      nil nil nil nil)
     ("any" "any? { |${e}| $0 }" "any? { |...| ... }" nil
       ("collections")
       nil nil nil nil)
     ("classify" "classify { |${e}| $0 }" "classify { |...| ... }" nil
       ("collections")
       nil nil nil nil)
     ("collect" "collect { |${e}| $0 }" "collect { |...| ... }" nil
       ("collections")
       nil nil nil nil)
     ("deli" "delete_if { |${e} $0 }" "delete_if { |...| ... }" nil
       ("collections")
       nil nil nil nil)
     ("det" "detect { |${e}| $0 }" "detect { |...| ... }" nil
       ("collections")
       nil nil nil nil)
     ("ea" "each { |${e}| $0 }" "each { |...| ... }" nil
       ("collections")
       nil nil nil nil)
     ("eac" "each_cons(${1:2}) { |${group}| $0 }" "each_cons(...) { |...| ... }" nil
       ("collections")
       nil nil nil nil)
     ("eai" "each_index { |${i}| $0 }" "each_index { |i| ... }" nil
       ("collections")
       nil nil nil nil)
     ("eav" "each_value { |${val}| $0 }" "each_value { |val| ... }" nil
       ("collections")
       nil nil nil nil)
     ("eawi" "each_with_index { |${e}, ${i}| $0 }" "each_with_index { |e, i| ... }" nil
       ("collections")
       nil nil nil nil)
     ("inject" "inject(${1:0}) { |${2:injection}, ${3:element}| $0 }" "inject(...) { |...| ... }" nil
       ("collections")
       nil nil nil nil)
     ("reject" "reject { |${1:element}| $0 }" "reject { |...| ... }" nil
       ("collections")
       nil nil nil nil)
     ("select" "select { |${1:element}| $0 }" "select { |...| ... }" nil
       ("collections")
       nil nil nil nil)
     ("zip" "zip(${enums}) { |${row}| $0 }" "zip(...) { |...| ... }" nil
       ("collections")
       nil nil nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'ruby-mode
  '(("forin" "for ${1:element} in ${2:collection}\n  $0\nend" "for ... in ...; ... end" nil
      ("control structure")
      nil nil nil nil)
     ("if" "if ${1:condition}\n  $0\nend" "if ... end" nil
       ("control structure")
       nil nil nil nil)
     ("ife" "if ${1:condition}\n  $2\nelse\n  $3\nend" "if ... else ... end" nil
       ("control structure")
       nil nil nil nil)
     ("tim" "times { |${n}| $0 }" "times { |n| ... }" nil
       ("control structure")
       nil nil nil nil)
     ("until" "until ${condition}\n  $0\nend" "until ... end" nil
       ("control structure")
       nil nil nil nil)
     ("upt" "upto(${n}) { |${i}|\n  $0\n}" "upto(...) { |n| ... }" nil
       ("control structure")
       nil nil nil nil)
     ("when" "when ${condition}\n  $0\nend" "when ... end" nil
       ("control structure")
       nil nil nil nil)
     ("while" "while ${condition}\n  $0\nend" "while ... end" nil
       ("control structure")
       nil nil nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'ruby-mode
  '(("Comp" "include Comparable\n\ndef <=> other\n  $0\nend" "include Comparable; def <=> ... end" nil
      ("definitions")
      nil nil nil nil)
     ("am" "alias_method :${new_name}, :${old_name}" "alias_method new, old" nil
       ("definitions")
       nil nil nil nil)
     ("cla" "class << ${self}\n  $0\nend" "class << self ... end" nil
       ("definitions")
       nil nil nil nil)
     ("cls" "class ${1:`(let ((fn (capitalize (file-name-nondirectory\n                                 (file-name-sans-extension\n				 (or (buffer-file-name)\n				     (buffer-name (current-buffer))))))))\n           (cond\n             ((string-match \"_\" fn) (replace-match \"\" nil nil fn))\n              (t fn)))`}\n  $0\nend" "class ... end" nil
       ("definitions")
       nil nil nil nil)
     ("mm" "def method_missing(method, *args)\n  $0\nend" "def method_missing ... end" nil
       ("definitions")
       nil nil nil nil)
     ("mod" "module ${1:`(let ((fn (capitalize (file-name-nondirectory\n                                 (file-name-sans-extension\n         (or (buffer-file-name)\n             (buffer-name (current-buffer))))))))\n           (cond\n             ((string-match \"_\" fn) (replace-match \"\" nil nil fn))\n              (t fn)))`}\n  $0\nend" "module ... end" nil
       ("definitions")
       nil nil nil nil)
     ("r" "attr_reader :" "attr_reader ..." nil
       ("definitions")
       nil nil nil nil)
     ("rw" "attr_accessor :" "attr_accessor ..." nil
       ("definitions")
       nil nil nil nil)
     ("w" "attr_writer :" "attr_writer ..." nil
       ("definitions")
       nil nil nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'ruby-mode
  '(("#" "# =>" "# =>" nil
      ("general")
      nil nil nil nil)
     ("=b" "=begin rdoc\n  $0\n=end" "=begin rdoc ... =end" nil
       ("general")
       nil nil nil nil)
     ("app" "if __FILE__ == $PROGRAM_NAME\n  $0\nend" "if __FILE__ == $PROGRAM_NAME ... end" nil
       ("general")
       nil nil nil nil)
     ("bm" "Benchmark.bmbm(${1:10}) do |x|\n  $0\nend" "Benchmark.bmbm(...) do ... end" nil
       ("general")
       nil nil nil nil)
     ("case" "case ${1:object}\nwhen ${2:condition}\n  $0\nend" "case ... end" nil
       ("general")
       nil nil nil nil)
     ("dee" "Marshal.load(Marshal.dump($0))" "deep_copy(...)" nil
       ("general")
       nil nil nil nil)
     ("rb" "#!/usr/bin/ruby -wKU" "/usr/bin/ruby -wKU" nil
       ("general")
       nil nil nil nil)
     ("req" "require \"$0\"" "require \"...\"" nil
       ("general")
       nil nil nil nil)
     ("rreq" "require File.join(File.dirname(__FILE__), $0)" "require File.join(File.dirname(__FILE__), ...)" nil
       ("general")
       nil nil nil nil)
     ("y" ":yields: $0" ":yields: arguments (rdoc)" nil
       ("general")
       nil nil nil nil)))


;;; Do not edit! File generated at Tue May  6 17:54:00 2014
