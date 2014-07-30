;;; Compiled snippets and support files for `markdown-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'markdown-mode
  '(("+" "+ ${1:Text}\n+$0" "Unordered List" nil nil nil nil nil nil)
     ("-" "- ${1:Text}\n-$0" "Unordered List" nil nil nil nil nil nil)
     ("_" "_${1:Text}_ $0" "Emphasis" nil nil nil nil nil nil)
     ("__" "**${1:Text}** $0" "Strong" nil nil nil nil nil nil)
     ("`" "\\`${1:Code}\\` $0" "Inline Code" nil nil nil nil nil nil)
     ("h1" "# ${1:Header 1} #\n\n$0" "Header 1 (#)" nil nil nil nil nil nil)
     ("h1" "${1:Header 1}\n${1:$(make-string (string-width text) ?\\=)}\n\n$0" "Header 1 (=)" nil nil nil nil nil nil)
     ("h2" "## ${1:Header 1} ##\n\n$0" "Header 2 (##)" nil nil nil nil nil nil)
     ("h2" "${1:Header 2}\n${1:$(make-string (string-width text) ?\\-)}\n\n$0" "Header 2 (-)" nil nil nil nil nil nil)
     ("h3" "### ${1:Header 3} ###\n\n$0" "Header 3" nil nil nil nil nil nil)
     ("h4" "#### ${1:Header 4} ####\n\n$0" "Header 4" nil nil nil nil nil nil)
     ("h5" "##### ${1:Header 5} #####\n\n$0" "Header 5" nil nil nil nil nil nil)
     ("h6" "###### ${1:Header 6} ######\n\n$0" "Header 6" nil nil nil nil nil nil)
     ("hr" "\n----------\n\n$0" "Horizontal Rule (-)" nil nil nil nil nil nil)
     ("hr" "\n*******\n\n$0" "Horizontal Rule (*)" nil nil nil nil nil nil)
     ("img" "![${1:Alt Text}](${2:URL} $3) $0" "Image" nil nil nil nil nil nil)
     ("link" "[${1:Link Text}](${2:URL} $3) $0" "Link" nil nil nil nil nil nil)
     ("ol" "${1:1}. ${2:Text}\n${1:$(number-to-string (1+ (string-to-number text)))}. $0" "Ordered List" nil nil nil nil nil nil)
     ("rimg" "![${1:Alt Text}][$2] $0" "Referenced Image" nil nil nil nil nil nil)
     ("rlb" "[${1:Reference}]: ${2:URL} $3\n$0" "Reference Label" nil nil nil nil nil nil)
     ("rlink" "[${1:Link Text}][$2] $0" "Reference Link" nil nil nil nil nil nil)))


;;; Do not edit! File generated at Tue May  6 17:54:00 2014
