;;; Compiled snippets and support files for `nxml-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'nxml-mode
  '(("body" "<body$1>\n  $0\n</body>" "<body>...</body>" nil nil nil nil nil nil)
     ("br" "<br />" "<br />" nil nil nil nil nil nil)
     ("code" "<code>\n  $0\n</code>" "<code>...</code>" nil nil nil nil nil nil)
     ("div" "<div$1>$0</div>" "<div...>...</div>" nil nil nil nil nil nil)
     ("form" "<form method=\"$1\" action=\"$2\">\n  $0\n</form>" "<form method=\"...\" action=\"...\"></form>" nil nil nil nil nil nil)
     ("head" "<head>\n  $0\n</head>" "<head>...</head>" nil nil nil nil nil nil)
     ("hr" "<hr />" "<hr />" nil nil nil nil nil nil)
     ("href" "<a href=\"$1\">$2</a>" "<a href=\"...\">...</a>" nil nil nil nil nil nil)
     ("html" "<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"${1:en}\" lang=\"${2:en}\">\n  $0\n</html>" "<html xmlns=\"...\">...</html>" nil nil nil nil nil nil)
     ("img" "<img src=\"$1\" alt=\"$2\" />" "<img src=\"...\" alt=\"...\" />" nil nil nil nil nil nil)
     ("input" "<input type=\"$1\" name=\"$2\" value=\"$3\" />" "<input ... />" nil nil nil nil nil nil)
     ("li" "<li>$1</li>" "<li>...</li>" nil nil nil nil nil nil)
     ("link" "<link rel=\"${1:stylesheet}\" href=\"${2:url}\" type=\"${3:text/css}\" media=\"${4:screen}\" />" "<link stylesheet=\"...\" />" nil nil nil nil nil nil)
     ("name" "<a name=\"$1\"></a>" "<a name=\"...\"></a>" nil nil nil nil nil nil)
     ("ol" "<ol>\n  $0\n</ol>" "<ol>...</ol>" nil nil nil nil nil nil)
     ("p" "<p>$1</p>" "<p>...</p>" nil nil nil nil nil nil)
     ("pre" "<pre>\n  $0\n</pre>" "<pre>...</pre>" nil nil nil nil nil nil)
     ("quote" "<blockquote>\n  $1\n</blockquote>" "<blockquote>...</blockquote>" nil nil nil nil nil nil)
     ("span" "<span>$1</span>" "<span>...</span>" nil nil nil nil nil nil)
     ("style" "<style type=\"text/css\" media=\"${1:screen}\">\n  $0\n</style>" "<style type=\"text/css\" media=\"...\">...</style>" nil nil nil nil nil nil)
     ("table" "<table>\n  $0\n</table>" "<table>...</table>" nil nil nil nil nil nil)
     ("tag" "<${1:tag}>$2</$1>$0" "<tag>...</tag>" nil nil nil nil nil nil)
     ("tag" "<${1:tag}>\n  $2\n</$1>$0" "<tag> \\n...\\n</tag>" nil nil nil nil nil nil)
     ("td" "<td$1>$2</td>" "<td>...</td>" nil nil nil nil nil nil)
     ("th" "<th$1>$2</th>" "<th>...</th>" nil nil nil nil nil nil)
     ("title" "<title>$1</title>" "<title>...</title>" nil nil nil nil nil nil)
     ("tr" "<tr>\n  $0\n</tr>" "<tr>...</tr>" nil nil nil nil nil nil)
     ("ul" "<ul>\n  $0\n</ul>" "<ul>...</ul>" nil nil nil nil nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'nxml-mode
  '(("h1" "<h1>$1</h1>" "<h1>...</h1>" nil
      ("header")
      nil nil nil nil)
     ("h2" "<h2>$1</h2>" "<h2>...</h2>" nil
       ("header")
       nil nil nil nil)
     ("h3" "<h3>$1</h3>" "<h3>...</h3>" nil
       ("header")
       nil nil nil nil)
     ("h4" "<h4>$1</h4>" "<h4>...</h4>" nil
       ("header")
       nil nil nil nil)
     ("h5" "<h5>$1</h5>" "<h5>...</h5>" nil
       ("header")
       nil nil nil nil)
     ("h6" "<h6>$1</h6>" "<h6>...</h6>" nil
       ("header")
       nil nil nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'nxml-mode
  '(("doctype" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">" "DocType XHTML 1.1" nil
      ("meta")
      nil nil nil nil)
     ("doctype" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">" "DocType XHTML 1.0 Strict" nil
       ("meta")
       nil nil nil nil)
     ("doctype" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">" "DocType XHTML 1.0 Transitional" nil
       ("meta")
       nil nil nil nil)
     ("meta" "<meta name=\"${1:generator}\" content=\"${2:content}\" />" "<meta name=\"...\" content=\"...\" />" nil
       ("meta")
       nil nil nil nil)))


;;; Do not edit! File generated at Tue May  6 17:54:00 2014
