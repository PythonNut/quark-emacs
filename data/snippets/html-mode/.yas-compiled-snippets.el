;;; Compiled snippets and support files for `html-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'html-mode
  '(("b" "<b>$0</b>" "<b>...</b>" nil nil nil nil nil nil)
     ("body" "<body$1>\n  $0\n</body>" "<body>...</body>" nil nil nil nil nil nil)
     ("br" "<br />" "<br />" nil nil nil nil nil nil)
     ("code" "<code>\n  $0\n</code>" "<code>...</code>" nil nil nil nil nil nil)
     ("code" "<code class=\"$1\">\n  $0\n</code>" "<code class=\"...\">...</code>" nil nil nil nil nil nil)
     ("div" "<div${1: id=\"${2:some_id}\"}${3: class=\"${4:some_class}\"}>$0</div>" "<div...>...</div>" nil nil nil nil nil nil)
     ("div" "<div class=\"$1\">\n  $0\n</div>" "<div class=\"...\">...</div>" nil nil nil nil nil nil)
     ("div" "<div id=\"$1\">\n  $0\n</div>" "<div id=\"...\">...</div>" nil nil nil nil nil nil)
     ("div" "<div id=\"$1\" class=\"$2\">\n  $0\n</div>" "<div id=\"...\" class=\"...\">...</div>" nil nil nil nil nil nil)
     ("dov" "a mirror up here $3\n\n\n<dov ${1:id=\"${2:some_id and here comes another nested field: ${3:nested_shit}}\"}>\n    $0\n</dov>\n<dov $1>\n    actually some other shit and $3\n</dov>" "<dov...>...</dov>" nil nil nil nil nil nil)
     ("form" "<form method=\"$1\" id=\"$2\" action=\"$3\">\n  $0\n</form>" "<form method=\"...\" id=\"...\" action=\"...\"></form>" nil nil nil nil nil nil)
     ("head" "<head>\n  $0\n</head>" "<head>...</head>" nil nil nil nil nil nil)
     ("hr" "<hr />" "<hr />" nil nil nil nil nil nil)
     ("href" "<a href=\"$1\">$2</a>" "<a href=\"...\">...</a>" nil nil nil nil nil nil)
     ("html" "<html>\n  $0\n</html>" "<html>...</html>" nil nil nil nil nil nil)
     ("html" "<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"${1:en}\" lang=\"${2:en}\">\n  $0\n</html>" "<html xmlns=\"...\">...</html>" nil nil nil nil nil nil)
     ("i" "<i>$0</i>" "<i>...</i>" nil nil nil nil nil nil)
     ("img" "<img src=\"$1\" class=\"$2\" alt=\"$3\" />" "<img src=\"...\" class=\"...\" alt=\"...\" />" nil nil nil nil nil nil)
     ("input" "<input type=\"$1\" name=\"$2\" value=\"$3\" />" "<input ... />" nil nil nil nil nil nil)
     ("link" "<link rel=\"${1:stylesheet}\" href=\"${2:url}\" type=\"${3:text/css}\" media=\"${4:screen}\" />" "<link stylesheet=\"...\" />" nil nil nil nil nil nil)
     ("link" "<!--[if IE]>\n<link rel=\"${1:stylesheet}\" href=\"${2:url}\" type=\"${3:text/css}\" media=\"${4:screen}\" />\n<![endif]-->" "<!--[if IE]><link stylesheet=\"...\" /><![endif]-->" nil nil nil nil nil nil)
     ("mailto" "<a href=\"mailto:$1@$2\">$0</a>" "<a href=\"mailto:...@...\">...</a>" nil nil nil nil nil nil)
     ("p" "<p>$1</p>" "<p>...</p>" nil nil nil nil nil nil)
     ("pre" "<pre>\n  $0\n</pre>" "<pre>...</pre>" nil nil nil nil nil nil)
     ("q" "<blockquote>\n$0\n</blockquote>" "<blockquote>...</blockquote>" nil nil nil nil nil nil)
     ("quote" "<blockquote>\n  $1\n</blockquote>" "<blockquote>...</blockquote>" nil nil nil nil nil nil)
     ("script" "<script type=\"text/javascript\">\n  $0\n</script>" "<script type=\"text/javascript\">...</script>" nil nil nil nil nil nil)
     ("script" "<script type=\"text/javascript\" src=\"$1\"></script>" "<script type=\"text/javascript\" src=\"...\"></script>" nil nil nil nil nil nil)
     ("span" "<span>$1</span>" "<span>...</span>" nil nil nil nil nil nil)
     ("span" "<span class=\"$1\">$2</span>" "<span class=\"...\">...</span>" nil nil nil nil nil nil)
     ("span" "<span id=\"$1\">$2</span>" "<span id=\"...\">...</span>" nil nil nil nil nil nil)
     ("style" "<style type=\"text/css\" media=\"${1:screen}\">\n  $0\n</style>" "<style type=\"text/css\" media=\"...\">...</style>" nil nil nil nil nil nil)
     ("textarea" "<textarea name=\"$1\" id=\"$2\" rows=\"$3\" cols=\"$4\" tabindex=\"$5\"></textarea>" "<textarea ...></textarea>" nil nil nil nil nil nil)
     ("title" "<title>$1</title>" "<title>...</title>" nil nil nil nil nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'html-mode
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
(yas-define-snippets 'html-mode
  '(("dd" "<dd>$1</dd>" "<dd> ... </dd>" nil
      ("list")
      nil nil nil nil)
     ("dl" "<dl>\n    $0\n</dl>" "<dl> ... </dl>" nil
       ("list")
       nil nil nil nil)
     ("dl" "<dl id=\"$1\">\n    $0\n</dl>" "<dl> ... </dl>" nil
       ("list")
       nil nil nil nil)
     ("dt" "<dt>$1</dt>" "<dt> ... </dt>" nil
       ("list")
       nil nil nil nil)
     ("li" "<li>$1</li>" "<li>...</li>" nil
       ("list")
       nil nil nil nil)
     ("li" "<li class=\"$1\">$2</li>" "<li class=\"...\">...</li>" nil
       ("list")
       nil nil nil nil)
     ("ol" "<ol>\n  $0\n</ol>" "<ol>...</ol>" nil
       ("list")
       nil nil nil nil)
     ("ol" "<ol class=\"$1\">\n  $0\n</ol>" "<ol class=\"...\">...</ol>" nil
       ("list")
       nil nil nil nil)
     ("ol" "<ol id=\"$1\">\n  $0\n</ol>" "<ol id=\"...\">...</ol>" nil
       ("list")
       nil nil nil nil)
     ("ul" "<ul>\n  $0\n</ul>" "<ul>...</ul>" nil
       ("list")
       nil nil nil nil)
     ("ul" "<ul class=\"$1\">\n  $0\n</ul>" "<ul class=\"...\">...</ul>" nil
       ("list")
       nil nil nil nil)
     ("ul" "<ul id=\"$1\">\n  $0\n</ul>" "<ul id=\"...\">...</ul>" nil
       ("list")
       nil nil nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'html-mode
  '(("doctype" "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">" "Doctype HTML 4.01 Strict" nil
      ("meta")
      nil nil nil nil)
     ("doctype" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Frameset//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd\">" "DocType XHTML 1.0 frameset" nil
       ("meta")
       nil nil nil nil)
     ("doctype" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">" "DocType XHTML 1.1" nil
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
       nil nil nil nil)
     ("meta" "<meta http-equiv=\"${1:Content-Type}\" content=\"${2:text/html; charset=UTF-8}\" />" "<meta http-equiv=\"...\" content=\"...\" />" nil
       ("meta")
       nil nil nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'html-mode
  '(("table" "<table width=\"$1\" cellspacing=\"$2\" cellpadding=\"$3\" border=\"$4\">\n  $0\n</table>" "<table ...>...</table>" nil
      ("table")
      nil nil nil nil)
     ("td" "<td$1>$2</td>" "<td>...</td>" nil
       ("table")
       nil nil nil nil)
     ("th" "<th$1>$2</th>" "<th>...</th>" nil
       ("table")
       nil nil nil nil)
     ("tr" "<tr>\n  $0\n</tr>" "<tr>...</tr>" nil
       ("table")
       nil nil nil nil)))


;;; Do not edit! File generated at Tue May  6 17:54:00 2014
