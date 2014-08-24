;;; thingatpt+-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (find-fn-or-var-nearest-point tap-redefine-std-fns
;;;;;;  tap-put-thing-at-point-props tap-near-point-y-distance tap-near-point-x-distance
;;;;;;  thing-at-point-plus) "thingatpt+" "thingatpt+.el" (21497
;;;;;;  60311 970012 448000))
;;; Generated autoloads from thingatpt+.el

(let ((loads (get 'thing-at-point-plus 'custom-loads))) (if (member '"thingatpt+" loads) nil (put 'thing-at-point-plus 'custom-loads (cons '"thingatpt+" loads))))

(defvar tap-near-point-x-distance 50 "\
*Maximum number of characters from point to search, left and right.
Used typically by functions that invoke
`tap-thing/form-nearest-point-with-bounds', and which provide default
text for minibuffer input.  Such functions can also ignore or override
this setting temporarily.")

(custom-autoload 'tap-near-point-x-distance "thingatpt+" t)

(defvar tap-near-point-y-distance 5 "\
*Maximum number of lines from point to search, up and down.
To constrain search to the same line as point, set this to zero.
Used typically by functions that invoke
`tap-thing/form-nearest-point-with-bounds', and which provide default
text for minibuffer input.  Such functions can also ignore or override
this setting temporarily.")

(custom-autoload 'tap-near-point-y-distance "thingatpt+" t)

(autoload 'tap-put-thing-at-point-props "thingatpt+" "\
Change `(bounds-of-)thing-at-point' properties for standard things.
This makes some things normally handled by `thingatpt.el' be handled
instead by functions defined in `thingatpt+.el'.

This also affects some things that are handled by `thingatpt.el' in
another way, not by setting these properties.

\(fn)" t nil)

(autoload 'tap-redefine-std-fns "thingatpt+" "\
Redefine some standard `thingatpt.el' functions, to fix them.
The standard functions replaced are these:
 `bounds-of-thing-at-point' - Better behavior.
                              Accept optional arg SYNTAX-TABLE.
 `form-at-point'            - Accept optional arg SYNTAX-TABLE.
 `list-at-point'            - Better behavior.
                              Accept optional arg SYNTAX-TABLE.
 `symbol-at-point'          - Use `emacs-lisp-mode-syntax-table'.
 `thing-at-point'           - Ensure it returns a string or nil.
                              Accept optional arg SYNTAX-TABLE.
 `thing-at-point-bounds-of-list-at-point'
                            - Better behavior.  Accept optional
                              args UP and UNQUOTEDP.

\(fn)" t nil)

(intern "whitespace-&-newlines")

(autoload 'find-fn-or-var-nearest-point "thingatpt+" "\
Go to the definition of the function or variable nearest the cursor.
With a prefix arg, or if no function or variable is near the cursor,
prompt for the function or variable to find, instead.

\(fn &optional CONFIRMP)" t nil)

;;;***

;;;### (autoloads nil nil ("thingatpt+-pkg.el") (21497 60311 989781
;;;;;;  375000))

;;;***

(provide 'thingatpt+-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; thingatpt+-autoloads.el ends here
