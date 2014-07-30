;;; nav-flash-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (nav-flash-show nav-flash) "nav-flash" "nav-flash.el"
;;;;;;  (21114 40774 922657 907000))
;;; Generated autoloads from nav-flash.el

(let ((loads (get 'nav-flash 'custom-loads))) (if (member '"nav-flash" loads) nil (put 'nav-flash 'custom-loads (cons '"nav-flash" loads))))

(autoload 'nav-flash-show "nav-flash" "\
Flash a temporary highlight to help the user find something.

POS is optional, and defaults to the current point.

If optional END-POS is set, flash the characters between the two
points, otherwise flash the entire line in which POS is found.

The flash is normally not inclusive of END-POS.  However, when
POS is equal to END-POS, the single character at POS will flash.

Optional FACE defaults to `nav-flash-face'.  Optional DELAY
defaults to `nav-flash-delay' seconds.  Setting DELAY to 0 makes
this function a no-op.

\(fn &optional POS END-POS FACE DELAY)" nil nil)

;;;***

;;;### (autoloads nil nil ("nav-flash-pkg.el") (21114 40774 937372
;;;;;;  286000))

;;;***

(provide 'nav-flash-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; nav-flash-autoloads.el ends here
