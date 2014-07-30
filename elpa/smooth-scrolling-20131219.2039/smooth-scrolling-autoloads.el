;;; smooth-scrolling-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (smooth-scroll-up smooth-scroll-down smooth-scroll-lines-from-window-bottom
;;;;;;  smooth-scroll-lines-from-window-top smooth-scroll-strict-margins
;;;;;;  smooth-scroll-margin) "smooth-scrolling" "smooth-scrolling.el"
;;;;;;  (21172 32377 673079 674000))
;;; Generated autoloads from smooth-scrolling.el

(defvar smooth-scroll-margin 10 "\
Number of lines of visible margin at the top and bottom of a window.
If the point is within these margins, then scrolling will occur
smoothly for `previous-line' at the top of the window, and for
`next-line' at the bottom.

This is very similar in its goal to `scroll-margin'.  However, it
is implemented by activating `smooth-scroll-down' and
`smooth-scroll-up' advise via `defadvice' for `previous-line' and
`next-line' respectively.  As a result it avoids problems
afflicting `scroll-margin', such as a sudden jump and unexpected
highlighting of a region when the mouse is clicked in the margin.

Scrolling only occurs when the point is closer to the window
boundary it is heading for (top or bottom) than the middle of the
window.  This is to intelligently handle the case where the
margins cover the whole buffer (e.g. `smooth-scroll-margin' set
to 5 and `window-height' returning 10 or less).

See also `smooth-scroll-strict-margins'.")

(custom-autoload 'smooth-scroll-margin "smooth-scrolling" t)

(defvar smooth-scroll-strict-margins t "\
If true, the advice code supporting `smooth-scroll-margin'
will use `count-screen-lines' to determine the number of
*visible* lines between the point and the window top/bottom,
rather than `count-lines' which obtains the number of actual
newlines.  This is because there might be extra newlines hidden
by a mode such as folding-mode, outline-mode, org-mode etc., or
fewer due to very long lines being displayed wrapped when
`truncate-lines' is nil.

However, using `count-screen-lines' can supposedly cause
performance issues in buffers with extremely long lines.  Setting
`cache-long-line-scans' may be able to address this;
alternatively you can set this variable to nil so that the advice
code uses `count-lines', and put up with the fact that sometimes
the point will be allowed to stray into the margin.")

(custom-autoload 'smooth-scroll-strict-margins "smooth-scrolling" t)

(autoload 'smooth-scroll-lines-from-window-top "smooth-scrolling" "\
Work out, using the function indicated by
`smooth-scroll-strict-margins', what the current screen line is,
relative to the top of the window.  Counting starts with 1 referring
to the top line in the window.

\(fn)" t nil)

(autoload 'smooth-scroll-lines-from-window-bottom "smooth-scrolling" "\
Work out, using the function indicated by
`smooth-scroll-strict-margins', how many screen lines there are
between the point and the bottom of the window.  Counting starts
with 1 referring to the bottom line in the window.

\(fn)" t nil)

(autoload 'smooth-scroll-down "smooth-scrolling" "\
Scroll down smoothly if cursor is within `smooth-scroll-margin'
lines of the top of the window.

\(fn)" nil nil)

(autoload 'smooth-scroll-up "smooth-scrolling" "\
Scroll up smoothly if cursor is within `smooth-scroll-margin'
lines of the bottom of the window.

\(fn)" nil nil)

(defadvice previous-line (after smooth-scroll-down (&optional arg try-vscroll) activate) (smooth-scroll-down))

(defadvice next-line (after smooth-scroll-up (&optional arg try-vscroll) activate) (smooth-scroll-up))

(defadvice isearch-repeat (after isearch-smooth-scroll (direction) activate) (if (eq direction 'forward) (smooth-scroll-up) (smooth-scroll-down)))

(setq scroll-margin 0)

(setq redisplay-dont-pause t)

;;;***

;;;### (autoloads nil nil ("smooth-scrolling-pkg.el") (21172 32377
;;;;;;  728411 555000))

;;;***

(provide 'smooth-scrolling-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; smooth-scrolling-autoloads.el ends here
