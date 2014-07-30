;;; iy-go-to-char-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (iy-go-up-to-char-continue-backward iy-go-up-to-char-continue
;;;;;;  iy-go-to-char-continue-backward iy-go-to-char-continue iy-go-to-or-up-to-continue-backward
;;;;;;  iy-go-to-or-up-to-continue iy-go-up-to-char-backward iy-go-up-to-char
;;;;;;  iy-go-to-char-backward iy-go-to-char) "iy-go-to-char" "iy-go-to-char.el"
;;;;;;  (21102 38700 28619 763000))
;;; Generated autoloads from iy-go-to-char.el

(autoload 'iy-go-to-char "iy-go-to-char" "\
Move forward to N occurrences of CHAR.
\\<iy-go-to-char-keymap>

Typing key of CHAR will move to the next occurence of CHAR.

Typing `iy-go-to-char-key-forward' will move to the next
occurence of CHAR.

Typing `iy-go-to-char-key-backward', will move to the previous
occurence of CHAR.

Typing \\[iy-go-to-char-quit] will quit and return to the
original point.

Typing \\[iy-go-to-char-isearch] or
\\[iy-go-to-char-isearch-backward]] will start `isearch` using
CHAR.

Typing \\[iy-go-to-char-kill-region] or
\\[iy-go-to-char-kill-ring-save] will kill/copy between current
point and the start point.

Unless quit using \\[iy-go-to-char-quit] or the region is
activated before searching, the start point is set as mark.

\(fn N CHAR)" t nil)

(autoload 'iy-go-to-char-backward "iy-go-to-char" "\
Move backward to N occurence of CHAR.
\\<iy-go-to-char-keymap>

Typing key of CHAR will move to the previous occurence of CHAR.

Typing `iy-go-to-char-key-forward' moves to the next occurrence
of CHAR.

Typing `iy-go-to-char-key-backward', moves to the previous
occurrence of CHAR.

Typing \\[iy-go-to-char-quit] will quit and return to the
original point.

Typing \\[iy-go-to-char-isearch] or
\\[iy-go-to-char-isearch-backward]] will start `isearch` using
CHAR.

\(fn N CHAR)" t nil)

(autoload 'iy-go-up-to-char "iy-go-to-char" "\
Move forward to N occurrences of CHAR.
Like `iy-go-to-char' but jump up to the CHAR so it is
not included in the region between search start position and
current point.

\(fn N CHAR)" t nil)

(autoload 'iy-go-up-to-char-backward "iy-go-to-char" "\
Move backward to N occurrences of CHAR.
Like `iy-go-to-char-backward' but jump up to the CHAR so it is
not included in the region between search start position and
current point.

\(fn N CHAR)" t nil)

(autoload 'iy-go-to-or-up-to-continue "iy-go-to-char" "\
Continue last `iy-go-to-char' or `iy-go-to-char-backward' by N steps.
Set `STOP-POSITION' to overwrite the last used stop position strategy.

\(fn N &optional STOP-POSITION)" t nil)

(autoload 'iy-go-to-or-up-to-continue-backward "iy-go-to-char" "\
Continue last `iy-go-to-char' or `iy-go-to-char-backward' by N steps.
Set `STOP-POSITION' to overwrite the last used stop position strategy.

\(fn N &optional STOP-POSITION)" t nil)

(autoload 'iy-go-to-char-continue "iy-go-to-char" "\
Continue last `iy-go-to-char' or `iy-go-to-char-backward' by N steps.

\(fn N)" t nil)

(autoload 'iy-go-to-char-continue-backward "iy-go-to-char" "\
Continue last `iy-go-to-char' or `iy-go-to-char-backward' by N steps.

\(fn N)" t nil)

(autoload 'iy-go-up-to-char-continue "iy-go-to-char" "\
Continue last `iy-go-up-to-char' or `iy-go-up-to-char-backward' by N steps.

\(fn N)" t nil)

(autoload 'iy-go-up-to-char-continue-backward "iy-go-to-char" "\
Continue last `iy-go-up-to-char' or `iy-go-up-to-char-backward' by N steps.

\(fn N)" t nil)

;;;***

;;;### (autoloads nil nil ("iy-go-to-char-pkg.el") (21102 38700 53085
;;;;;;  44000))

;;;***

(provide 'iy-go-to-char-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; iy-go-to-char-autoloads.el ends here
