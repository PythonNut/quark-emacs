;;; flex-isearch-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (flex-isearch-backward flex-isearch-forward global-flex-isearch-mode
;;;;;;  turn-off-flex-isearch turn-on-flex-isearch flex-isearch-mode
;;;;;;  flex-isearch-message-prefix flex-isearch-auto) "flex-isearch"
;;;;;;  "flex-isearch.el" (21255 35614 230919 619000))
;;; Generated autoloads from flex-isearch.el

(defvar flex-isearch-auto nil "\
Determines when flex searching is automatically activated.
If it is t, then flex matching is used for all isearches.  If it
is 'on-failed, flex matching will only be used after a standard
isearch failed.  If it is nil, flex searching will not be enabled
automatically.")

(custom-autoload 'flex-isearch-auto "flex-isearch" t)

(defvar flex-isearch-message-prefix "[FLEX] " "\
Prepended to the isearch prompt when flex searching is activated.")

(custom-autoload 'flex-isearch-message-prefix "flex-isearch" t)

(autoload 'flex-isearch-mode "flex-isearch" "\
Flex matching (similar to ido's flex matching) in incremental searches.

When activated, it transforms a regular isearch into a much looser
regexp search that will match the original string, but also
strings that simply contain the characters of the search string
in order.  For example, a search string of \"thlongstr\" matches
\"the=long_string\".  See `flex-isearch-regexp-compile' for the actual
regexp that the search string is transformed to.

When this minor mode is enabled, it puts advice on
`isearch-forward' and `isearch-backward', making them use the
flex mode when given a double prefix argument (e.g., C-u C-u
C-s).  It also uses `flex-isearch-auto' to possibly enable flex
searching during a normal isearch.

\(fn &optional ARG)" t nil)

(autoload 'turn-on-flex-isearch "flex-isearch" "\


\(fn)" t nil)

(autoload 'turn-off-flex-isearch "flex-isearch" "\


\(fn)" t nil)

(defvar global-flex-isearch-mode nil "\
Non-nil if Global-Flex-Isearch mode is enabled.
See the command `global-flex-isearch-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-flex-isearch-mode'.")

(custom-autoload 'global-flex-isearch-mode "flex-isearch" nil)

(autoload 'global-flex-isearch-mode "flex-isearch" "\
Toggle Flex-Isearch mode in all buffers.
With prefix ARG, enable Global-Flex-Isearch mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Flex-Isearch mode is enabled in all buffers where
`turn-on-flex-isearch' would do it.
See `flex-isearch-mode' for more information on Flex-Isearch mode.

\(fn &optional ARG)" t nil)

(autoload 'flex-isearch-forward "flex-isearch" "\
Like `isearch-forward', but with flex searching.

\(fn &optional REGEXP-P NO-RECURSIVE-EDIT)" t nil)

(autoload 'flex-isearch-backward "flex-isearch" "\
Like `isearch-backward', but with flex searching.

\(fn &optional REGEXP-P NO-RECURSIVE-EDIT)" t nil)

;;;***

;;;### (autoloads nil nil ("flex-isearch-pkg.el") (21255 35614 252202
;;;;;;  537000))

;;;***

(provide 'flex-isearch-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; flex-isearch-autoloads.el ends here
