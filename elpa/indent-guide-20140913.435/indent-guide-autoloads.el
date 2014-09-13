;;; indent-guide-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (indent-guide-global-mode indent-guide-mode) "indent-guide"
;;;;;;  "indent-guide.el" (21524 47827 291470 393000))
;;; Generated autoloads from indent-guide.el

(autoload 'indent-guide-mode "indent-guide" "\
show vertical lines to guide indentation

\(fn &optional ARG)" t nil)

(defvar indent-guide-global-mode nil "\
Non-nil if Indent-Guide-Global mode is enabled.
See the command `indent-guide-global-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `indent-guide-global-mode'.")

(custom-autoload 'indent-guide-global-mode "indent-guide" nil)

(autoload 'indent-guide-global-mode "indent-guide" "\
Toggle Indent-Guide mode in all buffers.
With prefix ARG, enable Indent-Guide-Global mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Indent-Guide mode is enabled in all buffers where
`(lambda nil (unless (memq major-mode indent-guide-inhibit-modes) (indent-guide-mode 1)))' would do it.
See `indent-guide-mode' for more information on Indent-Guide mode.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("indent-guide-pkg.el") (21524 47827 304975
;;;;;;  36000))

;;;***

(provide 'indent-guide-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; indent-guide-autoloads.el ends here
