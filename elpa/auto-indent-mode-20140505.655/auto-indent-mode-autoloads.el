;;; auto-indent-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (auto-indent-global-mode auto-indent-mode-on auto-indent-mode
;;;;;;  auto-indent-eol-char-newline auto-indent-eol-newline) "auto-indent-mode"
;;;;;;  "auto-indent-mode.el" (21351 47522 942317 104000))
;;; Generated autoloads from auto-indent-mode.el

(autoload 'auto-indent-eol-newline "auto-indent-mode" "\
Auto-indent function for `end-of-line' and then newline.

\(fn)" t nil)

(autoload 'auto-indent-eol-char-newline "auto-indent-mode" "\
Auto-indent function for `end-of-line', insert `auto-indent-eol-char', and then newline.

\(fn)" t nil)

(autoload 'auto-indent-mode "auto-indent-mode" "\
Auto Indent minor mode.

With no argument, this command toggles the mode.
positive prefix argument turns on the mode.
Negative prefix argument turns off the mode.

When auto-indent-mode minor mode is enabled, yanking or pasting automatically indents

Fall back to default, non-indented yanking by preceding the yanking commands with C-u.

Based on auto-indentation posts, slightly redefined to allow it to be a minor mode

http://www.emacswiki.org/emacs/AutoIndentation

\(fn &optional ARG)" t nil)

(autoload 'auto-indent-mode-on "auto-indent-mode" "\
Turn on auto-indent minor mode.

\(fn)" t nil)

(defvar auto-indent-global-mode nil "\
Non-nil if Auto-Indent-Global mode is enabled.
See the command `auto-indent-global-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `auto-indent-global-mode'.")

(custom-autoload 'auto-indent-global-mode "auto-indent-mode" nil)

(autoload 'auto-indent-global-mode "auto-indent-mode" "\
Toggle Auto-Indent mode in all buffers.
With prefix ARG, enable Auto-Indent-Global mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Auto-Indent mode is enabled in all buffers where
`auto-indent-mode-on' would do it.
See `auto-indent-mode' for more information on Auto-Indent mode.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("auto-indent-mode-pkg.el") (21351 47522
;;;;;;  970143 756000))

;;;***

(provide 'auto-indent-mode-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; auto-indent-mode-autoloads.el ends here
