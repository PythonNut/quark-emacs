;;; doremi-cmd-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (doremi-window-width+ doremi-window-height+ doremi-global-marks+
;;;;;;  doremi-marks+ doremi-buffers+ doremi-bookmarks+ doremi-color-themes+
;;;;;;  doremi-color-themes doremi-themes-update-flag doremi-misc-commands)
;;;;;;  "doremi-cmd" "doremi-cmd.el" (21185 65145 747937 975000))
;;; Generated autoloads from doremi-cmd.el

(let ((loads (get 'doremi-misc-commands 'custom-loads))) (if (member '"doremi-cmd" loads) nil (put 'doremi-misc-commands 'custom-loads (cons '"doremi-cmd" loads))))

(defvar doremi-themes-update-flag nil "\
*Non-nil means choosing a theme saves the updated list of themes.
This applies to commands `doremi-custom-themes+' and
`doremi-color-themes+' and their respective options,
`doremi-custom-themes' and `doremi-color-themes'.

A prefix arg to the command flips the option value for the duration of
the command.")

(custom-autoload 'doremi-themes-update-flag "doremi-cmd" t)

(defvar doremi-color-themes nil "\
*List of color themes to cycle through using `doremi-color-themes+'.")

(custom-autoload 'doremi-color-themes "doremi-cmd" t)

(autoload 'doremi-color-themes+ "doremi-cmd" "\
Successively cycle among color themes.
The themes used for cycling are those in option `doremi-color-themes'.

You can use `C-g' to quit and cancel changes made so far.
Alternatively, after using `doremi-color-themes+' you can use
`color-theme-select' and choose pseudo-theme `[Reset]' - that does the
same thing.  Note that in either case, some things might not be
restored.

Option `doremi-themes-update-flag' determines whether the updated
value of `doremi-color-themes' is saved.  A prefix arg to this command
flips the option value for the current invocation of the command.

To use this command, you must have loaded library `color-theme.el',
available from http://www.nongnu.org/color-theme.  See also:
http://www.emacswiki.org/ColorTheme.

\(fn &optional FLIP)" t nil)

(autoload 'doremi-bookmarks+ "doremi-cmd" "\
Successively cycle among all bookmarks.

\(fn)" t nil)

(autoload 'doremi-buffers+ "doremi-cmd" "\
Successively cycle among all existing buffers.
You can use `C-g' to quit and return to the original buffer.

\(fn)" t nil)

(autoload 'doremi-marks+ "doremi-cmd" "\
Successively cycle among all marks in the `mark-ring'.
You can use `C-g' to quit and return to the original position.
If library `crosshairs.el' is used, highlight each visited mark
position temporarily.

\(fn)" t nil)

(autoload 'doremi-global-marks+ "doremi-cmd" "\
Successively cycle among all marks in the `global-mark-ring'.
You can use `C-g' to quit and return to the original position.
If library `crosshairs.el' is used, highlight each visited mark
position temporarily.

\(fn)" t nil)

(autoload 'doremi-window-height+ "doremi-cmd" "\
Change height of WINDOW incrementally.
INCREMENT is the size increment.
WINDOW is selected.  WINDOW defaults to the selected window.

\(fn &optional INCREMENT WINDOW)" t nil)

(autoload 'doremi-window-width+ "doremi-cmd" "\
Change width of WINDOW incrementally.
INCREMENT is the size increment.
WINDOW is selected.  WINDOW defaults to the selected window.

\(fn &optional INCREMENT WINDOW)" t nil)

;;;***

;;;### (autoloads nil nil ("doremi-cmd-pkg.el") (21185 65145 758395
;;;;;;  634000))

;;;***

(provide 'doremi-cmd-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; doremi-cmd-autoloads.el ends here
