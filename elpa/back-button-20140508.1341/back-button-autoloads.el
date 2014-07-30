;;; back-button-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (back-button-global-forward back-button-global-backward
;;;;;;  back-button-local-forward back-button-local-backward back-button-global
;;;;;;  back-button-local back-button-mode back-button-push-mark-local-and-global
;;;;;;  back-button-keys back-button-index back-button) "back-button"
;;;;;;  "back-button.el" (21357 3201 694685 981000))
;;; Generated autoloads from back-button.el

(let ((loads (get 'back-button 'custom-loads))) (if (member '"back-button" loads) nil (put 'back-button 'custom-loads (cons '"back-button" loads))))

(let ((loads (get 'back-button-index 'custom-loads))) (if (member '"back-button" loads) nil (put 'back-button-index 'custom-loads (cons '"back-button" loads))))

(let ((loads (get 'back-button-keys 'custom-loads))) (if (member '"back-button" loads) nil (put 'back-button-keys 'custom-loads (cons '"back-button" loads))))

(autoload 'back-button-push-mark-local-and-global "back-button" "\
Push mark at LOCATION, and unconditionally add to `global-mark-ring'.

This function differs from `push-mark' in that `global-mark-ring'
is always updated.

LOCATION is optional, and defaults to the current point.

NOMSG and ACTIVATE are as documented at `push-mark'.

When CONSECUTIVES is set to 'limit and the new mark is in the same
buffer as the first entry in `global-mark-ring', the first entry
in `global-mark-ring' will be replaced.  Otherwise, a new entry
is pushed onto `global-mark-ring'.

When CONSECUTIVES is set to 'allow-dupes, it is possible to push
an exact duplicate of the current topmost mark onto `global-mark-ring'.

\(fn &optional LOCATION NOMSG ACTIVATE CONSECUTIVES)" t nil)

(defvar back-button-mode nil "\
Non-nil if Back-Button mode is enabled.
See the command `back-button-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `back-button-mode'.")

(custom-autoload 'back-button-mode "back-button" nil)

(autoload 'back-button-mode "back-button" "\
Turn on back-button-mode.

When called interactively with no prefix argument this command
toggles the mode.  With a prefix argument, it enables the mode
if the argument is positive and otherwise disables the mode.

When called from Lisp, this command enables the mode if the
argument is omitted or nil, and toggles the mode if the argument
is 'toggle.

\(fn &optional ARG)" t nil)

(autoload 'back-button-local "back-button" "\
Navigate through `mark-ring', using `back-button-pop-local-mark'.

If the point does not move, continue popping the ring until
motion occurs.

With universal prefix ARG, rotate the ring in the opposite
direction.  (The \"forward\" direction by analogy with a
web browser back-button.)

\(fn ARG)" t nil)

(autoload 'back-button-global "back-button" "\
Navigate through `global-mark-ring', using `pop-global-mark'.

If the point would not move, continue popping the ring until
motion occurs.

With universal prefix ARG, rotate the ring in the opposite
direction.  (The \"forward\" direction by analogy with a
web browser back-button.)

\(fn ARG)" t nil)

(autoload 'back-button-local-backward "back-button" "\
Run `back-button-local' in the backward direction.

Unlike `back-button-local', ignores any prefix argument.

This command is somewhat like a fancier version of
`pop-to-mark-command', though it leaves the mark and
`mark-ring' in a different state.

\(fn)" t nil)

(autoload 'back-button-local-forward "back-button" "\
Run `back-button-local' in the forward direction.

Unlike `back-button-local', ignores any prefix argument.

This command is somewhat like the reverse of
`pop-to-mark-command'.

\(fn)" t nil)

(autoload 'back-button-global-backward "back-button" "\
Run `back-button-global' in the backward direction.

Unlike `back-button-global', ignores any prefix argument.

This command is much like a fancier version of
`pop-global-mark'.

\(fn)" t nil)

(autoload 'back-button-global-forward "back-button" "\
Run `back-button-global' in the forward direction.

Unlike `back-button-global', ignores any prefix argument.

This command is much like the reverse of `pop-global-mark'.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("back-button-pkg.el") (21357 3201 710791
;;;;;;  885000))

;;;***

(provide 'back-button-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; back-button-autoloads.el ends here
