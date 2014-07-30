;;; browse-kill-ring-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (browse-kill-ring browse-kill-ring-default-keybindings)
;;;;;;  "browse-kill-ring" "browse-kill-ring.el" (21449 63323 298104
;;;;;;  227000))
;;; Generated autoloads from browse-kill-ring.el

(autoload 'browse-kill-ring-default-keybindings "browse-kill-ring" "\
Set up M-y (`yank-pop') so that it can invoke `browse-kill-ring'.
Normally, if M-y was not preceeded by C-y, then it has no useful
behavior.  This function sets things up so that M-y will invoke
`browse-kill-ring'.

\(fn)" t nil)

(autoload 'browse-kill-ring "browse-kill-ring" "\
Display items in the `kill-ring' in another buffer.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("browse-kill-ring-pkg.el") (21449 63323
;;;;;;  377430 786000))

;;;***

(provide 'browse-kill-ring-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; browse-kill-ring-autoloads.el ends here
