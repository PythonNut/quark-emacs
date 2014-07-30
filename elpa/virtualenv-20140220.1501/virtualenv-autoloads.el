;;; virtualenv-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (virtualenv-minor-mode virtualenv-deactivate virtualenv-workon)
;;;;;;  "virtualenv" "virtualenv.el" (21254 52596 424061 445000))
;;; Generated autoloads from virtualenv.el

(autoload 'virtualenv-workon "virtualenv" "\
Activate a virtual environment for python.
Optional argument ENV if non-nil, either use the string given as
the virtual environment or if not a string then query the user.

\(fn &optional ENV)" t nil)

(autoload 'virtualenv-deactivate "virtualenv" "\


\(fn)" t nil)

(autoload 'virtualenv-minor-mode "virtualenv" "\
Toggle Virtualenv minor mode on or off.
With a prefix argument ARG, enable Virtualenv minor mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil, and toggle it if ARG is `toggle'.
\\{virtualenv-minor-mode-map}

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("virtualenv-pkg.el") (21254 52596 436114
;;;;;;  524000))

;;;***

(provide 'virtualenv-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; virtualenv-autoloads.el ends here
