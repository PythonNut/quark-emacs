;;; region-bindings-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (region-bindings-mode) "region-bindings-mode" "region-bindings-mode.el"
;;;;;;  (21317 33604 876844 334000))
;;; Generated autoloads from region-bindings-mode.el

(autoload 'region-bindings-mode "region-bindings-mode" "\
Enable special bindings when working with an active region.

Do not invoke `region-bindings-mode' directly!

Toggling the mode on and off via this function will simply
enable/disable the bindings, but it will not honour
`region-bindings-mode-disabled-modes' or
`region-bindings-mode-disable-predicates', or toggle activation
of the hooks which automatically enable/disable the bindings when
the mark is activated or deactivated.

Instead, call `region-bindings-mode-enable' and
`region-bindings-mode-enable'.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("region-bindings-mode-pkg.el") (21317
;;;;;;  33604 887938 684000))

;;;***

(provide 'region-bindings-mode-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; region-bindings-mode-autoloads.el ends here
