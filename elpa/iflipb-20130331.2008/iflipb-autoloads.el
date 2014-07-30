;;; iflipb-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (iflipb-previous-buffer iflipb-next-buffer) "iflipb"
;;;;;;  "iflipb.el" (21120 1645 942801 773000))
;;; Generated autoloads from iflipb.el

(autoload 'iflipb-next-buffer "iflipb" "\
Flip to the next buffer in the buffer list. Consecutive
invocations switch to less recent buffers in the buffer list.
Buffers matching iflipb-always-ignore-buffers are always ignored.
Without a prefix argument, buffers matching iflipb-ignore-buffers
are also ignored.

\(fn ARG)" t nil)

(autoload 'iflipb-previous-buffer "iflipb" "\
Flip to the previous buffer in the buffer list. Consecutive
invocations switch to more recent buffers in the buffer list.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("iflipb-pkg.el") (21120 1645 962560 447000))

;;;***

(provide 'iflipb-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; iflipb-autoloads.el ends here
