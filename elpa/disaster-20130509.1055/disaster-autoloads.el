;;; disaster-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (disaster) "disaster" "disaster.el" (21538 18080
;;;;;;  353194 436000))
;;; Generated autoloads from disaster.el

(autoload 'disaster "disaster" "\
Shows assembly code for current line of C/C++ file.

Here's the logic path it follows:

- Is there a Makefile in this directory? Run `make bufname.o`.
- Or is there a Makefile in a parent directory? Run `make -C .. bufname.o`.
- Or is this a C file? Run `cc -g -O3 -c -o bufname.o bufname.c`
- Or is this a C++ file? Run `c++ -g -O3 -c -o bufname.o bufname.c`
- If build failed, display errors in compile-mode.
- Run objdump inside a new window while maintaining focus.
- Jump to line matching current line.

If FILE and LINE are not specified, the current editing location
is used.

\(fn &optional FILE LINE)" t nil)

;;;***

;;;### (autoloads nil nil ("disaster-pkg.el") (21538 18080 374593
;;;;;;  657000))

;;;***

(provide 'disaster-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; disaster-autoloads.el ends here
