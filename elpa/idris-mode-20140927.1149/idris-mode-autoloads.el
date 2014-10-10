;;; idris-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (idris-ipkg-mode) "idris-ipkg-mode" "idris-ipkg-mode.el"
;;;;;;  (21560 16613 502810 544000))
;;; Generated autoloads from idris-ipkg-mode.el

(autoload 'idris-ipkg-mode "idris-ipkg-mode" "\
Major mode for Idris package files
     \\{idris-ipkg-mode-map}
Invokes `idris-ipkg-mode-hook'.

\(fn)" t nil)

;;;***

;;;### (autoloads (idris-mode) "idris-mode" "idris-mode.el" (21560
;;;;;;  16613 542830 543000))
;;; Generated autoloads from idris-mode.el

(autoload 'idris-mode "idris-mode" "\
Major mode for Idris
     \\{idris-mode-map}
Invokes `idris-mode-hook'.

\(fn)" t nil)

(push '("\\.idr$" . idris-mode) auto-mode-alist)

(push '("\\.lidr$" . idris-mode) auto-mode-alist)

(eval-after-load 'flycheck '(progn (flycheck-define-checker idris "An Idris syntax and type checker." :command ("idris" "--check" "--nocolor" "--warnpartial" source) :error-patterns ((warning line-start (file-name) ":" line ":" column ":Warning - " (message (and (* nonl) (* "\n" (not (any "/" "~")) (* nonl))))) (error line-start (file-name) ":" line ":" column ":" (message (and (* nonl) (* "\n" (not (any "/" "~")) (* nonl)))))) :modes idris-mode) (add-to-list 'flycheck-checkers 'idris)))

;;;***

;;;### (autoloads (turn-on-idris-simple-indent idris-simple-indent-mode)
;;;;;;  "idris-simple-indent" "idris-simple-indent.el" (21560 16613
;;;;;;  532825 543000))
;;; Generated autoloads from idris-simple-indent.el

(autoload 'idris-simple-indent-mode "idris-simple-indent" "\
Simple Idris indentation mode that uses simple heuristic.
In this minor mode, `indent-for-tab-command' (bound to <tab> by
default) will move the cursor to the next indent point in the
previous nonblank line, whereas `idris-simple-indent-backtab'
\(bound to <backtab> by default) will move the cursor the
previous indent point. An indent point is a non-whitespace
character following whitespace.

Runs `idris-simple-indent-hook' on activation.

\(fn &optional ARG)" t nil)

(autoload 'turn-on-idris-simple-indent "idris-simple-indent" "\
Turn on function `idris-simple-indent-mode'.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("idris-commands.el" "idris-common-utils.el"
;;;;;;  "idris-compat.el" "idris-core.el" "idris-events.el" "idris-info.el"
;;;;;;  "idris-keys.el" "idris-log.el" "idris-metavariable-list.el"
;;;;;;  "idris-mode-pkg.el" "idris-prover.el" "idris-repl.el" "idris-settings.el"
;;;;;;  "idris-syntax.el" "idris-warnings-tree.el" "idris-warnings.el"
;;;;;;  "inferior-idris.el") (21560 16613 652042 314000))

;;;***

(provide 'idris-mode-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; idris-mode-autoloads.el ends here
