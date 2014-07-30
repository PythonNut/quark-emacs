;;; smart-mode-line-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (sml/setup) "smart-mode-line" "smart-mode-line.el"
;;;;;;  (21449 63124 118357 297000))
;;; Generated autoloads from smart-mode-line.el

(when load-file-name (let ((dir (file-name-as-directory (file-name-directory load-file-name)))) (add-to-list 'custom-theme-load-path dir) (when (file-directory-p (file-name-as-directory (concat dir "themes"))) (add-to-list 'custom-theme-load-path (file-name-as-directory (concat dir "themes"))))))

(autoload 'sml/setup "smart-mode-line" "\
Setup the mode-line to be smart and sexy.

ARG is ignored. Just call this function in your init file, and it
will be evaluated after Emacs finished initializing (we do this
to make sure that we are loaded after any themes).

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "smart-mode-line-dark-theme" "smart-mode-line-dark-theme.el"
;;;;;;  (21449 63124 168358 44000))
;;; Generated autoloads from smart-mode-line-dark-theme.el

(when load-file-name (add-to-list 'custom-theme-load-path (file-name-as-directory (file-name-directory load-file-name))))

;;;***

;;;### (autoloads nil "smart-mode-line-light-theme" "smart-mode-line-light-theme.el"
;;;;;;  (21449 63124 158357 897000))
;;; Generated autoloads from smart-mode-line-light-theme.el

(when load-file-name (add-to-list 'custom-theme-load-path (file-name-as-directory (file-name-directory load-file-name))))

;;;***

;;;### (autoloads nil "smart-mode-line-respectful-theme" "smart-mode-line-respectful-theme.el"
;;;;;;  (21449 63124 98356 984000))
;;; Generated autoloads from smart-mode-line-respectful-theme.el

(when load-file-name (add-to-list 'custom-theme-load-path (file-name-as-directory (file-name-directory load-file-name))))

;;;***

;;;### (autoloads nil nil ("smart-mode-line-pkg.el") (21449 63124
;;;;;;  187508 249000))

;;;***

(provide 'smart-mode-line-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; smart-mode-line-autoloads.el ends here
