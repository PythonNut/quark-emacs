(eval-when-compile (require 'cl))

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
(auto-compression-mode +1)
(transient-mark-mode +1)
(delete-selection-mode +1)
(global-hl-line-mode +1)

;; encryption mode
(setq epa-file-name-regexp "\\.\\(gpg\\|asc\\)$")
(epa-file-name-regexp-update)
(setenv "GPG_AGENT_INFO" nil)

(defun raise-minor-mode-map-alist (mode-symbol)
  "Raise `minor-mode-map-alist' priority of MODE-SYMBOL."
  (let ((x (assq mode-symbol minor-mode-map-alist)))
    (and x (setq minor-mode-map-alist (cons x (delq x minor-mode-map-alist))))))

(defun lower-minor-mode-map-alist (mode-symbol)
  "Lower `minor-mode-map-alist' priority of MODE-SYMBOL."
  (let ((rel (assq mode-symbol minor-mode-map-alist)))
    (setq minor-mode-map-alist (append (delete rel minor-mode-map-alist) (list rel)))))
