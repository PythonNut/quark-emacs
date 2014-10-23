(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
(auto-compression-mode +1)
(transient-mark-mode +1)
(delete-selection-mode +1)

;; encryption mode
(setq epa-file-name-regexp "\\.\\(gpg\\|asc\\)$")
(epa-file-name-regexp-update)
(setenv "GPG_AGENT_INFO" nil)
