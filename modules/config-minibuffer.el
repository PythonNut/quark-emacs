(eval-when-compile
  (progn
    (require 'cl)
    (require 'ido-ubiquitous)
    (require 'evil-leader)))

(require 'config-ido)
(require 'config-helm)
(require 'config-icicles)

(global-set-key (kbd "C-S-s") #'icicle-search-generic)

;; bind command to switch to minibuffer
(defun switch-to-minibuffer-window ()
  "switch to minibuffer window (if active)"
  (interactive)
  (when (active-minibuffer-window)
    (select-window (active-minibuffer-window))))

(global-set-key (kbd "C-'") #'switch-to-minibuffer-window)

;; don't let the cursor go into minibuffer prompt
(setq minibuffer-prompt-properties
  '(read-only t
     point-entered
     minibuffer-avoid-prompt
     face
     minibuffer-prompt))

;; recursive minibuffers
(setq
  enable-recursive-minibuffers t
  resize-mini-windows t)

(minibuffer-depth-indicate-mode t)

(define-key evil-normal-state-map (kbd "SPC SPC") #'smex)

(provide 'config-minibuffer)
