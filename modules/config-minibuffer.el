(eval-when-compile (require 'cl))

(load-library "config-ido")
(load-library "config-helm")

;; bind command to switch to minibuffer
(defun switch-to-minibuffer-window ()
  "switch to minibuffer window (if active)"
  (interactive)
  (when (active-minibuffer-window)
    (select-window (active-minibuffer-window))))

(global-set-key (kbd "C-'") 'switch-to-minibuffer-window)

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

(define-key evil-normal-state-map (kbd "SPC SPC") 'smex)
