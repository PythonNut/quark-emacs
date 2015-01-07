(eval-when-compile
  (with-demoted-errors
    (require 'cl)
    (require 'ido-ubiquitous)
    (require 'evil-leader)))

(require 'config-ido)
(require 'config-helm)
(require 'config-icicles)

;; bind command to switch to minibuffer
(defun switch-to-minibuffer-window ()
  "switch to minibuffer window (if active)"
  (interactive)
  (if (active-minibuffer-window)
    (select-window (active-minibuffer-window))
    (message "Minibuffer is not active")))

(global-set-key (kbd "C-'") #'switch-to-minibuffer-window)

(defun minibuffer-onetime-setup ()
  (unless (featurep 'mb-depth)
    (minibuffer-depth-indicate-mode t))

  (setq
    ;; don't let the cursor go into minibuffer prompt
    minibuffer-prompt-properties
    '(read-only t
       point-entered
       minibuffer-avoid-prompt
       face
       minibuffer-prompt)

    ;; recursive minibuffers
    enable-recursive-minibuffers t
    resize-mini-windows t)

  (remove-hook 'minibuffer-setup-hook 'minibuffer-onetime-setup))

(add-hook 'minibuffer-setup-hook 'minibuffer-onetime-setup)

(define-key evil-normal-state-map (kbd "SPC SPC") #'smex)

;; hl-line-mode breaks minibuffer in TTY
(add-hook 'minibuffer-setup-hook
  (lambda ()
    (make-variable-buffer-local 'global-hl-line-mode)
    (setq global-hl-line-mode nil)))


(provide 'config-minibuffer)
