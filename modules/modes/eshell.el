(eval-when-compile
  (with-demoted-errors
    (require 'hl-line)
    (require 'em-smart)
    (require 'em-unix)))

(defun my/generic-term-init ()
  ;; this disables key-chord-mode
  (set (make-local-variable 'input-method-function) nil)
  (adaptive-wrap-prefix-mode -1)
  (visual-line-mode -1)
  (yas-minor-mode -1)
  (make-variable-buffer-local 'global-hl-line-mode)
  (make-variable-buffer-local 'scroll-margin)
  (make-variable-buffer-local 'smooth-scroll-margin)
  (setq
    yas-dont-activate t
    global-hl-line-mode nil
    scroll-margin 0
    smooth-scroll-margin 0))

(add-hook 'term-mode-hook #'my/generic-term-init)
(add-hook 'shell-mode-hook #'my/generic-term-init)
(add-hook 'eshell-mode-hook #'my/generic-term-init)

(add-hook 'eshell-mode-hook
  (lambda ()
    (make-variable-buffer-local 'company-idle-delay)))

(defun eshell/clear ()
  (interactive)
  (let ((inhibit-read-only t)) (erase-buffer)))

(defun eshell-kill-whole-line ()
  (interactive)
  (eshell-bol)
  (kill-line))

(defun eshell-onetime-setup ()
  (evil-define-key 'insert eshell-mode-map (kbd "<tab>") #'company-complete)
  (evil-define-key 'insert eshell-mode-map (kbd "C-a") #'eshell-bol)
  (evil-define-key 'insert eshell-mode-map (kbd "<home>") #'eshell-bol)
  (evil-define-key 'insert eshell-mode-map (kbd "<C-S-backspace>") #'eshell-kill-whole-line)
  (evil-define-key 'insert eshell-mode-map (kbd "C-r") #'eshell-isearch-backward)

  (remove-hook 'eshell-mode-hook #'eshell-onetime-setup))

(with-eval-after-load 'eshell
  (add-hook 'eshell-directory-change-hook
    (lambda ()
      (setq company-idle-delay
        (if (file-remote-p default-directory)
          nil
          0.1))))
  (add-hook 'eshell-mode-hook #'eshell-onetime-setup)
  (setq
    eshell-scroll-to-bottom-on-input t
    eshell-scroll-show-maximum-output nil
    eshell-cp-interactive-query t
    eshell-ln-interactive-query t
    eshell-mv-interactive-query t
    eshell-rm-interactive-query t
    eshell-mv-overwrite-files nil))
