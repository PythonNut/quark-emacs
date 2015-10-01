;; -*- lexical-binding: t -*-

(eval-when-compile
  (with-demoted-errors "Load error: %s"
    (require 'evil)
    (require 'ivy)
    (require 'flx-isearch)))

(setq resize-mini-windows t

      ;; don't let the cursor go into minibuffer prompt
      minibuffer-prompt-properties
      '(read-only t
                  point-entered
                  minibuffer-avoid-prompt
                  face
                  minibuffer-prompt)

      ;; recursive minibuffers
      enable-recursive-minibuffers t)

(defun minibuffer-onetime-setup ()
  (unless (featurep 'mb-depth)
    (minibuffer-depth-indicate-mode t))

  (remove-hook 'minibuffer-setup-hook 'minibuffer-onetime-setup))

(add-hook 'minibuffer-setup-hook 'minibuffer-onetime-setup)

;; hl-line-mode breaks minibuffer in TTY
(add-hook 'minibuffer-setup-hook
          (lambda ()
            (make-variable-buffer-local 'global-hl-line-mode)
            (setq global-hl-line-mode nil)))

(setq ido-enable-flex-matching t
      ido-save-directory-list-file
      (expand-file-name "ido.last" user-emacs-directory)
      ido-use-faces nil)

(defun nadvice/completing-read-ivy (&rest _args)
  (ivy-mode +1)
  (advice-remove #'completing-read #'nadvice/completing-read-ivy))

(advice-add 'completing-read :before #'nadvice/completing-read-ivy)

(global-set-key (kbd "C-x b") #'ivy-switch-buffer)
(global-set-key (kbd "C-x f") #'find-file)

(global-set-key (kbd "C-M-s") #'flx-isearch-forward)
(global-set-key (kbd "C-M-r") #'flx-isearch-backward)

(with-eval-after-load 'ivy
  (diminish 'ivy-mode)
  (setq ivy-display-style t
        ivy-re-builders-alist '((t . ivy--regex-fuzzy))
        ivy-extra-directories nil
        ivy-count-format nil))

(define-key evil-normal-state-map (kbd "C-s") #'swiper)
(define-key evil-insert-state-map (kbd "C-s") #'swiper)

(global-set-key (kbd "M-x") #'counsel-M-x)
(define-key evil-normal-state-map (kbd "SPC SPC") #'counsel-M-x)

(provide 'config-ivy)
