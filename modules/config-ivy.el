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
  (minibuffer-depth-indicate-mode t)
  (remove-hook 'minibuffer-setup-hook #'minibuffer-onetime-setup))

(add-hook 'minibuffer-setup-hook #'minibuffer-onetime-setup)

;; hl-line-mode breaks minibuffer in TTY
(add-hook 'minibuffer-setup-hook
          (lambda ()
            (make-variable-buffer-local 'global-hl-line-mode)
            (setq global-hl-line-mode nil)))

(defun nadvice/completing-read-ivy (&rest _args)
  (ivy-mode +1)
  (advice-remove #'completing-read #'nadvice/completing-read-ivy))

(advice-add 'completing-read :before #'nadvice/completing-read-ivy)

(global-set-key (kbd "C-x b") #'ivy-switch-buffer)
(global-set-key (kbd "C-x f") #'find-file)

(global-set-key (kbd "C-M-s") #'flx-isearch-forward)
(global-set-key (kbd "C-M-r") #'flx-isearch-backward)

(with-eval-after-load 'swiper
  (set-face-attribute 'swiper-minibuffer-match-face-1 nil
                      :background nil)
  (set-face-attribute 'swiper-minibuffer-match-face-2 nil
                      :background nil
                      :foreground "#268bd2")
  (setq swiper-minibuffer-faces (list 'swiper-minibuffer-match-face-1
                                      'swiper-minibuffer-match-face-2)))

(with-eval-after-load 'ivy
  (diminish 'ivy-mode)
  (setq ivy-display-style 'fancy
        ivy-re-builders-alist '((t . ivy--regex-fuzzy))
        ivy-extra-directories nil
        ivy-count-format nil))

(define-key evil-normal-state-map (kbd "C-s") #'swiper)
(define-key evil-insert-state-map (kbd "C-s") #'swiper)

(global-set-key (kbd "M-x") #'counsel-M-x)
(define-key evil-normal-state-map (kbd "SPC SPC") #'counsel-M-x)

(provide 'config-ivy)
