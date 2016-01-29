;; -*- lexical-binding: t -*-

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
            (set (make-variable-buffer-local 'global-hl-line-mode) nil)))

(defun nadvice/completing-read-ivy (&rest _args)
  (ivy-mode +1)
  (advice-remove #'completing-read #'nadvice/completing-read-ivy))

(advice-add 'completing-read :before #'nadvice/completing-read-ivy)

(eval-when-compile
  (with-demoted-errors "Load error: %s"
    (require 'flx-isearch)))

(global-set-key (kbd "C-M-s") #'flx-isearch-forward)
(global-set-key (kbd "C-M-r") #'flx-isearch-backward)

(with-eval-after-load 'swiper
  (defun my/swiper-setup-faces ()
    (set-face-attribute 'swiper-minibuffer-match-face-1 nil
                        :background nil)
    (set-face-attribute 'swiper-minibuffer-match-face-2 nil
                        :background nil
                        :foreground "#268bd2"))

  (my/swiper-setup-faces)
  (add-hook 'load-theme-hook #'my/swiper-setup-faces))

(with-eval-after-load 'ivy
  (ivy-mode +1)

  (defun my/ivy-setup-faces ()
    (set-face-attribute 'ivy-minibuffer-match-face-1 nil
                        :background nil)
    (set-face-attribute 'ivy-minibuffer-match-face-2 nil
                        :background nil
                        :foreground "#268bd2")

    (setq ivy-minibuffer-faces (list 'ivy-minibuffer-match-face-1
                                     'ivy-minibuffer-match-face-2)))

  (eval-when-compile
    (with-demoted-errors "Load error: %s"
      (require 'ivy)))

  (diminish 'ivy-mode)
  (with-eval-after-load 'avy
    (eval-when-compile
      (require 'avy))

    (setf (cdr (assoc 'ivy-avy avy-styles-alist)) 'at-full))

  (setq ivy-display-style 'fancy
        ivy-re-builders-alist '((t . ivy--regex-fuzzy))
        ivy-extra-directories nil
        ivy-count-format ""
        ivy-flx-limit 2000)

  (my/ivy-setup-faces)
  (add-hook 'load-theme-hook #'my/ivy-setup-faces))

(with-eval-after-load 'counsel
  (eval-when-compile
    (require 'counsel))

  (setq counsel-find-file-ignore-regexp
        (eval-when-compile
          (concat "^.*"
           (regexp-opt
                   (list "~"
                         ".elc"
                         ".pyc"
                         ".swp"
                         ".zwc"
                         ".zwc.old"))
           "$"))))

(eval-when-compile
  (with-demoted-errors "Load error: %s"
    (require 'evil)))

(define-key evil-normal-state-map (kbd "C-s") #'swiper)
(define-key evil-insert-state-map (kbd "C-s") #'swiper)

(global-set-key (kbd "M-x") #'counsel-M-x)
(global-set-key (kbd "C-x b") #'ivy-switch-buffer)
(global-set-key (kbd "C-h f") #'counsel-describe-function)
(global-set-key (kbd "C-h v") #'counsel-describe-variable)
(global-set-key (kbd "C-x f") #'counsel-find-file)

(define-key evil-normal-state-map (kbd "SPC SPC") #'counsel-M-x)

;; let M-' intelligently resume whatever completion we were working on
(let ((my/last-used-completion-system))
  (with-eval-after-load 'ivy
    (defun nadvice/ivy--minibuffer-setup (&rest _args)
      (setq my/last-used-completion-system 'ivy))
    (advice-add 'ivy--minibuffer-setup :after #'nadvice/ivy--minibuffer-setup))

  (with-eval-after-load 'helm
    (defun my/helm-last-used-hook ()
      (setq my/last-used-completion-system 'helm))
    (add-hook 'helm-minibuffer-set-up-hook #'my/helm-last-used-hook))

  (defun minibuffer-completion-resume ()
    (interactive)
    (pcase my/last-used-completion-system
      (`helm (call-interactively #'helm-resume))
      (`ivy  (call-interactively #'ivy-resume))
      (_ (message "You haven't used a completion system yet.")))))

(global-set-key (kbd "M-'") #'minibuffer-completion-resume)

(provide 'config-ivy)
