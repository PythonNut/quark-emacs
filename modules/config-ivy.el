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
            (setq-local global-hl-line-mode nil)))

(use-package flx-isearch
  :commands (flx-isearch-forward
             flx-isearch-backward)
  :init
  (global-set-key (kbd "C-M-s") #'flx-isearch-forward)
  (global-set-key (kbd "C-M-r") #'flx-isearch-backward))

(use-package historian
  :config
  (setq historian-save-file (locate-user-emacs-file "data/.historian")))

(use-package ivy-historian)

(use-package ivy
  :init
  ;; shamelessly stolen from the el-patch docs
  (el-patch-defvar ivy-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map [remap switch-to-buffer]
        'ivy-switch-buffer)
      (define-key map [remap switch-to-buffer-other-window]
        'ivy-switch-buffer-other-window)
      map)
    "Keymap for `ivy-mode'.")

  (el-patch-define-minor-mode ivy-mode
    "Toggle Ivy mode on or off.
Turn Ivy mode on if ARG is positive, off otherwise.
Turning on Ivy mode sets `completing-read-function' to
`ivy-completing-read'.

Global bindings:
\\{ivy-mode-map}

Minibuffer bindings:
\\{ivy-minibuffer-map}"
    :group 'ivy
    :global t
    :keymap ivy-mode-map
    :lighter " ivy"
    (if ivy-mode
        (progn
          (setq completing-read-function 'ivy-completing-read)
          ;; effectively, ivy-do-completion-in-region = 2
          (el-patch-splice 2
            (when ivy-do-completion-in-region
              (setq completion-in-region-function 'ivy-completion-in-region))))
      (setq completing-read-function 'completing-read-default)
      (setq completion-in-region-function 'completion--in-region)))

  (ivy-mode +1)

  ;; needs to be done late.
  (diminish 'ivy-mode)

  (historian-mode +1)

  :config
  (eval-when-compile
    (with-demoted-errors "Load error: %s"
      (require 'historian)))

  (ivy-historian-mode +1)

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
          (rx line-start
              (zero-or-more not-newline)
              (or "~" ".elc" ".pyc" ".swp" ".zwc" ".zwc.old")
              line-end))))

(with-eval-after-load 'evil
  (eval-when-compile
    (with-demoted-errors "Load error: %s"
      (require 'evil)))

  (define-key evil-normal-state-map (kbd "C-s") #'isearch-forward-regexp)
  (define-key evil-insert-state-map (kbd "C-s") #'isearch-forward-regexp)
  (define-key evil-normal-state-map (kbd "SPC SPC") #'counsel-M-x))

(defun isearch-region-dwim-helper ()
  (when (region-active-p)
    (let* ((beg (min (mark) (point)))
           (end (+ (max (mark) (point)) (if (or (evil-normal-state-p)
                                                (evil-visual-state-p)
                                                (evil-motion-state-p)
                                                (evil-operator-state-p))
                                            1 0)))
           (search-text (buffer-substring-no-properties beg end))
           (symbol-bounds (bounds-of-thing-at-point 'symbol)))
      (when (and search-text
                 ;; Assume that multi-line regions should be extended,
                 ;; not searched literally.
                 (= (line-number-at-pos beg)
                    (line-number-at-pos end)))
        (deactivate-mark)
        (setq isearch-regexp t
              ;; If region is a subregion of the current symbol, then
              ;; limit it to the contents of symbols in the current buffer
              isearch-string (if (and (car symbol-bounds)
                                      (>= beg (car symbol-bounds))
                                      (<= end (cdr symbol-bounds)))
                                 (concat (rx symbol-start)
                                         ;; If the region matches the
                                         ;; beginning or end of a symbol
                                         ;; anchor it there.
                                         (if (= beg (car symbol-bounds)) ""
                                           (rx (zero-or-more (or (syntax _)
                                                                 (syntax w)))))
                                         (regexp-quote search-text)
                                         (if (= end (cdr symbol-bounds)) ""
                                           (rx (zero-or-more (or (syntax _)
                                                                 (syntax w)))))
                                         (rx symbol-end))
                               (regexp-quote search-text))
              isearch-message (mapconcat #'isearch-text-char-description
                                         isearch-string
                                         ""))))))

(add-hook 'isearch-mode-hook #'isearch-region-dwim-helper)

(use-package smex
  :config
  (eval-when-compile
    (with-demoted-errors "Load error: %s"
      (require 'smex)))

  (setq smex-save-file (locate-user-emacs-file ".smex-items")))

(global-set-key (kbd "M-x") #'counsel-M-x)
(global-set-key (kbd "C-x b") #'ivy-switch-buffer)
(global-set-key (kbd "C-h f") #'counsel-describe-function)
(global-set-key (kbd "C-h v") #'counsel-describe-variable)
(global-set-key (kbd "C-x f") #'counsel-find-file)

(global-set-key (kbd "C-S-y") #'counsel-yank-pop)

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
