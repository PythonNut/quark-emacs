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

(with-eval-after-load 'ivy
  (ivy-mode +1)
  (package-deferred-install '(historian :repo "PythonNut/historian.el"
                                        :fetcher github)
      :feature-name 'historian
      :autoload-names '('historian-mode
                        'historian-load
                        'historian-save))

  (eval-when-compile
    (require 'historian))

  (historian-mode +1)

  (defvar historian-ivy-freq-boost-factor 100)
  (defvar historian-ivy-recent-boost 100)
  (defvar historian-ivy-recent-decrement 5)
  (defvar historian--history-table (make-hash-table))

  (defun historian--nadvice/ivy-read/save-this-command (old-fun &rest args)
    (cl-letf ((saved-this-command this-command))
      (apply old-fun args)))

  (advice-add 'ivy-read :around #'historian--nadvice/ivy-read/save-this-command)

  (defun historian--nadvice/ivy--flx-sort (old-fun name cands)
    (if (not (bound-and-true-p historian-mode))
        (funcall old-fun name cands)
      (require 'flx)
      (cl-letf*
          ((old-flx-score (symbol-function #'flx-score))
           ((symbol-function #'flx-score)
            (lambda (str query &optional cache)
              (let* ((orig-score
                      (funcall old-flx-score str query cache))
                     (history (gethash (bound-and-true-p
                                        saved-this-command)
                                       historian--history-table)))
                (if history
                    (let ((freq (if (gethash str (cdr history))
                                    (/ (float (gethash str (cdr history) 0))
                                       (let ((total 0))
                                         (maphash
                                          (lambda (key value)
                                            (cl-incf total value))
                                          (cdr history))
                                         total))
                                  0))
                          (freq-boost (* freq historian-ivy-freq-boost-factor))
                          (recent-index (cl-position str (car history)))
                          (recent-boost (if recent-index
                                            (- historian-ivy-recent-boost
                                               (* historian-ivy-recent-decrement
                                                  recent-index))
                                          0)))
                      (message "%s %s %s %s %s" str orig-score freq-boost recent-boost history)
                      (cons
                       (+ (car orig-score) freq-boost recent-boost)
                       (cdr orig-score)))
                  orig-score)))))
        (funcall old-fun name cands))))

  (advice-add 'ivy--flx-sort :around #'historian--nadvice/ivy--flx-sort)

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

(eval-when-compile
  (with-demoted-errors "Load error: %s"
    (require 'evil)))

(define-key evil-normal-state-map (kbd "C-s") #'isearch-forward-regexp)
(define-key evil-insert-state-map (kbd "C-s") #'isearch-forward-regexp)

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

(with-eval-after-load 'smex
  (setq smex-save-file (locate-user-emacs-file ".smex-items")))

(global-set-key (kbd "M-x") #'counsel-M-x)
(global-set-key (kbd "C-x b") #'ivy-switch-buffer)
(global-set-key (kbd "C-h f") #'counsel-describe-function)
(global-set-key (kbd "C-h v") #'counsel-describe-variable)
(global-set-key (kbd "C-x f") #'counsel-find-file)

(define-key evil-normal-state-map (kbd "SPC SPC") #'counsel-M-x)
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
