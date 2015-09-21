;; -*- lexical-binding: t -*-

(eval-when-compile
  (with-demoted-errors "Load error: %s"
    (require 'evil)
    (require 'ivy)
    (require 'flx-isearch)))

(defun minibuffer-onetime-setup ()
  (unless (featurep 'mb-depth)
    (minibuffer-depth-indicate-mode t))

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
  (require 'flx)
  (setq ivy-display-style t
        ivy-extra-directories nil
        ivy-wrap t
        ivy-sort-functions-alist '((t . nil)))

  (defvar my/ivy-cache (flx-make-string-cache))
  (defvar my/ivy-flx-limit 500)

  (defun nadvice/ivy--filter (name candidates)
    (if (= (length name) 0)
        candidates
      (let* (;; an optimized regex for fuzzy matching
             ;; "abc" → "\\`[^a]*a[^b]*b[^c]*c"
             (fuzzy-regex (concat "\\`"
                                  (mapconcat
                                   (lambda (x)
                                     (setq x (string x))
                                     (concat "[^" x "]*" (regexp-quote x)))
                                   name
                                   "")))

             ;; disable side-effects of string-match
             (inhibit-changing-match-data t)
             (cands-left)
             (cands-to-sort))

        ;; filter out non-matching candidates
        (dolist (cand candidates)
          (when (string-match fuzzy-regex cand)
            (push cand cands-left)))

        ;; pre-sort the candidates by length before partitioning
        (setq cands-left (sort cands-left
                               (lambda (c1 c2)
                                 (< (length c1)
                                    (length c2)))))

        ;; partition the candidates into sorted and unsorted groups
        (dotimes (_n (min (length cands-left) my/ivy-flx-limit))
          (push (pop cands-left) cands-to-sort))

        (append
         ;; compute all of the flx scores in one pass and sort
         (mapcar #'car
                 (sort (mapcar
                        (lambda (cand)
                          (cons cand
                                (car (flx-score cand
                                                name
                                                my/ivy-cache))))
                        cands-to-sort)
                       (lambda (c1 c2)
                         (> (cdr c1)
                            (cdr c2)))))

         ;; add the unsorted candidates
         cands-left))))

  (advice-add 'ivy--filter :override #'nadvice/ivy--filter))

(define-key evil-normal-state-map (kbd "C-s") #'swiper)
(define-key evil-insert-state-map (kbd "C-s") #'swiper)

(provide 'config-ivy)
