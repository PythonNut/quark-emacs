;; -*- lexical-binding: t -*-

(eval-when-compile
  (with-demoted-errors "Load error: %s"
    (require 'ivy)
    (require 'flx-isearch)
    (require 'smex)))

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

(defun nadvice/completing-read-ivy (old-fun &rest args)
  (ivy-mode +1)
  (advice-remove #'completing-read #'nadvice/completing-read-ivy))

(advice-add 'completing-read :before #'nadvice/completing-read-ivy)

(global-set-key (kbd "C-x b") #'ivy-switch-buffer)
(global-set-key (kbd "C-x f") #'find-file)

(with-eval-after-load 'smex
  (setq smex-save-file
        (expand-file-name
         "smex-items"
         user-emacs-directory)))

(global-set-key (kbd "M-x") #'smex)

(global-set-key (kbd "C-M-s") #'flx-isearch-forward)
(global-set-key (kbd "C-M-r") #'flx-isearch-backward)

(with-eval-after-load 'ivy
  (require 'flx)
  (setq ivy-display-style t
        ivy-extra-directories nil
        ivy-wrap t
        ivy-use-virtual-buffers t
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

             ;; filter out non-fuzzy-matching candidates
             (cands (let ((res))
                      ;; this also lets us avoid a copy-seq
                      (dolist (cand candidates)
                        (when (string-match-p fuzzy-regex cand)
                          (push cand res)))
                      res))

             ;; partition the candidates into sorted and unsorted groups
             (cands-left)
             (cands-to-sort (if (< (length cands) my/ivy-flx-limit)
                                (progn
                                  (setq cands-left nil)
                                  cands)
                              (setq cands-left (sort (let ((res))
                                                       (dolist (cand cands)
                                                         (push cand res))
                                                       res)
                                                     (lambda (c1 c2)
                                                       (< (length c1)
                                                          (length c2)))))
                              (let ((num (min my/ivy-flx-limit
                                              (length cands)))
                                    (result nil))
                                ;; take the first num elements from cands-left
                                ;; and add them to result (cands-to-sort)
                                (while (and cands-left
                                            (>= (setq num (1- num)) 0))
                                  (push (pop cands-left) result))
                                result))))
        (append
         ;; compute all of the flx scores in one pass and sort
         (mapcar #'car
                 (sort (mapcar
                        (lambda (cand)
                          (cons cand
                                (or (car (flx-score cand
                                                    name
                                                    my/ivy-cache))
                                    0)))
                        cands-to-sort)
                       (lambda (c1 c2)
                         (> (cdr c1)
                            (cdr c2)))))

         ;; add the unsorted candidates
         cands-left))))

  (advice-add 'ivy--filter :override #'nadvice/ivy--filter))

(provide 'config-ido)
