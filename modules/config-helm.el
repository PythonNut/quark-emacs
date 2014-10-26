(require 'helm-config)
(require 'helm-imenu)
(require 'helm-files)
(eval-when-compile (require 'cl))

(run-with-idle-timer 5 nil
  '(lambda ()
     (require 'helm-ring)
     (run-with-idle-timer 5 nil
       '(lambda ()
          (require 'helm-misc)))))

(eval-after-load 'helm
  '(progn
     (setq helm-locate-command "locate %s -r %s -be -l 500")
     (setq helm-ff-transformer-show-only-basename nil)
     (setq helm-buffers-fuzzy-matching t)

     (set-face-attribute 'helm-selection nil :underline 'nil)
     (add-to-list 'helm-boring-file-regexp-list "\\.undo.xz$")
     (add-to-list 'helm-boring-file-regexp-list "\\#$")
     (setq helm-ff-newfile-prompt-p 'nil)

     (global-set-key (kbd "C-x f") 'ido-find-file)
     (global-set-key (kbd "C-S-x C-S-f") 'icicle-find-file)
     (global-set-key (kbd "C-S-X C-S-B") 'icicle-buffer)

     (global-set-key (kbd "M-:") 'helm-eval-expression)
     (global-set-key (kbd "C-c C-s") 'helm-swoop)

     (setq helm-locate
       `((name . "Locate")
          (init . helm-locate-set-command)
          (candidates-process . helm-locate-init)
          (type . file)
          (requires-pattern . 3)
          (history . ,'helm-file-name-history)
          (keymap . ,helm-generic-files-map)
          (help-message . helm-generic-file-help-message)
          (candidate-number-limit . 999)
          (mode-line . helm-generic-file-mode-line-string)))

     (defun my-helm-buffers (&rest arg)
       (interactive)
       (helm-other-buffer
         '(helm-source-buffers-list
            helm-source-recentf
            helm-source-files-in-current-dir
            helm-source-files-in-all-dired
            helm-source-buffer-not-found)
         "*helm-find-files"))

     (global-set-key (kbd "C-x C-b") 'my-helm-buffers)
     (global-set-key (kbd "C-x b") 'ido-switch-buffer)

     (defun my-helm-find-files (&rest arg)
       (interactive)
       (helm-other-buffer
         '(helm-source-recentf
            helm-source-buffers-list
            helm-source-files-in-current-dir
            helm-source-find-files
            helm-source-findutils
            helm-source-locate
            ;; helm-source-tracker-search
            )
         "*helm-find-files"))

     (global-set-key (kbd "C-x C-f") 'my-helm-find-files)

     (defvar my-helm-source-evaluation-result
       '((name . "Evaluation Result")
          (init . (lambda () (require 'edebug)))
          (dummy)
          (multiline)
          (mode-line . "C-RET: nl-and-indent, tab: reindent, C-tab:complete, C-p/n: next/prec-line.")
          (filtered-candidate-transformer .
            (lambda (candidates source)
              (list
                (condition-case nil
                  (with-helm-current-buffer
                    (pp-to-string
                      (if edebug-active
                        (edebug-eval-expression
                          (read helm-pattern))
                        (eval (read helm-pattern)))))
                  (error "")))))
          (action . (("Copy result to kill-ring" .
                       (lambda (candidate)
                         (with-current-buffer helm-buffer
                           (let ((end (save-excursion
                                        (goto-char (point-max))
                                        (search-backward "\n")
                                        (point))))
                             (kill-region (point) end)))))
                      ("copy sexp to kill-ring" .
                        (lambda (candidate)
                          (kill-new helm-input)))))))

     (defun my-helm-omni (&rest arg)
       (interactive)
       (helm-occur-init-source)
       (unless (fboundp 'helm-source-kill-ring)
         (require 'helm-ring))
       (unless (fboundp 'helm-source-lacarte)
         (require 'helm-misc))
       (unless (fboundp 'helm-source-semantic)
         (require 'helm-semantic))

       (let ((bufs (list (buffer-name (current-buffer)))))
         (helm-attrset 'moccur-buffers bufs helm-source-occur)
         (helm-set-local-variable 'helm-multi-occur-buffer-list bufs)
         (helm-set-local-variable
           'helm-multi-occur-buffer-tick
           (cl-loop for b in bufs
             collect (buffer-chars-modified-tick (get-buffer b))))
         (helm-other-buffer
           (append '(helm-source-buffers-list)

             ;; projectile explodes when not in project
             (when (my-projectile-project-root)
               (unless (fboundp 'helm-source-projectile-files-list)
                 (require 'helm-projectile)
                 (unless projectile-global-mode-buffers
                   (projectile-global-mode +1)))
               '(helm-source-projectile-recentf-list
                  helm-source-projectile-files-list
                  helm-source-projectile-buffers-list))

             '( ;; files
                helm-source-file-cache
                helm-source-recentf
                helm-source-files-in-current-dir
                helm-source-bookmarks
                ;; code search
		helm-source-semantic
                helm-source-imenu
                helm-source-occur
                ;; internal
                helm-source-kill-ring
                helm-source-mark-ring
                helm-source-register
                ;; code construction
                ;; helm-source-regexp
                helm-source-lacarte
                my-helm-source-evaluation-result
                ;; file location
                helm-source-findutils
                helm-source-locate
                ;; helm-source-tracker-search
                ;; fallback
                ;; helm-source-buffer-not-found
                ))
           "*helm-omni*")))

     (defadvice  evil-paste-pop (around auto-helm-omni activate)
       (if (memq last-command
	     '(evil-paste-after
		evil-paste-before
		evil-visual-paste))
         ad-do-it
         (call-interactively 'my-helm-omni)))

     (global-set-key (kbd "C-c C-o") 'my-helm-omni)
     (define-key evil-normal-state-map (kbd "C-c C-o") 'my-helm-omni)
     (define-key evil-insert-state-map (kbd "C-c C-o") 'my-helm-omni)
     (define-key evil-emacs-state-map (kbd "C-c C-o") 'my-helm-omni)
     (define-key evil-motion-state-map (kbd "C-c C-o") 'my-helm-omni)
     (define-key evil-replace-state-map (kbd "C-c C-o") 'my-helm-omni)))
