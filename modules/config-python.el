;; -*- lexical-binding: t -*-
(eval-when-compile (require 'config-macros))

;; =============================================================================
;; Python ======================================================================
;; =============================================================================

(with-eval-after-load 'pythonic
  (eval-when-compile
    (with-demoted-errors "Load error: %s"
      (require 'tramp)))

  (let (my/pythonic-remote-host-cache)
    (defun nadvice/pythonic-remote-host ()
      "Get host of the connection to the remote python interpreter."
      (unless my/pythonic-remote-host-cache
        (setq my/pythonic-remote-host-cache (make-hash-table :test #'equal)))

      (with-parsed-tramp-file-name default-directory parsed
        (let ((hostname (replace-regexp-in-string "#.*\\'" "" parsed-host)))
          (if (member parsed-method '("ssh"
                                      "scp"
                                      "scpx"
                                      "sshx"
                                      "rsync"))
              (or (gethash hostname my/pythonic-remote-host-cache)
                  (puthash hostname
                           (with-temp-buffer
                             (call-process "ssh" nil t nil "-G" hostname)
                             (goto-char (point-min))
                             (search-forward "\nhostname ")
                             (buffer-substring-no-properties (point) (line-end-position)))
                           my/pythonic-remote-host-cache))
            hostname)))))

  (advice-add 'pythonic-remote-host :override #'nadvice/pythonic-remote-host))

(use-package python
  :ensure nil
  :config
  (add-hook 'python-mode-hook
            (lambda ()
              ;; conflicts with `eldoc-mode'
              (semantic-idle-summary-mode -1)
              (setq mode-name "Py")))

  (define-key python-mode-map (kbd "M-RET") #'srefactor-refactor-at-point)

  (use-package company-anaconda
    :defer-install t
    :commands (company-anaconda))

  (use-package anaconda-mode
    :defer-install t
    :commands (anaconda-mode)
    :diminish (anaconda-mode . " ✶")
    :config
    (setq anaconda-mode-installation-directory (locate-user-emacs-file
                                                "data/anaconda-mode"))

    (defun my/anaconda-eldoc-callback-fallback (docstring)
      (let ((docstring
             (cond ((stringp docstring)
                    docstring)
                   ((arrayp docstring)
                    (s-join " " (--map
                                 (s-collapse-whitespace
                                  (aref it 3))
                                 docstring))))))
        (when docstring
          (eldoc-message
           (substring docstring 0 (min (frame-width) (length docstring)))))))

    (defun my/anaconda-eldoc-callback (result)
      (if result
          (eldoc-message (anaconda-mode-eldoc-format result))
        (anaconda-mode-call
         "goto_definitions"
         #'my/anaconda-eldoc-callback-fallback)))

    ;; also show object docstrings
    (defun nadvice/anaconda-mode-eldoc-function ()
      (anaconda-mode-call "eldoc" #'my/anaconda-eldoc-callback)
      nil)

    (advice-add 'anaconda-mode-eldoc-function :override
                #'nadvice/anaconda-mode-eldoc-function))

  (use-package evil
    :config
    (evil-define-key 'normal python-mode-map "gd" #'anaconda-mode-goto))

  (define-key python-mode-map (kbd "M-.") #'anaconda-mode-goto)
  (add-hook 'python-mode-hook #'anaconda-eldoc-mode)
  (add-hook 'python-mode-hook #'anaconda-mode)
  (add-hook 'python-mode-hook #'eldoc-mode)

  (use-package traad
    :defer-install t
    :commands (traad-open
               traad-close
               traad-running?
               traad-display-task-status
               traad-display-full-task-status
               traad-undo
               traad-redo
               traad-display-history
               traad-undo-info
               traad-redo-info
               traad-rename-current-file
               traad-rename
               traad-normalize-arguments
               traad-remove-argument
               traad-extract-method
               traad-extract-variable
               traad-organize-imports
               traad-expand-star-imports
               traad-froms-to-imports
               traad-relatives-to-absolutes
               traad-handle-long-imports
               traad-imports-super-smackdown
               traad-display-occurrences
               traad-display-implementations
               traad-goto-definition
               traad-findit
               traad-code-assist
               traad-display-calltip
               traad-popup-calltip
               traad-display-doc
               traad-popup-doc))

  (use-package live-py-mode
    :defer-install t
    :commands (live-py-mode))

  (use-package pipenv
    :init
    (add-hook 'python-mode-hook #'pipenv-mode)
    (setq pipenv-projectile-after-switch-function
          #'pipenv-projectile-after-switch-extended))

  (use-package blacken
    :defer-install t
    :commands (blacken-buffer
               blacken-mode)))

(use-package django-html-mode
  :recipe django-mode
  :defer-install t
  :commands (django-html-mode)
  :mode ("\\.djhtml$" . django-html-mode))

(use-package cython-mode
  :defer-install t
  :commands (cython-mode)
  :mode (("\\.pyx\\'" . cython-mode)
         ("\\.pyd\\'" . cython-mode)
         ("\\.pyi\\'" . cython-mode)))

(use-package sage-shell-mode
  :defer-install t
  :commands (run-sage
             run-new-sage
             sage-mode
             sage-shell:run-sage
             sage-shell:run-new-sage
             sage-shell:sage-mode)
  :mode ("\\.sage$" . sage-mode)
  :init
  (progn
    (autoload 'run-sage "sage-shell-mode" nil t)
    (autoload 'run-new-sage "sage-shell-mode" nil t)
    (autoload 'sage-mode "sage-shell-mode" nil t))
  :config
  (setq sage-shell:use-prompt-toolkit t
        sage-shell-view-default-resolution 200)
  (sage-shell:define-alias)

  (use-package evil
    :config
    (evil-set-initial-state 'sage-shell-mode 'insert))

  (add-hook 'sage-shell-mode-hook #'eldoc-mode)
  (add-hook 'sage-mode-hook #'eldoc-mode)

  (add-hook 'sage-shell-mode-hook
            (lambda () (semantic-idle-summary-mode -1)))

  (add-hook 'sage-mode-hook
            (lambda () (semantic-idle-summary-mode -1)))

  (add-hook 'sage-shell-after-prompt-hook #'sage-shell-view)

  (advice-add
   'run-sage :around
   (my/defun-as-value nadvice/run-sage (old-fun &optional arg)
     (interactive "P")
     (if (called-interactively-p 'any)
         (cond
          ((consp arg)
           (call-interactively old-fun))
          (t
           (funcall old-fun "sage"))))
     (funcall old-fun arg))))

(provide 'config-python)
