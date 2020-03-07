;; -*- lexical-binding: t -*-
(eval-when-compile (require 'config-macros))
(eval-when-compile (require 'config-setq))

;; =============================================================================
;; Python ======================================================================
;; =============================================================================

(defun my/python-find-virtualenv (&optional dir)
  "Find a virtualenv corresponding to the current buffer.
Return either a string or nil."
  (message "Searching for Python virtual environment at %s" default-directory)
  (let* ((default-directory (or dir default-directory))
         (path
          (cl-block nil
            (when (and (my/local-executable-find "poetry")
                       (locate-dominating-file default-directory
                                               "pyproject.toml"))
              ;; First, we try poetry env, which is safe and a bit
              ;; faster, but only works in poetry versions 1.0 and up
              (cl-destructuring-bind (return-code . output)
                  (my/process-file-to-string "poetry" nil '(t nil) nil
                                             "env"
                                             "info"
                                             "-p")
                (when (= 0 return-code)
                  (cl-return output)))
              ;; Fall back to poetry <1.0 check TODO: This code
              ;; will become stale over time, so get rid of it
              ;; after a while.
              ;; Radon: May create virtualenv, but whatever.
              (cl-destructuring-bind (return-code . output)
                  (my/process-file-to-string "poetry" nil '(t nil) nil
                                             "run"
                                             "which"
                                             "python")
                (when (and (= 0 return-code)
                           (string-match (rx bol
                                             (group (1+ any))
                                             "/bin/python\n")
                                         output))
                  (cl-return (match-string 1 output)))))
            (when (and (my/local-executable-find "pipenv")
                       (locate-dominating-file default-directory
                                               "Pipfile"))
              (cl-destructuring-bind (return-code . output)
                  (my/process-file-to-string "pipenv" nil '(t nil) nil
                                             "--venv")
                (when (= 0 return-code)
                  (cl-return (string-trim output)))))))
         (universal-path (if (tramp-tramp-file-p default-directory)
                             (my/tramp-build-name-from-localname path)
                           path)))
    (when (file-directory-p universal-path)
      universal-path)))

(defvar my/python-virtualenv-cache nil)
(add-to-list 'savehist-additional-variables 'my/python-virtualenv-cache)
(defun my/python-find-virtualenv-cached (&optional dir)
  (let* ((dir (or dir default-directory))
         (pyproject (locate-dominating-file dir "pyproject.toml")))
    (unless my/python-virtualenv-cache
      (setq my/python-virtualenv-cache (make-hash-table :test #'equal)))
    (if (and (my/local-executable-find "poetry")
             dir
             (not (file-exists-p (expand-file-name ".venv" dir))))
        (let ((cached (or (gethash dir my/python-virtualenv-cache)
                          (puthash dir
                                   (my/python-find-virtualenv dir)
                                   my/python-virtualenv-cache))))
          (if (file-exists-p cached)
              cached
            (my/python-find-virtualenv dir)))
      (my/python-find-virtualenv dir))))

(with-eval-after-load 'pythonic
  (eval-when-compile
    (with-demoted-errors "Load error: %s"
      (require 'tramp)))

  (defvar my/pythonic-remote-host-cache nil)
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
          hostname))))
  (advice-add 'pythonic-remote-host :override #'nadvice/pythonic-remote-host))

(use-package pyvenv
  :config
  (defun nadvice/pyvenv-activate (&optional arg)
    (interactive "P")
    (let ((default-venv (my/python-find-virtualenv-cached)))
      (if (or (not default-venv) (consp (car arg)))
          (list (read-directory-name "Activate venv: "))
        (list default-venv))))

  (advice-add 'pyvenv-activate :filter-args #'nadvice/pyvenv-activate))

(use-package python
  :ensure nil
  :config
  (add-hook 'python-mode-hook
            (lambda ()
              ;; conflicts with `eldoc-mode'
              (when (bound-and-true-p semantic-idle-summary-mode)
                (semantic-idle-summary-mode -1))
              (setq mode-name "Py")))

  (define-key python-mode-map (kbd "M-RET") #'srefactor-refactor-at-point)

  ;; Use the Microsoft python ls if we can
  (when (executable-find "dotnet")
    (use-package lsp-python-ms
      :init
      (require 'lsp-python-ms)
      (add-hook 'python-mode-hook
                (my/defun-as-value my/maybe-lsp (&rest _)
                  (unless (file-remote-p buffer-file-name)
                    (lsp-deferred))))

      :config
      ;; when on arch, check if we can use the system ls
      (let ((system-ls "/usr/lib/microsoft-python-language-server/")
            (system-ls-bin (executable-find "mspyls")))
        (if (and (file-directory-p system-ls) system-ls-bin)
            (setq lsp-python-ms-dir system-ls
                  lsp-python-ms-executable system-ls-bin)
          (setq lsp-python-ms-dir
                (locate-user-emacs-file "data/mspyls")
                lsp-python-ms-executable
                (concat lsp-python-ms-dir
                        "Microsoft.Python.LanguageServer"
                        (and (eq system-type 'windows-nt) ".exe")))))

      (advice-add
       'lsp-python-ms--extra-init-params :before
       (my/defun-as-value my/lsp-python-ms-discover-virtualenvs (&rest _)
         ;; Stolen from raxod502/radian
         (when-let ((venv (my/python-find-virtualenv)))
           (setq-local lsp-python-ms-extra-paths
                       (file-expand-wildcards
                        (expand-file-name
                         "lib/python*/site-packages" venv))))))))

  (add-hook 'python-mode-hook #'eldoc-mode)

  (with-eval-after-load 'lsp-mode
    (eval-when-compile
      (with-demoted-errors "Load error: %s"
        (require 'lsp-mode)))
    (lsp-register-client
     (make-lsp-client
      :new-connection
      (lsp-stdio-connection
       (my/defun-as-value my/python-find-project-pyls ()
         (let ((pyls-path (expand-file-name
                           "bin/pyls"
                           (my/python-find-virtualenv-cached))))
           (when (file-executable-p pyls-path)
             pyls-path))))
      :major-modes '(python-mode)
      :server-id 'pyls-poetry
      :library-folders-fn
      (lambda (_workspace)
        lsp-clients-python-library-directories)
      :initialized-fn
      (lambda (workspace)
        (with-lsp-workspace workspace
          (lsp--set-configuration (lsp-configuration-section "pyls"))))))

    (lsp-register-client
     (make-lsp-client
      :new-connection
      (lsp-tramp-connection
       (my/defun-as-value my/python-find-project-pyls ()
         (let ((pyls-path (expand-file-name
                           "bin/pyls"
                           (my/python-find-virtualenv-cached))))
           (when (file-executable-p pyls-path)
             (with-parsed-tramp-file-name pyls-path parsed
               parsed-localname)))))
      :major-modes '(python-mode)
      :remote? t
      :server-id 'pyls-remote
      :library-folders-fn
      (lambda (_workspace)
        lsp-clients-python-library-directories)
      :initialized-fn
      (lambda (workspace)
        (with-lsp-workspace workspace
          (lsp--set-configuration (lsp-configuration-section "pyls")))))))

  (use-package live-py-mode
    :defer-install t
    :commands (live-py-mode))

  (use-package conda)

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

(use-package hy-mode
  :defer-install t
  :commands (hy-mode)
  :mode ("\\.hy\\'" . hy-mode)
  :interpreter ("hy" . hy-mode))

(provide 'config-python)
