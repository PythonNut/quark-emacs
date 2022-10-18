;; -*- lexical-binding: t -*-
(eval-when-compile
  (require 'config-macros)
  (require 'config-core)
  (require 'config-package))


;; =============================================================================
;; Python ======================================================================
;; =============================================================================

(defun my/python-find-virtualenv (&optional dir)
  "Find a virtualenv corresponding to the current buffer.
Return either a string or nil."
  (require 's)
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
                  (cl-return (s-chop-suffix "\n" output)))))
            (when (and (my/local-executable-find "pipenv")
                       (locate-dominating-file default-directory
                                               "Pipfile"))
              (cl-destructuring-bind (return-code . output)
                  (my/process-file-to-string "pipenv" nil '(t nil) nil
                                             "--venv")
                (when (= 0 return-code)
                  (cl-return (s-chop-suffix "\n" output)))))))
         (universal-path (if (tramp-tramp-file-p default-directory)
                             (my/tramp-build-name-from-localname path)
                           path)))
    (when (and universal-path (file-directory-p universal-path))
      (substring-no-properties universal-path))))

(defvar my/python-virtualenv-cache nil)
(with-eval-after-load 'savehist
  (add-to-list 'savehist-additional-variables 'my/python-virtualenv-cache))
(defun my/python-find-virtualenv-cached (&optional dir)
  (let* ((dir (or dir default-directory))
         (pyproject (locate-dominating-file dir "pyproject.toml")))
    (when (and dir
               (my/local-executable-find "poetry")
               (or (file-remote-p dir)
                   (not (getenv "CONDA_PREFIX")))
               (not (file-exists-p (expand-file-name ".venv" dir))))
      (unless my/python-virtualenv-cache
        (setq my/python-virtualenv-cache (make-hash-table :test #'equal)))
      (let ((cached (gethash dir my/python-virtualenv-cache)))
        (if (and cached (file-exists-p cached))
            cached
          (puthash dir
                   (my/python-find-virtualenv dir)
                   my/python-virtualenv-cache))))))

(with-eval-after-load 'pythonic
  (eval-when-compile
    (with-demoted-errors "Load error: %s"
      (require 'tramp)))

  (defvar my/pythonic-remote-host-cache nil)
  (define-advice pythonic-remote-host
      (:override () smart-ssh-hosts)
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

(use-package pyvenv
  :config
  (define-advice pyvenv-activate
      (:filter-args (&optional arg) auto-find-venv)
    (interactive "P")
    (let ((default-venv (my/python-find-virtualenv-cached)))
      (if (or (not default-venv) (consp (car arg)))
          (list (read-directory-name "Activate venv: "))
        (list default-venv)))))

(use-package python
  :ensure nil
  :init
  (el-patch-feature python)

  :config
  (add-hook 'python-mode-hook
            (my/defun-as-value my/python-mode-setup ()
              (setq mode-name "Py")
              (setq-local tab-width 4)))

  (add-hook 'python-mode-hook #'eldoc-mode)

  (with-eval-after-load 'lsp-mode
    (eval-when-compile
      (with-demoted-errors "Load error: %s"
        (require 'lsp-mode)))
    (lsp-register-client
     (make-lsp-client
      :new-connection
      (lsp-stdio-connection
       (my/defun-as-value my/python-find-poetry-pylsp ()
         (let ((pyls-path (expand-file-name
                           "bin/pylsp"
                           (my/python-find-virtualenv-cached))))
           (when (file-executable-p pyls-path)
             pyls-path))))
      :major-modes '(python-mode)
      :server-id 'pylsp-poetry
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
       (my/defun-as-value my/python-find-remote-pylsp ()
         (let ((pyls-path (expand-file-name
                           "bin/pylsp"
                           (my/python-find-virtualenv-cached))))
           (when (file-executable-p pyls-path)
             (my/tramp-localname pyls-path)))))
      :major-modes '(python-mode)
      :remote? t
      :priority -2
      :server-id 'pylsp-remote
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

  (add-hook 'sage-shell-after-prompt-hook #'sage-shell-view)

  (define-advice run-sage
      (:around (old-fun &optional arg) default-sage)
    (interactive "P")
    (if (called-interactively-p 'any)
        (cond
         ((consp arg)
          (call-interactively old-fun))
         (t
          (funcall old-fun "sage"))))
    (funcall old-fun arg)))

(use-package hy-mode
  :defer-install t
  :commands (hy-mode)
  :mode ("\\.hy\\'" . hy-mode)
  :interpreter ("hy" . hy-mode))

(provide 'config-python)
