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
    (when (file-directory-p universal-path)
      (substring-no-properties universal-path))))

(defvar my/python-virtualenv-cache nil)
(with-eval-after-load 'savehist
  (add-to-list 'savehist-additional-variables 'my/python-virtualenv-cache))
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

  ;; Use the Microsoft python ls if we can
  (use-package lsp-python-ms
    :init
    (require 'lsp-python-ms)
    (add-hook 'python-mode-hook
              (my/defun-as-value my/python-mode-maybe-lsp (&rest _)
                (unless (file-remote-p buffer-file-name)
                  (lsp-deferred))))

    (el-patch-feature lsp-python-ms)

    :config
    (eval-when-compile
      (with-demoted-errors "Load error: %s"
        (require 'lsp)
        (require 'el-patch)))
    (setq lsp-pylsp-plugins-pydocstyle-enabled nil)
    (lsp-register-client
     (make-lsp-client
      :new-connection (lsp-tramp-connection
                       (my/defun-as-value my/python-find-mspyls ()
                         (my/local-executable-find "mspyls")))
      :major-modes (append '(python-mode) lsp-python-ms-extra-major-modes)
      :remote? t
      :server-id 'mspyls-remote
      :priority -3
      :initialization-options 'lsp-python-ms--extra-init-params
      :notification-handlers
      (lsp-ht ("python/languageServerStarted"
               'lsp-python-ms--language-server-started-callback)
              ("telemetry/event" 'ignore)
              ("python/reportProgress"
               'lsp-python-ms--report-progress-callback)
              ("python/beginProgress" 'lsp-python-ms--begin-progress-callback)
              ("python/endProgress" 'lsp-python-ms--end-progress-callback))
      :initialized-fn
      (lambda (workspace)
        (with-lsp-workspace workspace
          (lsp--set-configuration (lsp-configuration-section "python"))))))

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

    (el-patch-defun lsp-python-ms-locate-python (&optional dir)
      "Look for virtual environments local to the workspace."
      (let* ((el-patch-add
               (virtualenv (my/python-find-virtualenv-cached dir))
               (virtualenv-python (and virtualenv (f-expand "bin/python" virtualenv))))
             (pyenv-python (lsp-python-ms--dominating-pyenv-python dir))
             (venv-python (lsp-python-ms--dominating-venv-python dir))
             (conda-python (lsp-python-ms--dominating-conda-python dir))
             (sys-python (if (>= emacs-major-version 27)
                             (executable-find lsp-python-ms-python-executable-cmd lsp-python-ms-prefer-remote-env)
                           (executable-find lsp-python-ms-python-executable-cmd))))
        ;; pythons by preference: local pyenv version, local conda version

        (if lsp-python-ms-guess-env
            (cond ((lsp-python-ms--valid-python lsp-python-ms-python-executable))
                  (el-patch-add ((lsp-python-ms--valid-python virtualenv-python)))
                  ((lsp-python-ms--valid-python venv-python))
                  ((lsp-python-ms--valid-python pyenv-python))
                  ((lsp-python-ms--valid-python conda-python))
                  ((lsp-python-ms--valid-python sys-python)))
          (cond ((lsp-python-ms--valid-python sys-python))))))

    (el-patch-defun lsp-python-ms--get-python-ver-and-syspath (&optional workspace-root)
      "Return list with pyver-string and list of python search paths.

The WORKSPACE-ROOT will be prepended to the list of python search
paths and then the entire list will be json-encoded."
      (let* ((python (and t (lsp-python-ms-locate-python)))
             (workspace-root (and python (or workspace-root ".")))
             (default-directory (and workspace-root workspace-root))
             (init (and default-directory
                        "from __future__ import print_function; import sys; sys.path = list(filter(lambda p: p != '', sys.path)); import json;"))
             (ver (and init "v=(\"%s.%s\" % (sys.version_info[0], sys.version_info[1]));"))
             (sp (and ver (concat "sys.path.insert(0, '" workspace-root "'); p=sys.path;")))
             (ex (and sp "e=sys.executable;"))
             (val (and ex "print(json.dumps({\"version\":v,\"paths\":p,\"executable\":e}))")))
        (when val
          (with-temp-buffer
            (el-patch-wrap 3
              (if (tramp-tramp-file-p python)
                  (with-parsed-tramp-file-name python parsed
                    (let ((default-directory (file-name-directory python)))
                      (process-file parsed-localname nil t nil "-c"
                                    (concat init ver sp ex val))))
                (call-process python nil t nil "-c"
                              (concat init ver sp ex val))))
            (let* ((json-array-type 'vector)
                   (json-key-type 'string)
                   (json-object-type 'hash-table)
                   (json-string (buffer-string))
                   (json-hash (json-read-from-string json-string)))
              (list
               (gethash "version" json-hash)
               (gethash "paths" json-hash)
               (gethash "executable" json-hash)))))))

    (el-patch-defun lsp-python-ms--extra-init-params (&optional workspace)
      "Return form describing parameters for language server.

Old lsp will pass in a WORKSPACE, new lsp has a global
lsp-workspace-root function that finds the current buffer's
workspace root.  If nothing works, default to the current file's
directory"
      (let ((workspace-root
             (if workspace (lsp--workspace-root workspace)
               (lsp-python-ms--workspace-root))))
        (when lsp-python-ms-parse-dot-env-enabled
          (lsp-python-ms--parse-dot-env workspace-root))
        (cl-destructuring-bind (pyver pysyspath pyintpath)
            (lsp-python-ms--get-python-ver-and-syspath workspace-root)
          `(:interpreter
            (:properties
             (:InterpreterPath
              ,(el-patch-wrap 1 (my/tramp-localname pyintpath))
              :UseDefaultDatabase t
              :Version ,pyver))
            ;; preferredFormat "markdown" or "plaintext" experiment
            ;; to find what works best -- over here mostly plaintext
            :displayOptions
            (:preferredFormat
             "markdown"
             :trimDocumentationLines :json-false
             :maxDocumentationLineLength 0
             :trimDocumentationText :json-false
             :maxDocumentationTextLength 0)
            :searchPaths
            ,(vconcat lsp-python-ms-extra-paths
                      (el-patch-wrap 3
                        (cl-map 'vector #'my/tramp-localname pysyspath)))
            :analysisUpdates t
            :asyncStartup t
            :logLevel ,lsp-python-ms-log-level
            :typeStubSearchPaths
            ,(vector (expand-file-name
                      (f-join lsp-python-ms-dir "Typeshed"))))))))

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
