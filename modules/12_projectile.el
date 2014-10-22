;;; ==========
;;; Projectile
;;; ==========
;; (require 'projectile)
(require 'projectile-autoloads)
(projectile-global-mode +1)

(eval-after-load 'projectile
  '(progn
     (projectile-global-mode +1)))

(setq projectile-completion-system 'ido)
(setq projectile-enable-caching t)
(setq projectile-require-project-root t)

(global-set-key (kbd "C-c C-f") 'projectile-find-file)
(global-set-key (kbd "C-c C-d") 'projectile-find-dir)
(global-set-key (kbd "C-c C-p") 'projectile-switch-project)
(global-set-key (kbd "C-c C-b") 'projectile-switch-to-buffer)
(global-set-key (kbd "C-c p f") 'projectile-find-file)
(global-set-key (kbd "C-c p d") 'projectile-find-dir)
(global-set-key (kbd "C-c p p") 'projectile-switch-project)
(global-set-key (kbd "C-c p b") 'projectile-switch-to-buffer)

(autoload '->> "dash")
(setq my-projectile-project-root-files
  '(".projectile"       ; projectile project marker
     ".git"             ; Git VCS root dir
     ".hg"              ; Mercurial VCS root dir
     ".fslckout"        ; Fossil VCS root dir
     ".bzr"             ; Bazaar VCS root dir
     "_darcs"           ; Darcs VCS root dir
     "rebar.config"     ; Rebar project file
     "project.clj"      ; Leiningen project file
     "pom.xml"          ; Maven project file
     "build.sbt"        ; SBT project file
     "build.gradle"     ; Gradle project file
     "Gemfile"          ; Bundler file
     "requirements.txt" ; Pip file
     "Makefile"         ; Make project file
     ".svn"             ; SVN project file
     ))

(defun my-projectile-file-truename (file-name)
  "A thin wrapper around `expand-file-name' that handles nil.
Expand FILE-NAME using `default-directory'."
  (when file-name
    (file-truename file-name)))

(defun my-projectile-project-root ()
  (interactive)
  "Retrieves the root directory of a project if available.
The current directory is assumed to be the project's root otherwise."
  (or (->> my-projectile-project-root-files
        (--map (locate-dominating-file default-directory it))
        (-remove #'null)
        (car)
        (my-projectile-file-truename))
    ()))

