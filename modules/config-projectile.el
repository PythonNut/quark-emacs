(projectile-global-mode +1)

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

;; autoload some basic projectile functions or polyfill
(autoload '->> "dash")

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
