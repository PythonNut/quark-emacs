;;; **WARNING: This virtualenv package has been deprecated!**

;;; virtualenv.el --- Virtualenv for Python

;; Copyright (c) 2010-2012 Aaron Culich

;; Author: Aaron Culich <aculich@gmail.com>
;; Maintainer: Aaron Culich <aculich@gmail.com>
;; Version: 20140220.1501
;; X-Original-Version: 1.2.1
;; Created: September 2010
;; Updated: September 2014
;; Keywords: python virtualenv
;; Vcs-git: git://github.com/aculich/virtualenv.el.git
;; Vcs-Browser: http://github.com/aculich/virtualenv.el

;; virtualenv.el is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.
;;
;; virtualenv.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with virtualenv.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; **WARNING: This virtualenv package has been deprecated!**

;; I haven't maintained https://github.com/aculich/virtualenv.el in a
;; long time since I use docker and LXC for a better virtual
;; environment for my development purposes that provides stronger
;; isolation, first-class network interfaces, and support for
;; non-python stacks.

;; If you still want to work with virtualenv there are at least 3
;; newer, actively maintained packages available on MELPA
;; <http://melpa.milkbox.net/> that are superior to my old one that
;; have taken its place:
;;
;; pyvenv:             https://github.com/jorgenschaefer/pyvenv
;; virtualenvwrapper:  https://github.com/porterjamesj/virtualenvwrapper.el
;; python-environment: https://github.com/tkf/emacs-python-environment


;; This is a minor mode for setting the virtual environment for the
;; Python shell using virtualenv and supports both python-mode.el and
;; python.el. This minor mode was inspired by an earlier
;; implementation by Jesse Legg and Jeremiah Dodds, however this code
;; is a complete re-write with a GPLv3 license consistent with
;; GNU Emacs and python-mode.el.

;; There are two ways to use virtualenv.

;; 1) The quickest way to get started is to simply type:
;;      M-x virtualenv-workon
;;    Which will prompt you to enter the name of a directory in
;;    ~/.virtualenvs that contains your chosen environment. You can
;;    hit tab to show the available completions.

;;    You'll know that you're in virtualenv mode now when you see the
;;    name of the virtualenv you selected in brackets. So if I were to
;;    select my turbogears environment that I call tg2.1 then I would
;;    see [tg2.1] appear in the mode line. To make sure you're new
;;    python shell is set up correctly you can try running this little
;;    snippet of python code:

;;      import os, sys
;;      print os.environ
;;      print sys.path

;; 2) The recommended way to use virtualenv minor mode is to use a
;; .dir-locals.el file in the root of your project directory, however that
;; requires Emacs 23.1 or higher. There are two buffer-local variables that you
;; can set for virtualenv as shown in this example:

;; in file /path/to/project/.dir-locals.el:
;; ((nil . ((virtualenv-workon . "tg2.1")
;; 	    (virtualenv-default-directory . "/path/to/project/subdir"))))

;; The .dir-locals.el is new in Emacs23 and is useful for other
;; things, too. You should read the dir-locals docs to understand the
;; format. The variable virtualenv-workon should just be a string the
;; same as you'd give to the interactive function. The variable
;; virtualenv-default-directory is useful when you want to have your
;; python process rooted in a particular directory when it starts, so
;; that no matter where you are in your project's hierarchy, if you
;; launch a python shell. This method is recommended because it is
;; more flexible and will allow multiple virtualenvs running at once
;; in future versions.

;;; Bugs:

;;  All bug reports can be filed by opening a new issue on github at:
;;  http://github.com/aculich/virtualenv/issues

;;; TODO:

;; * support for multiple python processes in different virtualenvs

;; * add "paster shell" features

;; * add support for shell-command

;;; Code:

(defgroup virtualenv nil
  "Emacs support for python virtualenv."
  :group 'python)

(defcustom virtualenv-root
  (or (getenv "WORKON_HOME") "~/.virtualenvs")
  "Default location for user's virtual environments"
  :group 'virtualenv
  :type 'directory)

(defcustom virtualenv-mode-string-format " [%s]"
  "Format for the mode string. It should start with a space."
  :group 'virtualenv
  :type 'string)

(defcustom virtualenv-workon-starts-python t
  "If non-nil the `virtualenv-workon' will also start python."
  :group 'virtualenv
  :type 'boolean)

(defvar virtualenv-mode-name-default " Virtualenv"
  "The default name in the mode line in case `virtualenv-workon' is not set.
In practice you should never see this in the mode line, but it is
better to use this than for it to appear blank.")

(defvar virtualenv-mode-name virtualenv-mode-name-default)

(defvar virtualenv-executables-dir
  (if (eq system-type 'windows-nt) "/Scripts" "/bin")
  "The name of the directory containing executables. It is system
dependent.")

(defvar virtualenv-default-directory nil
  "Buffer-local variable that should be set in your project's
top-level .dir-locals.el file as the place you want to start the python shell.
When using paster set this to where your .ini files live, e.g.: \
((nil . ((virtualenv-default-directory . \"/projects/foo\"))))")
(put 'virtualenv-default-directory 'safe-local-variable 'stringp)

(defvar virtualenv-workon nil
  "Buffer-local variable that should be set in your project's
top-level .dir-locals.el file, e.g.: \
((nil . ((virtualenv-workon . \"tg2.1\"))))")
(put 'virtualenv-workon 'safe-local-variable 'stringp)

(defvar virtualenv-workon-session nil
  "The virtualenv that this emacs session will workon.")
(put 'virtualenv-default-directory 'risky-local-variable 'stringp)

(defvar virtualenv-workon-history nil
  "History list of virtual environments used.")

(defvar virtualenv-saved-path nil
  "Saves `exec-path' and the `PATH' environment variable when
  invoking `virtualenv-workon'.")

(defun virtualenv-formatted-mode-string (&optional name)
  "Format the `virtualenv-mode-name' string.
Optional argument NAME is a string that will appear as [NAME] in
the mode line, however if NAME begins with a space the string
will be used verbatim. If NAME is nil and `virtualenv-workon' is
not set, then use `virtualenv-mode-name-default'."
  (let* ((name (or name
		   virtualenv-workon-session
		   virtualenv-workon
		   virtualenv-mode-name-default)))
    (if (string= " " (substring name 0 1))
	name
      (format virtualenv-mode-string-format name))))

(defun virtualenv-update-mode-name (&optional name)
  "Update the mode line with a string formatted for virtualenv.
Optional argument NAME is a string that will appear as [NAME] in
the mode line, however if NAME begins with a space the string
will be used verbatim. If NAME is nil and `virtualenv-workon' is
not set, then use `virtualenv-mode-name-default'."
  (let ((string (virtualenv-formatted-mode-string name)))
    (make-local-variable 'virtualenv-mode-name)
    (setq virtualenv-mode-name string)))

(defalias 'virtualenv-old-hack-dir-local-variables (symbol-function 'hack-dir-local-variables))

;;;###autoload
(defun virtualenv-workon (&optional env)
  "Activate a virtual environment for python.
Optional argument ENV if non-nil, either use the string given as
the virtual environment or if not a string then query the user."
  (interactive "P")

  ;; reset virtualenv-workon-session if env is non-nil and also not a
  ;; string (e.g. invoked interactively with C-u prefix arg)
  (when (and env (not (stringp env)))
    (setq virtualenv-workon-session nil))

  (defalias 'hack-dir-local-variables 'virtualenv-hack-dir-local-variables)

  ;; if env is a string, then just use it, otherwise check to see if
  ;; we have already queried the user the session, at last querying
  ;; the user if all else fails.
  (let ((env
	 (cond
	  ((stringp env) env)
	  ((stringp virtualenv-workon-session)
	   virtualenv-workon-session)
	  (t
	   (let* ((default (car virtualenv-workon-history))
                  (root (or (unless (file-directory-p virtualenv-root)
                              (let ((dir (read-directory-name
                                          "Virtualenv Directory: "
                                          (expand-file-name "~"))))
                                (funcall
                                 (if (y-or-n-p
                                      (format
                                       "Save %s as virtualenv-root for future sessions?"
                                       dir))
                                     'customize-save-variable
                                   'customize-set-variable)
                                 'virtualenv-root
                                 dir)))
                            virtualenv-root))
		  (prompt (concat
			   "Virtualenv to activate"
			   (when default
			     (format " (default %s)" default))
			   ": "))
		  ;; look for directories in virtualenv-root that
		  ;; contain a bin directory for tab-completion
		  (dirs (remove
			 nil
			 (mapcar
			  (lambda (d)
			    (when (file-exists-p
				   (expand-file-name
				    (concat d virtualenv-executables-dir)
                                    root))
			      d))
			  (directory-files root nil "^[^.]"))))
		  (result (completing-read prompt dirs nil t nil
					   'virtualenv-workon-history
                                           default)))

	     ;; if the user entered nothing, then return the default
	     ;; if there is one
	     (if (not (string= result ""))
		 result
	       default))))))

    (let* ((deprecated
          "**WARNING: This virtualenv package has been deprecated!**
  Please switch to an alternate package:
    pyvenv:             https://github.com/jorgenschaefer/pyvenv
    virtualenvwrapper:  https://github.com/porterjamesj/virtualenvwrapper.el
    python-environment: https://github.com/tkf/emacs-python-environment
**WARNING: This virtualenv package has been deprecated!**

")
           (buffer (get-buffer "*Python*"))
	   (kill (or (when buffer
		       (yes-or-no-p
			"Python process already running. Kill? ")))))

      (if (or (not buffer) kill)
	  (progn
	    (when buffer
	      (kill-buffer buffer))
	    (setq virtualenv-workon-session env)
            (let* ((bin (expand-file-name
                         (concat env virtualenv-executables-dir)
                         virtualenv-root))
                   (oldpath (or (car virtualenv-saved-path)
                                (getenv "PATH")))
                   (oldexec (or (cdr virtualenv-saved-path)
                                exec-path)))
              (setq virtualenv-saved-path (cons oldpath oldexec))
              (add-to-list 'exec-path bin)
	      (setenv "PATH"
		      (if (eq system-type 'windows-nt)
			  (concat bin path-separator
				  (replace-regexp-in-string
				   (regexp-quote "/") "\\" oldpath))
			(concat bin path-separator oldpath))))
	    (when virtualenv-workon-starts-python
	      (cond ((fboundp 'python-shell-switch-to-shell)
                     (python-shell-switch-to-shell))
                    ((fboundp 'py-shell)
		     (py-shell))
		    ((fboundp 'python-shell)
		     (python-shell))
		    ((fboundp 'run-python)
		     (run-python))
		    (t (error "Could not start a python shell!"))))
	    (message (concat deprecated
                       (format "Now using virtualenv: %s" env))))
	(message (concat deprecated "Not changing virtualenv"))))))

;;;###autoload
(defun virtualenv-deactivate ()
  (interactive)

  (defalias 'hack-dir-local-variables (symbol-function 'virtualenv-old-hack-dir-local-variables))

  (when virtualenv-saved-path
    (setenv "PATH" (car virtualenv-saved-path))
    (setq exec-path (cdr virtualenv-saved-path)))

  (setq virtualenv-workon-session nil
        virtualenv-saved-path nil)
  (virtualenv-minor-mode 0))

;;;###autoload
(define-minor-mode virtualenv-minor-mode
  nil					; use default docstring
  nil					; the initial value
  virtualenv-mode-name  		; mode line indicator
  nil					; keymap
  :group 'virtualenv)			; group

(defun virtualenv-minor-mode-on ()
  (interactive)
  (when (or virtualenv-workon-session
	    virtualenv-workon)
    (virtualenv-minor-mode t)))

(add-hook 'virtualenv-minor-mode-hook 'virtualenv-update-mode-name)
(add-hook 'find-file-hook 'virtualenv-minor-mode-on t)

;; This provides support for both python-mode.el and python.el by
;; adding defadvice to py-shell and python-shell.
(dolist (list '((python-shell-switch-to-shell . "python")
                (py-shell . "python-mode")
		(python-shell . "python")
		(run-python . "python")))
  (let* ((func (car list))
	 (file (cdr list))
	 (doc (format "Set the environment with virtualenv before running %s." func)))

    (eval-after-load file
`(progn

(defadvice ,func (around virtualenv activate)
  ,doc
  (let ((workon (or virtualenv-workon-session
		    virtualenv-workon)))
    (if workon
	   (progn
	     (when (stringp virtualenv-default-directory)
	       (cd virtualenv-default-directory))
	     (let* ((activate (expand-file-name
			       "activate"
			       (expand-file-name
                                (concat workon virtualenv-executables-dir)
                                virtualenv-root)))
		    (process-environment
		     (when (file-exists-p activate)
		       (split-string
			(shell-command-to-string
			 (if (eq system-type 'windows-nt)
			     (format "call %s & cd %s && set"
				     activate default-directory)
			 (format "source %s; (cd %s && env)"
				 activate default-directory)))
			"\n")))
		    (exec-path (split-string (getenv "PATH") path-separator)))
	       ad-do-it
	       (hack-local-variables)
	       (virtualenv-minor-mode-on)
	       ))
	 ad-do-it)))


))))


;; This is a hack on top of a hack, but it's the way I think dir-local
;; variables should work. The original implementation only loads
;; dir-locals if the buffer has a filename associated with it, however
;; for a python comint buffer or for dired, a call to
;; (buffer-file-name) returns nil. In that case we should use the
;; value of default-directory to check for the presence of
;; .dir-locals.el. This should actually have no effect on any existing
;; code because this hack-local-variables is only added to the
;; find-file-hook, so we can selectively enable this for other buffers
;; that we create, like python shells or dired.

(defcustom virtualenv-enable-local-variables :all
  "Defaults to :all and allows `virtualenv-hack-dir-local-variables'
to override the value of `enable-local-variables' for convenience when
`virtualenv-workon' is enabled."
  :risky t
  :type '(choice (const :tag "Query Unsafe" t)
		 (const :tag "Safe Only" :safe)
		 (const :tag "Do all" :all)
		 (const :tag "Use value of `enable-local-variables'" nil)
		 (other :tag "Query" other))
  :group 'virtualenv)

(defun virtualenv-hack-dir-local-variables ()
  "Read per-directory local variables for the current buffer.
Store the directory-local variables in `dir-local-variables-alist'
and `file-local-variables-alist', without applying them."
  (let ((path (or (buffer-file-name)
		  default-directory)))
    (when (and (or virtualenv-enable-local-variables
                   enable-local-variables)
	       path
	       (not (file-remote-p path)))
      ;; Find the variables file.
      (let ((variables-file (dir-locals-find-file path))
	    (class nil)
	    (dir-name nil))
	(cond
	 ((stringp variables-file)
	  (setq dir-name (file-name-directory path))
	  (setq class (dir-locals-read-from-file variables-file)))
	 ((consp variables-file)
	  (setq dir-name (nth 0 variables-file))
	  (setq class (nth 1 variables-file))))
	(when class
	  (let ((variables
		 (dir-locals-collect-variables
		  (dir-locals-get-class-variables class) dir-name nil)))
	    (when variables
	      (dolist (elt variables)
		(unless (memq (car elt) '(eval mode))
		  (setq dir-local-variables-alist
			(assq-delete-all (car elt) dir-local-variables-alist)))
		(push elt dir-local-variables-alist))
              ;; override enable-local-variables with
              ;; virtualenv-enable-local-variables if set
	      (let ((enable-local-variables
                     (or virtualenv-enable-local-variables
                         enable-local-variables)))
                (hack-local-variables-filter variables dir-name)))))))))

(defvar virtualenv-dir-local-not-supported
  (cond ((featurep 'xemacs)
         "XEmacs is not officially supported.")
        ((not (and (>= emacs-major-version 23)
                   (>= emacs-minor-version 1)))
         "Emacs 23.1 is required for .dir-locals.el support.")))

(eval-after-load "dired"
  '(progn
     (unless virtualenv-dir-local-not-supported
       (add-hook 'dired-mode-hook 'hack-local-variables))
     (add-hook 'dired-mode-hook 'virtualenv-minor-mode-on t)))

(provide 'virtualenv)

;;; virtualenv.el ends here
