;;; nose.el --- Easy Python test running in Emacs

;; Copyright (C) 2009 Jason Pellerin, Augie Fackler

;; Licensed under the same terms as Emacs.

;; Version: 20140513.1747
;; X-Original-Version: 0.1.1
;; Keywords: nose python testing
;; Created: 04 Apr 2009

;; This file is NOT part of GNU Emacs.

;; Licensed under the same terms as Emacs.

;;; Commentary:
;; This gives a bunch of functions that handle running nosetests on a
;; particular buffer or part of a buffer.

;;; Installation

;; In your emacs config:
;;
;; (require 'nose)
;; ; next line only for people with non-eco non-global test runners
;; ; (add-to-list 'nose-project-names "my/crazy/runner")

;; Note that if your global nose isn't called "nosetests", then you'll want to
;; redefine nose-global-name to be the command that should be used.

;; By default, the root of a project is found by looking for any of the files
;; 'setup.py', '.hg' and '.git'. You can add files to check for to the file
;; list:
;;
;; ; (add-to-list 'nose-project-root-files "something")

;; or you can change the project root test to detect in some other way
;; whether a directory is the project root:
;;
;; ; (setq nose-project-root-test (lambda (dirname) (equal dirname "foo")))

;; If you want dots as output, rather than the verbose output:
;; (defvar nose-use-verbose nil) ; default is t

;; nose.el adds a minor mode called 'nose' that's currently only used to
;; manage keybindings and provide a hook for changing the behaviour of
;; the nose output buffer.

;; This is the recommended way to activate nose keybindings when viewing
;; Python files:
 
;; (add-hook 'python-mode-hook (lambda () (nose-mode t)))

;; nose-mode is also activated when nose displays the buffer that shows
;; the output of nosetests.

;; Code like that given below can be used to change nose.el's keybindings.
;; The bindings shown are nose.el's default bindings. If you wish to use
;; these bindings, then you don't need to include this code in .emacs

;; (define-key nose-mode-map "\C-ca" 'nosetests-all)
;; (define-key nose-mode-map "\C-cm" 'nosetests-module)
;; (define-key nose-mode-map "\C-c." 'nosetests-one)
;; (define-key nose-mode-map "\C-cc" 'nosetests-again)
;; (define-key nose-mode-map "\C-cpa" 'nosetests-pdb-all)
;; (define-key nose-mode-map "\C-cpm" 'nosetests-pdb-module)
;; (define-key nose-mode-map "\C-cp." 'nosetests-pdb-one)

(require 'cl) ;; for "reduce"

(defvar nose-project-names '("eco/bin/test"))

(defvar nose-project-root-files '("setup.py" ".hg" ".git")
  "A list of file names. A directory with any of the files
present is considered to be a 'project root'.")

(defvar nose-project-root-test 'nose-project-root
  "The function to use to discover the root of the current
project. The function should return a directory path.")

(defvar nose-global-name "nosetests"
  "The command to be run when executing nosetests.")

(defvar nose-use-verbose t
  "If t, then the 'verbose' option is passed to nosetests.")

(defvar nose-finish-functions nil
  "Functions to call when nosetests complete. See compilation-finish-functions.")

(defvar nose--last-run-params nil
  "Stores the last parameters passed to run-nose")

(defvar-local nose-local-project-root nil
  "This variable will define the project root when a 'current
file name' is inapplicable to the current buffer. An example is
during display of nosetest results (compilation buffer.) It
should be set local to such buffers at the time when they're
created." )

(define-minor-mode nose-mode
  "Minor mode enabling nosetests key commands."
  :keymap
  '(("\C-ca" . nosetests-all)
    ("\C-cm" . nosetests-module)
    ("\C-c." . nosetests-one)
    ("\C-cc" . nosetests-again)
    ("\C-cpa" . nosetests-pdb-all)
    ("\C-cpm" . nosetests-pdb-module)
    ("\C-cp." . nosetests-pdb-one)) )

(defun nose--finish-function-hook (buffer message)
  (if (string= (buffer-name buffer) "*nosetests*")
	  (dolist (func nose-finish-functions)
		(funcall func buffer message) )))

(add-to-list 'compilation-finish-functions 'nose--finish-function-hook)

(defun run-nose (&optional tests debug failed)
  "run nosetests"
  (setq nose--last-run-params (list tests debug failed))

  (let* ((nose (nose-find-test-runner))
         (where (or nose-local-project-root (nose-find-project-root)))
         (args (concat (if debug "--pdb" "")
                       " "
                       (if failed "--failed" "")))
         (tnames (if tests tests "")))
    (if (not where)
        (error
         (format (concat "abort: nosemacs couldn't find a project root, "
                         "looked for any of %S") nose-project-root-files)))

	;; Execute nosetests and display the result in a compilation buffer.
	;;
	;; Store the active project root in a buffer-local variable, so that nose
	;; can invoked from it by the user after execution is complete. This is
	;; necessary because the compilation buffer doesn't have a filename from
	;; which it could be discovered.
    (funcall (if debug
                 'pdb
               '(lambda (command)
                  (let ((compilation-error-regexp-alist
                         '(("  File \"\\(.*\\)\", line \\([0-9]+\\), in test_" 1 2))))
                    (save-current-buffer
					  (set-buffer (compilation-start command
													 nil
													 (lambda (mode) (concat "*nosetests*"))))
					  (setq-local nose-local-project-root where)))))
             (format
              (concat "%s "
                      (if nose-use-verbose "-v " "")
                      "%s -w %s -c %ssetup.cfg %s")
              nose args where where tnames)))
  )

(defun nosetests-all (&optional debug failed)
  "run all tests"
  (interactive)
  (run-nose nil debug failed))

(defun nosetests-failed (&optional debug)
  "run nosetests with the --failed option"
  (interactive)
  (nosetests-all debug t))

(defun nosetests-pdb-all ()
  "run all tests using the python debugger"
  (interactive)
  (nosetests-all t))

(defun nosetests-module (&optional debug)
  "run nosetests (via eggs/bin/test) on current buffer"
  (interactive)
  (run-nose buffer-file-name debug))

(defun nosetests-pdb-module ()
  "run tests in the current buffer using the Python debugger"
  (interactive)
  (nosetests-module t))

(defun nosetests-one (&optional debug)
  "run nosetests (via eggs/bin/test) on testable thing
 at point in current buffer"
  (interactive)
  (run-nose (format "%s:%s" buffer-file-name (nose-py-testable)) debug))

(defun nosetests-pdb-one ()
  "run nosetests (via eggs/bin/test) on testable thing
 at point in current buffer using the Python debugger"
  (interactive)
  (nosetests-one t))

(defun nosetests-again ()
  "runs the most recently executed 'nosetests' command again"
  (interactive)
  (apply 'run-nose nose--last-run-params))

(defun nose-find-test-runner ()
  (message
   (let ((result
		  (reduce '(lambda (x y) (or x y))
				  (mapcar 'nose-find-test-runner-names nose-project-names))))
	 (if result
		 result
	   nose-global-name))))

(defun nose-find-test-runner-names (runner)
  "find eggs/bin/test in a parent dir of current buffer's file"
  (nose-find-test-runner-in-dir-named
   (file-name-directory (nose--context-path)) runner))

(defun nose-find-test-runner-in-dir-named (dn runner)
  (let ((fn (expand-file-name runner dn)))
    (cond ((file-regular-p fn) fn)
      ((equal dn "/") nil)
      (t (nose-find-test-runner-in-dir-named
          (file-name-directory (directory-file-name dn))
          runner)))))

(defun nose-py-testable ()
  (let* ((inner-obj (inner-testable))
         (outer (outer-testable))
         ;; elisp can't return multiple values
         (outer-def (car outer))
         (outer-obj (cdr outer)))
    (cond ((equal outer-def "def") outer-obj)
          ((equal inner-obj outer-obj) outer-obj)
          (t (format "%s.%s" outer-obj inner-obj)))))

(defun inner-testable ()
  (save-excursion
    (re-search-backward
     "^\\(?: \\{0,4\\}\\|\t\\)\\(class\\|def\\)[ \t]+\\([a-zA-Z0-9_]+\\)" nil t)
    (buffer-substring-no-properties (match-beginning 2) (match-end 2))))

(defun outer-testable ()
  (save-excursion
    (re-search-backward
     "^\\(class\\|def\\)[ \t]+\\([a-zA-Z0-9_]+\\)" nil t)
    (let ((result
            (buffer-substring-no-properties (match-beginning 2) (match-end 2))))

      (cons
       (buffer-substring-no-properties (match-beginning 1) (match-end 1))
       result))))

(defun nose--context-path ()
  "Returns the best known file path, given available context,
 from which nose.el could use to figure out the location of
important resources such as project roots."
  (or buffer-file-name nose-local-project-root))

(defun nose-find-project-root (&optional dirname)
  (let ((dn
         (if dirname
             dirname
           (file-name-directory (nose--context-path)))))
    (cond ((funcall nose-project-root-test dn) (expand-file-name dn))
          ((equal (expand-file-name dn) "/") nil)
        (t (nose-find-project-root
             (file-name-directory (directory-file-name dn)))))))

(defun nose-project-root (dirname)
  (reduce '(lambda (x y) (or x y))
          (mapcar (lambda (d) (member d (directory-files dirname)))
                  nose-project-root-files)))

(provide 'nose)

;;; nose.el ends here
