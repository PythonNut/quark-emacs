;;; ack-menu.el --- A menu-based front-end for ack
;;
;; Copyright (C) 2009-2011 Nikolaj Schumacher
;;
;; Author: Steven Thomas
;;         Nikolaj Schumacher
;; Created: 06 Jan 2013
;; Version: 0.1.0
;; Keywords: tools matching convenience
;; URL: https://github.com/chumpage/ack-menu
;; Package-Requires: ((mag-menu "0.1.0"))
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; Ack-menu is a fork of Nikolaj Schumacher's full-ack. Instead of
;; offering a set of emacs variables that control what command line
;; options are passed to ack, it uses mag-menu.el to provide an
;; intuitive menu-based front-end to ack.
;;
;; The main function is ack-menu.
;;
;;; Code:

(require 'compile)
(require 'mag-menu)
(require 'cl)
(require 'ansi-color)
(require 'thingatpt)

(add-to-list 'debug-ignored-errors
             "^Moved \\(back before fir\\|past la\\)st match$")
(add-to-list 'debug-ignored-errors "^File .* not found$")

(defgroup ack-menu nil
  "A front-end for ack."
  :group 'tools
  :group 'matching)

(defcustom ack-executable (or (executable-find "ack")
                              (executable-find "ack-grep"))
  "*The location of the ack executable."
  :group 'ack-menu
  :type 'file)

(defcustom ack-arguments nil
  "*The arguments to use when running ack."
  :group 'ack-menu
  :type '(repeat (string)))

(defcustom ack-mode-type-alist nil
  "*Matches major modes to searched file types.
This overrides values in `ack-mode-default-type-alist'.  The car in each
list element is a major mode, the rest are strings representing values of
the --type argument used by `ack-same'."
  :group 'ack-menu
  :type '(repeat (cons (symbol :tag "Major mode")
                       (repeat (string :tag "ack type")))))

(defcustom ack-mode-extension-alist nil
  "*Matches major modes to searched file extensions.
This overrides values in `ack-mode-default-extension-alist'.  The car in
each list element is a major mode, the rest is a list of file extensions
that that should be searched in addition to the type defined in
`ack-mode-type-alist' by `ack-same'."
  :group 'ack-menu
  :type '(repeat (cons (symbol :tag "Major mode")
                       (repeat :tag "File extensions"
                               (string :tag "extension")))))

(defcustom ack-display-buffer t
  "*Determines whether `ack' should display the result buffer.
Special value 'after means display the buffer only after a successful search."
  :group 'ack-menu
  :type '(choice (const :tag "Don't display" nil)
                 (const :tag "Display immediately" t)
                 (const :tag "Display when done" 'after)))

(defcustom ack-root-directory-functions '(ack-guess-project-root)
  "*A list of functions used to find the ack base directory.
These functions are called until one returns a directory.  If successful,
`ack' is run from that directory instead of `default-directory'.  The
directory is verified by the user depending on `ack-promtp-for-directory'."
  :group 'ack-menu
  :type '(repeat function))

(defcustom ack-project-root-file-patterns
  '(".project\\'" ".xcodeproj\\'" ".sln\\'" "\\`Project.ede\\'"
    "\\`.git\\'" "\\`.bzr\\'" "\\`_darcs\\'" "\\`.hg\\'")
  "A list of project file patterns for `ack-guess-project-root'.
Each element is a regular expression.  If a file matching either element is
found in a directory, that directory is assumed to be the project root by
`ack-guess-project-root'."
  :group 'ack-menu
  :type '(repeat (string :tag "Regular expression")))

(defcustom ack-prompt-for-directory nil
  "*Determines whether `ack' asks the user for the root directory.
If this is 'unless-guessed, the value determined by
`ack-root-directory-functions' is used without confirmation.  If it is
nil, the directory is never confirmed."
  :group 'ack-menu
  :type '(choice (const :tag "Don't prompt" nil)
                 (const :tag "Don't Prompt when guessed " unless-guessed)
                 (const :tag "Prompt" t)))

(defcustom ack-current-project-directory nil
  "*The current project directory, which will be available in the
menu as a switch."
  :group 'ack-menu
  :type 'directory)

(defcustom ack-pushy-match-prompt nil
  "Prompt for match as soon as ack-menu is run."
  :group 'ack-menu
  :type 'boolean)

;;; faces ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defface ack-separator
  '((default (:foreground "gray50")))
  "*Face for the group separator \"--\" in `ack' output."
  :group 'ack-menu)

(defface ack-file
  '((((background dark)) (:foreground "green1"))
    (((background light)) (:foreground "green4")))
  "*Face for file names in `ack' output."
  :group 'ack-menu)

(defface ack-line
  '((((background dark)) (:foreground "LightGoldenrod"))
    (((background dark)) (:foreground "DarkGoldenrod")))
  "*Face for line numbers in `ack' output."
  :group 'ack-menu)

(defface ack-match
  '((default (:foreground "black"))
    (((background dark)) (:background "yellow"))
    (((background light)) (:background "yellow")))
  "*Face for matched text in `ack' output."
  :group 'ack-menu)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst ack-mode-default-type-alist
  ;; Some of these names are guessed.  More should be constantly added.
  '((actionscript-mode "actionscript")
    (LaTeX-mode "tex")
    (TeX-mode "tex")
    (asm-mode "asm")
    (batch-file-mode "batch")
    (c++-mode "cpp")
    (c-mode "cc")
    (cfmx-mode "cfmx")
    (cperl-mode "perl")
    (csharp-mode "csharp")
    (css-mode "css")
    (emacs-lisp-mode "elisp")
    (erlang-mode "erlang")
    (espresso-mode "js")
    (f90-mode "fortran")
    (fortran-mode "fortran")
    (haskell-mode "haskell")
    (hexl-mode "binary")
    (html-mode "html")
    (java-mode "java")
    (javascript-mode "js")
    (jde-mode "java")
    (js2-mode "js")
    (jsp-mode "jsp")
    (latex-mode "tex")
    (lisp-mode "lisp")
    (lua-mode "lua")
    (makefile-mode "make")
    (mason-mode "mason")
    (nxml-mode "xml")
    (objc-mode "objc" "objcpp")
    (ocaml-mode "ocaml")
    (parrot-mode "parrot")
    (perl-mode "perl")
    (php-mode "php")
    (plone-mode "plone")
    (python-mode "python")
    (ruby-mode "ruby")
    (scheme-mode "scheme")
    (shell-script-mode "shell")
    (smalltalk-mode "smalltalk")
    (sql-mode "sql")
    (tcl-mode "tcl")
    (tex-mode "tex")
    (text-mode "text")
    (tt-mode "tt")
    (vb-mode "vb")
    (vim-mode "vim")
    (xml-mode "xml")
    (yaml-mode "yaml"))
  "Default values for `ack-mode-type-alist', which see.")

(defconst ack-mode-default-extension-alist
  '((d-mode "d"))
  "Default values for `ack-mode-extension-alist', which see.")

(defun ack-create-type (extensions)
  (list "--type-set"
        (concat "ack-menu-custom-type=" (mapconcat 'identity extensions ","))
        "--type" "ack-menu-custom-type"))

(defun ack-type-for-major-mode (mode)
  "Return the --type and --type-set arguments for major mode MODE."
  (let ((types (cdr (or (assoc mode ack-mode-type-alist)
                        (assoc mode ack-mode-default-type-alist))))
        (ext (cdr (or (assoc mode ack-mode-extension-alist)
                      (assoc mode ack-mode-default-extension-alist))))
        result)
    (dolist (type types)
      (push type result)
      (push "--type" result))
    (if ext
        (if types
            `("--type-add" ,(concat (car types)
                                    "=" (mapconcat 'identity ext ","))
              . ,result)
          (ack-create-type ext))
      result)))

;;; root ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ack-guess-project-root ()
  "A function to guess the project root directory.
This can be used in `ack-root-directory-functions'."
  (catch 'root
    (let ((dir (expand-file-name (if buffer-file-name
                                     (file-name-directory buffer-file-name)
                                   default-directory)))
          (prev-dir nil)
          (pattern (mapconcat 'identity ack-project-root-file-patterns "\\|")))
      (while (not (equal dir prev-dir))
        (when (directory-files dir nil pattern t)
          (throw 'root dir))
        (setq prev-dir dir
              dir (file-name-directory (directory-file-name dir)))))))

;;; process ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar ack-buffer-name "*ack*")
(defvar ack-process nil)

(defvar ack-buffer--rerun-args nil)

(defun ack-count-matches ()
  "Count the matches printed by `ack' in the current buffer."
  (let ((c 0)
        (beg (point-min)))
    (setq beg (next-single-char-property-change beg 'ack-match))
    (while (< beg (point-max))
      (when (get-text-property beg 'ack-match)
        (incf c))
      (setq beg (next-single-char-property-change beg 'ack-match)))
    c))

(defun ack-sentinel (proc result)
  result ;; Use result just to silence the compiler warning
  (when (eq (process-status proc) 'exit)
    (with-current-buffer (process-buffer proc)
      (insert (ack-parse-sgr-sequences-finish 'ack-apply-faces))
      (let ((c (ack-count-matches)))
        (if (or (> c 0) (/= (buffer-size) 0))
            (when (eq ack-display-buffer 'after)
              (display-buffer (current-buffer)))
          (kill-buffer (current-buffer)))
        (message "Ack finished with %d match%s" c (if (eq c 1) "" "es"))))))

(defvar ack-parse-sgr-context nil
  "A dotted pair of the form (sgr-code . unfinished-string).
Both values are strings. This is used to store unfinished
colorized regions while parsing the ack output.")
(make-variable-buffer-local 'ack-parse-sgr-context)

(defun ack-parse-sgr-fragment (string &optional start)
  "Returns a pair of the form (string . sgr-fragment)"
  (let ((pos (string-match "\033" string (or start 0))))
    (if (and pos (<= (- (length string) pos) 10))
        `(,(substring string 0 pos) . ,(substring string pos))
        `(,string . nil))))

;; (assert (equal (ack-parse-sgr-fragment "abc123456789") '("abc" . "123456789")))
;; (assert (eq (ack-parse-sgr-fragment "abc1234567890") '("abc1234567890" . nil)))

(defun ack-parse-sgr-sequences (string fn)
  "This function filters ansi escape codes (see
http://en.wikipedia.org/wiki/ANSI_escape_code), specifically
searching for Select Graphic Rendition (sgr) sequences. Ack
color-codes certain parts of the output (file names, line
numbers, and matches) using sgr sequences. By finding the sgr
sequences we can easily extract the file names and line numbers
of the matches, and apply Emacs faces to the output to colorize
it however we want. Any ansi escape codes other than sgr
sequences are removed from the string.

This function takes a new STRING of ack process output, and a
callback FN which is called with two parameters for every color
coded string it finds: the string and the sgr color code (of the
form `1;33m', or `30;43m', etc). The color code will have already
been removed from the string. The callback function should return
a string with the appropriate text properties added.

ack-parse-sgr-sequences will return a string with ansi escape
sequences removed, and text properties added to the sgr-colored
portions of the string. The returned string may not represent the
entire input string, as some of the input string may be processed
during subsequent calls to ack-parse-sgr-sequences.

This function uses ack-parse-sgr-context to store temporary
parsing data between calls to ack-parse-sgr-sequences while
processing ack process output. When the ack process is finished,
ack-parse-sgr-sequences-finish must be called to finish
processing the temporary parsing data and reset
ack-parse-sgr-context.

This function is inspired by ansi-color-apply, which
unfortunately isn't generic enough for us to use. This function
does however use two values defined in ansi-color.el:
ansi-color-drop-regexp and ansi-color-regexp."
  (let ((sgr-code (car ack-parse-sgr-context))
        result)
    ;; First prepend the leftover string from the previous call
    (setq string (concat (or (cdr ack-parse-sgr-context) "")
                         string))
    ;; Strip unrecognized escape code sequences
    (while (string-match ansi-color-drop-regexp string)
        (setq string (replace-match "" nil nil string)))
    ;; Process color escape code sequences
    (let (pos)
      (while (setq pos (string-match ansi-color-regexp string))
        (let ((new-sgr-code (match-string 1 string)))
          ;; Remove the escape code sequence
          (setq string (replace-match "" nil nil string))
          (if (find new-sgr-code '("0m" "m") :test 'string=)
              ;; If we're closing a colorized string, call the callback,
              ;; save the result, and chop off the beginning of string
              (when sgr-code
                (push (funcall fn (substring string 0 pos) sgr-code) result)
                (setq string (substring string pos))
                (setq sgr-code nil))
              ;; If we encountered the start of a new sgr code, and we're not
              ;; currently parsing a colorized string, save the state for the
              ;; new coloration. If we're already parsing a colorized string,
              ;; just ignore the extra escape sequence.
              (when (null sgr-code)
                (push (substring string 0 pos) result)
                (setq string (substring string pos))
                (setq sgr-code new-sgr-code))))))
    ;; Set up our context for the next call
    (if sgr-code
        ;; We're currently processing a colored string. The context is the
        ;; sgr-code and the leftover string.
        (setq ack-parse-sgr-context `(,sgr-code . ,string))
        ;; Check for a possible start of an sgr sequence. Save the fragment and
        ;; following text as context for the next call. Anything before the
        ;; fragment isn't colorized and can be added to the return value.
        (destructuring-bind (unencoded-string . fragment) (ack-parse-sgr-fragment string)
          (setq ack-parse-sgr-context `(nil . ,fragment))
          (push unencoded-string result)))
    ;; Reverse the list of result strings as our return value
    (apply 'concat (nreverse result))))

(defun ack-parse-sgr-sequences-finish (fn)
  "This function finishes processing any remaining ack output
remaining from previous calls to ack-parse-sgr-sequences. It
takes a callback function that should work the same as the
callback supplied to ack-parse-sgr-sequences. This function
returns a string representing the last of the ack process
output."
  (let ((sgr-code (car ack-parse-sgr-context))
        (string (cdr ack-parse-sgr-context)))
    (setq ack-parse-sgr-context nil)
    (if (and sgr-code string)
        (funcall fn string sgr-code)
        (or string ""))))

(defun ack-apply-faces (string sgr-code)
  "The function passed to ack-parse-sgr-sequences to add our text
properties. The text properties that may be added:
  - font-lock-face: The face to use for the text. One of
    ack-line, ack-file, or ack-match.
  - ack-line: The line number (as a string).
  - ack-file: The file name.
  - ack-match: Set to t if this string represents an ack match.
  - mouse-face: Will be set to `highlight' for matches.
  - follow-line: Will be set to t for matches."
  (let ((props (cond ((string= sgr-code "1;33m") `(font-lock-face ack-line ack-line ,string))
                     ((string= sgr-code "1;32m") `(font-lock-face ack-file ack-file ,string))
                     ((string= sgr-code "30;43m") `(font-lock-face ack-match
                                                    ack-match t
                                                    mouse-face highlight
                                                    follow-link t)))))
    (add-text-properties 0 (length string) props string))
  string)

(defun ack-filter (proc output)
  (let ((buffer (process-buffer proc))
        (inhibit-read-only t))
    (if (buffer-live-p buffer)
      (with-current-buffer buffer
        (save-excursion
          (goto-char (point-max))
          (insert (ack-parse-sgr-sequences output 'ack-apply-faces))))
      (ack-abort))))

(defun ack-abort ()
  "Abort the running `ack' process."
  (interactive)
  (when (processp ack-process)
    (delete-process ack-process)))

(defun ack-option (name enabled)
  (format "--%s%s" (if enabled "" "no") name))

(defun ack-run-impl (directory &rest arguments)
  "Run ack in DIRECTORY with ARGUMENTS."
  (ack-abort)
  (setq directory
        (if directory
            (file-name-as-directory (expand-file-name directory))
          default-directory))
  (let ((buffer (get-buffer-create ack-buffer-name))
        (inhibit-read-only t)
        (default-directory directory)
        (rerun-args (cons directory arguments)))
    (setq next-error-last-buffer buffer
          ack-buffer--rerun-args rerun-args)
    (with-current-buffer buffer
      (erase-buffer)
      (ack-mode)
      (setq buffer-read-only t
            default-directory directory)
      (set (make-local-variable 'ack-buffer--rerun-args) rerun-args)
      (font-lock-mode)
      (when (eq ack-display-buffer t)
        (display-buffer (current-buffer))))
    (setq ack-process
          (apply 'start-process "ack" buffer ack-executable arguments))
    (set-process-sentinel ack-process 'ack-sentinel)
    (set-process-query-on-exit-flag ack-process nil)
    (set-process-filter ack-process 'ack-filter)))

(defun ack-version-string ()
  "Return the ack version string."
  (with-temp-buffer
    (call-process ack-executable nil t nil "--version")
    (goto-char (point-min))
    (re-search-forward " +")
    (buffer-substring (point) (point-at-eol))))

(defun ack-uses-line-color ()
  (>= (string-to-number (ack-version-string)) 1.94))

(defun ack-check-version ()
  (let ((version (ack-version-string)))
    (when (< (string-to-number version) 1.94)
      (error "Ack-menu only supports ack version 1.94 or later. Yours is %s." version))))

(defun ack-list-files (directory &rest arguments)
  (with-temp-buffer
    (let ((default-directory directory))
      (when (eq 0 (apply 'call-process ack-executable nil t nil "-f" "--print0"
                         arguments))
        (goto-char (point-min))
        (let ((beg (point-min))
              files)
          (while (re-search-forward "\0" nil t)
            (push (buffer-substring beg (match-beginning 0)) files)
            (setq beg (match-end 0)))
          files)))))

;;; commands ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar ack-directory-history nil
  "Directories recently searched with `ack'.")
(defvar ack-literal-history nil
  "Strings recently searched for with `ack'.")
(defvar ack-regexp-history nil
  "Regular expressions recently searched for with `ack'.")

(defun ack--read (regexp)
  (let ((default (ack--default-for-read))
        (type (if regexp "pattern" "literal"))
        (history-var (if regexp 'ack-regexp-history 'ack-literal-history)))
    (read-string (if default
                     (format "ack %s search (default %s): " type default)
                   (format "ack %s search: " type))
                 (ack--initial-contents-for-read)
                 history-var
                 default)))

(defun ack--initial-contents-for-read ()
  (when (ack--use-region-p)
    (buffer-substring-no-properties (region-beginning) (region-end))))

(defun ack--default-for-read ()
  (unless (ack--use-region-p)
    (thing-at-point 'symbol)))

(defun ack--use-region-p ()
  (or (and (fboundp 'use-region-p) (use-region-p))
      (and transient-mark-mode mark-active
           (> (region-end) (region-beginning)))))

(defun ack-read-dir ()
  (let ((dir (run-hook-with-args-until-success 'ack-root-directory-functions)))
    (if ack-prompt-for-directory
        (if (and dir (eq ack-prompt-for-directory 'unless-guessed))
            dir
          (read-directory-name "Directory: " dir dir t))
      (or dir
          (and buffer-file-name (file-name-directory buffer-file-name))
          default-directory))))

(defun ack-xor (a b)
  (if a (not b) b))

(defun ack-type ()
  (or (ack-type-for-major-mode major-mode)
      (when buffer-file-name
        (ack-create-type (list (file-name-extension buffer-file-name))))))

(defun ack-read-file (prompt choices)
  (if ido-mode
      (ido-completing-read prompt choices nil t)
    (require 'iswitchb)
    (with-no-warnings
      (let ((iswitchb-make-buflist-hook
             `(lambda () (setq iswitchb-temp-buflist ',choices))))
        (iswitchb-read-buffer prompt nil t)))))

;;;###autoload
(defun ack-find-same-file (&optional directory)
  "Prompt to find a file found by ack in DIRECTORY."
  (interactive (list (ack-read-dir)))
  (find-file (expand-file-name
              (ack-read-file "Find file: "
                             (apply 'ack-list-files directory (ack-type)))
              directory)))

;;;###autoload
(defun ack-find-file (&optional directory)
  "Prompt to find a file found by ack in DIRECTORY."
  (interactive (list (ack-read-dir)))
  (find-file (expand-file-name (ack-read-file "Find file: "
                                              (ack-list-files directory))
                               directory)))

;;; run again ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ack-again ()
  "Run the last ack search in the same directory."
  (interactive)
  (if ack-buffer--rerun-args
      (let ((ack-buffer-name (ack--again-buffer-name)))
        (apply 'ack-run-impl ack-buffer--rerun-args))
    (call-interactively 'ack)))

(defun ack--again-buffer-name ()
  (if (local-variable-p 'ack-buffer--rerun-args)
      (buffer-name)
    ack-buffer-name))

;;; text utilities ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ack-previous-property-value (property pos)
  "Find the value of PROPERTY at or somewhere before POS."
  (or (get-text-property pos property)
      (when (setq pos (previous-single-property-change pos property))
        (get-text-property (1- pos) property))))

(defun ack-property-beg (pos property)
  "Move to the first char of consecutive sequence with PROPERTY set."
  (when (get-text-property pos property)
    (if (or (eq pos (point-min))
            (not (get-text-property (1- pos) property)))
        pos
      (previous-single-property-change pos property))))

(defun ack-property-end (pos property)
  "Move to the last char of consecutive sequence with PROPERTY set."
  (when (get-text-property pos property)
    (if (or (eq pos (point-max))
            (not (get-text-property (1+ pos) property)))
        pos
      (next-single-property-change pos property))))

;;; next-error ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar ack-error-pos nil)
(make-variable-buffer-local 'ack-error-pos)

(defun ack-next-marker (pos arg marker marker-name)
  (setq arg (* 2 arg))
  (unless (get-text-property pos marker)
    (setq arg (1- arg)))
  (assert (> arg 0))
  (dotimes (i arg)
    i ;; Use i just to silence the compiler warning
    (setq pos (next-single-property-change pos marker))
    (unless pos
      (error (format "Moved past last %s" marker-name))))
  (goto-char pos)
  pos)

(defun ack-previous-marker (pos arg marker marker-name)
  (assert (> arg 0))
  (dotimes (i (* 2 arg))
    i ;; Use i just to silence the compiler warning
    (setq pos (previous-single-property-change pos marker))
    (unless pos
      (error (format "Moved back before first %s" marker-name))))
  (goto-char pos)
  pos)

(defun ack-next-match (pos arg)
  "Move to the next match in the *ack* buffer."
  (interactive "d\np")
  (ack-next-marker pos arg 'ack-match "match"))

(defun ack-previous-match (pos arg)
  "Move to the previous match in the *ack* buffer."
  (interactive "d\np")
  (ack-previous-marker pos arg 'ack-match "match"))

(defun ack-next-file (pos arg)
  "Move to the next file in the *ack* buffer."
  (interactive "d\np")
  ;; Workaround for problem at the begining of the buffer.
  (when (bobp) (incf arg))
  (ack-next-marker pos arg 'ack-file "file"))

(defun ack-previous-file (pos arg)
  "Move to the previous file in the *ack* buffer."
  (interactive "d\np")
  (ack-previous-marker pos arg 'ack-file "file"))

(defun ack-next-error-function (arg reset)
  (when (or reset (null ack-error-pos))
    (setq ack-error-pos (point-min)))
  (ack-find-match (if (<= arg 0)
                      (ack-previous-match ack-error-pos (- arg))
                    (ack-next-match ack-error-pos arg))))

(defun ack-create-marker (pos &optional force)
  (let ((file (ack-previous-property-value 'ack-file pos))
        (line (ack-previous-property-value 'ack-line pos))
        (offset (- pos (let ((line-pos (previous-single-property-change pos 'ack-line)))
                         (if line-pos (1+ line-pos) 0))))
        buffer)
    (if force
        (or (and file
                 line
                 (file-exists-p file)
                 (setq buffer (find-file-noselect file)))
            (error "File <%s> not found" file))
      (and file
           line
           (setq buffer (find-buffer-visiting file))))
    (when buffer
      (with-current-buffer buffer
        (save-excursion
          (ack--move-to-line (string-to-number line))
          (copy-marker (+ (point) offset)))))))

(defun ack--move-to-line (line)
  (save-restriction
    (widen)
    (goto-char (point-min))
    (forward-line (1- line))))

(defun ack-find-match (pos)
  "Jump to the match at POS."
  (interactive (list (let ((posn (event-start last-input-event)))
                       (set-buffer (window-buffer (posn-window posn)))
                       (posn-point posn))))
  (when (setq pos (ack-property-beg pos 'ack-match))
    (let ((marker (get-text-property pos 'ack-marker))
          (msg (copy-marker pos))
          (msg-end (ack-property-end pos 'ack-match))
          (inhibit-read-only t)
          (end (make-marker)))
      (setq ack-error-pos pos)

      (let ((bol (save-excursion (goto-char pos) (point-at-bol))))
        (if overlay-arrow-position
            (move-marker overlay-arrow-position bol)
          (setq overlay-arrow-position (copy-marker bol))))

      (unless (and marker (marker-buffer marker))
        (setq marker (ack-create-marker msg t))
        (add-text-properties msg msg-end (list 'ack-marker marker)))
      (set-marker end (+ marker (- msg-end msg))
                  (marker-buffer marker))
      (compilation-goto-locus msg marker end)
      (set-marker msg nil)
      (set-marker end nil))))

;;; ack-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar ack-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap [mouse-2] 'ack-find-match)
    (define-key keymap "\C-m" 'ack-find-match)
    (define-key keymap "n" 'ack-next-match) ; next-error-no-select
    (define-key keymap "p" 'ack-previous-match)
    (define-key keymap "\M-n" 'ack-next-file)
    (define-key keymap "\M-p" 'ack-previous-file)
    (define-key keymap "g" 'ack-again)
    (define-key keymap "r" 'ack-again)
    keymap))

(define-derived-mode ack-mode nil "ack"
  "Major mode for ack output."
  (set (make-local-variable 'font-lock-extra-managed-props)
       '(mouse-face follow-link ack-line ack-file ack-marker ack-match))
  (make-local-variable 'overlay-arrow-position)
  (set (make-local-variable 'overlay-arrow-string) "")

  (use-local-map ack-mode-map)

  (setq next-error-function 'ack-next-error-function
        ack-error-pos nil))

(defvar ack-menu-group
  `(ack
    (man-page ,(if (null ack-executable)
                   nil
                   (file-name-nondirectory (file-truename ack-executable))))
    (actions
     ("r" "Run" ack-menu-action))
    (switches
     ("-c" "Current project dir" "-c" ack-menu-current-project-switch)
     ("-bd" "Buffer dir" "-bd" ack-menu-buffer-dir-switch)
     ("-bp" "Buffer project root dir" "-bp" ack-menu-buffer-project-dir-switch)
     ("-a" "All files" "--all")
     ("-i" "Ignore case" "--ignore-case")
     ("-n" "No recurse" "--no-recurse")
     ("-fm" "Only print file names matched" "--files-with-matches" ack-menu-only-print-files-switch)
     ("-fs" "Only print file names searched" "-f" ack-menu-only-print-files-switch)
     ("-w" "Match whole word" "--word-regexp")
     ("-q" "Literal search, no regex" "--literal"))
    (arguments
     ("-m" "Match" "--match=" mag-menu-read-generic ack-menu-match-history)
     ("-d" "Directory" "--directory=" mag-menu-read-directory-name)
     ("-B" "Num context lines before" "--before-context=" mag-menu-read-generic)
     ("-A" "Num context lines after" "--after-context=" mag-menu-read-generic)
     ("-C" "Num context lines around" "--context=" mag-menu-read-generic))))

(defun ack-buffer-major-mode (buffer)
  (save-excursion
    (set-buffer buffer)
    major-mode))

(defun ack-buffer-dir (buffer)
  (let ((mode (ack-buffer-major-mode buffer)))
    (if (or (buffer-file-name buffer)
            (string= mode "shell-mode")
            (string= mode "term-mode")
            (string= mode "dired-mode"))
        (save-excursion
          (set-buffer buffer)
          default-directory)
        (file-name-as-directory (getenv "HOME")))))

(defun ack-get-current-word (default)
  (if (word-at-point)
      (substring-no-properties (word-at-point))
      default))

(defvar ack-menu-current-state nil)
(defvar ack-menu-options '(("--ignore-case")))
(defvar ack-menu-match-history nil)

(defun ack-menu-current-project-switch (option-name options)
  (if ack-current-project-directory
      (mag-menu-set-option options "--directory" ack-current-project-directory)
      (error "ack-current-project-directory isn't set")))

(defun ack-menu-buffer-dir-switch (option-name options)
  (let* ((buffer (car ack-menu-current-state))
         (dir (ack-buffer-dir buffer)))
    (if dir
        (mag-menu-set-option options "--directory" dir)
        (error "No directory for buffer '%s'" buffer))))

(defun ack-menu-buffer-project-dir-switch (option-name options)
  (let* ((buffer (car ack-menu-current-state))
         (project-root (save-excursion
                         (set-buffer buffer)
                         (ack-guess-project-root))))
    (if project-root
        (mag-menu-set-option options "--directory" project-root)
        (error "Failed to guess project root for buffer '%s'" buffer))))

(defun ack-menu-only-print-files-switch (option-name options)
  ;; --files-with-matches and -f are mutually exclusive
  (if (null (assoc option-name options))
      (dolist (file-match-option '("--files-with-matches" "-f") options)
        (if (string= file-match-option option-name)
            (setq options (mag-menu-set-option options file-match-option nil))
            (setq options (mag-menu-remove-option options file-match-option))))
      (mag-menu-remove-option options option-name)))

;;;###autoload
(defun ack-menu ()
  "Invoke the ack menu. When finished, ack will be run with the
specified options."
  (interactive)
  (ack-check-version)
  (setq ack-menu-current-state (list (current-buffer) (ack-get-current-word nil)))
  (let ((args (copy-tree ack-menu-options)))
    (when (null (assoc "--directory" args))
      (setq args (mag-menu-set-option args "--directory" (ack-buffer-dir (current-buffer)))))
    (when (null (assoc "--match" args))
      (setq args (mag-menu-set-option args "--match" (ack-get-current-word "search"))))
    (mag-menu ack-menu-group args)
    (when ack-pushy-match-prompt
      (mag-menu-add-argument ack-menu-group "--match=" 'mag-menu-read-generic 'ack-menu-match-history))))

(defun ack-filter-args (args args-to-remove)
  (let* ((args-to-remove (mapcar (lambda (arg) (cons arg (cdr (assoc arg args)))) args-to-remove))
         (kept-args (set-difference args args-to-remove :key 'car :test 'string=))
         (filtered-args (intersection args args-to-remove :key 'car :test 'string=)))
    (list kept-args filtered-args)))

(defun ack-form-args-list (args)
  (mapcar (lambda (arg)
            (if (cdr arg)
                (format "%s=%s" (car arg) (cdr arg))
                (car arg)))
          args))

(defun ack-process-args (args)
  (when (assoc "-f" args)
    (setq args (mag-menu-remove-option args "--match")))
  (destructuring-bind (pass-through-args filtered-args)
      (ack-filter-args args (split-string "-c -bd -bp --directory"))
    (let ((dir (cdr (assoc "--directory" filtered-args)))
          (hard-coded-args '(("--color") ("--nopager"))))
      (when (not (file-exists-p dir))
        (error "No such directory %s" dir))
      (list dir (ack-form-args-list (append hard-coded-args
                                            ack-arguments
                                            pass-through-args))))))

(defun ack-menu-action (options)
  (interactive)
  (setq ack-menu-options (copy-tree options))
  (destructuring-bind (dir args) (ack-process-args ack-menu-options)
    (apply 'ack-run-impl (cons dir args))))

(provide 'ack-menu)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; ack-menu.el ends here
