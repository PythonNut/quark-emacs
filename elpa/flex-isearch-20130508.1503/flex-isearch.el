;;; flex-isearch.el --- Flex matching (like ido) in isearch.

;; Copyright (C) 2011 Jonathan Kotta
;; Copyright (C) 2011 Le Wang

;; Author: Jonathan Kotta <jpkotta@gmail.com>
;; Contributors: Tomohiro Matsuyama, Le Wang
;; Keywords: convenience, search
;; Version: 20130508.1503
;; X-Original-Version: 20130508
;; URL: https://bitbucket.org/jpkotta/flex-isearch

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This was inspired by and based on Tomohiro Matsuyama's fuzzy.el.

;; This file defines a minor mode that allows flex matching in
;; isearch-mode.  The flex matching is very similar to the flex
;; matching in ido mode (see `ido-enable-flex-matching').  Given an
;; isearch string, it is transformed to a regexp that matches the
;; original and also similar strings (basically, strings that contain
;; the original characters in the same order, but possibly with other
;; characters in between).  You can redefine
;; `flex-isearch-compile-regexp' to change this behavior.

;; When `flex-isearch-mode' is enabled, the `flex-isearch-auto'
;; variable controls when the flex matching is activated.  See its
;; docstring for details.  Also, `isearch-forward' and
;; `isearch-backward' are advised so that double prefix args (C-u C-u
;; C-s) will use flex search.

;; Le Wang has improved this by providing a better
;; `flex-isearch-compile-regexp' and the advice on
;; `isearch-toggle-regexp'.

;;; Code:



(eval-when-compile
  (require 'cl))

;;; Customization

(defgroup flex-isearch nil
  "Flex matching (like ido) in isearch."
  :prefix "flex-isearch-"
  :group 'isearch
  :link '(url-link :tag "Development and bug reports"
                   "https://bitbucket.org/jpkotta/flex-isearch")
  :link '(url-link :tag "Wiki"
                   "http://www.emacswiki.org/emacs/FlexIsearch"))

;;;###autoload
(defcustom flex-isearch-auto nil
  "Determines when flex searching is automatically activated.
If it is t, then flex matching is used for all isearches.  If it
is 'on-failed, flex matching will only be used after a standard
isearch failed.  If it is nil, flex searching will not be enabled
automatically."
  :type '(choice (const :tag "Never" nil)
                 (const :tag "On failure" on-failed)
                 (const :tag "Always" t))
  :group 'flex-isearch)

;;;###autoload
(defcustom flex-isearch-message-prefix "[FLEX] "
  "Prepended to the isearch prompt when flex searching is activated."
  :type 'string
  :group 'flex-isearch)

;;; Variables

(defvar flex-isearch-activated nil
  "True if flex isearch is activated (i.e. it is overriding
  normal isearch behavior).")

(defvar flex-isearch-failed-count 0
  "Used to decide when to activate flex searching ")

;;; Internal Functions

(defun flex-isearch-activate ()
  (setq flex-isearch-activated t)
  (setq flex-isearch-failed-count 0))

(defun flex-isearch-deactivate ()
  (setq flex-isearch-activated nil)
  (setq flex-isearch-failed-count 0))

(defun flex-isearch-regexp-compile (string)
  "Transform a normal isearch query string to a regular
expression that matches the original string but also similar
strings.

TWO consecutive spaces inserts a '.*'
a non-word character inserts '.*<char>'
"
  (let (last-result-non-word)
    (mapconcat (lambda (str)
                 (concat "\\<"
                         (let ((first-run t))
                           (mapconcat
                            (lambda (str)
                              (prog1
                                  (cond ((zerop (length str))
                                         (if first-run
                                             "[[:graph:]]*?"
                                           str))
                                        ((eq ?w (char-syntax (aref str 0)))
                                         (setq last-result-non-word nil)
                                         (concat (regexp-quote str)
                                                 "[[:graph:]]*?"))
                                        ((memq (aref str 0) '(?  ?\t))
                                         (setq last-result-non-word t)
                                         "[ \t]+")
                                        (t
                                         (setq last-result-non-word t)
                                         (concat
                                          ".*?"
                                          (regexp-quote str))))
                                (setq first-run nil)))
                            (split-string str "")
                            ""))
                         (if last-result-non-word
                             ""
                           "\\>")))
               (split-string string " \\{2\\}")
               ".*")))

(defun flex-search-forward (string &optional bound noerror count)
  "A function suitable to be returned by
  `isearch-search-fun-function' (it is called like
  `search-forward')."
  (let ((regexp (flex-isearch-regexp-compile string)))
    (re-search-forward regexp bound t)))

(defun flex-search-backward (string &optional bound noerror count)
  "A function suitable to be returned by
  `isearch-search-fun-function' (it is called like
  `search-forward')."
  (let ((regexp (flex-isearch-regexp-compile string)))
    (re-search-backward regexp bound t)))

(defun flex-isearch-search-fun ()
  "Set to `isearch-search-fun-function' when `flex-isearch-mode' is
  enabled."
  (cond (isearch-word
         (if isearch-forward 'word-search-forward 'word-search-backward))
        (isearch-regexp
         (if isearch-forward 're-search-forward 're-search-backward))
        ((or flex-isearch-activated
            (eq flex-isearch-auto 'always)
            (and (eq flex-isearch-auto 'on-failed)
               (null isearch-success)
               isearch-wrapped
               (> (setq flex-isearch-failed-count (1+ flex-isearch-failed-count))
                  1)))
         (unless flex-isearch-activated
           ;;(goto-char isearch-opoint)
           (flex-isearch-activate))
         (if isearch-forward 'flex-search-forward 'flex-search-backward))
        (t
         (if isearch-forward 'search-forward 'search-backward))))

(defun flex-isearch-end-hook ()
  "Added to `isearch-mode-end-hook' when `flex-isearch-mode' is
  enabled."
  (flex-isearch-deactivate))

(defadvice isearch-message-prefix (after flex-isearch-message-prefix activate)
  (if flex-isearch-activated
      (setq ad-return-value (concat flex-isearch-message-prefix ad-return-value))
    ad-return-value))

(defadvice isearch-forward (around flex-isearch activate)
  (when (and flex-isearch-mode
           (equal (ad-get-arg 0) '(16)))
    (flex-isearch-activate)
    (ad-set-arg 0 nil))
  ad-do-it)

(defadvice isearch-backward (around flex-isearch activate)
  (when (and flex-isearch-mode
           (equal (ad-get-arg 0) '(16)))
    (flex-isearch-activate)
    (ad-set-arg 0 nil))
  ad-do-it)

;; this is activated in flex-isearch-mode
(defadvice isearch-toggle-regexp (around flex-isearch disable compile)
  "ISearch -> Regexp -> Flex -> Word -> ISearch"
  
  ;; The status stack is left unchanged.
  (cond
   (isearch-regexp
    ;; turn on flex (or word)
    (if flex-isearch-mode
        (progn 
          (flex-isearch-activate)
          (setq isearch-regexp nil
                isearch-word nil))
      (setq isearch-regexp nil
            isearch-word t)))
   (flex-isearch-activated
    ;; turnon word
    (flex-isearch-deactivate)
    (setq isearch-regexp nil
          isearch-word t))
   (isearch-word
    ;; turn on normal
    (setq isearch-regexp nil
          isearch-word nil))
   (t
    ;; turn on regexp
    (setq isearch-regexp t
          isearch-word nil)))
  (setq isearch-adjusted t
        isearch-success t)
  (isearch-update))

;;; External Functions

;;;###autoload
(define-minor-mode flex-isearch-mode
  "Flex matching (similar to ido's flex matching) in incremental searches.

When activated, it transforms a regular isearch into a much looser
regexp search that will match the original string, but also
strings that simply contain the characters of the search string
in order.  For example, a search string of \"thlongstr\" matches
\"the=long_string\".  See `flex-isearch-regexp-compile' for the actual
regexp that the search string is transformed to.

When this minor mode is enabled, it puts advice on
`isearch-forward' and `isearch-backward', making them use the
flex mode when given a double prefix argument (e.g., C-u C-u
C-s).  It also uses `flex-isearch-auto' to possibly enable flex
searching during a normal isearch."

  :init-value nil
  :group 'flex-isearch
  (if flex-isearch-mode
      (progn
        (set (make-local-variable 'isearch-search-fun-function) 'flex-isearch-search-fun)
        (add-hook 'isearch-mode-end-hook 'flex-isearch-end-hook)
        (ad-enable-advice 'isearch-toggle-regexp 'around 'flex-isearch)
        (ad-activate 'isearch-toggle-regexp)
        (flex-isearch-deactivate))
    (kill-local-variable 'isearch-search-fun-function)
    (remove-hook 'isearch-mode-end-hook 'flex-isearch-end-hook)
    (ad-disable-advice 'isearch-toggle-regexp 'around 'flex-isearch)
    (ad-activate 'isearch-toggle-regexp)
    (flex-isearch-deactivate)))

;;;###autoload
(defun turn-on-flex-isearch ()
  (interactive)
  (flex-isearch-mode 1))

;;;###autoload
(defun turn-off-flex-isearch ()
  (interactive)
  (flex-isearch-mode 0))

;;;###autoload
(define-global-minor-mode global-flex-isearch-mode
  flex-isearch-mode
  turn-on-flex-isearch)

;;;###autoload
(defun flex-isearch-forward (&optional regexp-p no-recursive-edit)
  "Like `isearch-forward', but with flex searching."
  (interactive "P\np")
  (when (and flex-isearch-mode
           (null regexp-p))
    (flex-isearch-activate))
  (isearch-mode t (not (null regexp-p)) nil (not no-recursive-edit)))

;;;###autoload
(defun flex-isearch-backward (&optional regexp-p no-recursive-edit)
  "Like `isearch-backward', but with flex searching."
  (interactive "P\np")
  (when (and flex-isearch-mode
           (null regexp-p))
    (flex-isearch-activate))
  (isearch-mode nil (not (null regexp-p)) nil (not no-recursive-edit)))

(provide 'flex-isearch)

;;; flex-isearch.el ends here
