;;; fsvn.el --- Another frontend of subversion
;; Copyright (C) 2008-2011 by Masahiro Hayashi

;; Author: Masahiro Hayashi <mhayashi1120@gmail.com>
;; URL: http://fsvn.sourceforge.jp/
;; Keywords: subversion, svn
;; Version: 0.9.13

;; fsvn.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; fsvn.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; fsvn supports
;;  * GNU Emacs 23.x or later.
;;  * Subversion from 1.5.x to 1.6.x. (1.7.x is testing)

;; fsvn has TortoiseSVN like user interface by using `svn' command.
;;  Have following advantages of other Emacs svn client.
;;  * tsvn:*, bugtraq:* like property supported. (or will be supported)
;;  * Using `svn help' output for completing read.
;;  * Fast in huge working copy by background process.
;;  * Has repository browser.
;;  * Has visualize blame/annotate/praise minor-mode.
;;  * Has svk support

;;  But following **disadvantage** has.
;;  * Key bindings are not familiar for legacy user.
;;  * Dired like interface but not exactly equals dired functions.
;;  * A little user help.

;; This package is tested on following environment.
;;      NTEmacs (based Emacs 23.3) on Windows.  svn 1.5.x - 1.7.x
;;      Emacs (23.3) on GNU/Linux (Debian).  svn svn 1.5.x - 1.7.x
;;      Emacs current (24.0.50) on GNU/Linux (Debian).  svn 1.5.x - 1.7.x

;;; Code:



(defgroup fsvn nil
  "Emacs subversion interface."
  :group 'tools
  :prefix "fsvn-")

(defconst fsvn-version "0.9.13"
  "Version of fsvn.")



;;; Configuration in dot-emacs

;; 1. Add svn command path to `exec-path' correctly.
;;    Otherwise set `fsvn-svn-command' `fsvn-svnadmin-command' value by full-path before loading/requiring.
;; 
;; (setq fsvn-svn-command "/path/to/svn.exe"
;;       fsvn-svnadmin-command "/path/to/svnadmin.exe")

;; 2. global key bindings (option)
;; (global-set-key "\C-xv!" 'fsvn-command)
;; (global-set-key "\C-xv," 'fsvn-backward-popup-result-buffer)
;; (global-set-key "\C-xv." 'fsvn-forward-popup-result-buffer)
;; (global-set-key "\C-xvV" 'fsvn-vc-commit)
;; (global-set-key "\C-xvG" 'fsvn-blame-minor-mode)
;; (global-set-key "\C-xvI" 'fsvn-import)
;; (global-set-key "\C-xvL" 'fsvn-vc-print-log)
;; (global-set-key "\C-xvO" 'fsvn-checkout)
;; (global-set-key "\C-xvP" 'fsvn-process-list)
;; (global-set-key "\C-xvZ" 'fsvn-debug-toggle)
;; (global-set-key "\C-xv\ec" 'fsvn-global-cleanup-buffer)
;; (global-set-key "\C-xvN" 'fsvn-vc-commit-non-query)
;; (global-set-key "\C-xv\eu" 'fsvn-upgrade)



(require 'dired)
(require 'menu-bar)

(require 'fsvn-deps)
(require 'fsvn-proc)
(require 'fsvn-xml)
(require 'fsvn-data)
(require 'fsvn-env)
(require 'fsvn-debug)
(require 'fsvn-fs)
(require 'fsvn-config)
(require 'fsvn-pub)

(require 'fsvn-browse)
(require 'fsvn-dired)

(require 'fsvn-proclist)

(require 'fsvn-minibuf)
(require 'fsvn-tortoise)
(require 'fsvn-blame)
(require 'fsvn-admin)
(require 'fsvn-magic)



(defvar system-type)
(when (memq system-type '(windows-nt))
  (require 'fsvn-win))



(fsvn-initialize-loading)

(provide 'fsvn)

;;; fsvn.el ends here
