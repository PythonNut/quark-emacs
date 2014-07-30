;;; backups-mode.el --- major mode for autosaving files and listing, viewing, and reverting Emacs generated backups
;; Copyright (C) 2011 by Chad Braun-Duin

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Author: Chad Braun-Duin <chadbraunduin@gmail.com>
;; Maintainer: Chad Braun-Duin
;; Created: 27 Aug 2011
;; Version: 2.0
;; Last-Updated: 12 Oct 2011
;;           By: Chad Braun-Duin
;; URL: https://github.com/chadbraunduin/backups-mode
;; Keywords: backup, backups, autosaving, autosave, diff
;; Compatibility: Emacs 23+

;;; Commentary:
;;  The purpose of these commands and this new major mode is to loosely approximate the autosave-with-versions API
;;  recently instituted by Apple for Mac OS X Lion.

;;; Installation
;;  git clone git@github.com:chadbraunduin/backups-mode.git
;;  cd backups-mode
;;  # copy to your emacs load-path
;;  cp backups-mode.el bm-utilities.el backup-walker.el ~/.emacs.d/
;;  # this assumes ~/.emacs.d/ is in your emacs load-path
;;  # add the following to .emacs
;;  (require 'backups-mode)
;;  (backups-mode-start)

;;; Addtional Configuration
;;   putting this in your .emacs will allow you to change version control settings. These are the default settings found in backups-mode.el.
;;   (setq backup-by-copying t
;;    delete-old-versions t
;;    kept-new-versions 6
;;    kept-old-versions 2
;;    version-control t)
;;   Documentation on these settings can be found here: http://www.gnu.org/software/emacs/elisp/html_node/Numbered-Backups.html

;;; My Personal Configuration
;;   As an example, here's the configuration from my .emacs file
;;   (require 'backups-mode)
;;   (defvar backup-directory "~/.emacs-backups/backups/")
;;   (defvar tramp-backup-directory "~/.emacs-backups/tramp-backups/")
;;   (backups-mode-start)
;;   keep all versions forever
;;   (setq delete-old-versions 1)

;;; Usage
;;  While editing a file-based buffer there are two new commands and some changes to note.
;;  Now, whenever you kill a buffer or kill emacs, all file-based buffers will be saved without prompting.
;;  New Commands while editing a file:
;;    save-version (\C-cv) will backup the previously saved version of the file.
;;    list-backups (\C-cb) will open a backups-mode buffer.
;;  The backups-mode buffer will list all backups Emacs has created for the file and will allow you these options:
;;    view-backup (<enter>) will open a backup file read-only.
;;    revert-backup (R) will backup the current file then replace the current file with the backup you've chosen.
;;    diff 2 files (d + d) You can choose from the current file or any backup files and diff two of them.
;;    purge backups (p [+ p] + x) You can delete backups in a batch fashion by marking backups then executing a deletion of all the marked backups.


;;; Code:

(provide 'backups-mode)
(load "bm-utilities.el")

(eval-when-compile (require 'cl))
(eval-and-compile (require 'diff))

(defun backup-walker-p ()
  (fboundp 'backup-walker-start))

;;; global variables and .emacs configuation default values
(defvar backups-mode-hook nil)
(defvar last-modified-date-command-function 'nix-last-modified-date-command) ;; platform specific way of getting last modified date
(defun nix-last-modified-date-command (file-name) (concat "date -r " file-name " +\"%x %r\""))
(defvar unknown-last-modified-date "date:") ;; platform specific output for unknown last modified date
(defvar backup-files-function 'bm-backup-files)

(defun backups-mode-start ()
  "Turns on emacs backups and keybindings to access the backups"
  (interactive)
  (global-set-key "\C-cv" 'save-version)
  (global-set-key "\C-cb" 'list-backups)
  (global-set-key "\C-ck" 'kill-buffer-prompt)
  (when (backup-walker-p) (global-set-key "\C-cw" 'backup-walker-start))

  ;; autosave configuration section
  (defvar emacs-directory "~/.emacs.d/")

  (defvar backup-directory (concat emacs-directory "backups/"))
  (make-directory backup-directory t)
  (setq backup-directory-alist `((".*" . ,backup-directory)))
  (setq auto-save-list-file-prefix (concat backup-directory ".auto-saves-"))
  (setq auto-save-file-name-transforms `((".*" ,backup-directory t)))

  (defvar tramp-backup-directory (concat emacs-directory "tramp-backups/"))
  (make-directory tramp-backup-directory t)
  (setq tramp-backup-directory-alist `((".*" . ,tramp-backup-directory)))

  ;; this next line turns on emacs version control with backups
  (setq backup-by-copying t
	delete-old-versions t
	kept-new-versions 6
	kept-old-versions 2
	version-control t)

  (defadvice kill-buffer (around kill-buffer)
    "Always save before killing a file buffer"
    (when (and (buffer-modified-p)
	       (buffer-file-name)
	       (file-exists-p (buffer-file-name)))
      (save-buffer))
    ad-do-it)
  (ad-activate 'kill-buffer)

  (defadvice save-buffers-kill-emacs (around save-buffers-kill-emacs)
    "Always save before killing emacs"
    (save-some-buffers t)
    ad-do-it)
  (ad-activate 'save-buffers-kill-emacs)

  (defun kill-buffer-prompt ()
    "Allows one to kill a buffer without saving it.
This is necessary since once you start backups-mode all file based buffers
are saved automatically when they are killed"
    (interactive)
    (if (and (buffer-modified-p) (buffer-file-name) (file-exists-p (buffer-file-name)) (y-or-n-p "Save buffer?"))
	(save-buffer)
      (set-buffer-modified-p nil))
    (kill-buffer)))

;; commands you can run from any file
(defun save-version ()
  "Make the most recently saved version of the file a backup"
  (interactive)
  (set-buffer-modified-p t)
  (save-buffer 16)) ;; archive a copy of the previous version

(defun list-backups ()
  "Lists all saved backups in a buffer whose major mode is backups-mode"
  (interactive)
  (let ((original-file (buffer-file-name)))
    (if original-file
	(list-backups-from-file original-file)
      (princ "No backups for a non-file buffer"))))

(defun* list-backups-from-file (original-file &key data)
  (setq first-config (current-window-configuration))
  (bm-switch-to-window (format "*Backups: %s*" (buffer-name (get-file-buffer original-file)))
		       'backups-major-mode-p)
  (backups-mode) ;; switch to backups-mode
  (erase-buffer)
  (setq second-config (current-window-configuration))

  (unless (assq :original-file data)
    (push `(:original-file . ,original-file) data))
  (push `(:backups . ,(bm-get-sorted-backups original-file backup-files-function)) data)
  (push `(:backups-buffer . ,(current-buffer)) data)
  (push `(:marked-for-purging . ,(list)) data)
  (unless (assq :first-config data)
    (push `(:first-config . ,first-config) data))
  (if (assq :second-config data)
      (setcdr (assq :second-config data) second-config)
    (push `(:second-config . ,second-config) data))

  (make-variable-buffer-local 'backups-mode-data-alist)
  (setq backups-mode-data-alist data)

  (make-variable-buffer-local 'first-diff-index)
  (setq first-diff-index nil)
  
  ;; do pretty print here
  (insert (format "%s\n" original-file))
  (insert
   (apply 'concat (mapcar
		   (lambda (file)
		     (let* ((version (bm-get-version file))
			    (version (if version (number-to-string version) "current"))
			    (last-modified-date (or (bm-get-last-modified-date file) (concat "unknown" "\t"))))
		       (format "  %-6s\t%s"
			       (propertize version 'face 'font-lock-keyword-face)
			       last-modified-date)))
		   (bm-get-backups data))))
  ;; move the cursor to the top
  (goto-char 1)
  (forward-line)
  (set-buffer-modified-p nil))

;;; backups-mode map and methods
(defun backups-mode-map ()
  "Keymap for backups major mode"
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (when (backup-walker-p)
      (define-key map "w" (lambda ()
			    (interactive)
			    (backup-walker-start (bm-get-original-file backups-mode-data-alist)
						 :index (1- (bm-get-index-number (line-number-at-pos)))
						 :data backups-mode-data-alist))))
    (define-key map (kbd "<return>") 'view-backup)
    (define-key map "r" (lambda () (interactive) (princ bm-revert-message)))
    (define-key map "R" 'revert-backup)
    (define-key map "d" 'diff-version)
    (define-key map "p" 'mark-backup-for-purge)
    (define-key map "x" 'purge-backups)
    (define-key map "q" (lambda () (interactive) (bm-kill-all-buffers backups-mode-data-alist))) ;; quit buffer and cleanup all other buffers opened up in the process
    (define-key map [remap next-line] 'forward-line)
    (define-key map [remap previous-line] (lambda () (interactive) (previous-line) (beginning-of-line)))
    map))

(defun backups-mode ()
  "Major mode for viewing and reverting backup files"
  (interactive)
  (kill-all-local-variables)
  (use-local-map (backups-mode-map))
  (buffer-disable-undo)
  (setq header-line-format (concat (when (backup-walker-p) "<w> backup-walker,")
				   " <return> view backup, <d> + <d> diff, <R> revert, <p> + <x> delete, <q> quit"))
  (setq major-mode 'backups-mode)
  (setq mode-name "Backups-list")
  (run-hooks 'backups-mode-hook))

(define-minor-mode view-backup-mode ()
  "Minor mode for viewing a single backup file"
  " Backup-file"
  '(("q" . (lambda () (interactive) (bm-kill-popup-buffer backups-mode-data-alist)))
    ("d" . diff-with-current)
    ("r" . (lambda () (interactive) (princ bm-revert-message)))
    ("R" . (lambda () (interactive) (bm-revert-backup-from-file (bm-get-original-file backups-mode-data-alist)
								(buffer-file-name)
								backups-mode-data-alist))))
  :init-value nil)

(define-minor-mode diff-backup-mode ()
  "Minor mode for viewing a backup diff"
  ""
  '(("q" . (lambda () (interactive) (bm-kill-popup-buffer backups-mode-data-alist)))
    ("1" . (lambda () (interactive) (view-backup-from-diff (cdr (assoc :first-file-name backups-mode-data-alist)))))
    ("2" . (lambda () (interactive) (view-backup-from-diff (cdr (assoc :second-file-name backups-mode-data-alist))))))
  :init-value nil)

;;; backups-mode private methods
(defun bm-get-file-name-from-index (index)
  (bm-get-file-name (nth index (bm-get-backups backups-mode-data-alist))))

(defun bm-get-index-number (line-number)
  (- line-number 2))

(defun bm-get-line-number (index)
  (+ index 2))

;;; backups-mode commands

;; view commands
(defun view-backup ()
  "View a single backup file in a popup window.
If you choose to view the current version of the file,
this will close backups-mode and move the user back to the current file."
  (interactive)
  (let ((index (bm-get-index-number (line-number-at-pos))))
    (cond ((zerop index)
	   (princ bm-current-file-message))
	  ((and (> index 0) (< index (length (bm-get-backups backups-mode-data-alist))))
	   (bm-open-file-read-only (bm-get-file-name-from-index index)))
	  (t (princ bm-no-file-message)))))

(defun view-backup-from-diff (filename)
  "View a single backup file"
  (interactive)
  (let ((original-file (bm-get-original-file backups-mode-data-alist)))
    (if (equal filename original-file)
	(princ bm-current-file-message)
      (bm-open-file-read-only filename))))

(defun bm-open-file-read-only (filename)
  (setq ro-buffer (find-file-noselect filename))
  (setq current-config (current-window-configuration))
  (let* ((orig-data (copy-alist backups-mode-data-alist)))
    (bm-switch-to-window ro-buffer 'backups-minor-mode-p)
    (view-backup-mode t)
    (bm-rename-buffer filename orig-data)
    (setq backups-mode-data-alist orig-data)
    (setq header-line-format (format "<d> diff with current, <R> revert, <q> quit"))))


;; revert commands
(defun revert-backup ()
  "Save the current file as a version then replace it with
the chosen backup."
  (interactive)
  (let ((index (bm-get-index-number (line-number-at-pos))))
    (cond ((zerop index)
	   (princ "Cannot revert current buffer"))
	  ((and (> index 0) (< index (length (bm-get-backups backups-mode-data-alist))))
	   (bm-revert-backup-from-file (bm-get-original-file backups-mode-data-alist)
				       (bm-get-file-name-from-index index)
				       backups-mode-data-alist))
	  (t (princ bm-no-file-message)))))

(defun bm-revert-backup-from-file (orig-file-name backup-file-name data)
  (let* ((temp-backup-file-name (concat backup-file-name "#temp#"))
	 (orig-buffer-name (buffer-name (get-file-buffer orig-file-name))))
    ;; using a temp file is necessary since saving the buffer may delete the backup file before it can be restored
    (bm-kill-all-buffers data)
    (copy-file backup-file-name temp-backup-file-name t) 
    (when orig-buffer-name
      (switch-to-buffer orig-buffer-name)
      (save-buffer) ;; first, save the buffer. This is so the current changes become a saved version
      (save-version) ;; save a version of the current buffer
      (kill-buffer))  ;; kill the original buffer
    (copy-file temp-backup-file-name orig-file-name t) ;; move the temp file to become the current file
    (delete-file temp-backup-file-name)
    (find-file orig-file-name)))

;; diff commands
(defun diff-version ()
  "Diff two versions of the file."
  (interactive)
  (let* ((line-number (line-number-at-pos))
	 (index (bm-get-index-number line-number))
	 (orig-buffer-name (buffer-name (get-file-buffer (bm-get-original-file backups-mode-data-alist)))))
    (if (and (>= index 0) (< index (length (bm-get-backups backups-mode-data-alist))))
	(progn
	  (cond ((eq first-diff-index index)
		 (beginning-of-line)
		 (delete-char 1)
		 (insert " ")
		 (setq first-diff-index nil)
		 (beginning-of-line))
		(first-diff-index
		 (goto-line (bm-get-line-number first-diff-index))
		 (delete-char 1)
		 (insert " ")
		 (goto-line line-number)
		 (progn
		   (when (and
			  (zerop first-diff-index)
			  (get-buffer orig-buffer-name)
			  (buffer-modified-p (get-buffer orig-buffer-name)))
		     (let ((backups-mode-buffer-name (buffer-name)))
		       (switch-to-buffer orig-buffer-name)
		       (save-buffer)
		       (switch-to-buffer backups-mode-buffer-name)))
		   (let ((first-file-name (bm-get-file-name-from-index first-diff-index))
			 (second-file-name (bm-get-file-name-from-index index)))
		     (setq first-diff-index nil)
		     (set-buffer-modified-p nil)
		     (bm-diff-files first-file-name second-file-name))))
		(t
		 (setq first-diff-index index)
		 (beginning-of-line)
		 (insert "d")
		 (delete-char 1)
		 (forward-line)))
	  (set-buffer-modified-p nil))
      (princ bm-no-file-message))) )

(defun diff-with-current ()
  "diff the current backup buffer with the current version of the file"
  (interactive)
  (let ((first-file-name (bm-get-original-file backups-mode-data-alist))
	(second-file-name (buffer-file-name)))
    (bm-diff-files first-file-name second-file-name)))

(defun bm-diff-files (first-file-name second-file-name)
  (setq diff-buffer (diff-no-select first-file-name second-file-name))
  (setq current-config (current-window-configuration))
  (with-current-buffer diff-buffer
    (diff-backup-mode t)) ;; must set minor mode before switching to diff buffer
  (let* ((orig-data (copy-alist backups-mode-data-alist)))
    (bm-switch-to-window diff-buffer 'backups-minor-mode-p)
    (push `(:first-file-name . ,first-file-name) orig-data)
    (push `(:second-file-name . ,second-file-name) orig-data)
    (setq backups-mode-data-alist orig-data)
    (setq header-line-format "<q> quit, <1> view first, <2> view second")))


;; deletion
(defun mark-backup-for-purge ()
  "mark backups for batch deletion"
  (interactive)
  (let ((index (bm-get-index-number (line-number-at-pos)))
	(marked (bm-get-marked-for-purging backups-mode-data-alist)))
    (cond ((zerop index)
	   (princ "Cannot mark the current file for purging"))
	  ((and (>= index 0) (< index (length (bm-get-backups backups-mode-data-alist))))
	   (if (memq index marked)
	       (progn
		 (beginning-of-line)
		 (delete-char 1)
		 (insert " ")
		 (setcdr (assq :marked-for-purging backups-mode-data-alist)
			 (delq index marked))
		 (beginning-of-line))
	     (progn
	       (beginning-of-line)
	       (insert "p")
	       (delete-char 1)
	       (setcdr (assq :marked-for-purging backups-mode-data-alist)
		       (push index marked))
	       (forward-line)))
	   (set-buffer-modified-p nil))
	  (t
	   (princ bm-no-file-message)))))

(defun purge-backups ()
  "Purge (delete) backups"
  (interactive)
  (let ((marked (bm-get-marked-for-purging backups-mode-data-alist)))
    (cond ((zerop (length marked))
	   (princ "No backups marked to purge"))
	  ((y-or-n-p "Purge the marked backups")
	   (mapc
	    (lambda (index)
	      (let* ((file-name (bm-get-file-name-from-index index))
		     (buf (get-file-buffer file-name)))
		(when (buffer-live-p buf)
		  (kill-buffer buf))
		(delete-file file-name)))
	    marked)
	   (list-backups-from-file (bm-get-original-file backups-mode-data-alist)
				   :data backups-mode-data-alist)))))

;; also require lewang's backup-walker if it exists
;; use the most up-to-date version first
;; fallback to a bundled version
(or (require 'backup-walker nil 'noerror)
	(let ((load-path
		   (cons (expand-file-name "fallback"
								   (file-name-directory load-file-name))
				 load-path)))
	  (require 'backup-walker)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; backups-mode.el ends here
