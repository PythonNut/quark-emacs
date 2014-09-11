;;; phi-search-core.el --- another incremental search interface

;; Copyright (C) 2013 zk_phi

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA

;; Author: zk_phi
;; URL: http://hins11.yu-yake.com/
;; Version: 1.2.0

;;; Commentary:

;; This library is required by phi-search, phi-replace, etc.

;;; Change Log:

;; 1.0.0 divided from phi-search.el 1.2.1
;; 1.1.0 handle "isearch-open-invisible" properties
;; 1.2.0 implement "guess" option for "phi-search-case-sensitive"

;;; Code:

(defconst phi-search-core-version "1.0.0")

;; + suppress byte-compiler

(declare-function sublimity--pre-command "sublimity")
(declare-function sublimity--post-command "sublimity")

;; + customs

(defgroup phi-search nil
  "another incremental search interface"
  :group 'emacs)

(defcustom phi-search-limit 1000
  "maximum number of accepted matches"
  :group 'phi-search)

(defcustom phi-search-case-sensitive nil
  "when non-nil, phi-search become case sensitive"
  :group 'phi-search)

(defcustom phi-search-default-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-s") 'phi-search-again-or-next)
    (define-key map (kbd "C-r") 'phi-search-again-or-previous)
    (define-key map [remap phi-search] 'phi-search-again-or-next)
    (define-key map [remap phi-search-backward] 'phi-search-again-or-previous)
    (define-key map [remap keyboard-quit] 'phi-search-abort)
    (define-key map [remap scroll-up] 'phi-search-scroll-up)
    (define-key map [remap pager-page-down] 'phi-search-scroll-up)
    (define-key map [remap scroll-down] 'phi-search-scroll-down)
    (define-key map [remap pager-page-up] 'phi-search-scroll-down)
    (define-key map [remap recenter] 'phi-search-recenter)
    (define-key map [remap kill-region] 'phi-search-yank-word)
    (define-key map [remap phi-rectangle-kill-region] 'phi-search-yank-word)
    (define-key map (kbd "RET") 'phi-search-complete)
    (define-key map (kbd "C-c C-c") 'phi-search-unlimit)
    map)
  "keymap for the phi-search prompt buffers"
  :group 'phi-search)

;; + faces

(defface phi-search-match-face
  '((((background light)) (:background "#b5dee9"))
    (t (:background "#194854")))
  "Face used to highlight matching items in phi-search.")

(defface phi-search-selection-face
  '((((background light)) (:background "e0d9de"))
    (t (:background "#594854")))
  "Face used to highlight selected items in phi-search.")

;; + utilities

(defun phi-search--search-backward (query limit &optional filter inclusive)
  "a handy version of search-backward-regexp"
  (ignore-errors
    (let* ((case-fold-search (or (not phi-search-case-sensitive)
                                 (and (eq phi-search-case-sensitive 'guess)
                                      (string= query (downcase query)))))
           (pos1 (point))
           (pos2 (search-backward-regexp query limit t)))
      (if (or (and (not inclusive) pos2 (= pos1 pos2))
              (and filter (not (save-match-data (funcall filter)))))
          (progn
            (backward-char 1)
            (phi-search--search-backward query limit filter t))
        pos2))))

(defun phi-search--search-forward (query limit &optional filter inclusive)
  "a handy version of search-forward-regexp"
  (ignore-errors
    (let* ((case-fold-search (or (not phi-search-case-sensitive)
                                 (and (eq phi-search-case-sensitive 'guess)
                                      (string= query (downcase query)))))
           (pos1 (point))
           (pos2 (search-forward-regexp query limit t)))
      (if (or (and (not inclusive) pos2 (= pos1 pos2))
              (and filter (not (save-match-data (funcall filter)))))
          (progn
            (forward-char 1)
            (phi-search--search-forward query limit filter t))
        pos2))))

(defun phi-search--open-invisible-temporary (hidep)
  "when nil, show invisible text at point. otherwise hide it."
  (mapc (lambda (ov)
          (let ((ioit (overlay-get ov 'isearch-open-invisible-temporary)))
            (cond (ioit
                   (funcall ioit ov hidep))
                  ((overlay-get ov 'isearch-open-invisible)
                   (if hidep
                       (overlay-put ov 'invisible (overlay-get ov 'phi-invisible))
                     (overlay-put ov 'phi-invisible (overlay-get ov 'invisible))
                     (overlay-put ov 'invisible nil))))))
        (overlays-at (point))))

(defun phi-search--open-invisible-permanently ()
  "make point visible permanently"
  (mapc (lambda (ov)
          (let ((ioi (overlay-get ov 'isearch-open-invisible)))
            (when ioi (funcall ioi ov))))
        (overlays-at (point))))

(defmacro phi-search--with-sublimity (&rest body)
  "if sublimity is installed, use it"
  `(if (and (boundp 'sublimity-mode) sublimity-mode)
       (progn
         (sublimity--pre-command)
         ,@body
         (sublimity--post-command))
     ,@body))

;; + private functions/variables for TARGET buffer
;; ++ variables

(defvar phi-search--last-executed nil
  "stores the last query")
(make-variable-buffer-local 'phi-search--last-executed)

(defvar phi-search--filter-function nil
  "when non-nil, candidates must pass this filter")
(make-variable-buffer-local 'phi-search--filter-function)

(defvar phi-search--original-position nil
  "stores position where this search started from.")
(make-variable-buffer-local 'phi-search--original-position)

(defvar phi-search--overlays nil
  "overlays currently active in this target buffer. is ordered.")
(make-variable-buffer-local 'phi-search--overlays)

(defvar phi-search--selection nil
  "stores which item is currently selected.
this value must be nil, if nothing is matched.")
(make-variable-buffer-local 'phi-search--selection)

(defvar phi-search--after-update-function nil
  "function called IN THE TARGET BUFFER as soon as overlays are updated")
(make-variable-buffer-local 'phi-search--after-update-function)

;; ++ functions

(defun phi-search--delete-overlays (&optional keep-point)
  "delete all overlays in THIS target buffer, and go to the original position"
  (mapc 'delete-overlay phi-search--overlays)
  (setq phi-search--overlays nil
        phi-search--selection nil)
  (unless keep-point
    (phi-search--open-invisible-temporary t)
    (goto-char phi-search--original-position)))

(defun phi-search--make-overlays-for (query &optional unlimited)
  "make overlays for all matching items in THIS target buffer."
  (save-excursion
    ;; POINT -> BOF
    (goto-char phi-search--original-position)
    (phi-search--make-overlays-for-1 query nil unlimited)
    ;; EOF -> POINT
    (goto-char (point-max))
    (phi-search--make-overlays-for-1 query phi-search--original-position unlimited))
  (let ((num (length phi-search--overlays)))
    ;; check errors
    (cond ((zerop num)
           (message "no matches")
           (setq phi-search--selection nil))
          ((and (not unlimited)
                (>= num phi-search-limit))
           (message "more than %d matches" phi-search-limit)
           (phi-search--delete-overlays)))))

(defun phi-search--make-overlays-for-1 (query limit &optional unlimited)
  (while (and (phi-search--search-backward query limit
                                           phi-search--filter-function)
              (let ((ov (make-overlay (match-beginning 0) (match-end 0))))
                (overlay-put ov 'face 'phi-search-match-face)
                (add-to-list 'phi-search--overlays ov)
                (or unlimited
                    (< (length phi-search--overlays) phi-search-limit))))))

(defun phi-search--select (n)
  "select Nth matching item and go there. return point, or nil for failuare."
  (when (and (>= n 0)
             (< n (length phi-search--overlays)))
    ;; unselect old item
    (when phi-search--selection
      (phi-search--open-invisible-temporary t)
      (overlay-put (nth phi-search--selection phi-search--overlays)
                   'face 'phi-search-match-face))
    ;; select new item if there
    (let ((ov (nth n phi-search--overlays)))
      (setq phi-search--selection n)
      (overlay-put ov 'face 'phi-search-selection-face)
      (goto-char (overlay-end ov))
      (phi-search--open-invisible-temporary nil)
      (point))))

;; + private functions/variables for PROMPT buffer
;; ++ variables

(defvar phi-search--target nil
  "the target (window . buffer) which this prompt buffer is for")
(make-variable-buffer-local 'phi-search--target)

(defvar phi-search--before-complete-function nil
  "function called before phi-search ends")
(make-variable-buffer-local 'phi-search--before-complete-function)

;; ++ functions

(defmacro phi-search--with-target-buffer (&rest body)
  "eval body with the target buffer selected.
\"target\" and \"query\" are brought from the prompt buffer"
  `(progn
     ;; assert that the window and the buffer live
     (cond ((null phi-search--target)
            (error "phi-search: unexpected error (phi-search--target is nil)"))
           ((not (window-live-p (car phi-search--target)))
            (error "phi-search: target window is deleted"))
           ((not (buffer-live-p (cdr phi-search--target)))
            (error "phi-search: target buffer is killed")))
     ;; visit the window, with variables from the prompt buffer
     (let ((target phi-search--target)
           (query (buffer-string)))
       (with-selected-window (car target)
         ;; if buffer is switched, switch back to the target
         (unless (eq (current-buffer) (cdr target))
           (switch-to-buffer (cdr target))
           (message "phi-search: buffer is switched"))
         ;; eval body
         ,@body))))

(defun phi-search-next ()
  "select next item."
  (phi-search--with-target-buffer
   (when (null phi-search--selection)
     (error "no matches"))
   (phi-search--with-sublimity
    (unless (phi-search--select (1+ phi-search--selection))
      (phi-search--select 0)
      (ding)
      (message "no more matches")))))

(defun phi-search-previous ()
  "select previous item."
  (phi-search--with-target-buffer
   (when (null phi-search--selection)
     (error "no matches"))
   (phi-search--with-sublimity
    (unless (phi-search--select (1- phi-search--selection))
      (phi-search--select (1- (length phi-search--overlays)))
      (ding)
      (message "no more matches")))))

(defun phi-search--update (&rest _)
  "update overlays for the target buffer"
  (phi-search--with-target-buffer
   (phi-search--with-sublimity
    (phi-search--delete-overlays)
    (phi-search--make-overlays-for query)
    (phi-search--select 0)
    (when phi-search--after-update-function
      (funcall phi-search--after-update-function)))))

;; + select commands

(defun phi-search-again-or-next ()
  "search again with the last query, or search next item"
  (interactive)
  (let ((str (phi-search--with-target-buffer
              phi-search--last-executed)))
    (if (not (string= (buffer-string) ""))
        (phi-search-next)
      (when str (insert str)))))

(defun phi-search-again-or-previous ()
  "search again with the last query, or search previous item"
  (interactive)
  (let ((str (phi-search--with-target-buffer
              phi-search--last-executed)))
    (if (not (string= (buffer-string) ""))
        (phi-search-previous)
      (when str (insert str)))))

;; + replace scroll commands

(defun phi-search-recenter ()
  "recenter target buffer"
  (interactive)
  (phi-search--with-target-buffer
   (when phi-search--selection
     (phi-search--with-sublimity
      (phi-search--select phi-search--selection)
      (recenter)))))

(defun phi-search-scroll-down ()
  (interactive)
  (phi-search--with-target-buffer
   (phi-search--with-sublimity
    (call-interactively 'scroll-down))))

(defun phi-search-scroll-up ()
  (interactive)
  (phi-search--with-target-buffer
   (phi-search--with-sublimity
    (call-interactively 'scroll-up))))

;; + other commands

(defun phi-search-unlimit ()
  "search for all occurrences, regardless of phi-search-limit"
  (interactive)
  (phi-search--with-target-buffer
   (phi-search--with-sublimity
    (phi-search--delete-overlays)
    (phi-search--make-overlays-for query t)
    (phi-search--select 0)
    (when phi-search--after-update-function
      (funcall phi-search--after-update-function)))))

(defun phi-search-yank-word ()
  "If there's a region in query buffer, kill-region as usual.
Otherwise yank a word from target buffer and expand query."
  (interactive)
  (if (or (not (use-region-p))
          (= (region-beginning) (region-end)))
      (insert
       (phi-search--with-target-buffer
        (buffer-substring-no-properties
         (point)
         (save-excursion (forward-word) (point)))))
    (kill-region (region-beginning) (region-end))))

;; + start/end phi-search

(defun phi-search--initialize (modeline-fmt keybinds filter-fn update-fn complete-fn)
  (setq phi-search--original-position     (point)
        phi-search--filter-function       filter-fn
        phi-search--after-update-function update-fn
        phi-search--selection             nil
        phi-search--overlays              nil)
  (let ((wnd (selected-window))
        (buf (current-buffer)))
    (select-window (split-window-vertically -4))
    (switch-to-buffer (generate-new-buffer "*phi-search*"))
    (add-hook 'after-change-functions 'phi-search--update nil t)
    (use-local-map
     (let ((map (copy-keymap phi-search-default-map)))
       (dolist (bind (reverse keybinds))
         (eval `(define-key map ,(car bind) ,(cdr bind))))
       map))
    (setq mode-line-format                     modeline-fmt
          phi-search--target                   (cons wnd buf)
          phi-search--before-complete-function complete-fn)))

(defun phi-search-complete (&rest args)
  "finish phi-search. (for developers: ARGS are passed to complete-function)"
  (interactive)
  (phi-search--open-invisible-permanently)
  (when phi-search--before-complete-function
    (apply phi-search--before-complete-function args))
  (phi-search--with-target-buffer
   (phi-search--delete-overlays t))
  (let ((wnd (car phi-search--target))
        (str (buffer-string)))
    (kill-buffer (current-buffer))
    (delete-window (selected-window))
    (select-window wnd)
    (setq phi-search--original-position     nil
          phi-search--filter-function       nil
          phi-search--after-update-function nil
          phi-search--selection             nil
          phi-search--overlays              nil
          phi-search--last-executed         str)))

(defun phi-search-abort ()
  "abort phi-search"
  (interactive)
  (phi-search--with-target-buffer
   (phi-search--with-sublimity
    (phi-search--delete-overlays)))
  (phi-search-complete))

;; + provide

(provide 'phi-search-core)

;;; phi-search-core.el ends here
