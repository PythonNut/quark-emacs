;;; indicate-changes.el --- Indicate buffer modifications in fringe

;; Copyright © 2013 Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: emacs, 
;; Created: 2013-02-19
;; Last changed: 2013-02-19 22:52:11
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;; 


;;; Code:

(eval-when-compile
  (require 'indicators))

(define-fringe-bitmap 'indicate-change-added [0 24 24 126 126 24 24 0])
(define-fringe-bitmap 'indicate-change-deleted [0 0 0 126 126 0 0 0])


(defun indicate-change-doit (beg end leng-before &optional no-property-change)
  (unless undo-in-progress
    (if (and (= beg end) (> leng-before 0))
	;; deletion
	(progn
	  (indicate-change-remove-indicator-at-point)
	  (indicate-change-set-removed-mark))
      (indicate-change-remove-indicator-at-point)
      (indicate-change-set-added-mark))))

(defun indicate-change-set-added-mark()
  (ind-create-indicator-at-line
   (line-number-at-pos (point))
   :face 'diff-indicator-added
   :relative nil :bitmap 'indicate-change-added :managed t))

(defun indicate-change-set-removed-mark()
  (ind-create-indicator-at-line
   (line-number-at-pos (point))
   :face 'diff-indicator-removed
   :relative nil :bitmap 'indicate-change-deleted :managed t))

(defun indicate-change-remove-indicator-at-point()
  ""
  (let ((current-line (line-number-at-pos (point))))
    (save-excursion
      (setq ind-managed-absolute-indicators
	    (loop for (m . o) in ind-managed-absolute-indicators
		  if (and
		      (= current-line (line-number-at-pos m))
		      (overlay-get o 'before-string-backup))
		  do (progn
		       (goto-char m)
		       (remove-overlays
			(point-at-bol) (point-at-eol)
			'ind-indicator-absolute t))
		  else
		  collect (cons m o))))))


(defun indicate-change-on ()
  (interactive)
  (add-hook 'after-change-functions 'indicate-change-doit nil t)
  (add-hook 'after-save-hook 'ind-clear-indicators-absolute nil t))

(defun indicate-change-off ()
  (interactive)
  (remove-hook 'after-change-functions 'indicate-change-doit t)
  (remove-hook 'after-save-hook 'ind-clear-indicators-absolute t))

(define-globalized-minor-mode global-indicate-change-mode
  indicate-change-mode indicate-change-mode)


(define-minor-mode indicate-change-mode
  "Toggle indicate-change mode."
  :init-value nil
  :group 'indicate-change-mode
  (when
      (and
       (not (string-match "^[ *]" (buffer-name)))
       (buffer-file-name))

    (if indicate-change-mode
	(indicate-change-on)
      (indicate-change-off))))

(provide 'indicate-changes)

;; indicate-changes.el ends here
