;;; visible-mark.el --- Make marks visible.
;; Version: 20140801.822

;; Copyright (C) 2014 by Ian Kelling

;; Maintainer: Ian Kelling <ian@iankelling.org>
;; Mailing list: https://lists.iankelling.org/listinfo/visible-mark
;; Author: Ian Kelling <ian@iankelling.org>
;; Author: Yann Hodique
;; Author: MATSUYAMA Tomohiro <t.matsuyama.pub@gmail.com>
;; Author: John Foerch <jjfoerch@earthlink.net>
;; Keywords: marking color faces
;; URL: https://gitlab.com/iankelling/visible-mark
;; Created: 2008-02-21

;;; License:

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

;; Emacs minor mode to highlight mark(s).
;;
;; Allows setting the number of marks to display, and the faces to display them.
;;
;; Example installation:
;;
;; 1. Put this file in Emacs's load-path
;;
;; 2. add custom faces to init file
;; (require 'visible-mark)
;; (global-visible-mark-mode 1) ;; or add (visible-mark-mode) to specific hooks
;;
;; 3. Add customizations. The defaults are very minimal. They could also be set
;; via customize.
;; 
;; (defface visible-mark-active ;; put this before (require 'visible-mark)
;;   '((((type tty) (class mono)))
;;     (t (:background "magenta"))) "")
;; (setq visible-mark-max 2)
;; (setq visible-mark-faces `(visible-mark-face1 my-visible-mark-face2))
;; 
;;
;; Additional useful functions like unpoping the mark are at
;; http://www.emacswiki.org/emacs/MarkCommands
;; and http://www.emacswiki.org/emacs/VisibleMark

;; Known bugs
;;
;; Observed in circe, when the buffer has a right margin, and there
;; is a mark at the beginning of a line, any text in the margin on that line
;; gets styled with the mark's face. May also happen for left margins, but
;; haven't tested yet.
;;
;; Patches / pull requests / feedback welcome.

;;; Code:

(eval-when-compile
  (require 'cl))

(defgroup visible-mark nil
  "Show the position of your mark."
  :group 'convenience
  :prefix "visible-mark-")

(defface visible-mark-active
  '((((type tty) (class color))
     (:background "gray" :foreground "black"))
    (((type tty) (class mono))
     (:inverse-video t))
    (((class color) (background dark))
     (:background "gray" :foreground "black"))
    (((class color) (background light))
     (:background "grey80"))
    (t (:background "gray")))
  "Face for the active mark. To redefine this in your init file,
do it before loading/requiring visible-mark."
  :group 'visible-mark)

(defcustom visible-mark-inhibit-trailing-overlay t
  "If non-nil, inhibit the extension of an overlay at the end of a line
to the window margin."
  :group 'visible-mark
  :type 'boolean)

(defcustom global-visible-mark-mode-exclude-alist nil
  "A list of buffer names to be excluded."
  :group 'visible-mark
  :type '(repeat regexp))


(defcustom visible-mark-max 1
  "The number of marks in the backward direction to be visible."
  :group 'visible-mark
  :type 'integer)

(defcustom visible-mark-forward-max 0
  "The number of marks in the forward direction to be visible."
  :group 'visible-mark
  :type 'integer)

(defcustom visible-mark-faces nil
  "A list of mark faces for marks in the backward direction.
If visible-mark-max is greater than the amount of visible-mark-faces,
the last defined face will be reused."
  :group 'visible-mark
  :type '(repeat face))

(defcustom visible-mark-forward-faces nil
  "A list of mark faces for marks in the forward direction."
  :group 'visible-mark
  :type '(repeat face))


;;; example faces

(defface visible-mark-face1
  '((((type tty) (class mono)))
    (t (:background "light salmon")))
  "Example face which can be customized and added to subsequent face lists."
  :group 'visible-mark)
  
(defface visible-mark-face2
  '((((type tty) (class mono)))
    (t (:background "light goldenrod")))
  "Example face which can be customized and added to subsequent face lists."
  :group 'visible-mark)

(defface visible-mark-forward-face1
  '((((type tty) (class mono)))
    (t (:background "pale green")))
  "Example face which can be customized and added to subsequent face lists."
  :group 'visible-mark)

(defface visible-mark-forward-face2
  nil
  "Placeholder face for customization and addition to subsequent face lists."
  :group 'visible-mark)




(defvar visible-mark-overlays nil
  "The overlays used for mark faces. Used internally by visible-mark-mode.")
(make-variable-buffer-local 'visible-mark-overlays)



(defun visible-mark-initialize-overlays ()
  (mapc
   (lambda (x)
     (when (eq 'visible-mark (overlay-get x 'category))
       (delete-overlay x)))
   (overlays-in (point-min) (point-max)))  
  (let (overlays)
    (dotimes (i (+ visible-mark-max visible-mark-forward-max))
      (let ((overlay (make-overlay (point-min) (point-min))))
        (overlay-put overlay 'category 'visible-mark)
        (push overlay overlays)))
    (setq visible-mark-overlays (nreverse overlays))))

(defun visible-mark-find-overlay-at (pos)
  (let ((overlays (overlays-at pos))
        found)
    (while (and overlays (not found))
      (let ((overlay (car overlays)))
        (if (eq 'visible-mark (overlay-get overlay 'category))
            (setq found overlay)))
      (setq overlays (cdr overlays)))
    found))

(defun visible-mark-move-overlays ()
  "Update overlays in `visible-mark-overlays'. This is run in the `post-command-hook'"
  (mapc (lambda (x) (delete-overlay x))
        visible-mark-overlays)
  (let ((marks (cons (mark-marker) mark-ring))
        (overlays visible-mark-overlays)
        (faces visible-mark-faces)
        (faces-forward visible-mark-forward-faces))
    (if mark-active (setq faces (cons 'visible-mark-active (cdr faces))))
    (dotimes (i visible-mark-max)
      (visible-mark-move-overlay (pop overlays) (pop marks) (car faces))
      (if (cdr faces) (pop faces)))
    (dotimes (i visible-mark-forward-max)
      (visible-mark-move-overlay (pop overlays) (car (last marks (1+ i))) (car faces-forward))
      (if (cdr faces-forward) (pop faces-forward)))))

(defun visible-mark-move-overlay (overlay mark face)
  "Set OVERLAY to position of MARK and display of FACE."
  (let ((pos (and mark (marker-position mark))))
    (when (and pos (not (equal (point) pos)))
      (cond
       ((and
         visible-mark-inhibit-trailing-overlay
         (save-excursion (goto-char pos) (eolp)))
        (overlay-put overlay 'face nil)
        (if (visible-mark-find-overlay-at pos)
            (progn (overlay-put overlay 'before-string nil))
          (overlay-put overlay 'before-string
                       (propertize
                        " "
                        'face face))
          (move-overlay overlay pos (1+ pos))))
       (t
        (overlay-put overlay 'before-string nil)
        (overlay-put overlay 'face face)
        (move-overlay overlay pos (1+ pos)))))))

(require 'easy-mmode)
(defun visible-mark-mode-maybe ()
  (when (cond
         ((minibufferp (current-buffer)) nil)
         ((cl-flet ((fun (arg)
                         (if (null arg) nil
                           (or (string-match (car arg) (buffer-name))
                               (fun (cdr arg))))))
            (fun global-visible-mark-mode-exclude-alist)) nil)
         (t t))
    (visible-mark-mode t)))

;;;###autoload
(define-minor-mode visible-mark-mode
  "A mode to make the mark visible."
  nil nil nil
  :group 'visible-mark
  (if visible-mark-mode
      (progn
        (visible-mark-initialize-overlays)
        (add-hook 'post-command-hook 'visible-mark-move-overlays nil t))
    (mapc 'delete-overlay visible-mark-overlays)
    (setq visible-mark-overlays nil)
    (remove-hook 'post-command-hook 'visible-mark-move-overlays t)))

;;;###autoload
(define-global-minor-mode
  global-visible-mark-mode visible-mark-mode visible-mark-mode-maybe
  :group 'visible-mark)

(provide 'visible-mark)
;;; visible-mark.el ends here
