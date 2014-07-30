;;; indicators.el --- Display the buffer relative location of line in the fringe.

;; Copyright (C) 2013 Matus Goljer

;; Author: Matus Goljer <matus.goljer@gmail.com>
;; Maintainer: Matus Goljer <matus.goljer@gmail.com>
;; Created: 16 Feb 2013
;; Version: 0.0.4
;; Keywords: fringe frames
;; URL: https://github.com/Fuco1/indicators.el

;; This file is not part of GNU Emacs.

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

;; See github readme at https://github.com/Fuco1/indicators.el

;; Known limitations:
;;
;; 1. you can't have more than one color on one "physical line".
;;    This is becuse fringes operate on "per-line" basis and it is
;;    only possible to set face for bitmap for the whole line.
;; 2. if you are at the end of file the indicators are displaied
;;    incorrectly. This is because it's impossible to set fringe
;;    bitmaps for lines past the end of file.
;;
;; Both of these are currently imposible to fix.

;;; Code:

(require 'fringe)

(defvar ind-managed-relative-indicators nil
  "Managed relative indicators.  Position of these on the fringe
is automatically updated when window properties change.  These
indicators display relative buffer position.

These are managed automatically by `indicators-mode'.  However,
you can manage your own lists and pass them to `ind-update'
function to be updated.")
(make-variable-buffer-local 'ind-managed-relative-indicators)

(defvar ind-managed-absolute-indicators nil
  "Managed absolute indicators.  These indicators are shown on
their absolute buffer position.")
(make-variable-buffer-local 'ind-managed-absolute-indicators)

(defvar ind-managed-list-relative nil
  "List of list variable names that should be automatically updated.
This variable can be used by client code to register their own
lists to be automatically updated by this package.  The lists
should contain relative indicators.

Usage:
\(add-to-list 'ind-managed-list-relative 'my-list-variable)")
(make-variable-buffer-local 'ind-managed-list-relative)

(defvar ind-managed-list-absolute nil
  "List of lists that should be automatically updated.
This variable can be used by client code to register their own
lists to be automatically updated by this package.  The lists
should contain absolute indicators.

Usage:
\(add-to-list 'ind-managed-list-absolute 'my-list-variable)")
(make-variable-buffer-local 'ind-managed-list-absolute)

(define-fringe-bitmap 'ind-dash [255 0])

(defun ind--pos-at-line (line)
  "Return the starting point of LINE."
  (save-excursion
    (goto-line line)
    (point)))

(defun ind--line-at-pos (pos)
  "Return the line at position POS."
  (count-lines (point-min) pos))

(defun ind--line-start-at-pos (pos)
  (save-excursion
    (goto-char pos)
    (beginning-of-line)
    (point)))

(defun ind--get-indicator-pos (pos-or-fun)
  "Return the beginning position for line on which POS-OR-FUN is.
POS-OR-FUN can be an integer, marker or a function.

If POS-OR-FUN is a nullary function this function is used to get
a buffer position P, then position of the beginning of line on
which P is is returned."
  (cond
   ((integer-or-marker-p pos-or-fun)
    (ind--line-start-at-pos pos-or-fun))
   ;; absolute line position
   ((and (listp pos-or-fun)
         (integer-or-marker-p (car pos-or-fun)))
    (ind--pos-at-line (car pos-or-fun)))
   (t
    (ind--line-start-at-pos (funcall pos-or-fun)))))

(defmacro ind--number-of-lines ()
  "Return number of lines in buffer."
  (1- (line-number-at-pos (point-max))))

(defun ind-update-event-handler (&optional beg end _len d e f g h)
  "Function that is called by the hooks to redraw the fringe bitmaps."
  (cond
   ;; called from after-change-functions.  Only update if some
   ;; newlines were added/removed.  This still does not catch all the
   ;; redundant updates :( `after-change-functions' with all its
   ;; property chanegs is completely useless!
   ((integerp _len)
    (when (or (= beg end)
              (= end (point-max))
              (string-match-p "\n" (buffer-substring-no-properties beg end)))
      (ind--run-updates)))
   (t (ind--run-updates))))

(defun ind--run-updates ()
  "Run all the update funcitons."
  (ignore-errors
    (ind-update)
    (ind-update-absolute)
    (mapc (lambda (list) (ind-update (symbol-value list))) ind-managed-list-relative)
    (mapc 'ind-update-absolute ind-managed-list-absolute)))

(defvar ind-indicator-height 1
  "Height of an indicator in pixels.
The value of 1 works best, but values up to `frame-char-height'
are possible.")

(defun ind--compute-indicator-positions (managed-list)
  "Compute the current positions of indicators on MANAGED-LIST."
  (sort (mapcar (lambda (pair) (cons (ind--get-indicator-pos (car pair))
                                     (cdr pair))) managed-list)
        (lambda (a b) (< (car a) (car b)))))

(defun ind-update-absolute (&optional mlist)
  "Update managed absolute indicators.
If optional argument MLIST is non-nil updated indicators on that list."
  (interactive)
  (let ((managed-list (or mlist ind-managed-absolute-indicators)))
    (when managed-list
      (let (ind-list)
        (setq ind-list (ind--compute-indicator-positions managed-list))

        (mapc (lambda (pair)
                (let ((pos (car pair))
                      (ov (cdr pair)))
                  (move-overlay ov pos pos)))
              ind-list)))))

(defvar ind--temp-bitmaps nil
  "List of temporary bitmaps.
These are automatically destroyed on each call of `ind-update'.")

(defun ind--update-split-by-fringe (mlist)
  "Split the indicators on MLIST by fringe (left or right)."
  (let (left right)
    (mapc (lambda (pair)
            (if (eq (plist-get (cdr pair) :fringe) 'right-fringe)
                (push pair right)
              (push pair left)))
          mlist)
    (cons left right)))

(defun ind--update (managed-list fringe)
  "Update."
  (let* ((max-line (ind--number-of-lines))
         (height (min max-line (window-body-height)))
         (px-height (* height (frame-char-height)))
         ind-list
         buckets buckets-face)
    (setq ind-list (ind--compute-indicator-positions managed-list))

    ;; next we need to compute their relative "pixel" positions
    (setq ind-list
          (mapcar (lambda (pair)
                    (cons (floor (* (/ (float (ind--line-at-pos (car pair))) (float max-line))
                                    (float px-height)))
                          (cdr pair)))
                  ind-list))
    ;; parition the indicators into buckets for the same line
    (mapc (lambda (pair)
            (let* ((line (/ (car pair) (frame-char-height)))
                   (ov-line (1+ (% (car pair) (frame-char-height)))))
              (if (plist-member buckets line)
                  (let ((bucket (plist-get buckets line))
                        (bucket-face (plist-get buckets-face line)))
                    (setq buckets
                          (plist-put buckets line
                                     (push ov-line bucket)))
                    (when (> (plist-get (cdr pair) :priority)
                             (plist-get bucket-face :priority))
                      (setq buckets-face (plist-put buckets-face line (cdr pair)))))
                (setq buckets (plist-put buckets line (list ov-line)))
                (setq buckets-face (plist-put buckets-face line (cdr pair))))))
          ind-list)
    ;; and now create the overlays
    (let ((top (window-start)))
      (while buckets
        (let* ((line (car buckets))
               (line-ov (cadr buckets))
               (fringe-str (symbol-name fringe))
               (line-bitmap (make-symbol (concat fringe-str "-ovbitmap-" (int-to-string line))))
               (fringe-display-prop (list fringe
                                          line-bitmap
                                          (plist-get (cadr buckets-face) :face)))
               (fringe-text (propertize "!" 'display fringe-display-prop))
               (ov (make-overlay 1 1)))
          (push line-bitmap ind--temp-bitmaps)
          (overlay-put ov 'before-string fringe-text)
          (overlay-put ov 'priority (plist-get (cadr buckets-face) :priority))
          (overlay-put ov 'ind-indicator t)
          (define-fringe-bitmap line-bitmap (ind--create-bitmap line-ov))
          (save-excursion
            (goto-char top)
            (forward-line line)
            (move-overlay ov (point) (point))))
        (setq buckets (cddr buckets))
        (setq buckets-face (cddr buckets-face))))))

(defun ind-update (&optional mlist)
  "Update managed relative indicators.
If optional argument MLIST is non-nil update indicators on that list."
  (interactive)
  (let ((managed-list (or mlist ind-managed-relative-indicators)))
    (when managed-list
      (let* ((split (ind--update-split-by-fringe managed-list))
             (left (car split))
             (right (cdr split)))
        (mapc 'destroy-fringe-bitmap ind--temp-bitmaps)
        (remove-overlays (point-min) (point-max) 'ind-indicator t)
        (ind--update left 'left-fringe)
        (ind--update right 'right-fringe)))))

(defun ind--create-bitmap (lines)
  "Create the bitmap according to LINES.

LINES is a list of numbers where each number indicate there
should be a line drawn on that line in the bitmap.  The numbers
of lines need to be in reverse order.

For example arugment (10 5 1) will return a bitmap [255 0 0 0 255
0 0 0 0 255 0 0 0 0 0] for 15 pixel high line."
  (let (lst (it (frame-char-height)))
    (while (> it 0)
      (if (not (member it lines))
          (progn
            (push 0 lst)
            (setq it (1- it)))
        (dotimes (i ind-indicator-height)
          (push 255 lst))
        (setq it (- it ind-indicator-height))))
    (vconcat lst)))

(defun* ind-create-indicator-at-line (line
                                      &key
                                      (dynamic t)
                                      (managed nil)
                                      (relative t)
                                      (bitmap 'ind-dash)
                                      (fringe 'right-fringe)
                                      (face font-lock-warning-face)
                                      (priority 10))
  "Add an indicator on LINE.

See `ind-create-indicator' for values of optional arguments."
  (let ((pos (if dynamic (ind--pos-at-line line) (list line))))
    (ind-create-indicator pos
                          :dynamic dynamic
                          :managed managed
                          :relative relative
                          :bitmap bitmap
                          :fringe fringe
                          :face face
                          :priority priority)))

(defun* ind-create-indicator (pos
                              &key
                              (dynamic t)
                              (managed nil)
                              (relative t)
                              (bitmap 'ind-dash)
                              (fringe 'right-fringe)
                              (face font-lock-warning-face)
                              (priority 10))
  "Add an indicator to position POS.

If keyword argument DYNAMIC is non-nil create a dynamic indicator
on this line.  That means the indicator position updates as the
text is inserted/removed.  With nil the indicator always points
to the same buffer position.

If keyword argument MANAGED is non-nil the indicator is
automatically managed by `indicators-mode'.  This means it will
be automatically updated on window scrolling, window
configuration changes and after buffer modifications.  If you
create unmanaged indicator, you can update it manually by calling
`ind-update' with a list of indicators to be updated.

If keyword argument RELATIVE is non-nil the indicator shows
relative buffer position.  Otherwise the indicator is only
displayed if it is inside the viewpoint.

Keyword argument BITMAP specifies a bitmap to be used to display
this indicator.  This setting can only be used with non-relative
indicator.  See variable `fringe-bitmaps' for built-in bitmaps
you can use.

Keyword argument FACE is a face to use when displaying the bitmap
for this indicator.

Keyword argument PRIORITY determines the face of the bitmap if
more indicators are on the same physical line."
  (when (and (not indicators-mode)
             managed)
    (indicators-mode t))
  (let ((pos (if (and dynamic
                      (integer-or-marker-p pos))
                 (let ((m (point-marker)))
                   (set-marker-insertion-type m t)
                   (set-marker m pos))
               pos)))
    (if relative
        (let ((indicator (cons pos (list :face face :priority priority :fringe fringe))))
          (when managed
            (push indicator ind-managed-relative-indicators)
            (ind-update))
          indicator)
      (let* ((fringe-display-prop (list fringe
                                        bitmap
                                        face))
             (fringe-text (propertize "!" 'display fringe-display-prop))
             (ov (make-overlay 1 1))
             (indicator (cons pos ov)))
        (overlay-put ov 'before-string fringe-text)
        (overlay-put ov 'before-string-backup fringe-text)
        (overlay-put ov 'priority priority)
        (overlay-put ov 'ind-indicator-absolute t)
        (when managed
          (push indicator ind-managed-absolute-indicators)
          (ind-update-absolute))
        indicator))))

(defun ind-clear-indicators ()
  "Remove all indicators managed by `indicators-mode'."
  (interactive)
  (ind-clear-indicators-relative)
  (ind-clear-indicators-absolute))

(defun ind-clear-indicators-relative ()
  "Remove all relative indicators managed by `indicators-mode'."
  (interactive)
  (remove-overlays (point-min) (point-max) 'ind-indicator t)
  (setq ind-managed-relative-indicators nil))

(defun ind-clear-indicators-absolute ()
  "Remove all absolute indicators managed by `indicators-mode'."
  (interactive)
  (setq ind-managed-absolute-indicators nil)
  (remove-overlays (point-min) (point-max) 'ind-indicator-absolute t))

(defun ind--hide-absolute-indicators (mlist)
  "Hide the absolute indicators on MLIST."
  (let ((ovs (mapcar 'cdr mlist)))
    (mapc (lambda (ov)
            (overlay-put ov 'before-string nil))
          ovs)))

(defun ind--show-absolute-indicators (mlist)
  "Hide the absolute indicators on MLIST."
  (let ((ovs (mapcar 'cdr mlist)))
    (mapc (lambda (ov)
            (let ((bfs (overlay-get ov 'before-string-backup)))
              (overlay-put ov 'before-string bfs)))
          ovs)))

(defun ind-hide-indicators ()
  "Hide all indicators managed by `indicators-mode'."
  (interactive)
  (ind--hide-absolute-indicators ind-managed-absolute-indicators)
  (mapc (lambda (lst)
          (ind--hide-absolute-indicators (symbol-value lst)))
        ind-managed-list-absolute)
  (remove-overlays (point-min) (point-max) 'ind-indicator t))

(defun ind-show-indicators ()
  "Show all indicators managed by `indicators-mode'"
  (interactive)
  (ind--show-absolute-indicators ind-managed-absolute-indicators)
  (mapc (lambda (lst)
          (ind--show-absolute-indicators (symbol-value lst)))
        ind-managed-list-absolute)
  (ind-update-event-handler))

(define-minor-mode indicators-mode
  "Toggle indicators mode."
  :init-value nil
  :lighter " Ind"
  :group 'indicators-mode
  (if indicators-mode
      (progn
        (add-hook 'window-scroll-functions 'ind-update-event-handler nil t)
        (add-hook 'window-configuration-change-hook 'ind-update-event-handler nil t)
        (add-hook 'after-change-functions 'ind-update-event-handler nil t)
        (ind-show-indicators))
    (remove-hook 'window-scroll-functions 'ind-update-event-handler t)
    (remove-hook 'window-configuration-change-hook 'ind-update-event-handler t)
    (remove-hook 'after-change-functions 'ind-update-event-handler t)
    (ind-hide-indicators)))

(provide 'indicators)

;;; indicators.el ends here
