;;; splitter.el --- Manage window splits
;;
;; Author: Steven Thomas
;; Created: 16 Oct 2011
;; Keywords: frames convenience
;; Version: 20130705.50
;; X-Original-Version: 0.1.0
;; URL: https://github.com/chumpage/chumpy-windows
;;
;;; Commentary:
;;
;; Splitter lets you easily declare and apply sophisticated window
;; layouts, create grids of windows, and intelligently shrink a window
;; layout to create room for a new window.
;;
;; See https://github.com/chumpage/chumpy-windows for more
;; documentation, and to submit patches.
;;
;;; Code:

(require 'cl)

(defun spl-clamp (val min max)
  (max (min val max) min))

(defun spl-pad-list (lst min-length value)
  (if (>= (length lst) min-length)
      lst
      (append lst (make-list (- min-length (length lst)) value))))

(defun spl-ndsort (lst predicate)
  (sort (copy-sequence lst) predicate))

(defun spl-flatten (lst)
  (mapcan (lambda (x) (if (listp x) x nil)) lst))

(defun spl-filter (filter-list predicate)
  (let ((result))
    (dolist (filter-list-val filter-list)
      (when (funcall predicate filter-list-val)
        (push filter-list-val result)))
    (nreverse result)))

(defun spl-partition (partition-list predicate)
  (let ((success-list) (fail-list))
    (dolist (partition-list-val partition-list)
      (if (funcall predicate partition-list-val)
          (push partition-list-val success-list)
          (push partition-list-val fail-list)))
    (list (nreverse success-list) (nreverse fail-list))))

(defun spl-window-size-for-split-direction (window direction)
  (if (eq direction 'h)
      (window-width window)
      (window-height window)))

(defun spl-split-direction-horizontal-p (direction)
  (if (eq direction 'h)
      t
      nil))

(defun spl-normalize-window-size (size window-size &optional round-fn)
  (when (null round-fn) (setq round-fn 'round))
  (let ((size-in-chars (if (and (> size -1) (< size 1))
                           (funcall round-fn (* (float size) window-size))
                           size)))
    (if (< size-in-chars 0)
        (- window-size size-in-chars)
        size-in-chars)))

(defun spl-tweak-window-size (direction size windows)
  (let* ((total-size
          (reduce '+ windows :key
                  (lambda (w) (spl-window-size-for-split-direction w direction))))
         (win-1-desired-size (spl-normalize-window-size size total-size)))
    (adjust-window-trailing-edge
     (car windows)
     (- win-1-desired-size
        (spl-window-size-for-split-direction (car windows) direction))
     (spl-split-direction-horizontal-p direction))))

(defun spl-split (direction &optional size window)
  "Used to split windows. DIRECTION should be 'h or 'v to specify
the direction in which to split.

SIZE specifies the size of the windows and defaults to .5. If
SIZE is positive, it applies to the left window in a horizontal
split, and the top window in a vertical split. If SIZE is
negative, it applies to the right window in a horizontal split,
and the bottom window in a vertical split. In addition, SIZE can
be given as either a float value between -1 and 1, in which case
it's interpreted as a percentage. If SIZE is a value outside that
range, it's interpreted as the number of characters to give to
the window.

WINDOW is the window to split, and defaults to the selected
window.

For example, to get a 75%-25% vertical split, you would do
  (spl-split 'v .75)

To get a vertical split where the bottom window is sized to 10
characters, you would do
  (spl-split 'v -10)

The return value is a list of the two windows, left/right in a
horizontal split and top/bottom in a vertical split."
  (when (null size) (setq size .5))
  (when (null window) (setq window (selected-window)))
  (destructuring-bind (wl wt wr wb) (window-pixel-edges window)
    (split-window window nil
                  (spl-split-direction-horizontal-p direction))
    (let ((windows (list (spl-get-window-from-top-left wl wt)
                         (spl-get-window-from-bottom-right wr wb))))
      (spl-tweak-window-size direction size windows)
      windows)))

(defun spl-verify-split*-sizes (num-windows sizes)
  (setq sizes (copy-tree sizes))
  (when (or (< (length sizes) (1- num-windows))
            (> (length sizes) num-windows))
    (error (format "wrong number of sizes given. num sizes = %d, num windows = %d."
                   (length sizes) num-windows)))
  (dolist (size sizes)
    (when (or (< size 0) (> size 1))
      (error (format "invalid size given: %f. should be between 0 and 1." size))))
  (let ((sum (apply '+ sizes)))
    (when (> sum (+ 1 1e-6))
      (error (format "sum of sizes (%f) too high. should be 1." sum)))
    (if (< (length sizes) num-windows)
      (setq sizes (append sizes (list (- 1 sum))))
      (when (< sum (- 1 1e-6))
        (error (format "sum of sizes (%f) too low. should be 1." sum)))))
  sizes)

(defun spl-tweak-window-size-multiple (direction sizes windows)
  (let* ((available-size
          (reduce '+ windows :key
                  (lambda (w) (spl-window-size-for-split-direction w direction))))
         (normalized-sizes
          (mapcar (lambda (size) (spl-normalize-window-size size available-size 'floor))
                  sizes))
         (used-size (reduce '+ normalized-sizes))
         (leftover (- available-size used-size)))
    (dotimes (n (- (length windows) 1))
      (adjust-window-trailing-edge
       (nth n windows)
       (+ (spl-clamp leftover 0 1)
          (- (nth n normalized-sizes)
             (spl-window-size-for-split-direction (nth n windows) direction)))
       (spl-split-direction-horizontal-p direction))
      (setq leftover (1- leftover)))))

(defun spl-split* (direction num-windows &optional sizes window)
  (when (null sizes) (setq sizes (make-list num-windows (/ 1.0 num-windows))))
  (setq sizes (spl-verify-split*-sizes num-windows sizes))
  (when (null window) (setq window (selected-window)))
  (let ((windows (list window)))
    (dotimes (n (- num-windows 1))
      (destructuring-bind (win-1 win-2)
          (spl-split direction window-min-height window)
        (setq window win-2)
        (setq windows (cons win-2 windows))))
    (setq windows (nreverse windows))
    (spl-tweak-window-size-multiple direction sizes windows)
    windows))

(defun spl-window-sort-predicate (w1 w2)
  (let* ((w1-x (nth 0 (window-pixel-edges w1)))
         (w1-y (nth 1 (window-pixel-edges w1)))
         (w2-x (nth 0 (window-pixel-edges w2)))
         (w2-y (nth 1 (window-pixel-edges w2))))
    (if (not (= w1-y w2-y))
        (< w1-y w2-y)
        (< w1-x w2-x))))

(defun spl-partition-list (lst fn)
  (let* ((non-matching-elts (remove-if fn lst))
         (matching-elts (set-difference lst non-matching-elts)))
    (list matching-elts non-matching-elts)))

(defun spl-sorted-window-list (&optional windows)
  "Provides a list of windows sorted from top-left to bottom-right."
  (when (null windows) (setq windows (window-list nil 'no-minibuf)))
  (spl-ndsort windows 'spl-window-sort-predicate))

(defun spl-load-buffer-in-window (buffer window)
  (if (and buffer (get-buffer buffer))
      (progn (set-window-buffer window buffer)
             (eq (window-buffer window) (get-buffer buffer)))
      nil))

(defun spl-grid (&optional rows cols buffers default-buffer)
  "Splits the frame into a grid of evenly sized windows of ROWS x
COLS dimensions. BUFFERS is a list of buffers to assign to the
newly created windows. The windows are ordered from top-left to
bottom-right, so that the first buffer will go in the top left
window and the last buffer will go in the bottom right window. If
the BUFFERS list is nil or too short, or if a specified buffer
doesn't exist, DEFAULT-BUFFER is loaded into the window instead.

Called interactively, window-grid prompts the user for the rows
and columns."
  (interactive)
  (when (not (or rows cols))
    (let* ((input-str (read-string "Window grid layout (e.g. '2 3'): "))
           (strs (or (split-string input-str) '("0" "0")))
           (r (string-to-number (or (elt strs 0) "0")))
           (c (string-to-number (or (elt strs 1) "0"))))
      (setq rows r cols c)))
  (when (<= (min rows cols) 0)
    (error "spl-grid: invalid rows/cols"))
  (delete-other-windows)
  (spl-split* 'v rows)
  (let ((windows (spl-sorted-window-list)))
    (assert (= (length windows) rows))
    (mapc (lambda (window)
            (select-window window)
            (spl-split* 'h cols))
          windows))
  (spl-set-window-buffers buffers default-buffer)
  (select-window (car (spl-sorted-window-list))))

(defun spl-set-window-buffers (buffers default-buffer)
  (let* ((windows (spl-sorted-window-list))
         (buffers (spl-pad-list buffers (length windows) nil)))
    (map 'list (lambda (window buffer) (or (spl-load-buffer-in-window buffer window)
                                           (spl-load-buffer-in-window default-buffer window)))
         windows buffers)))

(defun spl-left-edge (window) (nth 0 (window-pixel-edges window)))
(defun spl-top-edge (window) (nth 1 (window-pixel-edges window)))
(defun spl-right-edge (window) (nth 2 (window-pixel-edges window)))
(defun spl-bottom-edge (window) (nth 3 (window-pixel-edges window)))

(defun spl-windows-bounding-box (windows)
  (if (null windows)
      nil
      (let ((left most-positive-fixnum)
            (top most-positive-fixnum)
            (right most-negative-fixnum)
            (bottom most-negative-fixnum))
        (dolist (w windows)
          (when (< (spl-left-edge w) left) (setq left (spl-left-edge w)))
          (when (< (spl-top-edge w) top) (setq top (spl-top-edge w)))
          (when (> (spl-right-edge w) right) (setq right (spl-right-edge w)))
          (when (> (spl-bottom-edge w) bottom) (setq bottom (spl-bottom-edge w))))
        (list left top right bottom))))

(defun spl-get-windows-sharing-edge (windows edge-fn val)
  (spl-filter windows (lambda (w) (= (funcall edge-fn w) val))))

(defun spl-get-window-from-top-left (left top)
  (car (spl-filter (spl-sorted-window-list)
                  (lambda (w) (and (= (spl-left-edge w) left)
                                   (= (spl-top-edge w) top))))))

(defun spl-get-window-from-bottom-right (right bottom)
  (car (spl-filter (spl-sorted-window-list)
                  (lambda (w) (and (= (spl-right-edge w) right)
                                   (= (spl-bottom-edge w) bottom))))))

(defun spl-check-split-continuation (windows
                                    continuing-edge-fn
                                    continuing-edge-val
                                    next-window-edge-fn
                                    next-window-edge-val
                                    boundary-edge-fn
                                    boundary-edge-val)
  (block nil
    (while t
      (let ((next-window (car (spl-get-windows-sharing-edge
                               (spl-get-windows-sharing-edge windows next-window-edge-fn next-window-edge-val)
                               continuing-edge-fn continuing-edge-val))))
        (when (null next-window) (return nil))
        (setq next-window-edge-val (funcall boundary-edge-fn next-window))
        (when (= next-window-edge-val boundary-edge-val) (return t))))))

(defun spl-partition-windows-along-edge (windows direction edge)
  (case direction
    ('h (spl-partition windows (lambda (w) (<= (spl-right-edge w) edge))))
    ('v (spl-partition windows (lambda (w) (<= (spl-bottom-edge w) edge))))
    (t (error "spl-partition-windows-along-edge: direction should be 'h or 'v"))))

(defun spl-get-split-% (low high split-loc)
  (/ (float (- split-loc low)) (- high low)))

;; specifying the shrink-direction is a nasty hack. when i know i'm shrinking
;; vertically, i record the horizontal split sizes as character values rather than
;; percentages (and similarly for the vertical split sizes when i'm shrinking
;; horizontally). this is because when i apply the layout, the widths of the windows
;; aren't exactly the same as the way they were in the original layout, because i'm not
;; accounting for the fringes and divider lines when i split. i should fix this and
;; then get rid of the shrink-direction parameter. one way to fix it would be to have a
;; post-layout-application window size tweak process, that tries to get the window
;; sizes to account for the window decorations. another way to fix it would be to
;; detect multi-splits (i.e. spl-split* splits) and record those when i determine the
;; window layout. the spl-split* function already handles the window sizing issue i'm
;; trying to work around here.
(defun spl-determine-window-layout-recursive (windows &optional shrink-direction)
  (if (<= (length windows) 1)
      nil
      (block 'layout
        (let ((windows (spl-sorted-window-list windows))
              (box (spl-windows-bounding-box windows)))
          (dolist (w windows)
            (when (and (= (spl-left-edge w) (nth 0 box))
                       (/= (spl-bottom-edge w) (nth 3 box))
                       (spl-check-split-continuation windows
                                                    'spl-bottom-edge (spl-bottom-edge w)
                                                    'spl-left-edge (spl-left-edge w)
                                                    'spl-right-edge (nth 2 box)))
              (let ((partitioned-windows (spl-partition-windows-along-edge
                                          windows 'v (spl-bottom-edge w))))
                (return-from 'layout (list 'v
                                           (if (or (null shrink-direction) (eq shrink-direction 'v))
                                               (spl-get-split-% (spl-top-edge w) (nth 3 box) (spl-bottom-edge w))
                                               (window-height w))
                                           (spl-determine-window-layout-recursive (first partitioned-windows) shrink-direction)
                                           (spl-determine-window-layout-recursive (second partitioned-windows) shrink-direction)))))
            (when (and (= (spl-top-edge w) (nth 1 box))
                       (/= (spl-right-edge w) (nth 2 box))
                       (spl-check-split-continuation windows
                                                    'spl-right-edge (spl-right-edge w)
                                                    'spl-top-edge (spl-top-edge w)
                                                    'spl-bottom-edge (nth 3 box)))
              (let ((partitioned-windows (spl-partition-windows-along-edge
                                          windows 'h (spl-right-edge w))))
                (return-from 'layout (list 'h
                                           (if (or (null shrink-direction) (eq shrink-direction 'h))
                                               (spl-get-split-% (spl-left-edge w) (nth 2 box) (spl-right-edge w))
                                               (window-width w))
                                           (spl-determine-window-layout-recursive (first partitioned-windows) shrink-direction)
                                           (spl-determine-window-layout-recursive (second partitioned-windows) shrink-direction))))))
          (error "spl-determine-window-layout-recursive: i shouldn't ever get here")))))

(defun spl-determine-window-layout (&optional shrink-direction)
  (list (spl-determine-window-layout-recursive (spl-sorted-window-list) shrink-direction)
        ;; we need to explicitly preserve and reapply the window-start and
        ;; window-hscroll, otherwise redisplay will reposition those values so that
        ;; point is in the middle of the window
        (mapcar (lambda (w) (list (window-buffer w)
                                  (window-start w)
                                  (window-hscroll w)))
                (spl-sorted-window-list))))

(defun spl-apply-window-layout-recursive (layout window)
  (when layout
    ;; Each layout can have one of two forms:
    ;;   (h size layout1 layout2)
    ;;   (h* num-windows (size1 size2...) (layout1 layout2...))
    (if (find (car layout) '(h v))
        (destructuring-bind (direction size layout-1 layout-2) layout
          (destructuring-bind (win-1 win-2)
              (spl-split direction size window)
            (spl-apply-window-layout-recursive layout-1 win-1)
            (spl-apply-window-layout-recursive layout-2 win-2)))
        (destructuring-bind (direction num-windows sizes sub-layouts) layout
          (let* ((direction (if (eq direction 'h*) 'h 'v))
                 (windows (spl-split* direction num-windows sizes window)))
            (mapcar* (lambda (layout window) (spl-apply-window-layout-recursive layout window))
                     sub-layouts windows))))))

(defun spl-apply-window-layout (layout window)
  (destructuring-bind (wl wt wr wb) (window-pixel-edges window)
    (spl-apply-window-layout-recursive (first layout) window)
    (let ((windows (spl-filter (spl-sorted-window-list)
                              (lambda (w) (and (>= (spl-left-edge w) wl)
                                               (>= (spl-top-edge w) wt)
                                               (<= (spl-right-edge w) wr)
                                               (<= (spl-bottom-edge w) wb)))))
          (window-data (second layout)))
      (when window-data
        (assert (= (length windows) (length (second layout))))
        (map 'list (lambda (window window-info)
                     (when (spl-load-buffer-in-window (first window-info) window)
                       (set-window-start window (second window-info))
                       (set-window-hscroll window (third window-info))))
             windows (second layout)))
      nil)))

(defun spl-flex-layout (layout buffers default-buffer)
  (delete-other-windows)
  (spl-apply-window-layout (list layout nil) (selected-window))
  (spl-set-window-buffers buffers default-buffer)
  (select-window (car (spl-sorted-window-list))))

(defun spl-select-non-minibuffer-window ()
  (select-window (car (window-list nil 'no-minibuf))))

(defun spl-shrink-window-layout (direction size)
  (save-selected-window
    (spl-select-non-minibuffer-window)
    (let ((layout (spl-determine-window-layout direction)))
      (delete-other-windows)
      (destructuring-bind (win-1 win-2) (spl-split direction size)
        (spl-apply-window-layout layout win-1)
        win-2))))

(provide 'splitter)

;;; splitter.el ends here
