;;; mag-menu.el --- Intuitive keyboard-centric menu system
;;
;; Author: Steven Thomas
;; Created: 02 Jan 2013
;; Keywords: convenience
;; Version: 20130224.56
;; X-Original-Version: 0.1.0
;; URL: https://github.com/chumpage/mag-menu
;; Package-Requires: ((splitter "0.1.0"))
;;
;; Mag-menu is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 3, or (at your option) any
;; later version.
;;
;; Mag-menu is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Magit.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; Mag-menu provides a menu system intended to be used as an emacs
;; front-end to command line programs with lots of options (e.g. git or
;; ack). It presents the options in a visually simple layout in a window
;; at the bottom of the emacs frame, and makes it easy to toggle
;; switches or set argument values using just the keyboard.
;;
;; Mag-menu is derived from the magit-key-mode.el file in magit. The
;; code was pulled out to make it a standalone elpa package so it could
;; more easily be used by packages other than magit, and also to make
;; the code less specific to git. "Mag" in "mag-menu" is short for
;; magic, but is also meant to suggest its heritage from magit.
;;
;; The main function is mag-menu.
;;
;;; Code:

(require 'cl)
(require 'splitter)

(defvar mag-menu-buf-name "*mag-menu*"
  "Name of the buffer.")

(defvar mag-menu-use-splitter-shrink t
  "When set to t, use spl-shrink-window-layout to intelligently
shrink the current window layout to make room for the menu
window. If set to nil, instead crush the current layout when
bringing up the menu window.")

(defvar mag-menu-args-in-cols nil
  "When true, draw arguments in columns as with switches and
  options.")

;; The vars below are for internal use

;; !!! Replace current-args and current-options with custom-options
(defvar mag-menu-current-args nil
  "A hash-table of current argument set.")

(defvar mag-menu-current-options '()
  "Current option set.")

(defvar mag-menu-previous-window-config nil
  "The pre-menu window configuration, which will be restored when
mag-menu is finished.")

(defvar mag-menu-key-maps '()
  "This will be filled lazily with proper `define-key' built
keymaps as they're requested.")

(defvar mag-menu-prefix nil
  "For internal use.  Holds the prefix argument to the command
that brought up the key-mode window, so it can be used by the
command that's eventually invoked.")

(defun mag-menu-remove-option (options-alist name)
  (remove* name options-alist :key 'car :test 'equal))

(defun mag-menu-set-option (options-alist name val)
  (setq options-alist (copy-tree options-alist))
  (let ((opt (assoc name options-alist)))
    (if opt
        (setf (cdr opt) val)
        (setq options-alist (append options-alist (list (cons name val))))))
  options-alist)

(defun mag-menu-toggle-switch (options-alist name)
  (if (find name options-alist :key 'car :test 'equal)
      (mag-menu-remove-option options-alist name)
      (mag-menu-set-option options-alist name nil)))

(defun mag-menu-key-defined-p (group key)
  "If KEY is defined as any of switch, argument or action within
GROUP then return t"
  (catch 'result
    (dolist (type '(actions switches arguments))
      (when (assoc key (assoc type group))
        (throw 'result t)))))

(defun mag-menu-help (group)
  "Provide help for a key (which the user is prompted for) within
GROUP."
  (let* ((man-page (cadr (assoc 'man-page group)))
         (seq (read-key-sequence
               (format "Enter command prefix%s: "
                       (if man-page
                         (format ", `?' for man `%s'" man-page)
                         ""))))
         (actions (cdr (assoc 'actions group))))
    (cond
      ;; if it is an action popup the help for the to-be-run function
      ((assoc seq actions) (describe-function (nth 2 (assoc seq actions))))
      ;; if there is "?" show a man page if there is one
      ((equal seq "?")
       (if man-page
         (man man-page)
         (error "No man page associated with `%s'" (car group))))
      (t (error "No help associated with `%s'" seq)))))

(defun mag-menu-exec-at-point ()
  "Run action/args/option at point."
  (interactive)
  (let* ((key (or (get-text-property (point) 'key-group-executor)
                  (error "Nothing at point to do.")))
         (def (lookup-key (current-local-map) key)))
    (call-interactively def)))

(defun mag-menu-build-exec-point-alist ()
  (save-excursion
    (goto-char (point-min))
    (let* ((exec (get-text-property (point) 'key-group-executor))
           (exec-alist (if exec `((,exec . ,(point))) nil)))
      (do nil ((eobp) (nreverse exec-alist))
        (when (not (eq exec (get-text-property (point) 'key-group-executor)))
          (setq exec (get-text-property (point) 'key-group-executor))
          (when exec (push (cons exec (point)) exec-alist)))
        (forward-char)))))

(defun mag-menu-jump-to-next-exec ()
  "Jump to the next action/args/option point."
  (interactive)
  (let* ((exec-alist (mag-menu-build-exec-point-alist))
         (next-exec-pos (find-if (lambda (pos) (> pos (point))) exec-alist :key 'cdr)))
    (when (or next-exec-pos exec-alist)
      (goto-char (or (cdr next-exec-pos) (cdar exec-alist)))
      (skip-chars-forward " "))))

(defun mag-menu-build-keymap (group)
  "Construct a normal looking keymap for the key mode to use and
put it in mag-menu-key-maps for fast lookup."
  (let* ((name (car group))
         (actions (cdr (assoc 'actions group)))
         (switches (cdr (assoc 'switches group)))
         (arguments (cdr (assoc 'arguments group)))
         (map (make-sparse-keymap)))
    (suppress-keymap map 'nodigits)
    ;; ret dwim
    (define-key map (kbd "RET") 'mag-menu-exec-at-point)
    ;; tab jumps to the next "button"
    (define-key map (kbd "TAB") 'mag-menu-jump-to-next-exec)

    ;; all maps should `quit' with `C-g' or `q'
    (define-key map (kbd "C-g") (lambda ()
                                  (interactive)
                                  (mag-menu-command nil)))
    (define-key map (kbd "q") (lambda ()
                                (interactive)
                                (mag-menu-command nil)))
    ;; run help
    (define-key map (kbd "?") (lambda ()
                                (interactive)
                                (mag-menu-help group)))

    (flet ((defkey (k action)
             (when (and (lookup-key map (car k))
                        (not (numberp (lookup-key map (car k)))))
               (message "Warning: overriding binding for `%s' in %S"
                        (car k) name)
               (ding)
               (sit-for 2))
             (define-key map (car k)
               `(lambda () (interactive) ,action))))
      (when actions
        (dolist (k actions)
          (defkey k `(mag-menu-command ',(nth 2 k)))))
      (when switches
        (dolist (k switches)
          (defkey k `(mag-menu-add-option ',group ,(nth 2 k) ',(nth 3 k)))))
      (when arguments
        (dolist (k arguments)
          (defkey k `(mag-menu-add-argument
                      ',group ,(nth 2 k) ',(nth 3 k) ',(nth 4 k))))))

    (push (cons group map) mag-menu-key-maps)
    map))

(defun mag-menu-command (func)
  (let ((args '()))
    ;; why can't maphash return a list?!
    (maphash (lambda (k v)
               (push (concat k (shell-quote-argument v)) args))
             mag-menu-current-args)
    (let ((options-alist (mag-menu-form-options-alist mag-menu-current-options
                                                      mag-menu-current-args))
          (current-prefix-arg (or current-prefix-arg mag-menu-prefix)))
      (set-window-configuration mag-menu-previous-window-config)
      (when func
        (funcall func options-alist))
      (mag-menu-kill-buffer))))

(defun mag-menu-add-argument (group arg-name callback history-var)
  (let ((option-name (substring arg-name 0 (1- (length arg-name))))
        (options-alist (mag-menu-form-options-alist mag-menu-current-options
                                                    mag-menu-current-args)))
    (setq options-alist (funcall callback option-name options-alist history-var))
    (destructuring-bind (switches args) (mag-menu-extract-switches-and-args options-alist)
      (setq mag-menu-current-options switches)
      (setq mag-menu-current-args args)))
  (mag-menu-redraw group))

(defun mag-menu-add-option (group option-name callback)
  "Toggles the appearance of OPTION-NAME in
`mag-menu-current-options'."
  (let ((options-alist (mag-menu-form-options-alist mag-menu-current-options
                                                    mag-menu-current-args)))
    (if callback
        (setq options-alist (funcall callback option-name options-alist))
        (setq options-alist (mag-menu-toggle-switch options-alist option-name)))
    (destructuring-bind (switches args) (mag-menu-extract-switches-and-args options-alist)
      (setq mag-menu-current-options switches)
      (setq mag-menu-current-args args)))
  (mag-menu-redraw group))

(defun mag-menu-read-generic (option-name options history-var)
  ;; To automatically insert the last value in the prompt, use this line
  ;; (read-from-minibuffer (concat option-name ": ") (car (symbol-value history-var)) nil nil `(,history-var . 1))
  (let ((val (read-from-minibuffer (concat option-name ": ") nil nil nil history-var)))
    (if (= (length val) 0)
        (mag-menu-remove-option options option-name)
        (mag-menu-set-option options option-name val))))

(defun mag-menu-read-directory-name (option-name options history-var)
  (let ((dir (read-directory-name (concat option-name ": "))))
    (if (= (length dir) 0)
        (mag-menu-remove-option options option-name)
        (mag-menu-set-option options option-name dir))))

(defun mag-menu-kill-buffer ()
  (interactive)
  (kill-buffer mag-menu-buf-name))

(defun mag-menu-extract-switches-and-args (options-alist)
  (let ((switches)
        (args (make-hash-table :test 'equal)))
    (mapc (lambda (option)
            (if (null (cdr option))
                (push (car option) switches)
                (puthash (concat (car option) "=") (cdr option) args)))
          options-alist)
    (list (nreverse switches) args)))

(defun mag-menu-form-options-alist (switches args)
  (let ((alist nil))
    (mapc (lambda (switch) (push (cons switch nil) alist))
          switches)
    (maphash (lambda (name val) (push (cons (if (char-equal (aref name (- (length name) 1)) ?=)
                                                (substring name 0 (- (length name) 1)))
                                            val)
                                      alist))
             args)
    (nreverse alist)))

(defun mag-menu (group &optional options-alist)
  "Brings up a menu for the user to select options and then run
actions, all of which are described by GROUP. GROUP should have
the following form:

  `(group-name
     (man-page \"man-page\")
     (actions
      (\"r\" \"Run the command\" action-callback))
     (switches
      (\"-b\" \"Some on/off option\" \"--long-form-option-name\" switch-callback))
     (arguments
      (\"-f\" \"Some option that takes a value\" \"--value=\" arg-callback history-var)))

The group-name value is a symbol describing the program whose
options are being set (e.g. 'ack, 'git-log, etc). It's currently
unused, but may be used in the future. Set it to something
meaningful.

Actions represent commands that can be run, switches are simple
flags that the command can take, and arguments are options that
take a value. Each of these can have multiple entries, although
only one entry for each is used in the example above. The short
name of each entry will be bound as a key in the mag-menu buffer,
which will show up at the bottom of the frame. So in the example
above, \"r\", \"-b\", and \"-f\" will all be bound as keyboard
shortcuts.

The optional OPTIONS-ALIST arg is an assoc list containing
default values for the switches and arguments. The long form
names should be used. An example would be

  '((\"--switch1\") (\"--switch2\") (\"--argument1\" . \"value1\"))

As with any assoc list, each element is a cons pair. Switches
should appear in the cons pair alone, and for arguments, the car
is the argument name (again, the long form name) and the cdr is
the value.

The switch-callback function is optional. If present, it should
be a function that takes two arguments: the first is the option
name (long form), and the second is the current assoc list of
options. The callback should return an options assoc list with
the appropriate changes made. You may find the
mag-menu-toggle-switch function useful for toggling a switch
value. If you don't provide a callback, the switch is simply
toggled. Providing a callback is useful for example if some
switches are mutually exclusive, and you want to disable switch A
when switch B is activated.

Unlike the switch-callback, the arg-callback is mandatory. It
should be a function that takes three arguments: the option name,
the options assoc list, and a history variable. It should prompt
the user for the option value, then return an options assoc list
with the appropriate changes made. Mag-menu provides some
predefined callbacks are provided that may be suitable:
mag-menu-read-generic, mag-menu-read-directory-name, etc.

The action-callback is also mandatory. It should take just one
value, the options assoc list. It's recommended to copy this list
to a separate variable (via copy-tree), and then pass this
variable in as the OPTIONS-ALIST variable the next time you call
mag-menu.

You may want to look at ack-menu.el
\(https://github.com/chumpage/ack-menu) for a complete example of
how to use mag-menu."
  (interactive)
  ;; save the window config to restore it as was (no need to make this
  ;; buffer local)
  (setq mag-menu-previous-window-config (current-window-configuration))
  ;; setup the mode, draw the buffer
  (let ((buf (get-buffer-create mag-menu-buf-name)))
    (if mag-menu-use-splitter-shrink
        (select-window (spl-shrink-window-layout 'v (- window-safe-min-height)))
        (progn (delete-other-windows)
               (split-window-vertically)
               (other-window 1)))
    (switch-to-buffer buf)
    (kill-all-local-variables)
    (destructuring-bind (switches args) (mag-menu-extract-switches-and-args options-alist)
      (set (make-local-variable
            'mag-menu-current-options)
           switches)
      (set (make-local-variable
            'mag-menu-current-args)
           args))
    (set (make-local-variable 'mag-menu-prefix) current-prefix-arg)
    (mag-menu-redraw group))
  (message
   (concat
    "Type a prefix key to toggle it. Run 'actions' with their prefixes. "
    "'?' for more help.")))

(defun mag-menu-get-key-map (group)
  "Get or build the keymap for GROUP."
  ;; Could use avl-tree (or similar) to go from linear to logarithmic lookup here
  (or (cdr (assoc group mag-menu-key-maps))
      (mag-menu-build-keymap group)))

(defun mag-menu-redraw (group)
  "(re)draw the mag-menu buffer."
  (let ((buffer-read-only nil)
        (current-exec (get-text-property (point) 'key-group-executor))
        (new-exec-pos)
        (old-point (point))
        (is-first (zerop (buffer-size)))
        (actions-p nil))
    (erase-buffer)
    (make-local-variable 'font-lock-defaults)
    (use-local-map (mag-menu-get-key-map group))
    (setq actions-p (mag-menu-draw group))
    (delete-trailing-whitespace)
    (setq mode-name "mag-menu-mode" major-mode 'mag-menu-mode)
    (when current-exec
      (setq new-exec-pos (cdr (assoc current-exec (mag-menu-build-exec-point-alist)))))
    (if (and is-first actions-p)
      (progn (goto-char actions-p)
             (mag-menu-jump-to-next-exec))
      (if new-exec-pos
          (progn (goto-char new-exec-pos)
                 (skip-chars-forward " "))
          (goto-char old-point))))
  (setq buffer-read-only t)
  (fit-window-to-buffer))

(defun mag-menu-draw-header (header)
  "Draw a header with the correct face."
  (insert (propertize header 'face 'font-lock-keyword-face) "\n"))

(defun mag-menu-draw-args (args)
  "Draw the args part of the menu."
  (mag-menu-draw-buttons
   "Args"
   args
   (lambda (x)
     (format "(%s) %s"
             (nth 2 x)
             (propertize (gethash (nth 2 x) mag-menu-current-args "")
                         'face 'widget-field)))
   (not mag-menu-args-in-cols)))

(defun mag-menu-draw-switches (switches)
  "Draw the switches part of the menu."
  (mag-menu-draw-buttons
   "Switches"
   switches
   (lambda (x)
     (format "(%s)" (let ((s (nth 2 x)))
                      (if (member s mag-menu-current-options)
                        (propertize s 'face 'font-lock-warning-face)
                        s))))))

(defun mag-menu-draw-actions (actions)
  "Draw the actions part of the menu."
  (mag-menu-draw-buttons "Actions" actions nil))

(defun mag-menu-draw-buttons (section xs maker
                                    &optional one-col-each)
  (when xs
    (mag-menu-draw-header section)
    (mag-menu-draw-in-cols
     (mapcar (lambda (x)
               (let* ((head (propertize (car x) 'face 'font-lock-builtin-face))
                      (desc (nth 1 x))
                      (more (and maker (funcall maker x)))
                      (text (format " %s: %s%s%s"
                                    head desc (if more " " "") (or more ""))))
                 (propertize text 'key-group-executor (car x))))
             xs)
     one-col-each)))

(defun mag-menu-draw-in-cols (strings one-col-each)
  "Given a list of strings, print in columns (using `insert'). If
ONE-COL-EACH is true then don't columify, but rather, draw each
item on one line."
  (let ((longest-act (apply 'max (mapcar 'length strings))))
    (while strings
      (let ((str (car strings)))
        (let ((padding (make-string (- (+ longest-act 3) (length str)) ? )))
          (insert str)
          (if (or one-col-each
                  (and (> (+ (length padding) ;
                             (current-column)
                             longest-act)
                          (window-width))
                       (cdr strings)))
              (insert "\n")
            (insert padding))))
      (setq strings (cdr strings))))
  (insert "\n"))

(defun mag-menu-draw (group)
  "Function used to draw actions, switches and parameters.

Returns the point before the actions part, if any."
  (let* ((switches (cdr (assoc 'switches group)))
         (arguments (cdr (assoc 'arguments group)))
         (actions (cdr (assoc 'actions group)))
         (p nil))
    (mag-menu-draw-switches switches)
    (mag-menu-draw-args arguments)
    (when actions (setq p (point-marker)))
    (mag-menu-draw-actions actions)
    (insert "\n")
    p))

(provide 'mag-menu)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; mag-menu.el ends here
