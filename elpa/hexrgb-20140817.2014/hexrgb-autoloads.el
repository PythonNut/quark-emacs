;;; hexrgb-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (hexrgb-blue hexrgb-green hexrgb-red hexrgb-value
;;;;;;  hexrgb-saturation hexrgb-hue hexrgb-complement hexrgb-read-color
;;;;;;  hexrgb-canonicalize-defined-colors-flag) "hexrgb" "hexrgb.el"
;;;;;;  (21495 26339 843499 547000))
;;; Generated autoloads from hexrgb.el

(eval-and-compile (defun hexrgb-canonicalize-defined-colors (list) "Copy of LIST with color names canonicalized.\nLIST is a list of color names (strings).\nCanonical names are lowercase, with no whitespace.\nThere are no duplicate names." (let ((tail list) this new) (while tail (setq this (car tail) this (hexrgb-delete-whitespace-from-string (downcase this) 0 (length this))) (unless (member this new) (push this new)) (pop tail)) (nreverse new))) (defun hexrgb-delete-whitespace-from-string (string &optional from to) "Remove whitespace from substring of STRING from FROM to TO.\nIf FROM is nil, then start at the beginning of STRING (FROM = 0).\nIf TO is nil, then end at the end of STRING (TO = length of STRING).\nFROM and TO are zero-based indexes into STRING.\nCharacter FROM is affected (possibly deleted).  Character TO is not." (setq from (or from 0) to (or to (length string))) (with-temp-buffer (insert string) (goto-char (+ from (point-min))) (let ((count from) char) (while (and (not (eobp)) (< count to)) (setq char (char-after)) (if (memq char '(32 9 10)) (delete-char 1) (forward-char 1)) (setq count (1+ count))) (buffer-string)))))

(defconst hexrgb-defined-colors (eval-when-compile (and window-system (x-defined-colors))) "\
List of all supported colors.")

(defconst hexrgb-defined-colors-no-dups (eval-when-compile (and window-system (hexrgb-canonicalize-defined-colors (x-defined-colors)))) "\
List of all supported color names, with no duplicates.
Names are all lowercase, without any spaces.")

(defconst hexrgb-defined-colors-alist (eval-when-compile (and window-system (mapcar #'list (x-defined-colors)))) "\
Alist of all supported color names, for use in completion.
See also `hexrgb-defined-colors-no-dups-alist', which is the same
thing, but without any duplicates, such as \"light blue\" and
\"LightBlue\".")

(defconst hexrgb-defined-colors-no-dups-alist (eval-when-compile (and window-system (mapcar #'list (hexrgb-canonicalize-defined-colors (x-defined-colors))))) "\
Alist of all supported color names, with no duplicates, for completion.
Names are all lowercase, without any spaces.")

(defvar hexrgb-canonicalize-defined-colors-flag t "\
*Non-nil means remove duplicate color names.
Names are considered duplicates if they are the same when abstracting
from whitespace and letter case.")

(custom-autoload 'hexrgb-canonicalize-defined-colors-flag "hexrgb" t)

(autoload 'hexrgb-read-color "hexrgb" "\
Read a color name or hex RGB hexadecimal color value #RRRRGGGGBBBB.
Completion is available for color names, but not for RGB hex strings.
If you input an RGB hex string, it must have the form #XXXXXXXXXXXX or
XXXXXXXXXXXX, where each X is a hex digit.  The number of Xs must be a
multiple of 3, with the same number of Xs for each of red, green, and
blue.  The order is red, green, blue.

Color names that are normally considered equivalent are canonicalized:
They are lowercased, whitespace is removed, and duplicates are
eliminated.  E.g. \"LightBlue\" and \"light blue\" are both replaced
by \"lightblue\".  If you do not want this behavior, but want to
choose names that might contain whitespace or uppercase letters, then
customize option `hexrgb-canonicalize-defined-colors-flag' to nil.

In addition to standard color names and RGB hex values, the following
are available as color candidates.  In each case, the corresponding
color is used.

* `*copied foreground*'  - last copied foreground, if available
* `*copied background*'  - last copied background, if available
* `*mouse-2 foreground*' - foreground where you click `mouse-2'
* `*mouse-2 background*' - background where you click `mouse-2'
* `*point foreground*'   - foreground under the cursor
* `*point background*'   - background under the cursor

\(You can copy a color using eyedropper commands such as
`eyedrop-pick-foreground-at-mouse'.)

Optional arg PROMPT is the prompt - nil means use a default prompt.

Checks input to be sure it represents a valid color.  If not, raises
an error (but see exception for empty input with non-nil
ALLOW-EMPTY-NAME-P).

Interactively, or with optional arg CONVERT-TO-RGB-P non-nil, converts
an input color name to an RGB hex string.  Returns the RGB hex string.

Optional arg ALLOW-EMPTY-NAME-P controls what happens if you enter an
empty color name (that is, you just hit `RET').  If non-nil, then
`hexrgb-read-color' returns an empty color name, \"\".  If nil, then
it raises an error.  Calling programs must test for \"\" if
ALLOW-EMPTY-NAME-P is non-nil.  They can then perform an appropriate
action in case of empty input.

Interactively, or with non-nil MSGP, show color name in the echo area.

\(fn &optional PROMPT CONVERT-TO-RGB-P ALLOW-EMPTY-NAME-P MSGP)" t nil)

(autoload 'hexrgb-complement "hexrgb" "\
Return the color that is the complement of COLOR.
Non-interactively, non-nil optional arg MSG-P means show a message
with the complement.

\(fn COLOR &optional MSG-P)" t nil)

(autoload 'hexrgb-hue "hexrgb" "\
Return the hue component of COLOR, in range 0 to 1 inclusive.
COLOR is a color name or hex RGB string that starts with \"#\".

\(fn COLOR)" t nil)

(autoload 'hexrgb-saturation "hexrgb" "\
Return the saturation component of COLOR, in range 0 to 1 inclusive.
COLOR is a color name or hex RGB string that starts with \"#\".

\(fn COLOR)" t nil)

(autoload 'hexrgb-value "hexrgb" "\
Return the value component of COLOR, in range 0 to 1 inclusive.
COLOR is a color name or hex RGB string that starts with \"#\".

\(fn COLOR)" t nil)

(autoload 'hexrgb-red "hexrgb" "\
Return the red component of COLOR, in range 0 to 1 inclusive.
COLOR is a color name or hex RGB string that starts with \"#\".

\(fn COLOR)" t nil)

(autoload 'hexrgb-green "hexrgb" "\
Return the green component of COLOR, in range 0 to 1 inclusive.
COLOR is a color name or hex RGB string that starts with \"#\".

\(fn COLOR)" t nil)

(autoload 'hexrgb-blue "hexrgb" "\
Return the blue component of COLOR, in range 0 to 1 inclusive.
COLOR is a color name or hex RGB string that starts with \"#\".

\(fn COLOR)" t nil)

;;;***

;;;### (autoloads nil nil ("hexrgb-pkg.el") (21495 26339 866357 232000))

;;;***

(provide 'hexrgb-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; hexrgb-autoloads.el ends here
