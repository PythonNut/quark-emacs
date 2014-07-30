;;; doremi-frm-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (doremi-increment-color-component doremi-increment-face-fg-color
;;;;;;  doremi-all-faces-fg+ doremi-face-fg-color-name+ doremi-face-fg-hue-stepping-saturation+
;;;;;;  doremi-face-fg+ doremi-increment-face-bg-color doremi-all-faces-bg+
;;;;;;  doremi-face-bg-color-name+ doremi-face-bg-hue-stepping-saturation+
;;;;;;  doremi-face-bg+ doremi-undo-last-face-change doremi-toggle-wrap-color
;;;;;;  doremi-set-foreground-color doremi-increment-foreground-color
;;;;;;  doremi-all-frames-fg+ doremi-fg+ doremi-fg-hue-stepping-saturation+
;;;;;;  doremi-fg-value+ doremi-fg-saturation+ doremi-fg-hue+ doremi-fg-yellow+
;;;;;;  doremi-fg-magenta+ doremi-fg-cyan+ doremi-fg-blue+ doremi-fg-green+
;;;;;;  doremi-fg-red+ doremi-fg-color-name+ doremi-set-background-color
;;;;;;  doremi-increment-background-color doremi-all-frames-bg+ doremi-bg-hue-stepping-saturation+
;;;;;;  doremi-bg+ doremi-bg-value+ doremi-bg-saturation+ doremi-bg-hue+
;;;;;;  doremi-bg-yellow+ doremi-bg-magenta+ doremi-bg-cyan+ doremi-bg-blue+
;;;;;;  doremi-bg-green+ doremi-bg-red+ doremi-bg-color-name+ doremi-undo-last-frame-color-change
;;;;;;  doremi-frame-configs+ doremi-frame-vertically+ doremi-frame-horizontally+
;;;;;;  doremi-frame-height+ doremi-frame-width+ doremi-font+ doremi-frame-font-size+
;;;;;;  doremi-wrap-color-flag doremi-RGB-increment-factor doremi-push-frame-config-for-cmds-flag
;;;;;;  doremi-move-frame-wrap-within-display-flag doremi-frame-config-ring-size
;;;;;;  doremi-frame-commands) "doremi-frm" "doremi-frm.el" (21185
;;;;;;  65144 857927 460000))
;;; Generated autoloads from doremi-frm.el

(let ((loads (get 'doremi-frame-commands 'custom-loads))) (if (member '"doremi-frm" loads) nil (put 'doremi-frame-commands 'custom-loads (cons '"doremi-frm" loads))))

(defvar doremi-frame-config-ring-size 20 "\
*Maximum number of stored frame configurations.")

(custom-autoload 'doremi-frame-config-ring-size "doremi-frm" t)

(defvar doremi-move-frame-wrap-within-display-flag t "\
*Non-nil means wrap frame movements within the display.
Commands `doremi-frame-horizontally+' and `doremi-frame-vertically+'
then move the frame back onto the display when it moves off of it.
If nil, you can move the frame as far off the display as you like.")

(custom-autoload 'doremi-move-frame-wrap-within-display-flag "doremi-frm" t)

(defvar doremi-push-frame-config-for-cmds-flag nil "\
*Non-nil means commands that change frame config save it first.
This is done by advising all commands that change frame configuration
when library `doremi-frm.el' is loaded.")

(custom-autoload 'doremi-push-frame-config-for-cmds-flag "doremi-frm" t)

(defvar doremi-RGB-increment-factor 1 "\
*Factor to scale up RGB incrementing for some Do Re Mi functions.
Because RGB incrementing is by nature finer scale than HSV
incrementing, some Do Re Mi commands automatically scale up the
incrementing by this factor, so you need not iterate (cycle) so many
times to see an appreciable change.  When this is the case, it is
noted for the individual function.

The scale factor to use depends on how many hex digits there are in
your color representations.  A scale factor of 16 (and an input
increment of 1) means that, for each RGB component, it is the second
component digit from the right, not the rightmost, that is incremented
with each key press.  A factor of 256 means that the third digit from
the right cycles.  The default value is 1: no scaling.

If the digit that would be cycled is greater than the length of your
color components, then no incrementation occurs.  For example, if the
colors you use have the format #RRGGBB, so that each component has two
hex digits, then a factor of 256 is not appropriate, since it leaves
the component value unchanged (wraparound).  In that case, change the
value.

In general, 256 is good for colors represented as #RRRRGGGGBBBB, 16 is
good for #RRRGGGBBB, and 1 (no scaling) is appropriate for #RRGGBB.

What counts is the color representation you use, not what Emacs can
actually display for your screen.  On most platforms, Emacs can really
only display 8-bit color components, so #RRGGBB is the best it can do.
But you might well have defined your colors using the format
#RRRRGGGGBBBB.  That's OK, and it lets you see information reflecting
a more precise correspondance between RGB codes and color names, but
that extra precision is in fact ignored by Emacs and your display.

Personally, I use the longer format, ##RRRRGGGGBBBB, because I like to
see more info about the colors I use, even though my display cannot
really distinguish that many.  I also use libraries `hexrgb.el' and
`palette.el', and I convert color information between various formats
\(RGB, HSV, color names).  So I prefer to use the finer-grained
format, even though I can't see all the differences it provides.
Thus, I customize this option to 256.

The commands that use this option to scale up incrementing do so for
convenience.  You can always use other commands that perform no such
scaling.  For example, `doremi-bg+' scales RGB, but you can use
`doremi-increment-background-color' instead, for finer tuning.")

(custom-autoload 'doremi-RGB-increment-factor "doremi-frm" t)

(defvar doremi-wrap-color-flag t "\
*Non-nil means wrap color changes around past the max and min.
For example, if non-nil, a current color value has FFFF as the red
component, and the red component is incremented by 1, then the result
has a red component of 0000.  If nil, the same example yields FFFF,
because the red component is already at its maximum.")

(custom-autoload 'doremi-wrap-color-flag "doremi-frm" t)

(defalias 'doremi-font-size+ 'doremi-frame-font-size+)

(autoload 'doremi-frame-font-size+ "doremi-frm" "\
Change font size for FRAME by INCREMENT.
Interactively, INCREMENT is given by the prefix argument.
Optional FRAME parameter defaults to current frame.

\(fn &optional INCREMENT FRAME)" t nil)

(autoload 'doremi-font+ "doremi-frm" "\
Successively cycle among fonts, choosing by name.
Operates on the current frame. Cycled font list is (x-list-fonts \"*\").

\(fn)" t nil)

(autoload 'doremi-frame-width+ "doremi-frm" "\
Change width of current frame incrementally.
Width of frame FRAME is increased in increments of amount INCREMENT.

\(fn &optional INCREMENT FRAME)" t nil)

(autoload 'doremi-frame-height+ "doremi-frm" "\
Change height of current frame incrementally.
Height of frame FRAME is increased in increments of amount INCREMENT.

\(fn &optional INCREMENT FRAME)" t nil)

(autoload 'doremi-frame-horizontally+ "doremi-frm" "\
Move frame left/right incrementally.
Prefix arg is the INCREMENT to move (default value interactively: 10).
FRAME defaults to the selected frame.

Variable `doremi-move-frame-wrap-within-display-flag' controls whether
or not you can move the frame completely off the display. The default
behavior (value `t') is to wrap frame movement around the display.

\(fn &optional INCREMENT FRAME)" t nil)

(autoload 'doremi-frame-vertically+ "doremi-frm" "\
Move frame up/down incrementally.
Prefix arg is the INCREMENT to move (default value interactively: 10).
FRAME defaults to the selected frame.

Variable `doremi-move-frame-wrap-within-display-flag' controls whether or
not you can move the frame completely off the display. The default
behavior (value `t') is to wrap frame movement around the display.

\(fn &optional INCREMENT FRAME)" t nil)

(autoload 'doremi-frame-configs+ "doremi-frm" "\
Cycle among frame configurations recorded in `doremi-frame-config-ring'.

\(fn)" t nil)

(autoload 'doremi-undo-last-frame-color-change "doremi-frm" "\
Restore last frame color changed by `doremi-fg+' or `doremi-bg+'.
This acts as a toggle between the last two values.
Optional arg FRAME defaults to the selected frame.
  The last frame-color change must have been to FRAME, or the result
  will likely not be what you expect.
Note: This does not undo changes made by `doremi-all-frames-fg+' or
`doremi-all-frames-bg+'

\(fn &optional FRAME)" t nil)

(autoload 'doremi-bg-color-name+ "doremi-frm" "\
Successively cycle among background colors, choosing by name.
Operates on FRAME, which is the current frame when interactive.

\(fn &optional FRAME INTERACTIVE-P)" t nil)

(autoload 'doremi-bg-red+ "doremi-frm" "\
Change frame background red value incrementally.  See `doremi-bg+'.
Prefix arg is the INCREMENT to change.

\(fn &optional INCREMENT)" t nil)

(autoload 'doremi-bg-green+ "doremi-frm" "\
Change frame background green value incrementally.  See `doremi-bg+'.
Prefix arg is the INCREMENT to change.

\(fn &optional INCREMENT)" t nil)

(autoload 'doremi-bg-blue+ "doremi-frm" "\
Change frame background blue value incrementally.  See `doremi-bg+'.
Prefix arg is the INCREMENT to change.

\(fn &optional INCREMENT)" t nil)

(autoload 'doremi-bg-cyan+ "doremi-frm" "\
Change frame background cyan value incrementally.  See `doremi-bg+'.
Prefix arg is the INCREMENT to change.

\(fn &optional INCREMENT)" t nil)

(autoload 'doremi-bg-magenta+ "doremi-frm" "\
Change frame background green value incrementally.  See `doremi-bg+'.
Prefix arg is the INCREMENT to change.

\(fn &optional INCREMENT)" t nil)

(autoload 'doremi-bg-yellow+ "doremi-frm" "\
Change frame background blue value incrementally.  See `doremi-bg+'.
Prefix arg is the INCREMENT to change.

\(fn &optional INCREMENT)" t nil)

(autoload 'doremi-bg-hue+ "doremi-frm" "\
Change frame background hue incrementally.  See `doremi-bg+'.
Prefix arg is the INCREMENT to change.

\(fn &optional INCREMENT)" t nil)

(defalias 'doremi-bg-purity+ 'doremi-bg-saturation+)

(autoload 'doremi-bg-saturation+ "doremi-frm" "\
Change frame background color saturation incrementally.
Prefix arg is the INCREMENT to change.  See `doremi-bg+'.

\(fn &optional INCREMENT)" t nil)

(defalias 'doremi-bg-brightness+ 'doremi-bg-value+)

(autoload 'doremi-bg-value+ "doremi-frm" "\
Change frame background brightness (HSV \"value\") incrementally.
Prefix arg is the INCREMENT to change.  See `doremi-bg+'.

\(fn &optional INCREMENT)" t nil)

(autoload 'doremi-bg+ "doremi-frm" "\
Change FRAME's background color incrementally.
Optional arg FRAME defaults to the selected frame.

You are prompted for the color COMPONENT to increment/decrement (a
character):
  `r' - red
  `g' - green
  `b' - blue
  `h' - hue (basic color)
  `s' - saturation (purity)
  `v' - value (brightness)

  `R' - red, green, and blue, at the same time
  `H' - hue, saturation, and value, at the same time

`R' and `H' increment all components of the respective color spaces,
according to the value of INCREMENT.

You can at any time change, to increment/decrement a different color
component (r, g, b, h, s, v, R, or H).  For example, you can type `r'
and use the arrow keys or mouse wheel to change the red component,
then type `b' and use the arrows or wheel to change the blue
component, and so on, all in the same call to `doremi-bg+'.

Tip: To increment or decrement the cyan, magenta, or yellow component,
     just decrement or increment the red, green, or blue component,
     respectively.  CMY is just the opposite direction from RGB.

INCREMENT is the increment to change.  The value can be a number or a
list of 3 numbers.  The default value is 1.  You can use a prefix
argument to specify a number value.  Otherwise, you are prompted to
input the value.

If the value is a list of 3 numbers, they are used to increment the
individual components red, green, and blue, respectively, as well as
hue, saturation, and value, respectively.  If you change the
component(s) to increment, then the original input INCREMENT is
reapplied.

For example, if INCREMENT is (0.2 -0.5 1.1) and the initial COMPONENT
value is `R', then red is incremented by 0.2, green by -0.5, and blue
by 1.1.  If you then hit `h', hue is incremented by 0.2.  If you then
hit `b', blue is incremented by 1.1.

For RGB, INCREMENT is actually multiplied by
`doremi-RGB-increment-factor', for convenience.  If you need finer
control than that provides, use command
`doremi-increment-background-color' to refine the color.  If it seems
that no incrementing occurs, then reduce
`doremi-RGB-increment-factor'.

The initial color value is converted to a hexadecimal RGB (red, green,
blue) string that starts with \"#\".  The initial value is the current
background color of the selected frame.

If `eyedrop-picked-background' is non-nil and you use plain `C-u'
instead of a numeric prefix argument (or, non-interactively, PICKUP-P
is non-nil), then the frame background is first set to the value of
`eyedrop-picked-background'.  This happens only if library
`eyedropper.el' or `palette.el' is loaded.  This lets you pick up a
background color from somewhere, using `eyedrop-pick-background-at-*',
and then use that as the initial value for `doremi-bg+'.

Colors can be expressed in Emacs as color names or hex RGB strings.
Depending on your operating system, the RGB components for a given
Emacs color can have different numbers of hex digits.  For example, on
one system RGB component values might vary from 000 to FFF; on another
system they might vary from 0000 to FFFF.  Incrementing or
decrementing a given color's RGB spec makes it roll over when the
limit (say 000 or FFF) is reached.

As for all Do Re Mi incrementation commands, use
`doremi-boost-up-keys' and `doremi-boost-down-keys' for faster
incrementation.  The change is `doremi-boost-scale-factor' times
faster than for `doremi-up-keys' and `doremi-down-keys'.

\(fn COMPONENT &optional INCREMENT FRAME PICKUP-P INTERACTIVE-P)" t nil)

(autoload 'doremi-bg-hue-stepping-saturation+ "doremi-frm" "\
Increment frame background hue, stepping saturation down after each cycle.
Repeatedly increment hue until it reaches its maximum.  Then increment
saturation once.  Then repeatedly increment hue again - and so on.

You can think of this as moving along a row of the hue x saturation
color plane, then down to the next row and across, and so on.

See `doremi-bg+' for more info (e.g. other args).

\(fn &optional INCREMENT FRAME PICKUP-P INTERACTIVE-P)" t nil)

(autoload 'doremi-all-frames-bg+ "doremi-frm" "\
Change background color of all visible frames incrementally.
You are prompted for the color COMPONENT to increment.
Prefix arg is the INCREMENT to change; the default value is 1.

See command `doremi-bg+'.  This command behaves similarly, but it
changes the background color of all frames, not just one frame.

NOTE: You cannot use `C-g' to cancel and revert changes you make using
this command, and you cannot use `doremi-undo-last-frame-color-change'
to undo changes.  (There is no single initial color to revert to,
since multiple frames are affected.)

For RGB, INCREMENT is multiplied by `doremi-RGB-increment-factor', for
convenience.  If you need finer control than that provides, use
command `doremi-increment-background-color' to refine the color.  If
it seems that no incrementing occurs, then reduce
`doremi-RGB-increment-factor'.

Option `doremi-wrap-color-flag' is bound to nil during this command,
which means that an individual color change stops when the limit is
reached.

\(fn COMPONENT INCREMENT)" t nil)

(autoload 'doremi-increment-background-color "doremi-frm" "\
Change frame background color by INCREMENT of color COMPONENT.
You are prompted for the color COMPONENT to increment/decrement.
COMPONENT and INCREMENT are as for `doremi-increment-color'.
Optional arg FRAME defaults to the selected frame.  See `doremi-bg+'.

\(fn COMPONENT INCREMENT &optional FRAME INTERACTIVE-P)" t nil)

(autoload 'doremi-set-background-color "doremi-frm" "\
Set the background color of the FRAME to COLOR-NAME.
When called interactively, prompt for the name of the color to use.
To get the frame's current background color, use `frame-parameters'.
This is the same as `set-background-color', except that this accepts a
FRAME parameter.

\(fn COLOR-NAME &optional FRAME INTERACTIVE-P)" t nil)

(autoload 'doremi-fg-color-name+ "doremi-frm" "\
Successively cycle among foreground colors, choosing by name.
Operates on FRAME, which is the current frame when interactive.

\(fn &optional FRAME INTERACTIVE-P)" t nil)

(autoload 'doremi-fg-red+ "doremi-frm" "\
Change frame foreground red value incrementally.  See `doremi-fg+'.
Prefix arg is the INCREMENT to change.

\(fn &optional INCREMENT)" t nil)

(autoload 'doremi-fg-green+ "doremi-frm" "\
Change frame foreground green value incrementally.  See `doremi-fg+'.
Prefix arg is the INCREMENT to change.

\(fn &optional INCREMENT)" t nil)

(autoload 'doremi-fg-blue+ "doremi-frm" "\
Change frame foreground blue value incrementally.  See `doremi-fg+'.
Prefix arg is the INCREMENT to change.

\(fn &optional INCREMENT)" t nil)

(autoload 'doremi-fg-cyan+ "doremi-frm" "\
Change frame foreground cyan value incrementally.  See `doremi-fg+'.
Prefix arg is the INCREMENT to change.

\(fn &optional INCREMENT)" t nil)

(autoload 'doremi-fg-magenta+ "doremi-frm" "\
Change frame foreground green value incrementally.  See `doremi-fg+'.
Prefix arg is the INCREMENT to change.

\(fn &optional INCREMENT)" t nil)

(autoload 'doremi-fg-yellow+ "doremi-frm" "\
Change frame foreground blue value incrementally.  See `doremi-fg+'.
Prefix arg is the INCREMENT to change.

\(fn &optional INCREMENT)" t nil)

(autoload 'doremi-fg-hue+ "doremi-frm" "\
Change frame foreground hue incrementally.  See `doremi-fg+'.
Prefix arg is the INCREMENT to change.

\(fn &optional INCREMENT)" t nil)

(defalias 'doremi-fg-purity+ 'doremi-fg-saturation+)

(autoload 'doremi-fg-saturation+ "doremi-frm" "\
Change frame foreground color saturation incrementally.
See `doremi-fg+'.  Prefix arg is the INCREMENT to change.

\(fn &optional INCREMENT)" t nil)

(defalias 'doremi-fg-brightness+ 'doremi-fg-value+)

(autoload 'doremi-fg-value+ "doremi-frm" "\
Change frame foreground brightness (HSV \"value\") incrementally.
See `doremi-fg+'.  Prefix arg is the INCREMENT to change.

\(fn &optional INCREMENT)" t nil)

(autoload 'doremi-fg-hue-stepping-saturation+ "doremi-frm" "\
Increment frame foreground hue, stepping saturation down after each cycle.
See `doremi-bg-hue-stepping-saturation+'.
`doremi-fg-hue-stepping-saturation+' is the same, with \"foreground\"
substituted for \"background\".

\(fn &optional INCREMENT FRAME PICKUP-P INTERACTIVE-P)" t nil)

(autoload 'doremi-fg+ "doremi-frm" "\
Change FRAME's foreground color incrementally.
See `doremi-bg+'; `doremi-fg+' is the same, with \"foreground\"
substituted for \"background\".

\(fn COMPONENT &optional INCREMENT FRAME PICKUP-P INTERACTIVE-P)" t nil)

(autoload 'doremi-all-frames-fg+ "doremi-frm" "\
Change foreground color of all visible frames incrementally.
You are prompted for the color COMPONENT to increment.
Prefix arg is the INCREMENT to change; the default value is 1.

See command `doremi-fg+'.  This command behaves similarly, but it
changes the foreground color of all frames, not just one frame.

NOTE: You cannot use `C-g' to cancel and revert changes you make using
this command, and you cannot use `doremi-undo-last-frame-color-change'
to undo changes.  (There is no single initial color to revert to,
since multiple frames are affected.)

For RGB, INCREMENT is multiplied by `doremi-RGB-increment-factor', for
convenience.  If you need finer control than this, use command
`doremi-increment-foreground-color' to refine the color.  If it seems
that no incrementing occurs, then reduce
`doremi-RGB-increment-factor'.

Option `doremi-wrap-color-flag' is bound to nil during this command,
which means that an individual color change stops when the limit is
reached.

\(fn COMPONENT INCREMENT)" t nil)

(autoload 'doremi-increment-foreground-color "doremi-frm" "\
Change foreground color of FRAME by INCREMENT of color COMPONENT.
You are prompted for the color COMPONENT to increment/decrement.
COMPONENT and INCREMENT are as for `doremi-increment-color'.
Optional arg FRAME defaults to the selected frame.  See `doremi-bg+'.

\(fn COMPONENT INCREMENT &optional FRAME INTERACTIVE-P)" t nil)

(autoload 'doremi-set-foreground-color "doremi-frm" "\
Set the foreground color of the FRAME to COLOR-NAME.
When called interactively, prompt for the name of the color to use.
To get the frame's current foreground color, use `frame-parameters'.
This is the same as `set-foreground-color', except that this accepts a
FRAME parameter.

\(fn COLOR-NAME &optional FRAME INTERACTIVE-P)" t nil)

(defalias 'toggle-doremi-wrap-color 'doremi-toggle-wrap-color)

(autoload 'doremi-toggle-wrap-color "doremi-frm" "\
Toggle value of `doremi-wrap-color-flag'.

\(fn)" t nil)

(autoload 'doremi-undo-last-face-change "doremi-frm" "\
Return last face changed by `doremi-face-*' to its previous value.
This acts as a toggle between the last two values of the face.
Note: This does not undo changes made by `doremi-all-faces-fg+' or
`doremi-all-faces-bg+'.

\(fn)" t nil)

(autoload 'doremi-face-bg+ "doremi-frm" "\
Change background color of FACE incrementally.
The color is changed on all frames.
You are prompted for the FACE, the color COMPONENT to increment.
Unless you use a prefix argument, you are prompted for the INCREMENT.

See command `doremi-bg+'.  This command behaves the same, except that
it is the background color of FACE that is changed, not the frame
background color.

For RGB, INCREMENT is actually multiplied by
`doremi-RGB-increment-factor', for convenience.  If you need finer
control than this, use command `doremi-increment-face-bg-color' to
refine the color.  If it seems that no incrementing occurs, then
reduce `doremi-RGB-increment-factor'.

If `eyedrop-picked-background' is non-nil and you use plain `C-u'
instead of a numeric prefix argument (or, non-interactively, PICKUP-P
is non-nil), then the face background is first set to the value of
`eyedrop-picked-background'.  This happens only if library
`eyedropper.el' or `palette.el' is loaded.  This lets you pick up a
background color from somewhere, using `eyedrop-pick-background-at-*',
and then use that as the initial value for `doremi-face-bg+'.

\(fn FACE COMPONENT &optional INCREMENT PICKUP-P INTERACTIVE-P)" t nil)

(autoload 'doremi-face-bg-hue-stepping-saturation+ "doremi-frm" "\
Increment FACE background hue, stepping saturation down after each cycle.

See command `doremi-bg-hue-stepping-saturation+'.  This command
behaves the same, except that it is the background color of FACE that
is changed, not the frame background color.
See `doremi-face-bg+' for more info (e.g. other args).

\(fn FACE &optional INCREMENT PICKUP-P INTERACTIVE-P)" t nil)

(autoload 'doremi-face-bg-color-name+ "doremi-frm" "\
Successively cycle among background colors for FACE, choosing by name.
The color is changed on all frames.
You are prompted for the FACE.

See command `doremi-bg-color-name+'.  This command behaves the same,
except that it is the background color of FACE that is changed, not
the frame background color.

\(fn FACE &optional INTERACTIVE-P)" t nil)

(autoload 'doremi-all-faces-bg+ "doremi-frm" "\
Change background color of all faces incrementally, for all frames.
See command `doremi-face-bg+'.  This command behaves similarly, but it
is the background color of all faces that is changed, not one face.

For RGB, INCREMENT is actually multiplied by
`doremi-RGB-increment-factor'.  If you need finer control than this,
use command `doremi-increment-face-bg-color' to refine the color.  If
it seems that no incrementing occurs, then reduce
`doremi-RGB-increment-factor'.

Option `doremi-wrap-color-flag' is bound to nil during this command.

NOTE: You cannot use `C-g' to cancel and revert changes you make using
this command, and you cannot use `doremi-undo-last-face-change' to
undo changes.  (There is no single initial color to revert to, since
multiple faces are affected.)

\(fn COMPONENT INCREMENT)" t nil)

(autoload 'doremi-increment-face-bg-color "doremi-frm" "\
Change background color of FACE by INCREMENT of COMPONENT.
The color is changed on all frames.
You are prompted for the FACE and the color COMPONENT to increment.
COMPONENT and INCREMENT are as for `doremi-increment-color'.

\(fn FACE COMPONENT INCREMENT)" t nil)

(autoload 'doremi-face-fg+ "doremi-frm" "\
Change foreground color of FACE incrementally.
See `doremi-face-bg+'; `doremi-face-fg+' is the same, with
\"foreground\" substituted for \"background\".

\(fn FACE COMPONENT &optional INCREMENT PICKUP-P INTERACTIVE-P)" t nil)

(autoload 'doremi-face-fg-hue-stepping-saturation+ "doremi-frm" "\
Increment FACE background hue, stepping saturation down after each cycle.
See `doremi-face-bg+' for info about the other args.

\(fn FACE &optional INCREMENT PICKUP-P INTERACTIVE-P)" t nil)

(autoload 'doremi-face-fg-color-name+ "doremi-frm" "\
Successively cycle among foreground colors for FACE, choosing by name.
The color is changed on all frames.
You are prompted for the FACE.

See command `doremi-fg-color-name+'.  This command behaves the same,
except that it is the foreground color of FACE that is changed, not
the frame foreground color.

\(fn FACE &optional INTERACTIVE-P)" t nil)

(autoload 'doremi-all-faces-fg+ "doremi-frm" "\
Change foreground color of all faces incrementally, for all frames.
See command `doremi-face-fg+'.  This command behaves similarly, but it
is the foreground color of all faces that is changed, not one face.

For RGB, INCREMENT is actually multiplied by
`doremi-RGB-increment-factor'.  If you need finer control than this,
use command `doremi-increment-face-fg-color' to refine the color.  If
it seems that no incrementing occurs, then reduce
`doremi-RGB-increment-factor'.

Option `doremi-wrap-color-flag' is bound to nil during this command.

NOTE: You cannot use `C-g' to cancel and revert changes you make using
this command, and you cannot use `doremi-undo-last-face-change' to
undo changes.  (There is no single initial color to revert to, since
multiple faces are affected.)

\(fn COMPONENT INCREMENT)" t nil)

(autoload 'doremi-increment-face-fg-color "doremi-frm" "\
Change foreground color of FACE by INCREMENT of COMPONENT.
The color is changed on all frames.
You are prompted for the FACE and the color COMPONENT to increment.
COMPONENT and INCREMENT are as for `doremi-increment-color'.

\(fn FACE COMPONENT INCREMENT)" t nil)

(autoload 'doremi-increment-color-component "doremi-frm" "\
Increase COMPONENT (RGB or HSV) of COLOR by INCREMENT.
Returns a hexadecimal RGB code (a string) for the new color, of the
form #RRRRGGGGBBBB (RRRR: red, GGGG: green, BBBB: blue).

COMPONENT is the color component to increment (a character):
  `r' - red
  `g' - green
  `b' - blue
  `h' - hue (basic color)
  `s' - saturation (purity)
  `v' - value (brightness)
  The default is `h' (hue).
COLOR is a string representing a color.  It can be a color name or a
  hexadecimal RGB string of the form #RRRRGGGGBBBB.
INCREMENT is the increment to increase the value component of COLOR.

\(fn COMPONENT COLOR INCREMENT)" t nil)

;;;***

;;;### (autoloads nil nil ("doremi-frm-pkg.el") (21185 65144 871000
;;;;;;  250000))

;;;***

(provide 'doremi-frm-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; doremi-frm-autoloads.el ends here
