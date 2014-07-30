An attempt to get context sensitive Javascript completion in Emacs.
Basic completions are obtained by parsing Javascript code with
Js2-mode's parser.

Installation

Easiest way to get ac-js2 is to install it from MELPA. You may need
this snippet

`(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)'

if you don't have it already to fetch packages from MELPA.

Enable ac-js2 in js2-mode as follows:

(add-hook 'js2-mode-hook 'ac-js2-mode)

Ac-js2 does not require auto-complete mode but I suggest you grab
it anyway as ac-js2 is designed to work with a completion frontend.
Support for Company mode is on its way.

For more comprehensive completions you can opt to evaluate the code
for candidates. A browser needs to be connected to Emacs for the
evaluation completions to work. Put this in your init.el file.

`(setq ac-js2-evaluate-calls t)'

To add completions for external libraries add something like this:

(add-to-list 'ac-js2-external-libraries "path/to/lib/library.js")

Then connect a browser to Emacs by calling `(run-skewer)'. You may
need to save the buffer for completions to start.

If auto-complete mode is installed on your system then completions
should start showing up otherwise use `completion-at-point'.

Note: library completions will only work if `ac-js2-evaluate-calls'
is set and a browser is connected to Emacs.

Bonus: M-. is bound to `ac-js2-jump-to-definition' which will jump
to Javascript definitions found in the same buffer. Given the
following proprety reference:

foo.bar.baz();

placing the cursor on `foo', `bar' or `baz' and executing M-. will
take you straight to their respective definitions. Use M-, to jump
back to where you were. Also works for object literals.

Recently added `ac-js2-expand-function' that will expand a function's
parameters bound to `C-c C-c`. Expansion will only work if the cursor
is after the function.

If you have any issues or suggestions please create an issue on Github:
https://github.com/ScottyB/ac-js2
