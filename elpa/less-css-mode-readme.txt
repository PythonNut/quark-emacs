This mode provides syntax highlighting for LESS CSS files, plus
optional support for compilation of .less files to .css files at
the time they are saved: use `less-css-compile-at-save' to enable
this.

Command line utility "lessc" is required if setting
`less-css-compile-at-save' to t.  To install "lessc" using the
Node.js package manager, run "npm install less"

Also make sure the "lessc" executable is in emacs' PATH, example:
(setq exec-path (cons (expand-file-name "~/.gem/ruby/1.8/bin") exec-path))
or customize `less-css-lessc-command' to point to your "lessc" executable.

We target lessc >= 1.4.0, and thus use the `--no-color' flag by
default.  You may want to adjust `less-css-lessc-options' for
compatibility with older versions.

`less-css-mode' is derived from `css-mode', and indentation of
nested blocks may not work correctly with versions of `css-mode'
other than that bundled with recent Emacs.

You can specify per-file values for `less-css-compile-at-save',
`less-css-output-file-name' or `less-css-output-directory' using a
variables header at the top of your .less file, e.g.:

// -*- less-css-compile-at-save: t; less-css-output-directory: "../css" -*-

Alternatively, you can use directory local variables to set the
default value of `less-css-output-directory' for your project.

In the case of files which are included in other .less files, you
may want to trigger the compilation of a "master" .less file on
save: you can accomplish this with `less-css-input-file-name',
which is probably best set using directory local variables.

If you don't need CSS output but would like to be warned of any
syntax errors in your .less source, consider using `flymake-less':
https://github.com/purcell/flymake-less

Credits

The original code for this mode was, in large part, written using
Anton Johansson's scss-mode as a template -- thanks Anton!
https://github.com/antonj
