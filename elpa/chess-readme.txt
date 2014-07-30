Welcome to Emacs Chess, a chess playing module for GNU Emacs.

chess.el is an Emacs Lisp library and several clients on top of the
underlying library functionality for performing various activities
related to the game of chess.

Type `M-x chess', and play chess against one of the default engine modules.

Type `C-u M-x chess' to select a specific engine.
You can play against various external chess computer programs, like
crafty, fruit, glaurung, gnuchess, phalanx, sjeng and stockfish.
There is also an Emacs based chess computer module (ai) which does not
require any external programs.  However, the internal AI is not very strong.

To play against another human on a different machine running GNU Emacs,
type `C-u M-x chess RET network RET'.
To play on one of the internet chess servers, type `M-x chess-ics'.

If you'd like to view or edit Portable Game Notation (PGN) files,
`chess-pgn-mode' provides a text-mode derived mode which can display the
chess position at point.

To improve your chessaility, `M-x chess-tutorial' provides a simple knight
movement exercise to get you started, and `M-x chess-puzzle' can be used
to solve puzzle collections in EPD or PGN format.

There are some different types of chessboard display modules available.
* A simple character based display (chess-plain).
* A more verbose ASCII chessboard (chess-ics1).
* A graphical chessboard display which uses images (chess-images).

The variable `chess-default-display' controls which display modules
are tried when a chessboard should be displayed.  By default, chess-images
is tried first.  If Emacs is not running in a graphical environment,
chess-ics1 is used instead.  To enable the chess-plain display module,
customize `chess-default-display' accordingly.

Once this is working, the next thing to do is to customize
`chess-default-modules'.  This is a list of functionality modules used
by chess.el to provide additional functionality.  You can enable or
disable modules so that Emacs Chess better suites your tastes.
Those modules in turn often have configuration variables, and
appropriate documentation at the top of the related file.

Emacs Chess is designed in a highly modular fashion, using loosely
coupled modules that respond to events on the chess board.  This
makes it very easy for programmers to add their own types of
displays, opponents, analysis programs, etc.  See the documentation
in chess-module.el to learn more.

Most people will probably also be interested in reading the top
of chess-display.el and chess-pgn.el, which describe the user
interface commands available in each of those buffer types.