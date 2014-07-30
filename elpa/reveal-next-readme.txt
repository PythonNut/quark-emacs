   Progressively reveal text after the cursor as you move it.

You can use this during a presentation, such as in a class, to
progressively show more of the text in a buffer.  As you move the
cursor in any way, more text is revealed (or hidden, if moving
backward).

There are two levels: show at least a line at a time or show at
least a char at a time.  User option `reveal-next-char-level' sets
the level, but you can also switch levels using command
`reveal-next-char/line' or `reveal-next-cycle'.  The latter command
cycles among line-level, char-level, and off.


User options defined here:

   `reveal-next-char-level'.

Commands defined here:

   `reveal-next-char/line', `reveal-next-cycle',
   `reveal-next-mode'.

Non-interactive functions defined here:

   `reveal-next-update'.

Internal variables defined here:

   `reveal-next-overlay'.


This library was inspired by this Stack Overflow question:
http://stackoverflow.com/q/23677844/729907
