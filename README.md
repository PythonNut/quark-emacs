emacs-config
============

My personal emacs config.

*Featuring:*

 * `vim` emulation including custom textobjects, motions, and operators
 * Extensive version tracking backup system
 * Persistence of:
   * File locations
   * File major modes
   * Registers
   * Minibuffer histories
   * Kill ring
 * Everything that can be made fuzzy (Sublime style) has been made fuzzy, including:
   * Helm (fuzzy file finder, goto definition, etc.)
   * Company (code autocomplete)
   * Icicles (minibuffer completion)
   * Ivy (minibuffer completion)
   * Isearch
 * Extremely agressive file autoloading, and fast startup times (~0.4s)
 * An emphasis on correct code and robustness

Portability
===========

This config should be pretty portable, as long as your OS is UNIX-y. I currently run it on various Arch Linux, Fedora, and Ubuntu builds, as well as Raspberry Pis and Android phones.

One thing that it most certainly does _not_ support is older Emacs versions. Right now, the minimum version is 24.4, and yes, that has bit me a few times. However, given that I arrived on the Emacs scene only just before the release of 24.4, I have a lack of motivation to backport to older Emacsen.

Supported Languages
===================

I actively work in the following languages:

  * Python/SAGE
  * C++
  * JS/HTML/CSS
  * Octave
  * Elisp (duh)

So expect those languages to be well supported. Of course, this config supports other languages as well. Drop by my [major mode support table](https://github.com/PythonNut/emacs-config/wiki/Major-Mode-project) to see what languages this config supports. I think you'll be surprised.
