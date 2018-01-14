emacs-config
============

My personal Emacs config!

*Featuring:*

 * *A lot of weird stuff you might not see in other configs.*
 * [`vim` emulation](https://bitbucket.org/lyro/evil/wiki/Home) including custom textobjects, motions, and operators.
 * Extensive version tracking backup system, even for buffers that do not visit files.
 * Persistence of:
   * File locations
   * File major modes
   * Registers
   * Minibuffer histories
   * Kill ring
   * Jump list
 * Widespread [`flx`](https://github.com/lewang/flx) integration:
   * [Helm](https://github.com/emacs-helm/helm) (fuzzy file finder, goto definition, etc.)
   * [Company](https://github.com/company-mode/company-mode) (code autocomplete)
   * [Icicles](https://www.emacswiki.org/emacs/Icicles) (minibuffer completion)
   * [Ivy](https://github.com/abo-abo/swiper) (minibuffer completion)
   * Isearch
 * Extremely agressive file autoloading, and fast startup times (~1.0s).
   * Partly thanks to [`straight.el`](https://github.com/raxod502/straight.el)!
 * An emphasis on correct code and robustness without the need for explicit configuration.
   * Config should automatically determine system capabilities and adjust.

Portability
===========

This config should be pretty portable, as long as your OS is UNIX-y. 
I currently run it on various Arch Linux, Fedora, and Ubuntu builds, as well as Raspberry Pis, Android phones, and Cygwin installations. 
Windows should work in theory??

One thing that it does _not_ support is older versions of Emacs. 
Right now, the minimum version is 25.1.

Supported Languages
===================

I actively work in the following languages:

  * LaTeX
  * Python/SAGEmath
  * C/C++
  * JS(X)/HTML/CSS
  * Octave
  * Elisp (duh)

So expect those languages to be reasonably well supported. 
Of course, this config supports other languages as well, but not as deeply. 
Drop by my [major mode support table](https://github.com/PythonNut/emacs-config/wiki/Major-Mode-project) to see what major modes this config supports. 
