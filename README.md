Quark Emacs
===========

My personal Emacs config!

*Featuring:*

 * *A lot of weird stuff you might not see in other configs.*
 * Text editing built around `vim` emulation (using [evil](https://bitbucket.org/lyro/evil/wiki/Home)) including many custom textobjects, motions, and operators.
 * Comprehensive version tracking backup system, even for buffers that do not visit files.
 * Persistence of:
   * File major modes
   * File locations (thanks to saveplace)
   * Registers, minibuffer histories, kill ring, jump list (thanks to savehist)
   * Undo history (thanks to [undo-tree](https://elpa.gnu.org/packages/undo-tree.html))
 * Widespread [`flx`](https://github.com/lewang/flx) integration:
   * [Helm](https://github.com/emacs-helm/helm) (fuzzy file finder, goto definition, etc.)
   * [Company](https://github.com/company-mode/company-mode) (code autocomplete)
   * [Ivy](https://github.com/abo-abo/swiper) (minibuffer completion)
   * [Isearch](https://github.com/PythonNut/flx-isearch/)
 * Extremely aggressive file lazy loading, and fast startup times (~0.4s).
   * Made much easier by [`straight.el`](https://github.com/raxod502/straight.el)!
   * Packages are incrementally loaded during idle time immediately after init.
     By the time you open a file, most major packages will already have been loaded, but you won't notice the delay!
 * An emphasis on correct code and robustness without the need for explicit configuration.
   * Config should automatically determine system capabilities and adjust.

Portability
===========

This config should be pretty portable, as long as your OS is UNIX-y.
I currently run it on various Arch Linux, Fedora, macOS, as well as Android phones and Windows Subsystem for Linux.

One thing that it does _not_ support is older versions of Emacs.
Right now, the minimum version is 27.1.

Supported Languages
===================

I actively work in the following languages:

  * Elisp (of course)
  * LaTeX
  * Python
  * Julia
