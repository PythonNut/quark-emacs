evil-jumper is an add-on for evil-mode which replaces the
implementation of the jump list such that it mimics more closely
with Vim's behavior. Specifically, it will jump across buffer
boundaries and revive dead buffers if necessary. The jump list can
also be persisted to a file and restored between sessions.

Install:

(require 'evil-jumper)

Usage:

Requiring will automatically rebind C-o and C-i.
