Display completion items with its type and document.

If `tern-ac-on-dot' is non-nil (default), typing '.(dot)' invokes auto-complete with tern.
Calling the command `tern-ac-complete', you can invoke auto-complete manually.
This program does not provide an ac-source for arbitrary timing yet.

Installation:

Add following lines below the tern setup code.

(eval-after-load 'tern
   '(progn
      (require 'tern-auto-complete)
      (tern-ac-setup)))
