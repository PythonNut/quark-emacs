;;; Compiled snippets and support files for `f90-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'f90-mode
  '(("au" "automatic $0" "automatic" nil nil nil nil nil nil)
     ("bd" "block data $0" "block data" nil nil nil nil nil nil)
     ("c" "continue $0" "continue" nil nil nil nil nil nil)
     ("ch" "character $0" "character" nil nil nil nil nil nil)
     ("cx" "complex $0" "complex" nil nil nil nil nil nil)
     ("dc" "double complex $0" "double complex" nil nil nil nil nil nil)
     ("do" "do while (${1:condition})\n   $0\nend do" "do while (...) end do" nil nil nil nil nil nil)
     ("dp" "double precision $0" "double precision" nil nil nil nil nil nil)
     ("eq" "equivalence $0" "equivalence" nil nil nil nil nil nil)
     ("ib" "implicit byte $0" "implicit byte" nil nil nil nil nil nil)
     ("ic" "implicit complex $0" "implicit complex" nil nil nil nil nil nil)
     ("ich" "implicit character $0" "implicit character" nil nil nil nil nil nil)
     ("if" "if ( ${1:condition} ) then\n   $0\nend if" "if then end if" nil nil nil nil nil nil)
     ("ii" "implicit integer $0" "implicit integer" nil nil nil nil nil nil)
     ("il" "implicit logical $0" "implicit logical" nil nil nil nil nil nil)
     ("in" "implicit none" "implicit none" nil nil nil nil nil nil)
     ("inc" "include $0" "include" nil nil nil nil nil nil)
     ("intr" "intrinsic $0" "intrinsic" nil nil nil nil nil nil)
     ("ir" "implicit real $0" "implicit real" nil nil nil nil nil nil)
     ("l" "logical $0" "logical" nil nil nil nil nil nil)
     ("pa" "parameter $0" "parameter" nil nil nil nil nil nil)
     ("pr" "program ${1:name}\n  $0\nend program ${1:name}" "program ... end program ..." nil nil nil nil nil nil)
     ("re" "read (${1:*},${2:*}) $0" "read (*,*)" nil nil nil nil nil nil)
     ("st" "structure $0" "structure" nil nil nil nil nil nil)
     ("su" "subroutine $0" "subroutine" nil nil nil nil nil nil)
     ("wr" "write (${1:*},${2:*}) $0" "write (*,*)" nil nil nil nil nil nil)))


;;; Do not edit! File generated at Tue May  6 17:54:00 2014
