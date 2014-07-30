;;; Compiled snippets and support files for `emacs-lisp-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'emacs-lisp-mode
  '(("dk" "(define-key ${1:keymap}$0 (kbd \"${2:C-c }\") '${3:command})\n" "define-key" nil nil nil nil nil nil)
     ("ban" ";;; ${1:$(make-string (string-width text) ?\\=)}\n;;; ${1:Title}\n;;; ${1:$(make-string (string-width text) ?\\=)}\n" "emacs-lisp-banner" nil nil nil nil nil nil)
     ("gsk" "(global-set-key (kbd \"${1:C-c }\") '${2:command})\n" "global-set-key" nil nil nil nil nil nil)))


;;; Do not edit! File generated at Tue May  6 17:54:00 2014
