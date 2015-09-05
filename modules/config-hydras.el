;; -*- lexical-binding: t -*-

(eval-when-compile
  (with-demoted-errors
    (require 'hydra)))

(defun my/smart-registers-and-rectangles ()
  (interactive)
  (unless (fboundp 'hydra/registers-and-rectangles/body)
    (require 'hydra)
    (defhydra hydra/registers-and-rectangles (:color blue :hint nil :idle 0.3)
      "
REGISTER^^^^                     │   RECTANGLE
_SPC_ point →    _i_ insert ←    │   _c_ clear    _r_ copy-to-register
_f_ frameset →   _U_ undo ←      │   _d_ delete   _M-w_ copy-as-kill
_n_ number →     _u_ undo →      │   _k_ kill     _t_ string
_x_ copy →       _+_ increment   │   _o_ open     _N_ number-lines
_w_ windows →  ^^                │   _y_ yank"
      ("U"     undo-tree-restore-state-from-register)
      ("u"     undo-tree-save-state-to-register)
      ("C-@"   point-to-register)
      ("SPC"   point-to-register)
      ("+"     increment-register :color red)
      ("N"     rectangle-number-lines)
      ("b"     bookmark-jump)
      ("c"     clear-rectangle)
      ("d"     delete-rectangle)
      ("f"     frameset-to-register)
      ("g"     insert-register)
      ("i"     insert-register)
      ("j"     jump-to-register)
      ("k"     kill-rectangle)
      ("l"     bookmark-bmenu-list)
      ("m"     bookmark-set)
      ("n"     number-to-register)
      ("o"     open-rectangle)
      ("r"     copy-rectangle-to-register)
      ("s"     copy-to-register)
      ("t"     string-rectangle)
      ("w"     window-configuration-to-register)
      ("x"     copy-to-register)
      ("y"     yank-rectangle)
      ("C-SPC" point-to-register)
      ("M-w"   copy-rectangle-as-kill)
      ("ESC w" copy-rectangle-as-kill)))
  (hydra/registers-and-rectangles/body))

(global-set-key (kbd "C-x r") nil)
(global-set-key (kbd "C-x r") #'my/smart-registers-and-rectangles)

(defun my/smart-icicle-search-map ()
  (interactive)
  (unless (fboundp 'hydra/icicle-search-map/body)
    (require 'hydra)
    (defhydra hydra/icicle-search-map (:color blue :hint nil :idle 0.3)
      "
_f_   file     _D_ defs-full    _,_  tags                _g_ grep-saved-file-cands
_b_   buffer   _j_ bookmark     _x_ xml-element          _X_ xml-element-text-node
_l_   lines    _k_ keywords     _J_ bookmarks-together   _o_ occur
_w_   word     _p_ paragraphs   _c_ char-property        _i_ imenu
_d_   defs     _s_ sentences    _O_ overlay-property     _I_ imenu-full
_C-l_ pages    _t_ thing        _T_ text-property"
      ("C-l" icicle-search-pages)
      (","   icicle-tags-search)
      ("D"   icicle-search-defs-full)
      ("I"   icicle-imenu-full)
      ("J"   icicle-search-bookmarks-together)
      ("O"   icicle-search-overlay-property)
      ("T"   icicle-search-text-property)
      ("X"   icicle-search-xml-element-text-node)
      ("b"   icicle-search-buffer)
      ("c"   icicle-search-char-property)
      ("d"   icicle-search-defs)
      ("f"   icicle-search-file)
      ("g"   icicle-grep-saved-file-candidates)
      ("i"   icicle-imenu)
      ("j"   icicle-search-bookmark)
      ("k"   icicle-search-keywords)
      ("l"   icicle-search-lines)
      ("o"   icicle-occur)
      ("p"   icicle-search-paragraphs)
      ("s"   icicle-search-sentences)
      ("t"   icicle-search-thing)
      ("w"   icicle-search-word)
      ("x"   icicle-search-xml-element)))

  (hydra/icicle-search-map/body))

(defun my/smart-isearch-map ()
  (interactive)
  (unless (fboundp 'hydra/isearch-map/body)
    (require 'hydra)
    (defhydra hydra/isearch-map (:color blue :hint nil :idle 0.3)
      "
Isearch^^          │   Highlight
_._ → symbol @ ·   │   _h._ symbol @ ·   _hu_ unhiglight regex
___ → symbol       │   _hl_ lines        _hf_ HL find patterns
_w_ → word         │   _hp_ phrase       _hw_ HL write patterns
_o_ occur          │   _hr_ regex        _M-s_ Icicle search ..."
      ("."   isearch-forward-symbol-at-point)
      ("-"   isearch-forward-symbol)
      ("o"   helm-occur)
      ("w"   isearch-forward-word)
      ("h."  highlight-symbol-at-point)
      ("hf"  hi-lock-find-patterns)
      ("hl"  highlight-lines-matching-regexp)
      ("hp"  highlight-phrase)
      ("hr"  highlight-regexp)
      ("hu"  unhighlight-regexp)
      ("hw"  hi-lock-write-interactive-patterns)
      ("M-s" my/smart-icicle-search-map)))

  (hydra/isearch-map/body))

(global-set-key (kbd "M-s") #'my/smart-isearch-map)

(defun my/smart-frame-tools ()
  (interactive)
  (unless (fboundp 'hydra/frame-tools/body)
    (require 'hydra)
    (defhydra hydra/frame-tools (:color blue :hint nil :idle 0.3)
      "
Frame^^              │   Other Frame
_0_  delete          │   _f_ find file         _f_ find file read only
_1_  delete others   │   _d_ dired             _m_ compose mail
_2_  make            │   _b_ switch buffer     _._ find tag
_o_  other           │   _C-o_ display buffer"
      ("C-f" find-file-other-frame)
      ("C-o" display-buffer-other-frame)
      ("."   find-tag-other-frame)
      ("0"   delete-frame)
      ("1"   delete-other-frames)
      ("2"   make-frame-command)
      ("b"   switch-to-buffer-other-frame)
      ("d"   dired-other-frame)
      ("f"   find-file-other-frame)
      ("m"   compose-mail-other-frame)
      ("o"   other-frame)
      ("r"   find-file-read-only-other-frame)))

  (hydra/frame-tools/body))

(global-set-key (kbd "C-x 5") #'my/smart-frame-tools)

(defun my/smart-window-tools ()
  (interactive)
  (unless (fboundp 'hydra/window-tools/body)
    (require 'hydra)
    (defhydra hydra/window-tools (:color blue :hint nil :idle 0.3)
      "
Window^^                     │   Other window
_0_  kill buffer & window    │   _f_ find file       _a_ add changelog entry
_c_  clone indirect          │   _d_ dired           _r_ find file read only
_C-j_  dired jump            │   _b_ switch buffer   _m_ compose mail
_C-o_  display buffer        │   _._ find tag"
      ("C-f" find-file-other-window)
      ("C-j" dired-jump-other-window)
      ("C-o" display-buffer)
      ("."   find-tag-other-window)
      ("0"   kill-buffer-and-window)
      ("a"   add-change-log-entry-other-window)
      ("b"   switch-to-buffer-other-window)
      ("c"   clone-indirect-buffer-other-window)
      ("d"   dired-other-window)
      ("f"   find-file-other-window)
      ("m"   compose-mail-other-window)
      ("r"   find-file-read-only-other-window)))

  (hydra/window-tools/body))

(global-set-key (kbd "C-x 4") #'my/smart-window-tools)

(provide 'config-hydras)
