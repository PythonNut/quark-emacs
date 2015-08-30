(eval-when-compile
  (with-demoted-errors
    (require 'cl-lib)
    (require 'cl)
    (require 'hydra)
    (require 'key-chord)
    (require 'evil)
    (require 'diminish)
    (require 'evil-easymotion)
    (require 'config-modes)))

(require 'config-scroll)
(require 'config-line-numbers)
(require 'config-avy-easymotion)
(require 'config-undo)

(require 'config-smartparens)
(require 'config-rainbow-delimiters)


(with-eval-after-load 'multiple-cursors
  (define-key mc/keymap (kbd "<return>") nil)
  (define-key mc/keymap (kbd "C-c <return>") 'multiple-cursors-mode))

(cl-macrolet
  ((autoload-multiple-cursors (func)
     `(autoload ,func "multiple-cursors")))

  (with-no-warnings
    (generate-calls-single autoload-multiple-cursors
      (
        #'mc/mark-lines
        #'mc/mark-next-lines
        #'mc/mark-previous-lines
        #'mc/unmark-next-like-this
        #'mc/unmark-previous-like-this
        #'mc/skip-to-previous-like-this
        #'mc/mark-all-like-this
        #'mc/mark-all-words-like-this
        #'mc/mark-all-symbols-like-this
        #'mc/mark-all-in-region
        #'mc/mark-all-in-region-regexp
        #'mc/mark-more-like-this-extended
        #'mc/mmlte--up
        #'mc/mmlte--down
        #'mc/mmlte--left
        #'mc/mmlte--right
        #'mc/mark-all-like-this-dwim
        #'mc/mark-all-dwim
        #'mc/mark-all-like-this-in-defun
        #'mc/mark-all-words-like-this-in-defun
        #'mc/mark-all-symbols-like-this-in-defun
        #'mc/add-cursor-on-click
        #'mc/mark-sgml-tag-pair
        #'mc/mark-pop
        #'set-rectangular-region-anchor
        #'rrm/switch-to-multiple-cursors
        #'mc/insert-numbers
        #'mc/reverse-regions
        #'mc/sort-regions
        #'hum/keyboard-quit
        #'mc-hide-unmatched-lines-mode
        ))))

(global-set-key (kbd "C-c l") #'mc/edit-lines)
(global-set-key (kbd "C-c a") #'mc/mark-all-like-this-dwim)
(global-set-key (kbd "C-c s") #'mc/mark-all-in-region)
(global-set-key (kbd "C-c M") #'mc/mark-pop)
(global-set-key (kbd "C->") #'mc/mark-next-like-this)
(global-set-key (kbd "C-<") #'mc/mark-previous-like-this)

;; directional window movement
(global-set-key (kbd "<M-left>")  'windmove-left)
(global-set-key (kbd "<M-right>") 'windmove-right)
(global-set-key (kbd "<M-up>")    'windmove-up)
(global-set-key (kbd "<M-down>")  'windmove-down)

(defun framemove-onetime-setup ()
  (windmove-default-keybindings 'meta)
  (require 'framemove)
  (remove-hook 'before-make-frame-hook
    #'framemove-onetime-setup))

;; directional frame movement too
(add-hook 'emacs-startup-hook
  (lambda ()
    (add-hook 'before-make-frame-hook
      #'framemove-onetime-setup)))

(with-eval-after-load 'framemove
  (setq framemove-hook-into-windmove t))

(global-set-key (kbd "C-.") #'er/expand-region)

(defun pop-window-into-frame ()
  (interactive)
  (let ((buffer (current-buffer)))
    (unless (one-window-p)
      (delete-window))
    (display-buffer-pop-up-frame buffer nil)))

;;; ====================================
;;; iflib - switch buffers alt-tab style
;;; ====================================
(lexical-let ((iflipb-running-p))
  (with-eval-after-load 'iflipb
    (setq
      iflipb-ignore-buffers '("^ " "^*helm" "^*Compile" "^*Quail")
      iflipb-wrap-around 't)

    (defun iflipb-first-iflipb-buffer-switch-command ()
      "Determines whether this is the first invocation of
  iflipb-next-buffer or iflipb-previous-buffer this round."
      iflipb-running-p))

  (defun iflipb-smart-buffer ()
    (unless (fboundp 'iflipb-hydra/body)
      (require 'hydra)
      (defhydra iflipb-hydra
        (:pre (setq hydra-is-helpful nil)
          :post (setq hydra-is-helpful t))
        ("<C-tab>"
          (call-interactively #'iflipb-next-buffer))
        ("TAB"
          (call-interactively #'iflipb-next-buffer))
        ("<C-S-iso-lefttab>"
          (call-interactively #'iflipb-previous-buffer))
        ("<backtab>"
          (call-interactively #'iflipb-previous-buffer))))
    (iflipb-hydra/body))

  (defun iflipb-next-buffer-smart ()
    "A `hydra' enabled next-buffer"
    (interactive)
    (let ((iflipb-running-p t))
      (call-interactively #'iflipb-next-buffer))
    (iflipb-smart-buffer))

  (defun iflipb-previous-buffer-smart ()
    "A `hydra' enabled previous-buffer"
    (interactive)
    (let ((iflipb-running-p t))
      (call-interactively #'iflipb-previous-buffer))
    (iflipb-smart-buffer)))

(global-set-key (kbd "<C-tab>") 'iflipb-next-buffer-smart)
(global-set-key (kbd "C-S-<iso-lefttab>") 'iflipb-previous-buffer-smart)

(global-set-key (kbd "C-c TAB") 'iflipb-next-buffer-smart)
(global-set-key (kbd "C-c <backtab>") 'iflipb-previous-buffer-smart)

;; also allow undo/redo on window configs
(add-hook 'window-configuration-change-hook #'winner-mode)

(defun vhl-onetime-setup ()
  (require 'volatile-highlights)
  (remove-hook 'first-change-hook #'vhl-onetime-setup))

(add-hook 'emacs-startup-hook
  (lambda ()
    (add-hook 'first-change-hook #'vhl-onetime-setup)))

(with-eval-after-load 'volatile-highlights
  (diminish #'volatile-highlights-mode)
  (with-demoted-errors
    (vhl/define-extension 'my-evil-highlights
      'evil-yank
      'evil-paste-pop-proxy
      'evil-paste-pop-next
      'evil-paste-after
      'evil-paste-before)

    (vhl/install-extension 'my-evil-highlights)

    (vhl/define-extension 'my-undo-tree-highlights
      'undo-tree-undo
      'undo-tree-redo)

    (vhl/install-extension 'my-undo-tree-highlights)

    (volatile-highlights-mode +1)))

(with-eval-after-load 'auto-highlight-symbol
  (setq
    ahs-idle-interval 0.3))

(require 'auto-highlight-symbol)
(add-hook 'auto-highlight-symbol-mode-hook
  (lambda ()
    (diminish 'auto-highlight-symbol-mode)
    (set-face-attribute 'ahs-plugin-defalt-face nil
      :foreground nil)
    (set-face-attribute 'ahs-face nil
      :foreground nil
      :background "#073642")
    (set-face-attribute 'ahs-definition-face nil
      :foreground nil
      :background "#073642"
      :underline '(:color "#268bd2"))
    (set-face-attribute 'ahs-warning-face nil
      :foreground nil
      :underline '(:color "yellow"))
    (set-face-attribute 'ahs-plugin-bod-face nil
      :foreground nil
      :underline '(:color "purple"))
    (set-face-attribute 'ahs-plugin-whole-buffer-face nil
      :foreground nil
      :underline '(:color "black"))))

(global-auto-highlight-symbol-mode +1)

(global-set-key (kbd "<remap> <just-one-space>") #'cycle-spacing)

(defun smart-registers-and-rectangles ()
  (interactive)
  (unless (fboundp 'hydra/registers-and-rectangles/body)
    (require 'hydra)
    (defhydra hydra/registers-and-rectangles (:color blue :hint nil :idle 0.3)
      "
REGISTER                     │   RECTANGLE
^_SPC_^ point →    ^_i_^ insert ←    │   ^_c_^ clear    ^_r_^ copy-to-register
^_f_^ frameset →   ^_U_^ undo ←      │   ^_d_^ delete   ^_M-w_^ copy-as-kill
^_n_^ number →     ^_u_^ undo →      │   ^_k_^ kill     ^_t_^ string
^_x_^ copy →       ^_+_^ increment   │   ^_o_^ open     ^_N_^ number-lines
^_w_^ windows →                  │   ^_y_^ yank"
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
(global-set-key (kbd "C-x r") #'smart-registers-and-rectangles)

(defun smart-icicle-search-map ()
  (interactive)
  (unless (fboundp 'hydra/icicle-search-map/body)
    (require 'hydra)
    (defhydra hydra/icicle-search-map (:color blue :hint nil :idle 0.3)
      "
^_f_^   file     ^_D_^ defs-full    ^_,_^  tags                ^_g_^ grep-saved-file-cands
^_b_^   buffer   ^_j_^ bookmark     ^_x_^ xml-element          ^_X_^ xml-element-text-node
^_l_^   lines    ^_k_^ keywords     ^_J_^ bookmarks-together   ^_o_^ occur
^_w_^   word     ^_p_^ paragraphs   ^_c_^ char-property        ^_i_^ imenu
^_d_^   defs     ^_s_^ sentences    ^_O_^ overlay-property     ^_I_^ imenu-full
^_C-l_^ pages    ^_t_^ thing        ^_T_^ text-property"
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

(defun smart-isearch-map ()
  (interactive)
  (unless (fboundp 'hydra/isearch-map/body)
    (require 'hydra)
    (defhydra hydra/isearch-map (:color blue :hint nil :idle 0.3)
      "
Isearch^^^^          Highlight
^_._^ → symbol @ ·   ^_h._^ symbol @ ·   ^_hu_^ unhiglight regex
^___^ → symbol       ^_hl_^ lines        ^_hf_^ HL find patterns
^_w_^ → word         ^_hp_^ phrase       ^_hw_^ HL write patterns
^_o_^ occur          ^_hr_^ regex        ^_M-s_^ Icicle search ..."
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
      ("M-s" smart-icicle-search-map)))

  (hydra/isearch-map/body))

(global-set-key (kbd "M-s") #'smart-isearch-map)

(provide 'config-ui)
