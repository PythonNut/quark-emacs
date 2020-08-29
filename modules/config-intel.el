;; -*- lexical-binding: t -*-
(eval-when-compile (require 'config-macros))
(require 'cl-lib)
(require 'config-tramp)

(define-minor-mode read-passwd-show-hash-mode
  "Show the hashes of passwords read by read-passwd"
  :init-value t :global t)

(el-patch-defun read-passwd (prompt &optional confirm default)
  "Read a password, prompting with PROMPT, and return it.
If optional CONFIRM is non-nil, read the password twice to make sure.
Optional DEFAULT is a default password to use instead of empty input.

This function echoes `.' for each character that the user types.
You could let-bind `read-hide-char' to another hiding character, though.

Once the caller uses the password, it can erase the password
by doing (clear-string STRING)."
  (if confirm
      (let (success)
        (while (not success)
          (let ((first (read-passwd prompt nil default))
                (second (read-passwd "Confirm password: " nil default)))
            (if (equal first second)
                (progn
                  (and (arrayp second) (not (eq first second)) (clear-string second))
                  (setq success first))
              (and (arrayp first) (clear-string first))
              (and (arrayp second) (clear-string second))
              (message "Password not repeated accurately; please start over")
              (sit-for 1))))
        success)
    ((el-patch-swap let let*)
     ((el-patch-add ol)
      (hide-chars-fun
       (lambda (beg end _len)
         (clear-this-command-keys)
         (setq beg (min end (max (minibuffer-prompt-end)
                                 beg)))
         (el-patch-add
           (move-overlay ol (point-max) (point-max))
           (let ((len (- (point-max) (minibuffer-prompt-end)))
                 (hash (md5 (minibuffer-contents-no-properties))))
             (overlay-put ol 'after-string
                          (if (and (> len 10) read-passwd-show-hash-mode)
                              (format "  [%d chars, #%s]"
                                      len (substring hash 0 4))))))
         (dotimes (i (- end beg))
           (put-text-property (+ i beg) (+ 1 i beg)
                              'display (string (or read-hide-char ?.))))))
      minibuf)
     (minibuffer-with-setup-hook
         (lambda ()
           (setq minibuf (current-buffer))
           ;; Turn off electricity.
           (setq-local post-self-insert-hook nil)
           (setq-local buffer-undo-list t)
           (setq-local select-active-regions nil)
           (use-local-map read-passwd-map)
           (setq-local inhibit-modification-hooks nil) ;bug#15501.
           (setq-local show-paren-mode nil)		;bug#16091.
           (el-patch-add (setq ol (make-overlay (point-max) (point-max) nil t t)))
           (add-hook 'after-change-functions hide-chars-fun nil 'local))
       (unwind-protect
           (let ((enable-recursive-minibuffers t)
                 (read-hide-char (or read-hide-char ?.)))
             (read-string prompt nil t default)) ; t = "no history"
         (when (buffer-live-p minibuf)
           (with-current-buffer minibuf
             ;; Not sure why but it seems that there might be cases where the
             ;; minibuffer is not always properly reset later on, so undo
             ;; whatever we've done here (bug#11392).
             (remove-hook 'after-change-functions hide-chars-fun 'local)
             (kill-local-variable 'post-self-insert-hook)
             ;; And of course, don't keep the sensitive data around.
             (erase-buffer))))))))

(use-package semantic
  :ensure nil
  :init
  (with-eval-after-load 'semantic/db-file
    (advice-add
     'semanticdb-file-directory-exists-p :around
     (my/defun-as-value nadvice/semanticdb-file-directory-exists-p (old-fun &rest args)
       (cl-letf* (((symbol-function #'y-or-n-p) (lambda (prompt) t)))
         (apply old-fun args)))))

  :config
  (setq semanticdb-default-save-directory
        (locate-user-emacs-file "data/semanticdb")
        srecode-map-save-file
        (locate-user-emacs-file "data/srecode-map.el"))

  (advice-add
   'semantic-idle-summary-idle-function :around
   (my/defun-as-value nadvice/semantic-idle-summary-idle-function (old-fun &rest args)
     (unless (and (bound-and-true-p flycheck-mode)
                  (flycheck-overlays-at (point)))
       (apply old-fun args)))))

(use-package abbrev
  :ensure nil
  :config
  (setq abbrev-file-name (locate-user-emacs-file "data/.abbrev_defs")))

;;; ====================================
;;; flycheck - real-time syntax checking
;;; ====================================

(use-package flycheck
  :init (global-flycheck-mode +1)
  :config
  (setq flycheck-display-errors-function #'my/display-error-messages-condensed
        flycheck-indication-mode nil)

  (defun my/display-error-messages-condensed (errors)
    (eval-when-compile
      (use-package dash)
      (require 'dash))
    (-when-let (messages (-keep #'flycheck-error-message errors))
      (when (flycheck-may-use-echo-area-p)
        (display-message-or-buffer (mapconcat #'identity messages "\n")
                                   flycheck-error-message-buffer))))

  (set-face-background 'flycheck-fringe-warning nil)

  (set-face-attribute 'flycheck-error nil
                      :foreground nil
                      :background nil
                      :underline "#dc322f")

  (set-face-attribute 'flycheck-warning nil
                      :foreground nil
                      :background nil
                      :underline "#b58900")

  (set-face-attribute 'flycheck-info nil
                      :foreground nil
                      :background nil
                      :underline "#268bd2")

  ;; please don't give me emacs-lisp stylistic advice
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))

  (advice-add
   'flycheck-mode-line-status-text :override
   (my/defun-as-value nadvice/flycheck-mode-line-status-text (&optional status)
     (let ((text (pcase (or status flycheck-last-status-change)
                   (`not-checked "")
                   (`no-checker "-")
                   (`running "*")
                   (`errored "!")
                   (`finished
                    (if flycheck-current-errors
                        (let ((error-counts (flycheck-count-errors
                                             flycheck-current-errors)))
                          (format "%s/%s"
                                  (or (cdr (assq 'error error-counts)) "")
                                  (or (cdr (assq 'warning error-counts)) "")))
                      ""))
                   (`interrupted "-")
                   (`suspicious "?"))))
       (concat (if (display-graphic-p) " ✓" " Γ") text))))

  (defun flycheck-goto-nearest-error ()
    (interactive)
    (let ((next (flycheck-next-error-pos 1 nil))
          (prev (flycheck-next-error-pos -1 nil)))
      (cond
       ((and next prev)
        (goto-char (if  (< (abs (- (point) prev))
                           (abs (- (point) next)))
                       prev
                     next)))
       (next (goto-char next))
       (prev (goto-char prev))
       (t (user-error "No Flycheck errors")))))

  (define-key flycheck-mode-map (kbd "C-!") #'flycheck-goto-nearest-error))

(use-package eldoc
  :ensure nil
  :diminish eldoc-mode
  :config
  (eval-when-compile
    (with-demoted-errors "Load error: %s"
      (require 'flycheck)))

  (setq eldoc-idle-delay 0.2)

  (advice-add
   'eldoc-display-message-no-interference-p :around
   (my/defun-as-value nadvice/eldoc-display-message-no-interference-p (old-fun &rest args)
     (and (apply old-fun args)
          (not (and (bound-and-true-p sp-show-pair-overlays)
                    (not (minibufferp))))
          (not (and (bound-and-true-p flycheck-mode)
                    (flycheck-overlay-errors-at (point))))))))

;;; =======================================
;;; Flyspell - inline real time spell check
;;; =======================================
(use-package ispell
  :ensure nil
  :config
  (setq ispell-personal-dictionary
        (expand-file-name "data/user-dict.en.pws" user-emacs-directory))

  (cond ((executable-find "aspell")
         (add-to-list 'ispell-extra-args "--sug-mode=fast"))
        ((executable-find "hunspell")
         (setq ispell-program-name "hunspell")))

  (advice-add
   'ispell-init-process :around
   (my/defun-as-value nadvice/ispell-init-process (old-fun &rest args)
     (let ((inhibit-message t))
       (apply old-fun args)))))

(use-package flyspell-lazy
  :init
  (eval-when-compile
    (with-demoted-errors "Load error: %s"
      (require 'el-patch)))

  (autoload #'flyspell-lazy-load "flyspell-lazy")

  (el-patch-feature flyspell-lazy)

  (el-patch-define-minor-mode flyspell-lazy-mode
    "Turn on flyspell-lazy-mode.

Turning on flyspell-lazy-mode will set up hooks which
change how `flyspell-mode' works, in every buffer for which
flyspell is enabled.

When called interactively with no prefix argument this command
toggles the mode.  With a prefix argument, it enables the mode
if the argument is positive and otherwise disables the mode.

When called from Lisp, this command enables the mode if the
argument is omitted or nil, and toggles the mode if the argument
is 'toggle."
    :group 'flyspell-lazy
    :global t
    (cond
     (flyspell-lazy-mode
      (add-hook 'flyspell-mode-hook 'flyspell-lazy-load t)
      (when (and (el-patch-swap
                   (flyspell-lazy-called-interactively-p 'interactive)
                   (called-interactively-p 'interactive))
                 (not flyspell-lazy-less-feedback))
        (message "flyspell-lazy mode enabled")))
     (t
      (flyspell-lazy-unload 'global)
      (when (and (el-patch-swap
                   (flyspell-lazy-called-interactively-p 'interactive)
                   (called-interactively-p 'interactive))
                 (not flyspell-lazy-less-feedback))
        (message "flyspell-lazy mode disabled")))))

  (flyspell-lazy-mode +1)
  :config
  (setq flyspell-lazy-idle-seconds 1
        flyspell-lazy-window-idle-seconds 10
        flyspell-lazy-disallow-buffers nil))

(use-package flyspell
  :ensure nil
  :init
  (when (or (executable-find "ispell")
            (executable-find "aspell")
            (executable-find "hunspell"))
    (add-hook 'text-mode-hook #'flyspell-mode)
    (add-hook 'prog-mode-hook #'flyspell-prog-mode))

  :config
  (define-key flyspell-mode-map (kbd "C-.") nil)
  (define-key flyspell-mode-map (kbd "C-,") nil)

  (setq flyspell-issue-message-flag nil
        flyspell-issue-welcome-flag nil)

  (diminish 'flyspell-mode))

(use-package flyspell-correct-ivy
  :init
  (autoload #'flyspell-correct-ivy "flyspell-correct-ivy"
    "Run `ivy-read' for the given CANDIDATES.
List of CANDIDATES is given by flyspell for the WORD.
Return a selected word to use as a replacement or a tuple
of (command, word) to be used by `flyspell-do-correct'."))

(use-package flyspell-correct
  :init
  (with-eval-after-load 'flyspell
    (define-key flyspell-mode-map (kbd "C-M-i") #'flyspell-correct-at-point))
  :config
  (setq flyspell-correct-interface #'flyspell-correct-ivy))

;;; =============================================
;;; yasnippet -- extensible programmable snippets
;;; =============================================

(use-package yasnippet
  :init
  (setq yas-verbosity 0
        yas-alias-to-yas/prefix-p nil
        yas-use-menu nil)

  (defun my/yas-init ()
    (yas-global-mode +1))

  (my/onetime-setup yasnippet
    :hook 'first-change-hook
    :after-hook 'emacs-startup-hook
    :condition (get-buffer-window)
    (my/yas-init))

  :config
  (set-face-attribute 'yas-field-highlight-face nil
                      :foreground nil
                      :background nil
                      :inherit 'region)

  (add-hook
   'yas-global-mode-hook
   (my/defun-as-value my/diminish-yas-minor-mode ()
     (diminish 'yas-minor-mode (if (display-graphic-p) " ¥" " Y"))))

  (setq yas-snippet-dirs (list (locate-user-emacs-file "data/snippets"))
        yas-key-syntaxes (remove "w" yas-key-syntaxes)
        yas-triggers-in-field t
        yas-key-syntaxes (list "w_." "w_.()" #'yas-try-key-from-whitespace))

  (defun yas-company-complete-or-next-field ()
    (interactive)
    (if company-candidates
        (company-complete-common-or-complete-full)
      (yas-next-field)))

  (define-key yas-keymap (kbd "<tab>") 'yas-company-complete-or-next-field)
  (define-key yas-keymap (kbd "TAB") 'yas-company-complete-or-next-field)

  (add-to-list
   'yas-prompt-functions
   (my/defun-as-value my/ivy-yasnippet (_prompt choices &optional display-fn)
     "Use ivy to select a snippet. Put this into `yas-prompt-functions.'"
     (if (require 'ivy nil t)
         (let* ((disp-fn (or display-fn 'identity))
                (cands (mapcar (lambda (x) (cons (funcall disp-fn x) x)) choices))
                (result (ivy-read "Snippet: " (mapcar #'car cands))))
           (if (null result)
               (signal 'quit "user quit!")
             (cdr (assoc result cands))))
       nil))))

(use-package autoinsert
  :ensure nil
  :init
  ;; also use yasnippets for new file templates
  (defvar my/yas-template-dir (locate-user-emacs-file "data/templates"))

  (defun my/yatemplate-expand-yas-buffer ()
    "Expand the whole buffer with `yas-expand-snippet'."
    (require 'yasnippet)
    (yas-expand-snippet (buffer-string) (point-min) (point-max))
    (evil-insert-state))

  (defun my/yatemplate-fill-alist ()
    "Fill `auto-insert-alist'."
    (dolist (filename (nreverse (sort (file-expand-wildcards
                                       (concat my/yas-template-dir
                                               "**/*"))
                                      #'string<)))
      (let* ((split-name (split-string filename "="))
             (file-regex (if (eq (length split-name) 2)
                             (nth 1 split-name)
                           (lwarn "yatemplate" 'error
                                  "%s filename does not contain exactly one colon"
                                  filename)
                           nil)))
        (when file-regex
          (push (cons (intern file-regex)
                      (vector filename #'my/yatemplate-expand-yas-buffer))
                auto-insert-alist)))))

  (defvar auto-insert-alist nil)
  (my/yatemplate-fill-alist)

  (add-hook
   'after-change-major-mode-hook
   (my/defun-as-value my/maybe-auto-insert ()
     (when (and (= (point-min) (point-max))
                (assq major-mode auto-insert-alist))
       (auto-insert))))

  :config
  (my/yas-init))


;;; ==================================
;;; VLF intelligently edit large files
;;; ==================================

(defun my/buffer-binary-p (&optional buffer)
  "Return whether BUFFER or the current buffer is binary.

A binary buffer is defined as containing at least one null byte.

Returns either nil, or the position of the first null byte."
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      (goto-char (point-min))
      (search-forward (string ?\x00) 4096 t 1))))

(add-hook
 'find-file-hooks
 (my/defun-as-value my/hexl-if-binary ()
   "If `fundamental-mode' is active, and the current buffer
is binary, activate `hexl-mode'."
   (when (and (eq major-mode 'fundamental-mode)
              (my/buffer-binary-p))
     (hexl-mode)
     (message "Detected binary file. Switched to text mode."))))

(use-package vlf
  :defer-install t
  :commands (vlf)
  :init
  (advice-add
   'abort-if-file-too-large :around
   (my/defun-as-value nadvice/abort-if-file-too-large (_old-fun &rest args)
     (cl-destructuring-bind (size op-type filename &rest _offer-raw) args
       (when (and size
                  (not (zerop size))
                  large-file-warning-threshold
                  (< large-file-warning-threshold size))
         (let ((char nil))
           (while (not (memq (setq char
                                   (read-event
                                    (propertize
                                     (format
                                      "File %s is large (%s): \
%s normally (o), %s with vlf (v) or abort (a)"
                                      (if filename
                                          (file-name-nondirectory filename)
                                        "")
                                      (file-size-human-readable size)
                                      op-type op-type)
                                     'face 'minibuffer-prompt)))
                             '(?o ?O ?v ?V ?a ?A))))
           (cond ((memq char '(?v ?V))
                  (vlf filename)
                  (error ""))
                 ((memq char '(?a ?A))
                  (error "Aborted"))))))))

  :config
  (add-hook
   'vlf-mode-hook
   (my/defun-as-value my/vlf-hook ()
     (setq bidi-display-reordering nil)
     (flyspell-mode -1)
     (flycheck-mode -1)
     (ws-butler-mode -1)
     (visual-line-mode -1)
     (adaptive-wrap-prefix-mode -1)
     (setq-local global-hl-line-mode nil)
     (setq-local column-number-mode nil)
     (my/hexl-if-binary)
     (message "Use C-c C-v → VLF"))))

;;; =================================
;;; Emacs fasd - find files from fasd
;;; =================================

(use-package fasd
  :defer-install t
  :commands (fasd-find-file)
  :config
  (setq fasd-enable-initial-prompt nil)

  (advice-add
   'fasd-find-file :around
   (my/defun-as-value nadvice/fasd-find-file (old-fun &rest args)
     (require 'helm-mode)
     ;; overriding the completion system in emacs-fasd is surprisingly tricky
     (cl-letf (((symbol-function #'completing-read)
                #'helm--completing-read-default))
       (apply old-fun args)))))

;;; =================================================
;;; dumb-jump an unintelligent goto-definition system
;;; =================================================

(use-package dumb-jump
  :defer-install t
  :init
  (defun my/jump-to-definition-dwim ()
    (interactive)
    (if (and (executable-find "global")
             (or (getenv "GTAGSROOT")
                 (locate-dominating-file default-directory "GTAGS")))
        (helm-gtags-dwim)
      (dumb-jump-go)))

  :commands (dumb-jump-back
             dumb-jump-quick-look
             dumb-jump-go-other-window
             dumb-jump-go-current-window
             dumb-jump-go-prefer-external
             dumb-jump-go-prompt
             dumb-jump-go-prefer-external-other-window
             dumb-jump-go
             dumb-jump-mode))

;;; =====================================
;;; webpaste simple pastebin-like service
;;; =====================================

(use-package webpaste
  :defer-install t
  :commands (webpaste-paste-buffer
             webpaste-paste-region)
  :config
  (setq webpaste-provider-priority '("ix.io"
                                     "dpaste.com"
                                     "dpaste.de"
                                     "gist.github.com")))

;;; ============================================
;;; open a file manager in the current directory
;;; ============================================

(defun run-file-manager ()
  (interactive)
  (require 'config-core)
  (when (file-remote-p default-directory)
    (error "Opening file manager with remote directory not implemented!"))
  (let ((dir (my/file-name-first-existing-parent
              (expand-file-name default-directory))))
    (cond ((executable-find "nautilus")
           (call-process "nautilus" nil nil nil dir)))))

(defun pdf-print-buffer-with-faces (&optional filename)
  "Print file in the current buffer as pdf, including font, color, and
underline information.  This command works only if you are using a window system,
so it has a way to determine color values.

C-u COMMAND prompts user where to save the Postscript file (which is then
converted to PDF at the same location."
  (require 'ps-print)
  (interactive (list (if current-prefix-arg
                         (ps-print-preprint 4)
                       (concat (file-name-sans-extension (buffer-file-name))
                               ".ps"))))
  (ps-print-with-faces (point-min) (point-max) filename)
  (shell-command (concat "ps2pdf " filename))
  (delete-file filename)
  (message "Deleted %s" filename)
  (message "Wrote %s" (concat (file-name-sans-extension filename) ".pdf")))

(provide 'config-intel)
