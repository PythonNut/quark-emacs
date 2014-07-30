;;; fsvn-ui.el --- Global User Interface definitions for fsvn.el


;;; History:
;; 

;;; Commentary:
;; 

;;; Code:



(require 'dired)
(require 'fsvn-deps)
(require 'fsvn-url)
(require 'fsvn-xml)
(require 'fsvn-cmd)



(defcustom fsvn-generic-datetime-format "%Y-%m-%d %H:%M"
  "Date and time format in any."
  :group 'fsvn
  :type 'string)

(defcustom fsvn-help-locale nil
  "Locale of your favorite."
  :group 'fsvn
  :type 'string)

(defconst fsvn-mark-mark-char dired-marker-char
  "In `fsvn-browse-mode', the current mark character.
This is what the do-commands look for, and what the mark-commands store.")

(defconst fsvn-mark-delete-char dired-del-marker
  "Character used to flag files for deletion.")

(defgroup fsvn-faces nil
  "*"
  :group 'fsvn)

(defface fsvn-header-face
  '((t (:inherit dired-header)))
  "Face used for directory headers."
  :group 'fsvn-faces
  :version "22.1")

(defvar fsvn-header-face 'fsvn-header-face
  "Face name used for directory headers.")

(defface fsvn-header-key-face
  '((t (:inherit dired-header :bold t)))
  "Face used for directory headers."
  :group 'fsvn-faces
  :version "22.1")

(defvar fsvn-header-key-face 'fsvn-header-key-face
  "Face name used for directory headers.")

(defface fsvn-mark-face
  '((t (:inherit dired-mark)))
  "Face used for fsvn marks."
  :group 'fsvn-faces
  :version "22.1")

(defvar fsvn-mark-face 'fsvn-mark-face
  "Face name used for fsvn marks.")

(defface fsvn-marked-face
  '((t (:inherit dired-marked)))
  "Face used for marked files."
  :group 'fsvn-faces
  :version "22.1")

(defvar fsvn-marked-face 'fsvn-marked-face
  "Face name used for marked files.")

(defface fsvn-flagged-face
  '((t (:inherit dired-flagged)))
  "Face used for flagged files."
  :group 'fsvn-faces
  :version "22.1")

(defvar fsvn-flagged-face 'fsvn-flagged-face
  "Face name used for flagged files.")

(defface fsvn-warning-face
  '((t (:inherit dired-warning)))
  "Face used to highlight a part of a buffer that needs user attention."
  :group 'fsvn-faces
  :version "22.1")

(defvar fsvn-warning-face 'fsvn-warning-face
  "Face name used for a part of a buffer that needs user attention.")

(defface fsvn-directory-face
  '((t (:inherit dired-directory)))
  "Face used for subdirectories."
  :group 'fsvn-faces
  :version "22.1")

(defvar fsvn-directory-face 'fsvn-directory-face
  "Face name used for subdirectories.")

(defface fsvn-symlink-face
  '((t (:inherit dired-symlink)))
  "Face used for subdirectories."
  :group 'fsvn-faces
  :version "22.1")

(defvar fsvn-symlink-face 'fsvn-symlink-face
  "Face name used for subdirectories.")

(defface fsvn-ignored-face
  '((t (:inherit shadow)))
  "Face used for files suffixed with `completion-ignored-extensions'."
  :group 'fsvn-faces
  :version "22.1")

(defvar fsvn-ignored-face 'fsvn-ignored-face
  "Face name used for files suffixed with `completion-ignored-extensions'.")

(defface fsvn-keyname-face
  '((t (:inherit dired-directory)))
  "Face used for revision"
  :group 'fsvn-faces
  :version "22.1")

(defvar fsvn-keyname-face 'fsvn-keyname-face
  "Face name used for revision.")

(defface fsvn-link-face
  '((t (:foreground "blue" :underline "blue")))
  "Face used for any link"
  :group 'fsvn-faces
  :version "22.1")

(defvar fsvn-link-face 'fsvn-link-face
  "Face used for any link.")

(defface fsvn-diff-add-face
  '((((class color) (background light))
     :foreground "blue1")
    (((class color) (background dark))
     :foreground "cyan1"))
  "Face for lines in a diff that have been added."
  :group 'fsvn)

(defvar fsvn-diff-add-face 'fsvn-diff-add-face
  "Face used for added line in diff-mode")

(defface fsvn-diff-delete-face
  '((((class color) (background light))
     :foreground "red")
    (((class color) (background dark))
     :foreground "OrangeRed"))
  "Face for lines in a diff that have been deleted."
  :group 'fsvn)

(defvar fsvn-diff-delete-face 'fsvn-diff-delete-face
  "Face used for deleted line in diff-mode")

(defface fsvn-diff-whitespace-warning-face
  '((t :inherit trailing-whitespace))
  "Face for highlighting whitespace errors in fsvn diffs."
  :group 'fsvn)

(defvar fsvn-diff-whitespace-warning-face
  'fsvn-diff-whitespace-warning-face
  "Face for highlighting whitespace errors in fsvn diffs.")

(defun fsvn-face-status-create (face color)
  (custom-declare-face
   face
   `(;; (((class color) (background dark)) :foreground ,color :background "black" :weight bold)
     ;; (((class color) (background light)) :foreground ,light :background "white" :weight bold)
     (t :foreground ,color :background "gray76" :weight bold))
   nil))

(fsvn-face-status-create 'fsvn-status-replaced-face "blue")
(fsvn-face-status-create 'fsvn-status-modified-face "tomato")
(fsvn-face-status-create 'fsvn-status-conflicted-face "dark violet")
(fsvn-face-status-create 'fsvn-status-added-face "yellow")
(fsvn-face-status-create 'fsvn-status-deleted-face "red")



(defcustom fsvn-dired-copy-filename-separator " "
  "String value of separate multiple filenames when killing."
  :group 'fsvn-dired
  :type 'string)



;; playing
;;  http://ja.wiktionary.org/wiki/Category:%E8%8B%B1%E8%AA%9E_%E4%B8%8D%E8%A6%8F%E5%89%87%E8%A4%87%E6%95%B0%E5%BD%A2%E3%81%AE%E5%90%8D%E8%A9%9E

(defconst fsvn-word-irregular-plural-alist
  '(
    ("child" . "children" )
    ;;    ("crux" . "cruxes" )
    ("foot" . "feet" )
    ("knife" . "knives" )
    ("leaf" . "leaves" )
    ("louse" . "lice" )
    ("man" . "men" )
    ("medium" . "media" )
    ("mouse" . "mice" )
    ("oasis" . "oases" )
    ("person" . "people" )
    ("phenomenon" . "phenomena" )
    ("seaman" . "seamen" )
    ("snowman" . "snowmen" )
    ("tooth" . "teeth" )
    ("woman" . "women" )
    ))

(defun fsvn-word-plural (word)
  (cond
   ((fsvn-string-assoc word fsvn-word-irregular-plural-alist)
    (cdr (fsvn-string-assoc word fsvn-word-irregular-plural-alist)))
   ((string-match "\\(sh\\|ch\\|o\\|s\\|x\\)$" word)
    (concat word "es"))
   ((string-match "\\(y\\)$" word)
    (replace-match "ies" nil nil word 1))
   (t
    (concat word "s"))))



;; face utility

;; FIXME want to well contrast value
(defun fsvn-get-background-color (foreground &optional colors)
  (let ((colors (or colors (defined-colors)))
        (count (length colors))
        (rest (member foreground colors)))
    ;;FIXME
    (nth (% (+ (length rest) 100) count) colors)))


(defun fsvn-ui-symlink-trailer (linkp)
  (if linkp (concat " -> " linkp) ""))



(defun fsvn-header-tail (&optional len)
  (make-string (abs len) ?-))

(defun fsvn-header-tail-fill-line ()
  (let ((width (- (frame-width) (current-column))))
    (when (> width 0)
      (insert (fsvn-header-tail width) "\n"))))



(defun fsvn-buffer-popup-as-information (buffer)
  (let ((win (fsvn-buffer-popup-window)))
    (unless win
      (delete-other-windows)
      (setq win (split-window)))
    (set-window-buffer win buffer)
    (fsvn-save-window-only win
      (goto-char (point-min)))
    (redisplay t)))

(defun fsvn-buffer-popup-window ()
  (save-window-excursion
    (catch 'found
      (mapc
       (lambda (w)
         (select-window w)
         (when (eq major-mode 'fsvn-popup-result-mode)
           (throw 'found w)))
       (window-list))
      nil)))



(defconst fsvn-brief-message-buffer-name " *Fsvn Popup*")

(defun fsvn-brief-message-show-popup ()
  (let* ((buf (get-buffer-create fsvn-brief-message-buffer-name))
         (win (get-buffer-window buf)))
    (when (and win (window-live-p win))
      (delete-window win))
    (dired-pop-to-buffer buf)))

(defun fsvn-brief-message-clear-message ()
  (with-current-buffer (get-buffer-create fsvn-brief-message-buffer-name)
    (erase-buffer)))

(defun fsvn-brief-message-add-message (message)
  (with-current-buffer (get-buffer-create fsvn-brief-message-buffer-name)
    (goto-char (point-max))
    (insert message "\n")
    (fsvn-brief-message-show-popup)))

(defmacro fsvn-brief-message-showing (&rest form)
  `(save-window-excursion
     (fsvn-brief-message-clear-message)
     ,@form))



(require 'electric nil t)

(defvar unread-command-events)

(defvar fsvn-electric-line-select-mode-map nil)
(defvar fsvn-electric-scroll-terminate nil)

(unless fsvn-electric-line-select-mode-map
  (setq fsvn-electric-line-select-mode-map
        (let ((map (make-keymap)))
          (fillarray (car (cdr map)) 'undefined)

          (define-key map " " 'fsvn-electric-line-select-select)
          (define-key map "*" 'fsvn-electric-line-mark)
          (define-key map "?" 'Helper-describe-bindings)
          (define-key map "U" 'fsvn-electric-line-unmark-all)
          (define-key map "\C-]" 'fsvn-electric-line-select-quit)
          (define-key map "\C-c" nil)
          (define-key map "\C-c\C-c" 'fsvn-electric-line-select-select)
          (define-key map "\C-c\C-k" 'fsvn-electric-line-select-quit)
          (define-key map "\C-c\C-q" 'fsvn-electric-line-select-quit)
          (define-key map "\C-m" 'fsvn-electric-line-select-select)
          (define-key map "\C-n" 'fsvn-electric-next-line)
          (define-key map "\C-p" 'fsvn-electric-previous-line)
          (define-key map "\C-u" 'universal-argument)
          (define-key map "\C-v" 'fsvn-electric-scroll-up)
          (define-key map "\e" nil)
          (define-key map "\ev" 'fsvn-electric-scroll-down)
          (define-key map "m" 'fsvn-electric-line-mark)
          (define-key map "n" 'fsvn-electric-next-line)
          (define-key map "p" 'fsvn-electric-previous-line)
          (define-key map "q" 'fsvn-electric-line-select-quit)
          (define-key map "u" 'fsvn-electric-line-unmark)

          map)))

(defvar fsvn-electric-line-alist nil)
(defvar fsvn-electric-start-point nil)
(defvar fsvn-electric-end-point nil)
(defvar fsvn-electric-done-function nil)
(defvar fsvn-electric-next-data-function nil)
(defvar fsvn-electric-mark-function nil)
(defvar fsvn-electric-unmark-function nil)

(defconst fsvn-electric-line-select-buffer-local-variables
  '(
    (fsvn-electric-line-alist)
    (fsvn-electric-start-point)
    (fsvn-electric-end-point)
    (fsvn-electric-done-function)
    (fsvn-electric-mark-function)
    (fsvn-electric-unmark-function)
    (fsvn-electric-next-data-function)
    (truncate-lines)
    ))

(defcustom fsvn-electric-line-select-mode-hook nil
  "Run at the very end of `fsvn-electric-line-select-mode'."
  :group 'fsvn
  :type 'hook)

(define-minor-mode fsvn-electric-line-select-mode
  "
Keybindings:
\\{fsvn-electric-line-select-mode-map}

"
  nil " (Electric)" fsvn-electric-line-select-mode-map
  (fsvn-make-buffer-variables-internal fsvn-electric-line-select-buffer-local-variables))

(defun fsvn-electric-line-select (buffer prompt)
  (let (select message-log-max)
    (save-window-excursion
      (Electric-pop-up-window buffer)
      (unwind-protect
          (progn
            (set-buffer buffer)
            (fsvn-electric-line-select-buffer-update-highlight)
            (setq select
                  (catch 'fsvn-electric-buffer-menu-select
                    (message (or prompt "->"))
                    (when (eq (setq unread-command-events (list (read-event))) ?\s)
                      (setq unread-command-events nil)
                      (throw 'fsvn-electric-buffer-menu-select nil))
                    (let* ((start-point (point))
                           (first-form '(or fsvn-electric-start-point (point-min)))
                           (last-form '(or fsvn-electric-end-point 
                                           (progn 
                                             (goto-char (1- (point-max))) 
                                             (line-beginning-position))))
                           (first (eval first-form))
                           (last (eval last-form)))
                      ;; Use start-point if it is meaningful.
                      (goto-char (if (or (< start-point first)
                                         (> start-point last))
                                     first
                                   start-point))
                      (Electric-command-loop 'fsvn-electric-buffer-menu-select
                                             prompt
                                             t
                                             'fsvn-electric-line-select-buffer-menu-looper
                                             (cons first-form last-form))))))
        (message nil)))
    select))

(defun fsvn-electric-line-select-buffer-menu-looper (state condition)
  (cond 
   ((and condition
         (not (memq (car condition) '(buffer-read-only
                                      end-of-buffer
                                      beginning-of-buffer))))
    (signal (car condition) (cdr condition)))
   ((< (point) (save-excursion (eval (car state))))
    (goto-char (point-min)))
   ((> (point) (save-excursion (eval (cdr state))))
    (goto-char (point-max))
    (forward-line -1)
    (if (pos-visible-in-window-p (point-max))
        (recenter -1))))
  (fsvn-electric-line-select-buffer-update-highlight))

(defvar fsvn-electric-line-select-buffer-overlay nil)
(defun fsvn-electric-line-select-buffer-update-highlight ()
  (when fsvn-electric-line-select-mode
    ;; Make sure we have an overlay to use.
    (unless fsvn-electric-line-select-buffer-overlay
      (make-local-variable 'fsvn-electric-line-select-buffer-overlay)
      (setq fsvn-electric-line-select-buffer-overlay (make-overlay (point) (point))))
    (move-overlay fsvn-electric-line-select-buffer-overlay 
                  (line-beginning-position)
                  (line-end-position))
    (overlay-put fsvn-electric-line-select-buffer-overlay 'face 'highlight)))

(defun fsvn-electric-call-next-data ()
  (condition-case err
      (progn
        (message "Geting Next...")
        (funcall fsvn-electric-next-data-function))
    (error
     (setq fsvn-electric-scroll-terminate t)
     (setq fsvn-electric-next-data-function nil)
     (ding))))

(defun fsvn-electric-line-select-quit ()
  (interactive)
  (throw 'fsvn-electric-buffer-menu-select nil))

(defun fsvn-electric-line-select-select ()
  (interactive)
  (throw 'fsvn-electric-buffer-menu-select (funcall fsvn-electric-done-function)))

(defun fsvn-electric-line-mark ()
  (interactive)
  (if (null fsvn-electric-mark-function)
      (undefined)
    (funcall fsvn-electric-mark-function)
    (fsvn-electric-line-update-fontify)
    (fsvn-electric-next-line)))

(defun fsvn-electric-line-unmark ()
  (interactive)
  (if (null fsvn-electric-unmark-function)
      (undefined)
    (funcall fsvn-electric-unmark-function)
    (fsvn-electric-line-update-fontify)
    (fsvn-electric-next-line)))

(defun fsvn-electric-line-unmark-all ()
  (interactive)
  (if (null fsvn-electric-unmark-function)
      (undefined)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (funcall fsvn-electric-unmark-function)
        (fsvn-electric-next-line)))
    (font-lock-fontify-buffer)))

(defun fsvn-electric-line-update-fontify ()
  (font-lock-fontify-region (line-beginning-position) (line-end-position)))

(defmacro fsvn-electric-scroll (scroller error next-pos)
  `(condition-case nil
       (prog1
           ,scroller
         (setq fsvn-electric-scroll-terminate nil))
     (,error
      (if (and fsvn-electric-scroll-terminate
               (not (pos-visible-in-window-p ,next-pos)))
          (goto-char ,next-pos)
        (unless fsvn-electric-next-data-function
          (ding)))
      (setq fsvn-electric-scroll-terminate t))))

(defun fsvn-electric-scroll-down ()
  (interactive)
  (fsvn-electric-scroll (scroll-down) beginning-of-buffer (point-max)))

(defun fsvn-electric-scroll-up ()
  (interactive)
  (fsvn-electric-scroll (scroll-up) end-of-buffer (point-min))
  (when (and fsvn-electric-scroll-terminate fsvn-electric-next-data-function)
    (setq fsvn-electric-scroll-terminate nil)
    (fsvn-electric-call-next-data)))

(defun fsvn-electric-previous-line (&optional arg)
  (interactive "p")
  (forward-line (- arg)))

(defun fsvn-electric-next-line (&optional arg)
  (interactive "p")
  (forward-line arg)
  (when fsvn-electric-next-data-function
    (fsvn-electric-call-next-data)))



(require 'ls-lisp)

(defconst fsvn-electric-select-file-list-buffer-name " *Fsvn Electric* ")

(defvar fsvn-electric-select-file-font-lock-keywords
  (list
   (list (concat "^[" (char-to-string fsvn-mark-mark-char) "]")
         '("\\(.+\\)" (fsvn-electric-select-pre-match) nil (1 fsvn-marked-face)))
   (list "^..d"
         '("\\(.+\\)" (fsvn-electric-select-pre-match) nil (1 fsvn-directory-face)))
   (list "^..l"
         '("\\(.+\\)" (fsvn-electric-select-pre-match) nil (1 fsvn-symlink-face)))
   ))


(defun fsvn-electric-select-files (base-directory alist &optional prompt)
  "ALIST is displaying information about files.

Elements of the alist are:
0. File name.
1. Mark or not.
2. Message after insert file name.
3. Rest of data.
"
  (let ((specialize-prompt
         (concat prompt
                 (substitute-command-keys (concat
                                           "\\<fsvn-electric-line-select-mode-map>"
                                           "Type \\[fsvn-electric-line-mark] to mark, "
                                           "\\[fsvn-electric-line-unmark] to unmark, "
                                           "\\[fsvn-electric-line-select-select] to finish.")))))
    (fsvn-electric-fs-prepare-list base-directory specialize-prompt
      (fsvn-electric-select-files-insert base-directory alist)
      (setq fsvn-electric-done-function 'fsvn-electric-select-files-done)
      (setq fsvn-electric-mark-function 'fsvn-electric-select-files-mark)
      (setq fsvn-electric-unmark-function 'fsvn-electric-select-files-unmark)
      (setq fsvn-electric-line-alist alist))))

(defun fsvn-electric-select-file (base-directory files &optional prompt)
  (let ((specialize-prompt
         (concat prompt
                 (substitute-command-keys (concat
                                           "\\<fsvn-electric-line-select-mode-map>"
                                           "Type \\[fsvn-electric-line-select-select] to finish.")))))
    (fsvn-electric-fs-prepare-list base-directory specialize-prompt
      (fsvn-electric-select-file-insert base-directory files)
      (setq fsvn-electric-done-function 'fsvn-electric-select-file-done)
      (setq fsvn-electric-line-alist
            (mapcar (lambda (f) (cons f nil)) files)))))

(defun fsvn-electric-select-pre-match ()
  (let ((points (fsvn-points-of-filename)))
    (when points
      (goto-char (car points))
      (cdr points))))

(defun fsvn-electric-select-file-insert (base-directory files)
  (let (buffer-read-only)
    (mapc
     (lambda (file)
       (fsvn-electric-select-file-insert-line base-directory file))
     files)))

(defmacro fsvn-electric-fs-prepare-list (base-directory prompt &rest form)
  (declare (indent 2))
  `(let ((buffer (get-buffer-create fsvn-electric-select-file-list-buffer-name)))
     (with-current-buffer buffer
       (set (make-local-variable 'font-lock-defaults)
            '(fsvn-electric-select-file-font-lock-keywords t nil nil beginning-of-line))
       (let (buffer-read-only)
         (erase-buffer)
         (fsvn-electric-line-select-mode 1)
         (setq default-directory (file-name-as-directory ,base-directory))
         ,@form)
       (font-lock-mode 1)
       (font-lock-fontify-buffer)
       (run-hooks 'fsvn-electric-line-select-mode-hook))
     (fsvn-electric-line-select buffer ,prompt)))

(defun fsvn-electric-select-file-current-name ()
  (let ((filename (fsvn-current-filename)))
    (fsvn-expand-file filename)))

(defun fsvn-electric-select-file-done ()
  (fsvn-electric-select-file-current-name))

(defun fsvn-electric-select-file-insert-line (base-directory file)
  (insert (fsvn-electric-select-file-format base-directory file))
  (insert "\n"))

(defun fsvn-electric-select-files-insert (base-directory file-alist)
  (let (buffer-read-only)
    (mapc
     (lambda (file)
       (fsvn-electric-select-files-insert-line base-directory file))
     file-alist)))

(defun fsvn-electric-select-file-format (base-directory file)
  (let ((attr (file-attributes file)))
    (format "  %s %s %s %s"
            (nth 8 attr)
            (fsvn-generic-format-file-size (nth 7 attr))
            (format-time-string fsvn-generic-datetime-format (nth 5 attr))
            (propertize (fsvn-url-relative-name file base-directory) 'fsvn-filename t))))

(defun fsvn-electric-select-files-insert-line (base-directory file-info)
  (let* ((file (nth 0 file-info))
         (mark (nth 1 file-info))
         (msg (nth 2 file-info)))
    (insert (fsvn-electric-select-file-format base-directory file))
    (when msg
      (insert " : ")
      (insert msg))
    (when mark
      (fsvn-electric-select-files-mark))
    (insert "\n")))

(defun fsvn-electric-select-files-switch-mark (mark-char)
  (save-excursion
    (let ((inhibit-read-only t)
          buffer-read-only)
      (when (fsvn-move-to-filename)
        (forward-line 0)
        (delete-char 1)
        (insert mark-char)))))

(defun fsvn-electric-select-files-done ()
  (let ((inhibit-quit) ;; for safe
        (regexp (concat "^" (char-to-string fsvn-mark-mark-char)))
        name tmp ret)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when (looking-at regexp)
          (when (setq tmp (fsvn-electric-select-files-current-item))
            (setq ret (cons tmp ret))))
        (forward-line 1)))
    (cond
     (ret
      (nreverse ret))
     ((setq tmp (fsvn-electric-select-files-current-item))
      (list tmp))
     (t
      nil))))

(defun fsvn-electric-select-files-current-item ()
  (let ((name (fsvn-electric-select-file-current-name)))
    (assoc name fsvn-electric-line-alist)))

(defun fsvn-electric-select-files-mark ()
  (fsvn-electric-select-files-switch-mark fsvn-mark-mark-char))

(defun fsvn-electric-select-files-unmark ()
  (fsvn-electric-select-files-switch-mark ?\s))



(defun fsvn-generic-format-file-size (size &optional length)
  (fsvn-string-lpad
   (cond
    ((< size 1000000)
     (number-to-string size))
    ((< size 1000000000.0)
     (format "%0.1fM" (/ size 1000000.0)))
    (t
     (format "%0.1fG" (/ size 1000000000.0))))
   (or length 10)))


;;
;; window management
;;

(defun fsvn-window-setting ()
  (mapcar 
   (lambda (win)
     (list win (window-buffer win))) 
   (window-list)))

(defun fsvn-window-cleanup (settings)
  (mapc
   (lambda (setting)
     (let* ((win (nth 0 setting))
            (buf (nth 1 setting)))
       (unless (or (buffer-live-p buf)
                   (minibufferp buf))
         (when (and (window-live-p win)
                    (> (length (window-list)) 1))
           (delete-window win)))))
   settings))

(defmacro fsvn-window-with-cleanup (&rest form)
  (declare (indent 0))
  `(let ((SETTINGS (fsvn-window-setting)))
     ,@form
     (fsvn-window-cleanup SETTINGS)))



;;
;; mode line status (from psvn.el)
;;

(defvar fsvn-ui-fancy-modeline t) ; modeline mark display or not
(defvar fsvn-ui-fancy-tooltip nil) ; modeline tooltip display
(defvar fsvn-ui-image-directory
  (condition-case nil
      (let ((validator (lambda (file) (and (eq (car (file-attributes file)) t) file))))
        (or
         (funcall validator
                  (expand-file-name "images" (file-name-directory (locate-library "fsvn-ui"))))
         (funcall validator
                  (expand-file-name "images/fsvn" data-directory))))
    (error nil)))

(defcustom fsvn-ui-fancy-file-state-in-modeline t
  "Show a color dot in the modeline that describes the state of the current file."
  :type 'boolean
  :group 'fsvn)

(defvar fsvn-ui-fancy-object-picture-alist nil)
  
(defun fsvn-ui-fancy-object-picture (color)
  (let ((cell (assoc color fsvn-ui-fancy-object-picture-alist)))
    (cond
     (cell (cdr cell))
     (t
      (setq cell (cons color nil))
      (setq fsvn-ui-fancy-object-picture-alist
            (cons cell fsvn-ui-fancy-object-picture-alist))
      (when fsvn-ui-image-directory
        (let ((file (expand-file-name (concat color ".xpm") fsvn-ui-image-directory)))
          (when (file-exists-p file)
            (with-temp-buffer
              (insert-file-contents file)
              (setcdr cell (buffer-string))))))
      (cdr cell)))))

(defun fsvn-ui-fancy-get-picture (color)
  (or (fsvn-ui-fancy-object-picture color)
      (format "/* XPM */
static char * data[] = {
\"18 13 3 1\",
\"  c None\",
\"+ c #000000\",
\". c %s\",
\"                  \",
\"       +++++      \",
\"      +.....+     \",
\"     +.......+    \",
\"    +.........+   \",
\"    +.........+   \",
\"    +.........+   \",
\"    +.........+   \",
\"    +.........+   \",
\"     +.......+    \",
\"      +.....+     \",
\"       +++++      \",
\"                  \"};"
              color)))

(defun fsvn-ui-fancy-modeline-picture (color)
  (propertize "    "
              'help-echo 'fsvn-ui-fancy-tooltip
              'display
              `(image :type xpm
                      :data ,(fsvn-ui-fancy-get-picture color)
                      :ascent center)))

(defun fsvn-ui-fancy-install-state-mark (color)
  (add-hook 'after-revert-hook 'fsvn-ui-fancy-redraw nil t)
  (let ((mode `(fsvn-ui-fancy-modeline
                ,(fsvn-ui-fancy-modeline-picture color))))
    (unless (assq 'fsvn-ui-fancy-modeline mode-line-format)
      (setq mode-line-format (cons mode mode-line-format)))
    (force-mode-line-update t)))

(defun fsvn-ui-fancy-uninstall-state-mark ()
  (remove-hook 'after-revert-hook 'fsvn-ui-fancy-redraw t)
  (when (listp mode-line-format)
    (setq mode-line-format
          (assq-delete-all 'fsvn-ui-fancy-modeline
                           mode-line-format)))
  (force-mode-line-update t))

(defun fsvn-ui-fancy-update-state-mark-tooltip (tooltip)
  (setq fsvn-ui-fancy-tooltip tooltip))

(defun fsvn-ui-fancy-update-state-mark (color)
  (fsvn-ui-fancy-uninstall-state-mark)
  (fsvn-ui-fancy-install-state-mark color))

(defun fsvn-ui-fancy-redraw ()
  (if (and fsvn-ui-fancy-file-state-in-modeline
           (fsvn-vc-mode-p))
      (fsvn-ui-fancy-update-modeline)
    (fsvn-ui-fancy-uninstall-state-mark)))

(defadvice vc-find-file-hook (after fsvn-ui-fancy-vc-find-file-hook disable)
  "vc-find-file-hook advice for synchronizing psvn with vc-svn interface"
  (fsvn-ui-fancy-redraw))

(defadvice vc-after-save (after fsvn-ui-fancy-vc-after-save disable)
  "vc-after-save advice for synchronizing psvn when saving buffer"
  (fsvn-ui-fancy-redraw))

(defadvice ediff-refresh-mode-lines
  (around fsvn-ui-fancy-ediff-modeline-fixup disable compile)
  "Fixup svn file status in the modeline when using ediff"
  (ediff-with-current-buffer ediff-buffer-A
    (fsvn-ui-fancy-uninstall-state-mark))
  (ediff-with-current-buffer ediff-buffer-B
    (fsvn-ui-fancy-uninstall-state-mark))
  ad-do-it
  (ediff-with-current-buffer ediff-buffer-A
    (fsvn-ui-fancy-update-modeline))
  (ediff-with-current-buffer ediff-buffer-B
    (fsvn-ui-fancy-update-modeline)))

(defun fsvn-ui-fancy-update-modeline ()
  "Update modeline state dot mark properly"
  (when (and buffer-file-name (fsvn-vc-mode-p))
    (fsvn-ui-fancy-update-state-mark
     (fsvn-ui-fancy-interpret-state-mode-color
      (let ((status (fsvn-get-file-status buffer-file-name)))
        (fsvn-xml-status->target->entry=>wc-status.item status))))))

(defun fsvn-ui-fancy-interpret-state-mode-color (val)
  "Interpret vc-svn-state symbol to mode line color"
  (cond
   ((memq val '(deleted))
    ;; consider --keep-local
    "red")
   ((memq val '(replaced))
    "blue")
   ((memq val '(modified))
    "tomato")
   ((memq val '(added))
    "yellow")
   ((memq val '(conflicted incomplete))
    "violet")
   (t 
    "GreenYellow")))



(defmacro fsvn-cmd-read-subcommand-args (subcommand var)
  `(if current-prefix-arg
       (fsvn-read-svn-subcommand-args ,subcommand t ,var)
     ,var))



(provide 'fsvn-ui)

;;; fsvn-ui.el ends here
