
(defun key-combo-test-helper-execute (cmd)
  ;; (key-combo-mode 1)
  (execute-kbd-macro (key-combo-read-kbd-macro cmd))
  (substring-no-properties (buffer-string)))

;; can not use recent-keys because it does't record keyborad macro
;; (defvar my-key-combo-count 0)
(defun my-key-combo-keys-vector ()
  (vconcat (read-kbd-macro (substring (symbol-name
                                       (when (symbolp last-nonmenu-event)
                                           last-nonmenu-event)
                                       ) 1))))

(defun my-unread-events (vector)
  ;;cannot use push because need to concat vector and list
  (setq unread-command-events
        (append vector
                unread-command-events))
  ;;(reset-this-command-lengths)
  )

(defadvice my-key-combo-post-command-function (around intercept activate)
  (condition-case err
      ad-do-it
    ;; Let the debugger run
    ((debug error) (signal (car err) (cdr err)))))

(defun key-combo-execute-original ()
  (interactive)
  ;; for self-insert-command
  (setq last-command-event (aref (my-key-combo-keys-vector)0))
  (call-interactively (key-binding (vector last-command-event)))
  )

;; (setq debug-on-error t)

(defun my-key-combo-post-command-function ()
  ;;(message "this:%S" (this-command-keys-vector))
  ;; (message "this:%S" last-nonmenu-event)
  ;; (message "this:%s" last-command-event)
  ;; (message "this:%S" last-nonmenu-event)
  ;; (message "1 thi:%s li:%s ln:%s lc:%s"
  ;;          (key-description (this-command-keys-vector))
  ;;          (key-description (vector last-input-event))
  ;;          (key-description (vector last-nonmenu-event))
  ;;          (key-description (vector last-command-event))
  ;;          )
  (let* ((in-key-combo (eq 'key-combo
                           (if (< 0 (length (this-command-keys-vector)))
                               (aref (this-command-keys-vector) 0))))
         (keys-vector (if in-key-combo (my-key-combo-keys-vector) nil));
         (events (vector (read-event))))
    ;; (message "1.5 thi:%s rev:%S ev:%s li:%S lc:%S ln:%S vck:%s in:%s vc:%s"
    ;;          (key-description (this-command-keys-vector))
    ;;          events
    ;;          (if (equal events [-1]) "-1" (key-description events))
    ;;          last-input-event
    ;;          last-command-event
    ;;          last-nonmenu-event
    ;;          (if (equal events [-1]) "-1"
    ;;            (key-description (vconcat keys-vector events)))
    ;;          in-key-combo
    ;;          (vconcat keys-vector events))
    (cond
     ((or (not key-combo-mode)
          (minibufferp)
          isearch-mode
          (and (key-combo-comment-or-stringp)
               (not in-key-combo)))
      (when in-key-combo
        (message "finish1")
        (setq buffer-undo-list
              (append buffer-undo-list key-combo-original-undo-list)))
      (my-unread-events events)
      )
     ;; already start
     ((and in-key-combo (key-combo-key-binding (vconcat keys-vector events)))
      ;; keep
      (undo-boundary)
      (primitive-undo
       (+ 1 (key-combo-count-boundary buffer-undo-list)) buffer-undo-list)
      (my-unread-events
       (key-combo-make-key-vector (vconcat keys-vector events)))
      ;; http://www.update.uu.se/~ams/public_html/emacs/test/automated/undo-tests.el
      )
     ;; loop
     ((and in-key-combo
           (not (key-combo-key-binding (vconcat keys-vector events)))
           ;; for 1 key eg.<key-combo> SPC SPC
           (not (eq (length (vconcat keys-vector events)) 2))
           (key-combo-key-binding events)
           ;; all same key
           (equal [] (delete (aref (vconcat keys-vector events) 0)
                             (vconcat keys-vector events))))

      ;; finish
      (undo-boundary)
      (primitive-undo
       (+ 1 (key-combo-count-boundary buffer-undo-list)) buffer-undo-list)

      ;; (primitive-undo 1 buffer-undo-list)
      (my-unread-events (key-combo-make-key-vector events))
      )
     ((and in-key-combo
           (not (key-combo-key-binding (vconcat keys-vector events)))
           (key-binding (vconcat keys-vector events)))
      ;; finish
      ;; fall back prefix
      ;; Todo: multiple prefix
      (undo-boundary)
      (primitive-undo
       (+ 1 (key-combo-count-boundary buffer-undo-list)) buffer-undo-list)
      (my-unread-events (vconcat keys-vector events))
      )
     ((key-combo-key-binding events)
      ;; start
      (message "start")
      (setq key-combo-original-undo-list buffer-undo-list
            buffer-undo-list nil)
      (key-combo-set-start-position (cons (point) (window-start)))
      ;;enables cancel for insertion
      (when (memq (key-binding events)
                  '(self-insert-command skk-insert))
          ;; (eq (key-binding events) 'self-insert-command)
        (undo-boundary)
        (setq last-command-event (aref events 0))
        (call-interactively (key-binding events))
        (undo-boundary)
        (primitive-undo
         (+ 1 (key-combo-count-boundary buffer-undo-list)) buffer-undo-list)
        )
      (my-unread-events (key-combo-make-key-vector events))
      )
     (in-key-combo
       ;; finish
      (setq buffer-undo-list
             (append buffer-undo-list key-combo-original-undo-list))
      (message "finish2")
      (my-unread-events events)
      )
     (t
      ;; no key combo
      (my-unread-events events)
      )
     )
    ;; (message "2 thi:%s rev:%S ev:%s li:%S lc:%S"
    ;;          (key-description (this-command-keys-vector))
    ;;          events
    ;;          (if (equal events [-1]) "-1" (key-description events))
    ;;          last-input-event
    ;;          last-command-event)
    ;;(my-unread-events events)
    (reset-this-command-lengths)
    ;;(vconcat keys-vector events)
    )
  )

;; (local-set-key
;;  (key-combo-make-key-vector (kbd ";"))
;;  (lambda ()
;;    (interactive)
;;    (insert ";; ")
;;    ;;(message "kc[M-a]")
;;    )
;;  )

;; (dont-compile
;;   (when (fboundp 'describe)
;;     (describe ("key-combo in temp-buffer" :vars ((mode)))
;;       (around
;;         ;; (setq key-combo-command-keys nil)
;;         (with-temp-buffer
;;           (switch-to-buffer (current-buffer))
;;           (buffer-enable-undo)
;;           (emacs-lisp-mode)
;;           (let (;; (key-combo-prefix-mode-map (make-sparse-keymap))
;;                 ;; (key-combo-prefix-mode-map-alist nil)
;;                 (global-map-org (current-global-map))
;;                 (global-map
;;                  (let ((map (make-sparse-keymap)))
;;                    (set-keymap-parent map (current-global-map))
;;                    map)))
;;             (unwind-protect
;;                 (progn
;;                   (use-global-map global-map)
;;                   (funcall el-spec:example))
;;               (use-global-map global-map-org)))))
;;       (it ()
;;         (should (eq key-combo-mode nil)))
;;       (it ()
;;         (should (string= (key-combo-test-helper-execute "=") "= ")))
;;       (it ()
;;         (should (string= (key-combo-test-helper-execute "SPC") " ")))
;;       (it ()
;;         (should (string= (key-combo-test-helper-execute "= =") "eq ")))
;;       (it ()
;;         (should (string= (key-combo-test-helper-execute "= = =") "equal ")))
;;       (it ()
;;         (should (string= (key-combo-test-helper-execute "= = = =") "= ")))
;;       (it ()
;;         (should (string= (key-combo-test-helper-execute "M-a a") "kc[M-a a]")))
;;       (it ()
;;         ;; (should (string= (key-combo-test-helper-execute "M-a") "kc[M-a]"))
;;         ;; (should (string= (key-combo-test-helper-execute "a") "kc[M-a a]"))
;;         )
;;       (it ()
;;         (should (string= (key-combo-test-helper-execute "M-a") "kc[M-a]")))
;;       ;; "kc[M-a]b"
;;       (it ()
;;         (should (string= (key-combo-test-helper-execute "M-a b") "n[M-a b]")))
;;       (it ()
;;         (should (string= (key-combo-test-helper-execute ";") ";; ")))
;;       (it ()
;;         (should (eq (car (key-binding (key-combo-make-key-vector (kbd ";"))))
;;                     'lambda)))
;;       )))

(require 'ert)
(require 'el-spec)
(require 'el-spy)

(defun key-combo-test-helper-define-lookup (cmd)
  (key-combo-define-global ">>" cmd)
  (key-combo-key-binding ">>"))

(defun key-combo-test-helper-binding-execute (cmd)
  (key-combo-command-execute (key-combo-key-binding cmd))
  (substring-no-properties (buffer-string)))

(defun test1 ()
  (interactive)
  (insert "test1")
  )

(defun test2 ()
  (interactive)
  (insert "test2")
  )

(dont-compile
  (when (fboundp 'describe)
    (describe ("key-combo in temp-buffer" :vars ((mode)))
      (shared-examples "check post-command-hook"
        (it ()
          (key-combo-mode 1)
          (should (memq 'my-key-combo-post-command-function
                        post-command-hook)))
        (it ()
          (key-combo-mode -1)
          (should-not (memq 'my-key-combo-post-command-function
                            post-command-hook))))
      (shared-examples "C-a"
        (before
          (insert "B\n IP")
          (key-combo-mode 1))
        (it ()
          (should (key-combo-key-binding (kbd "C-a C-a"))))
        ;; (it ()
        ;;   (key-combo-mode -1)
        ;;   (should-not (key-combo-key-binding (kbd "C-a C-a"))))
        (it ()
          (key-combo-test-helper-execute "C-a")
          (should (equal (char-to-string (following-char)) "I"))
          (should (eq real-last-command 'back-to-indentation))
          )
        (it ()
          (key-combo-test-helper-execute "C-a C-a")
          (should (equal (char-to-string (following-char)) " "))
          (should (eq real-last-command 'move-beginning-of-line))
          )
        (it ()
          (key-combo-test-helper-execute "C-a C-a C-a")
          (should (equal (char-to-string (following-char)) "B"))
          (should (eq real-last-command 'beginning-of-buffer))
          )
        (it ()
          (key-combo-test-helper-execute "C-a C-p"))
        ;; fail in temp buffer?
        ;; (it (:vars ((cmd "C-a C-a C-a C-a")))
        ;;   (backward-char)
        ;;   (should (equal (char-to-string (following-char)) "P")))
        )

      (around
        (with-temp-buffer
          (switch-to-buffer (current-buffer))
          (let ((global-map-org (current-global-map))
                (global-map
                 (let ((map (make-sparse-keymap)))
                   (set-keymap-parent map (current-global-map))
                   map)))
            (unwind-protect
                (progn
                  (use-global-map global-map)
                  (funcall el-spec:example))
              (use-global-map global-map-org)))))

      (it "is key-combo element"
        (should (key-combo-elementp ">"))
        (should (key-combo-elementp '(lambda() (interactive) ())))
        (should (key-combo-elementp 'nil))
        (should (key-combo-elementp 'self-insert-command)))
      (it "is not key-combo element"
        (should-not (key-combo-elementp '(">")))
        (should-not (key-combo-elementp '((lambda() (interactive) ()))))
        (should-not (key-combo-elementp '(nil)))
        (should-not (key-combo-elementp '(self-insert-command)))
        (should-not (key-combo-elementp 'wrong-command)))
      (it "can define & lookup"
        (should (key-combo-test-helper-define-lookup '(lambda()(interactive)())))
        (should (key-combo-test-helper-define-lookup ">"))
        (should (key-combo-test-helper-define-lookup 'self-insert-command))
        (should (key-combo-test-helper-define-lookup '((lambda()(interactive)()))))
        (should (key-combo-test-helper-define-lookup '(">")))
        (should (key-combo-test-helper-define-lookup '(self-insert-command)))
        (should (key-combo-test-helper-define-lookup '(">" ">")))
        (should (key-combo-test-helper-define-lookup '(">" (lambda()(interactive)()))))
        (should (key-combo-test-helper-define-lookup '((lambda()(interactive)()) ">")))
        (should
         (key-combo-test-helper-define-lookup '((lambda()(interactive)()) (lambda()(interactive)()))))
        (should
         (key-combo-test-helper-define-lookup '(">" self-insert-command)))
        (should
         (key-combo-test-helper-define-lookup '(self-insert-command ">")))
        (should
         (key-combo-test-helper-define-lookup
          '(self-insert-command self-insert-command)))
        )
      ;;(include-examples "invalid examples name")
      (include-examples "check post-command-hook")
      (include-examples "C-a")

      (it "undo"
       ;; with-current-buffer (get-buffer-create "hoge")
        (erase-buffer)
        (buffer-disable-undo)
        (buffer-enable-undo)
        (insert "init:")
        (undo-boundary)
        (setq buffer-undo-list nil)

        (insert "1")
        (undo-boundary)
        (should (string= (buffer-string) "init:1"))

        (should (= (key-combo-count-boundary buffer-undo-list) 1))
        (primitive-undo 2 buffer-undo-list)
        (undo-boundary)
        (should (string= (buffer-string) "init:"))
        (setq buffer-undo-list (cdr buffer-undo-list))
        (insert "2")
        (undo-boundary)
        (should (string= (buffer-string) "init:2"))

        (should (= (key-combo-count-boundary buffer-undo-list) 2))
        (primitive-undo 3 buffer-undo-list)
        (undo-boundary)

        (should (string= (buffer-string) "init:"))
        (setq buffer-undo-list (cdr buffer-undo-list))
        (insert "3")
        (undo-boundary)
        (should (string= (buffer-string) "init:3"))
        (should (= (point) 7))

        (should (= (key-combo-count-boundary buffer-undo-list) 3))
        (primitive-undo 4 buffer-undo-list)
        (should (string= (buffer-string) "init:"))
        (insert "4")
        (undo-boundary)
        (should (string= (buffer-string) "init:4"))
        (should (= (point) 7))

        (undo)
        (should (string= (buffer-string) "init:3"))
        (should (= (point) 7))

        (undo-more 1)
        (should (string= (buffer-string) "init:2"))
        (should (= (point) 7))

        (undo-more 1)
        (should (string= (buffer-string) "init:1"))
        (should (= (point) 7))
        )

      (context "in default-mode"
        (context "with mock"
          (when (require 'el-mock nil t)
            (context "prefix"
              (before
                (buffer-enable-undo)
                (key-combo-mode 1))
              (it ()
                (with-el-spy
                  (defmock test3 () (interactive))
                  (define-prefix-command 'test-map)
                  (global-set-key (kbd "M-s") 'test-map)
                  (global-set-key (kbd "M-s z") 'test3)
                  (execute-kbd-macro (kbd "M-s z"))
                  (should (eq (el-spy:called-count 'test3) 1))
                  )
                )
              (it ()
                ;; no error
                (with-el-spy
                  (defmock test1 () (interactive) (insert "test1"))
                  (defmock test2 () (interactive) (insert "test2"))
                  (defmock test3 () (interactive) (insert "test3"))
                  (define-prefix-command 'test-map)
                  (global-set-key (kbd "M-s") 'test-map)
                  (define-key test-map (kbd "z")
                    'test3)
                  (define-key global-map
                    (key-combo-make-key-vector (kbd "M-s"))
                    'test1)
                  (define-key global-map
                    (key-combo-make-key-vector (kbd "M-s a"))
                    'test2)
                  (should (string= (key-combo-test-helper-execute "M-s a")
                                   "test2"))
                  (should (eq (el-spy:called-count 'test1) 1))
                  (should (eq (el-spy:called-count 'test2) 1))
                  (should (eq (el-spy:called-count 'test3) 0))
                  )
                )
              (it ()
                ;; no error
                (with-el-spy
                  (defmock test1 () (interactive) (insert "test1"))
                  (defmock test2 () (interactive) (insert "test2"))
                  (defmock test3 () (interactive) (insert "test3"))
                  (define-prefix-command 'test-map)
                  (global-set-key (kbd "M-s") 'test-map)
                  (define-key test-map (kbd "z")
                    'test3)
                  (define-key global-map
                    (key-combo-make-key-vector (kbd "M-s"))
                    'test1)
                  (define-key global-map
                    (key-combo-make-key-vector (kbd "M-s a"))
                    'test2)
                  (should (string= (key-combo-test-helper-execute "M-s")
                                   "test1"))
                  (should (eq (el-spy:called-count 'test1) 1))
                  (should (eq (el-spy:called-count 'test2) 0))
                  (should (eq (el-spy:called-count 'test3) 0))
                  )
                )
              (it ()
                ;; no error
                (with-el-spy
                  (defmock test1 () (interactive) (insert "test1"))
                  (defmock test2 () (interactive) (insert "test2"))
                  (defmock test3 () (interactive) (insert "test3"))
                  (define-prefix-command 'test-map)
                  (global-set-key (kbd "M-s") 'test-map)
                  (define-key test-map (kbd "z")
                    'test3)
                  (define-key global-map
                    (key-combo-make-key-vector (kbd "M-s"))
                    'test1)
                  (define-key global-map
                    (key-combo-make-key-vector (kbd "M-s a"))
                    'test2)
                  (should (string= (key-combo-test-helper-execute "M-s z")
                                   "test3"))
                  (should (eq (el-spy:called-count 'test1) 1))
                  (should (eq (el-spy:called-count 'test2) 0))
                  (should (eq (el-spy:called-count 'test3) 1))
                  )
                )
              (it ("multiple prefix")
                ;; "a M-s"
                )
              (it ("multiple prefix2")
                ;; "M-s M-s"
                )
              (it ("define key")
                ;; "M-s M-s"
                )
              )
            (it ()
              (should-error
               (with-mock
                 (mock (test1 *) :times 1)
                 (key-combo-define-global (kbd "M-C-d") '(test1 test2)))))
            (it ()
              ;; no error
              (with-el-spy
                (defmock test1 () (interactive))
                (defmock test2 () (interactive))
                (key-combo-mode 1)
                (key-combo-define-global (kbd "M-C-d") 'test1)
                (execute-kbd-macro (kbd "M-C-d"))
                ;; (should (eq (el-spy:called-count 'test1) 1))
                ))
            (it ()
              ;; no error
              (with-el-spy
                (defmock test1 () (interactive))
                (defmock test2 () (interactive))
                (key-combo-mode 1)
                (key-combo-define-global (kbd "M-C-d") '(test1 test2))
                (execute-kbd-macro (kbd "M-C-d"))
                ;; (should (eq (el-spy:called-count 'test1) 1))
                ))
            (it ()
              ;; no error
              (with-el-spy
                (defmock test1 () (interactive))
                (defmock test2 () (interactive))
                (key-combo-mode 1)
                (key-combo-define-global (kbd "M-C-d") '(test1 test2))
                (execute-kbd-macro (kbd "M-C-d M-C-d"))
                (should (eq (el-spy:called-count 'test1) 1))
                (should (eq (el-spy:called-count 'test2) 1))
                ))
            (it ()
              ;; no error
              (with-el-spy
                (defmock define-key (keymap key def) 0)
                (use-local-map (make-sparse-keymap))
                (key-combo-define-local "a" "a")
                (should (eq (el-spy:called-count 'define-key) 1))))
            (it ()
              ;; no error
              (with-el-spy
                (defmock define-key (keymap key def) 0)
                ;; (mock (define-key * * *) :times 1)
                (use-local-map (make-sparse-keymap))
                (key-combo-define-local "a" '("a"))
                (should (eq (el-spy:called-count 'define-key) 1))))
            (it ()
              ;; no error
              (with-el-spy
                (defmock define-key (keymap key def) 0)
                (use-local-map (make-sparse-keymap))
                ;; (mock (define-key * * *) :times 3);; 1 for recursive call?
                (key-combo-define-local "a" '("a" "b"))
                (should (eq (el-spy:called-count 'define-key) 2))))
            (it ()
              ;; no error
              (with-el-spy
                (defmock define-key (keymap key def) 0)
                (defmock lookup-key (keymap key) t)
                (use-local-map (make-sparse-keymap))
                ;; (mock (lookup-key * *) => t :times 2)
                ;; (mock (define-key * * *) :times 2);; 1 for recursive call?
                (key-combo-define-local "a" '("a" "b"))
                (should (eq (el-spy:called-count 'define-key) 2))
                (should (eq (el-spy:called-count 'lookup-key) 4))
                )))
          )
        (context "execute"
          (it ()
            (should (string= (key-combo-test-helper-execute ">") ">")))
          (it ()
            (should (string= (key-combo-test-helper-execute "=") "="))))
        (context "no execute"
          (it ()
            (key-combo-command-execute (lambda () (insert "a")))
            (should (string= (buffer-string) "a")))
          (it ()
            (should-error (key-combo-command-execute 'wrong-command)))
          (it ()
            (let ((last-command-event ?b))
              (key-combo-command-execute 'self-insert-command))
            (should (string= (buffer-string) "b")))
          (it ()
            (key-combo-command-execute (key-combo-get-command "a"))
            (should (string= (buffer-string) "a")))
          (it ()
            (key-combo-command-execute (key-combo-get-command "a`!!'a"))
            (should (string= (buffer-string) "aa"))
            (should (eq (point) 2)))
          (it ()
            (buffer-enable-undo)
            (let ((key-combo-undo-list))
              (key-combo-command-execute (lambda() (insert "a")))
              (key-combo-undo))
            (should (string= (buffer-string) "")))
          (it ()
            (buffer-enable-undo)
            (let ((key-combo-undo-list))
              (key-combo-command-execute (key-combo-get-command "a`!!'a"))
              (key-combo-undo))
            (should (string= (buffer-string) "")))
          (it ()
            (should-error (key-combo-define-global "a" 'wrong-command)))
          (it ()
            (should (key-combo-define-global "a" 'self-insert-command)))
          (it ()
            (should (eq (key-combo-define-global "a" nil) nil)))
          (it ()
            (should (eq (key-combo-define-global (kbd "C-M-g") nil) nil)))))
      (context "in emacs-lisp-mode"
        (before
          (buffer-enable-undo)
          (emacs-lisp-mode))
        (it ()
          (key-combo-define-global (kbd "M-s") "a"))
        (it ()
          (should-not (key-combo-comment-or-stringp)))
        (it ()
          (insert "\"")
          (should (key-combo-comment-or-stringp)))
        (it ()
          (insert ";")
          (should (key-combo-comment-or-stringp)))
        (it ()
          (insert ";\n")
          (should-not (key-combo-comment-or-stringp)))
        (context "isearch-mode"
          (it ()
            (insert "=")
            (should (string= (buffer-string) "="))
            (should (eq (point) 2)))
          (it ()
            (insert "=");; not to raise error from isearch-search
            (isearch-mode nil);; backward search
            (execute-kbd-macro "=")
            (should (string= (buffer-string) "="))
            (should (eq (point) 1))))
        (context "execute only"
          (it ()
            (should (string= (key-combo-test-helper-execute "=") "= ")))
          (it ()
            (should (string= (key-combo-test-helper-execute "= =") "eq ")))
          (it ()
            (should (string= (key-combo-test-helper-execute "=") "= "))
            (undo -1)
            (should (string= (buffer-string) "="))
            )
          (it ()
            (buffer-enable-undo)
            (should (null buffer-undo-list))
            (should (string= (key-combo-test-helper-execute "= =") "eq "))
            ;; (primitive-undo 1 buffer-undo-list)
            (undo -1)
            (should (string= (buffer-string) "= "))
            )
          (it ()
            (should (string= (key-combo-test-helper-execute ",") ",")))
          (it ()
            (should (string= (key-combo-test-helper-execute ",,") ",,")))
          (it ()
            (should (string= (key-combo-test-helper-execute ".") ".")))
          (it ()
            (should (string= (key-combo-test-helper-execute ". SPC") " . ")))
          (it ()
            (should (string= (key-combo-test-helper-execute ";") ";; ")))
          (it ()
            (should (string= (key-combo-test-helper-execute ";.") ";; .")))
          (it ()
            (should (string= (key-combo-test-helper-execute ";,") ";; ,")))
          (it ()
            (insert ";")
            (should (string= (key-combo-test-helper-execute "=") ";=")))
          )
        (context "in skk-mode"
          (when (require 'skk-autoloads nil t)
            (before
              (skk-mode 1)
              (setq this-command 'skk-insert)
              (insert ";")
              )
            (it ()
              (should (string= (key-combo-test-helper-execute ",") ";、")))
            (it ()
              (should (string= (key-combo-test-helper-execute ".") ";。")))
            )
          )
        (context ("insert & move & execute" :vars (pos pre-string))
          (it ()
            (insert "\"")
            (should (string= (key-combo-test-helper-execute "=") "\"=")))
          (it ()
            (insert ";")
            (should (string= (key-combo-test-helper-execute "=") ";=")))
          (it ()
            (insert ";")
            (should (string= (key-combo-test-helper-execute ",") ";,")))
          (it ()
            (insert ";\n")
            (should (string= (key-combo-test-helper-execute ";") ";\n;; ")))
          (it ()
            (insert ";")
            (should (string= (key-combo-test-helper-execute ".") ";.")))
          (it ()
            (insert "\"\"\n")
            (goto-char 3)
            (should (string= (key-combo-test-helper-execute ".") "\"\".\n")))
          (it ()
            (insert "\"\"a")
            (goto-char 3)
            (should (string= (key-combo-test-helper-execute ".") "\"\".a")))
          (it ()
            (insert "\"\"")
            (goto-char 3)
            (should (string= (key-combo-test-helper-execute ".") "\"\".")))
          (it ()
            (insert "\"\"")
            (goto-char 2)
            (should (string= (key-combo-test-helper-execute ".") "\".\"")))
          (it ()
            (insert "a\"\"")
            (goto-char 2)
            (should (string= (key-combo-test-helper-execute ".") "a.\"\"")))
          )
        (include-examples "C-a")
        (include-examples "check post-command-hook"))
      (context "in ruby"
        (before
          (buffer-enable-undo)
          (key-combo-mode 1)
          (ruby-mode)
          (when (boundp 'auto-complete-mode)
            (auto-complete-mode -1)))
        ;; bug?for auto-complete completion
        (it ()
          (should (string= (key-combo-test-helper-execute ".") ".")))
        (it ()
          (should (string= (key-combo-test-helper-execute "..") "..")))
        (it ()
          (should (string= (key-combo-test-helper-execute "...") "...")))
        (it ()
          (should (string= (key-combo-test-helper-execute "!~") " !~ ")))
        (it ()
          (should (string= (key-combo-test-helper-execute "**") "**")))
        (it ()
          (should (string= (key-combo-test-helper-execute "||=") " ||= "))))
      (context "in c-mode"
        (before
          (buffer-enable-undo)
          ;; (key-combo-mode 1)
          (c-mode))
        (context "execute+"
          (it ()
            (should (string= (key-combo-test-helper-execute "+") " + ")))
          (it ()
            (should (string= (key-combo-test-helper-execute "++") "++")))
          ;; (it ()
          ;;   (should (string= (key-description "+") "+")))
          (it ()
            (should (equal (listify-key-sequence "+") '(43))))
          (it ()
            (should (string= (key-description '(?+)) "+")))
          (it ()
            (should (equal (key-combo-make-key-vector '(?+))
                           ;;(vector 'key-combo (intern (key-description )))
                           [key-combo _+])))
          (it ("a")
            (should (not (null (key-binding
                                (key-combo-make-key-vector '(?+))
                                )))))
          (it ("c")
            (should (not (null (lookup-key
                                (current-local-map)
                                (key-combo-make-key-vector '(?+))
                                )))))
          (it ("b")
            (should (not (equal (key-binding
                                 (key-combo-make-key-vector '(?+)))
                                'key-combo-execute-original))))
          (it ()
            (should (not (null (key-combo-get-command "+")))))
          (it ()
            (should (not (equal (key-combo-get-command "+")
                                'key-combo-execute-original))))
          (it ("d")
            (key-combo-define-local "a" nil)
            ;; (key-combo-key-binding "a")
            ;; (key-binding (vector 'key-combo (intern (key-description "a"))))
            ;; accept-default bug?
            (should (eq (lookup-key (current-local-map)
                                    (key-combo-make-key-vector '(?a)))
                        nil))
            (key-combo-define-local "a" "a")
            (should (not (equal (lookup-key (current-local-map)
                                            (key-combo-make-key-vector '(?a)))
                                nil)))
            (key-combo-define-local "a" nil)
            )
          )
        (context "undo"
          (before
            (buffer-enable-undo))
          (it ()
            (should (string= (key-combo-test-helper-execute "=") " = ")))
          (it ()
            (should (string= (key-combo-test-helper-execute "=") " = "))
            (undo -1)
            (should (string= (buffer-string) "="))
            )
          (it ()
            (should (string= (key-combo-test-helper-execute "= C-x u") "=")))
          (it ()
            (should (string= (key-combo-test-helper-execute "== C-x u") " = ")))
          )
        (context "execute"
          (it ()
            (should (string= (key-combo-test-helper-execute "=") " = ")))
          (it ()
            (should (string= (key-combo-test-helper-execute "=*") " =* ")))
          (it ()
            (should (string= (key-combo-test-helper-execute "==") " == ")))
          (it ()
            (should (string= (key-combo-test-helper-execute "===") " === ")))
          (it "loop"
            (should (string= (key-combo-test-helper-execute "====") " = ")))
          (it ()
            (should (string= (key-combo-test-helper-execute "=>=") " => = ")))
          ;; (it ()
          ;; (should (string= (key-combo-test-helper-execute "==!") " ==! ")))
          (it ()
            (should (string= (key-combo-test-helper-execute "=>") " => ")))
          (it ()
            (should (string= (key-combo-test-helper-execute "/") "/")))
          (it ()
            (should (string= (key-combo-test-helper-execute "/ SPC") " / ")))
          (it ()
            (should (string= (key-combo-test-helper-execute "*") "*")))
          (it ()
            (should (string= (key-combo-test-helper-execute "**") "**")))
          (it ()
            (should (string= (key-combo-test-helper-execute "->") "->")))
          (it ()
            (should (string= (key-combo-test-helper-execute ".") ".")))
          ;; todo check position
          (it ()
            (should (string= (key-combo-test-helper-execute "/* RET")
                             "/*\n  \n */")))
          ;; todo depend on indent width
          ;; (it ()
          ;; (should (string= (key-combo-test-helper-execute "{ RET") "{\n  \n}"))
          )
        (context "funcall"
          ;; (before
          ;;   (key-combo-command-execute (key-combo-key-binding lookup-cmd)))
          (it ()
            (should (string=
                     (key-combo-test-helper-binding-execute "=") " = ")))
          (it ()
            (should (string=
                     (key-combo-test-helper-binding-execute "==") " == ")))
          (it ()
            (should (string=
                     (key-combo-test-helper-binding-execute [?=]) " = ")))
          (it ()
            (should (string=
                     (key-combo-test-helper-binding-execute [?= ?=]) " == ")))
          (it ()
            (should (string=
                     (key-combo-test-helper-binding-execute [?= ?>]) " => ")))
          (it ()
            (should (string=
                     (key-combo-test-helper-binding-execute [?= ?= ?=])
                     " === ")))
          ;; (it ()
          ;;   (funcall (key-combo-key-binding [?= ?= ?= ?=]))
          ;;   (should (string= (buffer-string) " ==== ")))
          (it ()
            (key-combo-define-global (kbd "C-M-h") " == ")
            (key-combo-command-execute (key-combo-key-binding (kbd "C-M-h")))
            (should (equal (buffer-string) " == ")))
          (it ()
            (should-not
             (equal
              (key-combo-lookup-key (current-global-map) (kbd "C-M-h"))
              " == ")))
          ;; (it ()
          ;;   (key-combo-define-global (kbd "C-M-h C-M-h") " === ")
          ;;   (execute-kbd-macro (kbd "C-M-h C-M-h"))
          ;;   (should (string= (buffer-string) " === "))
          ;;   )
          (it ()
            (key-combo-define-global (kbd "C-M-h C-M-h") " === ")
            (key-combo-command-execute
             (key-combo-key-binding (kbd "C-M-h C-M-h")))
            (should (string= (buffer-string) " === "))
            )
          (it ()
            (should-not (key-combo-key-binding [?= ?= ?= ?=])))
          (it ()
            (insert "a  ")
            (should (string= (key-combo-test-helper-execute "=") "a  = "))
            (should (string= (buffer-string) "a  = ")))
          )
        )
      )))
;; (recursive-edit)

