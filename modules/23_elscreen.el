;;; ========================
;;; Elscreen - tabs in emacs
;;; ========================
(require 'elscreen)
(setq elscreen-prefix-key "\C-l")
(setq elscreen-display-tab             nil)
(setq elscreen-tab-display-kill-screen nil)
(setq elscreen-display-screen-number   nil)
(setq elscreen-tab-display-control     nil)

;; (defun elscreen-frame-title-update ()
;;   (when (elscreen-screen-modified-p 'elscreen-frame-title-update)
;;     (let* ((screen-list (sort (elscreen-get-screen-list) '<))
;;             (screen-to-name-alist (elscreen-get-screen-to-name-alist))
;;             (title (mapconcat
;;                      (lambda (screen)
;;                        (format "%s"
;;                          (if (string= (elscreen-status-label screen) "+")
;;                            (concat "[" (get-alist screen screen-to-name-alist) "]")
;;                            (get-alist screen screen-to-name-alist))))
;;                      screen-list " - ")))
;;       (if (fboundp 'set-frame-name)
;;         (set-frame-name title)
;;         (setq frame-title-format title)))))

(eval-after-load "elscreen"
  '(progn
     (elscreen-start)
     ;; (add-hook 'elscreen-screen-update-hook 'elscreen-frame-title-update)
     (setq elscreen-prefix-key "\C-l")

     (set-face-background 'elscreen-tab-background-face "grey65")
     (set-face-attribute 'elscreen-tab-background-face nil :underline nil)
     (set-face-foreground 'elscreen-tab-current-screen-face "white")
     (set-face-background 'elscreen-tab-current-screen-face "grey50")
     (set-face-attribute 'elscreen-tab-current-screen-face nil :underline 'nil)
     (set-face-foreground 'elscreen-tab-other-screen-face "black")
     (set-face-background 'elscreen-tab-other-screen-face "grey65")
     (set-face-attribute 'elscreen-tab-other-screen-face nil :underline 'nil)

     ;; automatically create new if switching to blank screen
     (defmacro elscreen-create-automatically-open (ad-do-it)
       `(if (not (= (length (elscreen-get-screen-list)) (+ (elscreen-get-current-screen) 1)))
          , ad-do-it
          (elscreen-create)
          (elscreen-notify-screen-modification 'force-immediately)
          (elscreen-message "New screen is automatically created")))

     (defmacro elscreen-create-automatically (ad-do-it)
       `(if (not (elscreen-one-screen-p))
          , ad-do-it
          (elscreen-create)
          (elscreen-notify-screen-modification 'force-immediately)
          (elscreen-message "New screen is automatically created")))

     (defadvice elscreen-next (around elscreen-create-automatically activate)
       (elscreen-create-automatically-open ad-do-it))

     (defadvice elscreen-previous (around elscreen-create-automatically activate)
       (elscreen-create-automatically ad-do-it))

     (defadvice elscreen-toggle (around elscreen-create-automatically activate)
       (elscreen-create-automatically ad-do-it))))

(autoload 'elscreen-next     "elscreen")
(autoload 'elscreen-previous "elscreen")

;; Alt+(PgUp|PgDown) switches between elscreens
(global-set-key (kbd "M-<prior>") 'elscreen-previous)
(global-set-key (kbd "M-<next>") 'elscreen-next)

