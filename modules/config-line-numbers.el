(require 'linum)

(eval-when-compile
  (require 'cl)
  (require 'linum)
  (require 'linum-relative))

(add-hook 'prog-mode-hook
  (lambda ()
    (when (display-graphic-p)
      (linum-mode +1))))

(defun find-file-check-large-file ()
  "If a file is over a given size, make the buffer read only."
  (when (> (buffer-size) (* 1048576 5))
    (buffer-disable-undo)
    (size-indication-mode +1)
    (adaptive-wrap-prefix-mode -1)
    (linum-mode -1)))

(add-hook 'find-file-hooks #'find-file-check-large-file)

(with-eval-after-load 'linum
  (set-face-background 'linum nil)
  (set-face-foreground 'linum "grey51")
  (setq linum-delay t)

  (require 'linum-relative)

  ;; truncate current line to four digits
  (defun linum-relative (line-number)
    (let* ((diff1 (abs (- line-number linum-relative-last-pos)))
            (diff (if (minusp diff1)
                    diff1
                    (+ diff1 linum-relative-plusp-offset)))
            (current-p (= diff linum-relative-plusp-offset))
            (current-symbol (if (and linum-relative-current-symbol current-p)
                              (if (string= "" linum-relative-current-symbol)
                                (number-to-string (% line-number 1000))
                                linum-relative-current-symbol)
                              (number-to-string diff)))
            (face (if current-p 'linum-relative-current-face 'linum)))
      (propertize (format linum-relative-format current-symbol) 'face face)))


  (setq linum-relative-current-symbol "")
  (setq linum-relative-format "%3s ")

  (set-face-background 'linum-relative-current-face "grey15")
  (set-face-foreground 'linum-relative-current-face "grey70")

  (set-face-background 'linum-relative-current-face "#073642")
  (set-face-foreground 'linum-relative-current-face "#839496")

  (set-face-attribute 'linum-relative-current-face nil :weight 'extra-bold)

  (global-set-key (kbd "C-c L")
    (lambda ()
      (interactive)
      (if linum-mode (linum-relative-toggle)
        (linum-mode +1)))))

(provide 'config-line-numbers)
