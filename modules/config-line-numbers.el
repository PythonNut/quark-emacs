(eval-when-compile
  (with-demoted-errors
    (require 'linum-relative)))

(with-eval-after-load 'linum
  (set-face-background 'linum nil)

  (package-activate 'linum-relative)
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


  (setq
    linum-relative-current-symbol ""
    linum-relative-format "%3s "
    linum-delay t)

  (set-face-attribute 'linum-relative-current-face nil
    :weight 'extra-bold
    :foreground nil
    :background nil
    :inherit '(hl-line default)))

(defun linum-cycle ()
  (interactive)
  (if (and (featurep 'linum) linum-mode)
    (if (eq linum-format 'dynamic)
      (linum-mode -1)
      (setq linum-format 'dynamic))
    (progn
      (linum-mode +1)
      (setq linum-format 'linum-relative))))

(global-set-key (kbd "C-c L") #'linum-cycle)
(global-set-key (kbd "C-c C-l") #'linum-cycle)

(provide 'config-line-numbers)
