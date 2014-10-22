;;; =======================================
;;; Flyspell - inline real time spell check
;;; =======================================
;; text mode
(eval-after-load 'flyspell-mode
  '(progn
     (when (locate-file "hunspell" exec-path)
       (setq ispell-program-name "hunspell")
       (add-to-list 'ispell-extra-args "--sug-mode=ultra"))
     (set 'flyspell-issue-message-flag nil)))

(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

