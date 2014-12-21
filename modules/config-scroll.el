(eval-when-compile (require 'cl))

(setq
  mouse-wheel-scroll-amount '(3 ((shift) . 1))
  redisplay-dont-pause t
  mouse-wheel-progressive-speed nil
  mouse-wheel-follow-mouse t
  smooth-scroll-margin 5
  scroll-margin 5
  scroll-step 1
  auto-window-vscroll nil
  scroll-conservatively 1000)

;; scroll-margin is reset at start for some reason
(add-hook 'emacs-startup-hook
  (lambda ()
    (setq scroll-margin 5)))

(add-hook 'window-size-change-functions
  (lambda (frame)
    (if (< (window-body-height) 10)
      (setq scroll-margin (max 0 (1-  (/ (window-body-height) 2))))
      (setq scroll-margin 5))))

(global-set-key (kbd "<left-margin> <mouse-5>")
  (kbd "<mouse-5> <mouse-5> <mouse-5>"))
(global-set-key (kbd "<left-margin> <mouse-4>")
  (kbd "<mouse-4> <mouse-4> <mouse-4>"))

(provide 'config-scroll)
