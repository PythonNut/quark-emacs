(eval-when-compile '(require 'cl))

(setq
  mouse-wheel-scroll-amount '(3 ((shift) . 1))
  redisplay-dont-pause t
  mouse-wheel-progressive-speed nil
  mouse-wheel-follow-mouse t
  smooth-scroll-margin 5
  scroll-margin 5
  scroll-conservatively 0
  scroll-up-aggressively 0.01
  scroll-down-aggressively 0.01)

(setq-default
  scroll-up-aggressively 0.01
  scroll-down-aggressively 0.01)

;; scroll-margin is reset at start for some reason
(add-hook 'emacs-startup-hook
  '(lambda ()
     (setq scroll-margin 5)))

(add-hook 'window-size-change-functions
  '(lambda (frame)
     (if (< (window-body-height) 10)
       (setq scroll-margin (1-  (/ (window-body-height) 2)))
       (setq scroll-margin 5))))
