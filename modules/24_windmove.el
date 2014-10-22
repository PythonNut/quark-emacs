;;; =====================================
;;; Windmove - effortless window movement
;;; =====================================
(windmove-default-keybindings)

(defadvice make-frame-command (before load-framemove activate)
  (progn
    (require 'framemove)
    (setq framemove-hook-into-windmove t)))

(defadvice windmove-left (around try-elscreen activate)
  (if (and
        (= (length (window-list)) 1)
        (= (length (frame-list)) 1))
    (if (and (elscreen-get-screen-list)
          (> (length (elscreen-get-screen-list)) 1))
      (elscreen-previous)
      ad-do-it)
    ad-do-it))

(defadvice windmove-right (around try-elscreen activate)
  (if (and
        (= (length (window-list)) 1)
        (= (length (frame-list)) 1))
    (if (and (elscreen-get-screen-list)
          (> (length (elscreen-get-screen-list)) 1))
      (elscreen-next)
      (if (y-or-n-p-with-timeout "Start elscreen? " 2 nil)
        (elscreen-next)
        ad-do-it))
    ad-do-it))

