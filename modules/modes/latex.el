;;; ============================================
;;; AcuTeX - the most powerful LaTeX editor ever
;;; ============================================
(defun ac-latex-mode-setup ()
  (require 'tex-site)
  (require 'auto-complete-auctex)
  (require 'ac-math)

  (setq-default TeX-master nil)
  (setq TeX-parse-self t)
  (setq TeX-auto-save t)

  (add-to-list 'ac-modes 'latex-mode)
  (setq ac-sources
    (append
      '(ac-source-math-unicode
         ac-source-math-latex
         ac-source-latex-commands)
      ac-sources))
  (ac-auctex-setup))

(add-hook 'LaTeX-mode-hook 'ac-latex-mode-setup)

