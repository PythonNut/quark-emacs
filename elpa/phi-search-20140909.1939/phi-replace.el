;;; phi-replace.el --- another replace command building on phi-search

;; Copyright (C) 2013 zk_phi

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA

;; Author: zk_phi
;; URL: http://hins11.yu-yake.com/
;; Version: 2.0.2

;;; Commentary:

;; Add following expression in your init file :
;;
;;   (require 'phi-replace)
;;
;; and bind command "phi-replace" or "phi-replace-query"
;;
;;   (global-set-key (kbd "M-%") 'phi-replace)

;; For more details, see "Readme".

;;; Change Log:

;; 1.0.0 first released
;; 1.0.1 added weight for phi-replace
;; 1.0.2 use "sublimity" not "nurumacs"
;; 1.0.3 better integration with sublimity
;; 1.0.4 added a hook
;; 1.0.5 added some commands
;; 1.0.6 better handling of narrowed buffer
;; 1.0.7 fixed bug on completing replace without matches
;; 1.0.8 use "remap" for default keybindings
;; 2.0.0 follow "phi-search" update
;;       changed the default value of phi-replace-weight
;; 2.0.1 added phi-replace-init-hook
;; 2.0.2 compatible with phi-search-core v1.2.0

;;; Code:

(require 'phi-search-core)

;; + constant

(defconst phi-replace-version "2.0.2")

;; + suppress byte-compiler

(declare-function sublimity--pre-command "sublimity")
(declare-function sublimity--post-command "sublimity")

;; + customs

(defcustom phi-replace-weight 0
  "weight for \"phi-replace\""
  :group 'phi-search)

(defcustom phi-replace-init-hook nil
  "hook run after initialization of phi-replace"
  :group 'phi-search)

(defcustom phi-replace-additional-keybinds '()
  "additional bindings used in phi-replace"
  :group 'phi-search)

;; + variables

(defvar phi-replace--original-restriction nil)
(make-variable-buffer-local 'phi-replace--original-restriction)

(defvar phi-replace--query-mode nil)
(make-variable-buffer-local 'phi-replace--query-mode)

;; + start/end phi-replace

(defvar phi-replace--mode-line-format
  '(" *phi-replace*"
    (:eval (phi-search--with-target-buffer
            (format " [ %d ]" (length phi-search--overlays))))))

(defun phi-replace--complete-function ()
  ;; if the query is blank, use the last query
  (when (and (string= (buffer-string) "")
             phi-search--last-executed)
    (insert phi-search--last-executed))
  (phi-search--with-target-buffer
   (when phi-search--overlays
     (let ((orig-cursor (make-overlay phi-search--original-position
                                      phi-search--original-position))
           (str (read-from-minibuffer "replace with ? ")))
       (dotimes (n (length phi-search--overlays))
         (phi-search--with-sublimity
          (phi-search--select n))
         (unless phi-replace--query-mode
           (sit-for phi-replace-weight))
         (let ((ov (nth n phi-search--overlays)))
           (if (and phi-replace--query-mode
                    (not (y-or-n-p (format "replace with %s ?" str))))
               (delete-overlay ov)
             (goto-char (overlay-start ov))
             (delete-region (overlay-start ov) (overlay-end ov))
             (insert str))))
       (goto-char (overlay-start orig-cursor))))
   (when phi-replace--original-restriction
     (let ((beg (car phi-replace--original-restriction))
           (end (cdr phi-replace--original-restriction)))
       (narrow-to-region (overlay-start beg) (overlay-start end))
       (delete-overlay beg)
       (delete-overlay end)))
   (setq phi-replace--original-restriction nil
         phi-replace--query-mode           nil)))

(defun phi-replace--initialize (&optional query)
  (setq phi-replace--query-mode query)
  ;; narrow to region
  (when (use-region-p)
    (setq phi-replace--original-restriction
          (cons
           (make-overlay (point-min) (point-min))
           (make-overlay (point-max) (point-max))))
    (narrow-to-region (region-beginning) (region-end))
    (deactivate-mark))
  (phi-search--initialize
   phi-replace--mode-line-format
   nil nil nil
   'phi-replace--complete-function)
  (run-hooks 'phi-replace-init-hook))

;; + commands

;;;###autoload
(defun phi-replace ()
  "replace command using phi-search"
  (interactive)
  (if (and (boundp 'popwin:popup-window)
           (eq (selected-window) popwin:popup-window))
      (call-interactively 'replace-regexp)
    (phi-replace--initialize nil)))

;;;###autoload
(defun phi-replace-query ()
  "replace command using phi-search"
  (interactive)
  (if (and (boundp 'popwin:popup-window)
           (eq (selected-window) popwin:popup-window))
      (call-interactively 'query-replace-regexp)
    (phi-replace--initialize t)))

;; + provide

(provide 'phi-replace)

;;; phi-replace.el ends here
