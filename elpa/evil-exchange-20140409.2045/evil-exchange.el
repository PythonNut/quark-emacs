;;; evil-exchange.el --- Exchange text more easily within Evil

;; Copyright (C) 2013-2014 by Dewdrops

;; Author: Dewdrops <v_v_4474@126.com>
;; URL: http://github.com/Dewdrops/evil-exchange
;; Version: 20140409.2045
;; X-Original-Version: 0.2
;; Keywords: evil, plugin
;; Package-Requires: ((evil "1.0.7") (cl-lib "0.3"))

;; This file is NOT part of GNU Emacs.

;;; License:
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Port of vim-exchange by Tom McDonald (https://github.com/tommcdo/vim-exchange)
;;
;; Installation:
;;
;; put evil-exchange.el somewhere in your load-path and add these
;; lines to your .emacs:
;; (require 'evil-exchange)
;; ;; change default key bindings (if you want) HERE
;; ;; (setq evil-exchange-key (kbd "zx"))
;; (evil-exchange-install)

;;; Code:

(require 'evil)

(defgroup evil-exchange nil
  "Easy text exchange operator for Evil."
  :prefix "evil-exchange"
  :group 'evil)

(defcustom evil-exchange-key (kbd "gx")
  "Default binding for evil-exchange."
  :type `,(if (get 'key-sequence 'widget-type)
              'key-sequence
            'sexp)
  :group 'evil-exchange)

(defcustom evil-exchange-cancel-key (kbd "gX")
  "Default binding for evil-exchange-cancel."
  :type `,(if (get 'key-sequence 'widget-type)
              'key-sequence
            'sexp)
  :group 'evil-exchange)

(defcustom evil-exchange-highlight-face 'highlight
  "Face used to highlight marked area."
  :type 'sexp
  :group 'evil-exchange)

(defvar evil-exchange-position nil "Text position which will be exchanged")

(defvar evil-exchange-overlays nil "Overlays used to highlight marked area")


(defun evil-exchange-highlight (beg end)
  (let ((o (make-overlay beg end nil t nil)))
    (overlay-put o 'face evil-exchange-highlight-face)
    (add-to-list 'evil-exchange-overlays o)))

(defun evil-exchange-remove-overlays ()
  (mapc 'delete-overlay evil-exchange-overlays)
  (setq evil-exchange-overlays nil))

;;;###autoload
(autoload 'evil-exchange "evil-exchange"
  "Exchange two regions with evil motion." t)

(evil-define-operator evil-exchange (beg end type)
  "Exchange two regions with evil motion."
  :move-point nil
  (interactive "<R>")
  (let ((beg-marker (copy-marker beg t))
        (end-marker (copy-marker end nil)))
    (if (null evil-exchange-position)
        ;; call without evil-exchange-position set: store region
        (progn
          (setq evil-exchange-position (list beg-marker end-marker type))
          ;; highlight area marked to exchange
          (if (eq type 'block)
              (evil-apply-on-block #'evil-exchange-highlight beg end nil)
            (evil-exchange-highlight beg end)))
      ;; secondary call: do exchange
      (cl-destructuring-bind
          (orig-beg orig-end orig-type) evil-exchange-position
        (cond
         ;; exchange block region
         ((and (eq orig-type 'block) (eq type 'block))
          (let ((orig-rect (delete-extract-rectangle orig-beg orig-end))
                (curr-rect (delete-extract-rectangle beg-marker end-marker)))
            (save-excursion
              (goto-char orig-beg)
              (insert-rectangle curr-rect)
              (goto-char beg-marker)
              (insert-rectangle orig-rect)))
          (setq evil-exchange-position nil)
          (evil-exchange-remove-overlays))
         ;; signal error if regions incompatible
         ((or (eq orig-type 'block) (eq type 'block))
          (error "Can't exchange block region with non-block region."))
         ;; exchange normal region
         (t
          (transpose-regions orig-beg orig-end beg end)
          (setq evil-exchange-position nil)
          (evil-exchange-remove-overlays))))))
  ;; place cursor on beginning of line
  (when (and (evil-called-interactively-p) (eq type 'line))
    (evil-first-non-blank)))

;;;###autoload
(defun evil-exchange-cancel ()
  "Cancel current pending exchange."
  (interactive)
  (setq evil-exchange-position nil)
  (evil-exchange-remove-overlays)
  (message "Exchange cancelled"))

;;;###autoload
(defun evil-exchange-install ()
  "Setting evil-exchange key bindings."
  (define-key evil-normal-state-map evil-exchange-key 'evil-exchange)
  (define-key evil-visual-state-map evil-exchange-key 'evil-exchange)
  (define-key evil-normal-state-map evil-exchange-cancel-key 'evil-exchange-cancel)
  (define-key evil-visual-state-map evil-exchange-cancel-key 'evil-exchange-cancel))

(provide 'evil-exchange)
;;; evil-exchange.el ends here
