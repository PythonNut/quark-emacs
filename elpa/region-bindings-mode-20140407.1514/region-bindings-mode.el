;;; region-bindings-mode.el --- Enable custom bindings when mark is active.
;; Version: 20140407.1514

;; Copyright (C) 2012  Fabián E. Gallina
;; Author: Fabián E. Gallina <fabian@anue.biz>

;; URL: https://github.com/fgallina/region-bindings-mode
;; Created: Oct 2012
;; Package-Version: 1.0
;; Package-Requires: ()
;; Keywords: convenience

;; This file is NOT part of GNU Emacs.

;; region-bindings-mode is free software: you can redistribute it
;; and/or modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; region-bindings-mode is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied warranty
;; of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with region-bindings-mode.  If not, see
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Minor mode providing an extra keymap which is active when the
;; region is active. You can then add custom keybindings to the region
;; keymap, e.g. you could set "k" to call the command 'kill-region'
;; when the region is active.

;; This is a pretty good way to keep the global bindings clean.

;;; Installation:

;; Add this to your .emacs:

;; (add-to-list 'load-path "/folder/containing/file")
;; (require 'region-bindings-mode)
;; (region-bindings-mode-enable)

;; Alternatively, you can install this easily via MELPA through the
;; Emacs package manager. To add MELPA to the package archives:

;; (add-to-list 'package-archives
;;              '("melpa" . "http://melpa.milkbox.net/packages/") t)

;;; Usage:

;; Now that region-bindings-mode has been installed and initialized
;; all you need to do is to add keys to it, here's an example:

;; (define-key region-bindings-mode-map "g" 'keyboard-quit)

;; And as you would expect that will trigger a `keyboard-quit' when
;; pressing g, but this only happens when region is active.

;; If any of the bindings you define in region-bindings-mode-map leave
;; a region active (e.g. the various marking commands in
;; multiple-cursors) then you will probably want a binding which
;; temporarily disables region-bindings-mode for the currently active
;; region, so that you can type normally once you are finished using
;; the region-only bindings.  This can be achieved via:

;; (define-key region-bindings-mode-map "q" 'region-bindings-mode-off)

;; Then after pressing "q", the per-region bindings can be easily
;; reactivated by redefining or changing the active region.

;; You can have fine grained control on the situations where this mode
;; should not be enabled, the first is using the simple
;; `region-bindings-mode-disabled-modes' variable and the other is
;; using `region-bindings-mode-disable-predicates'.  This is just a
;; list of functions that receive no args and if any of them return
;; non-nil the mode is not enabled.

;; If you want to disable the mode completely, please use
;; `region-bindings-mode-disable'.

;;; Code:

(defgroup region-bindings-mode nil
  "Provide a dedicated keymap for use when the region is active."
  :group 'convenience)

(defvar region-bindings-mode-map
  (let ((region-bindings-mode-map (make-sparse-keymap)))
    region-bindings-mode-map)
  "Keymaps for command `region-bindings-mode-map'.")

(defcustom region-bindings-mode-disable-predicates nil
  "List of predicates that disable the mode.
Each function in the list receive no argument."
  :group 'region-bindings-mode)

(defcustom region-bindings-mode-enabled-modes nil
  "Modes where `region-bindings-mode' should activate.
Each value should correspond to the value of the `major-mode'
variable within the respective mode."
  :group 'region-bindings-mode
  :type '(repeat symbol))

(defcustom region-bindings-mode-disabled-modes nil
  "Modes where `region-bindings-mode' should not activate.
Each value should correspond to the value of the `major-mode'
variable within the respective mode."
  :group 'region-bindings-mode
  :type '(repeat symbol))

;;;###autoload
(define-minor-mode region-bindings-mode
  "Enable special bindings when working with an active region.

Do not invoke `region-bindings-mode' directly!

Toggling the mode on and off via this function will simply
enable/disable the bindings, but it will not honour
`region-bindings-mode-disabled-modes' or
`region-bindings-mode-disable-predicates', or toggle activation
of the hooks which automatically enable/disable the bindings when
the mark is activated or deactivated.

Instead, call `region-bindings-mode-enable' and
`region-bindings-mode-enable'."
  :lighter " rk" :group 'convenience)

(defun region-bindings-mode-on ()
  "Turn on region bindings mode.
Don't use this, use `region-bindings-mode-enable'."
  (and (or (not region-bindings-mode-enabled-modes)
           (memq major-mode region-bindings-mode-enabled-modes))
       (not (memq major-mode region-bindings-mode-disabled-modes))
       (not (catch 'disable
              (dolist (pred region-bindings-mode-disable-predicates)
                (and (funcall pred)
                     (throw 'disable t)))))
       (region-bindings-mode 1)))

(defun region-bindings-mode-off ()
  "Temporarily turn off region bindings mode.  It is useful to
bind this to a key in `region-bindings-mode-map' to temporarily
disable region bindings when the region is active.

To permanently turn off region bindings mode, instead use
`region-bindings-mode-disable'."
  (interactive)
  (region-bindings-mode -1))

(defun region-bindings-mode-enable ()
  "Add initialization hooks."
  (interactive)
  (add-hook 'activate-mark-hook 'region-bindings-mode-on)
  (add-hook 'deactivate-mark-hook 'region-bindings-mode-off))

(defun region-bindings-mode-disable ()
  "Remove initialization hooks and turn off."
  (interactive)
  (remove-hook 'activate-mark-hook 'region-bindings-mode-on)
  (remove-hook 'deactivate-mark-hook 'region-bindings-mode-off)
  (region-bindings-mode -1))

(provide 'region-bindings-mode)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; region-bindings-mode.el ends here
