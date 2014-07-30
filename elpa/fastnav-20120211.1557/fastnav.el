;;; fastnav.el --- Fast navigation and editing routines.

;; Copyright (C) 2008, 2009, 2010  Zsolt Terek <zsolt@google.com>
;; Copyright (C) 2010, 2011 Gleb Peregud <gleb.peregud@gmail.com>

;; Version: 20120211.1557
;; X-Original-Version: 1.0.7
;; Author: Zsolt Terek <zsolt@google.com>
;; Keywords: nav fast fastnav navigation
;; Compatibility: GNU Emacs 22, 23

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Commentary:
;; Inspired by zap-to-char, this library defines different routines operating on
;; the next/previous N'th occurrence of a character.  When invoking one of these
;; commands, the user is interactively queried for a character while the
;; potential target positions are highlighted.
;;
;; For example, META-s (jump-to-char-forward) highlights the next occurrences of
;; each character and prompts for one.  Once the user picks a char, the point is
;; moved to that position.  Subsequent invocations of META-s before picking a
;; character increases N, that is, the second, third, etc. occurrences are
;; highlighted and targeted.
;;
;; The fastnav-sprint-forward/backward commands apply iterative
;; jumping until return/C-G is hit, making it possible to reach any
;; point of the text with just a few keystrokes.
;;
;; To use it, simply put this file under ~/.emacs.d/, add (require 'fastnav) to
;; your emacs initialization file and define some key bindings, for example:
;;
;; (global-set-key "\M-z" 'fastnav-zap-up-to-char-forward)
;; (global-set-key "\M-Z" 'fastnav-zap-up-to-char-backward)
;; (global-set-key "\M-s" 'fastnav-jump-to-char-forward)
;; (global-set-key "\M-S" 'fastnav-jump-to-char-backward)
;; (global-set-key "\M-r" 'fastnav-replace-char-forward)
;; (global-set-key "\M-R" 'fastnav-replace-char-backward)
;; (global-set-key "\M-i" 'fastnav-insert-at-char-forward)
;; (global-set-key "\M-I" 'fastnav-insert-at-char-backward)
;; (global-set-key "\M-j" 'fastnav-execute-at-char-forward)
;; (global-set-key "\M-J" 'fastnav-execute-at-char-backward)
;; (global-set-key "\M-k" 'fastnav-delete-char-forward)
;; (global-set-key "\M-K" 'fastnav-delete-char-backward)
;; (global-set-key "\M-m" 'fastnav-mark-to-char-forward)
;; (global-set-key "\M-M" 'fastnav-mark-to-char-backward)
;;
;; (global-set-key "\M-p" 'fastnav-sprint-forward)
;; (global-set-key "\M-P" 'fastnav-sprint-backward)
;;
;; This library can be originally found at:
;;   http://www.emacswiki.org/emacs/FastNav
;; Package.el compatible version can be found at
;;   https://github.com/gleber/fastnav.el
;; it has been uploaded to ELPA and Marmalade

;;; Changes Log:
;;   2010-02-05: Fix for org mode, all commands were broken.
;;               Fix for electric characters in certain modes.
;;   2010-02-11: Yet another minor fix for switching to next/previous char.
;;   2010-05-28: Added sprint commands.
;;   2010-08-06: Make fastnav compatible with package.el
;;   2011-08-10: Add fastnav- prefix to autoload-ed functions
;;

;;; Code:

;;;###autoload
(defun fastnav-search-char-forward (arg char)
  "Moves to the arg-th occurrence of char forward (backward if N
is negative).  If there isn't room, go as far as possible (no
error)."
  (let ((old-case-fold-search case-fold-search))
    (setq case-fold-search nil)
    (if (< arg 0)
        (search-backward (char-to-string char) nil nil (- arg))
      (progn
        (forward-char 1)
        (search-forward (char-to-string char) nil nil arg)
        (backward-char 1)))
    (setq case-fold-search old-case-fold-search)))

;;;###autoload
(defun fastnav-search-char-backward (arg char)
  "Moves to the arg-th occurrence of char backward (forward if N
is negative).  If there isn't room, go as far as possible (no
error)."
  (fastnav-search-char-forward (- arg) char))

;;;###autoload
(defun fastnav-get-nth-chars (arg)
  "Computes and returns the positions of the ARG'th occurrence of
characters of the range 1 .. 255."
  (let ((seq '())
        (result '()))
    ;; Create character sequence to look for.
    (setq char 255)
    (while (>= char 1)
      (setq seq (cons char seq))
      (setq char (1- char)))
    ;; Find of nth occurrence of each character
    (let ((old-case-fold-search case-fold-search))
      (setq case-fold-search nil)
      (setq result
            (mapcar (lambda (char)
                      (let ((old-point (point)))
                        (save-excursion
                          (if (< arg 0)
                              (search-backward (char-to-string char) nil t (- arg))
                            (progn
                              (forward-char 1)
                              (search-forward (char-to-string char) nil t arg)
                              (backward-char 1)))
                          (if (= (point) old-point)
                              nil
                            (point)))))
                    seq))
      (setq case-fold-search old-case-fold-search)
      result)))

;;;###autoload
(defun fastnav-highlight-read-char (text arg forwarder backwarder)
  "Highlights the ARG'th occurences of each character while
querying one using message TEXT. Negative ARG means backward
search of occurences."
  (if (not (minibufferp))
      (message text))
  (unwind-protect
      (let ((result nil)
            (forwarders `(,forwarder forward-char next-line))
            (backwarders `(,backwarder backward-char previous-line)))
        (while (not result)
          (remove-overlays nil nil 'tag 'fastnav)
          (mapcar (lambda (p)
                    (if p
                        (let ((ov (make-overlay p (1+ p))))
                          (overlay-put ov 'priority 100)
                          (overlay-put ov 'tag 'fastnav)
                          (overlay-put ov 'face lazy-highlight-face)
                          ov)))
                  (fastnav-get-nth-chars arg))
          (let* ((event (read-event))
                 (char (event-basic-type event))
                 (delta 0)
                 (command (key-binding (vector event))))
            (if (or
                 (and (numberp event) (< event 256))
                 (member command
                         ;; which commands have a keystroke
                         ;; that is valid for search
                         '(self-insert-command
                           org-self-insert-command
                           newline newline-and-indent)))
                (setq result (list arg event))
              (progn
                (if (member command forwarders)
                    ;; increase argument
                    (setq delta +1)
                  (if (member command backwarders)
                      ;; decrease argument
                      (setq delta -1)
                    (keyboard-quit)))
                ;; ignore arg = 0
                (setq arg (if (= (+ arg delta) 0)
                              (+ arg (* 2 delta))
                            (+ arg delta)))))))
        result)
    (remove-overlays nil nil 'tag 'fastnav)
    ))

;; For debugging.
;;(key-binding (vector (read-event)))
;;(event-basic-type (read-event))

;;;###autoload
(defun fastnav-highlight-read-char-backward (text arg forwarder backwarder)
  "Highlights the backward ARG'th occurences of each character
while querying one using message TEXT."
  (let ((args (fastnav-highlight-read-char text (- arg) forwarder backwarder)))
    (list (- (car args)) (cadr args))))

;;;###autoload
(defun fastnav-jump-to-char-forward (arg)
  "Jump to the ARG'th occurence of a character that is queried
interactively while highlighting the possible positions."
  (interactive "p")
  (apply 'fastnav-search-char-forward (fastnav-highlight-read-char "Jump to char:" arg
                                                                   'fastnav-jump-to-char-forward
                                                                   'fastnav-jump-to-char-backward)))

;;;###autoload
(defun fastnav-jump-to-char-backward (arg)
  "Jump backward to the ARG'th occurence of a character that is
queried interactively while highlighting the possible positions."
  (interactive "p")
  (apply 'fastnav-search-char-backward
         (fastnav-highlight-read-char-backward "Jump to char backward:" arg
                                               'fastnav-jump-to-char-forward
                                               'fastnav-jump-to-char-backward)))

;;;###autoload
(defun fastnav-mark-up-to-char-forward (arg)
  "Set mark before the ARG'th occurence of a character queried
interactively."
  (interactive "p")
  (let ((args (fastnav-highlight-read-char "Copy to char: " arg
                                           'fastnav-mark-up-to-char-forward
                                           'fastnav-mark-up-to-char-backward)))
    (set-mark (point))
    (apply 'fastnav-search-char-forward args)
    (exchange-point-and-mark)))

;;;###autoload
(defun fastnav-mark-up-to-char-xbackward (arg)
  "Set mark backward after the ARG'th occurence of a character
queried interactively."
  (interactive "p")
  (let ((args (fastnav-highlight-read-char-backward "Copy to char backward: " arg
                                                    'fastnav-mark-up-to-char-forward
                                                    'fastnav-mark-up-to-char-backward)))
    (set-mark (point))
    (apply 'fastnav-search-char-backward args)
    (exchange-point-and-mark)))

;;;###autoload
(defun fastnav-mark-to-char-forward (arg)
  "Set mark before the ARG'th occurence of a character queried
interactively."
  (interactive "p")
  (let* ((args (fastnav-highlight-read-char "Copy to char: " arg
                                            'fastnav-mark-to-char-forward
                                            'fastnav-mark-to-char-backward))
         (pos (> (car args) 0)))
    (set-mark (point))
    (apply 'fastnav-search-char-forward args)
    (if pos (forward-char 1))
    (exchange-point-and-mark)))

;;;###autoload
(defun fastnav-mark-to-char-backward (arg)
  "Set mark backward after the ARG'th occurence of a character
queried interactively."
  (interactive "p")
  (let* ((args (fastnav-highlight-read-char-backward "Copy to char backward: " arg
                                                     'fastnav-mark-to-char-forward
                                                     'fastnav-mark-to-char-backward))
         (pos (> (car args) 0)))
    (set-mark (point))
    (apply 'fastnav-search-char-backward args)
    (if (not pos) (forward-char 1))
    (exchange-point-and-mark)))

;;;###autoload
(defun fastnav-zap-up-to-char-forward (arg)
  "Kill text up to the ARG'th occurence of a character queried
interactively."
  (interactive "p")
  (let ((args (fastnav-highlight-read-char "Zap up to char: " arg
                                           'fastnav-zap-up-to-char-forward
                                           'fastnav-zap-up-to-char-backward)))
    (kill-region (point)
                 (progn
                   (apply 'fastnav-search-char-forward args)
                   (point)))))

;;;###autoload
(defun fastnav-zap-up-to-char-backward (arg)
  "Kill text backward to the ARG'th occurence of a character
queried interactively."
  (interactive "p")
  (let ((args (fastnav-highlight-read-char-backward "Zap up to char backward: " arg
                                                    'fastnav-zap-up-to-char-forward
                                                    'fastnav-zap-up-to-char-backward)))
    (kill-region (point)
                 (progn
                   (apply 'fastnav-search-char-backward args)
                   (point)))))

;;;###autoload
(defun fastnav-zap-to-char-forward (arg)
  "Kill text up to and including the ARG'th occurence of a character queried
interactively."
  (interactive "p")
  (let* ((args (fastnav-highlight-read-char "Zap up to char: " arg
                                            'fastnav-zap-to-char-forward
                                            'fastnav-zap-to-char-backward))
         (pos (> (car args) 0)))
    (kill-region (point)
                 (if pos (progn
                           (apply 'fastnav-search-char-forward args)
                           (1+ (point)))
                   (apply 'fastnav-search-char-forward args)
                   (forward-char 1)
                   (1- (point))))))

;;;###autoload
(defun fastnav-zap-to-char-backward (arg)
  "Kill text backward to the ARG'th occurence of a character
queried interactively."
  (interactive "p")
  (let* ((args (fastnav-highlight-read-char-backward "Zap up to char backward: " arg
                                                     'fastnav-zap-to-char-forward
                                                     'fastnav-zap-to-char-backward))
         (pos (> (car args) 0)))
    (kill-region (point)
                 (if pos (progn
                           (apply 'fastnav-search-char-backward args)
                           (point))
                   (apply 'fastnav-search-char-backward args)
                   (1+ (point))))))

;;;###autoload
(defun fastnav-replace-char-forward (arg)
  "Interactively replaces the ARG'th occurence of a character."
  (interactive "p")
  (let ((args (fastnav-highlight-read-char "Replace char: " arg
                                           'fastnav-replace-char-forward
                                           'fastnav-replace-char-backward)))
    (save-excursion
      (apply 'fastnav-search-char-forward args)
      (let ((char (read-char (if (minibufferp) nil "With char: "))))
        (delete-char +1)
        (insert char)))))

;;;###autoload
(defun fastnav-replace-char-backward (arg)
  "Interactively replaces the ARG'th backward occurence of a
character."
  (interactive "p")
  (let ((args (fastnav-highlight-read-char-backward "Replace char backward: " arg
                                                    'fastnav-replace-char-forward
                                                    'fastnav-replace-char-backward)))
    (save-excursion
      (apply 'fastnav-search-char-backward args)
      (let ((char (read-char (if (minibufferp) nil "With char: "))))
        (delete-char +1)
        (insert char)))))

;;;###autoload
(defun fastnav-insert-at-char-forward (arg)
  "Queries for a character and a string that is insterted at
the ARG'th occurence of the character."
  (interactive "p")
  (let ((args (fastnav-highlight-read-char "Execute forward before: " arg
                                           'fastnav-insert-at-char-forward
                                           'fastnav-insert-at-char-backward)))
    (save-excursion
      (apply 'fastnav-search-char-forward args)
      (insert (read-string "String: ")))))

;;;###autoload
(defun fastnav-insert-at-char-backward (arg)
  "Queries for a character and a string that is insterted at
the backward ARG'th occurence of the character."
  (interactive "p")
  (let ((args (fastnav-highlight-read-char-backward "Execute backward before: " arg
                                                    'fastnav-insert-at-char-forward
                                                    'fastnav-insert-at-char-backward)))
    (save-excursion
      (apply 'fastnav-search-char-backward args)
      (insert (read-string "String: ")))))

;;;###autoload
(defun fastnav-execute-at-char-forward (arg)
  "Queries for a character and a key sequence that is executed at
the ARG'th occurence of the character."
  (interactive "p")
  (let ((args (fastnav-highlight-read-char "Execute forward before: " arg
                                           'fastnav-execute-at-char-forward
                                           'fastnav-execute-at-char-backward)))
    (save-excursion
      (apply 'fastnav-search-char-forward args)
      (execute-kbd-macro (read-key-sequence-vector
                          (if (minibufferp) nil "Key sequence: "))))))

;;;###autoload
(defun fastnav-execute-at-char-backward (arg)
  "Queries for a character and a key sequence that is executed at
the backward ARG'th occurence of the character."
  (interactive "p")
  (let ((args (fastnav-highlight-read-char-backward "Execute backward before: " arg
                                                    'fastnav-execute-at-char-forward
                                                    'fastnav-execute-at-char-backward)))
    (save-excursion
      (apply 'fastnav-search-char-backward args)
      (execute-kbd-macro (read-key-sequence-vector
                          (if (minibufferp) nil "Key sequence: "))))))

;;;###autoload
(defun fastnav-delete-char-forward (arg)
  "Deletes the ARG'th occurence of a character, which is queried
interactively while highlighting the possible positions."
  (interactive "p")
  (let ((args (fastnav-highlight-read-char "Delete forward before: " arg
                                           'fastnav-delete-char-forward
                                           'fastnav-delete-char-backward)))
    (save-excursion
      (apply 'fastnav-search-char-forward args)
      (delete-char +1))))

;;;###autoload
(defun fastnav-delete-char-backward (arg)
  "Deletes the backward ARG'th occurence of a character, which is
queried interactively while highlighting the possible positions."
  (interactive "p")
  (let ((args (fastnav-highlight-read-char-backward "Delete backward before: \n" arg
                                                    'fastnav-delete-char-forward
                                                    'fastnav-delete-char-backward)))
    (save-excursion
      (apply 'fastnav-search-char-backward args)
      (delete-char +1))))

;;;###autoload
(defun fastnav-sprint-forward (arg)
  "Performs a sequence of jumping forward to the next character
matching the keyboard event."
  (interactive "p")
  (let ((result t))
    (while result
      (if (setq result (fastnav-highlight-read-char "Sprint:" arg
                                                    'fastnav-sprint-forward
                                                    'fastnav-sprint-backward))
          (progn
            (apply 'fastnav-search-char-forward result)
            (setq arg (if (> (car result) 0) 1 -1)))))))


;;;###autoload
(defun fastnav-sprint-backward (arg)
  "Performs a sequence of jumping backward to the next character
matching the keyboard event."
  (interactive "p")
  (fastnav-sprint-forward (- arg)))

(provide 'fastnav)

;;; fastnav.el ends here
