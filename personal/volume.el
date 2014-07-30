;;; volume.el --- tweak your sound card volume from Emacs
;; Copyright (C) 2005, 2006, 2007, 2008  Daniel Brockman
;; Copyright (C) 1998, 2000, 2001, 2002, 2003, 2004, 2005
;;   Free Software Foundation, Inc.

;; Author: Daniel Brockman <daniel@brockman.se>
;; URL: http://www.brockman.se/software/volume-el/
;; Created: September 9, 2005
;; Updated: August 27, 2008
;; Version: 1.0

;; This file is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty
;; of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;; See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; To use this program, put this file in your `load-path',
;; and put the following autoload in your ~/.emacs:
;;
;;   (autoload 'volume "volume"
;;     "Tweak your sound card volume." t)
;;
;; Then type `M-x volume <RET>' to run the program.  Of course,
;; use `M-x customize-group <RET> volume <RET>' to customize it.

;; Tweaking the volume of my music used to be one of the
;; few things I constantly went outside of Emacs to do.
;; I just decided I've had enough of that, and so I wrote
;; this simple mixer frontend.

;; It comes with backend glue for aumix and amixer, but the
;; latter is pretty slow, so I have to recommend the former.
;; If you can't use either, writing your own glue should be
;; straightforward.  And if you do, please consider sending
;; the code to me, so I can integrate it into this file.

;;; TODO:

;; Multiple ALSA mixer controls can have the same name;
;; this situation messes everything up.  Deal with it.

;;; Code:

(defgroup volume nil
  "Tweak your sound card volume."
  :group 'multimedia)

(defcustom volume-backend
  (cond ((executable-find "aumix") 'volume-aumix-backend)
        ((executable-find "amixer") 'volume-amixer-backend))
  "The set of primitives used by Volume to do real work.
Value is an alist containing entries `get', `set', `nudge',
`current-channel', `switch-channel', `default-channel',
`channel-label', and `channels', or the name of a variable
containing such an alist."
  :type '(radio (const :tag "aumix" volume-aumix-backend)
                (const :tag "amixer" volume-amixer-backend)
                (const :tag "None" nil)
                (variable :tag "Custom"))
  :group 'volume)

(defcustom volume-electric-mode t
  "Run Volume electrically, in the echo area.
Electric mode saves some space, but uses its own command loop."
  :type 'boolean
  :group 'volume)

(defface volume-bar
  '((t (:inverse-video t :weight bold)))
  "Face used for the indicator bar in Volume mode."
  :group 'volume)

(defun volume-error (format &rest strings)
  "Either signal a real error, or manually beep and display message.
Real errors cannot be used in electric mode."
  (if (or (not volume-electric-mode)
          (null volume-buffer))
      (apply 'error format strings)
    (beep)
    (with-current-buffer volume-buffer
      (let ((inhibit-read-only t))
        (delete-region (point-min) (point-max))
        (insert (apply 'format format strings))
        (sit-for 2)))
    (volume-redisplay)))

(defun volume-backend-call (primitive &rest arguments)
  "Call PRIMITIVE from the current backend with ARGUMENTS.
See the variable `volume-backend'."
  (let ((backend (symbol-value volume-backend)))
    (when (null backend)
      (volume-error "No backend (see `volume-backend')"))
    (let ((function (cdr (assoc primitive backend))))
      (when (null function)
        (volume-error "No `%s' operation for current backend" primitive))
      (apply function arguments))))

(defvar volume-buffer nil
  "The current Volume buffer, or nil.")

(defun volume-call-process-to-string (program &rest arguments)
  "Call `volume-call-process' in a temporary buffer.
If `volume-call-process' returns normally, return the process output
as a string.  Otherwise, raise an error or return nil."
  (catch 'return
    (with-output-to-string
      (with-current-buffer standard-output
        (when (null (apply 'volume-call-process program arguments))
          (throw 'return nil))))))

(defun volume-call-process (program &rest arguments)
  "Call PROGRAM using `call-process' and raise an error on failure.
Specificaly, call (apply 'call-process PROGRAM nil t nil ARGUMENTS).
If `call-process' returns anything other than zero, raise an error.
If the process produced output and failed, display the output in a
  temporary buffer before raising the error.
If Volume is running an electric command loop, return nil on error.
If `call-process' returns zero, leave point at the start of the buffer
  and return non-nil."
  (if (equal 0 (apply 'call-process program nil t nil arguments))
      (prog1 t (goto-char (point-min)))
    (prog1 nil
      (let* ((command-line
              (mapconcat 'identity (cons program arguments) " "))
             (message
              (format "Process `%s' exited abnormally " command-line)))
        (if (= (point-min) (point-max))
            (volume-error "%s" (concat message " with no output"))
          (with-output-to-temp-buffer
              (format "*Process output: %s*" command-line)
            (princ (buffer-string)))
          (volume-error "%s" message))))))


;;;; The aumix backend

(defvar volume-aumix-backend
  '((get . volume-aumix-get)
    (set . volume-aumix-set)
    (nudge . volume-aumix-nudge)
    (current-channel . volume-aumix-current-channel)
    (switch-channel . volume-aumix-switch-channel)
    (default-channel . volume-aumix-default-channel)
    (channel-label . volume-aumix-channel-label)
    (channels . volume-aumix-channels)))

(defgroup volume-aumix nil
  "The aumix backend."
  :group 'volume)

(defcustom volume-aumix-program "aumix"
  "The name of the aumix program."
  :type 'string
  :group 'volume-aumix)

(defcustom volume-aumix-device nil
  "The name of the mixer device, or nil for the default.
This corresponds to the `-d' option of aumix."
  :type '(choice (const :tag "/dev/mixer" nil)
                 (const "/dev/mixer1")
                 (const "/dev/mixer2")
                 (const "/dev/mixer3")
                 (const "/dev/mixer4")
                 file)
  :group 'volume-aumix)

;;; These channel names were taken from <linux/soundcard.h>.
(defvar volume-aumix-all-channels
  '(("vol" "-v" "Master")
    ("bass" "-b" "Bass")
    ("treble" "-t" "Treble")
    ("pcm" "-w" "PCM")
    ("pcm2" "-W" "PCM 2")
    ("line" "-l" "Line")
    ("line1" "-1" "Line 1")
    ("line2" "-2" "Line 2")
    ("line3" "-3" "Line 3")
    ("synth" "-s" "Synthesizer")
    ("speaker" "-p" "PC speaker")
    ("cd" "-c" "CD")
    ("mix" "-x" "Mix")
    ("mic" "-m" "Microphone")
    ("rec" "-r" "Record")
    ("igain" "-i" "Input gain")
    ("ogain" "-o" "Output gain")
    ("dig1" nil "Digital 1")
    ("dig2" nil "Digital 2")
    ("dig3" nil "Digital 3")
    ("phin" nil "Phone in")
    ("phout" nil "Phone out")
    ("video" nil "Video")
    ("radio" nil "Radio")
    ("monitor" nil "Monitor"))
  "List of recognized aumix channels.")

(defvar volume-aumix-default-channels
  (mapcar (lambda (channel-name)
            (assoc channel-name volume-aumix-all-channels))
          '("vol" "bass" "treble" "pcm" "line"))
  "The default value of `volume-aumix-channels'.
You probably don't want to change this variable; instead,
change `volume-aumix-channels' directly.")

(defvar volume-aumix-available-channels
  (when (executable-find volume-aumix-program)
    (condition-case nil
        (let (result)
          (with-temp-buffer
            (volume-call-process volume-aumix-program "-q")
            (while (re-search-forward "^\\S-+" nil t)
              (let* ((channel-name (match-string 0))
                     (channel (assoc channel-name
                                     volume-aumix-all-channels)))
                (if channel
                    (when (cadr channel)
                      (setq result (cons channel result)))
                  (message "Unrecognized aumix channel: `%s'"
                           channel-name))))
            (reverse result)))
      (error nil)))
  "List of available aumix channels.
You probably don't want to change this variable; instead,
change `volume-aumix-channels'.")

(defcustom volume-aumix-channels
  (apply 'append
         (mapcar (lambda (channel)
                   (when (member channel volume-aumix-available-channels)
                     (list channel)))
                 volume-aumix-default-channels))
  "List of channels to manipulate using aumix."
  :type
  `(list (set :tag "Available channels" :inline t
              :format "%t:\n%v"
              ,@(mapcar (lambda (entry)
                          `(const :tag ,(nth 2 entry) ,entry))
                        volume-aumix-available-channels))
         (set :tag "Apparently unavailable channels" :inline t
              :format "%t:\n%v"
              ,@(apply
                 'append
                 (mapcar
                  (lambda (entry)
                    (when (cadr entry)
                      (unless (member entry volume-aumix-available-channels)
                        `((const :tag ,(nth 2 entry) ,entry)))))
                  volume-aumix-all-channels)))
         (repeat :tag "Other channels" :inline t
                 (list :tag "Channel"
                       (string :tag "Name (see `aumix -q')")
                       (string :tag "Option (see `aumix -h')")
                       (string :tag "Label (user-friendly name)"))))
  :group 'volume-aumix)

(defcustom volume-aumix-default-channel
  (if (executable-find volume-aumix-program)
      (or (car-safe (assoc "vol" volume-aumix-channels))
          (car-safe (assoc "pcm" volume-aumix-channels))
          (car-safe (car-safe volume-aumix-channels))
          "vol")
    "vol")
  "The name of the default audio channel to manipulate using aumix."
  :type `(choice ,@(mapcar (lambda (entry)
                             `(const :tag ,(nth 2 entry) ,(car entry)))
                           volume-aumix-channels)
                 (string :tag "Other (specify name as in `aumix -q')"))
  :group 'volume-aumix)

(defvar volume-aumix-current-channel volume-aumix-default-channel
  "The aumix option for the audio channel to manipulate.")

(defun volume-aumix-current-channel-option ()
  "Return the aumix option for the current channel."
  (nth 1 (volume-aumix-current-channel)))

(defun volume-aumix-channel (channel)
  "If CHANNEL is a channel, return it.
If CHANNEL is the name of a channel, return the channel."
  (if (consp channel)
      channel
    (or (assoc channel volume-aumix-channels)
        (assoc channel volume-aumix-all-channels))))

(defcustom volume-aumix-extra-arguments nil
  "Extra arguments to pass to the aumix program."
  :type '(repeat string)
  :group 'volume-aumix)

(defun volume-aumix-call (&rest arguments)
  "Call aumix with ARGUMENTS and return the output."
  (apply 'volume-call-process-to-string volume-aumix-program
         (append (when volume-aumix-device
                   (list "-d" volume-aumix-device))
                 volume-aumix-extra-arguments
                 arguments)))

(defun volume-aumix-parse-output (output)
  "Parse the OUTPUT of an aumix volume query.
Return the volume percentage as a floating-point number.
If OUTPUT cannot be parsed, raise an error."
  (when output
    (if (string-match "^\\S-+ \\([0-9]+\\)" output)
        (float (string-to-number (match-string 1 output)))
      (volume-error "Failed to parse aumix output"))))

;;; The following are API functions.

(defun volume-aumix-get ()
  "Return the current volume in percent, using aumix to get it."
  (volume-aumix-parse-output
   (volume-aumix-call
    (concat (volume-aumix-current-channel-option) "q"))))

(defun volume-aumix-set (n)
  "Use aumix to set the current volume to N percent.
Return the new volume in percent."
  (volume-aumix-parse-output
   (volume-aumix-call
    (concat (volume-aumix-current-channel-option)
            (number-to-string n))
    (concat (volume-aumix-current-channel-option) "q"))))

(defun volume-aumix-nudge (n)
  "Use aumix to change the volume by N percentage units.
Return the new volume in percent."
  (let ((sign (if (>= n 0) "+" "-")))
    (volume-aumix-parse-output
     (volume-aumix-call
      (concat (volume-aumix-current-channel-option)
              sign (number-to-string (abs n)) )
      (concat (volume-aumix-current-channel-option) "q")))))

(defun volume-aumix-current-channel ()
  "Return the current channel for aumix."
  (volume-aumix-channel volume-aumix-current-channel))

(defun volume-aumix-switch-channel (channel)
  "Make CHANNEL current for aumix."
  (setq volume-aumix-current-channel
        (volume-aumix-channel channel)))

(defun volume-aumix-default-channel ()
  "Return the default channel for aumix."
  (volume-aumix-channel volume-aumix-default-channel))

(defun volume-aumix-channel-label (channel)
  "Return the user-friendly name of CHANNEL."
  (nth 2 (volume-aumix-channel channel)))

(defun volume-aumix-channels ()
  "Return the list of available channels for aumix."
  volume-aumix-channels)


;;;; The amixer backend

(defvar volume-amixer-backend
  '((get . volume-amixer-get)
    (set . volume-amixer-set)
    (nudge . volume-amixer-nudge)
    (current-channel . volume-amixer-current-channel)
    (switch-channel . volume-amixer-switch-channel)
    (default-channel . volume-amixer-default-channel)
    (channel-label . volume-amixer-channel-label)
    (channels . volume-amixer-channels)))

(defgroup volume-amixer nil
  "The amixer backend."
  :group 'volume)

(defcustom volume-amixer-program "amixer"
  "The name of the amixer program."
  :type 'string
  :group 'volume-amixer)

(defcustom volume-amixer-card nil
  "The ALSA sound card number to use, or nil for the default.
This corresponds to the `-c' option of amixer."
  :type '(choice integer (const :tag "Default" nil))
  :group 'volume-amixer)

(defcustom volume-amixer-device nil
  "The ALSA device name to use, or nil for the default.
This corresponds to the `-D' option of amixer."
  :type '(choice string (const :tag "Default" nil))
  :group 'volume-amixer)

(defvar volume-amixer-default-channels
  '("Master" "Bass" "Treble" "PCM" "Line")
  "The default value of `volume-amixer-channels'.")

(defvar volume-amixer-available-channels
  (when (executable-find volume-amixer-program)
    (condition-case nil
        (let (result)
          (with-temp-buffer
            (volume-call-process volume-amixer-program)
            (while (re-search-forward (concat "^\\s-*Capabilities: "
                                              "\\<[a-z]*volume\\>")
                                      nil 'noerror)
              (save-excursion
                (re-search-backward "^\\s-*Simple mixer control '\\(.*\\)'")
                (setq result (cons (match-string 1) result)))))
          result)
      (error nil)))
  "List of available amixer channels.")

(defcustom volume-amixer-channels
  (apply 'append
         (mapcar
          (lambda (channel)
            (when (member channel volume-amixer-available-channels)
              (list channel)))
          volume-amixer-default-channels))
  "The names of the ALSA mixer channels to manipulate."
  :type `(set ,@(mapcar (lambda (channel)
                          `(const ,channel))
                        volume-amixer-available-channels)
              (repeat :tag "Others" :inline t
                      (string :tag "Channel")))
  :group 'volume-amixer)

(defcustom volume-amixer-default-channel
  (if (executable-find volume-amixer-program)
      (or (assoc "Master" volume-amixer-channels)
          (assoc "PCM" volume-amixer-channels)
          (car-safe volume-amixer-channels)
          "Master")
    "Master")
  "The name of the default ALSA mixer channel to manipulate."
  :type `(choice ,@(mapcar (lambda (channel)
                             `(const ,channel))
                           volume-amixer-available-channels)
                (string :tag "Other"))
  :group 'volume-amixer)

(when (fboundp 'define-obsolete-variable-alias)
  (define-obsolete-variable-alias 'volume-amixer-control
    'volume-amixer-default-channel))

(defvar volume-amixer-current-channel volume-amixer-default-channel
  "The name of the ALSA mixer channel to manipulate.")

(defcustom volume-amixer-extra-arguments nil
  "Extra arguments to pass to the amixer program."
  :type '(repeat string)
  :group 'volume-amixer)

(defun volume-amixer-call (&rest arguments)
  "Call amixer with ARGUMENTS and return the output."
  (apply 'volume-call-process-to-string volume-amixer-program
         (append (when volume-amixer-card
                   (list "-c" (number-to-string volume-amixer-card)))
                 (when volume-amixer-device
                   (list "-D" volume-amixer-device))
                 volume-amixer-extra-arguments
                 arguments)))

(defun volume-amixer-parse-output (output)
  "Parse the OUTPUT of an amixer control dump.
Return the volume percentage as a floating-point number.
If OUTPUT cannot be parsed, raise an error."
  (when output
    (if (string-match "\\[\\([0-9]+\\)%\\]" output)
        (float (string-to-number (match-string 1 output)))
      (volume-error "Failed to parse amixer output"))))

;;; The following are API functions.

(defun volume-amixer-get ()
  "Return the current volume, using amixer to get it."
  (volume-amixer-parse-output
   (volume-amixer-call "get" volume-amixer-current-channel)))

(defun volume-amixer-set (n)
  "Use amixer to set the current volume to N percent."
  (volume-amixer-parse-output
   (volume-amixer-call "set" volume-amixer-current-channel
                       (format "%d%%" n))))

(defun volume-amixer-nudge (n)
  "Use amixer to change the volume by N percentage units."
  (let ((sign (if (>= n 0) "+" "-"))
        (current (volume-amixer-get)))
    (when (and (equal current
                      (volume-amixer-parse-output
                       (volume-amixer-call
                        "set" volume-amixer-current-channel
                        (format "%d%%%s" (abs n) sign))))
               (not (null current)))
      ;; If nudging by `N%' didn't work, try `N'.
      (volume-amixer-parse-output
       (volume-amixer-call "set" volume-amixer-current-channel
                           (format "%d%s" (abs n) sign))))))

(defun volume-amixer-current-channel ()
  "Return the current channel for amixer."
  volume-amixer-current-channel)

(defun volume-amixer-switch-channel (channel)
  "Make CHANNEL current for amixer."
  (setq volume-amixer-current-channel channel))

(defun volume-amixer-default-channel ()
  "Return the default channel for amixer."
  volume-amixer-default-channel)

(defun volume-amixer-channel-label (channel)
  "Return the name of CHANNEL."
  channel)

(defun volume-amixer-channels ()
  "Return the list of available channels for amixer."
  volume-amixer-channels)


;;;; User interface

(defun volume-get ()
  "Return the current volume in percent."
  (volume-backend-call 'get))

(defun volume-set (n)
  "Set the volume to N percent."
  (interactive "nSet volume (in percent): ")
  (let ((new-value (volume-backend-call 'set n)))
    (when (interactive-p)
      (volume-show new-value))))

(defun volume-nudge (n)
  "Change the volume by N percentage units.
Return either the new volume or nil, depending on the backend."
  (volume-backend-call 'nudge n))

(defun volume-current-channel ()
  "Return the current channel."
  (volume-backend-call 'current-channel))

(defun volume-switch-channel (channel)
  "Make CHANNEL current."
  (volume-backend-call 'switch-channel channel))

(defun volume-default-channel ()
  "Retur the default channel."
  (volume-backend-call 'default-channel))

(defun volume-channel-label (channel)
  "Return the user-friendly name of CHANNEL."
  (volume-backend-call 'channel-label channel))

(when (fboundp 'define-obsolete-function-alias)
  (define-obsolete-function-alias 'volume-channel-name
    'volume-channel-label))

(defun volume-channels ()
  "Return the list of available channels."
  (volume-backend-call 'channels))

(defun volume-next-channel ()
  "Switch to the next channel."
  (interactive)
  (let* ((channels (volume-channels))
         (channel
          (or (car-safe
               (cdr-safe
                (member (volume-current-channel) channels)))
              (car-safe channels))))
    (if (null channel)
        (volume-error "Channel list is empty")
      (volume-switch-channel channel)
      (volume-update))))

(defun volume-previous-channel ()
  "Switch to the previous channel."
  (interactive)
  (let* ((reverse-channels (reverse (volume-channels)))
         (channel
          (or (car-safe
               (cdr-safe
                (member (volume-current-channel) reverse-channels)))
              (car-safe reverse-channels))))
    (if (null channel)
        (volume-error "Channel list is empty")
      (volume-switch-channel channel)
      (volume-update))))

(defun volume-show (&optional volume)
  "Display the current volume in the minibuffer.
If VOLUME is non-nil, take that to be the current volume."
  (interactive)
  (message "Volume%s: %d%%"
           (if (equal (volume-current-channel)
                      (volume-default-channel)) ""
             (concat " (" (volume-channel-label
                           (volume-current-channel)) ")"))
           (or volume (round (volume-get)))))

(defvar volume-redisplaying nil
  "Non-nil in the dynamic scope of `volume-redisplay'.")

(defvar volume-bar-start nil
  "Character position of the start of the volume bar.")
(make-variable-buffer-local 'volume-bar-start)

(defun volume-mouse-motion (event left right)
  (let ((window (posn-window (event-end event))))
    (when (windowp window)
      (let* ((x (+ (car (posn-x-y (event-end event)))
                   (car (window-inside-pixel-edges window)))))
        (volume-update
         (volume-set
          (/ (* 100.0 (- (max left (min x right)) left))
             (- right left))))))))

(defun volume-mouse-down (event)
  (interactive "@e")
  (let* ((window (posn-window (event-end event)))
         (buffer (and window (window-buffer window))))
    (when (and buffer (eq buffer volume-buffer))
      (with-current-buffer volume-buffer
        (catch 'abort
          (let* ((edges (window-inside-pixel-edges
                         (posn-window (event-end event))))
                 (left (+ (car (posn-x-y (or (posn-at-point volume-bar-start)
                                             (throw 'abort nil))))
                          (nth 0 edges)))
                 (right (nth 2 edges)))
            (volume-mouse-motion event left right)
            (track-mouse
              (let ((volume-tracking-mouse t))
                (while (progn (setq event (read-event))
                              (not (eq (event-basic-type event) 'mouse-1)))
                  (when (mouse-movement-p event)
                    (volume-mouse-motion event left right)))))))))))

(defun volume-electric-mouse-down (event)
  ;; For some reason, "@e" does not work in electric mode.
  (interactive "e")
  (volume-mouse-down event))

(defvar volume-label-map
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (define-key map [mouse-1]
        (lambda (e)
          (interactive "e")
          (volume-next-channel)))
      (define-key map [mouse-2]
        (lambda (e)
          (interactive "e")
          (volume-next-channel)))
      (define-key map [mouse-3]
        (lambda (e)
          (interactive "e")
          (volume-previous-channel))))))

(defvar volume-bar-map
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (define-key map [down-mouse-1] 'volume-mouse-down))))

(defvar volume-electric-bar-map
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (define-key map [down-mouse-1] 'volume-electric-mouse-down))))

(defun volume-redisplay (&optional volume)
  "Update the Volume buffer to reflect the current volume.
If VOLUME is non-nil, take that to be the current volume."
  (interactive)
  (unless volume-redisplaying
    (let ((volume-redisplaying t))
      (when (null volume)
        (setq volume (volume-get)))
      (let ((inhibit-read-only t))
        (set-buffer volume-buffer)
        (delete-region (point-min) (point-max))
        (insert "Volume")
        (unless (equal (volume-current-channel)
                       (volume-default-channel))
          (insert " (" (volume-channel-label
                        (volume-current-channel)) ")"))
        (when (> (length (volume-channels)) 1)
          (add-text-properties
           (point-min) (point-max)
           (list 'mouse-face 'highlight
                 'keymap volume-label-map
                 'help-echo (concat "mouse-1: next channel\n"
                                    "mouse-3: previous channel"))))
        (insert ": ")
        (let* ((bar-start (point))
               (available-width (- (window-width) bar-start))
               (bar-width (if volume
                              (round (* (/ volume 100.0) available-width))
                            available-width))
               (label (if volume
                          (format " %d%% " volume)
                        " (not available) "))
               (label-width (length label)))
          (setq volume-bar-start bar-start)
          (insert-char ?\  available-width)
          (goto-char
           (+ bar-start
              (if (< bar-width label-width) (1+ bar-width)
                (/ (1+ (- bar-width label-width)) 2))))
          (delete-char label-width)
          (insert label)
          (put-text-property bar-start (+ bar-start bar-width)
                             'face (if volume
                                       'volume-bar
                                     'font-lock-warning))
          (when volume
            (add-text-properties bar-start (point-max)
                                 (list 'pointer 'hdrag
                                       'keymap (if volume-electric-mode
                                                   volume-electric-bar-map
                                                 volume-bar-map))))
          (goto-char (+ bar-start bar-width)))))))

(defvar volume-tracking-mouse nil
  "Non-nil when tracking the mouse.")

(defun volume-update (&optional volume)
  "Maybe call `volume-show' or `volume-redisplay'; return VOLUME.
This function should be called by UI commands that change the volume."
  (prog1 volume
    (if volume-buffer
        ;; The electric command loop will trigger a redisplay
        ;; after each command anyway, so avoid doing it twice.
        (unless (and volume-electric-mode
                     (not volume-tracking-mouse))
          (volume-redisplay volume))
      (volume-show volume))))

(defun volume-assign (n)
  "Set the volume to N percent.
If N is negative, call `volume-raise' instead."
  (interactive "P")
  (if (integerp n)
      (if (< n 0) (volume-raise n)
        (volume-update (volume-set n)))
    (volume-error "Need integer argument")))

(make-obsolete 'volume-assign
               (concat "The digit keys now set the volume immediately, "
                       "so this function is no longer needed.  "
                       "Use `volume-set' to set the volume.")
               "2006-11-25")

(defun volume-lower (&optional n)
  "Lower the volume by N percentage units."
  (interactive "p")
  (volume-update (volume-nudge (- (or n 1)))))

(defun volume-raise (&optional n)
  "Raise the volume by N percentage units."
  (interactive "p")
  (volume-update (volume-nudge (or n 1))))

(defun volume-lower-10 (&optional n)
  "Lower the volume by 10 N percentage units."
  (interactive "p")
  (volume-lower (* n 10)))

(defalias 'volume-lower-more 'volume-lower-10)
(make-obsolete 'volume-lower-more 'volume-lower-10)

(defun volume-raise-10 (&optional n)
  "Raise the volume by 10 N percentage units."
  (interactive "p")
  (volume-raise (* n 10)))

(defalias 'volume-raise-more 'volume-raise-10)
(make-obsolete 'volume-raise-more 'volume-raise-10)

(defun volume-lower-50 (&optional n)
  "Lower the volume by 50 N percentage units."
  (interactive "p")
  (volume-lower (* n 50)))

(defun volume-raise-50 (&optional n)
  "Raise the volume by 50 N percentage units."
  (interactive "p")
  (volume-raise (* n 50)))

(dotimes (n 11)
  (eval `(defun ,(intern (format "volume-set-to-%d%%" (* n 10))) ()
           ,(format "Set the volume to %d%%." (* n 10))
           (interactive)
           (volume-update (volume-set ,(* n 10))))))

(defalias 'volume-minimize 'volume-set-to-0%)
(defalias 'volume-maximize 'volume-set-to-100%)

(defun volume-assign-and-quit (&optional n)
  "Set the volume to N percent and then quit Volume mode.
If N is nil, just quit Volume mode."
  (interactive "P")
  (when (integerp n)
    (volume-redisplay (volume-assign n))
    (sit-for 1))
  (volume-quit))

(make-obsolete 'volume-assign-and-quit
               (concat "The digit keys now set the volume immediately, "
                       "so this function is no longer needed.  "
                       "Use `volume-set' to set the volume.")
               "2006-11-25")

(defun volume-quit ()
  "Quit Volume mode."
  (interactive)
  (if volume-electric-mode
      (throw 'volume-done nil)
    (condition-case nil
        (while (get-buffer-window volume-buffer)
          (delete-window (get-buffer-window volume-buffer)))
      (error nil))
    (kill-buffer volume-buffer)
    (setq volume-buffer nil)))

(defun volume-mode ()
  "Major mode for tweaking your audio volume.

\\{volume-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'volume-mode)
  (setq mode-name "Volume")
  (use-local-map volume-mode-map)
  (setq buffer-undo-list t)
  (volume-update)
  (run-mode-hooks 'volume-mode-hook))

(defvar volume-mode-map
  (let ((map (make-sparse-keymap))
        (select-window (lambda (e) (interactive "@e"))))
    (suppress-keymap map 'no-digits)
    (define-key map [mouse-1] select-window)
    (define-key map [drag-mouse-1] select-window)
    (define-key map [mouse-2] select-window)
    (define-key map [drag-mouse-2] select-window)
    (define-key map [mouse-3] select-window)
    (define-key map [drag-mouse-3] select-window)
    (define-key map [left-fringe mouse-1] 'volume-minimize)
    (define-key map [right-fringe mouse-1] 'volume-maximize)
    (define-key map [left-fringe mouse-2] 'volume-minimize)
    (define-key map [right-fringe mouse-2] 'volume-maximize)
    (define-key map [left-fringe mouse-3] select-window)
    (define-key map [right-fringe mouse-3] select-window)
    (define-key map "b" 'volume-lower)
    (define-key map "f" 'volume-raise)
    (define-key map "\C-b" 'volume-lower)
    (define-key map "\C-f" 'volume-raise)
    (define-key map [mouse-4] 'volume-raise)
    (define-key map [mouse-5] 'volume-lower)
    (define-key map "\M-b" 'volume-lower-10)
    (define-key map "\M-f" 'volume-raise-10)
    (define-key map "\C-\M-b" 'volume-lower-50)
    (define-key map "\C-\M-f" 'volume-raise-50)
    (define-key map [left] 'volume-lower)
    (define-key map [right] 'volume-raise)
    (define-key map [(control left)] 'volume-lower-10)
    (define-key map [(control right)] 'volume-raise-10)
    (define-key map [(meta left)] 'volume-lower-10)
    (define-key map [(meta right)] 'volume-raise-10)
    (define-key map [(control meta left)] 'volume-lower-50)
    (define-key map [(control meta right)] 'volume-raise-50)
    (define-key map "a" 'volume-minimize)
    (define-key map "e" 'volume-maximize)
    (define-key map "\C-a" 'volume-minimize)
    (define-key map "\C-e" 'volume-maximize)
    (define-key map [home] 'volume-minimize)
    (define-key map [end] 'volume-maximize)
    (define-key map "`" 'volume-set-to-0%)
    (define-key map "1" 'volume-set-to-10%)
    (define-key map "2" 'volume-set-to-20%)
    (define-key map "3" 'volume-set-to-30%)
    (define-key map "4" 'volume-set-to-40%)
    (define-key map "5" 'volume-set-to-50%)
    (define-key map "6" 'volume-set-to-60%)
    (define-key map "7" 'volume-set-to-70%)
    (define-key map "8" 'volume-set-to-80%)
    (define-key map "9" 'volume-set-to-90%)
    (define-key map "0" 'volume-set-to-100%)
    (define-key map "n" 'volume-next-channel)
    (define-key map "p" 'volume-previous-channel)
    (define-key map "\C-n" 'volume-next-channel)
    (define-key map "\C-p" 'volume-previous-channel)
    (define-key map "\M-n" 'volume-next-channel)
    (define-key map "\M-p" 'volume-previous-channel)
    (define-key map [up] 'volume-next-channel)
    (define-key map [down] 'volume-previous-channel)
    (define-key map "g" 'volume-redisplay)
    (define-key map "\C-m" 'volume-quit)
    (define-key map "q" 'volume-quit)
    ;; This is good when `volume' is bound to `v'.
    ;; Then `v' can be used to toggle on or off.
    (define-key map "v" 'volume-quit)
    (define-key map "\C-g" 'volume-quit)
    (define-key map [escape escape] 'volume-quit)
    map)
  "Keymap for Volume mode.")

(defvar volume-electric-mode-map
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (set-keymap-parent map volume-mode-map)
      (define-key map [mouse-1] 'volume-quit) 
      (define-key map [mode-line mouse-1] 'volume-quit) 
      (define-key map [mouse-2] 'volume-quit) 
      (define-key map [mode-line mouse-2] 'volume-quit) 
      (define-key map [mouse-3] 'volume-quit) 
      (define-key map [mode-line mouse-3] 'volume-quit)))
  "Keymap for electric Volume mode.")

(defvar volume-running-electric-command-loop nil
  "Non-nil when Volume is running an electric command loop.")

;; This function was based on the function `calculator' from
;; calculator.el, which is copyrighted by the FSF.
;;;###autoload
(defun volume ()
  "Tweak your sound card volume."
  (interactive)
  (setq volume-buffer (get-buffer-create "*Volume*"))
  (if volume-electric-mode
      (unless volume-running-electric-command-loop
        (unwind-protect
            (save-window-excursion
              (require 'electric) (message nil)
              (let ((echo-keystrokes 0)
                    (garbage-collection-messages nil))
                (set-window-buffer (minibuffer-window) volume-buffer)
                (select-window (minibuffer-window))
                (let ((old-local-map (current-local-map))
                      (old-global-map (current-global-map)))
                  (use-local-map nil)
                  (use-global-map volume-electric-mode-map)
                  (unwind-protect
                      (progn
                        (volume-redisplay)
                        (run-hooks 'volume-mode-hook)
                        (catch 'volume-done
                          (let ((volume-running-electric-command-loop t))
                            (Electric-command-loop
                             'volume-done
                             ;; Avoid `noprompt' due to
                             ;; a bug in electric.el.
                             (lambda () 'noprompt)
                             nil
                             (lambda (x y) (volume-redisplay))))))
                    (use-local-map old-local-map)
                    (use-global-map old-global-map)))))
          (when volume-buffer
            (kill-buffer volume-buffer)
            (setq volume-buffer nil))))
    (cond ((null (get-buffer-window volume-buffer))
           (let ((window-min-height 2)
                 (split-window-keep-point nil))
             (select-window
              (split-window-vertically
               (if (and (fboundp 'face-attr-construct)
                        (plist-get (face-attr-construct 'mode-line) :box))
                   -3 -2)))
             (switch-to-buffer volume-buffer)))
          ((not (eq (current-buffer) volume-buffer))
           (select-window (get-buffer-window volume-buffer))))
    (volume-mode)
    (setq buffer-read-only t)))

;;; Local Variables:
;;; coding: utf-8
;;; time-stamp-format: "%:b %:d, %:y"
;;; time-stamp-start: ";; Updated: "
;;; time-stamp-end: "$"
;;; time-stamp-line-limit: 20
;;; End:

(provide 'volume)
;;; volume.el ends here.
