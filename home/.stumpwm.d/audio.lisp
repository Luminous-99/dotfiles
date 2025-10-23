(defpackage audio
  (:use :cl :stumpwm :misc :windows)
  (:import-from :alexandria
                #:when-let*)
  (:export
   #:volume-up
   #:volume-down
   #:volume-mute
   #:track-thread-action
   #:*track*
   #:current-track
   #:*player*
   #:set-player
   #:toggle-track
   #:next-track
   #:previous-track
   #:get-pactl-applications
   #:set-application-volume
   #:set-volume-from-menu
   #:toggle-application-mute
   #:toggle-mute-from-menu
   #:volume-Value))

(in-package :audio)

;; Volume
(defcommand volume-value () ()
  (let ((val (run-formatted "~~/.stumpwm.d/scripts/volume_notify.sh Volume")))
    val))

(defcommand volume-up () ()
  (run-shell-command "~/.stumpwm.d/scripts/volume_notify.sh Up"))

(defcommand volume-down () ()
  (run-shell-command "~/.stumpwm.d/scripts/volume_notify.sh Down"))

(defcommand volume-mute () ()
  (run-shell-command "~/.stumpwm.d/scripts/volume_notify.sh Mute"))

(defun list->alist (list &optional reverse)
  (loop for (k v) on list by #'cddr
        if reverse
          collect (cons v k)
        else
          collect (cons k v)))

(flet ((name-or-id (string)
         (unless (zerop (length string))
           (if (char= #\S (aref string 0))
               (cadr (split-string string '(#\#)))
               (string-trim '(#\Space #\" #\') (cadr (split-string string '(#\=))))))))
  (defun get-pactl-applications ()
    (with-input-from-string (sink-inputs (run-shell-command "pactl list sink-inputs" t))
      (let ((list (loop for line = (read-line sink-inputs nil nil)
                        while line
                        when (ppcre:scan "(node\\.name|t #[0-9]*)" line)
                          collect (name-or-id line))))
        (list->alist list t)))))

(defun set-application-volume (class volume)
  "Set an application's VOLUME."
  (let* ((applications (get-pactl-applications)))
    (run-formatted "pactl set-sink-input-volume ~A ~A%" (cdr (assoc class applications :test #'string-equal)) volume)))

(defun toggle-application-mute (class)
  "Toggle whether an application is muted."
  (let* ((applications (get-pactl-applications)))
    (run-formatted "pactl set-sink-input-mute ~A toggle" (cdr (assoc class applications :test #'string-equal)))))

(defcommand set-volume-from-menu () ()
  (when-let* ((window (stumpwm::select-window-from-menu (screen-windows (current-screen)) "%c" "App:"))
              (class (window-class window))
              (volume (read-one-line (current-screen) "Volume: ")))
    (set-application-volume class (parse-integer (remove #\% volume :test #'char=)))
    (values)))

(defcommand toggle-mute-from-menu () ()
  (when-let* ((window (stumpwm::select-window-from-menu (screen-windows (current-screen)) "%c" "App to mute:"))
              (class (window-class window)))
    (toggle-application-mute class)
    (values)))

(define-keys *top-map*
  ("XF86AudioRaiseVolume" . "volume-up")
  ("XF86AudioLowerVolume" . "volume-down")
  ("XF86AudioMute" . "volume-mute"))

;; Music
(defparameter *player* "spotify")
(defparameter *track* "  Paused  ")

(defun current-track ()
  (let ((fmt "playerctl --player=~a metadata --format \"{{trunc(artist,18)}} - {{trunc(title,16)}}\"" )
        (playing (string= "Playing" (run-formatted "playerctl --player=~a status" *player*))))
    (if playing
        (format nil "^(:fg \"#8f0075\") ^* ~a ^(:fg \"#8f0075\") ^*" (run-formatted fmt *player*))
        "^(:fg \"#8f0075\") ^* Paused ^(:fg \"#8f0075\") ^*" )))

(defun track-thread-action ()
  "This is the function for *TRACK-THREAD*, it updates the *TRACK* variable every second."
  (loop
    (setf *track* (current-track))
    (sleep 1)))

(defun list-players ()
  (mapcar (lambda (string)
            (car (uiop:split-string string :separator '(#\.))))
          (uiop:split-string (run-formatted "playerctl --list-all") :separator '(#\Newline))))

(defcommand set-player () ()
  (let ((selection (select-from-menu (current-screen) (list-players))))
    (when selection
      (setf *player* (car selection))))
  (values))

(defcommand toggle-track () ()
  (run-program (format nil "~~/.stumpwm.d/scripts/player_notify.sh ~A" *player*))
  (run-program (format nil "playerctl --player=~A play-pause" *player*)))

(defcommand next-track () ()
  (run-program (format nil "playerctl --player=~A next" *player*)))

(defcommand previous-track () ()
  (run-program (format nil "playerctl --player=~A previous" *player*)))

(define-keys *root-map*
  ("M-P" . "set-player")
  ("M-t" . "toggle-track")
  ("M-n" . "next-track")
  ("M-v" . "set-volume-from-menu")
  ("M-m" . "toggle-mute-from-menu")
  ("M-p". "previous-track"))
(define-keys *top-map*
  ("XF86AudioNext" . "next-track")
  ("XF86AudioPrev". "previous-track")
  ("Pause" . "toggle-track"))
