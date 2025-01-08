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
   #:toggle-application-mute))

(in-package :audio)

;; Volume
(defcommand volume-up () ()
  (run-shell-command "~/.stumpwm.d/scripts/volume_notify.sh Up"))

(defcommand volume-down () ()
  (run-shell-command "~/.stumpwm.d/scripts/volume_notify.sh Down"))

(defcommand volume-mute () ()
  (run-shell-command "~/.stumpwm.d/scripts/volume_notify.sh Mute"))

(defun get-pactl-applications ()
  (flet ((simplify-list (string)
           (unless (zerop (length string))
             (if (char= #\S (aref string 0))
                 (string-trim '(#\#) (car (last (uiop:split-string string :separator '(#\Space)))))
                 (string-downcase (string-trim '(#\" #\ ) (second (uiop:split-string (string-trim '(#\Space #\Tab) string) :separator '(#\=)))))))))
    (let* ((pactl-output (run-shell-command "pactl list sink-inputs | grep -e node.name -e ' #[0-9]*'" t))
           (output-list (mapcar #'simplify-list (uiop:split-string pactl-output :separator '(#\Newline)))))
      (loop for (x y) on output-list by #'cddr
            when x
              collect (cons y x)))))

(defun set-application-volume (class volume)
  "Set an application's VOLUME."
  (let* ((applications (get-pactl-applications)))
    (run-formatted "pactl set-sink-input-volume ~a ~a%" (cdr (assoc (string-downcase class) applications :test #'string=)) volume)))

(defun toggle-application-mute (class)
  "Toggle whether an application is muted."
  (let* ((applications (get-pactl-applications)))
    (run-formatted "pactl set-sink-input-volume ~a ~a%" (cdr (assoc (string-downcase class) applications :test #'string=)))))

(defcommand set-volume-from-menu () ()
  (when-let* ((window (stumpwm::select-window-from-menu (list-all-windows) "%c" "App:"))
              (class (window-class window))
              (volume (read-one-line (current-screen) "Volume: ")))
    (set-application-volume class (parse-integer volume)))
  (values))

(define-keys *top-map*
  ("XF86AudioRaiseVolume" . "volume-up")
  ("XF86AudioLowerVolume" . "volume-down")
  ("XF86AudioMute" . "volume-mute"))


;; Music
(defparameter *player* "spotify")
(defparameter *track* "  Paused  ")

(defun current-track ()
  (let ((fmt "playerctl --player=~a metadata --format \"{{trunc(artist,15)}} - {{trunc(title,16)}}\"" )
        (playing (string= "Playing" (run-formatted "playerctl --player=~a status" *player*))))
    (if playing
        (format nil "^(:fg \"#8f0075\") ^* ~a ^(:fg \"#8f0075\") ^*" (run-formatted fmt *player*))
        "^(:fg \"#8f0075\") ^* Paused ^(:fg \"#8f0075\") ^*" )))

(defun track-thread-action ()
  "This is the function for *TRACK-THREAD*, it updates the *TRACK* variable every second."
  (loop
    (setf *track* (current-track))
    (sleep 1)))

(defcommand set-player () ()
  (let ((selection (select-from-menu (current-screen)
                                     (mapcar (lambda (string)
                                               (car (uiop:split-string string :separator '(#\.))))
                                             (uiop:split-string (run-formatted "playerctl --list-all") :separator '(#\Newline))))))
    (when selection
      (setf *player* (car selection))))
  (values))

(defcommand toggle-track () ()
  (run-program (format nil ".stumpwm.d/scripts/player_notify.sh ~a" *player*))
  (run-program (format nil "playerctl --player=~a play-pause" *player*)))

(defcommand next-track () ()
  (run-program (format nil "playerctl --player=~a next" *player*)))

(defcommand previous-track () ()
  (run-program (format nil "playerctl --player=~a previous" *player*)))

(define-keys *root-map*
  ("M-P" . "set-player")
  ("M-t" . "toggle-track")
  ("M-n" . "next-track")
  ("M-v" . "set-volume-from-menu")
  ("M-p". "previous-track"))
(define-keys *top-map*
  ("XF86AudioNext" . "next-track")
  ("XF86AudioPrev". "previous-track")
  ("Pause" . "toggle-track"))
