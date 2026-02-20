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
   #:volume-Value
   #:forward-track
   #:backward-track
   #:player-notify))

(in-package :audio)

;; Volume
(defcommand volume-value () ()
  (string-trim '(#\Newline) (run-shell-command "~/.stumpwm.d/scripts/volume_notify.sh Volume" t)))

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
(defparameter *player* "org.mpris.MediaPlayer2.spotify")
(defparameter *track* "^(:fg \"#8f0075\") ^* Paused ^(:fg \"#8f0075\") ^*")

(defun truncate-string (string size &optional trail)
  (if (<= (length string) size)
      string
      (if trail
          (concatenate 'string (subseq string 0 size) trail)
          (subseq string 0 size))))

(let ((return-fmt (formatter "^(:fg \"#8f0075\") ^* ~A ^(:fg \"#8f0075\") ^*")))
  (defun current-track ()
    (handler-case
        (let* ((metadata (mpris:player-track-metadata *player*))
               (title (truncate-string (cadr (assoc "xesam:title" metadata :test #'string=)) 16 "…"))
               (album (truncate-string (cadr (assoc "xesam:album" metadata :test #'string=)) 18 "…"))
               (playing (mpris:player-playback-status *player*)))
          (if (string= playing "Playing")
              (format nil return-fmt (if (zerop (length album))
                                         title
                                         (format nil "~A - ~A" album title)))
              "^(:fg \"#8f0075\") ^* Paused ^(:fg \"#8f0075\") ^*"))
      (error () "^(:fg \"#8f0075\") ^* Paused ^(:fg \"#8f0075\") ^*"))))

(defun track-thread-action ()
  "This is the function for *TRACK-THREAD*, it updates the *TRACK* variable every second."
  (loop
    (setf *track* (current-track))
    (sleep 1)))

(defcommand set-player () ()
  (let ((selection (select-from-menu (current-screen) (mapcar (lambda (x) (fourth (uiop:split-string x :separator '(#\.)))) (mpris:list-players)))))
    (when selection
      (setf *player* (mpris:player-by-name (car selection)))))
  (values))

(flet ((gets (x y)
         (cadr (assoc x y :test #'string=)))
       (replace-spaces (x)
         (map 'string (lambda (x) (if (char= x #\Space) #\_ x)) x)))
  (defun player-notify (player)
    (ensure-directories-exist #P"/tmp/player_notify_icons/")
    (let* ((data (mpris:player-track-metadata player))
           (artist (car (gets "xesam:artist" data)))
           (title (gets "xesam:title" data))
           (album (gets "xesam:album" data))
           (url (gets "mpris:artUrl" data))
           (sartist (replace-spaces artist))
           (stitle (replace-spaces title))
           (icon-file (format nil "/tmp/player_notify_icons/~A/~A" sartist stitle))
           (body (format nil "~A - ~A" (truncate-string artist 30 "...") (truncate-string title 30 "..."))))
      (unless (probe-file icon-file)
        (ensure-directories-exist (format nil "/tmp/player_notify_icons/~A/" sartist))
        (run-shell-command (format nil "curl -o ~S -L ~S" icon-file url) t))
      (freedesktop-notifications:notify (truncate-string album 30 "...") body :app-icon icon-file))))

(defcommand toggle-track () ()
  (mpris:player-play-pause *player*)
  (bt:make-thread (lambda () (player-notify *player*))))

(defcommand forward-track () ()
  (mpris:player-seek *player* 5))

(defcommand backward-track () ()
  (mpris:player-seek *player* -5))

(defcommand next-track () ()
  (mpris:player-next *player*))

(defcommand previous-track () ()
  (mpris:player-previous *player*))

(defparameter *audio-map* (make-sparse-keymap))

(define-keys *audio-map*
  ("P" . "set-player")
  ("t" . "toggle-track")
  ("p". "previous-track")
  ("n" . "next-track")
  ("v" . "set-volume-from-menu")
  ("m" . "toggle-mute-from-menu"))

(define-keys *root-map*
  ("M-p" . '*audio-map*))

(define-keys *top-map*
  ("XF86AudioNext" . "next-track")
  ("XF86AudioPrev". "previous-track")
  ("M-XF86AudioNext" . "forward-track")
  ("M-XF86AudioPrev". "backward-track")
  ("Pause" . "toggle-track"))
