(defpackage mode-line
  (:use :cl :stumpwm :clx-truetype :contrib :misc :audio :alexandria :windows)
  (:export #:defclick)
  (:import-from :stumpwm
                #:decode-button-code
                #:window-by-id
                #:find-group
                #:switch-to-group))

(in-package :mode-line)

(defun display-groups-with-window-name ()
  (let ((groups (copy-list (screen-groups (current-screen)))))
    (loop for group in (sort groups #'< :key #'group-number)
          if (= (group-number (current-group))
                (group-number group))
            collect (format nil "^R[~a ~a]^r "
                            (group-number group)
                            (if-let ((window (current-window)))
                              (window-class window)
                              ""))
          else
            collect (format nil "[~a] " (group-number group)))))

(defmacro defclick (name (&rest arguments) &body body)
  "Define an on click function for the modeline. BODY is a list of lists where
CAR is either any value returned by decode-button-code or T for any unspecified match."
  `(progn
     (defun ,name (code ,@arguments)
       (declare (ignorable code))
       (case (decode-button-code code)
         ,@body))
     (register-ml-on-click-id ,(make-keyword name) #',name)))

(defclick music-player-click ()
  (:left-button (toggle-track))
  (:middle-button (set-volume-from-menu))
  (:right-button (set-player)))

(defclick next-track-click ()
  (t (audio:next-track)))

(defclick previous-track-click ()
  (t (audio:previous-track)))

(defparameter *stumpwm-font*
  (make-instance 'xft:font
                 :family (font-family *font*)
                 :subfamily (font-subfamily *font*)
                 :size 80))

(appendf (stumpwm::screen-fonts (car *screen-list*)) (list *stumpwm-font*))

(defclick lisp-icon ()
  (:left-button
   (message "^(:font 1)( ^(:fg \"#4d4d4d\")Stump^**^(:fg \"#ff7f2a\")WM^**)")
   (x-setup))
  (:middle-button
   (when (stumpwm::y-or-n-p (format nil "Do you want to shutdown?~%"))
     (when (stumpwm::y-or-n-p (format nil "Are you sure?~%"))
       (run-program "shutdown now")))
   (clear-messages))
  (:right-button
   (when (stumpwm::y-or-n-p (format nil "Auto run programs?~%"))
     (auto-start))
   (clear-messages)))

(defclick ml-on-click-focus-window (id)
  ((or :left-button :middle-button)
   (let ((window (window-by-id id)))
     (focus-window window)))
  (:right-button
   (let ((window (window-by-id id)))
     (toggle-float window)
     (focus-window window)))
  (:wheel-up (pull-hidden-next))
  (:wheel-down (pull-hidden-previous)))

(defclick ml-on-click-switch-to-group (group)
  (:wheel-up (gnext))
  (:wheel-down (gprev))
  (t (switch-to-group (find-group (current-screen) group))))

(setf *mode-line-background-color* *background-color*
      *mode-line-foreground-color* *foreground-color*
      *mode-line-border-width* 0
      *time-modeline-string* "%a %b %e %k:%M"
      *mode-line-timeout* 2)

(defun window-title-formatter (window)
  (string-upcase (cond
                   ((search "steam" (window-class window)) (window-title window))
                   (t (window-class window)))
                 :end 1))

(appendf *window-formatters* '((#\N window-title-formatter)))
(setf *window-format* "(%n . %N)")
(setf *group-format* "%n")

(setf *screen-mode-line-format*
      '(" ^(:on-click :lisp-icon) ^(:on-click-end) "
        "'(%g) '(%W) ^> ^(:on-click :music-player-click) "
        (:eval
         *track*)
        "^(:on-click-end)"
        "^(:on-click :previous-track-click)  ^(:on-click-end)"
        "^(:on-click :next-track-click) ^(:on-click-end)"
        "│ %C │ %M │ ^2󰁹^* %B │ %d"))
(enable-mode-line (current-screen) (current-head) t)
(defvar *track-thread* (bt:make-thread #'track-thread-action :name "Track Thread"))
