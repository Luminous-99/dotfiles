(defpackage mode-line
  (:use :cl :stumpwm :clx-truetype :contrib :misc :audio :alexandria :windows)
  (:export #:defclick))

(in-package :mode-line)

(defun display-groups-with-window-name ()
  (loop for group in (sort (copy-list (screen-groups (current-screen)))
                            (lambda (x y)
                              (< (group-number x)
                                 (group-number y))))
        if (= (group-number (current-group)) (group-number group))
          collect (format nil "^R[~a~a]^r "
                          (group-number group)
                          (if-let ((window (current-window)))
                            (format nil " ~a" (window-class window))
                            ""))
        else
          collect (format nil "[~a] " 
                          (group-number group))))

(defmacro defclick (name (&rest arguments) &body body)
  "Define an on click function for the modeline. BODY is a list of lists where
CAR is either any value returned by decode-button-code or T for any unspecified match."
  `(progn
     (defun ,name (code ,@arguments)
       (declare (ignorable code))
       (case (stumpwm::decode-button-code code)
         ,@body))
     (register-ml-on-click-id ,(make-keyword name) #',name)))

(defclick music-player-click ()
  (:left-button (toggle-track))
  (:middle-button (set-player))
  (:right-button (set-volume-from-menu)))

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
   (let ((window (stumpwm::window-by-id id)))
     (focus-window window)))
  (:right-button
   (let ((window (stumpwm::window-by-id id)))
     (toggle-float window)
     (focus-window window)))
  (:wheel-up (pull-hidden-next))
  (:wheel-down (pull-hidden-previous)))

(defclick ml-on-click-switch-to-group (group)
  (:wheel-up (gnext))
  (:wheel-down (gprev))
  (t (stumpwm::switch-to-group (stumpwm::find-group (current-screen) group))))

(setf *mode-line-background-color* *background-color*
      *mode-line-foreground-color* *foreground-color*
      *mode-line-border-width* 0
      *time-modeline-string* "%a %b %e %k:%M"
      *mode-line-timeout* 2)

(setf *window-format* "(%n . %c)")
(setf *group-format* "%n")

(setf *screen-mode-line-format*
      '(" ^(:on-click :lisp-icon) ^(:on-click-end) "
        "'(%g) '(%W) ^> ^(:on-click :music-player-click) "
        (:eval
         *track*)
        "^(:on-click-end) │ %C │ %M │ ^2󰁹^* %B │ %d"))
(enable-mode-line (current-screen) (current-head) t)
(defvar *track-thread* (bt:make-thread #'track-thread-action :name "Track Thread"))
