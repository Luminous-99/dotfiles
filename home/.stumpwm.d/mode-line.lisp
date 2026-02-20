(defpackage mode-line
  (:use :cl :stumpwm :clx-truetype :contrib :misc :audio :alexandria :windows)
  (:export #:defclick)
  (:import-from :stumpwm
                #:decode-button-code
                #:window-by-id
                #:find-group
                #:switch-to-group
                #:update-all-mode-lines)
  (:import-from :symbol-hooks #:define-symbol-hook)
  (:shadowing-import-from :symbol-hooks #:setf))

(in-package :mode-line)

(defmacro defclick (name (&rest arguments) &body body)
  "Define an on click function for the modeline. BODY is a list of lists where
CAR is either any value returned by decode-button-code or T for any unspecified match."
  (let* ((after (assoc :after body))
         (before (assoc :before body))
         (body (remove after (remove before body))))
    `(progn
       (defun ,name (code ,@arguments)
         (declare (ignorable code))
         ,@(cdr before)
         (case (decode-button-code code)
           ,@body)
         ,@(cdr after))
       (register-ml-on-click-id ,(make-keyword name) #',name))))

(defclick music-player-click ()
  (:after (update-all-mode-lines))
  (:left-button (toggle-track))
  (:middle-button (set-volume-from-menu))
  (:wheel-up (volume-up))
  (:wheel-down (volume-down))
  (:right-button (set-player)))

(defclick next-track-click ()
  (t (next-track)))

(defclick previous-track-click ()
  (t (previous-track)))

(defclick volume-text-click ()
  (:after (update-all-mode-lines))
  (t (volume-mute)))

(defparameter *stumpwm-font*
  (make-instance 'xft:font
                 :family (font-family *font*)
                 :subfamily (font-subfamily *font*)
                 :size 80))

(appendf (stumpwm::screen-fonts (car *screen-list*)) (list *stumpwm-font*))

(defun -y-or-n-p (message &optional (prompt "(y or n)"))
  "Ask a \"y or n\" question on the current screen and return T if the
user presses 'y'."
  (message "~A~A" message prompt)
  (eql (read-one-char (current-screen))
       #\y))

(setf *startup-message* "^(:font 1)^(:fg \"#4d4d4d\")( Stump^**^(:fg \"#ff7f2a\")WM^**^(:fg \"#4d4d4d\"))^**") 

(defclick lisp-icon ()
  (:left-button
   (message *startup-message*)
   (x-setup))
  (:middle-button
   (when (-y-or-n-p (format nil "^1Do you want to shutdown?~%^*") "(^2y^* or ^1n^*)")
     (when (-y-or-n-p (format nil "^1Are you sure?~%^*") "(^2y^* or ^1n^*)")
       (run-program "shutdown now")))
   (clear-messages))
  (:right-button
   (when (-y-or-n-p (format nil "Auto run programs?~%"))
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

(defclick add-group-click ()
  ((or :right-button :middle-button :left-button)
   (gnewbg (format nil "Group ~A" (1+ (length (screen-groups (current-screen))))))
   (update-all-mode-lines)))

(defclick ml-on-click-switch-to-group (group)
  (:wheel-up (gnext))
  (:wheel-down (gprev))
  (:middle-button
   (stumpwm::kill-group (find-group (current-screen) group) (current-group))
   (update-all-mode-lines))
  (t (gselect group)))

(defclick groups-click ()
  (:wheel-up (gnext))
  (:wheel-down (gprev)))

(defclick windows-click ()
  (:wheel-up (pull-hidden-next))
  (:wheel-down (pull-hidden-previous)))

(setf *mode-line-background-color* *background-color*
      *mode-line-foreground-color* "#333333"
      *mode-line-border-width* 0
      *time-modeline-string* "%a %b %d/%m/%y %H:%M"
      *mode-line-timeout* 2)

(macrolet ((refresh-mode-line ()
             `(progn
                (mode-line)
                (mode-line))))
  (define-symbol-hook *mode-line-background-color*
    (unless (string= *mode-line-background-color* new-value)
      (refresh-mode-line)))
  (define-symbol-hook *mode-line-foreground-color*
    (unless (string= *mode-line-foreground-color* new-value)
      (refresh-mode-line))))

(defun window-title-formatter (window)
  (let ((title (cond ((search "steam" (window-class window)) (window-title window))
                     (t (window-class window)))))
    (unless (zerop (length title))
      (string-upcase title :end 1))))

(appendf *window-formatters* '((#\N window-title-formatter)))
(setf *window-format* "(%n . %N)")
(setf *group-format* "%n")

(defmacro with-formatting (&body body)
  (flet ((on-click->string (x)
           (if (and (consp x) (eq (car x) :on-click))
               (let ((last (car (last x))))
                 (if (consp last)
                     `(list ,(format nil "^(:on-click ~{~S~^ ~})" (cdr (butlast x)))
                            ,last
                            "^(:on-click-end)")
                     (format nil "^(:on-click ~{~S~^ ~})~A^(:on-click-end)" (cdr (butlast x)) last)))
               x)))
    (let ((body (mapcar #'on-click->string body)))
      `',(reverse (loop for x in body
                        with list = (list)
                        finally (return list)
                        do (if (and (consp x) (eq (car x) 'list))
                               (dolist (x (cdr x))
                                 (push x list))
                               (if (and (stringp x) (stringp (car list))) 
                                   (setf (car list) (concatenate 'string (car list) x))
                                   (push x list))))))))

(defun battery-fmt (modeline)
  (declare (ignorable modeline))
  (if battery-portable::*preferred-drivers-failed*
      ""
      (format nil " (^2󰁹^* ~A)" (battery-portable::fmt-bat modeline))))

(defun volume-formatter (modeline)
  (declare (ignorable modeline))
  (handler-case
      (let ((volume (volume-value)))
        (format nil " ~A~:[ ~;%~]" volume (digit-char-p (schar volume 0))))
    (t () " 0%")))

(defun mem::fmt-mem-allocated (mem)
  "Returns a string representing the current allocated memory."
  (let* ((allocated (truncate (/ (nth 1 mem) 1000))))
    (format nil "~4D mb" allocated)))

(defun mem::fmt-mem-percent (mem)
  "Returns a string representing the current percent of used memory."
  (let* ((% (* 100 (nth 2 mem))))
    (format nil "^[~A ~4F%^]" (bar-zone-color %) %)))

(add-screen-mode-line-formatter #\B #'battery-fmt)
(add-screen-mode-line-formatter #\V #'volume-formatter)

(let ((format (with-formatting
                (:on-click :lisp-icon "   ")
                (:on-click :add-group-click "(gnew)")
                (:on-click :groups-click " '(%g)")
                (:on-click :windows-click " '(%W)")
                "^>( "
                (:on-click :music-player-click (:eval *track*))
                (:on-click :volume-text-click "%V")
                (:on-click :previous-track-click "  ") 
                (:on-click :next-track-click "  ")
                ") (%C) (%M)%B (%d)")))
  (setf *screen-mode-line-format* format))

(enable-mode-line (current-screen) (current-head) t)
(defvar *track-thread* (bt:make-thread #'track-thread-action :name "Track Thread"))
