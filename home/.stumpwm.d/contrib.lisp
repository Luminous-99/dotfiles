(defpackage contrib
  (:use :cl :stumpwm)
  (:import-from :cpu
                #:*cpu-modeline-fmt*
                #:*cpu-usage-modeline-fmt*)
  (:import-from :swm-gaps
                #:*outer-gaps-size*
                #:*inner-gaps-size*
                #:toggle-gaps-on
                #:toggle-gaps-off)
  (:import-from :mem #:*mem-modeline-fmt*)
  (:export
   #:toggle-gaps))

(in-package :contrib)

(toggle-gaps-off)
(setf *cpu-modeline-fmt* "%c %t"
      *cpu-usage-modeline-fmt*  " ^[~A~3D%^]"
      *mem-modeline-fmt* "  %a%p"
      *outer-gaps-size* 3
      *inner-gaps-size* 3)

(in-package :stumpwm)

(define-swm-class group ()
  ((screen :initarg :screen :accessor group-screen)
   (windows :initform nil :accessor group-windows)
   (current-window :initform nil :accessor group-current-window)
   (raised-window :initform nil :accessor group-raised-window)
   (number :initarg :number :accessor group-number)
   (name :initarg :name :accessor group-name)
   (on-top-windows :initform nil :accessor group-on-top-windows)
   (plist :initform nil :initarg :plist :accessor group-plist)))

(defcommand gselect (&optional to-group) (:rest)
  "Accepts numbers to select a group, otherwise grouplist selects."
  (if-let ((to-group (when to-group
                       (select-group (current-screen) to-group))))
    (switch-to-group to-group)
    (grouplist))
  (if (getf (group-plist (current-group)) :GAPS)
      (swm-gaps::add-head-gaps)
      (refresh-heads)))

(in-package :swm-gaps)

(defun apply-gaps-p (win)
  "Tell if gaps should be applied to this window"
  (and (getf (stumpwm::group-plist (current-group)) :GAPS)
       (not (stumpwm::window-transient-p win))
       (not (window-fullscreen win))))

(defun reset-all-windows ()
  "Reset the size for all tiled windows"
  (mapcar #'stumpwm::maximize-window
          (stumpwm::only-tile-windows (group-windows (current-group)))))

(defcommand toggle-gaps-on () ()
  "Turn gaps on"
  (setf (getf (stumpwm::group-plist (current-group)) :GAPS) t)
  (add-head-gaps)
  (reset-all-windows))

(defcommand toggle-gaps-off () ()
  "Turn gaps off"
  (setf (getf (stumpwm::group-plist (current-group)) :GAPS) nil)
  (stumpwm:refresh-heads))

(in-package :contrib)

(defun toggle-gaps ()
  (when (typep (current-group) 'stumpwm::tile-group)
    (if (cdr (stumpwm::group-frames (current-group)))
        (toggle-gaps-on)
        (toggle-gaps-off))))
