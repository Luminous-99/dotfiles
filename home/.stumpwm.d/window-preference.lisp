(defpackage window-preference
  (:use :cl :stumpwm :alexandria)
  (:import-from :stumpwm #:find-group)
  (:export
   #:place-windows
   #:preference-matches-p
   #:define-window-preferences
   #:define-window-preference
   #:*window-preferences*))

(in-package :window-preference)

(defvar *window-preferences* ()
  "A list of plists that define window preferences.")

(defun define-window-preference (&key class window-number group-name group-number)
  (let* ((preference (list :class class :window-number window-number
                           :group-name group-name :group-number group-number))
         (old-preference (find class *window-preferences*
                               :test (lambda (x y)
                                       (string= x (getf y :class))))))
    (if old-preference
        (setf (cdr old-preference) (cdr preference))
        (push preference *window-preferences*))))

(defmacro define-window-preferences (&body preferences)
  (let ((preference (gensym "PREFERENCE")))
    `(dolist (,preference ',preferences)
       (apply #'define-window-preference ,preference))))

(defun preference-matches-p (window preference)
  (when (string= (window-class window) (getf preference :class))
    preference))

(defun place-window (window)
  (when-let ((preference (find window *window-preferences* :test #'preference-matches-p)))
    (destructuring-bind (&key window-number group-name group-number &allow-other-keys)
        preference
      (when window-number
        (when-let ((old-window (find window-number (group-windows (window-group window))
                                     :key #'window-number)))
          (setf (window-number old-window) (window-number window)))
        (setf (window-number window) window-number))
      (when (and group-name (not (string= group-name (group-name (window-group window)))))
        (move-window-to-group window (find-group (window-screen window) group-name)))
      (when (and group-number (not (= group-number (group-number (window-group window)))))
        (let ((group (find group-number (screen-groups (window-screen window))
                           :key #'group-number)))
          (move-window-to-group window group))))))

(defcommand place-windows (&optional (screen (current-screen))) ()
  "Place windows according to *WINDOW-PREFERENCES*."
  (dolist (window (screen-windows screen))
    (place-window window)))
