(defpackage scratchpad
  (:use :cl :stumpwm :misc)
  (:export
   #:withdraw-window
   #:withdraw-all-windows
   #:restore-window
   #:restore-newest-window
   #:restore-oldest-window
   #:restore-all-windows
   #:wallpaper-mode
   #:*scratchpad-map*))

(in-package :scratchpad)

(defcommand withdraw-window (&optional (window (current-window))) ()
  (setf (gethash :number (window-plist window)) (window-number window))
  (stumpwm::withdraw-window window))

(defcommand withdraw-all-windows (&optional (group (current-group))) ()
  (dolist (window (group-windows group))
    (handler-case (stumpwm::withdraw-window window)
      (error (err) (err "~A" err)))))

(flet ((window-from-menu ()
         (let ((windows (stumpwm::screen-withdrawn-windows (current-screen))))
           (and windows (stumpwm::select-window-from-menu windows *window-format*)))))
  (defcommand restore-window (&optional (window (window-from-menu))) ()
    (when window
      (let* ((windows (group-windows (current-group)))
             (windows (stumpwm::sort-windows-by-number windows))
             (restored-number (gethash :number (window-plist window)))
             (obstruction (find (or restored-number (window-number window)) windows :key #'window-number :test #'=)))
        (setf (gethash :number (window-plist window)) nil)
        (if obstruction
            (setf (window-number obstruction) (window-number window)
                  (window-number window) restored-number)
            (setf (window-number window) restored-number))
        (setf (window-group window) (current-group))
        (stumpwm::restore-window window)))))

(defcommand restore-newest-window () ()
  (restore-window (car (stumpwm::screen-withdrawn-windows (current-screen)))))

(defcommand restore-oldest-window () ()
  (restore-window (car (last (stumpwm::screen-withdrawn-windows (current-screen))))))

(defcommand restore-all-windows () ()
  (dolist (window (stumpwm::screen-withdrawn-windows (current-screen)))
    (handler-case (restore-window window)
      (error (err) (err "~a" err)))))

(let ((wallpaper-mode nil))
  (defcommand wallpaper-mode () ()
    (setf wallpaper-mode (not wallpaper-mode))
    (if wallpaper-mode
        (progn
          (enable-mode-line (current-screen) (current-head) nil)
          (withdraw-all-windows)
          (define-key *top-map* (kbd "ESC") "wallpaper-mode"))
        (progn
          (enable-mode-line (current-screen) (current-head) t)
          (restore-all-windows)
          (undefine-key *top-map* (kbd "ESC"))))))

(defparameter *scratchpad-map*
  (let ((map (make-sparse-keymap)))
    (define-keys map
      ("w" . "withdraw-window")
      ("a" . "withdraw-all-windows")
      ("r" . "restore-window")
      ("1" . "restore-newest-window")
      ("0" . "restore-oldest-window")
      ("*" . "restore-all-windows")
      ("ESC" . "wallpaper-mode"))
    map))
