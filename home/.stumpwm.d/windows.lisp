(defpackage windows
  (:use :cl :stumpwm :misc :contrib)
  (:import-from :alexandria
                #:when-let*)
  (:export
   #:float-unless-maximized
   #:sort-current-group
   #:set-floating
   #:set-window-property
   #:list-all-windows))

(in-package :windows)

(setf *window-border-style* :none
      *new-window-preferred-frame* '(:unfocused)
      *new-frame-action* :last-window
      *mouse-focus-policy* :click
      *message-window-gravity* :center
      *input-window-gravity* :center
      stumpwm::*float-window-title-height* 15
      *maxsize-border-width* 0
      *normal-border-width* 0
      *transient-border-width* 0
      *message-window-padding* 5
      *message-window-y-padding* 5
      swm-gaps:*outer-gaps-size* 5
      swm-gaps:*inner-gaps-size* 5)

(defun set-window-property (window property value)
  (xlib:change-property (window-xwin window) property (list value) :integer 32))

(defun set-floating (window floating)
  (cond 
    ((eq floating nil)
     (when (stumpwm::float-window-p window)
       (stumpwm::unfloat-window window (current-group))
       (set-window-property window :_STUMPWM_FLOATING 0)))
    ((eq floating t)
     (stumpwm::float-window window (current-group))
     (set-window-property window :_STUMPWM_FLOATING 1))))

(defun sort-current-group (window)
  "Sort windows on destruction."
  (declare (ignorable window))
  (let* ((windows (stumpwm::list-windows (current-group)))
         (windows (sort windows (lambda (x y)
                                  (when y 
                                    (< (window-number x)
                                       (window-number y)))))))
    (loop for (win1 win2) on windows
          for num1 = (and win1 (window-number win1))
          for num2 = (and win2 (window-number win2))
          when (and num2 (> num2 (1+ num1))) 
            do (setf win2 (1+ num1)))))

(setf *destroy-window-hook* (list #'sort-current-group))

(defun float-unless-maximized (window)
  (unless swm-gaps:*gaps-on* 
      (when-let* ((screen (current-screen))
                  (mode-line (car (stumpwm::screen-mode-lines (current-screen))))
                  (maximized-width (screen-width screen))
                  (maximized-height (- (screen-height screen) (stumpwm::mode-line-height mode-line))))
        (let ((width (window-width window))
              (height (window-height window)))
          (when (or (< width maximized-width) (< height maximized-height))
            (set-floating window t)
            (focus-window window t))))))

(setf *new-window-hook* (list #'float-unless-maximized))

(defparameter *resizing-mode* nil)
(setf *resize-increment* 20)

(defun float-move-resize (window &key x y width height)
  (stumpwm::float-window-move-resize window
                                     :x (if x x (window-x window))
                                     :y (if y y (window-y window))
                                     :width (if width width (window-width window))
                                     :height (if height height (window-height window))))

(defun float-resize (window dir)
  (with-slots (width height) window
    (case dir
      (:up (float-move-resize window :height (- height *resize-increment*)))
      (:left (float-move-resize window :width (- width *resize-increment*)))
      (:right (float-move-resize window :width (+ width *resize-increment*)))
      (:down (float-move-resize window :height (+ height *resize-increment*))))))

(defun float-move (window dir)
  (with-slots (x y) window
    (case dir
      (:up (float-move-resize window :y (- y *resize-increment*)))
      (:left (float-move-resize window :x (- x *resize-increment*)))
      (:right (float-move-resize window :x (+ x *resize-increment*)))
      (:down (float-move-resize window :y (+ y *resize-increment*))))))

(defcommand move (dir) ((:direction "Direction: "))
  (let ((window (current-window)))
    (if (stumpwm::float-window-p window)
        (if *resizing-mode*
            (float-resize window dir)
            (float-move window dir))
        (move-focus dir))))

(defcommand iresize-float () ()
  (setf *resizing-mode* (not *resizing-mode*))
  (if *resizing-mode*
      (define-keys *top-map*
        ("Left" . "move left")
        ("Right" . "move right")
        ("Up" . "move up")
        ("Down" . "move down")
        ("RET" . "iresize-float"))
      (undefine-keys *top-map* "Left" "Right" "Up" "Down" "RET")))

(defcommand close-window-and-frame () ()
  (unless (only-one-frame-p)
    (delete-window)
    (remove-split))
  (toggle-gaps))

(defcommand terminate-this () ()
  (let ((window (current-window)))
    (kill-window window)
    (toggle-gaps)))

(defcommand toggle-float () ()
  (let ((window (current-window)))
    (if (stumpwm::float-window-p window)
        (set-floating window nil)
        (set-floating window t))))

(defun list-all-windows ()
  (apply #'append
         (loop for group in (screen-groups (current-screen))
               collect (stumpwm::list-windows group))))

(defcommand delete-split () ()
  (remove-split)
  (toggle-gaps))

(defcommand split-horizontal () ()
  (hsplit)
  (toggle-gaps))

(defcommand split-vertical () ()
  (vsplit)
  (toggle-gaps))

(defcommand clear-messages () ()
  (stumpwm::unmap-message-window (current-screen)))

(define-keys *root-map*
  ("f" . "toggle-float")
  ("F" . "toggle-always-on-top")
  ("R" . "iresize-float")
  ("x" . "delete-split")
  ("X" . "close-window-and-frame")
  ("s" . "split-vertical")
  ("S" . "split-horizontal")
  ("k" . "terminate-this")
  ("C-k" . "terminate-this")
  ("K" . "terminate-this")
  ("Up" . "move up")
  ("Left" . "move left")
  ("Right" . "move right")
  ("Down" . "move down")
  ("ESC" . "clear-messages")
  ("M-Up" . "exchange-direction up")
  ("M-Left" . "exchange-direction left")
  ("M-Right" . "exchange-direction right")
  ("M-Down" . "exchange-direction down"))

;; Groups
(dotimes (i 9)
  (define-key *root-map* (kbd (format nil "M-~a" (1+ i)))
    (format nil "gmove ~a" (1+ i))))

(dotimes (i 7)
  (gnew (format nil "Group ~a" (+ 2 i))))
(gnew-dynamic "Group 9")

(gselect "1")
