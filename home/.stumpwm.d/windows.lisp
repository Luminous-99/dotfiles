(defpackage windows
  (:use :cl :stumpwm :misc :contrib :window-preference :scratchpad)
  (:import-from :alexandria
                #:when-let*
                #:when-let)
  (:import-from :stumpwm
                #:find-group
                #:switch-to-group
                #:gravity-for-window
                #:if-let)
  (:export
   #:set-floating
   #:float-move-resize
   #:*resizing-mode*
   #:*resize-increment*
   #:float-resize
   #:float-move
   #:move
   #:iresize-float
   #:close-window-and-frame
   #:xprop
   #:terminate
   #:terminate-this
   #:terminate-by-number
   #:toggle-float
   #:delete-split
   #:split-horizontal
   #:place-windows
   #:renumber-to-next
   #:renumber-to-previous
   #:split-vertical
   #:clear-messages
   #:eval-line
   #:withdraw-all-windows
   #:wallpaper-mode
   #:restore-newest-window
   #:restore-oldest-window
   #:withdraw-window
   #:restore-all-windows
   #:restore-window))

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
      *message-window-padding* 15
      *message-window-y-padding* 15)

(set-msg-border-width 1)
(set-border-color *foreground-color*)

(defun (setf stumpwm::window-property) (value window property)
  (multiple-value-bind (value type format)
      (etypecase value
        (integer (values (list value) :integer 32))
        (string (values (map 'vector #'char-code value) :string 8))
        (symbol (values (map 'vector #'char-code (symbol-name value)) :string 8)))
    (xlib:change-property (if (xlib:window-p window) window (window-xwin window)) property value type format)))

(defun set-floating (window floating)
  (if floating
      (progn
        (stumpwm::float-window window (current-group))
        (setf (stumpwm::window-property window :_STUMPWM_FLOATING) 1))
      (when (stumpwm::float-window-p window)
        (stumpwm::unfloat-window window (current-group))
        (setf (stumpwm::window-property window :_STUMPWM_FLOATING) 0))))

(defsetf stumpwm::float-window-p set-floating)

(defun %repack-windows (window)
  (declare (ignore window))
  (repack-window-numbers))
(add-hook *destroy-window-hook* '%repack-windows)

(defun float-on-creation (window)
  (let* ((hints (window-normal-hints window))
         (state (mapcar (lambda (x) (xlib:atom-name *display* x)) (xlib:get-property (window-xwin window) :_net_wm_state)))
         (program-specified-size-p (xlib::wm-size-hints-program-specified-size-p hints))
         (user-specified-size-p (xlib::wm-size-hints-user-specified-size-p hints))
         (x (xlib:wm-size-hints-x hints))
         (y (xlib:wm-size-hints-y hints))
         (width (xlib:wm-size-hints-width hints))
         (max-width (xlib:wm-size-hints-max-width hints))
         (min-width (xlib:wm-size-hints-min-width hints))
         (height (xlib:wm-size-hints-height hints))
         (max-height (xlib:wm-size-hints-max-height hints))
         (min-height (xlib:wm-size-hints-min-height hints)))
    (cond
      ((equalp state '(:_net_wm_state_maximized_horz :_net_wm_state_maximized_vert)))
      ((or user-specified-size-p program-specified-size-p)
       (set-floating window t)
       (let ((x (or x (floor (- (screen-width (current-screen)) width) 2.0)))
             (y (or y (floor (- (screen-height (current-screen)) height) 2.0))))
         (float-move-resize window :x x :y y :width width :height height))
       (focus-window window t))
      ((and max-width
            max-height
            min-width
            min-height
            (= max-width min-width)
            (= max-height min-height))
       (set-floating window t)
       (let ((x (if (or (not x) (zerop x))
                    (floor (- (screen-width (current-screen)) max-width) 2.0)
                    x))
             (y (if (or (not y) (zerop y))
                    (floor (- (screen-height (current-screen)) max-height) 2.0)
                    y)))
         (float-move-resize window :x x :y y :width max-width :height max-height))
       (focus-window window t))
      (t))))

(add-hook *new-window-hook* 'float-on-creation)

(defparameter *resizing-mode* nil)
(setf *resize-increment* 20)

(defun float-move-resize (window &key (x (window-x window))
                                   (y (window-y window))
                                   (width (window-width window))
                                   (height (window-height window)))
  (stumpwm::float-window-move-resize window :x x :y y :width width :height height))

(defun float-resize (window dir)
  (with-accessors ((width window-width) (height window-height)) window
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

(defcommand terminate (window) ()
  (bt:with-lock-held ((gethash :lock (window-plist window)))
    (delete-window window)
    (toggle-gaps)))

(defcommand close-window-and-frame () ()
  (handler-case
      (unless (only-one-frame-p)
        (let ((window (current-window)))
          (delete-split)
          (terminate window)))
    (error (err)
      (err "~A" err))))

(defcommand terminate-this () ()
  (terminate (current-window)))

(defcommand terminate-by-number (&optional number) ((:number "Window number: "))
  (let ((window (find number (stumpwm::sort-windows-by-number (current-group))
                      :key #'window-number :test #'=)))
    (stumpwm::destroy-window window)
    (toggle-gaps)))

(defcommand toggle-float (&optional (window (current-window))) ()
  (set-floating window (not (stumpwm::float-window-p window))))

(defcommand delete-split () ()
  (remove-split)
  (toggle-gaps))

(defcommand split-horizontal () ()
  (hsplit)
  (toggle-gaps))

(defcommand split-vertical () ()
  (vsplit)
  (toggle-gaps))

(defcommand renumber-to-next () ()
  (renumber (1+ (window-number (current-window)))))

(defcommand renumber-to-previous () ()
  (let ((number (window-number (current-window))))
    (renumber (if (zerop number) number (1- number)))))

(defcommand clear-messages () ()
  (stumpwm::unmap-message-window (current-screen)))

(defcommand xprop () ()
  (message "~A" (run-shell-command (format nil "xprop -id ~A" (stumpwm::window-id (current-window))) t)))

(defcommand pull-other () ()
  (if (only-one-frame-p)
      (pull-hidden-other)
      (fnext)))

(define-keys *root-map*
  ("o" . '*scratchpad-map*)
  ("W" . "place-windows")
  ("f" . "toggle-float")
  ("F" . "toggle-always-on-top")
  (stumpwm::*escape-key* . "pull-other")
  ("R" . "iresize-float")
  ("x" . "delete-split")
  ("X" . "close-window-and-frame")
  ("s" . "split-vertical")
  ("S" . "split-horizontal")
  ("k" . "terminate-this")
  ("K" . "terminate-by-number")
  ("C-n" . "renumber-to-next")
  ("C-p" . "renumber-to-previous")
  ("Up" . "move up")
  ("Left" . "move left")
  ("Right" . "move right")
  ("Down" . "move down")
  ("ESC" . "clear-messages")
  ("M-x" . "xprop")
  ("C-m" . "copy-last-message")
  ("M-Up" . "exchange-direction up")
  ("M-Left" . "exchange-direction left")
  ("M-Right" . "exchange-direction right")
  ("M-Down" . "exchange-direction down"))

;; Groups
(setf (group-name (find-group (current-screen) "Default")) "Group 1")

(define-window-preferences
  (:class "Emacs" :window-number 0 :group-name "Group 1")
  (:class "firefox" :window-number 1 :group-name "Group 1")
  (:class "discord" :window-number 0 :group-name "Group 2")
  (:class "org.mozilla.Thunderbird" :window-number 0 :group-name "Group 3")
  (:class "Spotify" :window-number 1 :group-name "Group 2"))

(dotimes (i 9)
  (define-key *root-map* (kbd (format nil "M-~a" (1+ i)))
    (format nil "gmove ~A" (1+ i))))

(define-key *root-map* (kbd "M-n") "gselect")

(dotimes (i 7)
  (gnew (format nil "Group ~A" (+ 2 i))))
(gnew-float "Group 9")

(gselect "1")

(defcommand eval-line (cmd) ((:rest "Eval: "))
  "Evaluate the s-expression and display the result(s)."
  (handler-case
      (if cmd
          (message "~{~a~^~%~}"
                   (mapcar 'prin1-to-string
                           (multiple-value-list (eval (read-from-string cmd)))))
          (throw 'error :abort))
    (error (c)
      (err "^B^1*~A" c))))

(setf (stumpwm::window-property (stumpwm::screen-message-window (current-screen)) :_STUMPWM_FLOATING) 1)
(setf (stumpwm::window-property (stumpwm::screen-message-window (current-screen)) :WM_CLASS) :STUMPWM_MESSAGE)

;; Future work, needs more thought put into
;; (defun float-window-by-hints (window)
;;   #+nil(let* ((xwin (window-xwin window))
;;          (id (xlib:get-property xwin :_NET_WM_WINDOW_TYPE))
;;          (type (when id (xlib:atom-name *display* (car id))))
;;          (normal-hints (xlib:wm-normal-hints xwin))
;;          (width (or (xlib:wm-size-hints-width normal-hints)
;;                     (window-width window)))
;;          (height (or (xlib:wm-size-hints-height normal-hints)
;;                      (window-height window)))
;;          (x (xlib:wm-size-hints-x normal-hints))
;;          (y (xlib:wm-size-hints-y normal-hints))
;;          (gravity (gravity-for-window window))
;;          (screen-width (screen-width (current-screen)))
;;          (screen-height (screen-height (current-screen))))
;;     (multiple-value-bind (gx gy) (gravity-coords gravity width height 0 0 screen-width screen-height)
;;       (cond ((some (lambda (x) (eq type x)) '(:_NET_WM_WINDOW_TYPE_DIALOG
;;                                          :_NET_WM_WINDOW_TYPE_TOOLBAR
;;                                          :_NET_WM_WINDOW_TYPE_UTILITY
;;                                          :_NET_WM_WINDOW_TYPE_SPLASH))
;;              (set-floating window t)
;;              (float-move-resize window :x (or x gx) :y (or y gy)))
;;             ((or (xlib::wm-size-hints-user-specified-size-p normal-hints)
;;                  (xlib::wm-size-hints-program-specified-size-p normal-hints))
;;              (when (and x y)
;;                (set-floating window t))
;;              (float-move-resize window :width width :height height :x (or x gx) :y (or y gy)))))))

;; (in-package :stumpwm)

;; (define-stump-event-handler :map-request (parent send-event-p window)
;;   (unless send-event-p
;;     ;; This assumes parent is a root window and it should be.
;;     (dformat 3 "map request: ~a ~a ~a~%" window parent (find-window window))
;;     (let ((screen (find-screen parent))
;;           (win (find-window window))
;;           (wwin (find-withdrawn-window window)))
;;       ;; only absorb it if it's not already managed (it could be iconic)
;;       (cond
;;         (win (dformat 1 "map request for mapped window ~a~%" win))
;;         ((eq (xwin-type window) :dock)
;;          (when wwin
;;            (setf screen (window-screen wwin)))
;;          (dformat 1 "window is dock-type. attempting to place in mode-line.")
;;          (place-mode-line-window screen window)
;;          ;; Some panels are broken and only set the dock type after they map and withdraw.
;;          (when wwin
;;            (setf (screen-withdrawn-windows screen) (delete wwin (screen-withdrawn-windows screen))))
;;          t)
;;         (wwin (restore-window wwin))
;;         ((xlib:get-property window :_KDE_NET_WM_SYSTEM_TRAY_WINDOW_FOR)
;;          ;; Do nothing if this is a systray window (the system tray
;;          ;; will handle it, if there is one, and, if there isn't the
;;          ;; user doesn't want this popping up as a managed window
;;          ;; anyway.
;;          t)
;;         (t
;;          (xlib:with-server-grabbed (*display*)
;;            (let ((window (process-mapped-window screen window)))
;;              (group-raise-request (window-group window) window :map)
;;              (handler-case (windows::float-window-by-hints window)
;;                (error (err) (err "~a" err))))))))))
