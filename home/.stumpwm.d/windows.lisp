(defpackage windows
  (:use :cl :stumpwm :misc :contrib :window-decorator)
  (:import-from :alexandria
                #:when-let*
                #:when-let)
  (:import-from :stumpwm
                #:find-group
                #:switch-to-group
                #:gravity-for-window
                #:if-let)
  (:export
   #:float-unless-maximized
   #:sort-current-group
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
   #:place-all-windows
   #:renumber-to-next
   #:renumber-to-previous
   #:split-vertical
   #:clear-messages
   #:*window-preferences*
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

(defun sort-current-group (&optional window)
  "Sort windows."
  (declare (ignorable window))
  (let* ((windows (group-windows (current-group)))
         (windows (sort (copy-list windows)
                        (lambda (x y)
                          (< (window-number x)
                             (window-number y))))))
    (loop for (win1 win2) on windows
          for num1 = (window-number win1)
          for num2 = (and win2 (window-number win2))
          unless num2
            return (values)
          when (< (1+ num1) num2)
            do (setf (window-number win2) (1+ num1))))
  (stumpwm::update-all-mode-lines))

(setf *destroy-window-hook* (list 'sort-current-group))

(defun float-unless-maximized (window)
  (unless (getf (stumpwm::group-plist (current-group)) :GAPS)
    (when window
      (when-let* ((screen (current-screen))
                  (mode-line (car (stumpwm::screen-mode-lines (current-screen))))
                  (maximized-width (screen-width screen))
                  (maximized-height (- (screen-height screen) (stumpwm::mode-line-height mode-line))))
        (let ((width (window-width window))
              (height (window-height window)))
          (when (or (< width maximized-width) (< height maximized-height))
            (set-floating window t)
            (focus-window window t)))))))

(setf *new-window-hook* (list 'float-unless-maximized))

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
  (free-decorator window)
  (delete-window window)
  (toggle-gaps))

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
  (message (run-shell-command (format nil "xprop -id ~A" (stumpwm::window-id (current-window))) t)))

(define-keys *root-map*
  ("f" . "toggle-float")
  ("F" . "toggle-always-on-top")
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

(defvar *window-preferences* ()
  "A list of plists that define window preferences.")

(defun define-window-preference (&key class window-number group-name group-number)
  (let ((preference (list :class class
                          :window-number window-number
                          :group-name group-name
                          :group-number group-number)))
    (let ((old-preference  (find class *window-preferences*
                                 :test (lambda (x y)
                                         (string= x (getf y :class))))))
      (if old-preference
          (setf (cdr old-preference) (cdr preference))
          (push preference *window-preferences*)))))

(defmacro define-window-preferences (&body preferences)
  (let ((preference (gensym "PREFERENCE")))
    `(dolist (,preference ',preferences)
       (apply #'define-window-preference ,preference))))

(defun preference-matches-p (window preference)
  (when (string= (window-class window) (getf preference :class))
    preference))

(defun place-windows (&optional (screen (current-screen)))
  "Place windows according to *WINDOW-PREFERENCES*."
  (dolist (window (screen-windows screen))
    (when-let ((preference (find window *window-preferences*
                                 :test #'preference-matches-p)))
      (destructuring-bind (&key class window-number group-name group-number)
          preference
        (declare (ignorable class))
        (when window-number
          (let ((old-window (find window-number (group-windows (window-group window))
                                  :key #'window-number :test #'=)))
            (if old-window
                (setf (window-number old-window) (window-number window)
                      (window-number window) window-number)
                (setf (window-number window) window-number))))
        (cond
          ((and group-name (not (string= group-name (group-name (window-group window)))))
           (move-window-to-group window (find-group screen group-name)))
          ((and group-number (not (= group-number (group-number (window-group window)))))
           (let ((group (find group-number (screen-groups screen)
                              :test #'= :key #'group-number)))
             (move-window-to-group window group))))))))

(defcommand place-all-windows (&optional (screen (current-screen))) ()
  (place-windows screen))

(define-window-preferences
  (:class "Emacs" :window-number 0 :group-name "Group 1")
  (:class "firefox" :window-number 1 :group-name "Group 1")
  (:class "discord" :window-number 0 :group-name "Group 2")
  (:class "thunderbird" :window-number 0 :group-name "Group 3")
  (:class "Spotify" :window-number 1 :group-name "Group 2"))

(define-key *root-map* (kbd "W") "place-all-windows")

(defcommand withdraw-window (&optional (window (current-window))) ()
  (setf (gethash :number (window-plist window)) (window-number window))
  (stumpwm::withdraw-window window))

(defcommand withdraw-all-windows (&optional (group (current-group))) ()
  (dolist (window (group-windows group))
    (handler-case (stumpwm::withdraw-window window)
      (error (err) (err "~a" err)))))

(flet ((window-from-menu ()
         (let ((windows (stumpwm::screen-withdrawn-windows (current-screen))))
           (and windows (stumpwm::select-window-from-menu windows *window-format*)))))
  (defcommand restore-window (&optional (window (window-from-menu))) ()
    (when window
      (let* ((windows (group-windows (current-group)))
             (windows (stumpwm::sort-windows-by-number windows))
             (restored-number (gethash :number (window-plist window)))
             (obstruction (find (or restored-number (window-number window)) windows :key #'window-number :test #'=)))
        (if obstruction
            (setf (window-number obstruction) (window-number window)
                  (window-number window) restored-number)
            (setf (window-number window) restored-number))
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

(define-keys *root-map*
  ("o" . '*scratchpad-map*))

(dotimes (i 9)
  (define-key *root-map* (kbd (format nil "M-~a" (1+ i)))
    (format nil "gmove ~a" (1+ i))))

(dotimes (i 7)
  (gnew (format nil "Group ~a" (+ 2 i))))
(gnew-dynamic "Group 9")

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
