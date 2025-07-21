(defpackage windows
  (:use :cl :stumpwm :misc :contrib)
  (:import-from :alexandria
                #:when-let*
                #:when-let)
  (:import-from :stumpwm
                #:find-group
                #:switch-to-group
                #:if-let)
  (:export
   #:float-unless-maximized
   #:sort-current-group
   #:set-floating
   #:list-all-windows
   #:float-move-resize
   #:*resizing-mode*
   #:*resize-increment*
   #:float-resize
   #:float-move
   #:move
   #:iresize-float
   #:close-window-and-frame
   #:terminate-this
   #:toggle-float
   #:delete-split
   #:split-horizontal
   #:place-all-windows
   #:renumber-to-next
   #:renumber-to-previous
   #:split-vertical
   #:clear-messages
   #:*window-preferences*))

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

(defcommand close-window-and-frame () ()
  (unless (only-one-frame-p)
    (delete-window)
    (remove-split))
  (toggle-gaps))

(defcommand terminate-this () ()
  (let ((window (current-window)))
    (delete-window window)
    (toggle-gaps)))

(defcommand toggle-float (&optional (window (current-window))) ()
  (if (stumpwm::float-window-p window)
      (set-floating window nil)
      (set-floating window t)))

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

(defcommand renumber-to-next () ()
  (renumber (1+ (window-number (current-window)))))

(defcommand renumber-to-previous () ()
  (let ((number (window-number (current-window))))
    (renumber (if (zerop number) number (1- number)))))

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
  ("C-n" . "renumber-to-next")
  ("C-p" . "renumber-to-previous")
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
  (:class "vesktop" :window-number 0 :group-name "Group 2")
  (:class "Spotify" :window-number 1 :group-name "Group 2"))

(define-key *root-map* (kbd "W") "place-all-windows")

(defcommand withdraw-window (&optional (window (current-window))) ()
  (setf (gethash :number (window-plist window)) (window-number window))
  (stumpwm::withdraw-window window))

(defcommand withdraw-all-windows (&optional (group (current-group))) ()
  (dolist (window (group-windows group))
    (stumpwm::withdraw-window window)))

(flet ((window-from-menu ()
         (let ((windows (stumpwm::screen-withdrawn-windows (current-screen))))
           (and windows (stumpwm::select-window-from-menu windows *window-format*)))))
  (defcommand restore-window (&optional (window (window-from-menu))) ()
    (when window
      (let* ((windows (group-windows (current-group)))
             (windows (stumpwm::sort-windows-by-number windows))
             (restored-number (gethash :number (window-plist window)))
             (obstruction (find restored-number  windows :key #'window-number :test #'=)))
        (stumpwm::restore-window window)
        (if obstruction
            (setf (window-number obstruction) (window-number window)
                  (window-number window) restored-number)
            (setf (window-number window) restored-number))))))

(defcommand restore-newest-window () ()
  (restore-window (car (stumpwm::screen-withdrawn-windows (current-screen)))))

(defcommand restore-oldest-window () ()
  (restore-window (car (last (stumpwm::screen-withdrawn-windows (current-screen))))))

(defcommand restore-all-windows () ()
  (dolist (window (stumpwm::screen-withdrawn-windows (current-screen)))
    (restore-window window)))

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
(setf (stumpwm::window-property (stumpwm::screen-input-window (current-screen)) :_STUMPWM_FLOATING) 1)
(setf (stumpwm::window-property (stumpwm::screen-input-window (current-screen)) :WM_CLASS) :STUMPWM_MESSAGE)
