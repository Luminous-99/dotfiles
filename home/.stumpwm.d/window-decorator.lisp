(defpackage window-decorator
  (:use :cl :stumpwm :clx-truetype)
  (:import-from :stumpwm
                #:screen-font
                #:*float-window-title-height*
                #:xwin-grab-buttons
                #:default-border-width-for-type
                #:screen-unfocus-color
                #:screen-win-bg-color)
  (:shadow #:font-size)
  (:export
   #:*title-alignment*
   #:*title-padding-x*
   #:*title-padding-y*
   #:*title-source*
   #:start-decorator-thread
   #:free-decorator))

(in-package :window-decorator)

(declaim (type (member :left :center :right) *title-alignment*)
         (type (or function symbol) *title-source*)
         (type fixnum *title-padding-x* *title-padding-y*))
(defparameter *title-alignment* :center)
(defparameter *title-padding-x* 0)
(defparameter *title-padding-y* 0)
(defparameter *title-source* #'window-title)

(defun font-size (font)
  (typecase font
    (xlib:font (stumpwm::font-height font))
    (xft:font (xft:font-size font))))

(defun line-width (drawable font string)
  (typecase font
    (xlib:font (stumpwm::text-line-width font string))
    (xft:font (xft::text-line-width drawable font string))))

(declaim (ftype (function (window (or vector string)) (values fixnum fixnum))
                title-position))
(defun title-position (window title)
  (let* ((font (screen-font (current-screen)))
         (width (line-width (window-xwin window) font title)) 
         (title-height (font-size font)) 
         (window-title-height *float-window-title-height*)
         (y (floor (+ (/ window-title-height 2.0) (/ title-height 2.0) *title-padding-y*))))
    (case *title-alignment*
      (:left
       (values *title-padding-x* y))
      (:center
       (let ((x (floor (- (/ (window-width window) 2) (/ width 2.0)))))
         (values (+ x *title-padding-x*) y)))
      (:right
       (values (+ (- (window-width window) width) *title-padding-x*) y))
      (t
       (values *title-padding-y* y)))))

(defun floating-window-background (window)
  (if (eq (current-window) window)
      (screen-float-focus-color (window-screen window))
      (screen-float-unfocus-color (window-screen window))))

(defun floating-window-foreground  (window)
  (if (eq (current-window) window)
      (screen-float-unfocus-color (window-screen window))
      (screen-float-focus-color (window-screen window))))

(defun draw-title (window)
  (declare (type stumpwm::float-window window))
  (let ((previous-title "")
        (gc nil)
        (px nil)
        (old-background 0))
    (declare (optimize (speed 3) (safety 1)))
    (lambda (&optional free)
      (if (eq free :free)
          (progn
            (when px (xlib:free-pixmap px))
            (when gc (xlib:free-gcontext gc)))
          (let* ((title (funcall *title-source* window)) 
                 (parent (window-parent window))
                 (width (xlib:drawable-width parent))
                 (height *float-window-title-height*)
                 (depth (xlib:drawable-depth parent))
                 (font (screen-font (current-screen)))
                 (background (floating-window-background window))
                 (foreground (floating-window-foreground window)))
            (declare (type (simple-array character (*)) title previous-title)
                     (type fixnum width height background foreground))
            (setf (xlib:window-background parent) background)
            (xlib:clear-area parent)
            (unless px
              (setf px (xlib:create-pixmap :drawable parent :width width 
                                           :height height :depth depth))) 
            (unless gc (setf gc (xlib:create-gcontext :drawable px)))
            (cond
              ((not (string= previous-title title))
               (setf (xlib:drawable-width px) width
                     (xlib:drawable-height px) height)
               (setf old-background background)
               (xlib:with-gcontext (gc :foreground background :background foreground)
                 (xlib:draw-rectangle px gc 0 0 width height t)) 
               (xlib:with-gcontext (gc :foreground foreground :background background)
                 (multiple-value-bind (x y) (title-position window title)
                   (stumpwm::draw-image-glyphs px gc font x y title)))
               (xlib:copy-area px gc 0 0 width height parent 0 0)
               (setf previous-title title))
              (t
               (unless (= old-background background)
                 (setf old-background background)
                 (xlib:with-gcontext (gc :foreground background :background foreground)
                   (xlib:draw-rectangle px gc 0 0 width height t)) 
                 (xlib:with-gcontext (gc :foreground foreground :background background)
                   (multiple-value-bind (x y) (title-position window title)
                     (stumpwm::draw-image-glyphs px gc font x y title))))
               (xlib:copy-area px gc 0 0 width height parent 0 0))))))))

(let* ((locks (make-hash-table))
       (locks-lock (bt:make-recursive-lock "group-locks")))
  (dolist (group (screen-groups (current-screen)))
    (setf (gethash group locks) (bt:make-lock (group-name group))))
  (defun group-lock (group)
    (sb-thread:with-mutex (locks-lock)
	  (or (gethash group locks)
		  (setf (gethash group locks) (bt:make-lock (group-name group))))))

  (defmethod stumpwm::update-decoration ((window stumpwm::float-window))
    (sb-thread:with-mutex ((group-lock (window-group window)))
      (let ((decorator (gethash :decorator (window-plist window))))
        (unless (eq decorator :freed)
          (if decorator
              (funcall decorator)
              (funcall (setf (gethash :decorator (window-plist window)) (draw-title window)))))))))

(defun free-decorator (window)
  (symbol-macrolet ((decorator (gethash :decorator (window-plist window))))
    (when decorator
      (funcall decorator :free)
      (setf decorator :freed))))

(defun update-group-decorations (&optional (group (current-group)))
  (dolist (window (group-windows group))
    (when (stumpwm::float-window-p window)
	  (update-decoration window))))

(defmethod stumpwm::group-button-press :after (group button x y (window stumpwm::float-window))
  (update-decoration window))

(defun stumpwm::reparent-window (screen window)
  ;; apparently we need to grab the server so the client doesn't get
  ;; the mapnotify event before the reparent event. that's what fvwm
  ;; says.
  (let* ((xwin (window-xwin window))
	     (screen-root (screen-root (current-screen)))
	     (master-window (xlib:create-window
			             :parent screen-root
			             :x (xlib:drawable-x (window-xwin window))
			             :y (xlib:drawable-y (window-xwin window))
			             :width (window-width window)
			             :height (window-height window)
			             :background (if (eq (window-type window) :normal)
					                     (screen-win-bg-color screen)
					                     :none)
			             :border (screen-unfocus-color screen)
			             :border-width (default-border-width-for-type window)
			             :event-mask *window-parent-events*
			             :depth 24 
			             :visual (xlib:window-visual-info screen-root) 
			             :colormap (xlib:window-colormap screen-root))))
    (unless (eq (xlib:window-map-state (window-xwin window)) :unmapped)
      (incf (window-unmap-ignores window)))
    (xlib:reparent-window (window-xwin window) master-window 0 0)
    (xwin-grab-buttons xwin)
    (xlib:add-to-save-set (window-xwin window))
    (setf (window-parent window) master-window)))

(defun start-decorator (&optional (delay 0.5))
  (bt:make-thread (lambda ()
		            (loop
		              (sleep delay)
		              (update-group-decorations)))
		          :name "decorator"))

(defun kill-decorator ()
  (bt:destroy-thread (find "decorator" (bt:all-threads) :key #'bt:thread-name :test #'string=)))

(start-decorator)
