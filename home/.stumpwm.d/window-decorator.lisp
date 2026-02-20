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

(defclass decorator (sb-mop:funcallable-standard-object)
  ((window :initform nil :initarg :window :accessor decorator-window)
   (pixmap :initform nil :initarg :pixmap :accessor decorator-pixmap)
   (gcontext :initform nil :initarg :gcontext :accessor decorator-gcontext)
   (background :initform 0 :initarg :background :accessor decorator-background)
   (title :initform "" :initarg :title :accessor decorator-title))
  (:metaclass sb-mop:funcallable-standard-class))

(defmethod initialize-instance :after ((decorator decorator) &key &allow-other-keys)
  (sb-mop:set-funcallable-instance-function
   decorator
   (with-slots (window pixmap gcontext background title) decorator
     (declare (optimize (speed 3) (safety 1)))
     (lambda (&optional free)
       (if (eq free :free)
           (progn
             (when pixmap (xlib:free-pixmap pixmap))
             (when gcontext (xlib:free-gcontext gcontext)))
           (let* ((current-title (funcall *title-source* window))
                  (parent (window-parent window))
                  (width (xlib:drawable-width parent))
                  (height *float-window-title-height*)
                  (depth (xlib:drawable-depth parent))
                  (font (screen-font (current-screen)))
                  (current-background (floating-window-background window))
                  (foreground (floating-window-foreground window)))
             (declare (type (simple-array character (*)) current-title title)
                      (type fixnum width height current-background foreground background))
             (setf (xlib:window-background parent) current-background
                   pixmap (or pixmap (xlib:create-pixmap :drawable parent :width width
                                                         :height height :depth depth))
                   gcontext (or gcontext (xlib:create-gcontext :drawable pixmap)))
             (xlib:clear-area parent)
             (cond
               ((not (string= title current-title))
                (setf (xlib:drawable-width pixmap) width
                      (xlib:drawable-height pixmap) height
                      background current-background
                      title current-title)
                (xlib:with-gcontext (gcontext :foreground current-background :background foreground)
                  (xlib:draw-rectangle pixmap gcontext 0 0 width height t))
                (xlib:with-gcontext (gcontext :foreground foreground :background current-background)
                  (multiple-value-bind (x y) (title-position window current-title)
                    (stumpwm::draw-image-glyphs pixmap gcontext font x y current-title)))
                (xlib:copy-area pixmap gcontext 0 0 width height parent 0 0))
               (t
                (unless (= background current-background)
                  (setf background current-background)
                  (xlib:with-gcontext (gcontext :foreground current-background :background foreground)
                    (xlib:draw-rectangle pixmap gcontext 0 0 width height t))
                  (xlib:with-gcontext (gcontext :foreground foreground :background current-background)
                    (multiple-value-bind (x y) (title-position window current-title)
                      (stumpwm::draw-image-glyphs pixmap gcontext font x y current-title))))
                (xlib:copy-area pixmap gcontext 0 0 width height parent 0 0)))))))))

(defmethod initialize-instance :after ((window window) &key &allow-other-keys)
  (setf (gethash :decorator (window-plist window)) (make-instance 'decorator :window window)
        (gethash :lock (window-plist window)) (bt:make-lock)))

(defmethod stumpwm::update-decoration ((window stumpwm::float-window))
  (bt:with-lock-held ((gethash :lock (window-plist window)))
    (let ((decorator (gethash :decorator (window-plist window))))
      (unless (eq decorator :freed)
        (funcall decorator)))))

(defun free-decorator (window)
  (bt:with-lock-held ((gethash :lock (window-plist window)))
    (unless (gethash :number (window-plist window))
      (symbol-macrolet ((decorator (gethash :decorator (window-plist window))))
        (unless (eq decorator :free)
          (funcall decorator :free)
          (setf decorator :freed))))))

(add-hook *destroy-window-hook* 'free-decorator)

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
                         :depth (xlib:drawable-depth screen-root)
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
                  :name "Window Decorator"))

(defun kill-decorator ()
  (bt:destroy-thread (find "Window Decorator" (bt:all-threads) :key #'bt:thread-name :test #'string=)))

(start-decorator)
