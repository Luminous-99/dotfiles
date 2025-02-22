(defpackage contrib
  (:use :cl :stumpwm)
  (:export
   #:toggle-gaps))

(in-package :contrib)

(swm-gaps:toggle-gaps-off)
(setf cpu:*cpu-modeline-fmt* "%c %t"
      cpu::*cpu-usage-modeline-fmt*  " ^[~A~3D%^]"
      mem::*mem-modeline-fmt* "  %a %p"
      swm-gaps:*outer-gaps-size* 3
      swm-gaps:*inner-gaps-size* 3)

(defun toggle-gaps ()
  (if (null (cdr (stumpwm::group-frames (current-group))))
      (swm-gaps:toggle-gaps-off)
      (swm-gaps:toggle-gaps-on)))
