(defpackage symbol-hooks
  (:use :cl)
  (:shadow #:setf)
  (:nicknames #:symhook)
  (:export
   #:hooked-symbol-p
   #:setf
   #:define-symbol-hook))

(in-package :symbol-hooks)

(defun hooked-symbol-p (symbol) 
  (and (symbolp symbol) (get symbol :symbol-hook)))

(defmacro setf (&rest args) 
  `(progn 
	 ,@(loop for place in args by #'cddr 
		     for value in (cdr args) by #'cddr
		     if (hooked-symbol-p place) 
		       collect `(prog1
                            (cl:setf ,place ,value)
                          (funcall (get ',place :symbol-hook) ,value))
		     else collect `(cl:setf ,place ,value))))

(defmacro define-symbol-hook (symbol &body body)
  "Define a function in the SYMBOL-PLIST of SYMBOL called with NEW-VALUE from a SETF form."
  `(eval-when (:load-toplevel :execute)
     (let ((function  (lambda (,(intern "NEW-VALUE"))
             (declare (ignorable ,(intern "NEW-VALUE")))
             ,@body)))
       (setf (get ',symbol :symbol-hook) function))))

