(defpackage input
  (:use :cl :stumpwm :misc)
  (:import-from :stumpwm
                #:input-backward-kill-word
                #:input-forward-char
                #:input-backward-char
                #:input-line-string
                #:input-delete-forward-char
                #:input-delete-backward-char))

(in-package :input)

(setf (stumpwm::window-property (stumpwm::screen-input-window (current-screen)) :_STUMPWM_FLOATING) 1)
(setf (stumpwm::window-property (stumpwm::screen-input-window (current-screen)) :WM_CLASS) :STUMPWM_MESSAGE)

(defmacro with-read-case ((readtable read-case) &body body)
  (let ((old-case (gensym "OLD-CASE")))
    `(let ((,old-case (readtable-case ,readtable)))
       (setf (readtable-case ,readtable) ,read-case)
       (prog1 (progn ,@body)
         (setf (readtable-case ,readtable) ,old-case)))))

(defun input-forward-sexp (input key)
  (let* ((*read-eval* nil)
         (point (input-point input))
         (string (input-line-string input))
         (string (subseq string point)))
    (let ((length
            (with-read-case (*readtable* :preserve)
              (handler-case
                  (nth-value 1 (read-from-string string t nil :preserve-whitespace t))
                (error () 0)))))
      (dotimes (i length)
        (input-forward-char input key)))))

(defun input-forward-kill-sexp (input key)
  (let* ((*read-eval* nil)
         (point (input-point input))
         (string (input-line-string input))
         (string (subseq string point)))
    (let ((length
            (with-read-case (*readtable* :preserve)
              (handler-case
                  (nth-value 1 (read-from-string string t nil :preserve-whitespace t))
                (error () 0)))))
      (dotimes (i length)
        (input-delete-forward-char input key)))))

(define-keys *input-map*
  ("C-w" . 'input-backward-kill-word)
  ("C-M-k" . 'input-forward-kill-sexp)
  ("C-M-f" . 'input-forward-sexp))
