;; Demonstrate CLOG presentations
;;   links established between CLOG objects and Lisp objects

(defpackage #:clog-tut-29
  (:use #:cl #:clog)
  (:export start-tutorial))

(in-package :clog-tut-29)

(defclass my-class ()
  ((my-slot :accessor my-slot :initform "")))

(defun on-new-window (body)
  (let* ((lisp-obj (make-instance 'my-class))
	 (l1 (create-label body :content "Form value:"))
	 (i1 (create-form-element body :text))
	 (l2 (create-label body :content "(my-slot lisp-obj) value:"))
	 (i2 (create-form-element body :text))
	 (b1 (create-button body :content "Set (my-slot lisp-obj) Value"))
	 (b2 (create-button body :content "Get (my-slot lisp-obj) Value")))
    (link-form-element-to-slot i1 lisp-obj my-slot)
    (link-slot-to-form-element lisp-obj my-slot i1)
    (set-on-click b1
		  (lambda (obj)
		    (setf (my-slot lisp-obj) (value i2))))
    (set-on-click b2
		  (lambda (obj)
		    (setf (value i2) (my-slot lisp-obj))))))

(defun start-tutorial ()
  (initialize 'on-new-window)
  (open-browser))
