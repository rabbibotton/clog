;; Demonstrate CLOG presentations
;;   links established between CLOG objects and Lisp objects

(defpackage #:clog-tut-29
  (:use #:cl #:clog)
  (:export start-tutorial))

(in-package :clog-tut-29)

(defclass my-class ()
  ((my-slot :accessor my-slot :initform "")
   (my-count :accessor my-count :initform 500)))

(defun on-new-window (body)
  (let* ((lisp-obj (make-instance 'my-class))
	 (i1 (create-form-element body :text
				  :label (create-label body :content "Form value:")))
	 (i2 (create-form-element body :text
				  :label (create-label body :content "(my-slot lisp-obj) value:")))
	 (b1 (create-button body :content "Set (my-slot lisp-obj) Value"))
	 (b2 (create-button body :content "Get (my-slot lisp-obj) Value"))
	 (tmp (create-br body))
	 (t1 (create-div body :content "[counter]"))
	 (i3 (create-form-element body :text
				  :label (create-label body :content "Change my-count:"))))
    ;; We set up direct relationships between lisp obj and clog objects
    (link-form-element-to-slot i1 lisp-obj my-slot)  ;; any change to i1 will change my-slot
    (link-slot-to-form-element lisp-obj my-slot i1)  ;; any change to my-slot will change i1
    (link-slot-to-element lisp-obj my-count t1)      ;; any change to my-count will change h1
    (link-form-element-to-object
     i3 (my-count lisp-obj) :transform #'parse-integer) ;; any change to i3 will change my-count
    ;; This change of my-slot will immediately change in the web page
    (setf (my-slot lisp-obj) "First Value")
    (set-on-click b1
		  (lambda (obj)
		    (declare (ignore obj))
		    (setf (my-slot lisp-obj) (value i2))))
    (set-on-click b2
		  (lambda (obj)
		    (declare (ignore obj))
		    (setf (value i2) (my-slot lisp-obj))))
    ;; This updates an element on the page by just changing the value of the linked
    ;; slot
    (loop
       (cond ((> (my-count lisp-obj) 0)
	      (decf (my-count lisp-obj))
	      (sleep .2))
	     (t
	      (return))))))

(defun start-tutorial ()
  (initialize 'on-new-window)
  (open-browser))
