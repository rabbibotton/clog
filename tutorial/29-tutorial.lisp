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
				  :label (create-label body :content "Change my-count:")))
	 (tmp (create-br body))
	 (t2 (create-div body :content "'Hello'")))
    (declare (ignore tmp))
    ;; We set up direct relationships between lisp objects and clog objects
    ;; any change to i1 will change my-slot and any change to my-slot
    ;; will change i1 and transform it with #'string-upcase
    (link-slot-and-form-element lisp-obj my-slot i1
				:transform-to-element #'string-upcase)
    ;; any change to my-count will change t1
    (link-slot-to-element lisp-obj my-count t1)
    ;; any change to i3 will change my-count
    ;; and i3's value will be transformed to an integer
    (link-form-element-to-slot i3 lisp-obj my-count
			       :transform #'parse-integer)
    ;; Clicking on t2 will set my-slot to its text
    (link-element-to-slot t2 lisp-obj my-slot :set-event #'set-on-click)
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
    ;; slot and my-count can be adjusted mid loop from web page
    (loop
       (cond ((> (my-count lisp-obj) 0)
	      (decf (my-count lisp-obj))
	      (sleep .2))
	     (t
	      (return))))))

(defun start-tutorial ()
  (initialize 'on-new-window)
  (open-browser))
