(in-package :clog-tools)

(defparameter *import-types*
  (list '(:tag "label"
	  :control "label")
	'(:tag "input"
	  :control "input")
	'(:tag "span"
	  :control "span")
	'(:tag "div"
	  :control "div")))

(defparameter *props-location*
  `((:name "top"
     :get ,(lambda (control)
	     (if (equal (positioning control) "static")
		 "n/a"
		 (top control)))
     :set ,(lambda (control obj)
	     (setf (top control) (text obj))))
    (:name "left"
     :get  ,(lambda (control)
	      (if (equal (positioning control) "static")
		  "n/a"
		  (left control)))
     :set  ,(lambda (control obj)
	      (setf (left control) (text obj))))
    (:name "positioning"
     :setup ,(lambda (control td1 td2)
	       (declare (ignore td1))
	       (let ((dd (create-form-element td2 :text :value (positioning control))))
		 (make-data-list dd '("absolute"
				      "static"))
		 (set-on-blur dd (lambda (obj)
				   (declare (ignore obj))
				   (setf (positioning control) (value dd))
				   (set-geometry (get-placer control)
						 :top (position-top control)
						 :left (position-left control)
						 :width (client-width control)
						 :height (client-height control)))))))))
(defparameter *props-wh*
  '((:name "width"
     :setf clog:width)
    (:name "height"
     :setf clog:height)))

(defparameter *props-text*
  '((:name "text"
     :setf clog:text)))

(defparameter *props-value*
  '((:name "value"
     :setf clog:value)))

(defparameter *props-colors*
  '((:name "color"
     :style "color")
    (:name "background-color"
     :style "background-color")))

(defparameter *props-element*
  `(,@*props-location*
    ,@*props-wh*
    ,@*props-text*
    ,@*props-colors*))

(defparameter *props-form-element*
  `(,@*props-location*
    ,@*props-wh*
    ,@*props-value*
    ,@*props-colors*))

(defparameter *supported-controls*
  (list
   '(:name          "select"
     :description   "Selection Tool"
     :create         nil
     :create-type    nil
     :properties     nil
     :positioning    nil)
   `(:name           "label"
     :description    "Label"
     :clog-type      clog:clog-label
     :create         clog:create-label
     :create-type    :element
     :create-content "label"
     :properties     (,@*props-element*))
   `(:name            "button"
     :description     "Button"
     :clog-type       clog:clog-button
     :create          clog:create-button
     :create-type     :element
     :create-param    :button
     :create-content  "button"
     :properties     (,@*props-element*))
   `(:name            "input"
     :description     "Form Text Input"
     :clog-type       clog:clog-form-element
     :create          clog:create-form-element
     :create-type     :form
     :create-param    :input
     :create-value    ""
     :properties     (,@*props-form-element*))
   `(:name            "span"
     :description     "Span Control"
     :clog-type       clog:clog-span
     :create          clog:create-span
     :create-type     :element
     :create-content  "text here"
     :properties     (,@*props-element*))
   `(:name            "div"
     :description     "Div Control"
     :clog-type       clog:clog-div
     :create          clog:create-div
     :create-type     :element
     :create-content  ""
     :properties     (,@*props-element*))))
