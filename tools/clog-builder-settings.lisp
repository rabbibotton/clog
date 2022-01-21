(in-package :clog-tools)

(defparameter *import-types*
  (list '(:tag "label"
	  :control "label")
	'(:tag "input"
	  :control "input")
	'(:tag "form"
	  :control "form")
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
	       (let ((dd (create-select td2))
		     (v  (string-downcase (positioning control))))
		 (add-select-options dd `(,v
					  "absolute"
					  "static"))
		 (set-on-change dd (lambda (obj)
				     (declare (ignore obj))
				     (setf (positioning control) (value dd))
				     (set-geometry (get-placer control)
						   :top (position-top control)
						   :left (position-left control)
						   :width (client-width control)
						   :height (client-height control))
				     (on-populate-control-properties-win obj)))
		 nil)))))
(defparameter *props-with-height*
  '((:name "width"
     :setf clog:width)
    (:name "height"
     :setf clog:height)))

(defparameter *props-form-values*
  `((:name "value"
     :setf clog:value)
    (:name "name on form"
     :setf clog:name)
    (:name "size"
     :prop "size")
    (:name "place holder"
     :prop "placeholder")
    (:name "default value"
     :prop "defaultValue")
    (:name "image url"
     :prop "src")
    (:name "image alt"
     :prop "alt")
    (:name "checked"
     :get  ,(lambda (control)
	      (property control "checked"))
     :set  ,(lambda (control obj)
	     (if (equalp (text obj) "true")
		 (setf (checkedp control) t)
		 (setf (checkedp control) nil))
	      (property control "required")))
    (:name "read only"
     :get  ,(lambda (control)
	      (property control "readonly"))
     :set  ,(lambda (control obj)
	     (if (equalp (text obj) "true")
		 (setf (read-only-p control) t)
		 (setf (read-only-p control) nil))
	     (property control "readonly")))
    (:name "disabled"
     :get  ,(lambda (control)
	      (property control "disabled"))
     :set  ,(lambda (control obj)
	     (if (equalp (text obj) "true")
		 (setf (disabledp control) t)
		 (setf (disabledp control) nil))
	     (property control "disabled")))
    (:name "required"
     :get  ,(lambda (control)
	      (property control "required"))
     :set  ,(lambda (control obj)
	     (if (equalp (text obj) "true")
		 (setf (requiredp control) t)
		 (setf (requiredp control) nil))
	      (property control "required")))
    (:name "pattern"
     :prop "pattern")
    (:name "minimum"
     :prop "min")
    (:name "maximum"
     :prop "max")
    (:name "element step"
     :prop "step")
    (:name "minimum length"
     :prop "minlength")
    (:name "maximum length"
     :prop "maxlength")
    (:name "multiple"
     :get  ,(lambda (control)
	      (property control "multiple"))
     :set  ,(lambda (control obj)
	      (if (equalp (text obj) "true")
		 (setf (multiplep control) t)
		 (setf (multiplep control) nil))
	      (property control "multiple")))
    (:name "files accepted"
     :prop "accept")))

(defparameter *props-colors*
  `((:name "color"
     :setup ,(lambda (control td1 td2)
	       (declare (ignore td1))
	       (let ((d1 (create-form-element td2 :text  :value (color control)))
		     (dd (create-form-element td2 :color :value (color control))))
		 (make-data-list dd '("#ffffff"
				      "#ff0000"
				      "#00ff00"
				      "#0000ff"
				      "#ff00ff"))
		 (set-on-change dd (lambda (obj)
				     (declare (ignore obj))
				     (setf (value d1) (value dd))
				     (setf (color control) (value d1))))
		 (set-on-change d1 (lambda (obj)
				     (declare (ignore obj))
				     (setf (color control) (value d1)))))
	       nil))
    (:name "background color"
     :setup ,(lambda (control td1 td2)
	       (declare (ignore td1))
	       (let ((d1 (create-form-element td2 :text  :value (background-color control)))
		     (dd (create-form-element td2 :color :value (background-color control))))
		 (make-data-list dd '("#ffffff"
				      "#ff0000"
				      "#00ff00"
				      "#0000ff"
				      "#ff00ff"))
		 (set-on-change dd (lambda (obj)
				     (declare (ignore obj))
				     (setf (value d1) (value dd))
				     (setf (background-color control) (value d1))))
		 (set-on-change d1 (lambda (obj)
				     (declare (ignore obj))
				     (setf (background-color control) (value d1))))
		 nil)))))

(defparameter *props-text*
  `((:name "contents"
     :setup ,(lambda (control td1 td2)
	       (declare (ignore td1))
	       (let ((d1 (create-form-element td2 :text :value (inner-html control))))
		 (set-on-change d1 (lambda (obj)
				     (declare (ignore obj))
				     (setf (inner-html control) (value d1)))))
	       nil))))


(defparameter *props-css*
  `((:name "css classes"
     :prop "className")
    ))

(defparameter *props-base*
  `((:name "hidden"
     :get  ,(lambda (control)
	      (property control "hidden"))
     :set  ,(lambda (control obj)
	      (if (equalp (text obj) "true")
		 (setf (hiddenp control) t)
		 (setf (hiddenp control) nil))
	      (property control "hidden")))
    (:name "visible"
     :get  ,(lambda (control)
	      (style control "visibility"))
     :set  ,(lambda (control obj)
	      (if (or (equalp (text obj) "true")
		      (equalp (text obj) "visible"))
		 (setf (visiblep control) t)
		 (setf (visiblep control) nil))
	      (style control "visibility")))
    (:name "editable"
     :prop "contentEditable")
    ))

(defparameter *props-nav*
  '((:name "access key"
     :prop "access key")
    (:name "tool tip"
     :prop "title")
    ))

(defparameter *props-element*
  `(,@*props-location*
    ,@*props-with-height*
    ,@*props-text*
    ,@*props-css*
    ,@*props-colors*
    ,@*props-base*
    ,@*props-nav*))

(defparameter *props-form-element*
  `(,@*props-location*
    ,@*props-with-height*
    (:name "type"
     :setup ,(lambda (control td1 td2)
	       (declare (ignore td1))
	       (let ((dd (create-select td2))
		     (v  (string-downcase (attribute control "type"))))
		 (add-select-options dd `(,v
					  "button" "checkbox" "color" "date"
					  "datetime" "datetime-local" "email"
					  "image" "file" "hidden" "image"
					  "month" "number" "password" "radio"
					  "range" "reset" "search" "submit"
					  "tel" "text" "time" "url" "week"))
		 (set-on-change dd (lambda (obj)
				   (declare (ignore obj))
				   (setf (attribute control "type") (value dd))
				   (set-geometry (get-placer control)
						 :top (position-top control)
						 :left (position-left control)
						 :width (client-width control)
						 :height (client-height control))))
		 nil)))
    ,@*props-form-values*
    ,@*props-css*
    ,@*props-colors*
    ,@*props-base*
    ,@*props-nav*))

(defparameter *supported-controls*
  (list
   '(:name           "select"
     :description    "Selection Tool"
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
   `(:name           "button"
     :description    "Button"
     :clog-type      clog:clog-button
     :create         clog:create-button
     :create-type    :element
     :create-content "button"
     :properties     (,@*props-element*))
   `(:name           "form"
     :description    "Form"
     :clog-type      clog:clog-form
     :create         clog:create-form
     :create-type    :base
     :properties     ((:name "method"
		       :attr "method")
		      (:name "encoding"
		       :prop "encoding")
		      (:name "from element count"
		       :get ,(lambda (control) (form-element-count control)))))
   `(:name           "input"
     :description    "Form Input"
     :clog-type      clog:clog-form-element
     :create         clog:create-form-element
     :create-type    :form
     :create-param   :text
     :create-value   ""
     :properties     (,@*props-form-element*))
   `(:name           "span"
     :description    "Span Control"
     :clog-type      clog:clog-span
     :create         clog:create-span
     :create-type    :element
     :create-content "text here"
     :properties     (,@*props-element*))
   `(:name           "div"
     :description    "Div Control"
     :clog-type      clog:clog-div
     :create         clog:create-div
     :create-type    :element
     :create-content ""
     :properties     (,@*props-element*))))
