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

(defparameter *supported-controls*
  (list
   '(:name          "select"
     :description   "Selection Tool"
     :create         nil
     :create-type    nil
     :properties     nil
     :positioning    nil)
   '(:name           "label"
     :description    "Label"
     :clog-type      clog:clog-label
     :create         clog:create-label
     :create-type    :element
     :create-content "label"
     :properties     ((:name "text"
		       :prop clog:text)
		      (:name "positioning"
		       :prop clog:positioning)
		      (:name "color"
		       :prop clog:color)
		      (:name "background-color"
		       :prop clog:background-color)))
   '(:name            "button"
     :description     "Button"
     :clog-type       clog:clog-button
     :create          clog:create-button
     :create-type     :element
     :create-param    :button
     :create-content  "button"
     :properties      ((:name "text"
			:prop clog:text)
		       (:name "positioning"
			:prop clog:positioning)
		       (:name "color"
			:prop clog:color)
		       (:name "background-color"
			:prop clog:background-color)))
   '(:name            "input"
     :description     "Form Text Input"
     :clog-type       clog:clog-form-element
     :create          clog:create-form-element
     :create-type     :form
     :create-param    :input
     :create-value    ""
     :properties      ((:name "value"
			:prop clog:value)
		       (:name "name"
			:prop clog:name)
		       (:name "positioning"
			:prop clog:positioning)
		       (:name "color"
			:prop clog:color)
		       (:name "background-color"
			:prop clog:background-color)))
   '(:name            "span"
     :description     "Span Control"
     :clog-type       clog:clog-span
     :create          clog:create-span
     :create-type     :element
     :create-content  "text here"
     :properties      ((:name "text"
			:prop clog:text)
		       (:name "positioning"
			:prop clog:positioning)
		       (:name "color"
			:prop clog:color)
		       (:name "background-color"
			:prop clog:background-color)))
   '(:name            "div"
     :description     "Div Control"
     :clog-type       clog:clog-div
     :create          clog:create-div
     :create-type     :element
     :create-content  ""
     :properties      ((:name "text"
			:prop clog:text)
		       (:name "positioning"
			:prop clog:positioning)
		       (:name "color"
			:prop clog:color)
		       (:name "background-color"
			:prop clog:background-color)))))

