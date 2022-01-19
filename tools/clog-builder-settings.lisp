(in-package :clog-tools)

(defparameter supported-controls
  (list
   '(:name          "select"
     :description   "Selection Tool"
     :create         nil
     :create-type    nil
     :properties     nil
     :positioning    nil
     :events         nil)
   '(:name           "label"
     :description    "Text Label"
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
     :clog-type       clog:clog-form-element
     :create          clog:create-form-element
     :create-type     :form
     :create-param    :button
     :create-value    "button"
     :properties      ((:name "value"
			:prop clog:value)
		       (:name "positioning"
			:prop clog:positioning)
		       (:name "color"
			:prop clog:color)
		       (:name "background-color"
			:prop clog:background-color)))
   '(:name            "input"
     :description     "Text Input"
     :clog-type       clog:clog-form-element
     :create          clog:create-form-element
     :create-type     :form
     :create-param    :input
     :create-value    ""
     :properties      ((:name "value"
			:prop clog:value)
		       (:name "positioning"
			:prop clog:positioning)
		       (:name "color"
			:prop clog:color)
		       (:name "background-color"
			:prop clog:background-color)))
   '(:name            "div"
     :description     "DIV Control"
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

