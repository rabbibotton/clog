;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG Builder - UI Design tool for CLOG                                ;;;;
;;;; (c) 2020-2022 David Botton                                            ;;;;
;;;; License BSD 3 Clause                                                  ;;;;
;;;;                                                                       ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :clog-tools)

(defparameter *import-types*
  (list '(:tag "label"
	  :control "label")
	'(:tag "input"
	  :control "input")
	'(:tag "form"
	  :control "form")
	'(:tag "button"
	  :control "button")
	'(:tag "a"
	  :control "link")
	'(:tag "img"
	  :control "image")
	'(:tag "meter"
	  :control "meter")
	'(:tag "progress"
	  :control "progress")
	'(:tag "ol"
	  :control "ol")
	'(:tag "ul"
	  :control "ul")
	'(:tag "li"
	  :control "li")
	'(:tag "li"
	  :control "li")
	'(:tag "table"
	  :control "table")
	'(:tag "tr"
	  :control "tr")
	'(:tag "td"
	  :control "td")
	'(:tag "th"
	  :control "th")
	'(:tag "thead"
	  :control "thead")
	'(:tag "tbody"
	  :control "tbody")
	'(:tag "tfoot"
	  :control "tfoot")
	'(:tag "caption"
	  :control "tcaption")
	'(:tag "colgroup"
	  :control "tcolgroup")
	'(:tag "col"
	  :control "tcol")
	'(:tag "span"
	  :control "span")
	'(:tag "textarea"
	  :control "textarea")
	'(:tag "style"
	  :control "style-block")
	'(:tag "fieldset"
	  :control "fieldset")
	'(:tag "legend"
	  :control "legend")
	'(:tag "datalist"
	  :control "datalist")
	'(:tag "select"
	  :control "dropdown")
	'(:tag "option"
	  :control "option")
	'(:tag "optgroup"
	  :control "optgroup")
	'(:tag "dialog"
	  :control "dialog")
	'(:tag "p"
	  :control "p")
	'(:tag "br"
	  :control "br")
	'(:tag "hr"
	  :control "hr")
	'(:tag "dl"
	  :control "dl")
	'(:tag "dt"
	  :control "dt")
	'(:tag "dd"
	  :control "dd")
	'(:tag "details"
	  :control "details")
	'(:tag "summary"
	  :control "summary")
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
    (:name "bottom"
     :get ,(lambda (control)
	     (if (equal (positioning control) "static")
		 "n/a"
		 (bottom control)))
     :set ,(lambda (control obj)
	     (setf (bottom control) (text obj))))
    (:name "right"
     :get  ,(lambda (control)
	      (if (equal (positioning control) "static")
		  "n/a"
		  (right control)))
     :set  ,(lambda (control obj)
	      (setf (right control) (text obj))))
    (:name "positioning"
     :setup ,(lambda (control td1 td2)
	       (declare (ignore td1))
	       (let ((dd (create-select td2))
		     (v  (string-downcase (positioning control))))
		 (add-select-options dd `(,v
					  "absolute"
					  "static"
					  "relative"
					  "sticky"
					  "fixed"))
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
    (:name "opacity"
     :style "opacity")
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
		 nil)))
    (:name "background attachment"
     :style "background-attachment")
    (:name "background image"
     :style "background-image")
    (:name "background position"
     :style "background-position")
    (:name "background origin"
     :style "background-origin")
    (:name "background repeat"
     :style "background-repeat")
    (:name "background clip"
     :style "background-clip")
    (:name "background size"
     :style "background-size")
    (:name "border"
     :style "border")
    (:name "border radius"
     :style "border-radius")
    (:name "box shadow"
     :style "box-shadow")
    (:name "text shadow"
     :style "text-shadow")
    (:name "outline"
     :style "outline")
    (:name "margin"
     :style "margin")
    (:name "padding"
     :style "padding")
    (:name "cursor"
     :style "cursor")
    (:name "font"
     :style "font")
    (:name "text alignment"
     :style "text-align")
    (:name "vertical align"
     :style "vertical-align")))

(defparameter *props-contents*
  `((:name "contents"
     :setup ,(lambda (control td1 td2)
	       (declare (ignore td1))
	       (let ((d1 (create-text-area td2 :value (inner-html control))))
		 (set-on-change d1 (lambda (obj)
				     (declare (ignore obj))
				     (setf (inner-html control) (value d1)))))
	       nil))))

(defparameter *props-text*
  `((:name "text"
     :get  ,(lambda (control)
	      (clog::jquery-query control (format nil "contents().not(~A.children()).text()" (clog::jquery control))))
     :set  ,(lambda (control obj)
	      (clog::jquery-execute control (format nil "contents().not(~A.children()).get(0).nodeValue='~A'"
						     (clog::jquery control) (text obj)))))))

(defparameter *props-css*
  `((:name "css classes"
     :prop "className")))

(defparameter *props-display*
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
    (:name "display"
     :style "display")
    (:name "editable"
     :prop "contenteditable")
    (:name "spell check"
     :prop "spellcheck")
    (:name "text direction"
     :prop "test-direction")
    (:name "language code"
     :prop "lang")
    (:name "overflow"
     :style "overflow")
    (:name "resizable"
     :style "resize")
    (:name "minimum width"
     :style "min-width")
    (:name "minimum height"
     :style "min-height")
    (:name "maximum width"
     :style "max-width")
    (:name "maximum height"
     :style "max-height")))

(defparameter *props-nav*
  '((:name "access key"
     :prop "access key")
    (:name "tool tip"
     :prop "title")
    (:name "tab index"
     :prop "tabindex")
    (:name "z index"
     :style "z-index")))

(defparameter *props-base*
  `(,@*props-location*
    ,@*props-with-height*
    ,@*props-css*
    ,@*props-colors*
    ,@*props-display*
    ,@*props-nav*))

(defparameter *props-element*
  `(,@*props-location*
    ,@*props-with-height*
    ,@*props-text*
    ,@*props-css*
    ,@*props-colors*
    ,@*props-display*
    ,@*props-nav*
    ,@*props-contents*))

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
					  "image" "file" "hidden"
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
     (:name "data list"
		       :get ,(lambda (control)
			       (clog::js-query control (format nil "$('#~A').attr('data-clog-name')"
							       (attribute control "list"))))
		       :set ,(lambda (control obj)
			       (setf (attribute control "list")
				     (clog::js-query control (format nil "$(\"[data-clog-name='~A']\").attr('id')"
								     (text obj))))))
    ,@*props-form-values*
    ,@*props-css*
    ,@*props-colors*
    ,@*props-display*
    ,@*props-nav*))

(defparameter *events-multimedia*
  '((:name "on-media-abort"
     :parameters "target")
    (:name "on-media-error"
     :parameters "target")
    (:name "on-can-play"
     :parameters "target")
    (:name "on-can-play-through"
     :parameters "target")
    (:name "on-duration-change"
     :parameters "target")
    (:name "on-emptied"
     :parameters "target")
    (:name "on-ended"
     :parameters "target")
    (:name "on-loaded-data"
     :parameters "target")
    (:name "on-loaded-meta-data"
     :parameters "target")
    (:name "on-load-start"
     :parameters "target")
    (:name "on-play"
     :parameters "target")
    (:name "on-pause"
     :parameters "target")
    (:name "on-playing"
     :parameters "target")
    (:name "on-progress"
     :parameters "target")
    (:name "on-rate-change"
     :parameters "target")
    (:name "on-seek"
     :parameters "target")
    (:name "on-seeked"
     :parameters "target")
    (:name "on-seeking"
     :parameters "target")
    (:name "on-stalled"
     :parameters "target")
    (:name "on-suspend"
     :parameters "target")
    (:name "on-time-updat"
     :parameters "target")
    (:name "on-volume-change"
     :parameters "target")
    (:name "on-waiting"
     :parameters "target")))

(defparameter *events-element*
  '((:name        "on-click"
     :parameters  "target")
    (:name        "on-focus"
     :parameters  "target")
    (:name        "on-blur"
     :parameters  "target")
    (:name        "on-change"
     :parameters  "target")
    (:name        "on-focus-in"
     :parameters  "target")
    (:name        "on-focus-out"
     :parameters  "target")
    (:name        "on-reset"
     :parameters  "target")
    (:name        "on-search"
     :parameters  "target")
    (:name        "on-select"
     :parameters  "target")
    (:name        "on-submit"
     :parameters  "target")
    (:name        "on-context-menu"
     :parameters  "target")
    (:name        "on-double-click"
     :parameters  "target")
    (:name        "on-mouse-click"
     :parameters  "target data")
    (:name        "on-mouse-double-click"
     :parameters  "target data")
    (:name        "on-mouse-right-click"
     :parameters  "target data")
    (:name        "on-mouse-enter"
     :parameters  "target")
    (:name        "on-mouse-leave"
     :parameters  "target")
    (:name        "on-mouse-over"
     :parameters  "target")
    (:name        "on-mouse-out"
     :parameters  "target")
    (:name        "on-mouse-down"
     :parameters  "target data")
    (:name        "on-mouse-up"
     :parameters  "target data")
    (:name        "on-mouse-move"
     :parameters  "target data")
    (:name        "on-pointer-enter"
     :parameters  "target")
    (:name        "on-pointer-leave"
     :parameters  "target")
    (:name        "on-pointer-over"
     :parameters  "target")
    (:name        "on-pointer-out"
     :parameters  "target")
    (:name        "on-pointer-down"
     :parameters  "target data")
    (:name        "on-pointer-up"
     :parameters  "target data")
    (:name        "on-pointer-move"
     :parameters  "target data")
    (:name        "on-touch-start"
     :parameters  "target data")
    (:name        "on-touch-move"
     :parameters  "target data")
    (:name        "on-touch-end"
     :parameters  "target data")
    (:name        "on-touch-cancel"
     :parameters  "target data")
    (:name        "on-character"
     :parameters  "target data")
    (:name        "on-key-down"
     :parameters  "target data")
    (:name        "on-key-up"
     :parameters  "target data")
    (:name        "on-key-press"
     :parameters  "target data")
    (:name        "on-copy"
     :parameters  "target")
    (:name        "on-cut"
     :parameters  "target")
    (:name        "on-paste"
     :parameters  "target")
    (:name        "on-resize"
     :parameters  "target")
    (:name        "on-drag-start"
     :parameters  "target")
    (:name        "on-drag"
     :parameters  "target")
    (:name        "on-drag-end"
     :parameters  "target")
    (:name        "on-drag-enter"
     :parameters  "target")
    (:name        "on-drag-leave"
     :parameters  "target")
    (:name        "on-drag-over"
     :parameters  "target")
    (:name        "on-drop"
     :parameters  "target data")))

(defparameter *supported-controls*
  (list
   '(:name           "group"
     :description    "tools"
     :create         nil
     :create-type    nil
     :events         nil
     :properties     nil)
   '(:name           "select"
     :description    "Selection Tool"
     :create         nil
     :create-type    nil
     :events         nil
     :properties     nil)
   `(:name           "custom"
     :description    "Custom HTML"
     :clog-type      clog:clog-element
     :create         clog:create-child
     :create-type    :custom-query
     :create-content "<div></div>"
     :events         (,@*events-element*)
     :properties     (,@*props-element*))
   `(:name           "style-block"
     :description    "Style Block"
     :clog-type      clog:clog-style-block
     :create         clog:create-style-block
     :create-type    :base
     :events         (,@*events-element*)
     :properties     ((:name "media"
		       :attr "media")
		      (:name "type"
		       :prop "type")
		      ,@*props-contents*))
   '(:name           "group"
     :description    "basic html"
     :create         nil
     :create-type    nil
     :events         nil
     :properties     nil)
   `(:name           "label"
     :description    "Label"
     :clog-type      clog:clog-label
     :create         clog:create-label
     :create-type    :element
     :create-content "Label"
     :events         (,@*events-element*)
     :properties     ((:name "for"
		       :get ,(lambda (control)
			       (let ((f (attribute control "for")))
				 (unless (equal "" f)
				   (clog::js-query control (format nil "$('#~A').attr('data-clog-name')"
								   f)))))
		       :set ,(lambda (control obj)
			       (setf (attribute control "for")
				     (clog::js-query control (format nil "$(\"[data-clog-name='~A']\").attr('id')"
										     (text obj))))))
		      ,@*props-element*))
   `(:name           "button"
     :description    "Button"
     :clog-type      clog:clog-button
     :create         clog:create-button
     :create-type    :element
     :create-content "Button"
     :events         (,@*events-element*)
     :properties     (,@*props-element*))
   `(:name           "div"
     :description    "Div"
     :clog-type      clog:clog-div
     :create         clog:create-div
     :create-type    :element
     :create-content "div"
     :events         (,@*events-element*)
     :properties     (,@*props-element*))
   `(:name           "textarea"
     :description    "Text Area"
     :clog-type      clog:clog-text-area
     :create         clog:create-text-area
     :create-type    :textarea
     :create-value   ""
     :events         (,@*events-element*)
     :properties     ((:name "rows"
		       :prop "rows")
		      (:name "columns"
		       :prop "columns")
		      (:name "word wrap"
		       :prop "wrap")
		      ,@*props-form-element*))
   `(:name           "dropdown"
     :description    "Drop down select"
     :clog-type      clog:clog-select
     :create         clog:create-select
     :create-type    :base
     :events         (,@*events-element*)
     :properties     ((:name "multiple select"
		       :get  ,(lambda (control)
				(property control "multiple"))
		       :set  ,(lambda (control obj)
				(if (or (equalp (text obj) "true") (equalp (text obj) "multiple"))
				    (setf (attribute control "multiple") t)
				    (remove-attribute control "multiple"))
				(property control "multiple")))
		      ,@*props-form-element*))
   `(:name           "listbox"
     :description    "Listbox select"
     :clog-type      clog:clog-select
     :create         clog:create-select
     :create-type    :base
     :setup          ,(lambda (control content control-record)
			(declare (ignore content) (ignore control-record))
			(setf (size control) "4"))
     :events         (,@*events-element*)
     :properties     ((:name "multiple select"
		       :get  ,(lambda (control)
				(property control "multiple"))
		       :set  ,(lambda (control obj)
				(if (or (equalp (text obj) "true") (equalp (text obj) "multiple"))
				    (setf (attribute control "multiple") t)
				    (remove-attribute control "multiple"))
				(property control "multiple")))
		      ,@*props-form-element*))
   `(:name           "option"
     :description    "Option Item"
     :clog-type      clog:clog-option
     :create         clog:create-option
     :create-content "option item"
     :create-type    :element
     :events         (,@*events-element*)
     :properties     ((:name "value"
		       :prop "value")
		      (:name "selected"
		       :get  ,(lambda (control)
				(property control "selected"))
		       :set  ,(lambda (control obj)
				(if (or (equalp (text obj) "true") (equalp (text obj) "selected"))
				    (setf (attribute control "selected") t)
				    (remove-attribute control "selected"))
				(property control "selected")))
		      (:name "disabled"
		       :get  ,(lambda (control)
				(property control "disabled"))
		       :set  ,(lambda (control obj)
				(if (or (equalp (text obj) "true") (equalp (text obj) "disabled"))
				    (setf (attribute control "disabled") t)
				    (remove-attribute control "disabled"))
				(property control "disabled")))
		      ,@*props-element*))
   `(:name           "optgroup"
     :description    "Option Group"
     :clog-type      clog:clog-optgroup
     :create         clog:create-optgroup
     :create-content "option group"
     :create-type    :element
     :events         (,@*events-element*)
     :properties     ((:name "disabled"
		       :get  ,(lambda (control)
				(property control "disabled"))
		       :set  ,(lambda (control obj)
				(if (or (equalp (text obj) "true") (equalp (text obj) "disabled"))
				    (setf (attribute control "disabled") t)
				    (remove-attribute control "disabled"))
				(property control "disabled")))
		      ,@*props-element*))
   `(:name           "image"
     :description    "Image"
     :clog-type      clog:clog-img
     :create         clog:create-img
     :create-type    :base
     :setup          ,(lambda (control content control-record)
			(declare (ignore content) (ignore control-record))
			(setf (url-src control) "/img/clogicon.png")
			(setf (alt-text control) "Add image url"))
     :events         (,@*events-element*)
     :properties     ((:name "image url"
		       :prop "src")
		      (:name "alternative text"
		       :prop "alt")
		      ,@*props-base*))
   `(:name           "meter"
     :description    "Meter"
     :clog-type      clog:clog-meter
     :create         clog:create-meter
     :create-type    :base
     :events         (,@*events-element*)
     :properties     ((:name "value"
		       :prop "value")
		      (:name "high"
		       :prop "high")
		      (:name "low"
		       :prop "low")
		      (:name "maximum"
		       :prop "max")
		      (:name "minimum"
		       :prop "min")
		      (:name "optimum"
		       :prop "optimum")
		      ,@*props-base*))
   `(:name           "progress"
     :description    "Progress Bar"
     :clog-type      clog:clog-progress-bar
     :create         clog:create-progress-bar
     :create-type    :base
     :events         (,@*events-element*)
     :properties     ((:name "value"
		       :prop "value")
		      (:name "maximum"
		       :prop "max")
		      ,@*props-base*))
   `(:name           "dialog"
     :description    "Dialog"
     :clog-type      clog:clog-dialog
     :create         clog:create-dialog
     :create-type    :element
     :create-content ""
     :events         (,@*events-element*)
     :properties     ((:name "open"
		       :get  ,(lambda (control)
				(property control "open"))
		       :set  ,(lambda (control obj)
				(if (or (equalp (text obj) "true") (equalp (text obj) "open"))
				    (setf (attribute control "open") t)
				    (remove-attribute control "open"))
				(property control "open")))
		      (:name "return value"
		       :prop "returnValue")
		      ,@*props-element*))
   '(:name           "group"
     :description    "forms"
     :create         nil
     :create-type    nil
     :events         nil
     :properties     nil)
   `(:name           "form"
     :description    "Form"
     :clog-type      clog:clog-form
     :create         clog:create-form
     :create-type    :base
     :events         (,@*events-element*)
     :properties     ((:name "action"
		       :attr "action")
		      (:name "target"
		       :attr "target")
		      (:name "method"
		       :attr "method")
		      (:name "encoding"
		       :prop "encoding")
		      (:name "form element count"
		       :get ,(lambda (control) (form-element-count control)))
		      ,@*props-element*))
   `(:name           "input"
     :description    "Form Input"
     :clog-type      clog:clog-form-element
     :create         clog:create-form-element
     :create-type    :form
     :create-param   :text
     :create-value   ""
     :events         (,@*events-element*)
     :properties     (,@*props-form-element*))
   `(:name           "fbutton"
     :description    "Form Button"
     :clog-type      clog:clog-form-element
     :create         clog:create-form-element
     :create-type    :form
     :create-param   :button
     :create-value   "Button"
     :events         (,@*events-element*)
     :properties     (,@*props-form-element*))
   `(:name           "reset"
     :description    "Form Reset"
     :clog-type      clog:clog-form-element
     :create         clog:create-form-element
     :create-type    :form
     :create-param   :reset
     :create-value   "Reset"
     :events         (,@*events-element*)
     :properties     (,@*props-form-element*))
   `(:name           "submit"
     :description    "Form Submit"
     :clog-type      clog:clog-form-element
     :create         clog:create-form-element
     :create-type    :form
     :create-param   :submit
     :create-value   "Submit"
     :events         (,@*events-element*)
     :properties     (,@*props-form-element*))
   `(:name           "checkbox"
     :description    "Form Checkbox"
     :clog-type      clog:clog-form-element
     :create         clog:create-form-element
     :create-type    :form
     :create-param   :checkbox
     :create-value   ""
     :events         (,@*events-element*)
     :properties     (,@*props-form-element*))
   `(:name           "radio"
     :description    "Form Radio Button"
     :clog-type      clog:clog-form-element
     :create         clog:create-form-element
     :create-type    :form
     :create-param   :radio
     :create-value   ""
     :events         (,@*events-element*)
     :properties     (,@*props-form-element*))
   `(:name           "color"
     :description    "Form Color Picker"
     :clog-type      clog:clog-form-element
     :create         clog:create-form-element
     :create-type    :form
     :create-param   :color
     :create-value   ""
     :events         (,@*events-element*)
     :properties     (,@*props-form-element*))
   `(:name           "date"
     :description    "Form Date Picker"
     :clog-type      clog:clog-form-element
     :create         clog:create-form-element
     :create-type    :form
     :create-param   :date
     :create-value   ""
     :events         (,@*events-element*)
     :properties     (,@*props-form-element*))
   `(:name           "datetime"
     :description    "Form Datetime Picker"
     :clog-type      clog:clog-form-element
     :create         clog:create-form-element
     :create-type    :form
     :create-param   :datetime
     :create-value   ""
     :events         (,@*events-element*)
     :properties     (,@*props-form-element*))
   `(:name           "datetime-local"
     :description    "Form Datetime Local Picker"
     :clog-type      clog:clog-form-element
     :create         clog:create-form-element
     :create-type    :form
     :create-param   :datetime-local
     :create-value   ""
     :events         (,@*events-element*)
     :properties     (,@*props-form-element*))
   `(:name           "email"
     :description    "Form Email Input"
     :clog-type      clog:clog-form-element
     :create         clog:create-form-element
     :create-type    :form
     :create-param   :email
     :create-value   ""
     :events         (,@*events-element*)
     :properties     (,@*props-form-element*))
   `(:name           "file"
     :description    "Form File Input"
     :clog-type      clog:clog-form-element
     :create         clog:create-form-element
     :create-type    :form
     :create-param   :file
     :create-value   ""
     :events         (,@*events-element*)
     :properties     (,@*props-form-element*))
   `(:name           "hidden"
     :description    "Form Hidden Value"
     :clog-type      clog:clog-form-element
     :create         clog:create-form-element
     :create-type    :form
     :create-param   :hidden
     :create-value   ""
     :events         (,@*events-element*)
     :properties     (,@*props-form-element*))
   `(:name           "fimage"
     :description    "Form Image Input"
     :clog-type      clog:clog-form-element
     :create         clog:create-form-element
     :create-type    :form
     :create-param   :image
     :create-value   ""
     :setup          ,(lambda (control content control-record)
			(declare (ignore content) (ignore control-record))
			(setf (url-src control) "/img/clogicon.png")
			(setf (alt-text control) "Add image url"))
     :events         (,@*events-element*)
     :properties     (,@*props-form-element*))
   `(:name           "month"
     :description    "Form Month and Year Picker"
     :clog-type      clog:clog-form-element
     :create         clog:create-form-element
     :create-type    :form
     :create-param   :month
     :create-value   ""
     :events         (,@*events-element*)
     :properties     (,@*props-form-element*))
   `(:name           "number"
     :description    "Form Number Picker"
     :clog-type      clog:clog-form-element
     :create         clog:create-form-element
     :create-type    :form
     :create-param   :number
     :create-value   ""
     :events         (,@*events-element*)
     :properties     (,@*props-form-element*))
   `(:name           "password"
     :description    "Form Password Input"
     :clog-type      clog:clog-form-element
     :create         clog:create-form-element
     :create-type    :form
     :create-param   :password
     :create-value   ""
     :events         (,@*events-element*)
     :properties     (,@*props-form-element*))
   `(:name           "range"
     :description    "Form Range Picker"
     :clog-type      clog:clog-form-element
     :create         clog:create-form-element
     :create-type    :form
     :create-param   :range
     :create-value   ""
     :events         (,@*events-element*)
     :properties     (,@*props-form-element*))
   `(:name           "search"
     :description    "Form Search Input"
     :clog-type      clog:clog-form-element
     :create         clog:create-form-element
     :create-type    :form
     :create-param   :search
     :create-value   ""
     :events         (,@*events-element*)
     :properties     (,@*props-form-element*))
   `(:name           "tel"
     :description    "Form Tel Input"
     :clog-type      clog:clog-form-element
     :create         clog:create-form-element
     :create-type    :form
     :create-param   :tel
     :create-value   ""
     :events         (,@*events-element*)
     :properties     (,@*props-form-element*))
   `(:name           "time"
     :description    "Form Time Picker"
     :clog-type      clog:clog-form-element
     :create         clog:create-form-element
     :create-type    :form
     :create-param   :time
     :create-value   ""
     :events         (,@*events-element*)
     :properties     (,@*props-form-element*))
   `(:name           "url"
     :description    "Form URL Input"
     :clog-type      clog:clog-form-element
     :create         clog:create-form-element
     :create-type    :form
     :create-param   :url
     :create-value   ""
     :events         (,@*events-element*)
     :properties     (,@*props-form-element*))
   `(:name           "week"
     :description    "Form Week Input"
     :clog-type      clog:clog-form-element
     :create         clog:create-form-element
     :create-type    :form
     :create-param   :week
     :create-value   ""
     :events         (,@*events-element*)
     :properties     (,@*props-form-element*))
   `(:name           "fieldset"
     :description    "Fieldset"
     :clog-type      clog:clog-fieldset
     :create         clog:create-fieldset
     :create-type    :base
     :events         (,@*events-element*)
     :properties     (,@*props-base*))
   `(:name           "legend"
     :description    "Fieldset Legend"
     :clog-type      clog:clog-legend
     :create         clog:create-legend
     :create-content "Legend here"
     :create-type    :element
     :events         (,@*events-element*)
     :properties     (,@*props-element*))
   `(:name           "datalist"
     :description    "Data list"
     :clog-type      clog:clog-data-list
     :create         clog:create-data-list
     :create-type    :base
     :events         (,@*events-element*)
     :properties     (,@*props-base*))
   '(:name           "group"
     :description    "text display"
     :create         nil
     :create-type    nil
     :events         nil
     :properties     nil)
   `(:name           "span"
     :description    "Span"
     :clog-type      clog:clog-span
     :create         clog:create-span
     :create-type    :element
     :create-content "span"
     :events         (,@*events-element*)
     :properties     (,@*props-contents*
		      ,@*props-element*))
   `(:name           "link"
     :description    "Link"
     :clog-type      clog:clog-a
     :create         clog:create-a
     :create-type    :element
     :create-content "HTML Link"
     :events         (,@*events-element*)
     :properties     ((:name "href link"
		       :prop "href")
		      (:name "target"
		       :prop "target")
		      ,@*props-element*))
   `(:name           "hr"
     :description    "Horizontal Rule"
     :clog-type      clog:clog-hr
     :create         clog:create-hr
     :create-type    :base
     :events         (,@*events-element*)
     :properties     (,@*props-base*))
   `(:name           "br"
     :description    "Line Break"
     :clog-type      clog:clog-br
     :create         clog:create-br
     :create-type    :base
     :events         (,@*events-element*)
     :properties     (,@*props-base*))
   `(:name           "p"
     :description    "Paragraph"
     :clog-type      clog:clog-p
     :create         clog:create-p
     :create-content "Paragraph"
     :create-type    :element
     :events         (,@*events-element*)
     :properties     (,@*props-element*))
   `(:name           "ol"
     :description    "Ordered List"
     :clog-type      clog:clog-ordered-list
     :create         clog:create-ordered-list
     :create-type    :base
     :events         (,@*events-element*)
     :properties     ((:name "list kind"
		       :prop "list-style-type")
		      (:name "list location"
		       :prop "list-style-position")
		      ,@*props-element*))
   `(:name           "ul"
     :description    "Unordered List"
     :clog-type      clog:clog-unordered-list
     :create         clog:create-unordered-list
     :create-type    :base
     :events         (,@*events-element*)
     :properties     ((:name "value"
		       :prop "value")
		      ,@*props-element*))
   `(:name           "li"
     :description    "List Item"
     :clog-type      clog:clog-list-item
     :create         clog:create-list-item
     :create-type    :element
     :create-content "List Item"
     :events         (,@*events-element*)
     :properties     (,@*props-element*))
   `(:name           "table"
     :description    "Table"
     :clog-type      clog:clog-table
     :create         clog:create-table
     :create-type    :base
     :events         (,@*events-element*)
     :properties     (,@*props-base*))
   `(:name           "tr"
     :description    "Table Row"
     :clog-type      clog:clog-table-row
     :create         clog:create-table-row
     :create-type    :base
     :events         (,@*events-element*)
     :properties     (,@*props-base*))
   `(:name           "td"
     :description    "Table Column"
     :clog-type      clog:clog-table-column
     :create         clog:create-table-column
     :create-type    :element
     :create-content "Column"
     :events         (,@*events-element*)
     :properties     ((:name "column span"
		       :attr "colspan")
		      (:name "row span"
		       :attr "rowspan")
		      ,@*props-element*))
   `(:name           "th"
     :description    "Table Heading"
     :clog-type      clog:clog-table-heading
     :create         clog:create-table-heading
     :create-type    :element
     :create-content "Heading"
     :events         (,@*events-element*)
     :properties     ((:name "column span"
		       :attr "colspan")
		      (:name "row span"
		       :attr "rowspan")
		      (:name "abbreviated version"
		       :attr "abbr")
		      (:name "scope"
		       :attr "rowspan")
		      ,@*props-element*))
   `(:name           "tcolgroup"
     :description    "Table Column Group"
     :clog-type      clog:clog-table-column-group
     :create         clog:create-table-column-group
     :create-type    :base
     :events         (,@*events-element*)
     :properties     ((:name "span"
		       :attr "span")
		      ,@*props-base*))
   `(:name           "tcol"
     :description    "Table Column Group Item"
     :clog-type      clog:clog-table-column-group-item
     :create         clog:create-table-column-group-item
     :create-type    :base
     :create-content "Column Group Item"
     :events         (,@*events-element*)
     :properties     ((:name "span"
		       :attr "span")
		      ,@*props-base*))
   `(:name           "thead"
     :description    "Table Head"
     :clog-type      clog:clog-table-head
     :create         clog:create-table-head
     :create-type    :base
     :events         (,@*events-element*)
     :properties     (,@*props-base*))
   `(:name           "tbody"
     :description    "Table Body"
     :clog-type      clog:clog-table-body
     :create         clog:create-table-body
     :create-type    :base
     :events         (,@*events-element*)
     :properties     (,@*props-base*))
   `(:name           "tfoot"
     :description    "Table Footer"
     :clog-type      clog:clog-table-footer
     :create         clog:create-table-footer
     :create-type    :base
     :events         (,@*events-element*)
     :properties     (,@*props-base*))
   `(:name           "tcaption"
     :description    "Table Caption"
     :clog-type      clog:clog-table-caption
     :create         clog:create-table-caption
     :create-type    :element
     :create-content "Caption"
     :events         (,@*events-element*)
     :properties     ((:name "caption side"
		       :style "caption-side")
		      ,@*props-element*))
   `(:name           "dl"
     :description    "Definition List"
     :clog-type      clog:clog-definition-list
     :create         clog:create-definition-list
     :create-type    :base
     :events         (,@*events-element*)
     :properties     (,@*props-base*))
   `(:name           "dt"
     :description    "Definition Term"
     :clog-type      clog:clog-term
     :create         clog:create-term
     :create-content "Term"
     :create-type    :element
     :events         (,@*events-element*)
     :properties     (,@*props-element*))
   `(:name           "dd"
     :description    "Definition Description"
     :clog-type      clog:clog-description
     :create         clog:create-description
     :create-content "Description"
     :create-type    :element
     :events         (,@*events-element*)
     :properties     (,@*props-element*))
   `(:name           "details"
     :description    "Details Block"
     :clog-type      clog:clog-details
     :create         clog:create-details
     :create-type    :element
     :create-content "Details"
     :events         (,@*events-element*)
     :properties     ((:name "open"
		       :get  ,(lambda (control)
				(property control "open"))
		       :set  ,(lambda (control obj)
				(if (or (equalp (text obj) "true") (equalp (text obj) "open"))
				    (setf (attribute control "open") t)
				    (remove-attribute control "open"))
				(property control "open")))
		      ,@*props-element*))
   `(:name           "summary"
     :description    "Summary Block"
     :clog-type      clog:clog-summary
     :create         clog:create-summary
     :create-content "Summary"
     :create-type    :element
     :events         (,@*events-element*)
     :properties     (,@*props-element*))
   '(:name           "group"
     :description    "multi-media"
     :create         nil
     :create-type    nil
     :events         nil
     :properties     nil)
   `(:name           "audio"
     :description    "Audio Player"
     :clog-type      clog:clog-audio
     :create         clog:create-audio
     :create-type    :base
     :events         (,@*events-multimedia*
		      ,@*events-element*)
     :properties     ((:name "media url"
		       :prop "src")
		      (:name "volume"
		       :prop "volume")
		      (:name "controls"
		       :attr "controls")
		      (:name "preload"
		       :attr "preload")
		      (:name "autoplay"
		       :attr "autoplay")
		      (:name "muted"
		       :prop "muted")
		      (:name "loop"
		       :prop "loop")
		      ,@*props-base*))
   `(:name           "video"
     :description    "Video Player"
     :clog-type      clog:clog-video
     :create         clog:create-video
     :create-type    :base
     :events         (,@*events-multimedia*
		      ,@*events-element*)
     :properties     ((:name "media url"
		       :prop "src")
		      (:name "volume"
		       :prop "volume")
		      (:name "controls"
		       :attr "controls")
		      (:name "preload"
		       :attr "preload")
		      (:name "autoplay"
		       :attr "autoplay")
		      (:name "muted"
		       :prop "muted")
		      (:name "loop"
		       :prop "loop")
		      ,@*props-base*))
   '(:name           "group"
     :description    "graphics"
     :create         nil
     :create-type    nil
     :events         nil
     :properties     nil)
   `(:name           "canvas"
     :description    "Canvas"
     :clog-type      clog:clog-canvas
     :create         clog:create-canvas
     :create-type    :base
     :events         (,@*events-element*)
     :properties     (,@*props-base*))))

(defparameter *supported-templates*
  (list
   '(:name    "New CLOG Basic HTML Project"
     :code    "ncp"
     :type    :system
     :www     "templates/www/"
     :loc     "templates/projects/clog/")
   '(:name    "New CLOG-GUI Project"
     :code    "ncgp"
     :type    :system
     :www     "templates/www/"
     :loc     "templates/projects/clog-gui/")
   '(:name    "New CLOG-WEB Project"
     :code    "ncwp"
     :type    :system
     :www     "templates/www/"
     :loc     "templates/projects/clog-web/")))
