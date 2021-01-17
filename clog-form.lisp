;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG - The Common Lisp Omnificent GUI                                 ;;;;
;;;; (c) 2020-2021 David Botton                                            ;;;;
;;;; License BSD 3 Clause                                                  ;;;;
;;;;                                                                       ;;;;
;;;; clog-form.lisp                                                        ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:in-package :clog)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-form
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-form (clog-element)()
  (:documentation "CLOG Form Objecs is the base class for all html forms."))


;;;;;;;;;;;;;;;;;
;; create-form ;;
;;;;;;;;;;;;;;;;;

(defgeneric create-form (clog-obj &key action method target auto-place)
  (:documentation "Create a new CLOG-Form as child of CLOG-OBJ that organizes
a collection of form elements in to a single form if :AUTO-PLACE (default t)
place-inside-bottom-of CLOG-OBJ. In CLOG a form's on-submit handler should be
set and the form element values handled in that handler as opposed to the
HTML model of submitting to a new \"page\". If though one wishes to submit to
another page can use the :ACTION :METHOD and :TARGET keys and either do not
set an on-submit handler or call (submit CLOG-FORM) to perform the form
action."))

(defmethod create-form ((obj clog-obj)
			&key (action "")
			  (method "get")
			  (target "_self")
			  (auto-place t))
  (create-child obj (format nil "<form action='~A' method='~A' target='~A'/>"
			    action method target)
		:clog-type 'clog-form :auto-place auto-place))

;;;;;;;;;;;;;;;;;;;;;;;;
;; form-element-count ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric form-element-count (clog-form)
  (:documentation "Get form element count."))

(defmethod form-element-count ((obj clog-form))
  (property obj "length"))

;;;;;;;;;;;;
;; submit ;;
;;;;;;;;;;;;

(defgeneric submit (clog-form)
  (:documentation "Submit form."))

(defmethod submit ((obj clog-form))
  (execute obj "submit()"))

;;;;;;;;;;;
;; reset ;;
;;;;;;;;;;;

(defgeneric reset (clog-form)
  (:documentation "Reset form."))

(defmethod reset ((obj clog-form))
  (execute obj "reset()"))

;;;;;;;;;;;;;;;;;;;
;; autocompletep ;;
;;;;;;;;;;;;;;;;;;;

(defgeneric autocompletep (clog-form)
  (:documentation "Get/Setf form autocompletep."))

(defmethod autocompletep ((obj clog-form))
  (js-on-p (property obj "autocompletep")))

(defgeneric set-autocompletep (clog-form autocompletep)
  (:documentation "Set autocompletep for CLOG-FORM"))

(defmethod set-autocompletep ((obj clog-form) autocompletep)
  (setf (property obj "autocompletep") (p-on-js autocompletep)))
(defsetf autocompletep set-autocompletep)

;;;;;;;;;;;;;;
;; encoding ;;
;;;;;;;;;;;;;;

(defgeneric encoding (clog-form)
  (:documentation "Get/Setf form encoding. Comming values are:
      application/x-www-form-urlencoded
      multipart/form-data
      text/plain"))

(defmethod encoding ((obj clog-form))
  (property obj "encoding"))

(defgeneric set-encoding (clog-form encoding)
  (:documentation "Set encoding for CLOG-FORM"))

(defmethod set-encoding ((obj clog-form) encoding)
  (setf (property obj "encoding") encoding))
(defsetf encoding set-encoding)

;;;;;;;;;;;;;;;;;;;;;;;;
;; validate-on-submit ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric validate-on-submit (clog-form)
  (:documentation "Get/Setf form validate-on-submit."))

(defmethod validate-on-submit ((obj clog-form))
  (not (js-true-p (property obj "noValidate"))))

(defgeneric set-validate-on-submit (clog-form value)
  (:documentation "Set VALIDATE-ON-SUBMIT for CLOG-FORM"))

(defmethod set-validate-on-submit ((obj clog-form) value)
  (setf (property obj "noValidate") (p-true-js (not value))))
(defsetf validate-on-submit set-validate-on-submit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-form-clement
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-form-element (clog-element)()
  (:documentation "CLOG Form Element Object is the base class for all form
elements."))

;;;;;;;;;;;;;;;;;;;;;;;;
;; create-form-elemnt ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(deftype form-element-type ()
  '(member  :button :checkbox :color :date :datetime :datetime-local :email
    :file :hidden :image :month :number :password :radio :range
    :reset :search :submit :tel :text :time :url :week))
  
(defgeneric create-form-element (clog-obj element-type &key name value label)
  (:documentation "Create a new clog-form-element as child of CLOG-OBJ.
It is importamt that clog-form-elements are a child or descendant of a
clog-form in the DOM. The radio ELEMENT-TYPE groups by NAME."))

(defmethod create-form-element ((obj clog-obj) element-type
				&key (name nil) (value nil) (label nil))
  (let ((element (create-child
		  obj (format nil "<input type='~A'~A~A/>"
			      (escape-string element-type)
			      (if value
				  (format nil " value='~A'" value)
				  "")
			      (if name
				  (format nil " name='~A'" name)
				  ""))
		  :clog-type 'clog-form-element :auto-place t)))
    (when label
      (label-for label element))
    element))

;;;;;;;;;;;;;;;;;;
;; autocomplete ;;
;;;;;;;;;;;;;;;;;;

(defgeneric autocomplete (clog-form-element)
  (:documentation "Get/Setf form element autocomplete."))

(defmethod autocomplete ((obj clog-form-element))
  (property obj "autocomplete"))

(defgeneric set-autocomplete (clog-form-element value)
  (:documentation "Set autocomplete AUTOCOMPLETE for CLOG-FORM-ELEMENT"))

(defmethod set-autocomplete ((obj clog-form-element) value)
  (setf (property obj "autocomplete") value))
(defsetf autocomplete set-autocomplete)

;;;;;;;;;;;;;;;;
;; autofocusp ;;
;;;;;;;;;;;;;;;;

(defgeneric autofocusp (clog-form-element)
  (:documentation "Get/Setf form element autofocusp. Only one element should
have this set true. Autofocus on element when form loaded. "))

(defmethod autofocusp ((obj clog-form-element))
  (js-true-p (attribute obj "autofocus")))

(defgeneric set-autofocusp (clog-form-element value)
  (:documentation "Set autofocusp AUTOFOCUSP for CLOG-FORM-ELEMENT"))

(defmethod set-autofocusp ((obj clog-form-element) value)
  (if value
      (setf (attribute obj "autofocus") "true")
      (remove-attribute obj "autofocus")))
(defsetf autofocusp set-autofocusp)

;;;;;;;;;;;;;;;;;;
;; place-holder ;;
;;;;;;;;;;;;;;;;;;

(defgeneric place-holder (clog-form-element)
  (:documentation "Get/Setf form element place holder."))

(defmethod place-holder ((obj clog-form-element))
  (property obj "placeholder"))

(defgeneric set-place-holder (clog-form-element value)
  (:documentation "Set placeholder PLACE-HOLDER for CLOG-FORM-ELEMENT"))

(defmethod set-place-holder ((obj clog-form-element) value)
  (setf (property obj "placeholder") value))
(defsetf place-holder set-place-holder)

;;;;;;;;;;;;;;;
;; disabledp ;;
;;;;;;;;;;;;;;;

(defgeneric disabledp (clog-form-element)
  (:documentation "Get/Setf form element disabledp."))

(defmethod disabledp ((obj clog-form-element))
  (js-true-p (property obj "disabled")))

(defgeneric set-disabledp (clog-form-element value)
  (:documentation "Set disabledp DISABLEDP for CLOG-FORM-ELEMENT"))

(defmethod set-disabledp ((obj clog-form-element) value)
  (setf (property obj "disabled") (p-true-js value)))
(defsetf disabledp set-disabledp)

;;;;;;;;;;;;;;;;;
;; read-only-p ;;
;;;;;;;;;;;;;;;;;

(defgeneric read-only-p (clog-form-element)
  (:documentation "Get/Setf form element read-only-p."))

(defmethod read-only-p ((obj clog-form-element))
  (js-true-p (property obj "readonly")))

(defgeneric set-read-only-p (clog-form-element value)
  (:documentation "Set read-only-p READ-ONLY-P for CLOG-FORM-ELEMENT"))

(defmethod set-read-only-p ((obj clog-form-element) value)
  (setf (property obj "readonly") (p-true-js value)))
(defsetf read-only-p set-read-only-p)

;;;;;;;;;;;;;;;
;; requiredp ;;
;;;;;;;;;;;;;;;

(defgeneric requiredp (clog-form-element)
  (:documentation "Get/Setf form element requiredp."))

(defmethod requiredp ((obj clog-form-element))
  (js-true-p (property obj "required")))

(defgeneric set-requiredp (clog-form-element value)
  (:documentation "Set requiredp REQUIREDP for CLOG-FORM-ELEMENT"))

(defmethod set-requiredp ((obj clog-form-element) value)
  (setf (property obj "required") (p-true-js value)))
(defsetf requiredp set-requiredp)

;;;;;;;;;;
;; name ;;
;;;;;;;;;;

(defgeneric name (clog-form-element)
  (:documentation "Get/Setf form element name.
   Form element name, name is not the id of the element but rather how
   the data returned from the element will be named in the submit to a
   server. For example if Name is My_Field a GET request could look like
   http://localhost:8080?My_Field=xxxx"))

(defmethod name ((obj clog-form-element))
  (property obj "name"))

(defgeneric set-name (clog-form-element value)
  (:documentation "Set name VALUE for CLOG-FORM-ELEMENT"))

(defmethod set-name ((obj clog-form-element) value)
  (setf (property obj "name") value))
(defsetf name set-name)

;;;;;;;;;;;;;;;;;;;
;; default-value ;;
;;;;;;;;;;;;;;;;;;;

(defgeneric default-value (clog-form-element)
  (:documentation "Get/Setf form element default-value.
   If the form is reset the value will be set to default value
   If Value is set at time of creation it also sets it as the Default_Value"))

(defmethod default-value ((obj clog-form-element))
  (property obj "defaultValue"))

(defgeneric set-default-value (clog-form-element value)
  (:documentation "Set default-value VALUE for CLOG-FORM-ELEMENT"))

(defmethod set-default-value ((obj clog-form-element) value)
  (setf (property obj "defaultValue") value))
(defsetf default-value set-default-value)

;;;;;;;;;;;
;; value ;;
;;;;;;;;;;;

(defgeneric value (clog-form-element)
  (:documentation "Get/Setf form element value.
   Form element values are not accessible through the Text property but
   instead through the value property."))

(defmethod value ((obj clog-form-element))
  (property obj "value"))

(defgeneric set-value (clog-form-element value)
  (:documentation "Set value VALUE for CLOG-FORM-ELEMENT"))

(defmethod set-value ((obj clog-form-element) value)
  (setf (property obj "value") value))
(defsetf value set-value)

;;;;;;;;;;;;;
;; pattern ;;
;;;;;;;;;;;;;

(defgeneric pattern (clog-form-element)
  (:documentation "Get/Setf form element regular expression pattern.
   (see JavaScript RegExp object for specifics)
   Form validation pattern. validate-on-submit fields with input
   will validate against their pattern if set on submit.
   In cases where a specific input type is not supported like
   (date, week, etc.) Pattern can be set to ensure the expected results.
   This works since Input type will fall back to a text input."))

(defmethod pattern ((obj clog-form-element))
  (property obj "pattern"))

(defgeneric set-pattern (clog-form-element value)
  (:documentation "Set pattern VALUE for CLOG-FORM-ELEMENT"))

(defmethod set-pattern ((obj clog-form-element) value)
  (setf (property obj "pattern") value))
(defsetf pattern set-pattern)

;;;;;;;;;;;;;
;; minimum ;;
;;;;;;;;;;;;;

(defgeneric minimum (clog-form-element)
  (:documentation "Get/Setf form element minimum."))

(defmethod minimum ((obj clog-form-element))
  (property obj "min"))

(defgeneric set-minimum (clog-form-element value)
  (:documentation "Set minimum VALUE for CLOG-FORM-ELEMENT"))

(defmethod set-minimum ((obj clog-form-element) value)
  (setf (property obj "min") value))
(defsetf minimum set-minimum)

;;;;;;;;;;;;;
;; maximum ;;
;;;;;;;;;;;;;

(defgeneric maximum (clog-form-element)
  (:documentation "Get/Setf form element maximum."))

(defmethod maximum ((obj clog-form-element))
  (property obj "max"))

(defgeneric set-maximum (clog-form-element value)
  (:documentation "Set maximum VALUE for CLOG-FORM-ELEMENT"))

(defmethod set-maximum ((obj clog-form-element) value)
  (setf (property obj "max") value))
(defsetf maximum set-maximum)

;;;;;;;;;;;;;;;;;;
;; element-step ;;
;;;;;;;;;;;;;;;;;;

(defgeneric element-step (clog-form-element)
  (:documentation "Get/Setf form element step."))

(defmethod element-step ((obj clog-form-element))
  (property obj "step"))

(defgeneric set-element-step (clog-form-element value)
  (:documentation "Set element-step VALUE for CLOG-FORM-ELEMENT"))

(defmethod set-element-step ((obj clog-form-element) value)
  (setf (property obj "step") value))
(defsetf element-step set-element-step)

;;;;;;;;;;
;; size ;;
;;;;;;;;;;

(defgeneric size (clog-form-element)
  (:documentation "Get/Setf form element size."))

(defmethod size ((obj clog-form-element))
  (property obj "size"))

(defgeneric set-size (clog-form-element value)
  (:documentation "Set size VALUE for CLOG-FORM-ELEMENT"))

(defmethod set-size ((obj clog-form-element) value)
  (setf (property obj "size") value))
(defsetf size set-size)

;;;;;;;;;;;;;;;;;;;;
;; minimum-length ;;
;;;;;;;;;;;;;;;;;;;;

(defgeneric minimum-length (clog-form-element)
  (:documentation "Get/Setf form element minimum-length."))

(defmethod minimum-length ((obj clog-form-element))
  (property obj "minlength"))

(defgeneric set-minimum-length (clog-form-element value)
  (:documentation "Set minimum-length VALUE for CLOG-FORM-ELEMENT"))

(defmethod set-minimum-length ((obj clog-form-element) value)
  (setf (property obj "minlength") value))
(defsetf minimum-length set-minimum-length)

;;;;;;;;;;;;;;;;;;;;
;; maximum-length ;;
;;;;;;;;;;;;;;;;;;;;

(defgeneric maximum-length (clog-form-element)
  (:documentation "Get/Setf form element maximum-length."))

(defmethod maximum-length ((obj clog-form-element))
  (property obj "maxlength"))

(defgeneric set-maximum-length (clog-form-element value)
  (:documentation "Set maximum-length VALUE for CLOG-FORM-ELEMENT"))

(defmethod set-maximum-length ((obj clog-form-element) value)
  (setf (property obj "maxlength") value))
(defsetf maximum-length set-maximum-length)

;;;;;;;;;;;;
;; select ;;
;;;;;;;;;;;;

(defgeneric select (clog-form-element)
  (:documentation "Select and highlight the contents of element."))

(defmethod select ((obj clog-form-element))
  (execute obj "select()"))

;;;;;;;;;;;;;;;;;;;
;; set-data-list ;;
;;;;;;;;;;;;;;;;;;;

(defgeneric set-data-list (clog-form-element data-list)
  (:documentation "Set the option data list to use for this element."))

(defmethod set-data-list ((obj clog-form-element) data-list)
  (setf (attribute obj "list") (html-id data-list)))

;;;;;;;;;;;;;;;;;;;;
;; make-data-list ;;
;;;;;;;;;;;;;;;;;;;;

(defgeneric make-data-list (clog-form-element data)
  (:documentation "Set the option data list to use for this element."))

(defmethod make-data-list ((obj clog-form-element) data)
  (set-data-list obj (create-data-list obj :data-list data)))

;;;;;;;;;;;;;;;;;
;; file-accept ;;
;;;;;;;;;;;;;;;;;

(defgeneric file-accept (clog-form-element)
  (:documentation "Get/Setf form element file-accept. Only works with
the File form element type.
   example: (setf (file-accept obj) \"image/png, image/jpeg\")"))

(defmethod file-accept ((obj clog-form-element))
  (attribute obj "accept"))

(defgeneric set-file-accept (clog-form-element value)
  (:documentation "Set file-accept VALUE for CLOG-FORM-ELEMENT"))

(defmethod set-file-accept ((obj clog-form-element) value)
  (setf (attribute obj "accept") value))
(defsetf file-accept set-file-accept)

;;;;;;;;;;;;;
;; url-src ;;
;;;;;;;;;;;;;

(defgeneric url-src (clog-form-element)
  (:documentation "Get/Setf the url-src of the img."))

(defmethod url-src ((obj clog-form-element))
  (property obj "src"))

(defgeneric set-url-src (clog-form-element value)
  (:documentation "Set url-src VALUE for CLOG-FORM-ELEMENT"))

(defmethod set-url-src ((obj clog-form-element) value)
  (setf (property obj "src") value))
(defsetf url-src set-url-src)

;;;;;;;;;;;;;;
;; alt-text ;;
;;;;;;;;;;;;;;

(defgeneric alt-text (clog-form-element)
  (:documentation "Get/Setf the alt-text of the img."))

(defmethod alt-text ((obj clog-form-element))
  (attribute obj "alt"))

(defgeneric set-alt-text (clog-form-element value)
  (:documentation "Set alt-text VALUE for CLOG-FORM-ELEMENT"))

(defmethod set-alt-text ((obj clog-form-element) value)
  (setf (attribute obj "alt") value))
(defsetf alt-text set-alt-text)

;;;;;;;;;;;;;;
;; checkedp ;;
;;;;;;;;;;;;;;

(defgeneric checkedp (clog-form-element)
  (:documentation "Get/Setf form element checkedp."))

(defmethod checkedp ((obj clog-form-element))
  (js-true-p (property obj "checked")))

(defgeneric set-checkedp (clog-form-element value)
  (:documentation "Set VALUE if checkedp for CLOG-FORM-ELEMENT"))

(defmethod set-checkedp ((obj clog-form-element) value)
  (setf (property obj "checked") (p-true-js value)))
(defsetf checkedp set-checkedp)

;;;;;;;;;;;;;;;;
;; input-mode ;;
;;;;;;;;;;;;;;;;

(deftype input-mode-type ()
  '(member :none :text :tel :url :email :numeric :decimal :search))

(defgeneric input-mode (clog-form-element)
  (:documentation "Get/Setf hint the input-mode of an element for
virtual keyboards."))

(defmethod input-mode ((obj clog-form-element))
  (attribute obj "inputmode"))

(defgeneric set-input-mode (clog-form-element value)
  (:documentation "Set input-mode VALUE for CLOG-FORM-ELEMENT"))

(defmethod set-input-mode ((obj clog-form-element) value)
  (setf (attribute obj "inputmode") value))
(defsetf input-mode set-input-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-label
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-label (clog-element)()
  (:documentation "CLOG Form Element Label Object"));

;;;;;;;;;;;;;;;;;;
;; create-label ;;
;;;;;;;;;;;;;;;;;;


(defgeneric create-label (clog-obj &key content label-for)
  (:documentation "Create a new clog-label as child of CLOG-OBJ."))

(defmethod create-label ((obj clog-obj) &key (content "") (label-for nil))
  (create-child obj (format nil "<label for='~A'>~A</label>"
			    (if label-for
				(html-id label-for)
				"")
			    (escape-string content))
		:clog-type 'clog-label :auto-place t))

;;;;;;;;;;;;;;;
;; label-for ;;
;;;;;;;;;;;;;;;

(defgeneric label-for (clog-label element)
  (:documentation "Set label is for ELEMENT."))

(defmethod label-for ((obj clog-label) element)
  (setf (attribute obj "for") element))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-fieldset
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-fieldset (clog-element)()
  (:documentation "CLOG Form Element Fieldset Object"));

;;;;;;;;;;;;;;;;;;;;;
;; create-fieldset ;;
;;;;;;;;;;;;;;;;;;;;;


(defgeneric create-fieldset (clog-obj &key legend)
  (:documentation "Create a new clog-fieldset as child of CLOG-OBJ."))

(defmethod create-fieldset ((obj clog-obj) &key (legend nil))
  (create-child obj (format nil "<fieldset>~A</fieldset>"
			    (if legend
				(format nil "<legend>~A</legend>" legend)
				""))
		:clog-type 'clog-fieldset :auto-place t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-data-list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-data-list (clog-element)()
  (:documentation "CLOG Form Element Data-List Options Object"));

;;;;;;;;;;;;;;;;;;;;;;
;; create-data-list ;;
;;;;;;;;;;;;;;;;;;;;;;

(defgeneric create-data-list (clog-obj &key data-list)
  (:documentation "Create a new clog-data-list as child of CLOG-OBJ and
optionally fill in with contents of data-list."))

(defmethod create-data-list ((obj clog-obj) &key (data-list nil))
  (let ((element (create-child obj "<datalist />" :clog-type 'clog-data-list :auto-place t)))
    (when data-list
      (add-options element data-list))
    element))

;;;;;;;;;;;;;;;;
;; add-option ;;
;;;;;;;;;;;;;;;;

(defgeneric add-option (clog-data-list value)
  (:documentation "Add option VALUE to data-list."))

(defmethod add-option ((obj clog-data-list) value)
  (create-child obj (format nil "<option value='~A'>" (escape-string value))
		:clog-type 'clog-element :auto-place t))
  
;;;;;;;;;;;;;;;;;
;; add-options ;;
;;;;;;;;;;;;;;;;;

(defgeneric add-options (clog-data-list data-list)
  (:documentation "Add option VALUE to data-list."))

(defmethod add-options ((obj clog-data-list) data-list)
  (dolist (value data-list)
    (add-option obj value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-text-area
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-text-area (clog-form-element)()
  (:documentation "CLOG Form Element Text-Area Object"));

;;;;;;;;;;;;;;;;;;;;;;
;; create-text-area ;;
;;;;;;;;;;;;;;;;;;;;;;

(defgeneric create-text-area (clog-obj &key columns rows name value label)
  (:documentation "Create a new clog-text-area as child of CLOG-OBJ."))

(defmethod create-text-area ((obj clog-obj)
			     &key (columns 20)
			       (rows 2)
			       (name "")
			       (value "")
			       (label nil))
  (let ((element
	  (create-child obj
	    (format nil "<textarea name='~A' cols='~A' rows='~A'>~A</textarea>"
		    name columns rows (escape-string value))
	    :clog-type 'clog-text-area :auto-place t)))

    (when label
      (label-for label element))
    element))

;;;;;;;;;;;;;;;
;; word-wrap ;;
;;;;;;;;;;;;;;;

(defgeneric word-wrap (clog-text-area)
  (:documentation "Get/Setf form element word-wrap."))

(defmethod word-wrap ((obj clog-text-area))
  (property obj "wrap"))

(defgeneric set-word-wrap (clog-text-area value)
  (:documentation "Set word-wrap WORD-WRAP for CLOG-TEXT-AREA"))

(defmethod set-word-wrap ((obj clog-text-area) value)
  (setf (property obj "wrap") value))
(defsetf word-wrap set-word-wrap)

;;;;;;;;;;;;;;;
;; word-wrap ;;
;;;;;;;;;;;;;;;

(defgeneric word-wrap (clog-text-area)
  (:documentation "Get/Setf form element word-wrap."))

(defmethod word-wrap ((obj clog-text-area))
  (property obj "wrap"))

(defgeneric set-word-wrap (clog-text-area value)
  (:documentation "Set word-wrap WORD-WRAP for CLOG-TEXT-AREA"))

(defmethod set-word-wrap ((obj clog-text-area) value)
  (setf (property obj "wrap") value))
(defsetf word-wrap set-word-wrap)

;;;;;;;;;;;;;
;; columns ;;
;;;;;;;;;;;;;

(defgeneric columns (clog-text-area)
  (:documentation "Get/Setf form element columns."))

(defmethod columns ((obj clog-text-area))
  (property obj "cols"))

(defgeneric set-columns (clog-text-area value)
  (:documentation "Set columns COLUMNS for CLOG-TEXT-AREA"))

(defmethod set-columns ((obj clog-text-area) value)
  (setf (property obj "cols") value))
(defsetf columns set-columns)

;;;;;;;;;;
;; rows ;;
;;;;;;;;;;

(defgeneric rows (clog-text-area)
  (:documentation "Get/Setf form element rows."))

(defmethod rows ((obj clog-text-area))
  (property obj "rows"))

(defgeneric set-rows (clog-text-area value)
  (:documentation "Set rows ROWS for CLOG-TEXT-AREA"))

(defmethod set-rows ((obj clog-text-area) value)
  (setf (property obj "rows") value))
(defsetf rows set-rows)

;;;;;;;;;;;;;;;;;;;;
;; disable-resize ;;
;;;;;;;;;;;;;;;;;;;;

(defgeneric disable-resize (clog-text-area)
  (:documentation "Disable resize of text area."))

(defmethod disable-resize ((obj clog-text-area))
  (setf (resizable obj) :none))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-select
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-select (clog-form-element)()
  (:documentation "CLOG Form Element Select Options Object"));

;;;;;;;;;;;;;;;;;;;
;; create-select ;;
;;;;;;;;;;;;;;;;;;;

(defgeneric create-select (clog-obj &key name multiple label)
  (:documentation "Create a new clog-select as child of CLOG-OBJ."))

(defmethod create-select ((obj clog-obj) &key (name nil) (multiple nil) (label nil))
  (let ((element (create-child
		  obj (format nil "<select~A~A/>"
			      (if multiple
				  " multiple"
				  "")
			      (if name
				  (format nil " name='~A'" name)
				  ""))
		 :clog-type 'clog-select :auto-place t)))
    (when label
      (label-for label element))
    element))

;;;;;;;;;;;;;;;;;;;;;;;
;; add-select-option ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric add-select-option (clog-select value content)
  (:documentation "Add option VALUE to select."))

(defmethod add-select-option ((obj clog-select) value content)
  (create-child obj (format nil "<option value='~A'>~A</option>"
			    (escape-string value)
			    (escape-string content))
		:clog-type 'clog-element :auto-place t))
  
;;;;;;;;;;;;;;;;;;;;;;;;
;; add-select-options ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric add-select-options (clog-select select)
  (:documentation "Add group of options to select."))

(defmethod add-select-options ((obj clog-select) select)
  (dolist (value select)
    (add-select-option obj value value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-option
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-option (clog-form-element)()
  (:documentation "CLOG Form Element Option for CLOG Select Object"));

;;;;;;;;;;;;;;;;;;;
;; create-option ;;
;;;;;;;;;;;;;;;;;;;

(defgeneric create-option (clog-obj &key content value selected disabled)
  (:documentation "Create a new clog-option as child of CLOG-OBJ."))

(defmethod create-option ((obj clog-obj) &key
					   (content "")
					   (value nil)
					   (selected nil)
					   (disabled nil))			  			  
  (create-child obj (format nil "<option~A~A~A>~A</option>"
			    (if selected
				" selected"
				"")
			    (if disabled
				" disabled"
				"")
			    (if value
				(format nil " value='~A'" name)
				"")
			    content)
		:clog-type 'clog-option :auto-place t))

;;;;;;;;;;;;;;;
;; selectedp ;;
;;;;;;;;;;;;;;;

(defgeneric selectedp (clog-form-element)
  (:documentation "Get/Setf form element selectedp."))

(defmethod selectedp ((obj clog-form-element))
  (js-true-p (property obj "selected")))

(defgeneric set-selectedp (clog-form-element value)
  (:documentation "Set selectedp VALUE for CLOG-FORM-ELEMENT"))

(defmethod set-selectedp ((obj clog-form-element) value)
  (setf (property obj "selected") (p-true-js value)))
(defsetf selectedp set-selectedp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-optgroup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-optgroup (clog-form-element)()
  (:documentation "CLOG Form Element Optgroup for CLOG Select Object"));

;;;;;;;;;;;;;;;;;;;;;
;; create-optgroup ;;
;;;;;;;;;;;;;;;;;;;;;

(defgeneric create-optgroup (clog-obj &key content disabled)
  (:documentation "Create a new clog-optgroup as child of CLOG-OBJ."))

(defmethod create-optgroup ((obj clog-obj) &key
					   (content "")
					   (disabled nil))			  			  
  (create-child obj (format nil "<optgroup label='~A'~A/>"
			    content
			    (if disabled
				" disabled"
				""))
		:clog-type 'clog-optgroup :auto-place t))
