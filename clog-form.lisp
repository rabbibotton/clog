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

(defgeneric form-element-count (clog-formt)
  (:documentation "Get form element count."))

(defmethod form-element-count ((obj clog-form))
  (property obj "length"))

;;;;;;;;;;;;
;; submit ;;
;;;;;;;;;;;;

(defgeneric submit (clog-formt)
  (:documentation "Submit form."))

(defmethod submit ((obj clog-form))
  (execute obj "submit()"))

;;;;;;;;;;;
;; reset ;;
;;;;;;;;;;;

(defgeneric reset (clog-formt)
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
  
(defgeneric create-form-element (clog-form element-type &key name value label)
  (:documentation "Create a new clog-form-element as child of CLOG-FORM.
clog-form-elements are always placed with in the CLOG-FORM in the DOM"))

(defmethod create-form-element ((obj clog-form) element-type
				&key (name nil) (value "") (label nil))
  (let ((element (create-child
		  obj (format nil "<input type='~A' form='~A' value='~A' ~A/>"
			      (escape-string element-type)
			      (html-id obj)
			      value
			      (if name
				  (format nil "name='~A'" name)
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-label
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-label (clog-element)()
  (:documentation "CLOG Form Element Label Object"));

;;;;;;;;;;;;;;;;;;
;; create-label ;;
;;;;;;;;;;;;;;;;;;


(defgeneric create-label (clog-form &key content label-for)
  (:documentation "Create a new clog-label as child of CLOG-FORM."))

(defmethod create-label ((obj clog-form) &key (content "") (label-for nil))
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
;; Implementation - clog-data-list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-data-list (clog-element)()
  (:documentation "CLOG Form Element Data-List Options Object"));

;;;;;;;;;;;;;;;;;;;;;;
;; create-data-list ;;
;;;;;;;;;;;;;;;;;;;;;;

(defgeneric create-data-list (clog-obj)
  (:documentation "Create a new clog-data-list as child of CLOG-FORM."))

(defmethod create-data-list ((obj clog-obj))
  (create-child obj "<datalist />" :clog-type 'clog-data-list :auto-place t))

;;;;;;;;;;;;;;;;
;; add-option ;;
;;;;;;;;;;;;;;;;

(defgeneric add-option (clog-data-list value)
  (:documentation "Add option VALUE to data-list."))

(defmethod add-option ((obj clog-data-list) value)
  (create-child obj (format nil "<option value='~A'>" (escape-string value))
		:clog-type 'clog-element :auto-place t))

