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

(defgeneric create-form (clog-obj &key auto-place)
  (:documentation "Create a new CLOG-Form as child of CLOG-OBJ that organizes
a collection of form elements in to a singnle form if :AUTO-PLACE (default t)
place-inside-bottom-of CLOG-OBJ. In CLOG a form's on-submit handler should be
set and the form elelement values handled in that handler as opposed to the
HTML model of submitting to a new \"page\"."))

(defmethod create-form ((obj clog-obj) &key (auto-place t))
  (create-child obj "<form />" :clog-type 'clog-form :auto-place auto-place))


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

;;;;;;;;;;;
;; value ;;
;;;;;;;;;;;

(defgeneric value (clog-form-element)
  (:documentation "Get/Setf from element value."))

(defmethod value ((obj clog-form-element))
  (property obj "value"))

(defgeneric set-value (clog-form-element value)
  (:documentation "Set value VALUE for CLOG-FORM-ELEMENT"))

(defmethod set-value ((obj clog-form-element) value)
  (setf (property obj "value") value))
(defsetf value set-value)

;;;;;;;;;;;;;;;;;;
;; place-holder ;;
;;;;;;;;;;;;;;;;;;

(defgeneric place-holder (clog-form-element)
  (:documentation "Get/Setf from element place holder."))

(defmethod place-holder ((obj clog-form-element))
  (property obj "placeholder"))

(defgeneric set-place-holder (clog-form-element place-holder)
  (:documentation "Set placeholder PLACE-HOLDER for CLOG-FORM-ELEMENT"))

(defmethod set-place-holder ((obj clog-form-element) place-holder)
  (setf (property obj "placeholder") place-holder))
(defsetf place-holder set-place-holder)

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
