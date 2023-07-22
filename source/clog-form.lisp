;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG - The Common Lisp Omnificent GUI                                 ;;;;
;;;; (c) 2020-2022 David Botton                                            ;;;;
;;;; License BSD 3 Clause                                                  ;;;;
;;;;                                                                       ;;;;
;;;; clog-form.lisp                                                        ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:in-package :clog)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - form-data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric form-get-data (clog-obj)
  (:documentation "Get the form data as an a-list sent by the get method"))

(defmethod form-get-data ((obj clog-obj))
  (quri:uri-query-params
   (quri:uri (clog-connection:query (connection-id obj) "location.href"))))

(defgeneric form-post-data (clog-obj)
  (:documentation "Get the form data as an a-list sent by post method"))

(defmethod form-post-data ((obj clog-obj))
  (quri:url-decode-params
   (clog-connection:query (connection-id obj) "clog['post-data']")))

(defgeneric form-multipart-data (clog-obj)
  (:documentation "Get the form data as an a-list sent with the multipart
method used in file uploads. DELETE-MULTIPART-DATA must be called or will
never be GC'd. File upload items will be a four part list
(name stream file-name content-type)."))

(defmethod form-multipart-data ((obj clog-obj))
  (clog-connection:get-connection-data
   (caar (form-post-data obj))))

(defgeneric delete-multipart-data (clog-obj)
  (:documentation "Delete the multipart data upload"))

(defmethod delete-multipart-data ((obj clog-obj))
  (let ((id (caar (form-post-data obj))))
    (clog-connection:delete-connection-data id)))

(defun form-data-item (form-data item)
  "Return value for ITEM from FROM-DATA a-list"
  (cdr (assoc item form-data :test #'equalp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-form
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-form (clog-element)()
  (:documentation "CLOG Form Objecs is the base class for all html forms."))

;;;;;;;;;;;;;;;;;
;; create-form ;;
;;;;;;;;;;;;;;;;;

(deftype form-method-type () '(members :get :post :none))

(defgeneric create-form (clog-obj
                         &key action method target encoding
                           style hidden class html-id auto-place)
  (:documentation "Create a new CLOG-Form as child of CLOG-OBJ that organizes
a collection of form elements in to a single form if :AUTO-PLACE (default t)
place-inside-bottom-of CLOG-OBJ. In CLOG a form's on-submit handler should be
set and the form element values handled in that handler as opposed to the
HTML model of submitting to a new \"page\". If though one wishes to submit to
another page can use the :ACTION :METHOD and :TARGET keys and either do not
set an on-submit handler or call (submit CLOG-FORM) to perform the form
action. The default :ENCODING is application/x-www-form-urlencoded if
doing file upload use multipart/form-data"))

(defmethod create-form ((obj clog-obj)
                        &key (action "#")
                          (method :none)
                          (target "_self")
                          (encoding "application/x-www-form-urlencoded")
                          (style nil)
                          (hidden nil)
                          (class nil)
                          (html-id nil)
                          (auto-place t))
  (create-child obj
                (format nil "<form action='~A' ~A enctype='~A' target='~A'~@[~A~]~@[~A~]/>"
                        action
                        (if (eq method :none)
                            "onSubmit='return false;'"
                            (format nil "method='~A'" method))
                        encoding
                        target
                        (when class
                          (format nil " class='~A'"
                                  (escape-string class :html t)))
                        (when (or hidden style)
                          (format nil " style='~@[~a~]~@[~a~]'"
                                  (when hidden "visibility:hidden;")
                                  style)))
                :clog-type  'clog-form
                :html-id    html-id
                :auto-place auto-place))

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

(defgeneric (setf autocompletep) (autocompletep clog-form)
  (:documentation "Set autocompletep for CLOG-FORM"))

(defmethod (setf autocompletep) (autocompletep (obj clog-form))
  (if autocompletep
      (setf (attribute obj "autocomplete") "true")
      (remove-attribute obj "autocomplete")))

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

(defgeneric (setf encoding) (encoding clog-form)
  (:documentation "Set encoding for CLOG-FORM"))

(defmethod (setf encoding) (encoding (obj clog-form))
  (setf (property obj "encoding") encoding))

;;;;;;;;;;;;;;;;;;;;;;;;
;; validate-on-submit ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric validate-on-submit (clog-form)
  (:documentation "Get/Setf form validate-on-submit."))

(defmethod validate-on-submit ((obj clog-form))
  (not (js-true-p (property obj "noValidate"))))

(defgeneric (setf validate-on-submit) (value clog-form)
  (:documentation "Set VALIDATE-ON-SUBMIT for CLOG-FORM"))

(defmethod (setf validate-on-submit) (value (obj clog-form))
  (if (not value)
      (setf (attribute obj "noValidate") "true")
      (remove-attribute obj "noValidate")))

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

(defgeneric create-form-element (clog-obj element-type &rest args
                                 &key name class style hidden html-id auto-place label
                                   &allow-other-keys)
  (:documentation "Create a new clog-form-element as child of CLOG-OBJ.
It is importamt that clog-form-elements are a child or descendant of a
clog-form in the DOM. The radio ELEMENT-TYPE groups by
NAME. Additional keys will be added to the input tag as
attribute/value pairs in the form attr= 'value'"))

(defmethod create-form-element ((obj clog-obj) element-type &rest args
                                &key (name nil)
                                  (label nil)
                                  (class nil)
                                  (style nil)
                                  (hidden nil)
                                  (html-id nil)
                                  (auto-place t))
  (declare (ignorable name value min max))
  (dolist (key '(name class style hidden html-id auto-place))
    (remf args key))
  (let* (
         (element (create-child
                   obj (format nil "<input type='~A'~@[~A~]~@[~A~]~{~(~A~)= '~A'~^ ~}/>"
                               (escape-string element-type :html t)
                               (when class
                                 (format nil " class='~A'"
                                         (escape-string class :html t)))
                               (when (or hidden style)
                                 (format nil " style='~@[~a~]~@[~a~]'"
                                         (when hidden "visibility:hidden;")
                                         style))
                               args)
                   :clog-type  'clog-form-element
                   :html-id    html-id
                   :auto-place auto-place)))
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

(defgeneric (setf autocomplete) (value clog-form-element)
  (:documentation "Set autocomplete AUTOCOMPLETE for CLOG-FORM-ELEMENT"))

(defmethod (setf autocomplete) (value (obj clog-form-element))
  (if value
      (setf (attribute obj "autocomplete") "true")
      (remove-attribute obj "autocomplete")))

;;;;;;;;;;;;;;;;
;; autofocusp ;;
;;;;;;;;;;;;;;;;

(defgeneric autofocusp (clog-form-element)
  (:documentation "Get/Setf form element autofocusp. Only one element should
have this set true ever. Autofocus on element when form loaded."))

(defmethod autofocusp ((obj clog-form-element))
  (js-true-p (attribute obj "autofocus")))

(defgeneric (setf autofocusp) (value clog-form-element)
  (:documentation "Set autofocusp AUTOFOCUSP for CLOG-FORM-ELEMENT"))

(defmethod (setf autofocusp) (value (obj clog-form-element))
  (if value
      (setf (attribute obj "autofocus") "true")
      (remove-attribute obj "autofocus")))

;;;;;;;;;;;;;;;;;;
;; place-holder ;;
;;;;;;;;;;;;;;;;;;

(defgeneric place-holder (clog-form-element)
  (:documentation "Get/Setf form element place holder."))

(defmethod place-holder ((obj clog-form-element))
  (property obj "placeholder"))

(defgeneric (setf place-holder) (value clog-form-element)
  (:documentation "Set placeholder PLACE-HOLDER for CLOG-FORM-ELEMENT"))

(defmethod (setf place-holder) (value (obj clog-form-element))
  (setf (property obj "placeholder") value))

;;;;;;;;;;;;;;;
;; disabledp ;;
;;;;;;;;;;;;;;;

(defgeneric disabledp (clog-form-element)
  (:documentation "Get/Setf form element disabledp."))

(defmethod disabledp ((obj clog-form-element))
  (js-true-p (property obj "disabled")))

(defgeneric (setf disabledp) (value clog-form-element)
  (:documentation "Set disabledp DISABLEDP for CLOG-FORM-ELEMENT"))

(defmethod (setf disabledp) (value (obj clog-form-element))
  (if value
      (setf (attribute obj "disabled") "true")
      (remove-attribute obj "disabled")))

;;;;;;;;;;;;;;;;;
;; read-only-p ;;
;;;;;;;;;;;;;;;;;

(defgeneric read-only-p (clog-form-element)
  (:documentation "Get/Setf form element read-only-p."))

(defmethod read-only-p ((obj clog-form-element))
  (js-true-p (property obj "readonly")))

(defgeneric (setf read-only-p) (value clog-form-element)
  (:documentation "Set read-only-p READ-ONLY-P for CLOG-FORM-ELEMENT"))

(defmethod (setf read-only-p) (value (obj clog-form-element))
  (if value
      (setf (attribute obj "readonly") "true")
      (remove-attribute obj "readonly")))

;;;;;;;;;;;;;;;
;; requiredp ;;
;;;;;;;;;;;;;;;

(defgeneric requiredp (clog-form-element)
  (:documentation "Get/Setf form element requiredp."))

(defmethod requiredp ((obj clog-form-element))
  (js-true-p (property obj "required")))

(defgeneric (setf requiredp) (value clog-form-element)
  (:documentation "Set requiredp REQUIREDP for CLOG-FORM-ELEMENT"))

(defmethod (setf requiredp) (value (obj clog-form-element))
  (if value
      (setf (attribute obj "required") "true")
      (remove-attribute obj "required")))

;;;;;;;;;;;;;;;
;; multiplep ;;
;;;;;;;;;;;;;;;

(defgeneric multiplep (clog-form-element)
  (:documentation "Get/Setf form element multiplep."))

(defmethod multiplep ((obj clog-form-element))
  (js-true-p (property obj "multiple")))

(defgeneric (setf multiplep) (value clog-form-element)
  (:documentation "Set multiplep MULTIPLEP for CLOG-FORM-ELEMENT"))

(defmethod (setf multiplep) (value (obj clog-form-element))
  (if value
      (setf (attribute obj "multiple") "true")
      (remove-attribute obj "multiple")))

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

(defgeneric (setf name) (value clog-form-element)
  (:documentation "Set name VALUE for CLOG-FORM-ELEMENT"))

(defmethod (setf name) (value (obj clog-form-element))
  (setf (property obj "name") value))

;;;;;;;;;;;;;;;;;;;
;; default-value ;;
;;;;;;;;;;;;;;;;;;;

(defgeneric default-value (clog-form-element)
  (:documentation "Get/Setf form element default-value.
   If the form is reset the value will be set to default value
   If Value is set at time of creation it also sets it as the Default_Value"))

(defmethod default-value ((obj clog-form-element))
  (property obj "defaultValue"))

(defgeneric (setf default-value) (value clog-form-element)
  (:documentation "Set default-value VALUE for CLOG-FORM-ELEMENT"))

(defmethod (setf default-value) (value (obj clog-form-element))
  (setf (property obj "defaultValue") value))

;;;;;;;;;;;
;; value ;;
;;;;;;;;;;;

(defgeneric value (clog-form-element)
  (:documentation "Get/Setf form element value.
   Form element values are not accessible through the Text property but
   instead through the value property."))

(defmethod value ((obj clog-form-element))
  (property obj "value"))

(defgeneric (setf value) (value clog-form-element)
  (:documentation "Set value VALUE for CLOG-FORM-ELEMENT"))

(defmethod (setf value) (value (obj clog-form-element))
  (setf (property obj "value") value))

;; overloading of text-value to equal value on forms

(defmethod text-value ((obj clog-form-element))
  (property obj "value"))

(defmethod (setf text-value) (value (obj clog-form-element))
  (setf (property obj "value") value))

;;;;;;;;;;;;;;;;;
;; radio-value ;;
;;;;;;;;;;;;;;;;;

(defgeneric radio-value (clog-obj name)
  (:documentation "Returns the value of the selected radio button in a
group called NAME."))

(defmethod radio-value ((obj clog-obj) name)
  (clog-connection:query (connection-id obj)
            (format nil "$('input:radio[name=~A]:checked').val()"
                    name)))

;;;;;;;;;;;;;;;;;;;;
;; checkbox-value ;;
;;;;;;;;;;;;;;;;;;;;

(defgeneric checkbox-value (clog-obj name)
  (:documentation "Returns t or nil on the selected checkbox button."))

(defmethod checkbox-value ((obj clog-obj) name)
  (js-on-p (clog-connection:query (connection-id obj)
                     (format nil "$('input:checkbox[name=~A]:checked').val()"
                             name))))

;;;;;;;;;;;;;;;;;;
;; select-value ;;
;;;;;;;;;;;;;;;;;;

(defgeneric select-value (clog-obj name)
  (:documentation "Returns the value of select item called NAME and must
be unique name on entire document."))

(defmethod select-value ((obj clog-obj) name)
  (clog-connection:query (connection-id obj)
            (format nil "$('select[name=~A] option:selected').val()" name)))

;;;;;;;;;;;;;;;;
;; name-value ;;
;;;;;;;;;;;;;;;;

(defgeneric name-value (clog-obj name)
  (:documentation "Returns the value of input item called NAME and must
be unique name on entire document."))

(defmethod name-value ((obj clog-obj) name)
  (clog-connection:query (connection-id obj)
            (format nil "$('input[name=~A]').val()" name)))

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

(defgeneric (setf pattern) (value clog-form-element)
  (:documentation "Set pattern VALUE for CLOG-FORM-ELEMENT"))

(defmethod (setf pattern) (value (obj clog-form-element))
  (setf (property obj "pattern") value))

;;;;;;;;;;;;;
;; minimum ;;
;;;;;;;;;;;;;

(defgeneric minimum (clog-form-element)
  (:documentation "Get/Setf form element minimum."))

(defmethod minimum ((obj clog-form-element))
  (property obj "min"))

(defgeneric (setf minimum) (value clog-form-element)
  (:documentation "Set minimum VALUE for CLOG-FORM-ELEMENT"))

(defmethod (setf minimum) (value (obj clog-form-element))
  (setf (property obj "min") value))

;;;;;;;;;;;;;
;; maximum ;;
;;;;;;;;;;;;;

(defgeneric maximum (clog-form-element)
  (:documentation "Get/Setf form element maximum."))

(defmethod maximum ((obj clog-form-element))
  (property obj "max"))

(defgeneric (setf maximum) (value clog-form-element)
  (:documentation "Set maximum VALUE for CLOG-FORM-ELEMENT"))

(defmethod (setf maximum) (value (obj clog-form-element))
  (setf (property obj "max") value))

;;;;;;;;;;;;;;;;;;
;; element-step ;;
;;;;;;;;;;;;;;;;;;

(defgeneric element-step (clog-form-element)
  (:documentation "Get/Setf form element step."))

(defmethod element-step ((obj clog-form-element))
  (property obj "step"))

(defgeneric (setf element-step) (value clog-form-element)
  (:documentation "Set element-step VALUE for CLOG-FORM-ELEMENT"))

(defmethod (setf element-step) (value (obj clog-form-element))
  (setf (property obj "step") value))

;;;;;;;;;;
;; size ;;
;;;;;;;;;;

(defgeneric size (clog-form-element)
  (:documentation "Get/Setf form element size."))

(defmethod size ((obj clog-form-element))
  (property obj "size"))

(defgeneric (setf size) (value clog-form-element)
  (:documentation "Set size VALUE for CLOG-FORM-ELEMENT"))

(defmethod (setf size) (value (obj clog-form-element))
  (setf (property obj "size") value))

;;;;;;;;;;;;;;;;;;;;
;; minimum-length ;;
;;;;;;;;;;;;;;;;;;;;

(defgeneric minimum-length (clog-form-element)
  (:documentation "Get/Setf form element minimum-length."))

(defmethod minimum-length ((obj clog-form-element))
  (property obj "minlength"))

(defgeneric (setf minimum-length) (value clog-form-element)
  (:documentation "Set minimum-length VALUE for CLOG-FORM-ELEMENT"))

(defmethod (setf minimum-length) (value (obj clog-form-element))
  (setf (property obj "minlength") value))

;;;;;;;;;;;;;;;;;;;;
;; maximum-length ;;
;;;;;;;;;;;;;;;;;;;;

(defgeneric maximum-length (clog-form-element)
  (:documentation "Get/Setf form element maximum-length."))

(defmethod maximum-length ((obj clog-form-element))
  (property obj "maxlength"))

(defgeneric (setf maximum-length) (value clog-form-element)
  (:documentation "Set maximum-length VALUE for CLOG-FORM-ELEMENT"))

(defmethod (setf maximum-length) (value (obj clog-form-element))
  (setf (property obj "maxlength") value))

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

(defgeneric (setf file-accept) (value clog-form-element)
  (:documentation "Set file-accept VALUE for CLOG-FORM-ELEMENT"))

(defmethod (setf file-accept) (value (obj clog-form-element))
  (setf (attribute obj "accept") value))

;;;;;;;;;;;;;
;; url-src ;;
;;;;;;;;;;;;;

(defgeneric url-src (clog-form-element)
  (:documentation "Get/Setf the url-src of the img."))

(defmethod url-src ((obj clog-form-element))
  (property obj "src"))

(defgeneric (setf url-src) (value clog-form-element)
  (:documentation "Set url-src VALUE for CLOG-FORM-ELEMENT"))

(defmethod (setf url-src) (value (obj clog-form-element))
  (setf (property obj "src") value))

;;;;;;;;;;;;;;
;; alt-text ;;
;;;;;;;;;;;;;;

(defgeneric alt-text (clog-form-element)
  (:documentation "Get/Setf the alt-text of the img."))

(defmethod alt-text ((obj clog-form-element))
  (attribute obj "alt"))

(defgeneric (setf alt-text) (value clog-form-element)
  (:documentation "Set alt-text VALUE for CLOG-FORM-ELEMENT"))

(defmethod (setf alt-text) (value (obj clog-form-element))
  (setf (attribute obj "alt") value))

;;;;;;;;;;;;;;
;; checkedp ;;
;;;;;;;;;;;;;;

(defgeneric checkedp (clog-form-element)
  (:documentation "Get/Setf form element checkedp."))

(defmethod checkedp ((obj clog-form-element))
  (js-true-p (property obj "checked")))

(defgeneric (setf checkedp) (value clog-form-element)
  (:documentation "Set VALUE if checkedp for CLOG-FORM-ELEMENT"))

(defmethod (setf checkedp) (value (obj clog-form-element))
  (if value
      (setf (attribute obj "checked") "true")
      (remove-attribute obj "checked")))

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

(defgeneric (setf input-mode) (value clog-form-element)
  (:documentation "Set input-mode VALUE for CLOG-FORM-ELEMENT"))

(defmethod (setf input-mode) (value (obj clog-form-element))
  (setf (attribute obj "inputmode") value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-label
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-label (clog-element)()
  (:documentation "CLOG Form Element Label Object"));

;;;;;;;;;;;;;;;;;;
;; create-label ;;
;;;;;;;;;;;;;;;;;;


(defgeneric create-label (clog-obj &key content
                                     style hidden label-for class html-id
                                     auto-place)
  (:documentation "Create a new clog-label as child of CLOG-OBJ."))

(defmethod create-label ((obj clog-obj) &key (content "")
                                          (label-for nil)
                                          (style nil)
                                          (hidden nil)
                                          (class nil)
                                          (html-id nil)
                                          (auto-place t))
  (create-child obj (format nil "<label for='~@[~A~]'~@[~A~]~@[~A~]>~A</label>"
                            (when label-for (html-id label-for))
                            (when class
                              (format nil " class='~A'"
                                      (escape-string class :html t)))
                            (when (or hidden style)
                              (format nil " style='~@[~a~]~@[~a~]'"
                                      (when hidden "visibility:hidden;")
                                      style))
                            content)
                :clog-type  'clog-label
                :html-id    html-id
                :auto-place auto-place))

;;;;;;;;;;;;;;;
;; label-for ;;
;;;;;;;;;;;;;;;

(defgeneric label-for (clog-label element)
  (:documentation "Set label is for ELEMENT."))

(defmethod label-for ((obj clog-label) element)
  (setf (attribute obj "for") (html-id element)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-fieldset
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-fieldset (clog-element)()
  (:documentation "CLOG Form Element Fieldset Object"));

;;;;;;;;;;;;;;;;;;;;;
;; create-fieldset ;;
;;;;;;;;;;;;;;;;;;;;;

(defgeneric create-fieldset (clog-obj &key legend hidden
                                        style class html-id auto-place)
  (:documentation "Create a new clog-fieldset as child of CLOG-OBJ."))

(defmethod create-fieldset ((obj clog-obj) &key (legend nil)
                                             (style nil)
                                             (hidden nil)
                                             (class nil)
                                             (html-id nil)
                                             (auto-place t))
  (create-child obj (format nil "<fieldset~@[~A~]~@[~A~]>~@[~A~]</fieldset>"
                              (when class
                                (format nil " class='~A'"
                                        (escape-string class :html t)))
                              (when (or hidden style)
                                (format nil " style='~@[~a~]~@[~a~]'"
                                        (when hidden "visibility:hidden;")
                                        style))
                              (when legend
                                (format nil "<legend>~A</legend>" legend)))
                :clog-type  'clog-fieldset
                :html-id    html-id
                :auto-place auto-place))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-text-area
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-text-area (clog-form-element)()
  (:documentation "CLOG Form Element Text-Area Object"));

;;;;;;;;;;;;;;;;;;;;;;
;; create-text-area ;;
;;;;;;;;;;;;;;;;;;;;;;

(defgeneric create-text-area (clog-obj
                              &key columns rows name value label
                                style hidden class html-id auto-place)
  (:documentation "Create a new clog-text-area as child of CLOG-OBJ."))

(defmethod create-text-area ((obj clog-obj)
                             &key (columns 20)
                               (rows 2)
                               (name "")
                               (value "")
                               (label nil)
                               (style nil)
                               (hidden nil)
                               (class nil)
                               (html-id nil)
                               (auto-place t))
  (let ((element
          (create-child obj
            (format nil "<textarea name='~A' cols='~A' rows='~A'~@[~A~]~@[~A~]>~A</textarea>"
                    name columns rows
                    (when class
                      (format nil " class='~A'"
                              (escape-string class :html t)))
                    (when (or hidden style)
                      (format nil " style='~@[~a~]~@[~a~]'"
                              (when hidden "visibility:hidden;")
                              style))
                    (escape-string value :html t))
            :clog-type  'clog-text-area
            :html-id    html-id
            :auto-place auto-place)))

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

(defgeneric (setf word-wrap) (value clog-text-area)
  (:documentation "Set word-wrap WORD-WRAP for CLOG-TEXT-AREA"))

(defmethod (setf word-wrap) (value (obj clog-text-area))
  (setf (property obj "wrap") value))

;;;;;;;;;;;;;;;
;; word-wrap ;;
;;;;;;;;;;;;;;;

(defgeneric word-wrap (clog-text-area)
  (:documentation "Get/Setf form element word-wrap."))

(defmethod word-wrap ((obj clog-text-area))
  (property obj "wrap"))

(defgeneric (setf word-wrap) (value clog-text-area)
  (:documentation "Set word-wrap WORD-WRAP for CLOG-TEXT-AREA"))

(defmethod (setf word-wrap) (value (obj clog-text-area))
  (setf (property obj "wrap") value))

;;;;;;;;;;;;;
;; columns ;;
;;;;;;;;;;;;;

(defgeneric columns (clog-text-area)
  (:documentation "Get/Setf form element columns."))

(defmethod columns ((obj clog-text-area))
  (property obj "cols"))

(defgeneric (setf columns) (value clog-text-area)
  (:documentation "Set columns COLUMNS for CLOG-TEXT-AREA"))

(defmethod (setf columns) (value (obj clog-text-area))
  (setf (property obj "cols") value))

;;;;;;;;;;
;; rows ;;
;;;;;;;;;;

(defgeneric rows (clog-text-area)
  (:documentation "Get/Setf form element rows."))

(defmethod rows ((obj clog-text-area))
  (property obj "rows"))

(defgeneric (setf rows) (value clog-text-area)
  (:documentation "Set rows ROWS for CLOG-TEXT-AREA"))

(defmethod (setf rows) (value (obj clog-text-area))
  (setf (property obj "rows") value))

;;;;;;;;;;;;;;;;;;;;
;; disable-resize ;;
;;;;;;;;;;;;;;;;;;;;

(defgeneric disable-resize (clog-text-area)
  (:documentation "Disable resize of text area."))

(defmethod disable-resize ((obj clog-text-area))
  (setf (resizable obj) :none))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-legend
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-legend (clog-element)()
  (:documentation "CLOG Fieldset Legend Object"));

;;;;;;;;;;;;;;;;;;;
;; create-legend ;;
;;;;;;;;;;;;;;;;;;;

(defgeneric create-legend (clog-obj &key content
                                      style hidden class html-id auto-place)
  (:documentation "Create a new clog-legend as child of CLOG-OBJ."))

(defmethod create-legend ((obj clog-obj) &key (content "")
                                           (style nil)
                                           (hidden nil)
                                           (class nil)
                                           (html-id nil)
                                           (auto-place t))
  (create-child obj (format nil "<legend~@[~A~]~@[~A~]>~A</legend>"
                            (when class
                                (format nil " class='~A'"
                                        (escape-string class :html t)))
                            (when (or hidden style)
                              (format nil " style='~@[~a~]~@[~a~]'"
                                      (when hidden "visibility:hidden;")
                                      style))
                            content)
                :clog-type  'clog-legend
                :html-id    html-id
                :auto-place auto-place))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-data-list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-data-list (clog-element)()
  (:documentation "CLOG Form Element Data-List Options Object"));

;;;;;;;;;;;;;;;;;;;;;;
;; create-data-list ;;
;;;;;;;;;;;;;;;;;;;;;;

(defgeneric create-data-list (clog-obj &key data-list html-id auto-place)
  (:documentation "Create a new clog-data-list as child of CLOG-OBJ and
optionally fill in with contents of data-list."))

(defmethod create-data-list ((obj clog-obj) &key (data-list nil)
                                              (html-id nil) (auto-place t))
  (let ((element (create-child obj "<datalist />"
                               :clog-type  'clog-data-list
                               :html-id    html-id
                               :auto-place auto-place)))
    (when data-list
      (add-options element data-list))
    element))

;;;;;;;;;;;;;;;;
;; add-option ;;
;;;;;;;;;;;;;;;;

(defgeneric add-option (clog-data-list value)
  (:documentation "Add option VALUE to data-list."))

(defmethod add-option ((obj clog-data-list) value)
  (create-child obj (format nil "<option value='~A'>" (escape-string value :html t))
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
;; Implementation - clog-select
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-select (clog-form-element)()
  (:documentation "CLOG Form Element Select Options Object"));

;;;;;;;;;;;;;;;;;;;
;; create-select ;;
;;;;;;;;;;;;;;;;;;;

(defgeneric create-select (clog-obj &key name multiple label
                                      style hidden class html-id auto-place)
  (:documentation "Create a new clog-select as child of CLOG-OBJ."))

(defmethod create-select ((obj clog-obj)
                          &key (name nil)
                            (multiple nil)
                            (label nil)
                            (style nil)
                            (hidden nil)
                            (class nil)
                            (html-id nil)
                            (auto-place t))
  (let ((element (create-child
                  obj (format nil "<select~@[~A~]~@[~A~]~@[~A~]~@[~A~]/>"
                              (when multiple " multiple")
                              (when name (format nil " name='~A'" name))
                              (when class
                                (format nil " class='~A'"
                                        (escape-string class :html t)))
                              (when (or hidden style)
                                (format nil " style='~@[~a~]~@[~a~]'"
                                        (when hidden "visibility:hidden;")
                                        style)))
                  :clog-type  'clog-select
                  :html-id    html-id
                  :auto-place auto-place)))
    (when label
      (label-for label element))
    element))

;;;;;;;;;;;;;;;;;;;;;;;
;; add-select-option ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric add-select-option (clog-select value content &key selected disabled)
  (:documentation "Add option VALUE to select."))

(defmethod add-select-option ((obj clog-select) value content &key selected disabled)
  (create-child obj (format nil "<option~@[~A~]~@[~A~] value='~A'>~A</option>"
                            (when selected " selected")
                            (when disabled " disabled")
                            (escape-string value :html t)
                            content)
                :clog-type 'clog-element :auto-place t))

;;;;;;;;;;;;;;;;;;;;;;;;
;; add-select-options ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric add-select-options (clog-select content)
  (:documentation "Add group of options to select."))

(defmethod add-select-options ((obj clog-select) content)
  (dolist (value content)
    (add-select-option obj value value)))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; add-select-optgroup ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric add-select-optgroup (clog-select content)
  (:documentation "Add option VALUE to select."))

(defmethod add-select-optgroup ((obj clog-select) content)
  (create-child obj (format nil "<optgroup label='~A'/>"
                            content)
                :clog-type 'clog-element :auto-place t))

;;;;;;;;;;;;;;;;;
;; select-text ;;
;;;;;;;;;;;;;;;;;

(defgeneric select-text (clog-obj)
  (:documentation "Get/Setf the text of selected item."))

(defmethod select-text ((obj clog-obj))
  (js-query obj (format nil "$('#~A option:selected').text()" (html-id obj))))

(defgeneric (setf select-text) (value clog-obj)
  (:documentation "Setf the text of selected item."))

(defmethod (setf select-text) (value (obj clog-obj))
  (js-execute obj (format nil "$('#~A option:selected').text('~A')"
                          (html-id obj)
                          (escape-string value))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-option
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-option (clog-form-element)()
  (:documentation "CLOG Form Element Option for CLOG Select Object
or CLOG Data-List objects."));

;;;;;;;;;;;;;;;;;;;
;; create-option ;;
;;;;;;;;;;;;;;;;;;;

(defgeneric create-option (clog-obj
                           &key content value selected disabled
                             style hidden class html-id auto-place)
  (:documentation "Create a new clog-option as child of CLOG-OBJ."))

(defmethod create-option ((obj clog-obj) &key
                                           (content "")
                                           (value nil)
                                           (selected nil)
                                           (disabled nil)
                                           (style nil)
                                           (hidden nil)
                                           (class nil)
                                           (html-id nil)
                                           (auto-place t))
  (create-child obj (format nil "<option~@[~A~]~@[~A~]~@[~A~]~@[~A~]~@[~A~]>~A</option>"
                            (when selected " selected")
                            (when disabled " disabled")
                            (when value (format nil " value='~A'"
                                                (escape-string value :html t)))
                            (when class
                              (format nil " class='~A'"
                                      (escape-string class :html t)))
                            (when (or hidden style)
                              (format nil " style='~@[~a~]~@[~a~]'"
                                      (when hidden "visibility:hidden;")
                                      style))
                            content)
                :clog-type  'clog-option
                :html-id    html-id
                :auto-place auto-place))

;;;;;;;;;;;;;;;
;; selectedp ;;
;;;;;;;;;;;;;;;

(defgeneric selectedp (clog-form-element)
  (:documentation "Get/Setf form element selectedp."))

(defmethod selectedp ((obj clog-form-element))
  (js-true-p (property obj "selected")))

(defgeneric (setf selectedp) (value clog-form-element)
  (:documentation "Set selectedp VALUE for CLOG-FORM-ELEMENT"))

(defmethod (setf selectedp) (value (obj clog-form-element))
  (setf (property obj "selected") (p-true-js value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-optgroup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-optgroup (clog-form-element)()
  (:documentation "CLOG Form Element Optgroup for CLOG Select Object"));

;;;;;;;;;;;;;;;;;;;;;
;; create-optgroup ;;
;;;;;;;;;;;;;;;;;;;;;

(defgeneric create-optgroup (clog-obj &key content disabled
                                        style hidden class html-id auto-place)
  (:documentation "Create a new clog-optgroup as child of CLOG-OBJ."))

(defmethod create-optgroup ((obj clog-obj) &key (content "")
                                             (disabled nil)
                                             (style nil)
                                             (hidden nil)
                                             (class nil)
                                             (html-id nil)
                                             (auto-place t))
  (create-child obj (format nil "<optgroup label='~A'~@[~A~]~@[~A~]~@[~A~]/>"
                            content
                            (when class
                              (format nil " class='~A'"
                                      (escape-string class :html t)))
                            (when (or hidden style)
                              (format nil " style='~@[~a~]~@[~a~]'"
                                      (when hidden "visibility:hidden;")
                                      style))
                            (when disabled " disabled"))
                :clog-type  'clog-optgroup
                :html-id    html-id
                :auto-place auto-place))
