;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG - The Common Lisp Omnificent GUI                                 ;;;;
;;;; (c) 2020-2021 David Botton                                            ;;;;
;;;; License BSD 3 Clause                                                  ;;;;
;;;;                                                                       ;;;;
;;;; clog-element.lisp                                                     ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:in-package :clog)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-element
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-element (clog-obj)()
  (:documentation "CLOG Element Objects is the base class for all html
element objects."))

;;;;;;;;;;;;;;;;;;;;;;;
;; make-clog-element ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defun make-clog-element (connection-id html-id)
  "Construct a new clog-element. (Private)"
  (make-instance 'clog-element :connection-id connection-id :html-id html-id))

;;;;;;;;;;;;;;;;;;;;;;
;; create-with-html ;;
;;;;;;;;;;;;;;;;;;;;;;

(defun create-with-html (connection-id html)
  "Create a new clog-element and attach it to HTML on CONNECTION-ID. There must be
a single outer block that will be set to an internal id. The returned CLOG-Element
requires placement or will not be visible, ie. place-after, etc. (private)"
  (let ((web-id (cc:generate-id)))
    (cc:execute
     connection-id
     (format nil "clog['~A']=$(\"~A\"); clog['~A'].first().prop('id','~A')"
	     web-id html web-id web-id))
    (make-clog-element connection-id web-id)))

;;;;;;;;;;;;
;; attach ;;
;;;;;;;;;;;;

(defun attach (connection-id html-id)
  "Create a new clog-obj and attach an existing element with HTML-ID on
CONNECTION-ID to it and then return it. The HTML-ID must be unique. (private)"
  (cc:execute connection-id (format nil "clog['~A']=$('#~A')" html-id html-id))
  (make-clog-element connection-id html-id))

;;;;;;;;;;;;;;;;;;
;; create-child ;;
;;;;;;;;;;;;;;;;;;

(defgeneric create-child (clog-obj html &key auto-place)
  (:documentation "Create a new CLOG-Element from HTML as child of CLOG-OBJ
and if :AUTO-PLACE (default t) place-inside-bottom-of CLOG-OBJ"))

(defmethod create-child ((obj clog-obj) html &key (auto-place t))
  (let ((child (create-with-html (connection-id obj) html)))
    (if auto-place
	(place-inside-bottom-of obj child)
	child)))

;;;;;;;;;;;;;;;;;;;;;
;; attach-as-child ;;
;;;;;;;;;;;;;;;;;;;;;

(defgeneric attach-as-child (clog-obj html-id)
  (:documentation "Create a new CLOG-Element and attach an existing element with HTML-ID. The
HTML-ID must be unique."))

(defmethod attach-as-child ((obj clog-obj) html-id)
  (cc:execute (connection-id obj) (format nil "clog['~A']=$('#~A')" html-id html-id))
  (make-clog-element (connection-id obj) html-id))

;;;;;;;;;;;
;; style ;;
;;;;;;;;;;;

(defgeneric style (clog-element style-name)
  (:documentation "Get/Setf css style."))

(defmethod style ((obj clog-element) style-name)
  (jquery-query obj (format nil "css('~A')" style-name)))

(defgeneric set-style (clog-element style-name value)
  (:documentation "Set css style."))

(defmethod set-style ((obj clog-element) style-name value)
  (jquery-execute obj (format nil "css('~A','~A')" style-name (escape-string value))))
(defsetf style set-style)

;;;;;;;;;;;;;;;
;; attribute ;;
;;;;;;;;;;;;;;;

(defgeneric attribute (clog-element attribute-name)
  (:documentation "Get/Setf html tag attribute. (eg. src on img tag)"))

(defmethod attribute ((obj clog-element) attribute-name)
  (jquery-query obj (format nil "attr('~A')" attribute-name)))

(defgeneric set-attribute (clog-element attribute-name value)
  (:documentation "Set html tag attribute."))

(defmethod set-attribute ((obj clog-element) attribute-name value)
  (jquery-execute obj (format nil "attr('~A','~A')" attribute-name (escape-string value))))
(defsetf attribute set-attribute)

;;;;;;;;;;;;;;;;;
;; place-after ;;
;;;;;;;;;;;;;;;;;

(defgeneric place-after (clog-element next-obj)
  (:documentation "Places NEXT-OBJ after CLOG-ELEMENT in DOM"))

(defmethod place-after ((obj clog-element) next-obj)
  (jquery-execute obj (format nil "after(~A)" (script-id next-obj)))
  next-obj)

;;;;;;;;;;;;;;;;;;
;; place-before ;;
;;;;;;;;;;;;;;;;;;

(defgeneric place-before (clog-element next-obj)
  (:documentation "Places NEXT-OBJ before CLOG-ELEMENT in DOM"))

(defmethod place-before ((obj clog-element) next-obj)
  (jquery-execute obj (format nil "before(~A)" (script-id next-obj)))
  next-obj)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; place-inside-top-of ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric place-inside-top-of (clog-element next-obj)
  (:documentation "Places NEXT-OBJ inside top of CLOG-ELEMENT in DOM"))

(defmethod place-inside-top-of ((obj clog-element) next-obj)
  (jquery-execute obj (format nil "prepend(~A)" (script-id next-obj)))
  next-obj)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; place-inside-bottom-of ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric place-inside-bottom-of (clog-element next-obj)
  (:documentation "Places NEXT-OBJ inside bottom of CLOG-ELEMENT in DOM"))

(defmethod place-inside-bottom-of ((obj clog-element) next-obj)
  (jquery-execute obj (format nil "append(~A)" (script-id next-obj)))
  next-obj)

