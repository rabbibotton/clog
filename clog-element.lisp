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
requires placement or will not be visible, ie. place-after, etc. as it exists in
the javascript clog[] but is not in the DOM. (private)"
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

(defgeneric place-after (clog-obj next-obj)
  (:documentation "Places NEXT-OBJ after CLOG-OBJ in DOM"))

(defmethod place-after ((obj clog-obj) next-obj)
  (jquery-execute obj (format nil "after(~A)" (script-id next-obj)))
  next-obj)

;;;;;;;;;;;;;;;;;;
;; place-before ;;
;;;;;;;;;;;;;;;;;;

(defgeneric place-before (clog-obj next-obj)
  (:documentation "Places NEXT-OBJ before CLOG-OBJ in DOM"))

(defmethod place-before ((obj clog-obj) next-obj)
  (jquery-execute obj (format nil "before(~A)" (script-id next-obj)))
  next-obj)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; place-inside-top-of ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric place-inside-top-of (clog-obj next-obj)
  (:documentation "Places NEXT-OBJ inside top of CLOG-OBJ in DOM"))

(defmethod place-inside-top-of ((obj clog-obj) next-obj)
  (jquery-execute obj (format nil "prepend(~A)" (script-id next-obj)))
  next-obj)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; place-inside-bottom-of ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric place-inside-bottom-of (clog-obj next-obj)
  (:documentation "Places NEXT-OBJ inside bottom of CLOG-OBJ in DOM"))

(defmethod place-inside-bottom-of ((obj clog-obj) next-obj)
  (jquery-execute obj (format nil "append(~A)" (script-id next-obj)))
  next-obj)

;;;;;;;;;;;;;;;;
;; access-key ;;
;;;;;;;;;;;;;;;;

(defgeneric access-key (clog-element)
  (:documentation "Get/Setf access-key."))

(defmethod access-key ((obj clog-element))
  (property obj "accessKey"))

(defgeneric set-access-key (clog-element value)
  (:documentation "Set access-key VALUE for CLOG-ELEMENT"))

(defmethod set-access-key ((obj clog-element) value)
  (setf (property obj "accessKey") value))
(defsetf access-key set-access-key)

;;;;;;;;;;;;;;;;;;;;
;; advisory-title ;;
;;;;;;;;;;;;;;;;;;;;

(defgeneric advisory-title (clog-element)
  (:documentation "Get/Setf advisory-title."))

(defmethod advisory-title ((obj clog-element))
  (property obj "title"))

(defgeneric set-advisory-title (clog-element value)
  (:documentation "Set advisory-title VALUE for CLOG-ELEMENT"))

(defmethod set-advisory-title ((obj clog-element) value)
  (setf (property obj "title") value))
(defsetf advisory-title set-advisory-title)

;;;;;;;;;;;;;;;;
;; class-name ;;
;;;;;;;;;;;;;;;;

(defgeneric class-name (clog-element)
  (:documentation "Get/Setf class-name."))

(defmethod class-name ((obj clog-element))
  (property obj "className"))

(defgeneric set-class-name (clog-element value)
  (:documentation "Set class-name VALUE for CLOG-ELEMENT"))

(defmethod set-class-name ((obj clog-element) value)
  (setf (property obj "className") value))
(defsetf class-name set-class-name)

;;;;;;;;;;;;;;;
;; editablep ;;
;;;;;;;;;;;;;;;

(defgeneric editablep (clog-element)
  (:documentation "Get/Setf editable."))

(defmethod editablep ((obj clog-element))
  (js-true-p (property obj "isContentEditable")))

(defgeneric set-editablep (clog-element value)
  (:documentation "Set editable VALUE for CLOG-ELEMENT"))

(defmethod set-editablep ((obj clog-element) value)
  (if value
      (setf (property obj "contentEditable") "true")
      (setf (property obj "contentEditable") "false")))
(defsetf editablep set-editable)

;;;;;;;;;;;;;;;;
;; box-sizing ;;
;;;;;;;;;;;;;;;;

(deftype box-sizing-type () '(member :content-box :border-box))

(defgeneric box-sizing (clog-element)
  (:documentation "Get/Setf box-sizing."))

(defmethod box-sizing ((obj clog-element))
  (style obj "box-sizing"))

(defgeneric set-box-sizing (clog-element value)
  (:documentation "Set box-sizing VALUE for CLOG-ELEMENT"))

(defmethod set-box-sizing ((obj clog-element) value)
  (setf (style obj "box-sizing") (string value)))
(defsetf box-sizing set-box-sizing)

;;;;;;;;;;;;;;;;
;; clear-side ;;
;;;;;;;;;;;;;;;;

(deftype clear-side-type ()
  '(member :none :left :right :both :inline-start :inline-end))

(defgeneric clear-side (clog-element)
  (:documentation "Get/Setf clear-side."))

(defmethod clear-side ((obj clog-element))
  (style obj "clear"))

(defgeneric set-clear-side (clog-element value)
  (:documentation "Set clear-side VALUE for CLOG-ELEMENT"))

(defmethod set-clear-side ((obj clog-element) value)
  (setf (style obj "clear") (string value)))
(defsetf clear-side set-clear-side)

;;;;;;;;;;;;;;;;
;; float-wrap ;;
;;;;;;;;;;;;;;;;

(deftype float-wrap-type ()
    '(member :none :left :right :inline-start :inline-end))

(defgeneric float-wrap (clog-element)
  (:documentation "Get/Setf for element float left or right and other
elements wrap around it."))

(defmethod float-wrap ((obj clog-element))
  (style obj "float"))

(defgeneric set-float-wrap (clog-element value)
  (:documentation "Set float-wrap VALUE for CLOG-ELEMENT"))

(defmethod set-float-wrap ((obj clog-element) value)
  (setf (style obj "float") (string value)))
(defsetf float-wrap set-float-wrap)

;;;;;;;;;;;;;
;; display ;;
;;;;;;;;;;;;;

(deftype display-type () '(member :none :block :inline :inline-block :flex))

(defgeneric display (clog-element)
  (:documentation "Get/Setf display."))

(defmethod display ((obj clog-element))
  (style obj "display"))

(defgeneric set-display (clog-element value)
  (:documentation "Set display VALUE for CLOG-ELEMENT"))

(defmethod set-display ((obj clog-element) value)
  (setf (style obj "display") (string value)))
(defsetf display set-display)

