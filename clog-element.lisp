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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Low Level  - clog-element
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General Properties - clog-element
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Placement  - clog-element
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Properties - clog-element
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  (setf (property obj "contentEditable") (p-true-js value)))
(defsetf editablep set-editable)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Styles - clog-element
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  (setf (style obj "box-sizing") value))
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
  (setf (style obj "clear") value))
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
  (setf (style obj "float") value))
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
  (setf (style obj "display") value))
(defsetf display set-display)

;;;;;;;;;;;;;;
;; overflow ;;
;;;;;;;;;;;;;;

(deftype overflow-type () '(member :visible :hidden :clip :scroll :auto))

(defgeneric overflow (clog-element)
  (:documentation "Get/Setf overflow."))

(defmethod overflow ((obj clog-element))
  (style obj "overflow"))

(defgeneric set-overflow (clog-element value)
  (:documentation "Set overflow VALUE for CLOG-ELEMENT"))

(defmethod set-overflow ((obj clog-element) value)
  (setf (style obj "overflow") value))
(defsetf overflow set-overflow)

;;;;;;;;;;;;;;;;
;; overflow-x ;;
;;;;;;;;;;;;;;;;

(deftype overflow-x-type () '(member :visible :hidden :clip :scroll :auto))

(defgeneric overflow-x (clog-element)
  (:documentation "Get/Setf overflow-x."))

(defmethod overflow-x ((obj clog-element))
  (style obj "overflow-x"))

(defgeneric set-overflow-x (clog-element value)
  (:documentation "Set overflow-x VALUE for CLOG-ELEMENT"))

(defmethod set-overflow-x ((obj clog-element) value)
  (setf (style obj "overflow-x") value))
(defsetf overflow-x set-overflow-x)

;;;;;;;;;;;;;;;;
;; overflow-y ;;
;;;;;;;;;;;;;;;;

(deftype overflow-y-type () '(member :visible :hidden :clip :scroll :auto))

(defgeneric overflow-y (clog-element)
  (:documentation "Get/Setf overflow-y."))

(defmethod overflow-y ((obj clog-element))
  (style obj "overflow-y"))

(defgeneric set-overflow-y (clog-element value)
  (:documentation "Set overflow-y VALUE for CLOG-ELEMENT"))

(defmethod set-overflow-y ((obj clog-element) value)
  (setf (style obj "overflow-y") value))
(defsetf overflow-y set-overflow-y)

;;;;;;;;;;;;;
;; z-index ;;
;;;;;;;;;;;;;

(defgeneric z-index (clog-element)
  (:documentation "Get/Setf z-index."))

(defmethod z-index ((obj clog-element))
  (style obj "z-index"))

(defgeneric set-z-index (clog-element value)
  (:documentation "Set z-index VALUE for CLOG-ELEMENT"))

(defmethod set-z-index ((obj clog-element) value)
  (setf (style obj "z-index") value))
(defsetf z-index set-z-index)

;;;;;;;;;;;;;;;
;; resizable ;;
;;;;;;;;;;;;;;;

(deftype resizable-type () '(member :none :both :horizontal :vertical :block :inline))

(defgeneric resizable (clog-element)
  (:documentation "Get/Setf resizable."))

(defmethod resizable ((obj clog-element))
  (style obj "resize"))

(defgeneric set-resizable (clog-element value)
  (:documentation "Set resizable VALUE for CLOG-ELEMENT"))

(defmethod set-resizable ((obj clog-element) value)
  (setf (style obj "resize") value))
(defsetf resizable set-resizable)

;;;;;;;;;;;;;;;;;
;; positioning ;;
;;;;;;;;;;;;;;;;;

(deftype positioning-type () '(member :static :relative :absolute :sticky :fixed))

(defgeneric positioning (clog-element)
  (:documentation "Get/Setf positioning."))

(defmethod positioning ((obj clog-element))
  (style obj "position"))

(defgeneric set-positioning (clog-element value)
  (:documentation "Set positioning VALUE for CLOG-ELEMENT"))

(defmethod set-positioning ((obj clog-element) value)
  (setf (style obj "position") value))
(defsetf positioning set-positioning)

;;;;;;;;;;;;;;;;;;
;; position-top ;;
;;;;;;;;;;;;;;;;;;

(defgeneric position-top (clog-element)
  (:documentation "Position from top in pixels relative to Element's
parent in the DOM."))

(defmethod position-top ((obj clog-element))
  (jquery-query obj "position().top"))

;;;;;;;;;;;;;;;;;;;
;; position-left ;;
;;;;;;;;;;;;;;;;;;;

(defgeneric position-left (clog-element)
  (:documentation "Position from left in pixels relative to Element's
parent in the DOM."))

(defmethod position-left ((obj clog-element))
  (jquery-query obj "position().left"))

;;;;;;;;;;;;;;;;
;; offset-top ;;
;;;;;;;;;;;;;;;;

(defgeneric offset-top (clog-element)
  (:documentation "Position in pixels from top relative to the document."))

(defmethod offset-top ((obj clog-element))
  (jquery-query obj "offset().top"))

;;;;;;;;;;;;;;;;;
;; offset-left ;;
;;;;;;;;;;;;;;;;;

(defgeneric offset-left (clog-element)
  (:documentation "Position in pixels from left relative to the document."))

(defmethod offset-left ((obj clog-element))
  (jquery-query obj "offset().left"))

;;;;;;;;;;
;; left ;;
;;;;;;;;;;

(defgeneric left (clog-element)
  (:documentation "Get/Setf left."))

(defmethod left ((obj clog-element))
  (style obj "left"))

(defgeneric set-left (clog-element value)
  (:documentation "Set left VALUE for CLOG-ELEMENT"))

(defmethod set-left ((obj clog-element) value)
  (setf (style obj "left") value))
(defsetf left set-left)

;;;;;;;;;;;
;; right ;;
;;;;;;;;;;;

(defgeneric right (clog-element)
  (:documentation "Get/Setf right."))

(defmethod right ((obj clog-element))
  (style obj "right"))

(defgeneric set-right (clog-element value)
  (:documentation "Set right VALUE for CLOG-ELEMENT"))

(defmethod set-right ((obj clog-element) value)
  (setf (style obj "right") value))
(defsetf right set-right)

;;;;;;;;;
;; top ;;
;;;;;;;;;

(defgeneric top (clog-element)
  (:documentation "Get/Setf top."))

(defmethod top ((obj clog-element))
  (style obj "top"))

(defgeneric set-top (clog-element value)
  (:documentation "Set top VALUE for CLOG-ELEMENT"))

(defmethod set-top ((obj clog-element) value)
  (setf (style obj "top") value))
(defsetf top set-top)

;;;;;;;;;;;;
;; bottom ;;
;;;;;;;;;;;;

(defgeneric bottom (clog-element)
  (:documentation "Get/Setf bottom."))

(defmethod bottom ((obj clog-element))
  (style obj "bottom"))

(defgeneric set-bottom (clog-element value)
  (:documentation "Set bottom VALUE for CLOG-ELEMENT"))

(defmethod set-bottom ((obj clog-element) value)
  (setf (style obj "bottom") value))
(defsetf bottom set-bottom)

;;;;;;;;;;;;;;;;
;; box-height ;;
;;;;;;;;;;;;;;;;

(defgeneric box-height (clog-element)
  (:documentation "Get/Setf box-height."))

(defmethod box-height ((obj clog-element))
  (style obj "height"))

(defgeneric set-box-height (clog-element value)
  (:documentation "Set box-height VALUE for CLOG-ELEMENT"))

(defmethod set-box-height ((obj clog-element) value)
  (setf (style obj "height") value))
(defsetf box-height set-box-height)

;;;;;;;;;;;;;;;
;; box-width ;;
;;;;;;;;;;;;;;;

(defgeneric box-width (clog-element)
  (:documentation "Get/Setf box-width."))

(defmethod box-width ((obj clog-element))
  (style obj "width"))

(defgeneric set-box-width (clog-element value)
  (:documentation "Set box-width VALUE for CLOG-ELEMENT"))

(defmethod set-box-width ((obj clog-element) value)
  (setf (style obj "width") value))
(defsetf box-width set-box-width)

;;;;;;;;;;;;;;;;;;;;
;; maximum-height ;;
;;;;;;;;;;;;;;;;;;;;

(defgeneric maximum-height (clog-element)
  (:documentation "Get/Setf maximum-height."))

(defmethod maximum-height ((obj clog-element))
  (style obj "max-height"))

(defgeneric set-maximum-height (clog-element value)
  (:documentation "Set maximum-height VALUE for CLOG-ELEMENT"))

(defmethod set-maximum-height ((obj clog-element) value)
  (setf (style obj "max-height") value))
(defsetf maximum-height set-maximum-height)

;;;;;;;;;;;;;;;;;;;
;; maximum-width ;;
;;;;;;;;;;;;;;;;;;;

(defgeneric maximum-width (clog-element)
  (:documentation "Get/Setf maximum-width."))

(defmethod maximum-width ((obj clog-element))
  (style obj "max-width"))

(defgeneric set-maximum-width (clog-element value)
  (:documentation "Set maximum-width VALUE for CLOG-ELEMENT"))

(defmethod set-maximum-width ((obj clog-element) value)
  (setf (style obj "max-width") value))
(defsetf maximum-width set-maximum-width)

;;;;;;;;;;;;;;;;;;;;
;; minimum-height ;;
;;;;;;;;;;;;;;;;;;;;

(defgeneric minimum-height (clog-element)
  (:documentation "Get/Setf minimum-height."))

(defmethod minimum-height ((obj clog-element))
  (style obj "min-height"))

(defgeneric set-minimum-height (clog-element value)
  (:documentation "Set minimum-height VALUE for CLOG-ELEMENT"))

(defmethod set-minimum-height ((obj clog-element) value)
  (setf (style obj "min-height") value))
(defsetf minimum-height set-minimum-height)

;;;;;;;;;;;;;;;;;;;
;; minimum-width ;;
;;;;;;;;;;;;;;;;;;;

(defgeneric minimum-width (clog-element)
  (:documentation "Get/Setf minimum-width."))

(defmethod minimum-width ((obj clog-element))
  (style obj "min-width"))

(defgeneric set-minimum-width (clog-element value)
  (:documentation "Set minimum-width VALUE for CLOG-ELEMENT"))

(defmethod set-minimum-width ((obj clog-element) value)
  (setf (style obj "min-width") value))
(defsetf minimum-width set-minimum-width)

;;;;;;;;;;;;;;;;;;;;
;; maximum-height ;;
;;;;;;;;;;;;;;;;;;;;

(defgeneric maximum-height (clog-element)
  (:documentation "Get/Setf maximum-height."))

(defmethod maximum-height ((obj clog-element))
  (style obj "max-height"))

(defgeneric set-maximum-height (clog-element value)
  (:documentation "Set maximum-height VALUE for CLOG-ELEMENT"))

(defmethod set-maximum-height ((obj clog-element) value)
  (setf (style obj "max-height") value))
(defsetf maximum-height set-maximum-height)

;;;;;;;;;;;;;;;;
;; draggablep ;;
;;;;;;;;;;;;;;;;

(defgeneric draggablep (clog-element)
  (:documentation "Get/Setf draggablep."))

(defmethod draggablep ((obj clog-element))
  (js-true-p (property obj "draggable")))

(defgeneric set-draggablep (clog-element value)
  (:documentation "Set draggablep VALUE for CLOG-ELEMENT"))

(defmethod set-draggablep ((obj clog-element) value)
  (setf (property obj "draggable") (p-true-js value)))
(defsetf draggablep set-draggablep)

;;;;;;;;;;;;;
;; hiddenp ;;
;;;;;;;;;;;;;

(defgeneric hiddenp (clog-element)
  (:documentation "Get/Setf hiddenp."))

(defmethod hiddenp ((obj clog-element))
  (js-true-p (property obj "hidden")))

(defgeneric set-hiddenp (clog-element value)
  (:documentation "Set hiddenp VALUE for CLOG-ELEMENT"))

(defmethod set-hiddenp ((obj clog-element) value)
  (setf (property obj "hidden") (p-true-js value)))
(defsetf hiddenp set-hiddenp)

