;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG - The Common Lisp Omnificent GUI                                 ;;;;
;;;; (c) 2020-2022 David Botton                                            ;;;;
;;;; License BSD 3 Clause                                                  ;;;;
;;;;                                                                       ;;;;
;;;; clog-element.lisp                                                     ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:in-package :clog)

;;; clog-elements represent the base object for visual html elements.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-element
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-element (clog-obj)()
  (:documentation "CLOG Element Objects is the base class for all html
element objects."))

;;;;;;;;;;;;;;;;;;;;;;;
;; make-clog-element ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defun make-clog-element (connection-id html-id &key (clog-type 'clog-element))
  "Construct a new clog-element or sub-element of CLOG-TYPE. (Private)"
  (make-instance clog-type :connection-id connection-id :html-id html-id))

;;;;;;;;;;;;;;;;;;;;;;
;; create-with-html ;;
;;;;;;;;;;;;;;;;;;;;;;

(defun create-with-html (connection-id html
			 &key (clog-type 'clog-element) (html-id nil))
  "Create a new clog-element and attach it to HTML on
CONNECTION-ID. There must be a single outer block that will be set to
an internal id. The returned CLOG-Element requires placement or will
not be visible, ie. place-after, etc. as it exists in the javascript
clog[] but is not in the DOM. If HTML-ID is nil one is generated.
(private)"
  (let ((web-id (if html-id
		    html-id
		    (format nil "CLOG~A" (clog-connection:generate-id)))))
    (clog-connection:execute
     connection-id
     (format nil
	     "clog['~A']=$(\"~A\").get(0); $(clog['~A']).first().prop('id','~A')"
	     web-id html web-id web-id))
    (make-clog-element connection-id web-id :clog-type clog-type)))

;;;;;;;;;;;;
;; attach ;;
;;;;;;;;;;;;

(defun attach (connection-id html-id)
  "Create a new clog-obj and attach an existing element with HTML-ID on
CONNECTION-ID to it and then return it. The HTML-ID must be unique. (private)"
  (clog-connection:execute connection-id
	      (format nil "clog['~A']=$('#~A').get(0)" html-id html-id))
  (make-clog-element connection-id html-id))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Low Level  - clog-element
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;
;; create-child ;;
;;;;;;;;;;;;;;;;;;

(defgeneric create-child (clog-obj html &key html-id auto-place clog-type)
  (:documentation "Create a new CLOG-Element or sub-type of CLOG-TYPE from HTML
as child of CLOG-OBJ and if :AUTO-PLACE (default t) place-inside-bottom-of
CLOG-OBJ. If HTML-ID is nil one will be generated."))

(defmethod create-child ((obj clog-obj) html &key (html-id nil)
					       (auto-place t)
					       (clog-type 'clog-element))
  (let ((child (create-with-html (connection-id obj) (escape-string html)
				 :clog-type clog-type
				 :html-id   html-id)))
    (if auto-place
	(place-inside-bottom-of obj child)
	child)))

;;;;;;;;;;;;;;;;;;;;;
;; attach-as-child ;;
;;;;;;;;;;;;;;;;;;;;;

(defgeneric attach-as-child (clog-obj html-id &key clog-type new-id)
  (:documentation "Create a new CLOG-ELEMENT or sub-type of CLOG-TYPE and
attach an existing element with HTML-ID. The HTML-ID must be unique and
must be in DOM, ie placed or auto-placed. If new-id is true the HTML-ID
after attachment is changed to one unique to this session."))

(defmethod attach-as-child ((obj clog-obj) html-id
			    &key (clog-type 'clog-element)
			      (new-id nil))
  (if new-id
      (let ((id (format nil "CLOG~A" (clog-connection:generate-id))))
	(clog-connection:execute (connection-id obj)
	 (format nil "$('#~A').attr('id','~A');clog['~A']=$('#~A').get(0)"
		 html-id id id id))
	(setf html-id id))
      (clog-connection:execute (connection-id obj)
			       (format nil "clog['~A']=$('#~A').get(0)"
				       html-id html-id)))
  (make-clog-element (connection-id obj) html-id :clog-type clog-type))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General Properties - clog-element
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;
;; style ;;
;;;;;;;;;;;

(defgeneric style (clog-element style-name &key default-answer)
  (:documentation "Get/Setf css style."))

(defmethod style ((obj clog-element) style-name
		      &key (default-answer nil))
  (jquery-query obj (format nil "css('~A')" style-name)
		:default-answer default-answer))

(defgeneric set-style (clog-element style-name value)
  (:documentation "Set css style."))

(defmethod set-style ((obj clog-element) style-name value)
  (jquery-execute obj (format nil "css('~A','~A')"
			      style-name (escape-string value)))
  value)
(defsetf style set-style)

(defgeneric set-styles (clog-element style-list)
  (:documentation "Set css styles using a list of list of name value pairs."))

(defmethod set-styles ((obj clog-element) style-list)
  (jquery-execute obj (format nil "css({~{~A~^,~}})"
			      (remove nil (mapcar
					   (lambda (n)
					     (when n
					       (format nil "'~A':'~A'"
						       (first n)
						       (second n))))
					   style-list)))))

;;;;;;;;;;;;;;;
;; attribute ;;
;;;;;;;;;;;;;;;

(defgeneric attribute (clog-element attribute-name &key default-answer)
  (:documentation "Get/Setf html tag attribute. (eg. src on img tag)"))

(defmethod attribute ((obj clog-element) attribute-name
		      &key (default-answer nil))
  (jquery-query obj (format nil "attr('~A')" attribute-name)
		:default-answer default-answer))

(defgeneric remove-attribute (clog-element attribute-name)
  (:documentation "Remove html tag attribute. (eg. src on img tag)"))

(defmethod remove-attribute ((obj clog-element) attribute-name)
  (jquery-execute obj (format nil "removeAttr('~A')" attribute-name))
  attribute-name)

(defgeneric set-attribute (clog-element attribute-name value)
  (:documentation "Set html tag attribute."))

(defmethod set-attribute ((obj clog-element) attribute-name value)
  (jquery-execute obj (format nil "attr('~A','~A')"
			      attribute-name (escape-string value)))
  value)
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
  (:documentation "Get/Setf access-key. Used for hot key access to element.
[special key] + Access_Key

   The [special key] per browser and platform is:
  
    Browser              Windows       Linux           Mac
    -----------------    -------       -----           ---
    Internet Explorer     [Alt]         N/A            N/A
    Chrome                [Alt]        [Alt]     [Control][Alt]
    Firefox           [Alt][Shift] [Alt][Shift]  [Control][Alt]
    Safari                [Alt]         N/A      [Control][Alt]
    Opera 15+             [Alt]        [Alt]          [Alt]"))

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
  (:documentation "Get/Setf advisory title of Element, usually
used for body and image maps but creates in forms and many
elements a tool tip."))

(defmethod advisory-title ((obj clog-element))
  (property obj "title"))

(defgeneric set-advisory-title (clog-element value)
  (:documentation "Set advisory-title VALUE for CLOG-ELEMENT"))

(defmethod set-advisory-title ((obj clog-element) value)
  (setf (property obj "title") value))
(defsetf advisory-title set-advisory-title)

;;;;;;;;;;;;;;;;;;;;
;; css-class-name ;;
;;;;;;;;;;;;;;;;;;;;

(defgeneric css-class-name (clog-element)
  (:documentation "Get/Setf css-class-name. CSS Class name, can be multiple
seperated by <space>. See add-class, remove-class and toggle-class methods
for adding and removing individual or groups of classes in an easier way."))

(defmethod css-class-name ((obj clog-element))
  (property obj "className"))

(defgeneric set-css-class-name (clog-element value)
  (:documentation "Set css-class-name VALUE for CLOG-ELEMENT"))

(defmethod set-css-class-name ((obj clog-element) value)
  (setf (property obj "className") value))
(defsetf css-class-name set-css-class-name)

;;;;;;;;;;;;;;;
;; editablep ;;
;;;;;;;;;;;;;;;

(defgeneric editablep (clog-element)
  (:documentation "Get/Setf editable. This will make almost any element with
content editable, even non-form types in most browsers."))

(defmethod editablep ((obj clog-element))
  (js-true-p (property obj "isContentEditable")))

(defgeneric set-editablep (clog-element value)
  (:documentation "Set editable VALUE for CLOG-ELEMENT"))

(defmethod set-editablep ((obj clog-element) value)
  (setf (property obj "contentEditable") (p-true-js value)))
(defsetf editablep set-editablep)

;;;;;;;;;;;;;;;;
;; draggablep ;;
;;;;;;;;;;;;;;;;

(defgeneric draggablep (clog-element)
  (:documentation "Get/Setf draggablep. In order to make an object draggable
in addition to Draggable being true the on-drag-start event _must_ be bound
as well to set the drag-text. To receive a drop, you need to bind on-drop.
See clog-base.lisp"))

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
  (:documentation "Get/Setf hiddenp. The hidden property will make an element
invisible, however unlike visiblep, hiddenp implies the element is semantically
not relevant not just visually and will _also_ remove it from layout similar to
setting display (None)."))

(defmethod hiddenp ((obj clog-element))
  (unless (equalp (attribute obj "hidden") "undefined")
    t))

(defgeneric set-hiddenp (clog-element value)
  (:documentation "Set hiddenp VALUE for CLOG-ELEMENT"))

(defmethod set-hiddenp ((obj clog-element) value)
  (if value
      (setf (attribute obj "hidden") t)
      (remove-attribute obj "hidden")))
(defsetf hiddenp set-hiddenp)

;;;;;;;;;;;;;;
;; visiblep ;;
;;;;;;;;;;;;;;

(defgeneric visiblep (clog-element)
  (:documentation "Get/Setf visiblep. This will cause the Element to no longer
be visible but it will still take up space where it was in the layout. Use
hiddenp to also remove from layout.
Note: that each property, visiblep, hiddenp and display (None) all work
      independantly and do not reflect the actual client side visual state
      but the property state. To check if an object is for sure not visible
      would require checking all three properties."))

(defmethod visiblep ((obj clog-element))
  (equalp (style obj "visibility") "visible"))

(defgeneric set-visiblep (clog-element value)
  (:documentation "Set visiblep VALUE for CLOG-ELEMENT"))

(defmethod set-visiblep ((obj clog-element) value)
  (if value
      (setf (style obj "visibility") "visible")
      (setf (style obj "visibility") "hidden")))
(defsetf visiblep set-visiblep)

;;;;;;;;;;;;;;;;
;; inner-html ;;
;;;;;;;;;;;;;;;;

(defgeneric inner-html (clog-element)
  (:documentation "Get/Setf inner-html. This will completely replace the inner
html of an element. This will remove any Elements within Element from the DOM.
If those elements were created in CLOG they are still available and can be
placed in the DOM again using the placement methods. However if they were
created through html writes or otherwise not assigned an ID by CLOG, they are
lost forever."))

(defmethod inner-html ((obj clog-element))
  (jquery-query obj "html()"))

(defgeneric set-inner-html (clog-element value)
  (:documentation "Set inner-html VALUE for CLOG-ELEMENT"))

(defmethod set-inner-html ((obj clog-element) value)
  (jquery-execute obj (format nil "html('~A')" (escape-string value)))
  value)
(defsetf inner-html set-inner-html)

;;;;;;;;;;;;;;;;
;; outer-html ;;
;;;;;;;;;;;;;;;;

(defgeneric outer-html (clog-element)
  (:documentation "Get/Setf outer-html. Returns the HTML for Element and all
its contents"))

(defmethod outer-html ((obj clog-element))
  (query obj "outerHTML"))

;;;;;;;;;;;;;;;;;
;; spellcheckp ;;
;;;;;;;;;;;;;;;;;

(defgeneric spellcheckp (clog-element)
  (:documentation "Get/Setf spellcheckp. If true Element is subject to browser
spell checking if Editable is also true."))

(defmethod spellcheckp ((obj clog-element))
  (js-true-p (attribute obj "spellcheck")))

(defgeneric set-spellcheckp (clog-element value)
  (:documentation "Set spellcheckp VALUE for CLOG-ELEMENT"))

(defmethod set-spellcheckp ((obj clog-element) value)
  (setf (attribute obj "spellcheck") (p-true-js value)))
(defsetf spellcheckp set-spellcheckp)

;;;;;;;;;;;;;;;
;; tab-index ;;
;;;;;;;;;;;;;;;

(defgeneric tab-index (clog-element)
  (:documentation "Get/Setf tab-index. If -1 not focusable.
If 0 element in tab index. If >0 sets order in tab index.
Normally index follows normal sequence of elements."))

(defmethod tab-index ((obj clog-element))
  (property obj "tabindex"))

(defgeneric set-tab-index (clog-element value)
  (:documentation "Set tab-index VALUE for CLOG-ELEMENT"))

(defmethod set-tab-index ((obj clog-element) value)
  (setf (property obj "tabindex") value))
(defsetf tab-index set-tab-index)

;;;;;;;;;;
;; text ;;
;;;;;;;;;;

(defgeneric text (clog-element)
  (:documentation "Get/Setf text.

<tag>Text Content</tag> - Text content is the content contained by the
                          tag. This should not be confused with the
                          'Value' of a Form Tag. (See clog-form.lisp)"))

(defmethod text ((obj clog-element))
  (jquery-query obj "text()"))

(defgeneric set-text (clog-element value)
  (:documentation "Set text VALUE for CLOG-ELEMENT"))

(defmethod set-text ((obj clog-element) value)
  (jquery-execute obj (format nil "text('~A')" (escape-string value)))
  value)
(defsetf text set-text)

;;;;;;;;;;;;;;;;;;;;
;; text-direction ;;
;;;;;;;;;;;;;;;;;;;;

(deftype text-direction-type () '(member :ltr :rtl :inherit))

(defgeneric text-direction (clog-element)
  (:documentation "Get/Setf BiDi text-direction."))

(defmethod text-direction ((obj clog-element))
  (property obj "dir"))

(defgeneric set-text-direction (clog-element value)
  (:documentation "Set text-direction VALUE for CLOG-ELEMENT"))

(defmethod set-text-direction ((obj clog-element) value)
  (setf (property obj "dir") value))
(defsetf text-direction set-text-direction)

;;;;;;;;;;;;;;;;;;;
;; language-code ;;
;;;;;;;;;;;;;;;;;;;

(defgeneric language-code (clog-element)
  (:documentation "Get/Setf language-code."))

(defmethod language-code ((obj clog-element))
  (property obj "lang"))

(defgeneric set-language-code (clog-element value)
  (:documentation "Set language-code VALUE for CLOG-ELEMENT"))

(defmethod set-language-code ((obj clog-element) value)
  (setf (property obj "lang") value))
(defsetf language-code set-language-code)

;;;;;;;;;;;;;;;;;;;
;; position-left ;;
;;;;;;;;;;;;;;;;;;;

(defgeneric position-left (clog-element)
  (:documentation "Get position-left. The relative left border of an element
in pixels (css left in pixels)."))

(defmethod position-left ((obj clog-element))
  (parse-integer (jquery-query obj "position().left" :default-answer 0) :junk-allowed t))

;;;;;;;;;;;;;;;;;;
;; position-top ;;
;;;;;;;;;;;;;;;;;;

(defgeneric position-top (clog-element)
  (:documentation "Get position-top. The relative top border of an element
in pixels (css top in pixels)."))

(defmethod position-top ((obj clog-element))
  (parse-integer (jquery-query obj "position().top" :default-answer 0) :junk-allowed t))

;;;;;;;;;;;;;;;;;
;; client-left ;;
;;;;;;;;;;;;;;;;;

(defgeneric client-left (clog-element)
  (:documentation "Get client-left. The width of the left border of an element
in pixels. It does not include the margin or padding."))

(defmethod client-left ((obj clog-element))
  (parse-integer (property obj "clientLeft" :default-answer 0) :junk-allowed t))

;;;;;;;;;;;;;;;;
;; client-top ;;
;;;;;;;;;;;;;;;;

(defgeneric client-top (clog-element)
  (:documentation "Get client-top. The width of the top border of an element
in pixels. It does not include the margin or padding."))

(defmethod client-top ((obj clog-element))
  (parse-integer (property obj "clientTop" :default-answer 0) :junk-allowed t))

;;;;;;;;;;;;;;;;;;
;; client-width ;;
;;;;;;;;;;;;;;;;;;

(defgeneric client-width (clog-element)
  (:documentation "Get client-width. Inner width of an element in pixels.
CSS width + CSS padding - width of vertical scrollbar (if present)
Does not include the border or margin."))

(defmethod client-width ((obj clog-element))
  (parse-integer (property obj "clientWidth" :default-answer 0) :junk-allowed t))

;;;;;;;;;;;;;;;;;;;
;; client-height ;;
;;;;;;;;;;;;;;;;;;;

(defgeneric client-height (clog-element)
  (:documentation "Get client-right. Inner height of an element in pixels.
CSS height + CSS padding - height of horizontal scrollbar (if present)
Does not include the border or margin."))

(defmethod client-height ((obj clog-element))
  (parse-integer (property obj "clientHeight" :default-answer 0) :junk-allowed t))

;;;;;;;;;;;;;;;;;
;; offset-left ;;
;;;;;;;;;;;;;;;;;

(defgeneric offset-left (clog-element)
  (:documentation "Get offset-left. The width from parent element border to
child border left."))

(defmethod offset-left ((obj clog-element))
  (property obj "offsetLeft"))

;;;;;;;;;;;;;;;;
;; offset-top ;;
;;;;;;;;;;;;;;;;

(defgeneric offset-top (clog-element)
  (:documentation "Get offset-top. The width from parent element border to
child border top."))

(defmethod offset-top ((obj clog-element))
  (property obj "offsetTop"))

;;;;;;;;;;;;;;;;;;
;; offset-width ;;
;;;;;;;;;;;;;;;;;;

(defgeneric offset-width (clog-element)
  (:documentation "Get offset-width. CSS width + CSS padding + width of
vertical scrollbar (if present) + Border"))

(defmethod offset-width ((obj clog-element))
  (property obj "offsetWidth"))

;;;;;;;;;;;;;;;;;;;
;; offset-height ;;
;;;;;;;;;;;;;;;;;;;

(defgeneric offset-height (clog-element)
  (:documentation "Get offset-height. CSS height + CSS padding + height of
horizontal scrollbar (if present) + Border"))

(defmethod offset-height ((obj clog-element))
  (property obj "offsetHeight"))

;;;;;;;;;;;;;;;;;
;; scroll-left ;;
;;;;;;;;;;;;;;;;;

(defgeneric scroll-left (clog-element)
  (:documentation "Get scroll-left. The number of pixels that an element's
content is scrolled to the left. For RTL languages is negative."))

(defmethod scroll-left ((obj clog-element))
  (parse-integer (property obj "scrollLeft" :default-answer 0) :junk-allowed t))

(defgeneric set-scroll-left (clog-element value)
  (:documentation "Set scroll-left VALUE for CLOG-ELEMENT"))

(defmethod set-scroll-left ((obj clog-element) value)
  (setf (property obj "scrollLeft") value))
(defsetf scroll-left set-scroll-left)

;;;;;;;;;;;;;;;;
;; scroll-top ;;
;;;;;;;;;;;;;;;;

(defgeneric scroll-top (clog-element)
  (:documentation "Get scroll-top. The number of pixels that an element's
content has been scrolled upward."))

(defmethod scroll-top ((obj clog-element))
  (parse-integer (property obj "scrollTop" :default-answer 0) :junk-allowed t))

(defgeneric set-scroll-top (clog-element value)
  (:documentation "Set scroll-top VALUE for CLOG-ELEMENT"))

(defmethod set-scroll-top ((obj clog-element) value)
  (setf (property obj "scrollTop") value))
(defsetf scroll-top set-scroll-top)

;;;;;;;;;;;;;;;;;;
;; scroll-width ;;
;;;;;;;;;;;;;;;;;;

(defgeneric scroll-width (clog-element)
  (:documentation "Get scroll-width. Either the width in pixels of the content
of an element or the width of the element itself, whichever is greater."))

(defmethod scroll-width ((obj clog-element))
  (parse-integer (property obj "scrollWidth" :default-answer 0) :junk-allowed t))

;;;;;;;;;;;;;;;;;;;
;; scroll-height ;;
;;;;;;;;;;;;;;;;;;;

(defgeneric scroll-height (clog-element)
  (:documentation "Get scroll-height. Height of an element's content, including
content not visible on the screen due to overflow."))

(defmethod scroll-height ((obj clog-element))
  (parse-integer (property obj "scrollHeight" :default-answer 0) :junk-allowed t))

;;;;;;;;;;;;;;
;; html-tag ;;
;;;;;;;;;;;;;;

(defgeneric html-tag (clog-element)
  (:documentation "Get html-tag."))

(defmethod html-tag ((obj clog-element))
  (property obj "tagName"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Styles - clog-element
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;
;; box-sizing ;;
;;;;;;;;;;;;;;;;

(deftype box-sizing-type () '(member :content-box :border-box))

(defgeneric box-sizing (clog-element)
  (:documentation "Get/Setf box-sizing. Affects if height and width
properteries represent just the content or the border, marging, padding,
scroll and conent area as a whole. The default is content-box"))

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
  (:documentation "Get/Setf clear-side. When using 'float' for layout sets
if the right or left side of block should be clear of any 'floated' Element."))

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

(deftype display-type () '(member :none :block :inline :inline-block :flex
			   :grid :inline-grid))

(defgeneric display (clog-element)
  (:documentation "Get/Setf display. Display sets the CSS Display property that
handles how elements are treated by the browser layout engine.
   
    Common Values:

    none         - Remove Element from layout but remain in the DOM this is
                   similar to hiddenp, but not like visiblep that makes the
                   element not visible but still take up space in layout.
   
    block        - Displays an element starting on a new line and stretches
                   out to the left and right as far as it can. e.g. <div> by
                   default
   
    inline       - Wraps with text in a paragraph. e.g. <span> by default
   
    inline-block - Flows with paragraph but will always fill from left to
                   right.
   
    flex         - Turn this item in to a flexbox container. The flexbox
                   properties for container to adjust are:

                      justify-content - how items are spaced in flexbox
                      align-content - how items spaced when wrapped
                      align-items - when placed (from start, center, from end)
                      flex-direction - flex-box left<>right or top<>bottom
                      flex-wrap - keep in one line or wrap to more

                   The properties to adjust for items in the flexbox are:

                      flex - sets the relative grow,shrink,basis
                      order - sets visual item order in flexbox
                      align-self - override flexbox's align-items for item

                   :flex-start    [---  ]
                   :flex-end      [   ---]
                   :center        [ --- ]
                   :space-between [-  -  -]
                   :space-around  [ -   -   - ]
                   :space-evenly  [ - - - ]

    grid         - Turn this item in to a grid container block level. The grid
                   properties to adjust for container are:

                      grid-template-columns
                      grid-template-rows
                      grid-template-areas
                      column-gap
                      row-gap
                      align-items
                      justify-items
                      justify-content - align the grid as a whole in container
                      align-content - align the grid as a whole in container
                      grid-auto-columns
                      grid-auto-rows
                      grid-auto-flow

                    The properties to adjust for grid items is:

                      grid-column-start
                      grid-column-end
                      grid-row-start
                      grid-row-end
                      align-self
                      justify-self

    inline-grid   - Turn this item in to a grid container inline level."))

(defmethod display ((obj clog-element))
  (style obj "display"))

(defgeneric set-display (clog-element value)
  (:documentation "Set display VALUE for CLOG-ELEMENT"))

(defmethod set-display ((obj clog-element) value)
  (setf (style obj "display") value))
(defsetf display set-display)

;;;;;;;;;;;
;; order ;;
;;;;;;;;;;;

(defgeneric order (clog-element)
  (:documentation "Get/Setf visual item order flexbox packing but
not actual order in document or tab order etc."))

(defmethod order ((obj clog-element))
  (style obj "order"))

(defgeneric set-order (clog-element value)
  (:documentation "Set order VALUE for CLOG-ELEMENT"))

(defmethod set-order ((obj clog-element) value)
  (setf (style obj "order") value))
(defsetf order set-order)

;;;;;;;;;;
;; flex ;;
;;;;;;;;;;

(defgeneric flex (clog-element)
  (:documentation "Get item's flexbox relative grow, shrink,
and basis"))

(defmethod flex ((obj clog-element))
  (style obj "flex"))

(defgeneric set-flex (clog-element &key grow shrink flex-basis)
  (:documentation "Set flex grow (default 0) shrink (default 1) and
flex-basis (default :auto = use width or height) for CLOG-ELEMENT"))

(defmethod set-flex ((obj clog-element)
		     &key (grow 0) (shrink 1) (flex-basis :auto))
  (setf (style obj "flex") (format nil "~A ~A ~A" grow shrink flex-basis)))


;;;;;;;;;;;;;;;
;; flex-wrap ;;
;;;;;;;;;;;;;;;

(deftype flex-wrap-type ()
  '(member :nowrap :wrap :wrap-reverse))

(defgeneric flex-wrap (clog-element)
  (:documentation "Get/Setf direction of flexbox packing."))

(defmethod flex-wrap ((obj clog-element))
  (style obj "flex-wrap"))

(defgeneric set-flex-wrap (clog-element value)
  (:documentation "Set flex-wrap VALUE for CLOG-ELEMENT"))

(defmethod set-flex-wrap ((obj clog-element) value)
  (setf (style obj "flex-wrap") value))
(defsetf flex-wrap set-flex-wrap)

;;;;;;;;;;;;;;;;;;;;
;; flex-direction ;;
;;;;;;;;;;;;;;;;;;;;

(deftype flex-direction-type ()
  '(member :row :row-reverse :column :column-reverse))

(defgeneric flex-direction (clog-element)
  (:documentation "Get/Setf direction of flexbox packing."))

(defmethod flex-direction ((obj clog-element))
  (style obj "flex-direction"))

(defgeneric set-flex-direction (clog-element value)
  (:documentation "Set flex-direction VALUE for CLOG-ELEMENT"))

(defmethod set-flex-direction ((obj clog-element) value)
  (setf (style obj "flex-direction") value))
(defsetf flex-direction set-flex-direction)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; grid-template-columns ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric grid-template-columns (clog-element)
  (:documentation "Get/Setf grid-template-columns."))

(defmethod grid-template-columns ((obj clog-element))
  (style obj "grid-template-columns"))

(defgeneric set-grid-template-columns (clog-element value)
  (:documentation "Set grid-template-columns VALUE for CLOG-ELEMENT"))

(defmethod set-grid-template-columns ((obj clog-element) value)
  (setf (style obj "grid-template-columns") value))
(defsetf grid-template-columns set-grid-template-columns)

;;;;;;;;;;;;;;;;;;;;;;;;
;; grid-template-rows ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric grid-template-rows (clog-element)
  (:documentation "Get/Setf grid-template-rows."))

(defmethod grid-template-rows ((obj clog-element))
  (style obj "grid-template-rows"))

(defgeneric set-grid-template-rows (clog-element value)
  (:documentation "Set grid-template-rows VALUE for CLOG-ELEMENT"))

(defmethod set-grid-template-rows ((obj clog-element) value)
  (setf (style obj "grid-template-rows") value))
(defsetf grid-template-rows set-grid-template-rows)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; grid-template-areas ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric grid-template-areas (clog-element)
  (:documentation "Get/Setf grid-template-areas."))

(defmethod grid-template-areas ((obj clog-element))
  (style obj "grid-template-areas"))

(defgeneric set-grid-template-areas (clog-element value)
  (:documentation "Set grid-template-areas VALUE for CLOG-ELEMENT"))

(defmethod set-grid-template-areas ((obj clog-element) value)
  (setf (style obj "grid-template-areas") value))
(defsetf grid-template-areas set-grid-template-areas)

;;;;;;;;;;;;;;;;
;; column-gap ;;
;;;;;;;;;;;;;;;;

(defgeneric column-gap (clog-element)
  (:documentation "Get/Setf column-gap."))

(defmethod column-gap ((obj clog-element))
  (style obj "column-gap"))

(defgeneric set-column-gap (clog-element value)
  (:documentation "Set column-gap VALUE for CLOG-ELEMENT"))

(defmethod set-column-gap ((obj clog-element) value)
  (setf (style obj "column-gap") value))
(defsetf column-gap set-column-gap)

;;;;;;;;;;;;;
;; row-gap ;;
;;;;;;;;;;;;;

(defgeneric row-gap (clog-element)
  (:documentation "Get/Setf row-gap."))

(defmethod row-gap ((obj clog-element))
  (style obj "row-gap"))

(defgeneric set-row-gap (clog-element value)
  (:documentation "Set row-gap VALUE for CLOG-ELEMENT"))

(defmethod set-row-gap ((obj clog-element) value)
  (setf (style obj "row-gap") value))
(defsetf row-gap set-row-gap)

;;;;;;;;;;;;;;;;;;;;;;;
;; grid-auto-columns ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric grid-auto-columns (clog-element)
  (:documentation "Get/Setf grid-auto-columns."))

(defmethod grid-auto-columns ((obj clog-element))
  (style obj "grid-auto-columns"))

(defgeneric set-grid-auto-columns (clog-element value)
  (:documentation "Set grid-auto-columns VALUE for CLOG-ELEMENT"))

(defmethod set-grid-auto-columns ((obj clog-element) value)
  (setf (style obj "grid-auto-columns") value))
(defsetf grid-auto-columns set-grid-auto-columns)

;;;;;;;;;;;;;;;;;;;;
;; grid-auto-rows ;;
;;;;;;;;;;;;;;;;;;;;

(defgeneric grid-auto-rows (clog-element)
  (:documentation "Get/Setf grid-auto-rows."))

(defmethod grid-auto-rows ((obj clog-element))
  (style obj "grid-auto-rows"))

(defgeneric set-grid-auto-rows (clog-element value)
  (:documentation "Set grid-auto-rows VALUE for CLOG-ELEMENT"))

(defmethod set-grid-auto-rows ((obj clog-element) value)
  (setf (style obj "grid-auto-rows") value))
(defsetf grid-auto-rows set-grid-auto-rows)

;;;;;;;;;;;;;;;;;;;;
;; grid-auto-flow ;;
;;;;;;;;;;;;;;;;;;;;

(defgeneric grid-auto-flow (clog-element)
  (:documentation "Get/Setf grid-auto-flow."))

(defmethod grid-auto-flow ((obj clog-element))
  (style obj "grid-auto-flow"))

(defgeneric set-grid-auto-flow (clog-element value)
  (:documentation "Set grid-auto-flow VALUE for CLOG-ELEMENT"))

(defmethod set-grid-auto-flow ((obj clog-element) value)
  (setf (style obj "grid-auto-flow") value))
(defsetf grid-auto-flow set-grid-auto-flow)

;;;;;;;;;;;;;;;;;;;;;;;
;; grid-column-start ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric grid-column-start (clog-element)
  (:documentation "Get/Setf grid-column-start."))

(defmethod grid-column-start ((obj clog-element))
  (style obj "grid-column-start"))

(defgeneric set-grid-column-start (clog-element value)
  (:documentation "Set grid-column-start VALUE for CLOG-ELEMENT"))

(defmethod set-grid-column-start ((obj clog-element) value)
  (setf (style obj "grid-column-start") value))
(defsetf grid-column-start set-grid-column-start)

;;;;;;;;;;;;;;;;;;;;;
;; grid-column-end ;;
;;;;;;;;;;;;;;;;;;;;;

(defgeneric grid-column-end (clog-element)
  (:documentation "Get/Setf grid-column-end."))

(defmethod grid-column-end ((obj clog-element))
  (style obj "grid-column-end"))

(defgeneric set-grid-column-end (clog-element value)
  (:documentation "Set grid-column-end VALUE for CLOG-ELEMENT"))

(defmethod set-grid-column-end ((obj clog-element) value)
  (setf (style obj "grid-column-end") value))
(defsetf grid-column-end set-grid-column-end)

;;;;;;;;;;;;;;;;;;;;
;; grid-row-start ;;
;;;;;;;;;;;;;;;;;;;;

(defgeneric grid-row-start (clog-element)
  (:documentation "Get/Setf grid-row-start."))

(defmethod grid-row-start ((obj clog-element))
  (style obj "grid-row-start"))

(defgeneric set-grid-row-start (clog-element value)
  (:documentation "Set grid-row-start VALUE for CLOG-ELEMENT"))

(defmethod set-grid-row-start ((obj clog-element) value)
  (setf (style obj "grid-row-start") value))
(defsetf grid-row-start set-grid-row-start)

;;;;;;;;;;;;;;;;;;
;; grid-row-end ;;
;;;;;;;;;;;;;;;;;;

(defgeneric grid-row-end (clog-element)
  (:documentation "Get/Setf grid-row-end."))

(defmethod grid-row-end ((obj clog-element))
  (style obj "grid-row-end"))

(defgeneric set-grid-row-end (clog-element value)
  (:documentation "Set grid-row-end VALUE for CLOG-ELEMENT"))

(defmethod set-grid-row-end ((obj clog-element) value)
  (setf (style obj "grid-row-end") value))
(defsetf grid-row-end set-grid-row-end)

;;;;;;;;;;;;;;;;;
;; align-items ;;
;;;;;;;;;;;;;;;;;

(deftype align-items-type ()
  '(member :flex-start :flex-end :start :end :center
    :space-between :space-around :space-evenly :stretch))

(defgeneric align-items (clog-element)
  (:documentation "Get/Setf align items in a flexbox/grid on column axis."))

(defmethod align-items ((obj clog-element))
  (style obj "align-items"))

(defgeneric set-align-items (clog-element value)
  (:documentation "Set align-items VALUE for CLOG-ELEMENT"))

(defmethod set-align-items ((obj clog-element) value)
  (setf (style obj "align-items") value))
(defsetf align-items set-align-items)

;;;;;;;;;;;;;;;;
;; align-self ;;
;;;;;;;;;;;;;;;;

(deftype align-self-type ()
  '(member :flex-start :flex-end :start :end :center
    :space-between :space-around :space-evenly :stretch))

(defgeneric align-self (clog-element)
  (:documentation "Get/Setf override align-items for this item
in a flexbox/grid."))

(defmethod align-self ((obj clog-element))
  (style obj "align-self"))

(defgeneric set-align-self (clog-element value)
  (:documentation "Set align-self VALUE for CLOG-ELEMENT"))

(defmethod set-align-self ((obj clog-element) value)
  (setf (style obj "align-self") value))
(defsetf align-self set-align-self)

;;;;;;;;;;;;;;;;;;;
;; justify-items ;;
;;;;;;;;;;;;;;;;;;;

(deftype justify-items-type ()
  '(member :start :end :center :space-between :space-around
    :space-evenly :stretch))

(defgeneric justify-items (clog-element)
  (:documentation "Get/Setf justify items in a grid on row axis."))

(defmethod justify-items ((obj clog-element))
  (style obj "justify-items"))

(defgeneric set-justify-items (clog-element value)
  (:documentation "Set justify-items VALUE for CLOG-ELEMENT"))

(defmethod set-justify-items ((obj clog-element) value)
  (setf (style obj "justify-items") value))
(defsetf justify-items set-justify-items)

;;;;;;;;;;;;;;;;;;
;; justify-self ;;
;;;;;;;;;;;;;;;;;;

(deftype justify-self-type ()
  '(member :start :end :center :space-between :space-around
    :space-evenly :stretch))

(defgeneric justify-self (clog-element)
  (:documentation "Get/Setf override align this item in grid on row axis."))

(defmethod justify-self ((obj clog-element))
  (style obj "justify-self"))

(defgeneric set-justify-self (clog-element value)
  (:documentation "Set justify-self VALUE for CLOG-ELEMENT"))

(defmethod set-justify-self ((obj clog-element) value)
  (setf (style obj "justify-self") value))
(defsetf justify-self set-justify-self)

;;;;;;;;;;;;;;;;;;;;;
;; justify-content ;;
;;;;;;;;;;;;;;;;;;;;;

(deftype justify-content-type ()
  '(member :flex-start :flex-end :start :end :center
    :space-between :space-around :space-evenly))

(defgeneric justify-content (clog-element)
  (:documentation "Get/Setf justify content for items inline of a
 flexbox or grid on row access."))

(defmethod justify-content ((obj clog-element))
  (style obj "justify-content"))

(defgeneric set-justify-content (clog-element value)
  (:documentation "Set justify-content VALUE for CLOG-ELEMENT"))

(defmethod set-justify-content ((obj clog-element) value)
  (setf (style obj "justify-content") value))
(defsetf justify-content set-justify-content)

;;;;;;;;;;;;;;;;;;;
;; align-content ;;
;;;;;;;;;;;;;;;;;;;

(deftype align-content-type ()
  '(member :flex-start :flex-end :start :end :center
    :space-between :space-around :space-evenly))

(defgeneric align-content (clog-element)
  (:documentation "Get/Setf align content wrapped inline of a flexbox
on opposite sides of each other or grid on column axis."))

(defmethod align-content ((obj clog-element))
  (style obj "align-content"))

(defgeneric set-align-content (clog-element value)
  (:documentation "Set align-content VALUE for CLOG-ELEMENT"))

(defmethod set-align-content ((obj clog-element) value)
  (setf (style obj "align-content") value))
(defsetf align-content set-align-content)

;;;;;;;;;;;;;;
;; overflow ;;
;;;;;;;;;;;;;;

(deftype overflow-type () '(member :visible :hidden :clip :scroll :auto))

(defgeneric overflow (clog-element)
  (:documentation "Get/Setf overflow.  How to handle overflow of contents of
an element's box. The default is visible - no clipping."))

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
  (:documentation "Get/Setf overflow-x. How to handle overflow of contents of
an element's box for X. The default is Visible - no clipping."))

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
  (:documentation "Get/Setf overflow-y. How to handle overflow of contents of
an element's box for Y. The default is Visible - no clipping."))

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
  (:documentation "Get/Setf z-index. Set stack order of element.
Note: z-index only works on Elements with Position Type of absolute,
      relative and fixed."))

(defmethod z-index ((obj clog-element))
  (parse-integer (style obj "z-index") :junk-allowed t))

(defgeneric set-z-index (clog-element value)
  (:documentation "Set z-index VALUE for CLOG-ELEMENT"))

(defmethod set-z-index ((obj clog-element) value)
  (setf (style obj "z-index") value))
(defsetf z-index set-z-index)

;;;;;;;;;;;;;;;
;; resizable ;;
;;;;;;;;;;;;;;;

(deftype resizable-type ()
  '(member :none :both :horizontal :vertical :block :inline))

(defgeneric resizable (clog-element)
  (:documentation "Get/Setf resizable. If overflow is not set to visible,
resizeable sets if element can be resized by user."))

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

(deftype positioning-type ()
  '(member :static :relative :absolute :sticky :fixed))

(defgeneric positioning (clog-element)
  (:documentation "Get/Setf positioning. Determins how the properties left,
right, top and bottom are interpreted.

   Static   - According to document flow, position properties have no
              affect.
   Absolute - Position properties are relative to the first non-static
              element in the DOM before Element
   Fixed    - Position properties are relative to browser window
   Relative - Position properties are relative to where the static position
              of the element would in the normal document flow."))

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
  (parse-integer (jquery-query obj "position().top" :default-answer 0) :junk-allowed t))

;;;;;;;;;;;;;;;;;;;
;; position-left ;;
;;;;;;;;;;;;;;;;;;;

(defgeneric position-left (clog-element)
  (:documentation "Position from left in pixels relative to Element's
parent in the DOM."))

(defmethod position-left ((obj clog-element))
  (parse-integer (jquery-query obj "position().left" :default-answer 0) :junk-allowed t))

;;;;;;;;;;;;;;;;
;; offset-top ;;
;;;;;;;;;;;;;;;;

(defgeneric offset-top (clog-element)
  (:documentation "Position in pixels from top relative to the document."))

(defmethod offset-top ((obj clog-element))
  (parse-integer (jquery-query obj "offset().top" :default-answer 0) :junk-allowed t))

;;;;;;;;;;;;;;;;;
;; offset-left ;;
;;;;;;;;;;;;;;;;;

(defgeneric offset-left (clog-element)
  (:documentation "Position in pixels from left relative to the document."))

(defmethod offset-left ((obj clog-element))
  (parse-integer (jquery-query obj "offset().left" :default-answer 0) :junk-allowed t))


;;;;;;;;;;;;;;;;;;
;; set-geometry ;;
;;;;;;;;;;;;;;;;;;

(defgeneric set-geometry (clog-element &key left top right bottom width height units)
  (:documentation "Change the geometry :LEFT :TOP :RIGHT :BOTTOM :WIDTH :HEIGHT each optional
in UNITS (default :px)"))

(defmethod set-geometry ((obj clog-element) &key left top right bottom width height (units :px))
  (jquery-execute obj (format nil "css({~A~A~A~A~A~A})"
			      (if left
				  (format nil "'left':'~A~A'," left units)
				  "")
			      (if top
				  (format nil "'top':'~A~A'," top units)
				  "")
			      (if right
				  (format nil "'right':'~A~A'," right units)
				  "")
			      (if bottom
				  (format nil "'bottom':'~A~A'," bottom units)
				  "")
			      (if width
				  (format nil "'width':'~A~A'," width units)
				  "")
			      (if height
				  (format nil "'height':'~A~A'," height units)
				  ""))))

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
  (:documentation "Get/Setf box-height. Height based on box sizing."))

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
  (:documentation "Get/Setf box-width. Width based on box sizing."))

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

;;  For reference:
;;  | Margin | Border | Padding | Scroll | [Element] | Scroll | Padding ...
;;
;;  Height and Width of Element are in clog-base
;;  All the following have the advantage of the CSS related size properties
;;  in that the results are always pixels and numeric.

;;;;;;;;;;;;;;;;;;
;; inner-height ;;
;;;;;;;;;;;;;;;;;;

(defgeneric inner-height (clog-element)
  (:documentation "Get/Setf inner-height. Includes padding but not border."))

(defmethod inner-height ((obj clog-element))
  (jquery-query obj "innerHeight()"))

(defgeneric set-inner-height (clog-element value)
  (:documentation "Set inner-height VALUE for CLOG-ELEMENT"))

(defmethod set-inner-height ((obj clog-element) value)
  (jquery-execute obj (format nil "innerHeight('~A')" (escape-string value)))
  value)
(defsetf inner-height set-inner-height)

;;;;;;;;;;;;;;;;;
;; inner-width ;;
;;;;;;;;;;;;;;;;;

(defgeneric inner-width (clog-element)
  (:documentation "Get/Setf inner-width. Includes padding but not border."))

(defmethod inner-width ((obj clog-element))
  (jquery-query obj "innerWidth()"))

(defgeneric set-inner-width (clog-element value)
  (:documentation "Set inner-width VALUE for CLOG-ELEMENT"))

(defmethod set-inner-width ((obj clog-element) value)
  (jquery-execute obj (format nil "innerWidth('~A')" (escape-string value)))
  value)
(defsetf inner-width set-inner-width)

;;;;;;;;;;;;;;;;;;
;; outer-height ;;
;;;;;;;;;;;;;;;;;;

(defgeneric outer-height (clog-element)
  (:documentation "Get outer-height. Includes padding and border but not
margin."))

(defmethod outer-height ((obj clog-element))
  (jquery-query obj "outerHeight()"))

;;;;;;;;;;;;;;;;;
;; outer-width ;;
;;;;;;;;;;;;;;;;;

(defgeneric outer-width (clog-element)
  (:documentation "Get outer-width. Includes padding and border but not margin."))

(defmethod outer-width ((obj clog-element))
  (jquery-query obj "outerWidth()"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; outer-height-to-margin ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric outer-height-to-margin (clog-element)
  (:documentation "Get outer-height-to-margin. Includes padding and border and
margin."))

(defmethod outer-height-to-margin ((obj clog-element))
  (jquery-query obj "outerHeight(true)"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; outer-width-to-margin ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric outer-width-to-margin (clog-element)
  (:documentation "Get outer-width-to-margin. Includes padding and border and
margin."))

(defmethod outer-width-to-margin ((obj clog-element))
  (jquery-query obj "outerWidth(true)"))

;;;;;;;;;;;
;; color ;;
;;;;;;;;;;;

(defgeneric color (clog-element)
  (:documentation "Get/Setf color."))

(defmethod color ((obj clog-element))
  (style obj "color"))

(defgeneric set-color (clog-element value)
  (:documentation "Set color VALUE for CLOG-ELEMENT"))

(defmethod set-color ((obj clog-element) value)
  (setf (style obj "color") value))
(defsetf color set-color)

;;;;;;;;;;;;;
;; opacity ;;
;;;;;;;;;;;;;

(defgeneric opacity (clog-element)
  (:documentation "Get/Setf opacity."))

(defmethod opacity ((obj clog-element))
  (style obj "opacity"))

(defgeneric set-opacity (clog-element value)
  (:documentation "Set opacity VALUE for CLOG-ELEMENT"))

(defmethod set-opacity ((obj clog-element) value)
  (setf (style obj "opacity") value))
(defsetf opacity set-opacity)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; background-attachment ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype background-attachment-type () '(member :scroll :fixed :local))

(defgeneric background-attachment (clog-element)
  (:documentation "Get/Setf background-attachment."))

(defmethod background-attachment ((obj clog-element))
  (style obj "background-attachment"))

(defgeneric set-background-attachment (clog-element value)
  (:documentation "Set background-attachment VALUE for CLOG-ELEMENT"))

(defmethod set-background-attachment ((obj clog-element) value)
  (setf (style obj "background-attachment") value))
(defsetf background-attachment set-background-attachment)

;;;;;;;;;;;;;;;;;;;;;;
;; background-color ;;
;;;;;;;;;;;;;;;;;;;;;;

(defgeneric background-color (clog-element)
  (:documentation "Get/Setf background-color."))

(defmethod background-color ((obj clog-element))
  (style obj "background-color"))

(defgeneric set-background-color (clog-element value)
  (:documentation "Set background-color VALUE for CLOG-ELEMENT"))

(defmethod set-background-color ((obj clog-element) value)
  (setf (style obj "background-color") value))
(defsetf background-color set-background-color)

;;;;;;;;;;;;;;;;;;;;;;
;; background-image ;;
;;;;;;;;;;;;;;;;;;;;;;

(defgeneric background-image (clog-element)
  (:documentation "Get/Setf background-image url. proper syntax is
'url(...)' | nil to clear"))

(defmethod background-image ((obj clog-element))
  (style obj "background-image"))

(defgeneric set-background-image (clog-element value)
  (:documentation "Set background-image VALUE for CLOG-ELEMENT"))

(defmethod set-background-image ((obj clog-element) value)
  (if value
      (setf (style obj "background-image") (format nil "url('~A')" value))
      (setf (style obj "background-image") "none")))
(defsetf background-image set-background-image)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; background-position ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric background-position (clog-element)
  (:documentation "Get/Setf background-position. combination of 2 -
left/right/center/top/bottom | %x %y | x y"))

(defmethod background-position ((obj clog-element))
  (style obj "background-position"))

(defgeneric set-background-position (clog-element value)
  (:documentation "Set background-position VALUE for CLOG-ELEMENT"))

(defmethod set-background-position ((obj clog-element) value)
  (setf (style obj "background-position") value))
(defsetf background-position set-background-position)

;;;;;;;;;;;;;;;;;;;;;;;
;; background-origin ;;
;;;;;;;;;;;;;;;;;;;;;;;

(deftype background-origin-type ()
  '(member :border-box :padding-box :content-box))

(defgeneric background-origin (clog-element)
  (:documentation "Get/Setf background-origin. Background position property
is relative to origin of: padding-box|border-box|content-box"))

(defmethod background-origin ((obj clog-element))
  (style obj "background-origin"))

(defgeneric set-background-origin (clog-element value)
  (:documentation "Set background-origin VALUE for CLOG-ELEMENT"))

(defmethod set-background-origin ((obj clog-element) value)
  (setf (style obj "background-origin") value))
(defsetf background-origin set-background-origin)

;;;;;;;;;;;;;;;;;;;;;;;
;; background-repeat ;;
;;;;;;;;;;;;;;;;;;;;;;;

(deftype background-repeat-type ()
  '(member :repeat-x :repeat-y :repeat :space :round :no-repeat))

(defgeneric background-repeat (clog-element)
  (:documentation "Get/Setf background-repeat. repeat-x | repeat-y |
[ repeat | space | round | no-repeat ]{1,2}"))

(defmethod background-repeat ((obj clog-element))
  (style obj "background-repeat"))

(defgeneric set-background-repeat (clog-element value)
  (:documentation "Set background-repeat VALUE for CLOG-ELEMENT"))

(defmethod set-background-repeat ((obj clog-element) value)
  (setf (style obj "background-repeat") value))
(defsetf background-repeat set-background-repeat)

;;;;;;;;;;;;;;;;;;;;;
;; background-clip ;;
;;;;;;;;;;;;;;;;;;;;;

(deftype background-clip-type ()
  '(member :border-box :padding-box :content-box :text))
    
(defgeneric background-clip (clog-element)
  (:documentation "Get/Setf background-clip. If an element's background extends
underneath its border box, padding box, or content box."))

(defmethod background-clip ((obj clog-element))
  (style obj "background-clip"))

(defgeneric set-background-clip (clog-element value)
  (:documentation "Set background-clip VALUE for CLOG-ELEMENT"))

(defmethod set-background-clip ((obj clog-element) value)
  (setf (style obj "background-clip") value))
(defsetf background-clip set-background-clip)

;;;;;;;;;;;;;;;;;;;;;
;; background-size ;;
;;;;;;;;;;;;;;;;;;;;;

(defgeneric background-size (clog-element)
  (:documentation "Get/Setf background-size.
auto | w h | % = cover of parent | contain"))

(defmethod background-size ((obj clog-element))
  (style obj "background-size"))

(defgeneric set-background-size (clog-element value)
  (:documentation "Set background-size VALUE for CLOG-ELEMENT"))

(defmethod set-background-size ((obj clog-element) value)
  (setf (style obj "background-size") value))
(defsetf background-size set-background-size)

;;;;;;;;;;;;
;; border ;;
;;;;;;;;;;;;

(deftype border-style-type ()
  '(member :none :hidden :dotted :dashed :solid
    :double :groove :ridge :inset :outset))

(defgeneric border (clog-element)
  (:documentation "Get border. <line-width> <border-style> <line-color>"))

(defmethod border ((obj clog-element))
  (style obj "border"))

;;;;;;;;;;;;;;;;
;; set-border ;;
;;;;;;;;;;;;;;;;

(defgeneric set-border (clog-element line-width border-style line-color)
  (:documentation "Set border width style and color.
line-width - size or medium|thin|thick|length|initial|inherit"))

(defmethod set-border ((obj clog-element) line-width border-style line-color)
  (setf (style obj "border") (format nil "~A ~A ~A"
				     line-width border-style line-color)))

;;;;;;;;;;;;;;;;;;;
;; border-radius ;;
;;;;;;;;;;;;;;;;;;;

(defgeneric border-radius (clog-element)
  (:documentation "Get/Setf border-radius."))

(defmethod border-radius ((obj clog-element))
  (style obj "border-radius"))

(defgeneric set-border-radius (clog-element value)
  (:documentation "Set border-radius VALUE for CLOG-ELEMENT"))

(defmethod set-border-radius ((obj clog-element) value)
  (setf (style obj "border-radius") value))
(defsetf border-radius set-border-radius)

;;;;;;;;;;;;;;;;
;; box-shadow ;;
;;;;;;;;;;;;;;;;

(defgeneric box-shadow (clog-element)
  (:documentation "Get/Setf box-shadow."))

(defmethod box-shadow ((obj clog-element))
  (style obj "box-shadow"))

(defgeneric set-box-shadow (clog-element value)
  (:documentation "Set box-shadow. See https://developer.mozilla.org/en-US/docs/Web/CSS/CSS_Background_and_Borders/Box-shadow_generator"))

(defmethod set-box-shadow ((obj clog-element) value)
  (setf (style obj "box-shadow") value))
(defsetf box-shadow set-box-shadow)

;;;;;;;;;;;;;;;;;
;; text-shadow ;;
;;;;;;;;;;;;;s;;;

(defgeneric text-shadow (clog-element)
  (:documentation "Get/Setf text-shadow."))

(defmethod text-shadow ((obj clog-element))
  (style obj "text-shadow"))

(defgeneric set-text-shadow (clog-element value)
  (:documentation "Set text-shadow. See https://developer.mozilla.org/en-US/docs/Web/CSS/CSS_Background_and_Borders/Text-shadow_generator"))

(defmethod set-text-shadow ((obj clog-element) value)
  (setf (style obj "text-shadow") value))
(defsetf text-shadow set-text-shadow)

;;;;;;;;;;;;;
;; outline ;;
;;;;;;;;;;;;;

(deftype outline-style-type ()
  '(member :none :hidden :dotted :dashed :solid :double
    :groove :ridge :inset :outset))

(defgeneric outline (clog-element) 
  (:documentation "Get outline. <line-color> <outline-style> <line-width>"))

(defmethod outline ((obj clog-element))
  (style obj "outline"))

;;;;;;;;;;;;;;;;;
;; set-outline ;;
;;;;;;;;;;;;;;;;;

(defgeneric set-outline (clog-element line-color outline-style line-width)
  (:documentation "Set outline <line-color> <outline-style> <line-width>
line-width -  size or medium|thin|thick|length|initial|inherit"))

(defmethod set-outline ((obj clog-element) line-color outline-style line-width)
  (setf (style obj "outline") (format nil "~A ~A ~A"
				      line-color outline-style line-width)))

;;;;;;;;;;;;
;; margin ;;
;;;;;;;;;;;;

(defgeneric margin (clog-element)
  (:documentation "Get margin."))

(defmethod margin ((obj clog-element))
  (style obj "margin"))

;;;;;;;;;;;;;;;;
;; set-margin ;;
;;;;;;;;;;;;;;;;

(defgeneric set-margin (clog-element top right bottom left)
  (:documentation "Set margins, Each can be - <length>|auto|initial|inherit"))

(defmethod set-margin ((obj clog-element) top right bottom left)
  (setf (style obj "margin") (format nil "~A ~A ~A ~A" top right bottom left)))

;;;;;;;;;;;;;;;;;;;;;
;; set-margin-side ;;
;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-margin-side (clog-element side value)
  (:documentation "Set margin SIDE (:top :right :bottom or :left),
VALUE can be - <length>|auto|initial|inherit"))

(defmethod set-margin-side ((obj clog-element) side value)
  (setf (style obj (format nil "margin-~A" (string-downcase side))) value))


;;;;;;;;;;;;;
;; padding ;;
;;;;;;;;;;;;;

(defgeneric padding (clog-element)
  (:documentation "Get padding."))

(defmethod padding ((obj clog-element))
  (style obj "padding"))

(defgeneric set-padding (clog-element top right bottom left)
  (:documentation "Set padding. Each can be - <length>|initial|inherit"))

(defmethod set-padding ((obj clog-element) top right bottom left)
  (setf (style obj "padding") (format nil "~A ~A ~A ~A"
				      top right bottom left)))
(defsetf padding set-padding)

;;;;;;;;;;;;;;;;;;;;;;
;; set-padding-side ;;
;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-padding-side (clog-element side value)
  (:documentation "Set padding SIDE (:top :right :bottom or :left),
VALUE can be - <length>|auto|initial|inherit"))

(defmethod set-padding-side ((obj clog-element) side value)
  (setf (style obj (format nil "padding-~A" (string-downcase side))) value))

;;;;;;;;;;;;
;; cursor ;;
;;;;;;;;;;;;

(defgeneric cursor (clog-element)
  (:documentation "Get/Setf cursor. Sets the cursor to a standard type or an
image if set to url(url_to_image). When using a url is best to suggest an
alternate cursor, e.g. 'url(url_to_image),auto'
A list of standard cursor types can be found at:
  http://www.w3schools.com/cssref/pr_class_cursor.asp"))

(defmethod cursor ((obj clog-element))
  (style obj "cursor"))

(defgeneric set-cursor (clog-element value)
  (:documentation "Set cursor VALUE for CLOG-ELEMENT"))

(defmethod set-cursor ((obj clog-element) value)
  (setf (style obj "cursor") value))
(defsetf cursor set-cursor)

;;;;;;;;;;
;; font ;;
;;;;;;;;;;

(deftype font-style-type () '(member :normal :italic :unique))

(deftype font-variant-type () '(member :normal :small-caps))

(deftype system-font-type () ; can use with setf of font
  '(member :caption :icon :menu :message-box :small-caption :status-bar))

(defgeneric font (clog-element)
  (:documentation "Get/Setf font."))

(defmethod font ((obj clog-element))
  (style obj "font"))

(defgeneric set-font
    (clog-element font-style font-variant font-weight font-height font-family)
(:documentation "Set font."))

(defmethod set-font
    ((obj clog-element)
     font-style font-variant font-weight font-height font-family)
  (setf (style obj "font")
	(format nil "~A ~A ~A ~A ~A"
		font-style font-variant font-weight font-height font-family)))

(defgeneric set-font-css (clog-element value)
  (:documentation "Set font VALUE for CLOG-ELEMENT"))

(defmethod set-font-css ((obj clog-element) value)
  (setf (style obj "font") value))
(defsetf font set-font-css)

;;;;;;;;;;;;;;;;;;;;
;; text-alignment ;;
;;;;;;;;;;;;;;;;;;;;

(deftype text-alignment-type ()
  '(member :start :end :left :right :center :justify :match-parent))

(defgeneric text-alignment (clog-element)
  (:documentation "Get/Setf text-alignment."))

(defmethod text-alignment ((obj clog-element))
  (style obj "text-align"))

(defgeneric set-text-alignment (clog-element value)
  (:documentation "Set text-alignment VALUE for CLOG-ELEMENT"))

(defmethod set-text-alignment ((obj clog-element) value)
  (setf (style obj "text-align") value))
(defsetf text-alignment set-text-alignment)

;;;;;;;;;;;;;;;;;;;;
;; vertical-align ;;
;;;;;;;;;;;;;;;;;;;;

(deftype vertical-align-type ()
  '(member :baseline :sub :super :text-top :text-bottom :middle :top :bottom))

(defgeneric vertical-align (clog-element)
  (:documentation "Get/Setf vertical-align in table cells or if display
is set to :table-cell or for labels on form elements."))

(defmethod vertical-align ((obj clog-element))
  (style obj "vertical-align"))

(defgeneric set-vertical-align (clog-element value)
  (:documentation "Set vertical-align VALUE for CLOG-ELEMENT"))

(defmethod set-vertical-align ((obj clog-element) value)
  (setf (style obj "vertical-align") value))
(defsetf vertical-align set-vertical-align)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Methods - clog-element
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;
;; add-class ;;
;;;;;;;;;;;;;;;

(defgeneric add-class (clog-element css-class-name)
  (:documentation "add-class."))

(defmethod add-class ((obj clog-element) css-class-name)
  (jquery-execute obj (format nil "addClass('~A')"
			      (escape-string css-class-name))))

;;;;;;;;;;;;;;;;;;
;; remove-class ;;
;;;;;;;;;;;;;;;;;;

(defgeneric remove-class (clog-element css-class-name)
  (:documentation "remove-class."))

(defmethod remove-class ((obj clog-element) css-class-name)
  (jquery-execute obj (format nil "removeClass('~A')"
			      (escape-string css-class-name))))

;;;;;;;;;;;;;;;;;;
;; toggle-class ;;
;;;;;;;;;;;;;;;;;;

(defgeneric toggle-class (clog-element css-class-name)
  (:documentation "toggle-class."))

(defmethod toggle-class ((obj clog-element) css-class-name)
  (jquery-execute obj (format nil "toggleClass('~A')"
			      (escape-string css-class-name))))

;;;;;;;;;;;;;;;;;;;;;
;; remove-from-dom ;;
;;;;;;;;;;;;;;;;;;;;;

(defgeneric remove-from-dom (clog-element)
  (:documentation "Remove CLOG-Element from the DOM."))

(defmethod remove-from-dom ((obj clog-element))
  (jquery-execute obj "remove()"))

;;;;;;;;;;;;;;;;;;;;;;
;; remove-from-clog ;;
;;;;;;;;;;;;;;;;;;;;;;

(defgeneric remove-from-clog (clog-element)
  (:documentation "Remove CLOG-Element from the clog cache on browser."))

(defmethod remove-from-clog ((obj clog-element))
  (js-execute obj (format nil "~A=null;" (script-id obj))))

;;;;;;;;;;;;;
;; destroy ;;
;;;;;;;;;;;;;

(defgeneric destroy (clog-element)
  (:documentation "Remove CLOG-Element from the DOM on browser and clog cache
on browser."))

(defmethod destroy ((obj clog-element))
  (remove-from-dom obj)
  (remove-from-clog obj))

;;;;;;;;;;;
;; click ;;
;;;;;;;;;;;

(defgeneric click (clog-element)
  (:documentation "simulate click."))

(defmethod click ((obj clog-element))
  (jquery-execute obj "click()"))


;;;;;;;;;;;;;;;;;;;;
;; parent-element ;;
;;;;;;;;;;;;;;;;;;;;

(defgeneric parent-element (clog-element)
  (:documentation "Return a new clog-element represeting the parent of
CLOG-ELEMENT."))

(defmethod parent-element ((obj clog-element))
  (attach-as-child obj (jquery-query obj (format nil "parent().prop('id')"))))


;;;;;;;;;;;;;;;;;
;; first-child ;;
;;;;;;;;;;;;;;;;;

(defgeneric first-child (clog-element)
  (:documentation "Traverse to first child element. If Child does not have an
html id than Element_Type will have an ID of undefined and therefore attached
to no actual HTML element."))

(defmethod first-child ((obj clog-element))
  (attach-as-child obj (jquery-query obj "children().first().prop('id')")))

;;;;;;;;;;;;;;;;;;
;; next-sibling ;;
;;;;;;;;;;;;;;;;;;

(defgeneric next-sibling (clog-element)
  (:documentation "Traverse to next sibling element. If Child does not have an
html id than Element_Type will have an ID of undefined and therefore attached
to no actual HTML elemen."))

(defmethod next-sibling ((obj clog-element))
  (attach-as-child obj (jquery-query obj "next().prop('id')")))

;;;;;;;;;;;;;;;;;;;;;;
;; previous-sibling ;;
;;;;;;;;;;;;;;;;;;;;;;

(defgeneric previous-sibling (clog-element)
  (:documentation "Traverse to previous sibling element. If Child does not have an
html id than Element_Type will have an ID of undefined and therefore attached
to no actual HTML elemen."))

(defmethod previous-sibling ((obj clog-element))
  (attach-as-child obj (jquery-query obj "previous().prop('id')")))
