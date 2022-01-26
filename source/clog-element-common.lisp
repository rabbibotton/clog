;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG - The Common Lisp Omnificent GUI                                 ;;;;
;;;; (c) 2020-2022 David Botton                                            ;;;;
;;;; License BSD 3 Clause                                                  ;;;;
;;;;                                                                       ;;;;
;;;; clog-element-commont.lisp                                             ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:in-package :clog)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-a
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-a (clog-element)()
  (:documentation "CLOG A, anchor, Objects."))

;;;;;;;;;;;;;;
;; create-a ;;
;;;;;;;;;;;;;;

(defgeneric create-a (clog-obj
		      &key link content target hidden class html-id auto-place)
  (:documentation "Create a new CLOG-A as child of CLOG-OBJ with :LINK and
:CONTENT (default \"\") and :TARGET (\"_self\") and if :AUTO-PLACE (default t)
place-inside-bottom-of CLOG-OBJ.

   Target of link, name of a frame or:
   _blank  = new window
   _top    = top most frame (full browser window)
   _parent = parent frame or window
   _self   = current frame or window"))

(defmethod create-a ((obj clog-obj)
		     &key (link "#")
		       (content "")
		       (target "_self")
		       (hidden nil)
		       (class nil)
		       (html-id nil) (auto-place t))
  (create-child obj (format nil "<a~A~A target='~A' href='~A'>~A</a>"
			    (if class
				(format nil " class='~A'"
					(escape-string class))
				"")
			    (if hidden
				" style='visibility:hidden;'"
				"")
			    (escape-string target)
			    (escape-string link)
			    (escape-string content))
		:clog-type  'clog-a
		:html-id    html-id
		:auto-place auto-place))

;;;;;;;;;;
;; link ;;
;;;;;;;;;;

(defgeneric link (clog-a)
  (:documentation "Get/Setf the HREF link of the anchor."))

(defmethod link ((obj clog-a))
  (property obj "href"))

(defgeneric set-link (clog-a value)
  (:documentation "Set link VALUE for CLOG-A"))

(defmethod set-link ((obj clog-a) value)
  (setf (property obj "href") value))
(defsetf link set-link)

;;;;;;;;;;;;
;; target ;;
;;;;;;;;;;;;

(defgeneric target (clog-a)
  (:documentation "Get/Setf the link target of the anchor."))

(defmethod target ((obj clog-a))
  (property obj "target"))

(defgeneric set-target (clog-a value)
  (:documentation "Set target VALUE for CLOG-A"))

(defmethod set-target ((obj clog-a) value)
  (setf (property obj "target") value))
(defsetf target set-target)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-br
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-br (clog-element)()
  (:documentation "CLOG BR Objects for line breaks."))

;;;;;;;;;;;;;;;
;; create-br ;;
;;;;;;;;;;;;;;;

(defgeneric create-br (clog-obj &key hidden class html-id auto-place)
  (:documentation "Create a new CLOG-BR as child of CLOG-OBJ that creates a
line break and if :AUTO-PLACE (default t) place-inside-bottom-of CLOG-OBJ"))

(defmethod create-br ((obj clog-obj) &key (hidden nil)
				       (class nil)
				       (html-id nil)
				       (auto-place t))
  (create-child obj (format nil "<br~A~A/>"
			    (if hidden
				" style='visibility:hidden;'"
				"")
			    (if class
				(format nil " class='~A'"
					(escape-string class))
				""))
		:clog-type  'clog-br
		:html-id    html-id
		:auto-place auto-place))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-button
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-button (clog-element)()
  (:documentation "CLOG Button Objects."))

;;;;;;;;;;;;;;;;;;;
;; create-button ;;
;;;;;;;;;;;;;;;;;;;

(defgeneric create-button (clog-obj &key content hidden class html-id auto-place)
  (:documentation "Create a new CLOG-Button as child of CLOG-OBJ with :CONTENT
(default \"\") and if :AUTO-PLACE (default t) place-inside-bottom-of
CLOG-OBJ"))

(defmethod create-button ((obj clog-obj) &key (content "")
					   (hidden nil)
					   (class nil)
					   (html-id nil)
					   (auto-place t))
  (create-child obj (format nil "<button~A~A>~A</button>"
			    (if class
				(format nil " class='~A'"
					(escape-string class))
				"")
			    (if hidden
				" style='visibility:hidden;'"
				"")
			    (escape-string content))
		:clog-type  'clog-button
		:html-id    html-id
		:auto-place auto-place))

;;;;;;;;;;;;;;;
;; disabledp ;;
;;;;;;;;;;;;;;;

(defgeneric disabledp (clog-button)
  (:documentation "Get/Setf disabled status of button."))

(defmethod disabledp ((obj clog-button))
  (js-true-p (property obj "disabled")))

(defgeneric set-disabledp (clog-button value)
  (:documentation "Set editable VALUE for CLOG-BUTTON"))

(defmethod set-disabledp ((obj clog-button) value)
  (if value
      (setf (property obj "disabled") (p-true-js value))
      (remove-attribute obj "disabled")))
(defsetf disabledp set-editable)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-div
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-div (clog-element)()
  (:documentation "CLOG Div Objects."))

;;;;;;;;;;;;;;;;
;; create-div ;;
;;;;;;;;;;;;;;;;

(defgeneric create-div (clog-obj &key content hidden class html-id auto-place)
  (:documentation "Create a new CLOG-Div as child of CLOG-OBJ with :CONTENT
(default \"\") and if :AUTO-PLACE (default t) place-inside-bottom-of
CLOG-OBJ. If hidden is true visiblep is set to nil."))

(defmethod create-div ((obj clog-obj) &key (content "")
					(hidden nil)
					(class nil)
					(html-id nil)
					(auto-place t))
  (create-child obj (format nil "<div~A~A>~A</div>"
			    (if class
				(format nil " class='~A'" (escape-string class))
				"")
			    (if hidden
				" style='visibility:hidden;'"
				"")
			    (escape-string content))
		:clog-type  'clog-div
		:html-id    html-id
		:auto-place auto-place))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-dialog
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-dialog (clog-element)()
  (:documentation "CLOG Dialog Objects."))

;;;;;;;;;;;;;;;;;;;
;; create-dialog ;;
;;;;;;;;;;;;;;;;;;;

(defgeneric create-dialog (clog-obj &key content hidden class html-id auto-place)
  (:documentation "Create a new CLOG-Dialog as child of CLOG-OBJ with :CONTENT
(default \"\") and if :AUTO-PLACE (default t) place-inside-bottom-of
CLOG-OBJ. If hidden is true visiblep is set to nil. Modal does not work on
firefox and does not work at all on IE."))

(defmethod create-dialog ((obj clog-obj) &key (content "")
					(hidden nil)
					(class nil)
					(html-id nil)
					(auto-place t))
  (create-child obj (format nil "<dialog~A~A>~A</dialog>"
			    (if class
				(format nil " class='~A'" (escape-string class))
				"")
			    (if hidden
				" style='visibility:hidden;'"
				"")
			    (escape-string content))
		:clog-type  'clog-dialog
		:html-id    html-id
		:auto-place auto-place))

;;;;;;;;;;;;;;;;;;
;; return-value ;;
;;;;;;;;;;;;;;;;;;

(defgeneric return-value (clog-dialog)
  (:documentation "Get/Setf return-value of dialog."))

(defmethod return-value ((obj clog-dialog))
  (property obj "returnValue"))

(defgeneric set-return-value (clog-dialog value)
  (:documentation "Set return-value VALUE for CLOG-DIALOG"))

(defmethod set-return-value ((obj clog-dialog) value)
  (setf (property obj "returnValue") value))
(defsetf return-value set-return-value)

;;;;;;;;;;;;;;;;;;
;; dialog-openp ;;
;;;;;;;;;;;;;;;;;;

(defgeneric dialog-openp (clog-dialog)
  (:documentation "Get/Setf dialog-openp. Will show dialog "))

(defmethod dialog-openp ((obj clog-dialog))
  (unless (equalp (attribute obj "open") "undefined")
    t))

(defgeneric set-dialog-openp (clog-dialog value)
  (:documentation "Set dialog-openp VALUE for CLOG-DIALOG"))

(defmethod set-dialog-openp ((obj clog-dialog) value)
  (if value
      (setf (attribute obj "open") t)
      (remove-attribute obj "open")))
(defsetf dialog-openp set-dialog-openp)

;;;;;;;;;;;;;;;;;
;; show-dialog ;;
;;;;;;;;;;;;;;;;;

(defgeneric show-dialog (clog-dialog &key modal)
  (:documentation "Close dialog."))

(defmethod show-dialog ((obj clog-dialog) &key (modal nil))
  (if modal
      (jquery-execute obj (format nil "showModal()"))
      (jquery-execute obj (format nil "show()"))))

;;;;;;;;;;;;;;;;;;
;; close-dialog ;;
;;;;;;;;;;;;;;;;;;

(defgeneric close-dialog (clog-dialog)
  (:documentation "Close dialog."))

(defmethod close-dialog ((obj clog-dialog))
  (jquery-execute obj (format nil "close()")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-hr
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-hr (clog-element)()
  (:documentation "CLOG HR Objects for horizontal rules."))

;;;;;;;;;;;;;;;
;; create-hr ;;
;;;;;;;;;;;;;;;

(defgeneric create-hr (clog-obj &key hidden class html-id auto-place)
  (:documentation "Create a new CLOG-HR as child of CLOG-OBJ that creates a
horizontal rule (line) and if :AUTO-PLACE (default t) place-inside-bottom-of
CLOG-OBJ"))

(defmethod create-hr ((obj clog-obj) &key (hidden nil)
				       (class nil)
				       (html-id nil)
				       (auto-place t))
  (create-child obj (format nil "<hr~A~A/>"
			    (if hidden
				" style='visibility:hidden;'"
				"")
			    (if class
				(format nil " class='~A'"
					(escape-string class))
				""))
		:clog-type  'clog-hr
		:html-id    html-id
		:auto-place auto-place))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-img
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-img (clog-element)()
  (:documentation "CLOG Img Objects."))

;;;;;;;;;;;;;;;;
;; create-img ;;
;;;;;;;;;;;;;;;;

(defgeneric create-img (clog-obj
			&key url-src alt-text
			  hidden class html-id auto-place)
  (:documentation "Create a new CLOG-Img as child of CLOG-OBJ with :URL-SRC
(default \"\") and :ALT-TEXT (default \"\") if :AUTO-PLACE (default t)
place-inside-bottom-of CLOG-OBJ. Use width and height properties before
placing image to constrain image size."))

(defmethod create-img ((obj clog-obj) &key
					(url-src "")
					(alt-text "")
					(hidden nil)
					(class nil)
					(html-id nil)
					(auto-place t))
  (create-child obj (format nil "<img~A~A src='~A' alt='~A'>)"
			    (if hidden
				" style='visibility:hidden;'"
				"")
			    (if class
				(format nil " class='~A'"
					(escape-string class))
				"")
			    (escape-string url-src)
			    (escape-string alt-text))
		:clog-type  'clog-img
		:html-id    html-id
		:auto-place auto-place))

;;;;;;;;;;;;;
;; url-src ;;
;;;;;;;;;;;;;

(defgeneric url-src (clog-img)
  (:documentation "Get/Setf the url-src of the img."))

(defmethod url-src ((obj clog-img))
  (property obj "src"))

(defgeneric set-url-src (clog-img value)
  (:documentation "Set url-src VALUE for CLOG-IMG"))

(defmethod set-url-src ((obj clog-img) value)
  (setf (property obj "src") value))
(defsetf url-src set-url-src)

;;;;;;;;;;;;;;
;; alt-text ;;
;;;;;;;;;;;;;;

(defgeneric alt-text (clog-img)
  (:documentation "Get/Setf the alt-text of the img."))

(defmethod alt-text ((obj clog-img))
  (attribute obj "alt"))

(defgeneric set-alt-text (clog-img value)
  (:documentation "Set alt-text VALUE for CLOG-IMG"))

(defmethod set-alt-text ((obj clog-img) value)
  (setf (attribute obj "alt") value))
(defsetf alt-text set-alt-text)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-meter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-meter (clog-element)()
  (:documentation "CLOG Meter Objects."))

;;;;;;;;;;;;;;;;;;
;; create-meter ;;
;;;;;;;;;;;;;;;;;;

(defgeneric create-meter (clog-obj &key value high low maximum minimum optimum
				     hidden
				     class
				     html-id
				     auto-place)
  (:documentation "Create a new CLOG-Meter as child of CLOG-OBJ with VALUE
(default 0) HIGH (default 100) LOW (default 0) MAXIMUM (default 100) MINIMUM
(default 0) OPTIMUM (default 50) and if :AUTO-PLACE (default t)
place-inside-bottom-of CLOG-OBJ."))

(defmethod create-meter ((obj clog-obj) &key
					  (value 0)
					  (high 100)
					  (low 0)
					  (maximum 100)
					  (minimum 0)
					  (optimum 50)
					  (hidden nil)
					  (class nil)
					  (html-id nil)
					  (auto-place t))
  (create-child obj (format nil
	    "<meter value=~A high=~A low=~A max=~A min=~A optimum=~A~A~A/>"
			    value high low maximum minimum optimum
			    (if hidden
				" style='visibility:hidden;'"
				"")
			    (if class
				(format nil " class='~A'"
					(escape-string class))
				""))
		:clog-type  'clog-meter
		:html-id    html-id
		:auto-place auto-place))

;;;;;;;;;;;
;; value ;;
;;;;;;;;;;;

(defgeneric value (clog-meter)
  (:documentation "Get/Setf the value of the meter."))

(defmethod value ((obj clog-meter))
  (property obj "value"))

(defgeneric set-value (clog-meter value)
  (:documentation "Set value VALUE for CLOG-METER"))

(defmethod set-value ((obj clog-meter) value)
  (setf (property obj "value") value))
(defsetf value set-value)

;;;;;;;;;;
;; high ;;
;;;;;;;;;;

(defgeneric high (clog-meter)
  (:documentation "Get/Setf the high of the meter."))

(defmethod high ((obj clog-meter))
  (property obj "high"))

(defgeneric set-high (clog-meter high)
  (:documentation "Set high HIGH for CLOG-METER"))

(defmethod set-high ((obj clog-meter) high)
  (setf (property obj "high") high))
(defsetf high set-high)

;;;;;;;;;
;; low ;;
;;;;;;;;;

(defgeneric low (clog-meter)
  (:documentation "Get/Setf the low of the meter."))

(defmethod low ((obj clog-meter))
  (property obj "low"))

(defgeneric set-low (clog-meter low)
  (:documentation "Set low LOW for CLOG-METER"))

(defmethod set-low ((obj clog-meter) low)
  (setf (property obj "low") low))
(defsetf low set-low)

;;;;;;;;;;;;;
;; maximum ;;
;;;;;;;;;;;;;

(defgeneric maximum (clog-meter)
  (:documentation "Get/Setf the maximum of the meter."))

(defmethod maximum ((obj clog-meter))
  (property obj "max"))

(defgeneric set-maximum (clog-meter maximum)
  (:documentation "Set maximum MAXIMUM for CLOG-METER"))

(defmethod set-maximum ((obj clog-meter) maximum)
  (setf (property obj "max") maximum))
(defsetf maximum set-maximum)

;;;;;;;;;;;;;
;; minimum ;;
;;;;;;;;;;;;;

(defgeneric minimum (clog-meter)
  (:documentation "Get/Setf the minimum of the meter."))

(defmethod minimum ((obj clog-meter))
  (property obj "min"))

(defgeneric set-minimum (clog-meter minimum)
  (:documentation "Set minimum MINIMUM for CLOG-METER"))

(defmethod set-minimum ((obj clog-meter) minimum)
  (setf (property obj "min") minimum))
(defsetf minimum set-minimum)

;;;;;;;;;;;;;
;; optimum ;;
;;;;;;;;;;;;;

(defgeneric optimum (clog-meter)
  (:documentation "Get/Setf the optimum of the meter."))

(defmethod optimum ((obj clog-meter))
  (property obj "optimum"))

(defgeneric set-optimum (clog-meter optimum)
  (:documentation "Set optimum OPTIMUM for CLOG-METER"))

(defmethod set-optimum ((obj clog-meter) optimum)
  (setf (property obj "optimum") optimum))
(defsetf optimum set-optimum)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-progress-bar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-progress-bar (clog-element)()
  (:documentation "CLOG Progress-Bar Objects."))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; create-progress-bar ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric create-progress-bar (clog-obj
				 &key value maximum
				   hidden class html-id auto-place)
  (:documentation "Create a new CLOG-Progress-Bar as child of CLOG-OBJ with
VALUE (default 0) MAXIMUM (default 100) and if :AUTO-PLACE (default t)
place-inside-bottom-of CLOG-OBJ."))

(defmethod create-progress-bar ((obj clog-obj) &key (value 0)
						 (maximum 100)
						 (hidden nil)
						 (class nil)
						 (html-id nil)
						 (auto-place t))
  (create-child obj (format nil "<progress value=~A max=~A ~A~A/>"
			    value maximum
			    (if hidden
				" style='visibility:hidden;'"
				"")
			    (if class
				(format nil " class='~A'"
					(escape-string class))
				""))
		:clog-type  'clog-progress-bar
		:html-id    html-id
		:auto-place auto-place))

;;;;;;;;;;;
;; value ;;
;;;;;;;;;;;

(defgeneric value (clog-progress-bar)
  (:documentation "Get/Setf the value of the progress-bar."))

(defmethod value ((obj clog-progress-bar))
  (property obj "value"))

(defgeneric set-value (clog-progress-bar value)
  (:documentation "Set value VALUE for CLOG-PROGRESS-BAR"))

(defmethod set-value ((obj clog-progress-bar) value)
  (setf (property obj "value") value))
(defsetf value set-value)

;;;;;;;;;;;;;
;; maximum ;;
;;;;;;;;;;;;;

(defgeneric maximum (clog-progress-bar)
  (:documentation "Get/Setf the maximum of the progress-bar."))

(defmethod maximum ((obj clog-progress-bar))
  (property obj "max"))

(defgeneric set-maximum (clog-progress-bar maximum)
  (:documentation "Set maximum MAXIMUM for CLOG-PROGRESS-BAR"))

(defmethod set-maximum ((obj clog-progress-bar) maximum)
  (setf (property obj "max") maximum))
(defsetf maximum set-maximum)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-p
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-p (clog-element)()
  (:documentation "CLOG P Objects."))

;;;;;;;;;;;;;;
;; create-p ;;
;;;;;;;;;;;;;;

(defgeneric create-p (clog-obj &key content hidden class html-id auto-place)
  (:documentation "Create a new CLOG-P as child of CLOG-OBJ with :CONTENT
(default \"\") and if :AUTO-PLACE (default t) place-inside-bottom-of
CLOG-OBJ"))

(defmethod create-p ((obj clog-obj) &key (content "")
				      (hidden nil)
				      (class nil)
				      (html-id nil)
				      (auto-place t))
  (create-child obj (format nil "<p~A~A>~A</p>"
			    (if hidden
				" style='visibility:hidden;'"
				"")
			    (if class
				(format nil " class='~A'"
					(escape-string class))
				"")
			    (escape-string content))
		:clog-type  'clog-p
		:html-id    html-id
		:auto-place auto-place))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-span
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-span (clog-element)()
  (:documentation "CLOG Span Objects."))

;;;;;;;;;;;;;;;;;
;; create-span ;;
;;;;;;;;;;;;;;;;;

(defgeneric create-span (clog-obj &key content hidden class html-id auto-place)
  (:documentation "Create a new CLOG-Span as child of CLOG-OBJ with CONTENT
and if :AUTO-PLACE (default t) place-inside-bottom-of CLOG-OBJ. A span is
an inline element while a div is a block element (one that takes up the entire
browser width)."))

(defmethod create-span ((obj clog-obj) &key (content "")
					 (hidden nil)
					 (class nil)
					 (html-id nil)
					 (auto-place t))
  (create-child obj (format nil "<span~A~A>~A</span>"
			    (if hidden
				" style='visibility:hidden;'"
				"")
			    (if class
				(format nil " class='~A'"
					(escape-string class))
				"")
			    (escape-string content))
		:clog-type  'clog-span
		:html-id    html-id
		:auto-place auto-place))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-section
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-section (clog-element)()
  (:documentation "CLOG Section Objects."))

;;;;;;;;;;;;;;;;;;;;
;; create-section ;;
;;;;;;;;;;;;;;;;;;;;

(deftype section-type () '(member :address :article :aside :header :main :nav
			   :p :pre :section :blockquote :h1 :h2 :h3 :h4 :h5 :h6
			   :hgroup))

(defgeneric create-section (clog-obj section
			    &key content hidden class html-id auto-place)
  (:documentation "Create a new CLOG-Section of section type  as child of
CLOG-OBJ with CONTENT and if :AUTO-PLACE (default t) place-inside-bottom-of
CLOG-OBJ"))

(defmethod create-section ((obj clog-obj) section
			   &key (content "")
			     (hidden nil)
			     (class nil)
			     (html-id nil) (auto-place t))
  (create-child obj (format nil "<~A~A~A>~A</~A>"
			    section
			    (if hidden
				" style='visibility:hidden;'"
				"")
			    (if class
				(format nil " class='~A'"
					(escape-string class))
				"")
			    (escape-string content)
			    section)
		:clog-type  'clog-section
		:html-id    html-id
		:auto-place auto-place))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-phrase
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-phrase (clog-element)()
  (:documentation "CLOG Phrase Objects."))

;;;;;;;;;;;;;;;;;;;
;; create-phrase ;;
;;;;;;;;;;;;;;;;;;;

(deftype phrase-type () '(member :abbr :code :strong :em :dfn :samp :kbd :var
			  :marked :del :ins :s :q :big :small :time :tt :cite
			  :i :b :u :sub :su :center))

(defgeneric create-phrase (clog-obj phrase
			   &key content hidden class html-id auto-place)
  (:documentation "Create a new CLOG-Phrase of phrase type  as child of
CLOG-OBJ with CONTENT and if :AUTO-PLACE (default t) place-inside-bottom-of
CLOG-OBJ"))

(defmethod create-phrase ((obj clog-obj) phrase
			  &key (content "")
			    (hidden nil)
			    (class nil)
			    (html-id nil)
			    (auto-place t))
  (create-child obj (format nil "<~A~A~A>~A</~A>"
			    phrase
			    (if hidden
				" style='visibility:hidden;'"
				"")
			    (if class
				(format nil " class='~A'"
					(escape-string class))
				"")
			    (escape-string content)
			    phrase)
		:clog-type  'clog-phrase
		:html-id    html-id
		:auto-place auto-place))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-ordered-list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-ordered-list (clog-element)()
  (:documentation "CLOG Ordered-List Objects."))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; create-ordered-list ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric create-ordered-list (clog-obj &key hidden class html-id auto-place)
  (:documentation "Create a new CLOG-Ordered-List as child of CLOG-OBJ
and if :AUTO-PLACE (default t) place-inside-bottom-of CLOG-OBJ"))

(defmethod create-ordered-list ((obj clog-obj)
				&key (hidden nil)
				  (class nil)
				  (html-id nil)
				  (auto-place t))
  (create-child obj (format nil "<ol~A~A/>"
			    (if hidden
				" style='visibility:hidden;'"
				"")
			    (if class
				(format nil " class='~A'"
					(escape-string class))
				""))
		:clog-type  'clog-ordered-list
		:html-id    html-id
		:auto-place auto-place))

;;;;;;;;;;;;;;;
;; list-kind ;;
;;;;;;;;;;;;;;;

(deftype list-kind-type () '(member :disc :armenian :circle :cjk-ideographic
			     :decimal :decimal-leading-zero :georgian :hebrew
			     :hiragana :hiragana-iroha :katakana
			     :katakana-iroha :lower-alpha :lower-greek
			     :lower-latin :lower-roman :none :square
			     :upper-alpha :upper-latin :upper-roman))

(defgeneric list-kind (clog-ordered-list)
  (:documentation "Get/Setf list list-kind."))

(defmethod list-kind ((obj clog-ordered-list))
  (style obj "list-style-type"))

(defgeneric set-list-kind (clog-ordered-list value)
  (:documentation "Set list-kind VALUE for  CLOG-ORDERED-LIST"))

(defmethod set-list-kind ((obj clog-ordered-list) value)
  (setf (style obj "list-style-type") value))
(defsetf list-kind set-list-kind)

;;;;;;;;;;;;;;;;;;;
;; list-location ;;
;;;;;;;;;;;;;;;;;;;

(deftype list-location-type () '(member :inside :outside))

(defgeneric list-location (clog-ordered-list)
  (:documentation "Get/Setf list list-location. Default
is outside."))

(defmethod list-location ((obj clog-ordered-list))
  (style obj "list-style-position"))

(defgeneric set-list-location (clog-ordered-list value)
  (:documentation "Set list-location VALUE for CLOG-ORDERED-LIST"))

(defmethod set-list-location ((obj clog-ordered-list) value)
  (setf (style obj "list-style-position") value))
(defsetf list-location set-list-location)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-unordered-list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-unordered-list (clog-element)()
  (:documentation "CLOG Unordered-List Objects."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; create-unordered-list ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric create-unordered-list (clog-obj
				   &key hidden class html-id auto-place)
  (:documentation "Create a new CLOG-Unordered-List as child of CLOG-OBJ
and if :AUTO-PLACE (default t) place-inside-bottom-of CLOG-OBJ"))

(defmethod create-unordered-list ((obj clog-obj) &key (hidden nil)
						   (class nil)
						   (html-id nil)
						   (auto-place t))
  (create-child obj (format nil "<ul~A~A/>"
			    (if hidden
				" style='visibility:hidden;'"
				"")
			    (if class
				(format nil " class='~A'"
					(escape-string class))
				""))
		:clog-type  'clog-unordered-list
		:html-id    html-id
		:auto-place auto-place))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-list-item
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-list-item (clog-element)()
  (:documentation "CLOG List-Item Objects."))

;;;;;;;;;;;;;;;;;;;;;;
;; create-list-item ;;
;;;;;;;;;;;;;;;;;;;;;;

(defgeneric create-list-item (clog-obj &key content class html-id auto-place)
  (:documentation "Create a new CLOG-List-Item as child of CLOG-OBJ
and if :AUTO-PLACE (default t) place-inside-bottom-of CLOG-OBJ"))

(defmethod create-list-item ((obj clog-obj) &key (content "")
					      (class nil)
					      (html-id nil) (auto-place t))
  (create-child obj (format nil "<li~A>~A</li>"
			    (if class
				(format nil " class='~A'"
					(escape-string class))
				"")
			    (escape-string content))
		:clog-type  'clog-list-item
		:html-id    html-id
		:auto-place auto-place))

;;;;;;;;;;;;;;;;
;; item-value ;;
;;;;;;;;;;;;;;;;

(defgeneric item-value (clog-list-item)
  (:documentation "Get/Setf list item-value."))

(defmethod item-value ((obj clog-list-item))
  (property obj "value"))

(defgeneric set-item-value (clog-list-item value)
  (:documentation "Set item-value VALUE for  CLOG-LIST-ITEM"))

(defmethod set-item-value ((obj clog-list-item) value)
  (setf (property obj "value") value))
(defsetf item-value set-item-value)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-definition-list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-definition-list (clog-element)()
  (:documentation "CLOG Definition-List Objects."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; create-definition-list ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric create-definition-list (clog-obj
				    &key hidden class html-id auto-place)
  (:documentation "Create a new CLOG-Definition-List as child of CLOG-OBJ
and if :AUTO-PLACE (default t) place-inside-bottom-of CLOG-OBJ"))

(defmethod create-definition-list ((obj clog-obj) &key (hidden nil)
						    (class nil)
						    (html-id nil)
						    (auto-place t))
  (create-child obj (format nil "<dl~A~A/>"
			    (if hidden
				" style='visibility:hidden;'"
				"")
			    (if class
				(format nil " class='~A'" (escape-string class))
				""))
		:clog-type  'clog-definition-list
		:html-id    html-id
		:auto-place auto-place))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-term
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-term (clog-element)()
  (:documentation "CLOG Term Objects."))

;;;;;;;;;;;;;;;;;
;; create-term ;;
;;;;;;;;;;;;;;;;;

(defgeneric create-term (clog-obj
			 &key content
			   hidden class html-id auto-place)
  (:documentation "Create a new CLOG-Term as child of CLOG-OBJ
and if :AUTO-PLACE (default t) place-inside-bottom-of CLOG-OBJ"))

(defmethod create-term ((obj clog-obj)
			&key (hidden nil)
			  (content "")
			  (class nil)
			  (html-id nil) (auto-place t))
  (create-child obj (format nil "<dt~A~A>~A</dt>"
			    (if hidden
				" style='visibility:hidden;'"
				"")
			    (if class
				(format nil " class='~A'"
					(escape-string class))
				"")
			    (escape-string content))
		:clog-type  'clog-term
		:html-id    html-id
		:auto-place auto-place))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-description
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-description (clog-element)()
  (:documentation "CLOG Description Objects."))

;;;;;;;;;;;;;;;;;;;;;;;;
;; create-description ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric create-description (clog-obj
				&key hidden content class html-id auto-place)
  (:documentation "Create a new CLOG-Description as child of CLOG-OBJ
and if :AUTO-PLACE (default t) place-inside-bottom-of CLOG-OBJ"))

(defmethod create-description ((obj clog-obj)
			       &key (content "")
				 (hidden nil)
				 (class nil)
				 (html-id nil)
				 (auto-place t))
  (create-child obj (format nil "<dd~A~A>~A</dd>"
			    (if hidden
				" style='visibility:hidden;'"
				"")
			    (if class
				(format nil " class='~A'"
					(escape-string class))
				"")
			    (escape-string content))
		:clog-type  'clog-description
		:html-id    html-id
		:auto-place auto-place))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-table
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-table (clog-element)()
  (:documentation "CLOG Table Objects."))

;;;;;;;;;;;;;;;;;;
;; create-table ;;
;;;;;;;;;;;;;;;;;;

(defgeneric create-table (clog-obj &key hidden class html-id auto-place)
  (:documentation "Create a new CLOG-Table as child of CLOG-OBJ
and if :AUTO-PLACE (default t) place-inside-bottom-of CLOG-OBJ"))

(defmethod create-table ((obj clog-obj)
			 &key (hidden nil)
			   (class nil) (html-id nil) (auto-place t))
  (create-child obj (format nil "<table~A~A/>"
			    (if hidden
				" style='visibility:hidden;'"
				"")
			    (if class
				(format nil " class='~A'"
					(escape-string class))
				""))
		:clog-type  'clog-table
		:html-id    html-id
		:auto-place auto-place))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-table-row
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-table-row (clog-element)()
  (:documentation "CLOG Table-Row Objects."))

;;;;;;;;;;;;;;;;;;;;;;
;; create-table-row ;;
;;;;;;;;;;;;;;;;;;;;;;

(defgeneric create-table-row (clog-obj &key hidden class html-id auto-place)
  (:documentation "Create a new CLOG-Table-Row as child of CLOG-OBJ
and if :AUTO-PLACE (default t) place-inside-bottom-of CLOG-OBJ"))

(defmethod create-table-row ((obj clog-obj)
			     &key (hidden nil)
			       (class nil) (html-id nil) (auto-place t))
  (create-child obj (format nil "<tr~A~A/>"
			    (if hidden
				" style='visibility:hidden;'"
				"")
			    (if class
				(format nil " class='~A'"
					(escape-string class))
				""))
		:clog-type  'clog-table-row
		:html-id    html-id
		:auto-place auto-place))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-table-column
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-table-column (clog-table-row)()
  (:documentation "CLOG Table-Column Objects."))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; create-table-column ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric create-table-column (clog-obj &key content
					    column-span
					    row-span
					    hidden
					    class
					    html-id
					    auto-place)
  (:documentation "Create a new CLOG-Table-Column as child of CLOG-OBJ
and if :AUTO-PLACE (default t) place-inside-bottom-of CLOG-OBJ"))

(defmethod create-table-column ((obj clog-obj) &key (content "")
						 (column-span 1)
						 (row-span 1)
						 (hidden nil)
						 (class nil)
						 (html-id nil)
						 (auto-place t))
  (create-child obj (format nil "<td colspan=~A rowspan=~A~A~A>~A</td>"
			    column-span
			    row-span
			    (if hidden
				" style='visibility:hidden;'"
				"")
			    (if class
				(format nil " class='~A'"
					(escape-string class))
				"")
			    (escape-string content))
		:clog-type  'clog-table-column
		:html-id    html-id
		:auto-place auto-place))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-table-heading
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-table-heading (clog-table-row)()
  (:documentation "CLOG Table-Heading Objects."))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; create-table-heading ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric create-table-heading (clog-obj &key content
					     column-span
					     row-span
					     hidden
					     class
					     html-id
					     auto-place)
  (:documentation "Create a new CLOG-Table-Heading as child of CLOG-OBJ
and if :AUTO-PLACE (default t) place-inside-bottom-of CLOG-OBJ"))

(defmethod create-table-heading ((obj clog-obj) &key (content "")
						  (column-span 1)
						  (row-span 1)
						  (hidden nil)
						  (class nil)
						  (html-id nil)
						  (auto-place t))
  (create-child obj (format nil "<th colspan=~A rowspan=~A~A~A>~A</th>"
			    column-span
			    row-span
			    (if hidden
				" style='visibility:hidden;'"
				"")
			    (if class
				(format nil " class='~A'"
					(escape-string class))
				"")
			    (escape-string content))
		:clog-type  'clog-table-heading
		:html-id    html-id
		:auto-place auto-place))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-table-head
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-table-head (clog-table)()
  (:documentation "CLOG Table-Head Objects."))

;;;;;;;;;;;;;;;;;;;;;;;
;; create-table-head ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric create-table-head (clog-obj &key hidden class html-id auto-place)
  (:documentation "Create a new CLOG-Table-Head as child of CLOG-OBJ
and if :AUTO-PLACE (default t) place-inside-bottom-of CLOG-OBJ"))

(defmethod create-table-head ((obj clog-obj)
			      &key (hidden nil)
				(class nil) (html-id nil) (auto-place t))
  (create-child obj (format nil "<thead~A~A/>"
			    (if hidden
				" style='visibility:hidden;'"
				"")
			    (if class
				(format nil " class='~A'"
					(escape-string class))
				""))
		:clog-type  'clog-table-head
		:html-id    html-id
		:auto-place auto-place))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-table-body
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-table-body (clog-table)()
  (:documentation "CLOG Table-Body Objects."))

;;;;;;;;;;;;;;;;;;;;;;;
;; create-table-body ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric create-table-body (clog-obj &key hidden class html-id auto-place)
  (:documentation "Create a new CLOG-Table-Body as child of CLOG-OBJ
and if :AUTO-PLACE (default t) place-inside-bottom-of CLOG-OBJ"))

(defmethod create-table-body ((obj clog-obj)
			      &key (hidden nil)
				(class nil) (html-id nil) (auto-place t))
  (create-child obj (format nil "<tbody~A~A/>"
			    (if hidden
				" style='visibility:hidden;'"
				"")
			    (if class
				(format nil " class='~A'"
					(escape-string class))
				""))
		:clog-type  'clog-table-body
		:html-id    html-id
		:auto-place auto-place))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-table-caption
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-table-caption (clog-table)()
  (:documentation "CLOG Table-Caption Objects."))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; create-table-caption ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric create-table-caption (clog-obj
				  &key content
				    hidden class html-id auto-place)
  (:documentation "Create a new CLOG-Table-Caption as child of CLOG-OBJ
and if :AUTO-PLACE (default t) place-inside-bottom-of CLOG-OBJ"))

(defmethod create-table-caption ((obj clog-obj)
				 &key (content "")
				   (hidden nil)
				   (class nil)
				   (html-id nil)
				   (auto-place t))
  (create-child obj (format nil "<caption~A~A/>~A</caption>"
			    (if hidden
				" style='visibility:hidden;'"
				"")
			    (if class
				(format nil " class='~A'"
					(escape-string class))
				"")
			    (escape-string content))
		:clog-type  'clog-table-caption
		:html-id    html-id
		:auto-place auto-place))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-table-footer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-table-footer (clog-table)()
  (:documentation "CLOG Table-Footer Objects."))

;;;;;;;;;;;;;;;;;;;;;;;
;; create-table-footer ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric create-table-footer (clog-obj &key hidden
					    class html-id auto-place)
  (:documentation "Create a new CLOG-Table-Footer as child of CLOG-OBJ
and if :AUTO-PLACE (default t) place-inside-bottom-of CLOG-OBJ"))

(defmethod create-table-footer ((obj clog-obj)
				&key (hidden nil)
				  (class nil) (html-id nil) (auto-place t))
  (create-child obj (format nil "<tfoot~A~A/>"
			    (if hidden
				" style='visibility:hidden;'"
				"")
			    (if class
				(format nil " class='~A'"
					(escape-string class))
				""))
		:clog-type  'clog-table-footer
		:html-id    html-id
		:auto-place auto-place))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-table-column-group
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-table-column-group (clog-table)()
  (:documentation "CLOG Table-Column-Group Objects."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; create-table-column-group ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric create-table-column-group (clog-obj
				       &key hidden class html-id auto-place)
  (:documentation "Create a new CLOG-Table-Column-Group as child of CLOG-OBJ
and if :AUTO-PLACE (default t) place-inside-bottom-of CLOG-OBJ"))

(defmethod create-table-column-group ((obj clog-obj)
				      &key (hidden nil)
					(class nil) (html-id nil) (auto-place t))
  (create-child obj (format nil "<colgroup~A~A/>"
			    (if hidden
				" style='visibility:hidden;'"
				"")
			    (if class
				(format nil " class='~A'"
					(escape-string class))
				""))
		:clog-type  'clog-table-column-group
		:html-id    html-id
		:auto-place auto-place))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-table-column-group-item
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-table-column-group-item (clog-table-column-group)()
  (:documentation "CLOG Table-Column-Group-Item Objects."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; create-table-column-group-item ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric create-table-column-group-item (clog-obj
					    &key column-span
					      hidden class html-id auto-place)
  (:documentation "Create a new CLOG-Table-Column-Group-Item as child of CLOG-OBJ
and if :AUTO-PLACE (default t) place-inside-bottom-of CLOG-OBJ"))

(defmethod create-table-column-group-item ((obj clog-obj)
					   &key (column-span 1)
					     (hidden nil)
					     (class nil)
					     (html-id nil)
					     (auto-place t))
  (create-child obj (format nil "<col span=~A~A~A/>"
			    column-span
			    (if hidden
				" style='visibility:hidden;'"
				"")
			    (if class
				(format nil " class='~A'"
					(escape-string class))
				""))
		:clog-type  'clog-table-column-group-item
		:html-id    html-id
		:auto-place auto-place))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-details
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-details (clog-element)()
  (:documentation "CLOG Details Objects."))

;;;;;;;;;;;;;;;;;;;;
;; create-details ;;
;;;;;;;;;;;;;;;;;;;;

(defgeneric create-details (clog-obj &key content hidden class html-id auto-place)
  (:documentation "Create a new CLOG-Details as child of CLOG-OBJ with :CONTENT
(default \"\") and if :AUTO-PLACE (default t) place-inside-bottom-of
CLOG-OBJ. If hidden is true visiblep is set to nil."))

(defmethod create-details ((obj clog-obj) &key (content "")
					(hidden nil)
					(class nil)
					(html-id nil)
					(auto-place t))
  (create-child obj (format nil "<details~A~A>~A</details>"
			    (if class
				(format nil " class='~A'" (escape-string class))
				"")
			    (if hidden
				" style='visibility:hidden;'"
				"")
			    (escape-string content))
		:clog-type  'clog-details
		:html-id    html-id
		:auto-place auto-place))

;;;;;;;;;;;;;;;;;;;
;; details-openp ;;
;;;;;;;;;;;;;;;;;;;

(defgeneric details-openp (clog-details)
  (:documentation "Get/Setf details-openp. Will show details "))

(defmethod details-openp ((obj clog-details))
  (unless (equalp (attribute obj "open") "undefined")
    t))

(defgeneric set-details-openp (clog-details value)
  (:documentation "Set details-openp VALUE for CLOG-DETAILS"))

(defmethod set-details-openp ((obj clog-details) value)
  (if value
      (setf (attribute obj "open") t)
      (remove-attribute obj "open")))
(defsetf details-openp set-details-openp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-summary
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-summary (clog-element)()
  (:documentation "CLOG Summary Objects."))

;;;;;;;;;;;;;;;;;;;;
;; create-summary ;;
;;;;;;;;;;;;;;;;;;;;

(defgeneric create-summary (clog-obj &key content hidden class html-id auto-place)
  (:documentation "Create a new CLOG-Summary as child of CLOG-OBJ with :CONTENT
(default \"\") and if :AUTO-PLACE (default t) place-inside-bottom-of
CLOG-OBJ. If hidden is true visiblep is set to nil. Use inside a CLOG-DETAILS
object for drop reveal."))

(defmethod create-summary ((obj clog-obj) &key (content "")
					(hidden nil)
					(class nil)
					(html-id nil)
					(auto-place t))
  (create-child obj (format nil "<summary~A~A>~A</summary>"
			    (if class
				(format nil " class='~A'" (escape-string class))
				"")
			    (if hidden
				" style='visibility:hidden;'"
				"")
			    (escape-string content))
		:clog-type  'clog-summary
		:html-id    html-id
		:auto-place auto-place))
