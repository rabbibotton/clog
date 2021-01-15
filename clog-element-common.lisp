;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG - The Common Lisp Omnificent GUI                                 ;;;;
;;;; (c) 2020-2021 David Botton                                            ;;;;
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

(defgeneric create-a (clog-obj &key link content target auto-place)
  (:documentation "Create a new CLOG-A as child of CLOG-OBJ with :LINK and
:CONTENT (default \"\") and :TARGET (\"_self\") and if :AUTO-PLACE (default t)
place-inside-bottom-of CLOG-OBJ.

   Target of link, name of a frame or:
   _blank  = new window
   _top    = top most frame (full browser window)
   _parent = parent frame or window
   _self   = current frame or window"))

(defmethod create-a ((obj clog-obj)
		     &key (link "")
		       (content "")
		       (target "_self")
		       (auto-place t))
  (create-child obj (format nil "<a target='~A' href='~A'>~A</a>"
			    (escape-string target)
			    (escape-string link)
			    (escape-string content))
		:clog-type 'clog-a
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

(defgeneric create-br (clog-obj &key auto-place)
  (:documentation "Create a new CLOG-BR as child of CLOG-OBJ that creates a
line break and if :AUTO-PLACE (default t) place-inside-bottom-of CLOG-OBJ"))

(defmethod create-br ((obj clog-obj) &key (auto-place t))
  (create-child obj "<br />" :clog-type 'clog-br :auto-place auto-place))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-button
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-button (clog-element)()
  (:documentation "CLOG Button Objects."))

;;;;;;;;;;;;;;;;;;;
;; create-button ;;
;;;;;;;;;;;;;;;;;;;

(defgeneric create-button (clog-obj &key content auto-place)
  (:documentation "Create a new CLOG-Button as child of CLOG-OBJ with :CONTENT
(default \"\") and if :AUTO-PLACE (default t) place-inside-bottom-of
CLOG-OBJ"))

(defmethod create-button ((obj clog-obj) &key (content "") (auto-place t))
  (create-child obj (format nil "<button>~A</button>" (escape-string content))
		:clog-type 'clog-button :auto-place auto-place))

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
  (setf (property obj "disabled") (p-true-js value)))
(defsetf disabledp set-editable)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-div
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-div (clog-element)()
  (:documentation "CLOG Div Objects."))

;;;;;;;;;;;;;;;;
;; create-div ;;
;;;;;;;;;;;;;;;;

(defgeneric create-div (clog-obj &key content auto-place)
  (:documentation "Create a new CLOG-Div as child of CLOG-OBJ with :CONTENT
(default \"\") and if :AUTO-PLACE (default t) place-inside-bottom-of
CLOG-OBJ"))

(defmethod create-div ((obj clog-obj) &key (content "") (auto-place t))
  (create-child obj (format nil "<div>~A</div>" (escape-string content))
		:clog-type 'clog-div :auto-place auto-place))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-hr
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-hr (clog-element)()
  (:documentation "CLOG HR Objects for horizontal rules."))

;;;;;;;;;;;;;;;
;; create-hr ;;
;;;;;;;;;;;;;;;

(defgeneric create-hr (clog-obj &key auto-place)
  (:documentation "Create a new CLOG-HR as child of CLOG-OBJ that creates a
horizontal rule (line) and if :AUTO-PLACE (default t) place-inside-bottom-of
CLOG-OBJ"))

(defmethod create-hr ((obj clog-obj) &key (auto-place t))
  (create-child obj "<hr />" :clog-type 'clog-hr :auto-place auto-place))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-img
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-img (clog-element)()
  (:documentation "CLOG Img Objects."))

;;;;;;;;;;;;;;;;
;; create-img ;;
;;;;;;;;;;;;;;;;

(defgeneric create-img (clog-obj &key url-src alt-text auto-place)
  (:documentation "Create a new CLOG-Img as child of CLOG-OBJ with :URL-SRC
(default \"\") and :ALT-TEXT (default \"\") if :AUTO-PLACE (default t)
place-inside-bottom-of CLOG-OBJ. Use width and height properties before
placing image to constrain image size."))

(defmethod create-img ((obj clog-obj) &key (url-src "") (alt-text "") (auto-place t))
  (create-child obj (format nil "<img src='~A' alt='~A'>)"
			    (escape-string url-src)
			    (escape-string alt-text))
		:clog-type 'clog-img :auto-place auto-place))

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
					  (auto-place t))
  (create-child obj (format nil "<meter value=~A high=~A low=~A max=~A min=~A optimum=~A />"
			    value high low maximum minimum optimum)
		:clog-type 'clog-meter :auto-place auto-place))

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

(defgeneric create-progress-bar (clog-obj &key value maximum auto-place)
  (:documentation "Create a new CLOG-Progress-Bar as child of CLOG-OBJ with VALUE
(default 0) MAXIMUM (default 100) and if :AUTO-PLACE (default t)
place-inside-bottom-of CLOG-OBJ."))

(defmethod create-progress-bar ((obj clog-obj) &key
					  (value 0)
					  (maximum 100)
					  (auto-place t))
  (create-child obj (format nil "<progress value=~A max=~A />" value maximum)
		:clog-type 'clog-progress-bar :auto-place auto-place))

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

(defgeneric create-p (clog-obj &key content auto-place)
  (:documentation "Create a new CLOG-P as child of CLOG-OBJ with :CONTENT
(default \"\") and if :AUTO-PLACE (default t) place-inside-bottom-of
CLOG-OBJ"))

(defmethod create-p ((obj clog-obj) &key (content "") (auto-place t))
  (create-child obj (format nil "<p>~A</p>" (escape-string content))
		:clog-type 'clog-p :auto-place auto-place))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-span
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-span (clog-element)()
  (:documentation "CLOG Span Objects."))

;;;;;;;;;;;;;;;;;
;; create-span ;;
;;;;;;;;;;;;;;;;;

(defgeneric create-span (clog-obj &key content auto-place)
  (:documentation "Create a new CLOG-Span as child of CLOG-OBJ with CONTENT
and if :AUTO-PLACE (default t) place-inside-bottom-of
CLOG-OBJ"))

(defmethod create-span ((obj clog-obj) &key (content "") (auto-place t))
  (create-child obj (format nil "<span>~A</span>" (escape-string content))
		:clog-type 'clog-span :auto-place auto-place))

