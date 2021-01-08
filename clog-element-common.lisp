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
  (:documentation "Create a new CLOG-Img as child of CLOG-OBJ with :CONTENT
(default \"\") and if :AUTO-PLACE (default t) place-inside-bottom-of
CLOG-OBJ. Use width and height properties before placing image to constrain
image size."))

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
  (:documentation "Create a new CLOG-Span as child of CLOG-OBJ with :CONTENT
(default \"\") and if :AUTO-PLACE (default t) place-inside-bottom-of
CLOG-OBJ"))

(defmethod create-span ((obj clog-obj) &key (content "") (auto-place t))
  (create-child obj (format nil "<span>~A</span>" (escape-string content))
		:clog-type 'clog-span :auto-place auto-place))

