;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG - The Common Lisp Omnificent GUI                                 ;;;;
;;;; (c) 2020-2022 David Botton                                            ;;;;
;;;; License BSD 3 Clause                                                  ;;;;
;;;;                                                                       ;;;;
;;;; clog-panel.lisp                                                       ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:in-package :clog)

;;; clog-panels are for doing layouts, base class for pluggins and custom
;;; widgets.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-panel
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-panel (clog-element)()
  (:documentation "CLOG Panel objects."))

;;;;;;;;;;;;;;;;;;
;; create-panel ;;
;;;;;;;;;;;;;;;;;;

(defgeneric create-panel (clog-obj &key left top right bottom
				     width height units
				     margin-left margin-top
				     margin-right margin-bottom
				     border-style border-width border-color
				     background-color
				     positioning overflow resizable content
				     style hidden class html-id auto-place)
  (:documentation "Create a new CLOG-Panel as child of
CLOG-OBJ. Optionally you can set the :X, :Y, :WIDTH and :HEIGHT (in
:UNITS defulting to :px, if set to nil unit type must be provided for
x,y,width and height), BORDER-STYLE (see BORDER-STYLE-TYPE),
BORDER-WIDTH, BORDER-COLOR, :POSITIONING (default is :FIXED) (see
POSITIONING-TYPE), :OVERFLOW (default is :CLIP) with :CONTENT (default
\"\") and :RESIZABLE defaults to :NONE. Additional css styles can be
set in :STYLE (default \"\") if :AUTO-PLACE (default t)
place-inside-bottom-of CLOG-OBJ. If hidden is true visiblep is set to
nil. Resizable only works if overflow is set to :SCROLL"))

(defmethod create-panel ((obj clog-obj) &key
					  (left nil)
					  (top nil)
					  (right nil)
					  (bottom nil)
					  (width nil)
					  (height nil)
					  (units :px)
					  (margin-left nil)
					  (margin-top nil)
					  (margin-right nil)
					  (margin-bottom nil)
					  (border-style nil)
					  (border-width nil)
					  (border-color nil)
					  (background-color nil)
					  (positioning :absolute)
					  (overflow :clip)
					  (display nil)
					  (resizable nil)
					  (content "")
					  (style "")
					  (hidden nil)
					  (class nil)
					  (html-id nil)
					  (auto-place t))
  (create-child obj
     (format nil "<div~A style='~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A'>~A</div>"
	     (if class
		 (format nil " class='~A'" (escape-string class))
		 "")
	     (if style
		 (format nil "~A;" (escape-string style))
		 "")
	     (if left
		 (format nil "left:~A~A;" left units)
		 "")
	     (if top
		 (format nil "top:~A~A;" top units)
		 "")
	     (if right
		 (format nil "right:~A~A;" right units)
		 "")
	     (if bottom
		 (format nil "bottom:~A~A;" bottom units)
		 "")
	     (if margin-left
		 (format nil "margin-left:~A~A;" margin-left units)
		 "")
	     (if margin-top
		 (format nil "margin-top:~A~A;" margin-top units)
		 "")
	     (if margin-right
		 (format nil "margin-right:~A~A;" margin-right units)
		 "")
	     (if margin-bottom
		 (format nil "margin-bottom:~A~A;" margin-bottom units)
		 "")
	     (if width
		 (format nil "width:~A~A;" width units)
		 "")
	     (if height
		 (format nil "height:~A~A;" height units)
		 "")
	     (if border-style
		 (format nil "border-style:~A;" border-style)
		 "")
	     (if border-width
		 (format nil "border-width:~A;" border-width)
		 "")
	     (if border-color
		 (format nil "border-color:~A;" border-color)
		 "")
	     (if background-color
		 (format nil "background-color:~A;" background-color)
		 "")
	     (if overflow
		 (format nil "overflow:~A;" overflow)
		 "")
	     (if display
		 (format nil "display:~A;" display)
		 "")
	     (if resizable
		 (format nil "resize:~A;" resizable)
		 "")
	     (if positioning
		 (format nil "position:~A;"
			 (escape-string positioning))
		 "")
	     (if hidden
		 "visibility:hidden;"
		 "")
	     (escape-string content))
     :clog-type  'clog-panel
     :html-id    html-id
     :auto-place auto-place))

;;;;;;;;;;;;;;;;;;;;;
;; center-children ;;
;;;;;;;;;;;;;;;;;;;;;

(defgeneric center-children (clog-element &key vertical horizontal)
  (:documentation "Align children of CLOG-ELEMENT VERTICAL (default t)
and/or HORIZONTAL (default t). This will set the DISPLAY property of
CLOG-ELEMENT to :FLEX. Note: if children of clog-element are using
:absolute positioning they will not flow with flex and will not be
centered. Using :relative wrapped in div with :static positioning
will work."))

(defmethod center-children ((obj clog-element) &key (vertical t) (horizontal t))
  (set-styles obj `(("display" "flex")
		   ,(when vertical '("align-items" "center"))
		   ,(when horizontal '("justify-content" "center")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-panel-box-layout
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-panel-box-layout ()
  ((center-panel
    :accessor center-panel
    :documentation "Center Panel")
   (top-panel
    :accessor top-panel
    :documentation "Top Panel")
   (left-panel
    :accessor left-panel
    :documentation "Left Panel")
   (bottom-panel
    :accessor bottom-panel
    :documentation "Bottom Panel")
   (right-panel
    :accessor right-panel
    :documentation "Right Panel"))
  (:documentation "CLOG Panel Box Layout Objects."))

;;;;;;;;;;;;;;;;;;
;; center-panel ;;
;;;;;;;;;;;;;;;;;;

(defgeneric center-panel (clog-panel-box-layout)
  (:documentation "Returns the center panel of a clog-panel-box-layout object."))

;;;;;;;;;;;;;;;
;; top-panel ;;
;;;;;;;;;;;;;;;

(defgeneric top-panel (clog-panel-box-layout)
  (:documentation "Returns the top panel of a clog-panel-box-layout object."))

;;;;;;;;;;;;;;;
;; top-panel ;;
;;;;;;;;;;;;;;;

(defgeneric top-panel (clog-panel-box-layout)
  (:documentation "Returns the top panel of a clog-panel-box-layout object."))

;;;;;;;;;;;;;;;;
;; left-panel ;;
;;;;;;;;;;;;;;;;

(defgeneric left-panel (clog-panel-box-layout)
  (:documentation "Returns the left panel of a clog-panel-box-layout object."))

;;;;;;;;;;;;;;;;;;
;; bottom-panel ;;
;;;;;;;;;;;;;;;;;;

(defgeneric bottom-panel (clog-panel-box-layout)
  (:documentation "Returns the bottom panel of a clog-panel-box-layout object."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; create-panel-box-layout ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun create-panel-box-layout (clog-obj &key (top-height 50) (left-width 50)
					   (bottom-height 50) (right-width 50)
					   (units "px")
					   (html-id nil))
  "Create a five panel app layout that fills entire contents of CLOG-OBJ.
HTML-ID if set is the base and top,left,right,center, bottom are added e.g.
if :HTML-ID \"myid\" then the HTML-ID for center will be: myid-center"
  (let ((panel-box (make-instance 'clog-panel-box-layout)))
    (unless html-id
      (setf html-id (clog-connection:generate-id)))
    (setf (top-panel panel-box)
	  (create-panel clog-obj :left 0 :top 0 :right 0 :height top-height
				 :units units
				 :html-id (format nil "~A-top" html-id)))
    (setf (left-panel panel-box)
	  (create-panel clog-obj :left 0 :top 0 :bottom 0 :width left-width
				 :margin-top top-height
				 :margin-bottom bottom-height
				 :units units
				 :html-id (format nil "~A-left" html-id)))
    (setf (right-panel panel-box)
	  (create-panel clog-obj :right 0 :top 0 :bottom 0 :width right-width
				 :margin-top top-height
				 :margin-bottom bottom-height
				 :units units
				 :html-id (format nil "~A-right" html-id)))
    (setf (center-panel panel-box)
	  (create-panel clog-obj :left 0 :top 0 :right 0 :bottom 0
				 :margin-left left-width
				 :margin-top top-height
				 :margin-right right-width
				 :margin-bottom bottom-height
				 :units units
				 :html-id (format nil "~A-center" html-id)))
    (setf (bottom-panel panel-box)
	  (create-panel clog-obj :left 0 :bottom 0 :right 0
				 :height bottom-height
				 :units units
				 :html-id (format nil "~A-bottom" html-id)))
    panel-box))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-panel-box
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-panel-box (clog-element)
  ((panel-box
    :accessor panel-box
    :documentation "CLOG-PANEL-BOX-LAYOUT access"))
  (:documentation "CLOG Panel-Box Objects."))

;;;;;;;;;;;;;;;;;;;;;;
;; create-panel-box ;;
;;;;;;;;;;;;;;;;;;;;;;

(defgeneric create-panel-box (clog-obj &key width height hidden class html-id auto-place)
  (:documentation "Create a new CLOG-Panel-Box, a div containg a
CLOG-PANEL-BOX-LAYOUT as child of CLOG-OBJ with and if :AUTO-PLACE
(default t) place-inside-bottom-of CLOG-OBJ. If hidden is true visiblep
is set to nil."))

(defmethod create-panel-box ((obj clog-obj) &key (width "100%") (height "100%")
					      (hidden nil)
					      (class nil)
					      (html-id nil)
					      (auto-place t))
  (let ((parent (create-child obj (format nil "<div~A~A~A~A/>"
					  (if class
					      (format nil " class='~A'" (escape-string class))
					      "")
					  (if width
					      (format nil " width='~A'" width)
					      "")
					  (if height
					      (format nil " height='~A'" height)
					      "")
					  (if hidden
					      " style='visibility:hidden;'"
					      ""))
			      :clog-type  'clog-panel-box
			      :html-id    html-id
			      :auto-place auto-place)))
    (setf (panel-box parent) (create-panel-box-layout parent :html-id (html-id parent)))
    parent))

;;;;;;;;;;;;;;;
;; panel-box ;;
;;;;;;;;;;;;;;;

(defgeneric panel-box (clog-panel-box)
  (:documentation "Returns the CLOG-PANEL-BOX-LAYOUT object contained in the CLOG-PANEL-BOX."))
