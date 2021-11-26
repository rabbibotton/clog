;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG - The Common Lisp Omnificent GUI                                 ;;;;
;;;; (c) 2020-2021 David Botton                                            ;;;;
;;;; License BSD 3 Clause                                                  ;;;;
;;;;                                                                       ;;;;
;;;; clog-panel.lisp                                                       ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:in-package :clog)

;;; clog-panels are for doing layouts specializing in fixed or absolute
;;; positioning.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-panel
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-panel (clog-element)()
  (:documentation "CLOG Panels objects."))

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
  (create-child obj (format nil "<span~A style='~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A'>~A</span>"
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
				""
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
