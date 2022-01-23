;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG - The Common Lisp Omnificent GUI                                 ;;;;
;;;; (c) 2020-2021 David Botton                                            ;;;;
;;;; License BSD 3 Clause                                                  ;;;;
;;;;                                                                       ;;;;
;;;; clog-canvas.lisp                                                      ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:in-package :clog)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-canvas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-canvas (clog-element)()
  (:documentation "CLOG Canvas Objects."))

;;;;;;;;;;;;;;;;;;;
;; create-canvas ;;
;;;;;;;;;;;;;;;;;;;

(defgeneric create-canvas (clog-obj &key width height
				      class hidden html-id auto-place)
  (:documentation "Create a new CLOG-Canvas as child of CLOG-OBJ if
:AUTO-PLACE (default t) place-inside-bottom-of CLOG-OBJ."))

(defmethod create-canvas ((obj clog-obj)
			  &key (width 300) (height 150)
			    (class nil) (hidden nil)
			    (html-id nil) (auto-place t))
  (create-child obj (format nil "<canvas~A~A width=~A height=~A/>"
			    (if class
				(format nil " class='~A'"
					(escape-string class))
				"")
			    (if hidden
				" style='visibility:hidden;'"
				"")
			    width height)
		:clog-type  'clog-canvas
		:html-id    html-id
		:auto-place auto-place))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-context2d
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-context2d (clog-obj)())

;;;;;;;;;;;;;;;;;;;;;;
;; create-context2d ;;
;;;;;;;;;;;;;;;;;;;;;;

(defgeneric create-context2d (clog-canvas)
  (:documentation "Create a new CLOG-Context2d from a CLOG-Canvas"))


(defmethod create-context2d ((obj clog-canvas))
  (let ((web-id (clog-connection:generate-id)))
    (clog-connection:execute (connection-id obj)
		(format nil "clog['~A']=clog['~A'].getContext('2d')"
			web-id
			(html-id obj)))

    (make-instance 'clog-context2d
		   :connection-id (connection-id obj)
		   :html-id web-id)))

;;;;;;;;;;;;;;;;
;; clear-rect ;;
;;;;;;;;;;;;;;;;

(defgeneric clear-rect (clog-context2d x y width height)
  (:documentation "Clear rectangle to transparent black"))

(defmethod clear-rect ((obj clog-context2d) x y width height)
  (execute obj (format nil "clearRect(~A,~A,~A,~A)"
		       x y width height)))

;;;;;;;;;;;;;;;
;; fill-rect ;;
;;;;;;;;;;;;;;;

(defgeneric fill-rect (clog-context2d x y width height)
  (:documentation "Fill rectangle with current fill-color"))

(defmethod fill-rect ((obj clog-context2d) x y width height)
  (execute obj (format nil "fillRect(~A,~A,~A,~A)"
		       x y width height)))

;;;;;;;;;;;;;;;;;
;; stroke-rect ;;
;;;;;;;;;;;;;;;;;

(defgeneric stroke-rect (clog-context2d x y width height)
  (:documentation "Fill rectangle using current stroke-style"))

(defmethod stroke-rect ((obj clog-context2d) x y width height)
  (execute obj (format nil "strokeRect(~A,~A,~A,~A)"
		       x y width height)))

;;;;;;;;;;;;;;;
;; fill-text ;;
;;;;;;;;;;;;;;;

(defgeneric fill-text (clog-context2d text x y &key max-width)
  (:documentation "Fill text with current fill-color"))

(defmethod fill-text ((obj clog-context2d) text x y &key (max-width nil))
  (execute obj (format nil "fillText('~A',~A,~A~A)"
		       (escape-string text)
		       x y
		       (if max-width
			   (format nil ",~A" max-width)
			   ""))))
;;;;;;;;;;;;;;;;;
;; stroke-text ;;
;;;;;;;;;;;;;;;;;

(defgeneric stroke-text (clog-context2d text x y &key max-width)
  (:documentation "Stroke text with current stroke-style"))

(defmethod stroke-text ((obj clog-context2d) text x y &key (max-width nil))
  (execute obj (format nil "strokeText('~A',~A,~A~A)"
		       (escape-string text)
		       x y
		       (if max-width
			   (format nil ",~A" max-width)
			   ""))))
;;;;;;;;;;;;;;;;;;
;; measure-text ;;
;;;;;;;;;;;;;;;;;;

(defgeneric measure-text (clog-context2d text)
  (:documentation "Measure text"))

(defmethod measure-text ((obj clog-context2d) text)
  ;; (let ((text-metric (query obj
  ;;(format nil "measureText('~A')" (escape-string text)))))
    ;; needs way to query like for events
    )

;;;;;;;;;;;;;;;;
;; line-width ;;
;;;;;;;;;;;;;;;;

(defgeneric line-width (clog-context2d value)
  (:documentation "Set line style width"))

(defmethod line-width ((obj clog-context2d) value)
  (execute obj (format nil "lineWidth=~A" value)))

;;;;;;;;;;;;;;
;; line-cap ;;
;;;;;;;;;;;;;;

(deftype line-cap-type () '(member :butt :round :square))

(defgeneric line-cap (clog-context2d value)
  (:documentation "Set line cap style"))

(defmethod line-cap ((obj clog-context2d) value)
  (execute obj (format nil "lineCap='~A'" value)))

;;;;;;;;;;;;;;;
;; line-join ;;
;;;;;;;;;;;;;;;

(deftype line-join-type () '(member :bevel :round :miter))

(defgeneric line-join (clog-context2d value)
  (:documentation "Set line join style"))

(defmethod line-join ((obj clog-context2d) value)
  (execute obj (format nil "lineJoin='~A'" value)))

;;;;;;;;;;;;;;;;;
;; miter-limit ;;
;;;;;;;;;;;;;;;;;

(Defgeneric miter-limit (clog-context2d value)
  (:documentation "Set miter style limit"))

(defmethod miter-limit ((obj clog-context2d) value)
  (execute obj (format nil "miterLimit=~A" value)))

;;;;;;;;;;;;;;;;;;;
;; get-line-dash ;;
;;;;;;;;;;;;;;;;;;;

(Defgeneric get-line-dash (clog-context2d)
  (:documentation "Set line style dash pattern, e.g. [10, 20]"))

(defmethod get-line-dash ((obj clog-context2d))
  (query obj (format nil "getLineDash()")))

;;;;;;;;;;;;;;;;;;;
;; set-line-dash ;;
;;;;;;;;;;;;;;;;;;;

(defgeneric set-line-dash (clog-context2d value)
  (:documentation "Set line style dash pattern, e.g. [10, 20]"))

(defmethod set-line-dash ((obj clog-context2d) value)
  (execute obj (format nil "setLineDash(~A)" value)))

;;;;;;;;;;;;;;;;;;;;;;
;; line-dash-offset ;;
;;;;;;;;;;;;;;;;;;;;;;

(defgeneric line-dash-offset (clog-context2d value)
  (:documentation "Set miter style limit"))

(defmethod line-dash-offset ((obj clog-context2d) value)
  (execute obj (format nil "lineDashOffset=~A" value)))

;;;;;;;;;;;;;;;;
;; font-style ;;
;;;;;;;;;;;;;;;;

(defgeneric font-style (clog-context2d value)
  (:documentation "Set font style using css font string
     https://developer.mozilla.org/en-US/docs/Web/CSS/font"))

(defmethod font-style ((obj clog-context2d) value)
  (execute obj (format nil "font='~A'" value)))

;;;;;;;;;;;;;;;;
;; text-align ;;
;;;;;;;;;;;;;;;;

(deftype text-align-type () '(member :left :right :center :start :end))

(defgeneric text-align (clog-context2d value)
  (:documentation "Set text alignment style"))

(defmethod text-align ((obj clog-context2d) value)
  (execute obj (format nil "textAlign='~A'" value)))

;;;;;;;;;;;;;;;;;;;;
;; text-base-line ;;
;;;;;;;;;;;;;;;;;;;;

(deftype text-baseline-type ()
  '(member :top :hanging :middle :alphabetic :ideographic :bottom))

(defgeneric text-baseline (clog-context2d value)
  (:documentation "Set text baseline style"))

(defmethod text-baseline ((obj clog-context2d) value)
  (execute obj (format nil "textBaseline='~A'" value)))

;;;;;;;;;;;;;;
;; text-dir ;;
;;;;;;;;;;;;;;

(defgeneric text-dir (clog-context2d value)
  (:documentation "Set text direction style"))

(defmethod text-dir ((obj clog-context2d) value)
  (execute obj (format nil "direction='~A'" value)))

;;;;;;;;;;;;;;;;
;; fill-style ;;
;;;;;;;;;;;;;;;;

(defgeneric fill-style (clog-context2d value)
  (:documentation "Set text direction style"))

(defmethod fill-style ((obj clog-context2d) value)
  (execute obj (format nil "fillStyle='~A'" value)))

;;;;;;;;;;;;;;;;;;
;; stroke-style ;;
;;;;;;;;;;;;;;;;;;

(defgeneric stroke-style (clog-context2d value)
  (:documentation "Set text stroke style"))

(defmethod stroke-style ((obj clog-context2d) value)
  (execute obj (format nil "strokeStyle='~A'" value)))

;; need to add createLinearGradient
;; need to add createRadialGradient
;; need to add createPattern

;;;;;;;;;;;;;;;;;
;; shadow-blur ;;
;;;;;;;;;;;;;;;;;

(Defgeneric shadow-blur (clog-context2d value)
  (:documentation "Set text shadow blur"))

(defmethod shadow-blur ((obj clog-context2d) value)
  (execute obj (format nil "shadowBlur='~A'" value)))

;;;;;;;;;;;;;;;;;;
;; shadow-color ;;
;;;;;;;;;;;;;;;;;;

(defgeneric shadow-color (clog-context2d value)
  (:documentation "Set text shadow color"))

(defmethod shadow-color ((obj clog-context2d) value)
  (execute obj (format nil "shadowColor='~A'" value)))

;;;;;;;;;;;;;;;;;;;;;
;; shadow-offset-x ;;
;;;;;;;;;;;;;;;;;;;;;

(defgeneric shadow-offset-x (clog-context2d value)
  (:documentation "Set text shadow offset x"))

(defmethod shadow-offset-x ((obj clog-context2d) value)
  (execute obj (format nil "shadowOffsetX='~A'" value)))

;;;;;;;;;;;;;;;;;;;;;
;; shadow-offset-y ;;
;;;;;;;;;;;;;;;;;;;;;

(defgeneric shadow-offset-y (clog-context2d value)
  (:documentation "Set text shadow offset y"))

(defmethod shadow-offset-y ((obj clog-context2d) value)
  (execute obj (format nil "shadowOffsetY='~A'" value)))

;;;;;;;;;;;;;;;;
;; begin-path ;;
;;;;;;;;;;;;;;;;

(defgeneric begin-path (clog-context2d)
  (:documentation "Starts a new path empting any previous points."))

(defmethod begin-path ((obj clog-context2d))
  (execute obj "beginPath()"))

;;;;;;;;;;;;;;;;
;; close-path ;;
;;;;;;;;;;;;;;;;

(defgeneric close-path (clog-context2d)
  (:documentation "Adds a line to start point of path."))

(defmethod close-path ((obj clog-context2d))
  (execute obj "closePath()"))

;;;;;;;;;;;;;
;; move-to ;;
;;;;;;;;;;;;;

(defgeneric move-to (clog-context2d x y)
  (:documentation "Moves start point of path."))

(defmethod move-to ((obj clog-context2d) x y)
  (execute obj (format nil "moveTo(~A,~A)" x y)))

;;;;;;;;;;;;;
;; line-to ;;
;;;;;;;;;;;;;

(defgeneric line-to (clog-context2d x y)
  (:documentation "Adds a line to the current path."))

(defmethod line-to ((obj clog-context2d) x y)
  (execute obj (format nil "lineTo(~A,~A)" x y)))

;;;;;;;;;;;;;;;;;;;;;
;; bezier-curve-to ;;
;;;;;;;;;;;;;;;;;;;;;

(defgeneric bezier-curve-to (clog-context2d cp1x cp1y cp2x cp2y x y)
  (:documentation "Adds a cubic Bezier curve to the current path."))

(defmethod bezier-curve-to ((obj clog-context2d) cp1x cp1y cp2x cp2y x y)
  (execute obj (format nil "bezierCurveTo(~A,~A,~A,~A,~A,~A)"
		       cp1x cp1y cp2x cp2y x y)))

;;;;;;;;;;;;;;;;;;;;;;;;
;; quadratic-curve-to ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric quadratic-curve-to (clog-context2d cpx cpy x y)
  (:documentation "Adds a quadratic Bezier curve to the current path."))

(defmethod quadratic-curve-to ((obj clog-context2d) cpx cpy x y)
  (execute obj (format nil "quadraticCurveTo(~A,~A,~A,~A)" cpx cpy x y)))

;;;;;;;;;
;; arc ;;
;;;;;;;;;

(Defgeneric arc (clog-context2d x y radius start-angle end-angle
		 &key anticlockwise)
  (:documentation "Adds a circular arc to the current path."))

(defmethod arc ((obj clog-context2d) x y radius start-angle end-angle
		&key (anticlockwise nil))
  (execute obj (format nil "arc(~A,~A,~A,~A,~A~A)"
		       x y radius start-angle end-angle
		       (if anticlockwise
			   (format nil ",~A" anticlockwise)
			   ""))))

;;;;;;;;;;;;
;; arc-to ;;
;;;;;;;;;;;;

(defgeneric arc-to (clog-context2d x1 y1 x2 y2)
  (:documentation "Adds an arc to the current path."))

(defmethod arc-to ((obj clog-context2d) x1 y1 x2 y2)
  (execute obj (format nil "arcTo(~A,~A,~A,~A)" x1 y1 x2 y2)))

;;;;;;;;;;;;;
;; ellipse ;;
;;;;;;;;;;;;;

(defgeneric ellipse (clog-context2d x y radius-x radius-y rotation
		     start-angle end-angle
		     &key anticlockwise)
  (:documentation "Adds an elliptical arc to the current path."))

(defmethod ellipse ((obj clog-context2d) x y radius-x radius-y rotation
		    start-angle end-angle
		    &key (anticlockwise nil))
  (execute obj (format nil "ellipse(~A,~A,~A,~A,~A,~A,~A~A)"
		       x y radius-x radius-y rotation start-angle end-angle
		       (if anticlockwise
			   (format nil ",~A" anticlockwise)
			   ""))))

;;;;;;;;;;
;; rect ;;
;;;;;;;;;;

(defgeneric rect (clog-context2d x y width height)
  (:documentation "Adds a rectangle to the current path."))

(defmethod rect ((obj clog-context2d) x y width height)
  (execute obj (format nil "rect(~A,~A,~A,~A)" x y width height)))

;;;;;;;;;;;;;;;
;; path-fill ;;
;;;;;;;;;;;;;;;

(defgeneric path-fill (clog-context2d)
  (:documentation "Fill a path."))

(defmethod path-fill ((obj clog-context2d))
  (execute obj "fill()"))

;;;;;;;;;;;;;;;;;
;; path-stroke ;;
;;;;;;;;;;;;;;;;;

(defgeneric path-stroke (clog-context2d)
  (:documentation "Stroke a path."))

(defmethod path-stroke ((obj clog-context2d))
  (execute obj "stroke()"))

;;;;;;;;;;;;;;;
;; path-clip ;;
;;;;;;;;;;;;;;;

(defgeneric path-clip (clog-context2d)
  (:documentation "Clip a path."))

(defmethod path-clip ((obj clog-context2d))
  (execute obj "clip()"))

;;;;;;;;;;;;;;;;;
;; canvas-save ;;
;;;;;;;;;;;;;;;;;

(defgeneric canvas-save (clog-context2d)
  (:documentation "Save canvas to stack"))

(defmethod canvas-save ((obj clog-context2d))
  (execute obj "save()"))

;;;;;;;;;;;;;;;;;;;;
;; canvas-restore ;;
;;;;;;;;;;;;;;;;;;;;

(defgeneric canvas-restore (clog-context2d)
  (:documentation "Restore canvas from stack"))

(defmethod canvas-restore ((obj clog-context2d))
  (execute obj "restore()"))
