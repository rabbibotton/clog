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
                                        (escape-string class :html t))
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
    (js-execute obj (format nil "clog['~A']=clog['~A'].getContext('2d')"
                            web-id
                            (html-id obj)))
    (make-instance 'clog-context2d
                   :connection-id (connection-id obj)
                   :html-id web-id)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Properties - clog-context2d
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;
;; fill-style ;;
;;;;;;;;;;;;;;;;

(defgeneric fill-style (clog-context2d)
  (:documentation "Setf/get property fill style"))

(defmethod fill-style ((obj clog-context2d))
  (query obj "fillStyle"))

(defmethod (setf fill-style) (value (obj clog-context2d))
  (execute obj (format nil "fillStyle='~A'" value)))

;;;;;;;;;;;;;;;;;;;
;; canvas-filter ;;
;;;;;;;;;;;;;;;;;;;

(defgeneric canvas-filter (clog-context2d)
  (:documentation "Setf/get filter dsl -
See https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/filter"))

(defmethod canvas-filter ((obj clog-context2d))
  (query obj "filter"))

(defmethod (setf canvas-filter) (value (obj clog-context2d))
  (execute obj (format nil "filter='~A'" value)))

;;;;;;;;;;;;;;;;
;; font-style ;;
;;;;;;;;;;;;;;;;

(defgeneric font-style (clog-context2d)
  (:documentation "Setf/get font using css font string
     https://developer.mozilla.org/en-US/docs/Web/CSS/font"))

(defmethod font-style ((obj clog-context2d))
  (query obj "font"))

(defmethod (setf font-style) (value (obj clog-context2d))
  (execute obj (format nil "font='~A'" value)))

;;;;;;;;;;;;;;;;;;
;; global-alpha ;;
;;;;;;;;;;;;;;;;;;

(defgeneric global-alpha (clog-context2d)
  (:documentation "Setf/get global alpha"))

(defmethod global-alpha ((obj clog-context2d))
  (parse-float (query obj "globalAlpha")))

(defmethod (setf global-alpha) (value (obj clog-context2d))
  (execute obj (format nil "globalAlpha=~A" value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; global-composite-operation ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric global-composite-operation (clog-context2d)
  (:documentation "Setf/get composite blend mode -
https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/globalCompositeOperation"))

(defmethod global-composite-operation ((obj clog-context2d))
  (query obj "globalCompositeOperation"))

(defmethod (setf global-composite-operation) (value (obj clog-context2d))
  (execute obj (format nil "globalCompositeOperation='~A'" value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; image-smoothing-enabled ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric image-smoothing-enabled (clog-context2d)
  (:documentation "Setf/get text shadow blur"))

(defmethod image-smoothing-enabled ((obj clog-context2d))
  (js-true-p (query obj "imageSmoothingEnabled")))

(defmethod (setf image-smoothing-enabled) (value (obj clog-context2d))
  (execute obj (format nil "imageSmoothingEnabled=~A" (p-true-js value))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; image-smoothing-quality ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric image-smoothing-quality (clog-context2d)
  (:documentation "Setf/get text shadow blur"))

(defmethod image-smoothing-quality ((obj clog-context2d))
  (query obj "imageSmoothingQuality"))

(defmethod (setf image-smoothing-quality) (value (obj clog-context2d))
  (execute obj (format nil "imageSmoothingQuality='~A'" value)))

;;;;;;;;;;;;;;
;; line-cap ;;
;;;;;;;;;;;;;;

(deftype line-cap-type () '(member :butt :round :square))

(defgeneric line-cap (clog-context2d)
  (:documentation "Setf/get line cap style"))

(defmethod line-cap ((obj clog-context2d))
  (query obj "lineCap"))

(defmethod (setf line-cap) (value (obj clog-context2d))
  (execute obj (format nil "lineCap='~A'" value)))

;;;;;;;;;;;;;;;;;;;;;;
;; line-dash-offset ;;
;;;;;;;;;;;;;;;;;;;;;;

(defgeneric line-dash-offset (clog-context2d)
  (:documentation "Setf/get miter style limit"))

(defmethod line-dash-offset ((obj clog-context2d))
  (parse-float (query obj "lineDashOffset")))

(defmethod (setf line-dash-offset) (value (obj clog-context2d))
  (execute obj (format nil "lineDashOffset=~A" value)))

;;;;;;;;;;;;;;;
;; line-join ;;
;;;;;;;;;;;;;;;

(deftype line-join-type () '(member :bevel :round :miter))

(defgeneric line-join (clog-context2d)
  (:documentation "Setf/get line join style"))

(defmethod line-join ((obj clog-context2d))
  (query obj "lineJoin"))

(defmethod (setf line-join) (value (obj clog-context2d))
  (execute obj (format nil "lineJoin='~A'" value)))

;;;;;;;;;;;;;;;;
;; line-width ;;
;;;;;;;;;;;;;;;;

(defgeneric line-width (clog-context2d)
  (:documentation "Set line style width"))

(defmethod line-width ((obj clog-context2d))
  (query obj "lineWidth"))

(defmethod (setf line-width) (value (obj clog-context2d))
  (execute obj (format nil "lineWidth=~A" value)))

;;;;;;;;;;;;;;;;;
;; miter-limit ;;
;;;;;;;;;;;;;;;;;

(Defgeneric miter-limit (clog-context2d)
  (:documentation "Setf/get miter style limit"))

(defmethod miter-limit ((obj clog-context2d))
  (parse-float (query obj "miterLimit")))

(defmethod (setf miter-limit) (value (obj clog-context2d))
  (execute obj (format nil "miterLimit=~A" value)))

;;;;;;;;;;;;;;;;;
;; shadow-blur ;;
;;;;;;;;;;;;;;;;;

(defgeneric shadow-blur (clog-context2d)
  (:documentation "Setf/get text shadow blur"))

(defmethod shadow-blur ((obj clog-context2d))
  (parse-float (query obj "shadowBlur")))

(defmethod (setf shadow-blur) (value (obj clog-context2d))
  (execute obj (format nil "shadowBlur=~A" value)))

;;;;;;;;;;;;;;;;;;
;; shadow-color ;;
;;;;;;;;;;;;;;;;;;

(defgeneric shadow-color (clog-context2d)
  (:documentation "Setf/get text shadow color"))

(defmethod shadow-color ((obj clog-context2d))
  (query obj "shadowColor"))

(defmethod (setf shadow-color) (value (obj clog-context2d))
  (execute obj (format nil "shadowColor='~A'" value)))

;;;;;;;;;;;;;;;;;;;;;
;; shadow-offset-x ;;
;;;;;;;;;;;;;;;;;;;;;

(defgeneric shadow-offset-x (clog-context2d)
  (:documentation "Setf/get text shadow offset x"))

(defmethod shadow-offset-x ((obj clog-context2d))
  (parse-float (query obj "shadowOffsetX")))

(defmethod (setf shadow-offset-x) (value (obj clog-context2d))
  (execute obj (format nil "shadowOffsetX=~A" value)))

;;;;;;;;;;;;;;;;;;;;;
;; shadow-offset-y ;;
;;;;;;;;;;;;;;;;;;;;;

(defgeneric shadow-offset-y (clog-context2d)
  (:documentation "Setf/get text shadow offset y"))

(defmethod shadow-offset-y ((obj clog-context2d))
  (parse-float (query obj "shadowOffsetY=~A")))

(defmethod (setf shadow-offset-y) (value (obj clog-context2d))
  (execute obj (format nil "shadowOffsetY=~A" value)))

;;;;;;;;;;;;;;;;;;
;; stroke-style ;;
;;;;;;;;;;;;;;;;;;

(defgeneric stroke-style (clog-context2d)
  (:documentation "Setf/get text stroke style"))

(defmethod stroke-style ((obj clog-context2d))
  (query obj "strokeStyle"))

(defmethod (setf stroke-style) (value (obj clog-context2d))
  (execute obj (format nil "strokeStyle='~A'" value)))

;;;;;;;;;;;;;;;;
;; text-align ;;
;;;;;;;;;;;;;;;;

(deftype text-align-type () '(member :left :right :center :start :end))

(defgeneric text-align (clog-context2d)
  (:documentation "Setf/get text alignment style"))

(defmethod text-align ((obj clog-context2d))
  (query obj "textAlign"))

(defmethod (setf text-align) (value (obj clog-context2d))
  (execute obj (format nil "textAlign='~A'" value)))

;;;;;;;;;;;;;;;;;;;;
;; text-base-line ;;
;;;;;;;;;;;;;;;;;;;;

(deftype text-baseline-type ()
  '(member :top :hanging :middle :alphabetic :ideographic :bottom))

(defgeneric text-baseline (clog-context2d)
  (:documentation "Set text baseline style"))

(defmethod text-baseline ((obj clog-context2d))
  (query obj "textBaseline"))

(defmethod (setf text-baseline) (value (obj clog-context2d))
  (execute obj (format nil "textBaseline='~A'" value)))

;;;;;;;;;;;;;;
;; text-dir ;;
;;;;;;;;;;;;;;

(defgeneric text-dir (clog-context2d)
  (:documentation "Setf/get text direction style"))

(defmethod text-dir ((obj clog-context2d))
  (query obj "direction"))

(defmethod (setf text-dir) (value (obj clog-context2d))
  (execute obj (format nil "direction='~A'" value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Methods - clog-context2d
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;
;; begin-path ;;
;;;;;;;;;;;;;;;;

(defgeneric begin-path (clog-context2d)
  (:documentation "Starts a new path empting any previous points."))

(defmethod begin-path ((obj clog-context2d))
  (execute obj "beginPath()"))

;;;;;;;;;;;;;;;;;;;;;
;; bezier-curve-to ;;
;;;;;;;;;;;;;;;;;;;;;

(defgeneric bezier-curve-to (clog-context2d cp1x cp1y cp2x cp2y x y)
  (:documentation "Adds a cubic Bezier curve to the current path."))

(defmethod bezier-curve-to ((obj clog-context2d) cp1x cp1y cp2x cp2y x y)
  (execute obj (format nil "bezierCurveTo(~A,~A,~A,~A,~A,~A)"
                       cp1x cp1y cp2x cp2y x y)))

;;;;;;;;;;;;;;;;
;; clear-rect ;;
;;;;;;;;;;;;;;;;

(defgeneric clear-rect (clog-context2d x y width height)
  (:documentation "Clear rectangle to transparent black"))

(defmethod clear-rect ((obj clog-context2d) x y width height)
  (execute obj (format nil "clearRect(~A,~A,~A,~A)"
                       x y width height)))

;;;;;;;;;;;;;;;
;; path-clip ;;
;;;;;;;;;;;;;;;

(defgeneric path-clip (clog-context2d)
  (:documentation "Clip a path."))

(defmethod path-clip ((obj clog-context2d))
  (execute obj "clip()"))

;; to add
;; clip(path)
;; clip(fillRule)
;; clip(path, fillRule)

;;;;;;;;;;;;;;;;
;; close-path ;;
;;;;;;;;;;;;;;;;

(defgeneric close-path (clog-context2d)
  (:documentation "Adds a line to start point of path."))

(defmethod close-path ((obj clog-context2d))
  (execute obj "closePath()"))


;; createConicGradient
;; createImageData
;; need to add createLinearGradient
;; need to add createRadialGradient
;; need to add createPattern
;; drawFocusIfNeeded

;;;;;;;;;;;;;;;;
;; draw-image ;;
;;;;;;;;;;;;;;;;

(defgeneric draw-image (clog-context2d clog-obj x y)
  (:documentation "Draw image at x y"))

(defmethod draw-image ((obj clog-context2d) clog-obj x y)
  (execute obj (format nil "drawImage(~A,~A,~A)"
                       (script-id clog-obj) x y)))

;; need other versions of draw-image

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

;;;;;;;;;;;;;;;
;; path-fill ;;
;;;;;;;;;;;;;;;

(defgeneric path-fill (clog-context2d)
  (:documentation "Fill a path."))

(defmethod path-fill ((obj clog-context2d))
  (execute obj "fill()"))


;;;;;;;;;;;;;;;
;; fill-rect ;;
;;;;;;;;;;;;;;;

(defgeneric fill-rect (clog-context2d x y width height)
  (:documentation "Fill rectangle with current fill-color"))

(defmethod fill-rect ((obj clog-context2d) x y width height)
  (execute obj (format nil "fillRect(~A,~A,~A,~A)"
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

;; getImageData()

;;;;;;;;;;;;;;;;;;;
;; get-line-dash ;;
;;;;;;;;;;;;;;;;;;;

(defgeneric get-line-dash (clog-context2d)
  (:documentation "Set line style dash pattern, e.g. 10, 20"))

(defmethod get-line-dash ((obj clog-context2d))
  (query obj (format nil "getLineDash()")))

;; getTransform
;; isPointinPath
;; isPointinStroke

;;;;;;;;;;;;;
;; line-to ;;
;;;;;;;;;;;;;

(defgeneric line-to (clog-context2d x y)
  (:documentation "Adds a line to the current path."))

(defmethod line-to ((obj clog-context2d) x y)
  (execute obj (format nil "lineTo(~A,~A)" x y)))

;;;;;;;;;;;;;;;;;;
;; measure-text ;;
;;;;;;;;;;;;;;;;;;

(defgeneric measure-text (clog-context2d text)
  (:documentation "Measure text returns a clog-text-metrics object"))

(defmethod measure-text ((obj clog-context2d) text)
  (let ((web-id (clog-connection:generate-id)))
    (js-execute obj (format nil "clog['~A']=~A.measureText('~A')"
                            web-id
                            (script-id obj) text))
    (make-instance 'clog-text-metrics
                   :connection-id (clog::connection-id obj)
                   :html-id web-id)))

;;;;;;;;;;;;;
;; move-to ;;
;;;;;;;;;;;;;

(defgeneric move-to (clog-context2d x y)
  (:documentation "Moves start point of path."))

(defmethod move-to ((obj clog-context2d) x y)
  (execute obj (format nil "moveTo(~A,~A)" x y)))

;; putImageData

;;;;;;;;;;;;;;;;;;;;;;;;
;; quadratic-curve-to ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric quadratic-curve-to (clog-context2d cpx cpy x y)
  (:documentation "Adds a quadratic Bezier curve to the current path."))

(defmethod quadratic-curve-to ((obj clog-context2d) cpx cpy x y)
  (execute obj (format nil "quadraticCurveTo(~A,~A,~A,~A)" cpx cpy x y)))

;;;;;;;;;;
;; rect ;;
;;;;;;;;;;

(defgeneric rect (clog-context2d x y width height)
  (:documentation "Adds a rectangle to the current path."))

(defmethod rect ((obj clog-context2d) x y width height)
  (execute obj (format nil "rect(~A,~A,~A,~A)" x y width height)))

;; resetTransform

;;;;;;;;;;;;;;;;;;;;
;; canvas-restore ;;
;;;;;;;;;;;;;;;;;;;;

(defgeneric canvas-restore (clog-context2d)
  (:documentation "Restore canvas from stack"))

(defmethod canvas-restore ((obj clog-context2d))
  (execute obj "restore()"))

;;;;;;;;;;;;
;; rotate ;;
;;;;;;;;;;;;

(defgeneric rotate (clog-context2d value)
  (:documentation "Rotate"))

(defmethod rotate ((obj clog-context2d) value)
  (execute obj (format nil "rotate(~A)" value)))

;;;;;;;;;;;;;;;;;
;; canvas-save ;;
;;;;;;;;;;;;;;;;;

(defgeneric canvas-save (clog-context2d)
  (:documentation "Save canvas to stack"))

(defmethod canvas-save ((obj clog-context2d))
  (execute obj "save()"))

;;;;;;;;;;;
;; scale ;;
;;;;;;;;;;;

(defgeneric scale (clog-context2d x y)
  (:documentation "Scale"))

(defmethod scale ((obj clog-context2d) x y)
  (execute obj (format nil "scale(~A,~A)" x y)))

;;;;;;;;;;;;;;;;;;;
;; set-line-dash ;;
;;;;;;;;;;;;;;;;;;;

(defgeneric set-line-dash (clog-context2d value)
  (:documentation "Set line style dash pattern, e.g. 10, 20"))

(defmethod set-line-dash ((obj clog-context2d) value)
  (execute obj (format nil "setLineDash(~A)" value)))

;; setTransform

;;;;;;;;;;;;;;;;;
;; path-stroke ;;
;;;;;;;;;;;;;;;;;

(defgeneric path-stroke (clog-context2d)
  (:documentation "Stroke a path."))

(defmethod path-stroke ((obj clog-context2d))
  (execute obj "stroke()"))

;;;;;;;;;;;;;;;;;
;; stroke-rect ;;
;;;;;;;;;;;;;;;;;

(defgeneric stroke-rect (clog-context2d x y width height)
  (:documentation "Fill rectangle using current stroke-style"))

(defmethod stroke-rect ((obj clog-context2d) x y width height)
  (execute obj (format nil "strokeRect(~A,~A,~A,~A)"
                       x y width height)))

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
;;;;;;;;;;;;;;;
;; transform ;;
;;;;;;;;;;;;;;;

(defgeneric transform (clog-context2d a b c d e f)
  (:documentation "Transform"))

(defmethod transform ((obj clog-context2d) a b c d e f)
  (execute obj (format nil "transform(~A,~A,~A,~A,~A,~A)" a b c d e f)))

;;;;;;;;;;;;;;;
;; translate ;;
;;;;;;;;;;;;;;;

(defgeneric translate (clog-context2d x y)
  (:documentation "Translate"))

(defmethod translate ((obj clog-context2d) x y)
  (execute obj (format nil "translate(~A,~A)" x y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-text-metrics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-text-metrics (clog-obj)())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Properties - clog-text-metrics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod width ((obj clog-text-metrics))
  "Width of text"
  (parse-float (query obj "width")))

(defgeneric actual-bounding-box-left (clog-text-metrics)
  (:documentation "Actual bounding box left"))

(defmethod actual-bounding-box-left ((obj clog-text-metrics))
  (parse-float (query obj "actualBoundingBoxLeft")))

(defgeneric actual-bounding-box-right (clog-text-metrics)
  (:documentation "Actual bounding box right"))

(defmethod actual-bounding-box-right ((obj clog-text-metrics))
  (parse-float (query obj "actualBoundingBoxRight")))

(defgeneric actual-bounding-box-ascent (clog-text-metrics)
  (:documentation "Actual bounding box ascent"))

(defmethod actual-bounding-box-ascent ((obj clog-text-metrics))
  (parse-float (query obj "actualBoundingBoxAscent")))

(defgeneric actual-bounding-box-descent (clog-text-metrics)
  (:documentation "Actual bounding box descent"))

(defmethod actual-bounding-box-descent ((obj clog-text-metrics))
  (parse-float (query obj "actualBoundingBoxDescent")))

(defgeneric font-bounding-box-ascent (clog-text-metrics)
  (:documentation "Font bounding box ascent"))

(defmethod font-bounding-box-ascent ((obj clog-text-metrics))
  (parse-float (query obj "fontBoundingBoxAscent")))

(defgeneric font-bounding-box-descent (clog-text-metrics)
  (:documentation "Font bounding box descent"))

(defmethod font-bounding-box-descent ((obj clog-text-metrics))
  (parse-float (query obj "fontBoundingBoxDescent")))

(defgeneric em-height-ascent (clog-text-metrics)
  (:documentation "'M' height ascent"))

(defmethod em-height-ascent ((obj clog-text-metrics))
  (parse-float (query obj "emHeightAscent")))

(defgeneric em-height-descent (clog-text-metrics)
  (:documentation "'M' height descent"))

(defmethod em-height-descent ((obj clog-text-metrics))
  (parse-float (query obj "emHeightDescent")))

(defgeneric hanging-baseline (clog-text-metrics)
  (:documentation "Hanging baseline"))
  
(defmethod hanging-baseline ((obj clog-text-metrics))
  (parse-float (query obj "hangingBaseline")))

(defgeneric alphabetic-baseline (clog-text-metrics)
  (:documentation "Alphabetic baseline"))
  
(defmethod alphabetic-baseline ((obj clog-text-metrics))
  (parse-float (query obj "alphabeticBaseline")))

(defgeneric ideographic-baseline (clog-text-metrics)
  (:documentation "Ideographic baseline"))

(defmethod ideographic-baseline ((obj clog-text-metrics))
  (parse-float (query obj "ideographicBaseline")))
