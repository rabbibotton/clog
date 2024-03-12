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
  (let ((web-id (generate-id)))
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
  (execute obj (format nil "fillStyle=~A"
                       (if (typep value 'clog-obj)
                           (script-id value)
                           (format nil "'~A'" value)))))

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
  (js-to-float (query obj "globalAlpha")))

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
  (js-to-float (query obj "lineDashOffset")))

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
  (js-to-float (query obj "miterLimit")))

(defmethod (setf miter-limit) (value (obj clog-context2d))
  (execute obj (format nil "miterLimit=~A" value)))

;;;;;;;;;;;;;;;;;
;; shadow-blur ;;
;;;;;;;;;;;;;;;;;

(defgeneric shadow-blur (clog-context2d)
  (:documentation "Setf/get text shadow blur"))

(defmethod shadow-blur ((obj clog-context2d))
  (js-to-float (query obj "shadowBlur")))

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
  (js-to-float (query obj "shadowOffsetX")))

(defmethod (setf shadow-offset-x) (value (obj clog-context2d))
  (execute obj (format nil "shadowOffsetX=~A" value)))

;;;;;;;;;;;;;;;;;;;;;
;; shadow-offset-y ;;
;;;;;;;;;;;;;;;;;;;;;

(defgeneric shadow-offset-y (clog-context2d)
  (:documentation "Setf/get text shadow offset y"))

(defmethod shadow-offset-y ((obj clog-context2d))
  (js-to-float (query obj "shadowOffsetY=~A")))

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

(defgeneric path-clip (clog-context2d &key path2d fill-rule)
  (:documentation "Clip a path."))

(defmethod path-clip ((obj clog-context2d) &key path2d fill-rule)
  (execute obj (format nil "clip(~A~A~A)"
                       (if path2d
                           (script-id path2d)
                           "")
                       (if (and path2d fill-rule)
                           ","
                           "")
                       (if fill-rule
                           (format nil "'~A'" fill-rule)
                           ""))))

;;;;;;;;;;;;;;;;
;; close-path ;;
;;;;;;;;;;;;;;;;

(defgeneric close-path (clog-context2d)
  (:documentation "Adds a line to start point of path."))

(defmethod close-path ((obj clog-context2d))
  (execute obj "closePath()"))


;;;;;;;;;;;;;;;;;;;;;;;
;; create-image-data ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric create-image-data (clog-context2d width height)
  (:documentation "Create blank image data"))

(defmethod create-image-data ((obj clog-context2d) width height)
  (let ((web-id (generate-id)))
    (js-execute obj (format nil "clog['~A']=~A.createImageData(~A,~A)"
                            web-id (script-id obj)
                            width height))
    (make-instance 'clog-image-data
                   :connection-id (clog::connection-id obj)
                   :html-id web-id)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; create-conic-gradient ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric create-conic-gradient (clog-context2d start-angle x y)
  (:documentation "Create conic gradient"))

(defmethod create-conic-gradient ((obj clog-context2d) start-angle x y)
  (let ((web-id (generate-id)))
    (js-execute obj (format nil "clog['~A']=~A.createConicGradient(~A,~A,~A)"
                            web-id (script-id obj)
                            start-angle x y))
    (make-instance 'clog-canvas-gradient
                   :connection-id (clog::connection-id obj)
                   :html-id web-id)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; create-linear-gradient ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric create-linear-gradient (clog-context2d x0 y0 x1 y1)
  (:documentation "Create linear gradient"))

(defmethod create-linear-gradient ((obj clog-context2d) x0 y0 x1 y1)
  (let ((web-id (generate-id)))
    (js-execute obj (format nil "clog['~A']=~A.createLinearGradient(~A,~A,~A,~A)"
                            web-id (script-id obj)
                             x0 y0 x1 y1))
    (make-instance 'clog-canvas-gradient
                   :connection-id (clog::connection-id obj)
                   :html-id web-id)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; create-radial-gradient ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric create-radial-gradient (clog-context2d x0 y0 r0 x1 y1 r1)
  (:documentation "Create radial gradient"))

(defmethod create-radial-gradient ((obj clog-context2d) x0 y0 r0 x1 y1 r1)
  (let ((web-id (generate-id)))
    (js-execute obj (format nil "clog['~A']=~A.createRadialGradient(~A,~A,~A,~A,~A,~A)"
                            web-id (script-id obj)
                            x0 y0 r0 x1 y1 r1))
    (make-instance 'clog-canvas-gradient
                   :connection-id (clog::connection-id obj)
                   :html-id web-id)))

;;;;;;;;;;;;;;;;;;;;
;; create-pattern ;;
;;;;;;;;;;;;;;;;;;;;

(defgeneric create-pattern (clog-context2d clog-obj repetition)
  (:Documentation "Create pattern"))

(defmethod create-pattern ((obj clog-context2d) clog-obj repetition)
  (execute obj (format nil "createPattern(~A,'~A')"
                       (script-id clog-obj) repetition)))

;;;;;;;;;;;;;;;;
;; draw-image ;;
;;;;;;;;;;;;;;;;

(defgeneric draw-image (clog-context2d clog-obj dx dy &key dwidth dheight)
  (:documentation "Draw image at dx dy optionally dwidth and dheight"))

(defmethod draw-image ((obj clog-context2d) clog-obj dx dy &key dwidth dheight)
  (execute obj (format nil "drawImage(~A,~A,~A~A)"
                       (script-id clog-obj) dx dy
                       (if dwidth
                           (format nil "~A,~A" dwidth dheight)
                           ""))))

;;;;;;;;;;;;;;;;;;;;;;;;
;; draw-image-from-to ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric draw-image-from-to (clog-context2d clog-obj
                                sx sy swidth sheight
                                dx dy dwidth dheight)
  (:documentation "Draw image at sx sy swidth sheight to dx dy dwidth dheight"))

(defmethod draw-image-from-to ((obj clog-context2d) clog-obj
                               sx sy swidth sheight
                               dx dy dwidth dheight)
  (execute obj (format nil "drawImage(~A,~A,~A,~A,~A,~A,~A,~A,~A)"
                       (script-id clog-obj)
                       sx sy swidth sheight
                       dx dy dwidth dheight)))

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

(defgeneric path-fill (clog-context2d &key path2d fill-rule)
  (:documentation "Fill a path."))

(defmethod path-fill ((obj clog-context2d) &key path2d fill-rule)
  (execute obj (format nil "fill(~A~A~A)"
                       (if path2d
                           (script-id path2d)
                           "")
                       (if (and path2d fill-rule)
                           ","
                           "")
                       (if fill-rule
                           (format nil "'~A'" fill-rule)
                           ""))))

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

;;;;;;;;;;;;;;;;;;;;
;; get-image-data ;;
;;;;;;;;;;;;;;;;;;;;

(defgeneric get-image-data (clog-context2d sx sy sw sh)
  (:documentation "Get image data from clog-context2d. Returns a CLOG-IMAGE-DATA"))

(defmethod get-image-data ((obj clog-context2d) sx sy sw sh)
  (let ((web-id (generate-id)))
    (js-execute obj (format nil "clog['~A']=~A.getImageData(~A,~A,~A,~A)"
                            web-id (script-id obj)
                            sx sy sw sh))
    (make-instance 'clog-image-data
                   :connection-id (clog::connection-id obj)
                   :html-id web-id)))

;;;;;;;;;;;;;;;;;;;
;; get-line-dash ;;
;;;;;;;;;;;;;;;;;;;

(defgeneric get-line-dash (clog-context2d)
  (:documentation "Set line style dash pattern, e.g. 10, 20"))

(defmethod get-line-dash ((obj clog-context2d))
  (query obj "getLineDash()"))


;;;;;;;;;;;;;;;;;;;
;; get-transform ;;
;;;;;;;;;;;;;;;;;;;

(defgeneric get-transform (clog-context2d)
  (:documentation "Get current transform matrix as clog-matrix"))

(defmethod get-transform ((obj clog-context2d))
  (let ((web-id (generate-id)))
    (js-execute obj (format nil "clog['~A']=~A.getTransform()"
                            web-id (script-id obj)))
    (make-instance 'clog-matrix
                   :connection-id (clog::connection-id obj)
                   :html-id web-id)))

;;;;;;;;;;;;;;;;;;;;;;
;; is-point-in;path ;;
;;;;;;;;;;;;;;;;;;;;;;

(defgeneric is-point-in-path (clog-context2d x y &key path2d fill-rule)
  (:documentation "Returns t if point is in path or PATH2D if specfified"))

(defmethod is-point-in-path ((obj clog-context2d) x y &key path2d fill-rule)
  (js-true-p (query obj (format nil "isPointInPath(~A~A,~A~A)"
                                (if path2d
                                    (format nil "~A," (script-id path2d))
                                    "")
                                x y
                                (if fill-rule
                                    (format nil ",'~A'" fill-rule)
                                    "")))))

;;;;;;;;;;;;;;;;;;;;;;;;
;; is-point-in-stroke ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric is-point-in-stroke (clog-context2d x y &key path2d)
  (:documentation "Returns t if point is in stroke or PATH2D if specfified"))

(defmethod is-point-in-stroke ((obj clog-context2d) x y &key path2d)
  (js-true-p (query obj (format nil "isPointInStroke(~A~A,~A)"
                                (if path2d
                                    (format nil "~A," (script-id path2d))
                                    "")
                                x y))))

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
  (let ((web-id (generate-id)))
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

;;;;;;;;;;;;;;;;;;;;
;; put-image-data ;;
;;;;;;;;;;;;;;;;;;;;

(defgeneric put-image-data (clog-context2d image-data x y)
  (:documentation "Put image-data at x y"))

(defmethod put-image-data ((obj clog-context2d) image-data x y)
  (execute obj (format nil "putImageData(~A,~A,~A)" (script-id image-data) x y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; put-image-data-dirty ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric put-image-data-dirty (clog-context2d image-data x y
                                  dx dy dwidth dheight)
  (:documentation "Put portion of image-data at x y"))

(defmethod put-image-data-dirty ((obj clog-context2d) image-data x y
                                 dx dy dwidth dheight)
  (execute obj (format nil "putImageData(~A,~A,~A,~A,~A,~A,~A)" (script-id image-data) x y
                       dx dy dwidth dheight)))

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

;;;;;;;;;;;;;;;;;;;;;
;; reset-transform ;;
;;;;;;;;;;;;;;;;;;;;;

(defgeneric reset-transform (clog-context2d)
  (:documentation "Restore canvas from stack"))

(defmethod reset-transform ((obj clog-context2d))
  (execute obj "resetTransform()"))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set-transform-with-matrix ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-transform-with-matrix (clog-context2d clog-matrix)
  (:documentation "Set-Transform-With-Matrix"))

(defmethod set-transform-with-matrix ((obj clog-context2d) clog-matrix)
  (execute obj (format nil "set-transform(~A)" (script-id clog-matrix))))

;;;;;;;;;;;;;;;;;;;
;; set-transform ;;
;;;;;;;;;;;;;;;;;;;

(defgeneric set-transform (clog-context2d a b c d e f g)
  (:documentation "Set-Transform"))

(defmethod set-transform ((obj clog-context2d) a b c d e f g)
  (execute obj (format nil "setYransform(~A,~A,~A,~A,~A,~A,~A)" a b c d e f g)))

;;;;;;;;;;;;;;;;;
;; path-stroke ;;
;;;;;;;;;;;;;;;;;

(defgeneric path-stroke (clog-context2d &key path2d)
  (:documentation "Stroke a path."))

(defmethod path-stroke ((obj clog-context2d) &key path2d)
  (execute obj (format nil "stroke(~A)"
                       (if path2d
                           (script-id path2d)
                           ""))))

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
;; Implementation - clog-canvas-gradient
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-canvas-gradient (clog-obj)())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Methods - clog-canvas-gradient
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric add-color-stop (clog-canvas-gradient offset color)
  (:documentation "Add a color stop"))

(defmethod add-color-stop ((obj clog-canvas-gradient) offset color)
  (execute obj (format nil "addColorStop(~A,'~A')" offset color)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-image-data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-image-data (clog-obj)())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Properties - clog-image-data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod width ((obj clog-image-data))
  (js-to-integer (query obj "width")))

(defmethod height ((obj clog-image-data))
  (js-to-integer (query obj "height")))

(defgeneric json-image-data (clog-image-data)
  (:documentation "Setf/get json image data"))

(defmethod json-image-data ((obj clog-image-data))
  (js-query obj (format nil "JSON.stringify(~A.data)" (script-id obj))))

(defmethod (setf json-image-data) (value (obj clog-image-data))
  (js-execute obj (format nil "~A=new ImageData(new Uint8ClampedArray(Object.values(~A)), ~A.width)"
                          (script-id obj) value (script-id obj))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-text-metrics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-text-metrics (clog-obj)())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Properties - clog-text-metrics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod width ((obj clog-text-metrics))
  "Width of text"
  (js-to-float (query obj "width")))

(defgeneric actual-bounding-box-left (clog-text-metrics)
  (:documentation "Actual bounding box left"))

(defmethod actual-bounding-box-left ((obj clog-text-metrics))
  (js-to-float (query obj "actualBoundingBoxLeft")))

(defgeneric actual-bounding-box-right (clog-text-metrics)
  (:documentation "Actual bounding box right"))

(defmethod actual-bounding-box-right ((obj clog-text-metrics))
  (js-to-float (query obj "actualBoundingBoxRight")))

(defgeneric actual-bounding-box-ascent (clog-text-metrics)
  (:documentation "Actual bounding box ascent"))

(defmethod actual-bounding-box-ascent ((obj clog-text-metrics))
  (js-to-float (query obj "actualBoundingBoxAscent")))

(defgeneric actual-bounding-box-descent (clog-text-metrics)
  (:documentation "Actual bounding box descent"))

(defmethod actual-bounding-box-descent ((obj clog-text-metrics))
  (js-to-float (query obj "actualBoundingBoxDescent")))

(defgeneric font-bounding-box-ascent (clog-text-metrics)
  (:documentation "Font bounding box ascent"))

(defmethod font-bounding-box-ascent ((obj clog-text-metrics))
  (js-to-float (query obj "fontBoundingBoxAscent")))

(defgeneric font-bounding-box-descent (clog-text-metrics)
  (:documentation "Font bounding box descent"))

(defmethod font-bounding-box-descent ((obj clog-text-metrics))
  (js-to-float (query obj "fontBoundingBoxDescent")))

(defgeneric em-height-ascent (clog-text-metrics)
  (:documentation "'M' height ascent"))

(defmethod em-height-ascent ((obj clog-text-metrics))
  (js-to-float (query obj "emHeightAscent")))

(defgeneric em-height-descent (clog-text-metrics)
  (:documentation "'M' height descent"))

(defmethod em-height-descent ((obj clog-text-metrics))
  (js-to-float (query obj "emHeightDescent")))

(defgeneric hanging-baseline (clog-text-metrics)
  (:documentation "Hanging baseline"))

(defmethod hanging-baseline ((obj clog-text-metrics))
  (js-to-float (query obj "hangingBaseline")))

(defgeneric alphabetic-baseline (clog-text-metrics)
  (:documentation "Alphabetic baseline"))

(defmethod alphabetic-baseline ((obj clog-text-metrics))
  (js-to-float (query obj "alphabeticBaseline")))

(defgeneric ideographic-baseline (clog-text-metrics)
  (:documentation "Ideographic baseline"))

(defmethod ideographic-baseline ((obj clog-text-metrics))
  (js-to-float (query obj "ideographicBaseline")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-matrix
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-matrix (clog-obj)())

;;;;;;;;;;;;;;;;;;;
;; create-matrix ;;
;;;;;;;;;;;;;;;;;;;

(defgeneric create-matrix (clog-canvas &key matrix)
  (:documentation "Create a new CLOG-Matrix. MATRIX can be
json array 6 element for 2d or 16 for 3d."))


(defmethod create-matrix ((obj clog-canvas) &key matrix)
  (let ((web-id (generate-id)))
    (js-execute obj (format nil "clog['~A']=DOMMatrix(~A)"
                            web-id
                            (if matrix
                                (if (typep matrix 'clog-matrix)
                                    (script-id matrix)
                                    matrix)
                                "")))
    (make-instance 'clog-matrix
                   :connection-id (connection-id obj)
                   :html-id web-id)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Properties - clog-matrix
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;
;; is-2d ;;
;;;;;;;;;;;

(Defgeneric is-2d (clog-matrix)
  (:documentation "Setf/get miter style limit"))

(defmethod is-2d ((obj clog-matrix))
  (js-true-p (query obj "is2d")))

;;;;;;;;;;;;;;;;;
;; is-identity ;;
;;;;;;;;;;llllll;

(Defgeneric is-identity (clog-matrix)
  (:documentation "Setf/get miter style limit"))

(defmethod is-identity ((obj clog-matrix))
  (js-true-p (query obj "isIdentity")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Methods - clog-matrix
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;
;; flip-x ;;
;;;;;;;;;;;;

(defgeneric flip-x (clog-matrix)
  (:documentation "Return flip-x a clog-matrix"))

(defmethod flip-x ((obj clog-matrix))
  (let ((web-id (generate-id)))
    (js-execute obj (format nil "clog['~A']=~A.flipX()"
                            web-id (script-id obj)))
    (make-instance 'clog-matrix
                   :connection-id (clog::connection-id obj)
                   :html-id web-id)))

;;;;;;;;;;;;
;; flip-y ;;
;;;;;;;;;;;;

(defgeneric flip-y (clog-matrix)
  (:documentation "Return flip-y a clog-matrix"))

(defmethod flip-y ((obj clog-matrix))
  (let ((web-id (generate-id)))
    (js-execute obj (format nil "clog['~A']=~A.flipY()"
                            web-id (script-id obj)))
    (make-instance 'clog-matrix
                   :connection-id (clog::connection-id obj)
                   :html-id web-id)))

;;;;;;;;;;;;;
;; inverse ;;
;;;;;;;;;;;;;

(defgeneric inverse (clog-matrix)
  (:documentation "Return inverse a clog-matrix"))

(defmethod inverse ((obj clog-matrix))
  (let ((web-id (generate-id)))
    (js-execute obj (format nil "clog['~A']=~A.inverse()"
                            web-id (script-id obj)))
    (make-instance 'clog-matrix
                   :connection-id (clog::connection-id obj)
                   :html-id web-id)))

;;;;;;;;;;;;;;
;; multiply ;;
;;;;;;;;;;;;;;

(defgeneric multiply (clog-matrix by-matrix)
  (:documentation "Return multiply a clog-matrix"))

(defmethod multiply ((obj clog-matrix) by-matrix)
  (let ((web-id (generate-id)))
    (js-execute obj (format nil "clog['~A']=~A.multiply(~A)"
                            web-id (script-id obj) (script-id by-matrix)))
    (make-instance 'clog-matrix
                   :connection-id (clog::connection-id obj)
                   :html-id web-id)))

;;; DOMMatrixReadOnly.rotateAxisAngle()

;;;;;;;;;;;;
;; rotate ;;
;;;;;;;;;;;;

(defgeneric rotate (clog-matrix angle)
  (:documentation "Return rotate a clog-matrix"))

(defmethod rotate ((obj clog-matrix) angle)
  (let ((web-id (generate-id)))
    (js-execute obj (format nil "clog['~A']=~A.rotate(~A)"
                            web-id (script-id obj) angle))
    (make-instance 'clog-matrix
                   :connection-id (clog::connection-id obj)
                   :html-id web-id)))

;;; DOMMatrixReadOnly.rotateFromVector()

;;;;;;;;;;;;;;;;;;
;; scale-matrix ;;
;;;;;;;;;;;;;;;;;;

(defgeneric scale-matrix (clog-matrix sx &optional sy sz ox oy oz)
  (:documentation "Return scale a clog-matrix by sx and optionally to
sy sz ox oy oz"))

(defmethod scale-matrix ((obj clog-matrix) sx &optional sy sz ox oy oz)
  (let ((web-id (generate-id)))
    (js-execute obj (format nil "clog['~A']=~A.scale(~A~A~A~A~A~A)"
                            web-id (script-id obj) sx
                            (if sy (format nil ",~A" sy) "")
                            (if sz (format nil ",~A" sz) "")
                            (if ox (format nil ",~A" ox) "")
                            (if oy (format nil ",~A" oy) "")
                            (if oz (format nil ",~A" oz) "")))
    (make-instance 'clog-matrix
                   :connection-id (clog::connection-id obj)
                   :html-id web-id)))

;;;;;;;;;;;;;
;; scale3d ;;
;;;;;;;;;;;;;

(defgeneric scale3d (clog-matrix sx &optional sy sz ox oy oz)
  (:documentation "Return scale3d a clog-matrix by sx and optionally to
sy sz ox oy oz"))

(defmethod scale3d ((obj clog-matrix) sx &optional sy sz ox oy oz)
  (let ((web-id (generate-id)))
    (js-execute obj (format nil "clog['~A']=~A.scale3d(~A~A~A~A~A~A)"
                            web-id (script-id obj) sx
                            (if sy (format nil ",~A" sy) "")
                            (if sz (format nil ",~A" sz) "")
                            (if ox (format nil ",~A" ox) "")
                            (if oy (format nil ",~A" oy) "")
                            (if oz (format nil ",~A" oz) "")))
    (make-instance 'clog-matrix
                   :connection-id (clog::connection-id obj)
                   :html-id web-id)))

;; skewX skewY

;;;;;;;;;;;;;;;;;;;;;;
;; translate-matrix ;;
;;;;;;;;;;;;;;;;;;;;;;

(defgeneric translate-matrix (clog-matrix x y &optional z)
  (:documentation "Return translate-matrix a clog-matrix by x y and optionally z"))

(defmethod translate-matrix ((obj clog-matrix) x y &optional z)
  (let ((web-id (generate-id)))
    (js-execute obj (format nil "clog['~A']=~A.translate(~A,~A~A)"
                            web-id (script-id obj) x y
                            (if z (format nil ",~A" z) "")))
    (make-instance 'clog-matrix
                   :connection-id (clog::connection-id obj)
                   :html-id web-id)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-path2d
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-path2d (clog-obj)())

;;;;;;;;;;;;;;;;;;;
;; create-path2d ;;
;;;;;;;;;;;;;;;;;;;

(defgeneric create-path2d (clog-canvas &key path2d)
  (:documentation "Create a new CLOG-Path2d. If CLOG-PATH2D creates a copy."))


(defmethod create-path2d ((obj clog-canvas) &key path2d)
  (let ((web-id (generate-id)))
    (js-execute obj (format nil "clog['~A']=Path2D(~A)"
                            web-id
                            (if path2d
                                (if (typep path2d 'clog-path2d)
                                    (script-id path2d)
                                    path2d)
                                "")))
    (make-instance 'clog-path2d
                   :connection-id (connection-id obj)
                   :html-id web-id)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Methods - clog-path2d
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
;; add-path ;;
;;;;;;;;;;;;;;

(defgeneric add-path (clog-path2d path2d)
  (:documentation "Add Path to this Path"))

(defmethod add-path ((obj clog-path2d) path2d)
  (execute obj (format nil "addPath(~A)" (script-id path2d))))

;;;;;;;;;;;;;;;;
;; close-path ;;
;;;;;;;;;;;;;;;;

(defmethod close-path ((obj clog-path2d))
  (execute obj "closePath()"))

;;;;;;;;;;;;;
;; move-to ;;
;;;;;;;;;;;;;

(defmethod move-to ((obj clog-path2d) x y)
  (execute obj (format nil "moveTo(~A,~A)" x y)))

;;;;;;;;;;;;;
;; line-to ;;
;;;;;;;;;;;;;

(defmethod line-to ((obj clog-path2d) x y)
  (execute obj (format nil "lineTo(~A,~A)" x y)))

;;;;;;;;;;;;;;;;;;;;;
;; bezier-curve-to ;;
;;;;;;;;;;;;;;;;;;;;;

(defmethod bezier-curve-to ((obj clog-path2d) cp1x cp1y cp2x cp2y x y)
  (execute obj (format nil "bezierCurveTo(~A,~A,~A,~A,~A,~A)"
                       cp1x cp1y cp2x cp2y x y)))

;;;;;;;;;;;;;;;;;;;;;;;;
;; quadratic-curve-to ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod quadratic-curve-to ((obj clog-path2d) cpx cpy x y)
  (execute obj (format nil "quadraticCurveTo(~A,~A,~A,~A)" cpx cpy x y)))

;;;;;;;;;
;; arc ;;
;;;;;;;;;

(defmethod arc ((obj clog-path2d) x y radius start-angle end-angle
                &key (anticlockwise nil))
  (execute obj (format nil "arc(~A,~A,~A,~A,~A~A)"
                       x y radius start-angle end-angle
                       (if anticlockwise
                           (format nil ",~A" anticlockwise)
                           ""))))

;;;;;;;;;;;;
;; arc-to ;;
;;;;;;;;;;;;

(defmethod arc-to ((obj clog-path2d) x1 y1 x2 y2)
  (execute obj (format nil "arcTo(~A,~A,~A,~A)" x1 y1 x2 y2)))


;;;;;;;;;;;;;
;; ellipse ;;
;;;;;;;;;;;;;

(defmethod ellipse ((obj clog-path2d) x y radius-x radius-y rotation
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

(defmethod rect ((obj clog-path2d) x y width height)
  (execute obj (format nil "rect(~A,~A,~A,~A)" x y width height)))
