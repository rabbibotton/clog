;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG - The Common Lisp Omnificent GUI                                 ;;;;
;;;; (c) 2020-2022 David Botton                                            ;;;;
;;;; License BSD 3 Clause                                                  ;;;;
;;;;                                                                       ;;;;
;;;; clog-utilities.lisp                                                   ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:in-package :clog)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - make-hash-table*
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-hash-table* (&rest args)
  "Use native concurrent hash tables"
  ;; This covers sbcl ecl mazzano lw and ccl.
  ;; (lw and ccl default hash is synchronized)
  #+(or sbcl ecl mezzano)
  (apply #'make-hash-table :synchronized t args)
  #-(or sbcl ecl mezzano) (apply #'make-hash-table args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-group
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-group ()
  ((controls
   :accessor controls
   :initform (make-hash-table* :test 'equalp))))

(defun create-group ()
  "Return a new CLOG-GROUP object for storing CLOG-OBJs. They are indexed by
their HTML-ID or an arbitrary NAME."
  (make-instance 'clog-group))

(defgeneric add (clog-group clog-obj &key name)
  (:documentation "Add CLOG-OBJ to a CLOG-GROUP indexed by the html-id of
CLOG-OBJ unless :NAME is set and is used instead."))

(defmethod add ((group clog-group) clog-obj &key (name nil))
  (let ((id (if name
		name
		(html-id clog-obj))))
    (setf (gethash id (controls group)) clog-obj)))

(defgeneric obj (clog-group name)
  (:documentation "Retrieve from CLOG-GROUP the CLOG-OBJ with name"))

(defmethod obj ((group clog-group) name)
  (gethash name (controls group)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - JS Utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;
;; js-true-p ;;
;;;;;;;;;;;;;;;

(defun js-true-p (value)
  "Return true if VALUE equalp the string true"
  (equalp value "true"))

;;;;;;;;;;;;;;;
;; p-true-js ;;
;;;;;;;;;;;;;;;

(defun p-true-js (value)
  "Return \"true\" if VALUE t"
  (if value
      "true"
      "false"))

;;;;;;;;;;;;;
;; js-on-p ;;
;;;;;;;;;;;;;

(defun js-on-p (value)
  "Return true if VALUE equalp the string on"
  (equalp value "on"))

;;;;;;;;;;;;;
;; p-on-js ;;
;;;;;;;;;;;;;

(defun p-on-js (value)
  "Return \"on\" if VALUE t or return \"off\""
  (if value
      "on"
      "off"))

;;;;;;;;;;;;;;;;;;;
;; escape-string ;;
;;;;;;;;;;;;;;;;;;;

(defun escape-string (str &key (no-nil nil))
  "Escape STR for sending to browser script. If no-nil is t (default is nil)
if str is NIL returns empty string otherwise returns nil."
  (if (and (not str) (not no-nil))
      nil
      (let ((res))
	(setf res (format nil "~@[~A~]" str))
	(setf res (ppcre:regex-replace-all "\\x22" res "\\x22"))
	(setf res (ppcre:regex-replace-all "\\x27" res "\\x27"))
	(setf res (ppcre:regex-replace-all "\\x0A" res "\\x0A"))
	(setf res (ppcre:regex-replace-all "\\x0D" res "\\x0D"))
	res)))

;;;;;;;;;;;;;;
;; lf-to-br ;;
;;;;;;;;;;;;;;

(defun lf-to-br (str)
  "Change line feeds to <br>."
  (ppcre:regex-replace-all "\\x0A" str "<br>"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - Color Utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;
;; rgb ;;
;;;;;;;;;

(defun rgb (red green blue)
  "Return RGB string, red green and blue may be 0-255"
  (format nil "rgb(~A, ~A, ~A)" red green blue))

;;;;;;;;;;
;; rgba ;;
;;;;;;;;;;

(defun rgba (red green blue alpha)
  "Return RGBA string, red green and blue may be 0-255, alpha 0.0 - 1.0"
  (format nil "rgba(~A, ~A, ~A, ~A)" red green blue alpha))

;;;;;;;;;
;; hsl ;;
;;;;;;;;;

(defun hsl (hue saturation lightness)
  "Return HSL string, hue 0-360, saturation and lightness 0%-100%"
  (format nil "hsl(~A, ~A, ~A)" hue saturation lightness))

;;;;;;;;;;
;; hsla ;;
;;;;;;;;;;

(defun hsla (hue saturation lightness alpha)
  "Return HSLA string, hue 0-360, saturation and lightness 0%-100%,
alpha 0.0 - 1.0"
  (format nil "hsla(~A, ~A, ~A, ~A)" hue saturation lightness alpha))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - Units
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;
;; unit ;;
;;;;;;;;;;

;; cm	centimeters
;; mm	millimeters
;; in	inches (1in = 96px = 2.54cm
;; px  	pixels (1px = 1/96th of 1in)
;; pt	points (1pt = 1/72 of 1in)
;; pc	picas (1pc = 12 pt)
;; em	Relative to the font-size of the element (2em means 2 times the size of the current font)	
;; ex	Relative to the x-height of the current font (rarely used)	
;; ch	Relative to the width of the "0" (zero)	
;; rem	Relative to font-size of the root element	
;; vw	Relative to 1% of the width of the viewport*	
;; vh	Relative to 1% of the height of the viewport*	
;; vmin	Relative to 1% of viewport's* smaller dimension	
;; vmax	Relative to 1% of viewport's* larger dimension	
;; %	Relative to the parent element
;;
;; * Viewport = the browser window size. If the viewport is 50cm wide, 1vw = 0.5cm.

(deftype unit-type () '(member :cm :mm :in :px :pt :pc :em :ex :ch :rem :vw
			 :vh :vmin :vmax :%))

(defun unit (unit-type value)
  "produce a string from numeric value with UNIT-TYPE appended."
  (format nil "~A~A" value unit-type))
  
;; https://www.w3schools.com/colors/colors_names.asp
;;
;; From - https://www.w3schools.com/
;;
;; linear-gradient(direction, color-stop1, color-stop2, ...);
;; radial-gradient(shape size at position, start-color, ..., last-color);
;; repeating-linear-gradient(angle | to side-or-corner, color-stop1, color-stop2, ...);
;; repeating-radial-gradient(shape size at position, start-color, ..., last-color);
;;
;;
;; The following list are the best web safe fonts for HTML and CSS:
;;
;; Arial (sans-serif)
;; Verdana (sans-serif)
;; Helvetica (sans-serif)
;; Tahoma (sans-serif)
;; Trebuchet MS (sans-serif)
;; Times New Roman (serif)
;; Georgia (serif)
;; Garamond (serif)
;; Courier New (monospace)
;; Brush Script MT (cursive)
