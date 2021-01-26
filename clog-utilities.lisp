;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG - The Common Lisp Omnificent GUI                                 ;;;;
;;;; (c) 2020-2021 David Botton                                            ;;;;
;;;; License BSD 3 Clause                                                  ;;;;
;;;;                                                                       ;;;;
;;;; clog-system.lisp                                                      ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:in-package :clog)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - CLOG Utilities
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

;;;;;;;;;;;;;;;;;;
;; open-browser ;;
;;;;;;;;;;;;;;;;;;

(defun open-browser (&key (url "http://127.0.0.1:8080"))
  "Open a web browser to URL."
  (trivial-open-browser:open-browser url))

;;;;;;;;;;;;;;;;;;;
;; escape-string ;;
;;;;;;;;;;;;;;;;;;;

(defun escape-string (str)
  "Escape STR for sending to browser script."
  (let ((res))
    (setf res (format nil "~A" str))
    (setf res (ppcre:regex-replace-all "\\x22" res "\\x22"))
    (setf res (ppcre:regex-replace-all "\\x27" res "\\x27"))
    (setf res (ppcre:regex-replace-all "\\x0A" res "\\x0A"))
    (setf res (ppcre:regex-replace-all "\\x0D" res "\\x0D"))
    res))

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

;; From - https://www.w3schools.com/
;;
;; linear-gradient(direction, color-stop1, color-stop2, ...);
;; radial-gradient(shape size at position, start-color, ..., last-color);
;; repeating-linear-gradient(angle | to side-or-corner, color-stop1, color-stop2, ...);
;; epeating-radial-gradient(shape size at position, start-color, ..., last-color);

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
