;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG - The Common Lisp Omnificent GUI                                 ;;;;
;;;; (c) 2020-2021 David Botton                                            ;;;;
;;;; License BSD 3 Clause                                                  ;;;;
;;;;                                                                       ;;;;
;;;; clog-window.lisp                                                      ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:in-package :clog)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-window
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-window (clog-obj)()
  (:documentation "CLOG Window Objects encapsulate the window."))

;;;;;;;;;;;;;;;;;;;;;;
;; make-clog-window ;;
;;;;;;;;;;;;;;;;;;;;;;

(defun make-clog-window (connection-id)
  "Construct a new clog-window. (Private)"
  (make-instance 'clog-window :connection-id connection-id :html-id "window"))

;;;;;;;;;;;;;;;;;
;; window-name ;;
;;;;;;;;;;;;;;;;;

(defgeneric window-name (clog-window)
  (:documentation "Get/Setf name for use by hyperlink \"target\" for this
window."))

(defmethod window-name ((obj clog-window))
  (property obj "name"))

(defgeneric set-window-name (clog-window value))
  
(defmethod set-window-name ((obj clog-window) value)
  (setf (property obj "name") value))
(defsetf window-name set-window-name)

;;;;;;;;;;;;;;;;
;; status-bar ;;
;;;;;;;;;;;;;;;;

(defgeneric status-bar (clog-window)
  (:documentation "Get/Setf status bar text."))

(defmethod status-bar ((obj clog-window))
  (property obj "status"))

(defgeneric set-status-bar (clog-window value))
  
(defmethod set-status-bar ((obj clog-window) value)
  (setf (property obj "status") value))
(defsetf status-bar set-status-bar)

;;;;;;;;;;;;;;;;;;
;; inner-height ;;
;;;;;;;;;;;;;;;;;;

(defgeneric inner-height (clog-window)
  (:documentation "Get/Setf inner height of browser window."))

(defmethod inner-height ((obj clog-window))
  (property obj "innerHeight"))

(defgeneric set-inner-height (clog-window value))
  
(defmethod set-inner-height ((obj clog-window) value)
  (setf (property obj "innerHeight") value))
(defsetf inner-height set-inner-height)

;;;;;;;;;;;;;;;;;
;; inner-width ;;
;;;;;;;;;;;;;;;;;

(defgeneric inner-width (clog-window)
  (:documentation "Get/Setf inner width of browser window."))

(defmethod inner-width ((obj clog-window))
  (property obj "innerWidth"))

(defgeneric set-inner-width (clog-window value))
  
(defmethod set-inner-width ((obj clog-window) value)
  (setf (property obj "innerWidth") value))
(defsetf inner-width set-inner-width)

;;;;;;;;;;;;;;;;;;
;; outer-height ;;
;;;;;;;;;;;;;;;;;;

(defgeneric outer-height (clog-window)
  (:documentation "Get/Setf outer height of browser window."))

(defmethod outer-height ((obj clog-window))
  (property obj "outerHeight"))

(defgeneric set-outer-height (clog-window value))
  
(defmethod set-outer-height ((obj clog-window) value)
  (setf (property obj "outerHeight") value))
(defsetf outer-height set-outer-height)

;;;;;;;;;;;;;;;;;
;; outer-width ;;
;;;;;;;;;;;;;;;;;

(defgeneric outer-width (clog-window)
  (:documentation "Get/Setf outer width of browser window."))

(defmethod outer-width ((obj clog-window))
  (property obj "outerWidth"))

(defgeneric set-outer-width (clog-window value))
  
(defmethod set-outer-width ((obj clog-window) value)
  (setf (property obj "outerWidth") value))
(defsetf outer-width set-outer-width)

;;;;;;;;;;;;;;
;; x-offset ;;
;;;;;;;;;;;;;;

(defgeneric x-offset (clog-window)
  (:documentation "Get/Setf browser window x offset from left edge."))

(defmethod x-offset ((obj clog-window))
  (property obj "pageXOffset"))

(defgeneric set-x-offset (clog-window value))
  
(defmethod set-x-offset ((obj clog-window) value)
  (setf (property obj "pageXOffset") value))
(defsetf x-offset set-x-offset)

;;;;;;;;;;;;;;
;; y-offset ;;
;;;;;;;;;;;;;;

(defgeneric y-offset (clog-window)
  (:documentation "Get/Setf browser window y offset from top edge."))

(defmethod y-offset ((obj clog-window))
  (property obj "pageYOffset"))


(defgeneric set-y-offset (clog-window value))
  
(defmethod set-y-offset ((obj clog-window) value)
  (setf (property obj "pageYOffset") value))
(defsetf y-offset set-y-offsett)

;;;;;;;;;
;; top ;;
;;;;;;;;;

(defgeneric top (clog-window)
  (:documentation "Get/Setf browser y postion."))

(defmethod top ((obj clog-window))
  (property obj "screenY"))

(defgeneric set-top (clog-window value))
  
(defmethod set-top ((obj clog-window) value)
  (setf (property obj "screenY") value))
(defsetf top set-top)

;;;;;;;;;;
;; left ;;
;;;;;;;;;;

(defgeneric left (clog-window)
  (:documentation "Get/Setf browser x position."))

(defmethod left ((obj clog-window))
  (property obj "screenX"))

(defgeneric set-left (clog-window value))
  
(defmethod set-left ((obj clog-window) value)
  (setf (property obj "screenX") value))
(defsetf left set-x-offset)

;;;;;;;;;;;
;; alert ;;
;;;;;;;;;;;

(defgeneric alert (clog-window message)
  (:documentation "Launch an alert box. Note that as long as not dismissed
events and messages may not be trasmitted on most browsers."))

(defmethod alert ((obj clog-window) message)
  (cc:alert-box (connection-id obj) message))

;;;;;;;;;;;;;;;;;
;; log-console ;;
;;;;;;;;;;;;;;;;;

(defgeneric log-console (clog-window message)
  (:documentation "Print message to browser console."))

(defmethod log-console ((obj clog-window) message)
  (cc:execute (connection-id obj) (format nil "console.log('~A')"
					  (cc:escape-string message))))

;;;;;;;;;;;;;;;
;; log-error ;;
;;;;;;;;;;;;;;;

(defgeneric log-error (clog-window message)
  (:documentation "Print error message to browser console."))

(defmethod log-error ((obj clog-window) message)
  (cc:execute (connection-id obj) (format nil "console.error('~A')"
					  (cc:escape-string message))))

;;;;;;;;;;;;;;;;;;
;; print-window ;;
;;;;;;;;;;;;;;;;;;

(defgeneric print-window (clog-window)
  (:documentation "Send browser window to printer."))

(defmethod print-window ((obj clog-window))
  (cc:execute (connection-id obj) "print()"))

;;;;;;;;;;;;;;;
;; scroll-by ;;
;;;;;;;;;;;;;;;

(defgeneric scroll-by (clog-window x y)
  (:documentation "Scroll browser window by x y."))

(defmethod scroll-by ((obj clog-window) x y)
  (jquery-execute obj (format nil "scrollBy(~A,~A)")))

;;;;;;;;;;;;;;;
;; scroll-to ;;
;;;;;;;;;;;;;;;

(defgeneric scroll-to (clog-window x y)
  (:documentation "Scroll browser window to x y."))

(defmethod scroll-to ((obj clog-window) x y)
  (jquery-execute obj (format nil "scrollTo(~A,~A)")))
