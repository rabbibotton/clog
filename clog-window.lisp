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

;;;;;;;;;;;;;;;;;
;; pixel-ratio ;;
;;;;;;;;;;;;;;;;;

(defgeneric pixel-ratio (clog-window)
  (:documentation "Get device pixel ratio."))

(defmethod pixel-ratio ((obj clog-window))
  (property obj "devicePixelRatio"))

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
;; Scroll-by ;;
;;;;;;;;;;;;;;;

(defgeneric scroll-by (clog-window x y)
  (:documentation "Scroll browser window by x y."))

(defmethod scroll-by ((obj clog-window) x y)
  (jquery-execute obj (format nil "scrollBy(~A,~A)" x y)))

;;;;;;;;;;;;;;;
;; scroll-to ;;
;;;;;;;;;;;;;;;

(defgeneric scroll-to (clog-window x y)
  (:documentation "Scroll browser window to x y."))

(defmethod scroll-to ((obj clog-window) x y)
  (jquery-execute obj (format nil "scrollTo(~A,~A)" x y)))

;;;;;;;;;;;;;;;;;;
;; close-window ;;
;;;;;;;;;;;;;;;;;;

(defgeneric close-window (clog-window)
  (:documentation "Close browser window."))

(defmethod close-window ((obj clog-window))
  (jquery-execute obj "close()"))

;;;;;;;;;;;;;;;;;;;;;;
;; close-connection ;;
;;;;;;;;;;;;;;;;;;;;;;

(defgeneric close-connection (clog-window)
  (:documentation "Close connection to browser with out closing browser."))

(defmethod close-connection ((obj clog-window))
  (cc:cclose (connection-id obj)))

;;;;;;;;;;;;;;;;;;
;; Set-on-abort ;;
;;;;;;;;;;;;;;;;;;

(defgeneric set-on-abort (clog-window on-abort-handler)
  (:documentation "Set the ON-ABORT-HANDLER for CLOG-OBJ. If ON-ABORT-HANDLER
is nil unbind the event."))

(defmethod set-on-abort ((obj clog-window) on-abort-handler)
  (let ((on-abort on-abort-handler))      
    (set-event obj "abort"
	       (lambda (data)
		 (declare (ignore data))
		 (funcall on-abort)))))

;;;;;;;;;;;;;;;;;;
;; Set-on-error ;;
;;;;;;;;;;;;;;;;;;

(defgeneric set-on-error (clog-window on-error-handler)
  (:documentation "Set the ON-ERROR-HANDLER for CLOG-OBJ. If ON-ERROR-HANDLER
is nil unbind the event."))

(defmethod set-on-error ((obj clog-window) on-error-handler)
  (let ((on-error on-error-handler))      
    (set-event obj "error"
	       (lambda (data)
		 (declare (ignore data))
		 (funcall on-error)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set-on-before-unload ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-before-unload (clog-window on-before-unload-handler)
  (:documentation "Set the ON-BEFORE-UNLOAD-HANDLER for CLOG-OBJ. If
ON-BEFORE-UNLOAD-HANDLER is nil unbind the event."))

(defmethod set-on-before-unload ((obj clog-window) on-before-unload-handler)
  (let ((on-before-unload on-before-unload-handler))      
    (set-event obj "beforeunload"
	       (lambda (data)
		 (declare (ignore data))
		 (funcall on-before-unload)))))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Set-on-hash-change ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-hash-change (clog-window on-hash-change-handler)
  (:documentation "Set the ON-HASH-CHANGE-HANDLER for CLOG-OBJ. If
ON-HASH-CHANGE-HANDLER is nil unbind the event."))

(defmethod set-on-hash-change ((obj clog-window) on-hash-change-handler)
  (let ((on-hash-change on-hash-change-handler))      
    (set-event obj "hashchange"
	       (lambda (data)
		 (declare (ignore data))
		 (funcall on-hash-change)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set-on-orientation-change ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-orientation-change (clog-window
				       on-orientation-change-handler)
  (:documentation "Set the ON-ORIENTATION-CHANGE-HANDLER for CLOG-OBJ.
If ON-ORIENTATION-CHANGE-HANDLER is nil unbind the event."))

(defmethod set-on-orientation-change ((obj clog-window)
				      on-orientation-change-handler)
  (let ((on-orientation-change on-orientation-change-handler))      
    (set-event obj "orientationchange"
	       (lambda (data)
		 (declare (ignore data))
		 (funcall on-orientation-change)))))

;;;;;;;;;;;;;;;;;;;;
;; Set-on-storage ;;
;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-storage (clog-window on-storage-handler)
  (:documentation "Set the ON-STORAGE-HANDLER for CLOG-OBJ. If
ON-STORAGE-HANDLER is nil unbind the event."))

(defmethod set-on-storage ((obj clog-window) on-storage-handler)
  (let ((on-storage on-storage-handler))      
    (set-event obj "storage"
	       (lambda (data)
		 (declare (ignore data))
		 (funcall on-storage)))))

;;;;;;;;;;;;;;;;;;;
;; Set-on-resize ;;
;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-resize (clog-window on-resize-handler)
  (:documentation "Set the ON-RESIZE-HANDLER for CLOG-OBJ. If ON-RESIZE-HANDLER
is nil unbind the event."))

(defmethod set-on-resize ((obj clog-window) on-resize-handler)
  (let ((on-resize on-resize-handler))      
    (set-event obj "resize"
	       (lambda (data)
		 (declare (ignore data))
		 (funcall on-resize)))))
