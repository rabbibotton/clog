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
  (query obj "name"))

(defgeneric set-window-name (clog-window value))
  
(defmethod set-window-name ((obj clog-window) value)
  (execute obj (format nil "name='~A'" (escape-string value)))
  value)
(defsetf window-name set-window-name)

;;;;;;;;;;;;;;;;
;; status-bar ;;
;;;;;;;;;;;;;;;;

(defgeneric status-bar (clog-window)
  (:documentation "Get/Setf status bar text."))

(defmethod status-bar ((obj clog-window))
  (query obj "status"))

(defgeneric set-status-bar (clog-window value))
  
(defmethod set-status-bar ((obj clog-window) value)
  (execute obj (format nil "status='~A'" (escape-string value)))
  value)
(defsetf status-bar set-status-bar)

;;;;;;;;;;;;;;;;;;
;; inner-height ;;
;;;;;;;;;;;;;;;;;;

(defgeneric inner-height (clog-window)
  (:documentation "Get/Setf inner height of browser window."))

(defmethod inner-height ((obj clog-window))
  (parse-integer (query obj "innerHeight") :junk-allowed t))

(defgeneric set-inner-height (clog-window value))
  
(defmethod set-inner-height ((obj clog-window) value)
  (execute obj (format nil "innerHeight='~A'" (escape-string value)))
  value)
(defsetf inner-height set-inner-height)

;;;;;;;;;;;;;;;;;
;; inner-width ;;
;;;;;;;;;;;;;;;;;

(defgeneric inner-width (clog-window)
  (:documentation "Get/Setf inner width of browser window."))

(defmethod inner-width ((obj clog-window))
  (parse-integer (query obj "innerWidth") :junk-allowed t))

(defgeneric set-inner-width (clog-window value))
  
(defmethod set-inner-width ((obj clog-window) value)
  (execute obj (format nil "innerWidth='~A'" (escape-string value)))
  value)
(defsetf inner-width set-inner-width)

;;;;;;;;;;;;;;;;;;
;; outer-height ;;
;;;;;;;;;;;;;;;;;;

(defgeneric outer-height (clog-window)
  (:documentation "Get/Setf outer height of browser window."))

(defmethod outer-height ((obj clog-window))
  (parse-integer (query obj "outerHeight") :junk-allowed t))

(defgeneric set-outer-height (clog-window value))
  
(defmethod set-outer-height ((obj clog-window) value)
  (execute obj (format nil "outerHeight='~A'" (escape-string value)))
  value)
(defsetf outer-height set-outer-height)

;;;;;;;;;;;;;;;;;
;; outer-width ;;
;;;;;;;;;;;;;;;;;

(defgeneric outer-width (clog-window)
  (:documentation "Get/Setf outer width of browser window."))

(defmethod outer-width ((obj clog-window))
  (parse-integer (query obj "outerWidth") :junk-allowed t))

(defgeneric set-outer-width (clog-window value))
  
(defmethod set-outer-width ((obj clog-window) value)
  (execute obj (format nil "outerWidth='~A'" (escape-string value)))
  value)
(defsetf outer-width set-outer-width)

;;;;;;;;;;;;;;
;; x-offset ;;
;;;;;;;;;;;;;;

(defgeneric x-offset (clog-window)
  (:documentation "Get/Setf browser window x offset from left edge."))

(defmethod x-offset ((obj clog-window))
  (parse-integer (query obj "pageXOffset") :junk-allowed t))

(defgeneric set-x-offset (clog-window value))
  
(defmethod set-x-offset ((obj clog-window) value)
  (execute obj (format nil "pageXOffset='~A'" (escape-string value)))
  value)
(defsetf x-offset set-x-offset)

;;;;;;;;;;;;;;
;; y-offset ;;
;;;;;;;;;;;;;;

(defgeneric y-offset (clog-window)
  (:documentation "Get/Setf browser window y offset from top edge."))

(defmethod y-offset ((obj clog-window))
  (parse-integer (query obj "pageYOffset") :junk-allowed t))

(defgeneric set-y-offset (clog-window value))
  
(defmethod set-y-offset ((obj clog-window) value)
  (execute obj (format nil "pageYOffset='~A'" (escape-string value)))
  value)
(defsetf y-offset set-y-offset)

;;;;;;;;;
;; top ;;
;;;;;;;;;

(defgeneric top (clog-window)
  (:documentation "Get/Setf browser y postion."))

(defmethod top ((obj clog-window))
  (parse-integer (query obj "screenY") :junk-allowed t))

(defgeneric set-top (clog-window value))
  
(defmethod set-top ((obj clog-window) value)
  (execute obj (format nil "screenY='~A'" (escape-string value)))
  value)
(defsetf top set-top)

;;;;;;;;;;
;; left ;;
;;;;;;;;;;

(defgeneric left (clog-window)
  (:documentation "Get/Setf browser x position."))

(defmethod left ((obj clog-window))
  (parse-integer (query obj "screenX") :junk-allowed t))

(defgeneric set-left (clog-window value))
  
(defmethod set-left ((obj clog-window) value)
  (execute obj (format nil "screenX='~A'" (escape-string value)))
  value)
(defsetf left set-left)

;;;;;;;;;;;;;;;;;
;; pixel-ratio ;;
;;;;;;;;;;;;;;;;;

(defgeneric pixel-ratio (clog-window)
  (:documentation "Get device pixel ratio."))

(defmethod pixel-ratio ((obj clog-window))
  (query obj "devicePixelRatio"))

;;;;;;;;;;;;;;;;;;
;; screen-width ;;
;;;;;;;;;;;;;;;;;;

(defgeneric screen-width (clog-window)
  (:documentation "Get screen width."))

(defmethod screen-width ((obj clog-window))
  (parse-integer (query obj "screen.width") :junk-allowed t))

;;;;;;;;;;;;;;;;;;;
;; screen-height ;;
;;;;;;;;;;;;;;;;;;;

(defgeneric screen-height (clog-window)
  (:documentation "Get screen height."))

(defmethod screen-height ((obj clog-window))
  (parse-integer (query obj "screen.height") :junk-allowed t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; screen-available-width ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric screen-available-width (clog-window)
  (:documentation "Get available screen width."))

(defmethod screen-available-width ((obj clog-window))
  (parse-integer (query obj "screen.availWidth") :junk-allowed t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; screen-available-height ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric screen-available-height (clog-window)
  (:documentation "Get available screen height."))

(defmethod screen-available-height ((obj clog-window))
  (parse-integer (query obj "screen.availHeight") :junk-allowed t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; screen-available-left ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric screen-available-left (clog-window)
  (:documentation "Get available screen left."))

(defmethod screen-available-left ((obj clog-window))
  (parse-integer (query obj "screen.availLeft") :junk-allowed t))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; screen-available-top ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric screen-available-top (clog-window)
  (:documentation "Get available screen top."))

(defmethod screen-available-top ((obj clog-window))
  (parse-integer (query obj "screen.availTop") :junk-allowed t))

;;;;;;;;;;;;;;;;;;;;;;;;
;; screen-color-depth ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric screen-color-depth (clog-window)
  (:documentation "Get screen color depth."))

(defmethod screen-color-depth ((obj clog-window))
  (query obj "screen.colorDepth"))

;;;;;;;;;;;
;; alert ;;
;;;;;;;;;;;

(defgeneric alert (clog-window message)
  (:documentation "Launch an alert box. Note that as long as not dismissed
events and messages may not be trasmitted on most browsers."))

(defmethod alert ((obj clog-window) message)
  (execute obj (format nil "alert('~A');" (escape-string message))))

;;;;;;;;;;;;;;;;;
;; log-console ;;
;;;;;;;;;;;;;;;;;

(defgeneric log-console (clog-window message)
  (:documentation "Print message to browser console."))

(defmethod log-console ((obj clog-window) message)
  (execute obj (format nil "console.log('~A')"
		       (escape-string message))))

;;;;;;;;;;;;;;;
;; log-error ;;
;;;;;;;;;;;;;;;

(defgeneric log-error (clog-window message)
  (:documentation "Print error message to browser console."))

(defmethod log-error ((obj clog-window) message)
  (execute obj (format nil "console.error('~A')"
		       (escape-string message))))

;;;;;;;;;;;;;;;;;;
;; print-window ;;
;;;;;;;;;;;;;;;;;;

(defgeneric print-window (clog-window)
  (:documentation "Send browser window to printer."))

(defmethod print-window ((obj clog-window))
  (execute obj "print()"))

;;;;;;;;;;;;;;;
;; scroll-by ;;
;;;;;;;;;;;;;;;

(defgeneric scroll-by (clog-window x y)
  (:documentation "Scroll browser window by x y."))

(defmethod scroll-by ((obj clog-window) x y)
  (execute obj (format nil "scrollBy(~A,~A)" x y)))

;;;;;;;;;;;;;;;
;; scroll-to ;;
;;;;;;;;;;;;;;;

(defgeneric scroll-to (clog-window x y)
  (:documentation "Scroll browser window to x y."))

(defmethod scroll-to ((obj clog-window) x y)
  (execute obj (format nil "scrollTo(~A,~A)" x y)))

;;;;;;;;;;;;;;;;;;;;
;; move-window-by ;;
;;;;;;;;;;;;;;;;;;;;

(defgeneric move-window-by (clog-window x y)
  (:documentation "Move browser window by x y."))

(defmethod move-window-by ((obj clog-window) x y)
  (execute obj (format nil "moveBy(~A,~A)" x y)))

;;;;;;;;;;;;;;;;;;;;
;; move-window-to ;;
;;;;;;;;;;;;;;;;;;;;

(defgeneric move-window-to (clog-window x y)
  (:documentation "Move browser window to x y."))

(defmethod move-window-to ((obj clog-window) x y)
  (execute obj (format nil "moveTo(~A,~A)" x y)))

;;;;;;;;;;;;;;;
;; resize-by ;;
;;;;;;;;;;;;;;;

(defgeneric resize-by (clog-window x y)
  (:documentation "Resize browser window by x y."))

(defmethod resize-by ((obj clog-window) x y)
  (execute obj (format nil "resizeBy(~A,~A)" x y)))

;;;;;;;;;;;;;;;
;; resize-to ;;
;;;;;;;;;;;;;;;

(defgeneric resize-to (clog-window x y)
  (:documentation "Resize browser window to x y."))

(defmethod resize-to ((obj clog-window) x y)
  (execute obj (format nil "resizeTo(~A,~A)" x y)))

;;;;;;;;;;;;;;;;;
;; open-window ;;
;;;;;;;;;;;;;;;;;

(defgeneric open-window (clog-window url &key name specs replace)
  (:documentation "This will launch a new window of current browser where
CLOG-WINDOW is displayed (remote or local). In modern browsers it is
very limitted to just open a new tab with url unless is a localhost url."))

(defmethod open-window ((obj clog-window) url &key
						(name "_blank")
						(specs "")
						(replace "false"))
  (execute obj (format nil "open('~A','~A','~A',~A)" url name specs replace)))

;;;;;;;;;;;;;;;;;;
;; close-window ;;
;;;;;;;;;;;;;;;;;;

(defgeneric close-window (clog-window)
  (:documentation "Close browser window."))

(defmethod close-window ((obj clog-window))
  (execute obj "close()"))

;;;;;;;;;;;;;;;;;;;;;;
;; close-connection ;;
;;;;;;;;;;;;;;;;;;;;;;

(defgeneric close-connection (clog-window)
  (:documentation "Close connection to browser with out closing browser."))

(defmethod close-connection ((obj clog-window))
  (clog-connection:cclose (connection-id obj)))

;;;;;;;;;;;;;;;;;;
;; set-on-abort ;;
;;;;;;;;;;;;;;;;;;

(defgeneric set-on-abort (clog-window on-abort-handler)
  (:documentation "Set the ON-ABORT-HANDLER for CLOG-OBJ. If ON-ABORT-HANDLER
is nil unbind the event."))

(defmethod set-on-abort ((obj clog-window) handler)
  (set-on-event obj "abort" handler))

;;;;;;;;;;;;;;;;;;
;; set-on-error ;;
;;;;;;;;;;;;;;;;;;

(defgeneric set-on-error (clog-window on-error-handler)
  (:documentation "Set the ON-ERROR-HANDLER for CLOG-OBJ. If ON-ERROR-HANDLER
is nil unbind the event."))

(defmethod set-on-error ((obj clog-window) handler)
  (set-on-event obj "error" handler))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set-on-before-unload ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-before-unload (clog-window on-before-unload-handler)
  (:documentation "Set the ON-BEFORE-UNLOAD-HANDLER for CLOG-WINDOW.
Return and empty string in order to prevent navigation off page.
If ON-BEFORE-UNLOAD-HANDLER is nil unbind the event."))

(defmethod set-on-before-unload ((obj clog-window) handler)
  (set-on-event obj "beforeunload" handler))

;;;;;;;;;;;;;;;;;;;;;;;;
;; set-on-hash-change ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-hash-change (clog-window on-hash-change-handler)
  (:documentation "Set the ON-HASH-CHANGE-HANDLER for CLOG-OBJ. If
ON-HASH-CHANGE-HANDLER is nil unbind the event."))

(defmethod set-on-hash-change ((obj clog-window) handler)
  (set-on-event obj "hashchange" handler))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set-on-orientation-change ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-orientation-change (clog-window
				       on-orientation-change-handler)
  (:documentation "Set the ON-ORIENTATION-CHANGE-HANDLER for CLOG-OBJ.
If ON-ORIENTATION-CHANGE-HANDLER is nil unbind the event."))

(defmethod set-on-orientation-change ((obj clog-window) handler)
  (set-on-event obj "orientationchange" handler))

;;;;;;;;;;;;;;;;;;;;
;; set-on-storage ;;
;;;;;;;;;;;;;;;;;;;;

(defparameter storage-event-script
  "+ encodeURIComponent(e.originalEvent.key) + ':' +
     encodeURIComponent(e.originalEvent.oldValue) + ':' +
     encodeURIComponent(e.originalEvent.newValue) + ':'")

(defun parse-storage-event (data)
  (let ((f (ppcre:split ":" data)))
    (list
     :event-type :storage
     :key        (quri:url-decode (or (nth 0 f) ""))
     :old-value  (quri:url-decode (or (nth 1 f) ""))
     :value      (quri:url-decode (or (nth 2 f) "")))))

(defgeneric set-on-storage (clog-window on-storage-handler)
  (:documentation "Set the ON-STORAGE-HANDLER for CLOG-OBJ. The
on-storage event is fired for changes to :local storage keys."))

(defmethod set-on-storage ((obj clog-window) handler)
  (set-event obj "storage"
	     (when handler
	       (lambda (data)
		 (funcall handler obj (parse-storage-event data))))
	     :call-back-script storage-event-script))

;;;;;;;;;;;;;;;;;;;;
;; storage-length ;;
;;;;;;;;;;;;;;;;;;;;

(deftype storage-type () '(member local session))

(defgeneric storage-length (clog-window storage-type)
  (:documentation "Number of entries in browser STORAGE-TYPE.
(local = persistant or session)"))

(defmethod storage-length ((obj clog-window) storage-type)
  (parse-integer (query obj (format nil "~(~a~)Storage.length" storage-type))))

;;;;;;;;;;;;;;;;;
;; storage-key ;;
;;;;;;;;;;;;;;;;;

(defgeneric storage-key (clog-window storage-type key-num)
  (:documentation "Return the key for entry number KEY-NUM in browser
STORAGE-TYPE. (local = persistant or session)"))

(defmethod storage-key ((obj clog-window) storage-type key-num)
  (query obj (format nil "~(~a~)Storage.key(~A)" storage-type key-num)))

;;;;;;;;;;;;;;;;;;;;
;; storage-remove ;;
;;;;;;;;;;;;;;;;;;;;

(defgeneric storage-remove (clog-window storage-type key-name)
  (:documentation "Remove the storage key and value in browser
STORAGE-TYPE. (local = persistant or session)"))

(defmethod storage-remove ((obj clog-window) storage-type key-name)
  (execute obj (format nil "~(~a~)Storage.removeItem(~A)" storage-type key-name)))

;;;;;;;;;;;;;;;;;;;;;
;; storage-element ;;
;;;;;;;;;;;;;;;;;;;;;

(defgeneric storage-element (clog-window storage-type key-name)
  (:documentation "Get/Setf storage-element on browser client."))

(defmethod storage-element ((obj clog-window) storage-type key-name)
  (query obj (format nil "~(~a~)Storage.getItem('~A')"
		     storage-type
		     (escape-string key-name))))

(defgeneric set-storage-element (clog-window storage-type key-name value)
  (:documentation "Set storage-element."))

(defmethod set-storage-element ((obj clog-window) storage-type key-name value)
  (execute obj (format nil "~(~a~)Storage.setItem('~A','~A')"
		       storage-type
		       (escape-string key-name)
		       (escape-string value)))
  value)
(defsetf storage-element set-storage-element)
