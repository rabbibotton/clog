;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG - The Common Lisp Omnificent GUI                                 ;;;;
;;;; (c) David Botton                                                      ;;;;
;;;;                                                                       ;;;;
;;;; clog-window.lisp                                                      ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:in-package :clog)

;; clog-window and clop-popup a clog based system to support child windows
;; even without browser support or premissions.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-window
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-window (clog-obj)()
  (:documentation "CLOG Window Objects encapsulate the window."))

;;;;;;;;;;;;;;;;;;;;;;
;; make-clog-window ;;
;;;;;;;;;;;;;;;;;;;;;;

(defun make-clog-window (connection-id &key html-id)
  "Construct a new clog-window. (Private)"
  (make-instance 'clog-window :connection-id connection-id
                              :html-id (if html-id
                                           html-id
                                           "window")))

;;;;;;;;;;;;;;;;;
;; window-name ;;
;;;;;;;;;;;;;;;;;

(defgeneric window-name (clog-window)
  (:documentation "Get/Setf name for use by hyperlink \"target\" for this
window."))

(defmethod window-name ((obj clog-window))
  (query obj "name"))

(defgeneric (setf window-name) (value clog-window))

(defmethod (setf window-name) (value (obj clog-window))
  (execute obj (format nil "name='~A'" (escape-string value)))
  value)

;;;;;;;;;;;;;;;;;
;; url-rewrite ;;
;;;;;;;;;;;;;;;;;

(defgeneric url-rewrite (clog-window rewrite-url)
  (:documentation "Rewrite browser history and url with REWRITE-URL
no redirection of browser takes place. REWRITE-URL must be same domain."))

(defmethod url-rewrite ((obj clog-window) rewrite-url)
  (execute obj (format nil "history.replaceState({},'','~A')" rewrite-url)))

;;;;;;;;;;;;;;;;
;; status-bar ;;
;;;;;;;;;;;;;;;;

(defgeneric status-bar (clog-window)
  (:documentation "Get/Setf status bar text."))

(defmethod status-bar ((obj clog-window))
  (query obj "status"))

(defgeneric (setf status-bar) (value clog-window))

(defmethod (setf status-bar) (value (obj clog-window))
  (execute obj (format nil "status='~A'" (escape-string value)))
  value)

;;;;;;;;;;;;;;;;;;
;; inner-height ;;
;;;;;;;;;;;;;;;;;;

(defgeneric inner-height (clog-window)
  (:documentation "Get/Setf inner height of browser window."))

(defmethod inner-height ((obj clog-window))
  (js-to-integer (query obj "innerHeight")))

(defgeneric (setf inner-height) (value clog-window))

(defmethod (setf inner-height) (value (obj clog-window))
  (execute obj (format nil "innerHeight='~A'" (escape-string value)))
  value)

;;;;;;;;;;;;;;;;;
;; inner-width ;;
;;;;;;;;;;;;;;;;;

(defgeneric inner-width (clog-window)
  (:documentation "Get/Setf inner width of browser window."))

(defmethod inner-width ((obj clog-window))
  (js-to-integer (query obj "innerWidth")))

(defgeneric (setf inner-width) (value clog-window))

(defmethod (setf inner-width) (value (obj clog-window))
  (execute obj (format nil "innerWidth='~A'" (escape-string value)))
  value)

;;;;;;;;;;;;;;;;;;
;; outer-height ;;
;;;;;;;;;;;;;;;;;;

(defgeneric outer-height (clog-window)
  (:documentation "Get/Setf outer height of browser window."))

(defmethod outer-height ((obj clog-window))
  (js-to-integer (query obj "outerHeight")))

(defgeneric (setf outer-height) (value clog-window))

(defmethod (setf outer-height) (value (obj clog-window))
  (execute obj (format nil "outerHeight='~A'" (escape-string value)))
  value)

;;;;;;;;;;;;;;;;;
;; outer-width ;;
;;;;;;;;;;;;;;;;;

(defgeneric outer-width (clog-window)
  (:documentation "Get/Setf outer width of browser window."))

(defmethod outer-width ((obj clog-window))
  (js-to-integer (query obj "outerWidth")))

(defgeneric (setf outer-width) (value clog-window))

(defmethod (setf outer-width) (value (obj clog-window))
  (execute obj (format nil "outerWidth='~A'" (escape-string value)))
  value)

;;;;;;;;;;;;;;
;; x-offset ;;
;;;;;;;;;;;;;;

(defgeneric x-offset (clog-window)
  (:documentation "Get/Setf browser window x offset from left edge."))

(defmethod x-offset ((obj clog-window))
  (js-to-integer (query obj "pageXOffset")))

(defgeneric (setf x-offset) (value clog-window))

(defmethod (setf x-offset) (value (obj clog-window))
  (execute obj (format nil "pageXOffset='~A'" (escape-string value)))
  value)

;;;;;;;;;;;;;;
;; y-offset ;;
;;;;;;;;;;;;;;

(defgeneric y-offset (clog-window)
  (:documentation "Get/Setf browser window y offset from top edge."))

(defmethod y-offset ((obj clog-window))
  (js-to-integer (query obj "pageYOffset")))

(defgeneric (setf y-offset) (value clog-window))

(defmethod (setf y-offset) (value (obj clog-window))
  (execute obj (format nil "pageYOffset='~A'" (escape-string value)))
  value)

;;;;;;;;;
;; top ;;
;;;;;;;;;

(defgeneric top (clog-window)
  (:documentation "Get/Setf browser y postion."))

(defmethod top ((obj clog-window))
  (js-to-integer (query obj "screenY")))

(defgeneric (setf top) (value clog-window))

(defmethod (setf top) (value (obj clog-window))
  (execute obj (format nil "screenY='~A'" (escape-string value)))
  value)

;;;;;;;;;;
;; left ;;
;;;;;;;;;;

(defgeneric left (clog-window)
  (:documentation "Get/Setf browser x position."))

(defmethod left ((obj clog-window))
  (js-to-integer (query obj "screenX")))

(defgeneric (setf left) (value clog-window))

(defmethod (setf left) (value (obj clog-window))
  (execute obj (format nil "screenX='~A'" (escape-string value)))
  value)

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
  (js-to-integer (query obj "screen.width")))

;;;;;;;;;;;;;;;;;;;
;; screen-height ;;
;;;;;;;;;;;;;;;;;;;

(defgeneric screen-height (clog-window)
  (:documentation "Get screen height."))

(defmethod screen-height ((obj clog-window))
  (js-to-integer (query obj "screen.height")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; screen-available-width ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric screen-available-width (clog-window)
  (:documentation "Get available screen width."))

(defmethod screen-available-width ((obj clog-window))
  (js-to-integer (query obj "screen.availWidth")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; screen-available-height ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric screen-available-height (clog-window)
  (:documentation "Get available screen height."))

(defmethod screen-available-height ((obj clog-window))
  (js-to-integer (query obj "screen.availHeight")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; screen-available-left ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric screen-available-left (clog-window)
  (:documentation "Get available screen left."))

(defmethod screen-available-left ((obj clog-window))
  (js-to-integer (query obj "screen.availLeft")))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; screen-available-top ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric screen-available-top (clog-window)
  (:documentation "Get available screen top."))

(defmethod screen-available-top ((obj clog-window))
  (js-to-integer (query obj "screen.availTop")))

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

(defgeneric open-window (clog-window url &key name specs)
  (:documentation "This will launch a new window of current browser where
CLOG-WINDOW is displayed (remote or local) and returns a new clog-window.
In modern browsers it is very limitted to just open a new tab with url
unless is a localhost url."))

(defmethod open-window ((obj clog-window) url &key
                                                (name "_blank")
                                                (specs ""))
  (let ((new-id (format nil "CLOG~A" (generate-id))))
    (execute obj (format nil "clog['~A']=open('~A','~A','~A')"
                         new-id url name specs))
    (make-clog-window (connection-id obj) :html-id new-id)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; request-animation-frame ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric request-animation-frame (clog-window)
  (:documentation "Requests the browser to send an on-animation-frame
on the next screen redraw. This event only fires one time per request.
The data parementer of the event function contains the time stamp
to the millisecond."))

(defmethod request-animation-frame ((obj clog-window))
  (execute obj (format nil "requestAnimationFrame(function (s)
                             {~A.trigger('clog-animate', s)})"
                       (jquery obj))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set-on-animation-frame ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-animation-frame (clog-window on-animation-frame-handler)
  (:documentation "Set the ON-ANIMATION-FRAME-HANDLER for CLOG-OBJ the data
parameter of the function is the time stamp. If
ON-ANIMATION-FRAME-HANDLER is nil unbind the event."))

(defmethod set-on-animation-frame ((obj clog-window) handler)
  (set-on-event-with-data obj "clog-animate" handler))

;;;;;;;;;;;;;;;;;;;;;;
;; set-on-pop-state ;;
;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-pop-state (clog-window on-pop-state-handler)
  (:documentation "Set the ON-POP-STATE-HANDLER for CLOG-WINDOW. If ON-POP-STATE-HANDLER
is nil unbind the event."))

(defmethod set-on-pop-state ((obj clog-window) handler)
  (set-on-event obj "popstate" handler))

;;;;;;;;;;;;;;;;;;;;
;; url-push-state ;;
;;;;;;;;;;;;;;;;;;;;

(defgeneric url-push-state (clog-window rewrite-url)
  (:documentation "Method adds an entry to the browser's session history stack."))

(defmethod url-push-state ((obj clog-window) rewrite-url)
  (execute obj (format nil "history.pushState({},'','~A')" rewrite-url)))

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
  (js-to-integer (query obj (format nil "~(~a~)Storage.length" storage-type))))

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
  (execute obj (format nil "~(~a~)Storage.removeItem('~A')" storage-type key-name)))

;;;;;;;;;;;;;;;;;;;;;
;; storage-element ;;
;;;;;;;;;;;;;;;;;;;;;

(defgeneric storage-element (clog-window storage-type key-name)
  (:documentation "Get/Setf storage-element on browser client."))

(defmethod storage-element ((obj clog-window) storage-type key-name)
  (query obj (format nil "~(~a~)Storage.getItem('~A')"
                     storage-type
                     (escape-string key-name))))

(defgeneric (setf storage-element) (value clog-window storage-type key-name)
  (:documentation "Set storage-element."))

(defmethod (setf storage-element) (value (obj clog-window) storage-type key-name)
  (execute obj (format nil "~(~a~)Storage.setItem('~A','~A')"
                       storage-type
                       (escape-string key-name)
                       (escape-string value)))
  value)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog popup windows
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *clog-popup-sync-hash* (make-hash-table :test 'equalp)
  "Used for syncing clog popup window creation with the thread creating
them.")

(defparameter *clog-popup-path* "/clogwin"
  "Default URL for popup windows")

(defun clog-popup-handler (body)
  "Handle the connection of a new popup clog window (internal)"
  (let ((sync (form-data-item (form-get-data body) "sync")))
    (cond (sync
           (clog-popup-openned body sync))
          (t
           (create-div body :content "Invalid Access")))))

;;;;;;;;;;;;;;;;;;;;;;;
;; enable-clog-popup ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defun enable-clog-popup (&key (path *clog-popup-path*) (boot-file "/boot.html"))
  "Enable handling of clog enabled popups"
  (set-on-new-window 'clog-popup-handler :path path :boot-file boot-file))

;;;;;;;;;;;;;;;;;;;;;
;; open-clog-popup ;;
;;;;;;;;;;;;;;;;;;;;;

(defgeneric open-clog-popup (clog-obj &key path
                                        add-sync-to-path
                                        sync-key
                                        name
                                        specs
                                        wait-timeout)
  (:documentation "Open a new browser window/popup in most cases a tab.
Since they are controlled by clog you have full control of the new popups
and are more flexible than using open-windo. Returns the clog-body and the
clog-window in the same connnection as obj of the new window on the new
connection or nil if failed within :WAIT-TIMEOUT"))

(defmethod open-clog-popup ((obj clog-obj) &key (path *clog-popup-path*)
                                             (add-sync-to-path t)
                                             (sync-key (random-hex-string))
                                             (name "_blank")
                                             (specs "")
                                             (wait-timeout 10))
  (let* ((sem     (bordeaux-threads:make-semaphore))
         (mpath   (if add-sync-to-path
                      (format nil "~A?sync=~A" path sync-key)
                      path))
         (new-win (open-window (window (connection-body obj)) mpath :specs specs :name name)))
    (setf (gethash sync-key *clog-popup-sync-hash*) sem)
    (bordeaux-threads:wait-on-semaphore sem :timeout wait-timeout)
    (setf sem (gethash sync-key *clog-popup-sync-hash*))
    (remhash sync-key *clog-popup-sync-hash*)
    (if (typep sem 'clog-obj)
        (progn
          (setf (connection-data-item sem "clog-popup") new-win)
          (values sem new-win))
        nil)))

;;;;;;;;;;;;;;;;;;;;;;;;
;; clog-popup-openned ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric clog-popup-openned (clog-obj sync-key)
  (:documentation "Used to notify open-clog-popup the new popup window
is ready used for custom clog-popup handlers."))

(defmethod clog-popup-openned ((obj clog-obj) sync-key)
  (let ((sem (gethash sync-key *clog-popup-sync-hash*)))
    (cond (sem
           (setf (gethash sync-key *clog-popup-sync-hash*) (connection-body obj))
           (bordeaux-threads:signal-semaphore sem))
          (t
           (create-div obj :content "Invalid Sync")))))

;;;;;;;;;;;;;;;;;;;;;
;; in-clog-popup-p ;;
;;;;;;;;;;;;;;;;;;;;;

(defgeneric in-clog-popup-p (clog-obj)
  (:documentation "Returns obj if clog-gui-window is a in a clog-popup window"))
  
(defmethod in-clog-popup-p ((obj clog-obj))
  (when (connection-data-item obj "clog-popup")
    obj))

