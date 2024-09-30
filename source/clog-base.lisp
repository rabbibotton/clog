;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG - The Common Lisp Omnificent GUI                                 ;;;;
;;;; (c) David Botton                                                      ;;;;
;;;;                                                                       ;;;;
;;;; clog-base.lisp                                                        ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:in-package :clog)

;;; Implements the base object clog-obj that encapsulates the communication
;;; between the browser and the lisp code for each object in the HTML DOM.
;;; This includes properties, methods and events. Each clog-obj also has
;;; methods to retrieve connection-data (data that is associated with the
;;; current page regardless of object or thread of execution is lisp).

(pushnew :clog *features*)

(defvar *connection-cache* nil
"Dynamic variable containing optional cache. Every thread has its
own context and therefore its own copy of this variable when
dynamically bound. As a result no thread protection is needed to
access. To use dynamically bind the *connection-cache* and set it
to (list :cache) turn on caching. By default this is off its main use
is during initial setup of complex pages. (private)
See macro with-connection-cache.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-obj
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-obj ()
  ((connection-id
     :reader connection-id
     :initarg :connection-id)
   (html-id
     :reader html-id
     :initarg :html-id)
   (parent
     :accessor parent
     :initform nil))
  (:documentation "CLOG objects (clog-obj) encapsulate the connection between
lisp and an HTML DOM element."))

;;;;;;;;;;;;;;;;;;
;; print-object ;;
;;;;;;;;;;;;;;;;;;

(defmethod print-object ((obj clog-obj) stream)
      (print-unreadable-object (obj stream :type t)
        (with-slots (connection-id html-id) obj
          (format stream "connection-id: ~a html-id: ~a" connection-id html-id))))

;;;;;;;;;;;;;;;;;;;
;; create-parent ;;
;;;;;;;;;;;;;;;;;;;

(defgeneric parent (clog-obj)
  (:documentation "Returns the clog-obj of the obj that was used as creation
parent if was set or nil. This is not per se the parent in the DOM."))

;;;;;;;;;;;;;;;;;;;
;; connection-id ;;
;;;;;;;;;;;;;;;;;;;

(defgeneric connection-id (clog-obj)
  (:documentation "Reader for connection-id slot. (Private)"))

;;;;;;;;;;;;;
;; html-id ;;
;;;;;;;;;;;;;

(defgeneric html-id (clog-obj)
  (:documentation "Internal html-id of clog-obj. (Internal)"))

;;;;;;;;;;;;;;;;;;;
;; make-clog-obj ;;
;;;;;;;;;;;;;;;;;;;

(defun make-clog-obj (connection-id html-id)
  "Construct a new clog-obj. (Private)"
  (make-instance 'clog-obj :connection-id connection-id :html-id html-id))

;;;;;;;;;;;;;;;
;; script-id ;;
;;;;;;;;;;;;;;;

(defgeneric script-id (clog-obj)
  (:documentation "Return the script id for CLOG-OBJ based on the html-id set
during attachment. (Private)"))

(defmethod script-id ((obj clog-obj))
  (if (eql (html-id obj) 0)
      "'body'"
      (format nil "clog['~A']" (html-id obj))))

;;;;;;;;;;;;;;;;;;;
;; cached-excute ;;
;;;;;;;;;;;;;;;;;;;

(defun cached-execute (connection-id script)
  "Excute SCRIPT on browser, if connection cache enabled use. (Private)"
  (if *connection-cache*
      (push script *connection-cache*)
      (clog-connection:execute connection-id script)))

;;;;;;;;;;;;;;;;
;; js-execute ;;
;;;;;;;;;;;;;;;;

(defgeneric js-execute (clog-obj script)
  (:documentation "Execute JavaScript SCRIPT on browser. CLOG-OBJ is used to
obtain the connection the script should run on. Result is discarded, return
CLOG-OBJ. (Internal)"))

(defmethod js-execute ((obj clog-obj) script)
  (cached-execute (connection-id obj) script)
  obj)

;;;;;;;;;;;;;;
;; js-query ;;
;;;;;;;;;;;;;;

(defgeneric js-query (clog-obj script &key default-answer)
  (:documentation "Execute JavaScript SCRIPT on browser and return result.
CLOG-OBJ us used to obtain the connection the script should run on. (Internal)"))

(defmethod js-query ((obj clog-obj) script &key (default-answer nil))
  (flush-connection-cache obj)
  (clog-connection:query (connection-id obj) script
                         :default-answer default-answer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; with-connection-cache ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro with-connection-cache ((clog-obj) &body body)
  "Caches writes to the connection-id of CLOG-OBJ until
flushed with FLUSH-CONNECTION-CACHE or a query is made."
  `(let ((*connection-cache* (list :cache)))
     ,@body
     (clog:flush-connection-cache ,clog-obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; flush-connection-cache ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun flush-connection-cache (clog-obj)
  "Flush connection cache if on CLOG-OBJ is located on."
  (when *connection-cache*
    (dolist (script (reverse *connection-cache*))
      (unless (eq script :cache)
        (clog-connection:execute (connection-id clog-obj) script)))
    (setf *connection-cache* (list :cache))))

;;;;;;;;;;;;;
;; execute ;;
;;;;;;;;;;;;;

(defgeneric execute (clog-obj method)
  (:documentation "Execute the JavaScript METHOD on OBJ. Returns
CLOG-OBJ. see JQUERY-EXECUTE (Internal)"))

(defmethod execute ((obj clog-obj) method)
  (js-execute obj (format nil "~A.~A" (script-id obj) method)))

;;;;;;;;;;;
;; query ;;
;;;;;;;;;;;

(defgeneric query (clog-obj method &key default-answer)
  (:documentation "Execute the JavaScript query METHOD on OBJ and return
result or if time out DEFAULT-ANSWER. see JQUERY-QUERY (Internal)"))

(defmethod query ((obj clog-obj) method &key (default-answer nil))
  (js-query obj (format nil "~A.~A" (script-id obj) method)
            :default-answer default-answer))

;;;;;;;;;;;;;;;;;;;;;;;
;; bind-event-script ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric bind-event-script (clog-obj event call-back)
  (:documentation "Create the code client side for EVENT CALL-BACK.
(Private)"))

(defmethod bind-event-script ((obj clog-obj) event call-back)
  (jquery-execute obj (format nil "on('~A',function (e, data){~A})"
                              event call-back)))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; unbind-event-script ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric unbind-event-script (clog-obj event)
  (:documentation "Remove the client call back code for EVENT. (Private)"))

(defmethod unbind-event-script ((obj clog-obj) event)
  (jquery-execute obj (format nil "off('~A')" event)))

;;;;;;;;;;;;;;;;;;;;;;;
;; parse-mouse-event ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defparameter mouse-event-script
  "+ (e.clientX - e.currentTarget.getBoundingClientRect().left + e.currentTarget.scrollLeft) + ':' +
     (e.clientY - e.currentTarget.getBoundingClientRect().top + e.currentTarget.scrollTop) + ':' +
     e.screenX + ':' + e.screenY + ':' + e.which + ':' + e.altKey + ':' +
     e.ctrlKey + ':' + e.shiftKey + ':' + e.metaKey + ':' +
     e.clientX + ':' + e.clientY + ':' + e.pageX + ':' + e.pageY"
  "JavaScript to collect mouse event data from browser.")
;; e.buttons would be better but not supported currently outside
;; of firefox and would always return 0 on Mac so using e.which.
;; The use of offsetLeft and offsetTop is to correct the X and Y
;; to the actual X,Y of the target.

(defun parse-mouse-event (data)
  (let ((f (ppcre:split ":" data)))
    (list
      :event-type   :mouse
      :x            (js-to-integer (nth 0 f))
      :y            (js-to-integer (nth 1 f))
      :screen-x     (js-to-integer (nth 2 f))
      :screen-y     (js-to-integer (nth 3 f))
      :which-button (js-to-integer (nth 4 f))
      :alt-key      (js-true-p (nth 5 f))
      :ctrl-key     (js-true-p (nth 6 f))
      :shift-key    (js-true-p (nth 7 f))
      :meta-key     (js-true-p (nth 8 f))
      :client-x     (js-to-integer (nth 9 f))
      :client-Y     (js-to-integer (nth 10 f))
      :page-x       (js-to-integer (nth 11 f))
      :page-Y       (js-to-integer (nth 12 f)))))

;;;;;;;;;;;;;;;;;;;;;;;
;; parse-touch-event ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defparameter touch-event-script
  "+ (e.touches[0].clientX -
	e.touches[0].target.getBoundingClientRect().left +
	e.touches[0].target.scrollLeft) + ':' +
     (e.touches[0].clientY -
	e.touches[0].target.getBoundingClientRect().top +
	e.touches[0].target.scrollTop) + ':' +
     e.touches[0].screenX + ':' + e.touches[0].screenY + ':' + e.touches.length + ':' +
     e.altKey + ':' +
     e.ctrlKey + ':' +
     e.shiftKey + ':' +
     e.metaKey + ':' +
     e.touches[0].clientX + ':' + e.touches[0].clientY + ':' +
     e.touches[0].pageX + ':' + e.touches[0].pageY"
  "JavaScript to collect touch event data from browser.")

(defun parse-touch-event (data)
  (let ((f (ppcre:split ":" data)))
    (list
      :event-type     :touch
      :x              (js-to-integer (nth 0 f))
      :y              (js-to-integer (nth 1 f))
      :screen-x       (js-to-integer (nth 2 f))
      :screen-y       (js-to-integer (nth 3 f))
      :number-fingers (js-to-integer (nth 4 f))
      :alt-key        (js-true-p (nth 5 f))
      :ctrl-key       (js-true-p (nth 6 f))
      :shift-key      (js-true-p (nth 7 f))
      :meta-key       (js-true-p (nth 8 f))
      :client-x       (js-to-integer (nth 9 f))
      :client-Y       (js-to-integer (nth 10 f))
      :page-x         (js-to-integer (nth 11 f))
      :page-Y         (js-to-integer (nth 12 f)))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; parse-pointer-event ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter pointer-event-script
  "+ (e.clientX - e.currentTarget.getBoundingClientRect().left + e.currentTarget.scrollLeft) + ':' +
     (e.clientY - e.currentTarget.getBoundingClientRect().top + e.currentTarget.scrollTop) + ':' +
     e.screenX + ':' + e.screenY + ':' + e.which + ':' + e.altKey + ':' +
     e.ctrlKey + ':' + e.shiftKey + ':' + e.metaKey + ':' +
     e.clientX + ':' + e.clientY + ':' + e.pageX + ':' + e.pageY"
  "JavaScript to collect pointer event data from browser.")

(defun parse-pointer-event (data)
  (let ((f (ppcre:split ":" data)))
    (list
      :event-type   :pointer
      :x            (js-to-integer (nth 0 f))
      :y            (js-to-integer (nth 1 f))
      :screen-x     (js-to-integer (nth 2 f))
      :screen-y     (js-to-integer (nth 3 f))
      :which-button (js-to-integer (nth 4 f))
      :alt-key      (js-true-p (nth 5 f))
      :ctrl-key     (js-true-p (nth 6 f))
      :shift-key    (js-true-p (nth 7 f))
      :meta-key     (js-true-p (nth 8 f))
      :client-x     (js-to-integer (nth 9 f))
      :client-Y     (js-to-integer (nth 10 f))
      :page-x       (js-to-integer (nth 11 f))
      :page-Y       (js-to-integer (nth 12 f)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parse-keyboard-event ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter keyboard-event-script
  "+ e.key.charCodeAt(0) + ':' + e.charCode + ':' + e.altKey + ':' +
     e.ctrlKey + ':' + e.shiftKey + ':' + e.metaKey + ':' +
     (e.key == ':' ? 'colon' : e.key)"
  "JavaScript to collect keyboard event data from browser.")

(defun parse-keyboard-event (data)
  (let ((f (ppcre:split ":" data)))
    (list
      :event-type :keyboard
      :key-code   (js-to-integer (nth 0 f))
      :char-code  (js-to-integer (nth 1 f))
      :alt-key    (js-true-p (nth 2 f))
      :ctrl-key   (js-true-p (nth 3 f))
      :shift-key  (js-true-p (nth 4 f))
      :meta-key   (js-true-p (nth 5 f))
      :key        (if (equal (nth 6 f) "colon")
                      ":"
                      (nth 6 f)))))

;;;;;;;;;;;;;;;;;;;;;;
;; parse-drop-event ;;
;;;;;;;;;;;;;;;;;;;;;;

(defparameter drop-event-script
  "+ (e.clientX - e.currentTarget.getBoundingClientRect().left + e.currentTarget.scrollLeft) + ':' +
     (e.clientY - e.currentTarget.getBoundingClientRect().top + e.currentTarget.scrollTop) + ':' +
     e.which + ':' + e.altKey + ':' + e.ctrlKey + ':' + e.shiftKey + ':' + e.metaKey + ':' +
     encodeURIComponent(e.originalEvent.dataTransfer.getData('~A'))"
  "JavaScript to collect drop event data from browser.")

(defun parse-drop-event (data)
  (let ((f (ppcre:split ":" data)))
    (list
      :event-type   :drop
      :x            (js-to-integer (nth 0 f))
      :y            (js-to-integer (nth 1 f))
      :which-button (js-to-integer (nth 2 f))
      :alt-key      (js-true-p (nth 3 f))
      :ctrl-key     (js-true-p (nth 4 f))
      :shift-key    (js-true-p (nth 5 f))
      :meta-key     (js-true-p (nth 6 f))
      :drag-data    (quri:url-decode (or (nth 7 f) "")))))

;;;;;;;;;;;;;;;
;; set-event ;;
;;;;;;;;;;;;;;;

(defgeneric set-event (clog-obj event handler
                       &key call-back-script
                       pre-eval
                       eval-script
                       post-eval
                       cancel-event
                       one-time)
  (:documentation "Create the low-level hook for incoming events. (Private)"))

(defmethod set-event ((obj clog-obj) event handler
                      &key (call-back-script "")
                      (pre-eval "")
                      (eval-script "")
                      (post-eval "")
                      (cancel-event nil)
                      (one-time nil))
  (let ((hook (format nil "~A:~A" (html-id obj) event))
        (cd   (connection-data obj)))
    (if cd
        (cond (handler
                (bind-event-script
                  obj event (format nil "~A~Aws.send('E:~A '~A)~A~@[~A~]~@[~A~]"
                                    pre-eval
                                    eval-script
                                    hook
                                    call-back-script
                                    post-eval
                                    (when one-time
                                      (format nil "; ~A.off('~A')"
                                              (jquery obj)
                                              event))
                                    (when cancel-event "; return false")))
                (setf (gethash hook cd) handler))
              (t
                (unbind-event-script obj event)
                (remhash hook cd)))
        (format t "Attempt to set event on non-existant connection.~%"))))

;;;;;;;;;;;;;;
;; property ;;
;;;;;;;;;;;;;;

(defgeneric property (clog-obj property-name &key default-answer)
  (:documentation "Get/Setf html property."))

(defmethod property ((obj clog-obj) property-name &key (default-answer nil))
  (jquery-query obj (format nil "prop('~A')" property-name)
                :default-answer default-answer))

(defgeneric (setf property) (value clog-obj property-name)
  (:documentation "Set html property."))

(defmethod (setf property) (value (obj clog-obj) property-name)
  (jquery-execute obj (format nil "prop('~A','~A')"
                              property-name
                              (escape-string value)))
  value)

;;;;;;;;;;;;
;; height ;;
;;;;;;;;;;;;

(defgeneric height (clog-obj)
  (:documentation "Get/Setf html height in pixels."))

(defmethod height ((obj clog-obj))
  (js-to-integer (jquery-query obj "height()")))

(defgeneric (setf height) (value clog-obj)
  (:documentation "Set height VALUE for CLOG-OBJ"))

(defmethod (setf height) (value (obj clog-obj))
  (jquery-execute obj (format nil "height('~A')" (escape-string value)))
  value)

;;;;;;;;;;;
;; width ;;
;;;;;;;;;;;

(defgeneric width (clog-obj)
  (:documentation "Get/Setf html width in pixels."))

(defmethod width ((obj clog-obj))
  (js-to-integer (jquery-query obj "width()")))

(defgeneric (setf width) (value clog-obj)
  (:documentation "Set width VALUE for CLOG-OBJ"))

(defmethod (setf width) (value (obj clog-obj))
  (jquery-execute obj (format nil "width('~A')" (escape-string value)))
  value)

;;;;;;;;;;;
;; focus ;;
;;;;;;;;;;;

(defgeneric focus (clog-obj)
  (:documentation "Focus on CLOG-OBJ"))

(defmethod focus ((obj clog-obj))
  (jquery-execute obj "focus()"))

;;;;;;;;;;
;; blur ;;
;;;;;;;;;;

(defgeneric blur (clog-obj)
  (:documentation "Remove focus from CLOG-OBJ"))

(defmethod blur ((obj clog-obj))
  (jquery-execute obj "blur()"))

;;;;;;;;;;;;
;; validp ;;
;;;;;;;;;;;;

(defgeneric validp (clog-obj)
  (:documentation "Returns true if connection is valid on this CLOG-OBJ."))

(defmethod validp ((obj clog-obj))
  (clog-connection:validp (connection-id obj)))

;;;;;;;;;;;;;;;;;;;;;
;; connection-data ;;
;;;;;;;;;;;;;;;;;;;;;

(defgeneric connection-data (clog-obj)
  (:documentation "Get connection-data that is associated with
clog-obj's connection that will persist regardless of thread calling.
The event hooks are stored in this string based hash in the format of:
\"html-id:event-name\" => #'event-handler. clog-* keys are reserved
for internal use of clog.

The following default keys are set:
\"clog-body\"      clog-body of this connection, see CONNECTION-BODY
\"clog-path\"      html path used, see CONNECTION-PATH
\"clog-sync\"      mutex used for syncing events, see CONNECTION-SYNC"))

(defmethod connection-data ((obj clog-obj))
  (clog-connection:get-connection-data (connection-id obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; connection-data-item ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric connection-data-item (clog-obj item-name)
  (:documentation "Get/Setf item-name from connection-data."))

(defmethod connection-data-item ((obj clog-obj) item-name)
  (ignore-errors
    (gethash item-name (connection-data obj))))

(defgeneric (setf connection-data-item) (value clog-obj item-name)
  (:documentation "Set connection-data item-name with value."))

(defmethod (setf connection-data-item) (value (obj clog-obj) item-name)
  (ignore-errors
    (setf (gethash item-name (connection-data obj)) value))
  value)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; remove-connection-data-item ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric remove-connection-data-item (clog-obj item-name)
  (:documentation "Remove item-name from connection-data."))

(defmethod remove-connection-data-item ((obj clog-obj) item-name)
  (ignore-errors
    (remhash item-name (connection-data obj))))

;;;;;;;;;;;;;;;;;;;;;
;; connection-body ;;
;;;;;;;;;;;;;;;;;;;;;

(defgeneric connection-body (clog-obj)
  (:documentation "Get connection's clog-body."))

(defmethod connection-body (clog-obj)
  (connection-data-item clog-obj "clog-body"))

;;;;;;;;;;;;;;;;;;;;;
;; connection-path ;;
;;;;;;;;;;;;;;;;;;;;;

(defgeneric connection-path (clog-obj)
  (:documentation "Get the HTML passed used to make the connection."))

(defmethod connection-path (clog-obj)
  (connection-data-item clog-obj "clog-path"))

;;;;;;;;;;;;;;;;;;;;;
;; connection-sync ;;
;;;;;;;;;;;;;;;;;;;;;

(defgeneric connection-sync (clog-obj)
  (:documentation "Get connection's clog-sync for optional syncing events."))

(defmethod connection-sync (clog-obj)
  (connection-data-item clog-obj "clog-sync"))

;;;;;;;;;;;;;;;;;;;;;
;; with-sync-event ;;
;;;;;;;;;;;;;;;;;;;;;

(defmacro with-sync-event ((clog-obj) &body body)
  "Place at start of event to serialize access to the event. All events in
an application share per connection the same queue of serialized events."
  `(bordeaux-threads:with-lock-held (,`(connection-sync ,clog-obj))
                                    ,@body))

;;;;;;;;;;;;;;;;;;
;; set-on-event ;;
;;;;;;;;;;;;;;;;;;

(defgeneric set-on-event (clog-obj event-name handler
                          &key cancel-event one-time)
  (:documentation "Set a HANDLER for EVENT-NAME on CLOG-OBJ. If handler is
nil unbind all event handlers. (Internal)"))

(defmethod set-on-event ((obj clog-obj) event-name handler
                         &key
                         (cancel-event nil)
                         (one-time nil))
  (set-event obj event-name
             (when handler
               (lambda (data)
                 (declare (ignore data))
                 (funcall handler obj)))
             :cancel-event cancel-event
             :one-time     one-time))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set-on-event-with-data ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-event-with-data (clog-obj event-name handler
                                    &key cancel-event one-time)
  (:documentation "Set a HANDLER for EVENT-NAME on CLOG-OBJ.
 If handler is nil unbind all event handlers. Handler is called with a data
option passed from javascript calling the jQuery custom event mechanism
.trigger('event_name', data) (Internal)"))

(defmethod set-on-event-with-data ((obj clog-obj) event-name handler
                                   &key
                                   (cancel-event nil)
                                   (one-time     nil))
  (set-event obj event-name
             (when handler
               (lambda (data)
                 (funcall handler obj data)))
             :call-back-script "+data"
             :cancel-event cancel-event
             :one-time     one-time))

;;;;;;;;;;;;;;;;;;;
;; set-on-resize ;;
;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-resize (clog-obj on-resize-handler)
  (:documentation "Set the ON-RESIZE-HANDLER for CLOG-OBJ. If ON-RESIZE-HANDLER
is nil unbind the event. In most modern browser this only works on the clog-window
object, unless a ResizeObserver is put in place, so one is installed except for
clog-window making this event functional."))

(defmethod set-on-resize ((obj clog-obj) handler)
  (set-on-event obj "resize" handler :cancel-event t)
  (unless (typep obj 'clog-window)
    (js-execute obj (format nil "new ResizeObserver(() => {
                                  ~A.trigger('resize');
                                 }).observe(~A)"
                            (jquery obj)
                            (script-id obj)))))

;;;;;;;;;;;;;;;;;;
;; set-on-focus ;;
;;;;;;;;;;;;;;;;;;

(defgeneric set-on-focus (clog-obj on-focus-handler)
  (:documentation "Set the ON-FOCUS-HANDLER for CLOG-OBJ. If ON-FOCUS-HANDLER
is nil unbind the event."))

(defmethod set-on-focus ((obj clog-obj) handler)
  (set-on-event obj "focus" handler))

;;;;;;;;;;;;;;;;;
;; set-on-blur ;;
;;;;;;;;;;;;;;;;;

(defgeneric set-on-blur (clog-obj on-blur-handler)
  (:documentation "Set the ON-BLUR-HANDLER for CLOG-OBJ. If ON-BLUR-HANDLER
is nil unbind the event."))

(defmethod set-on-blur ((obj clog-obj) handler)
  (set-on-event obj "blur" handler))

;;;;;;;;;;;;;;;;;;;
;; set-on-change ;;
;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-change (clog-obj on-change-handler)
  (:documentation "Set the ON-CHANGE-HANDLER for CLOG-OBJ. If ON-CHANGE-HANDLER
is nil unbind the event."))

(defmethod set-on-change ((obj clog-obj) handler)
  (set-on-event obj "change" handler))

;;;;;;;;;;;;;;;;;;
;; set-on-input ;;
;;;;;;;;;;;;;;;;;;

(defgeneric set-on-input (clog-obj on-input-handler)
  (:documentation "Set the ON-INPUT-HANDLER for CLOG-OBJ. If ON-INPUT-HANDLER
is nil unbind the event."))

(defmethod set-on-input ((obj clog-obj) handler)
  (set-on-event obj "input" handler))

;;;;;;;;;;;;;;;;;;;;;;;
;; set-on-drag-start ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-drag-start (clog-obj on-drag-start-handler
                               &key drag-data drag-type)
  (:documentation "Set the ON-DRAG-START-HANDLER for CLOG-OBJ.
If ON-DRAG-START-HANDLER is nil unbind the event."))

(defmethod set-on-drag-start ((obj clog-obj) handler
                              &key (drag-data "") (drag-type "text/plain"))
  (set-event obj "dragstart"
             (when handler
               (lambda (data)
                 (declare (ignore data))
                 (funcall handler obj)))
             :eval-script (format nil
                                  "e.originalEvent.dataTransfer.setData('~A','~A'); "
                                  drag-type
                                  drag-data)))

;;;;;;;;;;;;;;;;;
;; set-on-drag ;;
;;;;;;;;;;;;;;;;;

(defgeneric set-on-drag (clog-obj on-drag-handler)
  (:documentation "Set the ON-DRAG-HANDLER for CLOG-OBJ. If ON-DRAG-HANDLER
is nil unbind the event."))

(defmethod set-on-drag ((obj clog-obj) handler)
  (set-on-event obj "drag" handler))

;;;;;;;;;;;;;;;;;;;;;
;; set-on-drag-end ;;
;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-drag-end (clog-obj on-drag-end-handler)
  (:documentation "Set the ON-DRAG-END-HANDLER for CLOG-OBJ.
 If ON-DRAG-END-HANDLER is nil unbind the event."))

(defmethod set-on-drag-end ((obj clog-obj) handler)
  (set-on-event obj "dragend" handler))

;;;;;;;;;;;;;;;;;;;;;;;
;; set-on-drag-enter ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-drag-enter (clog-obj on-drag-enter-handler)
  (:documentation "Set the ON-DRAG-ENTER-HANDLER for CLOG-OBJ.
 If ON-DRAG-ENTER-HANDLER is nil unbind the event."))

(defmethod set-on-drag-enter ((obj clog-obj) handler)
  (set-on-event obj "dragenter" handler))

;;;;;;;;;;;;;;;;;;;;;;;
;; set-on-drag-leave ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-drag-leave (clog-obj on-drag-leave-handler)
  (:documentation "Set the ON-DRAG-LEAVE-HANDLER for CLOG-OBJ.
 If ON-DRAG-LEAVE-HANDLER is nil unbind the event."))

(defmethod set-on-drag-leave ((obj clog-obj) handler)
  (set-on-event obj "dragleave" handler))

;;;;;;;;;;;;;;;;;;;;;;
;; set-on-drag-over ;;
;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-drag-over (clog-obj on-drag-over-handler)
  (:documentation "Set the ON-DRAG-OVER-HANDLER for CLOG-OBJ.
 If ON-DRAG-OVER-HANDLER is nil unbind the event."))

(defmethod set-on-drag-over ((obj clog-obj) handler)
  (set-event obj "dragover"
             (when handler
               (lambda (data)
                 (declare (ignore data))
                 (funcall handler obj)))
             :cancel-event t
             :eval-script "e.preventDefault(); "))

;;;;;;;;;;;;;;;;;
;; set-on-drop ;;
;;;;;;;;;;;;;;;;;

(defgeneric set-on-drop (clog-obj on-drop-handler &key drag-type)
  (:documentation "Set the ON-DROP-HANDLER for CLOG-OBJ. If ON-DROP-HANDLER
is nil unbind the event."))

(defmethod set-on-drop ((obj clog-obj) handler &key (drag-type "text/plain"))
  (set-event obj "drop"
             (when handler
               (lambda (data)
                 (funcall handler obj (parse-drop-event data))))
             :call-back-script (format nil drop-event-script drag-type)
             :eval-script "e.preventDefault(); "
             :cancel-event t))

;;;;;;;;;;;;;;;;;;;;;
;; set-on-focus-in ;;
;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-focus-in (clog-obj on-focus-in-handler)
  (:documentation "Set the ON-FOCUS-IN-HANDLER for CLOG-OBJ. If
ON-FOCUS-IN-HANDLER is nil unbind the event."))

(defmethod set-on-focus-in ((obj clog-obj) handler)
  (set-on-event obj "focusin" handler))

;;;;;;;;;;;;;;;;;;;;;;
;; set-on-focus-out ;;
;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-focus-out (clog-obj on-focus-out-handler)
  (:documentation "Set the ON-FOCUS-OUT-HANDLER for CLOG-OBJ.
If ON-FOCUS-OUT-HANDLER is nil unbind the event."))

(defmethod set-on-focus-out ((obj clog-obj) handler)
  (set-on-event obj "focusout" handler))

;;;;;;;;;;;;;;;;;;
;; set-on-reset ;;
;;;;;;;;;;;;;;;;;;

(defgeneric set-on-reset (clog-obj on-reset-handler)
  (:documentation "Set the ON-RESET-HANDLER for CLOG-OBJ. If ON-RESET-HANDLER
is nil unbind the event. This event is activated by using reset on a form. If
this event is bound, you must call the form reset manually."))

(defmethod set-on-reset ((obj clog-obj) handler)
  (set-event obj "reset"
             (when handler
               (lambda (data)
                 (declare (ignore data))
                 (funcall handler obj)))
             :cancel-event t))

;;;;;;;;;;;;;;;;;;;
;; set-on-search ;;
;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-search (clog-obj on-search-handler)
  (:documentation "Set the ON-SEARCH-HANDLER for CLOG-OBJ. If ON-SEARCH-HANDLER
is nil unbind the event."))

(defmethod set-on-search ((obj clog-obj) handler)
  (set-on-event obj "search" handler))

;;;;;;;;;;;;;;;;;;;
;; set-on-select ;;
;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-select (clog-obj on-select-handler)
  (:documentation "Set the ON-SELECT-HANDLER for CLOG-OBJ. If ON-SELECT-HANDLER
is nil unbind the event."))

(defmethod set-on-select ((obj clog-obj) handler)
  (set-on-event obj "select" handler))

;;;;;;;;;;;;;;;;;;;
;; set-on-submit ;;
;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-submit (clog-obj on-submit-handler)
  (:documentation "Set the ON-SUBMIT-HANDLER for CLOG-OBJ. If ON-SUBMIT-HANDLER
is nil unbind the event. This event is activated by using submit on a form. If
this event is bound, you must call the (submit clog-form) manually if wish the
form action to be run. See CLOG-Form SUBMIT for more details."))

(defmethod set-on-submit ((obj clog-obj) handler)
  (set-event obj "submit"
             (when handler
               (lambda (data)
                 (declare (ignore data))
                 (funcall handler obj)))
             :cancel-event t))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; set-on-context-menu ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-context-menu (clog-obj on-context-menu-handler &key one-time)
  (:documentation "Set the ON-CONTEXT-MENU-HANDLER for CLOG-OBJ. If
ON-CONTEXT-MENU-HANDLER is nil unbind the event. Setting
on-mouse-right-click will replace this handler. If :ONE-TIME unbind
event on right click."))

(defmethod set-on-context-menu ((obj clog-obj) handler &key one-time)
  (set-event obj "contextmenu"
             (when handler
               (lambda (data)
                 (declare (ignore data))
                 (funcall handler obj)))
             :one-time one-time
             :cancel-event t))

;;;;;;;;;;;;;;;;;;
;; set-on-click ;;
;;;;;;;;;;;;;;;;;;

(defgeneric set-on-click (clog-obj on-click-handler &key one-time cancel-event)
  (:documentation "Set the ON-CLICK-HANDLER for CLOG-OBJ. If ON-CLICK-HANDLER
is nil unbind the event. Setting this event will replace an on-mouse click if
set. If :ONE-TIME unbind event on click."))

(defmethod set-on-click ((obj clog-obj) handler &key one-time cancel-event)
  (set-event obj "click"
             (when handler
               (lambda (data)
                 (declare (ignore data))
                 (funcall handler obj)))
             :one-time one-time
             :cancel-event cancel-event))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; set-on-double-click ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-double-click (clog-obj on-double-click-handler
                                 &key one-time cancel-event)
  (:documentation "Set the ON-DOUBLE-CLICK-HANDLER for CLOG-OBJ. If
ON-DOUBLE-CLICK-HANDLER is nil unbind the event. Setting the
on-mouse-double-click event will replace this handler."))

(defmethod set-on-double-click ((obj clog-obj) handler &key one-time cancel-event)
  (set-event obj "dblclick"
             (when handler
               (lambda (data)
                 (declare (ignore data))
                 (funcall handler obj)))
             :one-time one-time
             :cancel-event cancel-event))

;;;;;;;;;;;;;;;;;;;;;;;;
;; set-on-mouse-click ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-mouse-click (clog-obj on-mouse-click-handler &key one-time cancel-event)
  (:documentation "Set the ON-MOUSE-CLICK-HANDLER for CLOG-OBJ. If
ON-MOUSE-CLICK-HANDLER is nil unbind the event. Setting this event will replace
on an on-click event."))

(defmethod set-on-mouse-click ((obj clog-obj) handler &key one-time cancel-event)
  (set-event obj "click"
             (when handler
               (lambda (data)
                 (funcall handler obj (parse-mouse-event data))))
             :one-time one-time
             :cancel-event cancel-event
             :call-back-script mouse-event-script))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set-on-mouse-double-click ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-mouse-double-click (clog-obj on-mouse-double-click-handler
                                       &key one-time cancel-event)
  (:documentation "Set the ON-MOUSE-DOUBLE-CLICK-HANDLER for CLOG-OBJ. If
ON-MOUSE-DOUBLE-CLICK-HANDLER is nil unbind the event. Setting this event will
replace on an on-double-click event."))

(defmethod set-on-mouse-double-click ((obj clog-obj) handler &key one-time cancel-event)
  (set-event obj "dblclick"
             (when handler
               (lambda (data)
                 (funcall handler obj (parse-mouse-event data))))
             :one-time one-time
             :cancel-event cancel-event
             :call-back-script mouse-event-script))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set-on-mouse-right-click ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-mouse-right-click (clog-obj on-mouse-right-click-handler
                                      &key one-time cancel-event)
  (:documentation "Set the ON-MOUSE-RIGHT-CLICK-HANDLER for CLOG-OBJ. If
ON-MOUSE-RIGHT-CLICK-HANDLER is nil unbind the event. Setting this event will
replace on an on-context-menu event."))

(defmethod set-on-mouse-right-click ((obj clog-obj) handler
                                     &key one-time (cancel-event t))
  (set-event obj "contextmenu"
             (when handler
               (lambda (data)
                 (funcall handler obj (parse-mouse-event data))))
             :one-time one-time
             :call-back-script mouse-event-script
             :cancel-event cancel-event))

;;;;;;;;;;;;;;;;;;;;;;;;
;; set-on-mouse-enter ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-mouse-enter (clog-obj on-mouse-enter-handler)
  (:documentation "Set the ON-MOUSE-ENTER-HANDLER for CLOG-OBJ.
 If ON-MOUSE-ENTER-HANDLER is nil unbind the event."))

(defmethod set-on-mouse-enter ((obj clog-obj) handler)
  (set-on-event obj "mouseenter" handler))

;;;;;;;;;;;;;;;;;;;;;;;;
;; set-on-mouse-leave ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-mouse-leave (clog-obj on-mouse-leave-handler)
  (:documentation "Set the ON-MOUSE-LEAVE-HANDLER for CLOG-OBJ.
If ON-MOUSE-LEAVE-HANDLER is nil unbind the event."))

(defmethod set-on-mouse-leave ((obj clog-obj) handler)
  (set-on-event obj "mouseleave" handler))

;;;;;;;;;;;;;;;;;;;;;;;
;; set-on-mouse-over ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-mouse-over (clog-obj on-mouse-over-handler)
  (:documentation "Set the ON-MOUSE-OVER-HANDLER for CLOG-OBJ.
If ON-MOUSE-OVER-HANDLER is nil unbind the event."))

(defmethod set-on-mouse-over ((obj clog-obj) handler)
  (set-on-event obj "mouseover" handler))

;;;;;;;;;;;;;;;;;;;;;;
;; set-on-mouse-out ;;
;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-mouse-out (clog-obj on-mouse-out-handler)
  (:documentation "Set the ON-MOUSE-OUT-HANDLER for CLOG-OBJ.
 If ON-MOUSE-OUT-HANDLER is nil unbind the event."))

(defmethod set-on-mouse-out ((obj clog-obj) handler)
  (set-on-event obj "mouseout" handler))

;;;;;;;;;;;;;;;;;;;;;;;
;; set-on-mouse-down ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-mouse-down (clog-obj on-mouse-down-handler
                               &key one-time cancel-event)
  (:documentation "Set the ON-MOUSE-DOWN-HANDLER for CLOG-OBJ. If
ON-MOUSE-DOWN-HANDLER is nil unbind the event. If cancel-event is true event
does not bubble."))

(defmethod set-on-mouse-down ((obj clog-obj) handler
                              &key (one-time nil) (cancel-event nil))
  (set-event obj "mousedown"
             (when handler
               (lambda (data)
                 (funcall handler obj (parse-mouse-event data))))
             :one-time one-time
             :cancel-event cancel-event
             :call-back-script mouse-event-script))

;;;;;;;;;;;;;;;;;;;;;
;; set-on-mouse-up ;;
;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-mouse-up (clog-obj on-mouse-up-handler
                             &key one-time cancel-event)
  (:documentation "Set the ON-MOUSE-UP-HANDLER for CLOG-OBJ. If
ON-MOUSE-UP-HANDLER is nil unbind the event."))

(defmethod set-on-mouse-up ((obj clog-obj) handler
                            &key (one-time nil) (cancel-event nil))
  (set-event obj "mouseup"
             (when handler
               (lambda (data)
                 (funcall handler obj (parse-mouse-event data))))
             :one-time one-time
             :cancel-event cancel-event
             :call-back-script mouse-event-script))

;;;;;;;;;;;;;;;;;;;;;;;
;; set-on-mouse-move ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-mouse-move (clog-obj on-mouse-move-handler
                               &key one-time cancel-event)
  (:documentation "Set the ON-MOUSE-MOVE-HANDLER for CLOG-OBJ. If
ON-MOUSE-MOVE-HANDLER is nil unbind the event."))

(defmethod set-on-mouse-move ((obj clog-obj) handler
                              &key (one-time nil) (cancel-event nil))
  (set-event obj "mousemove"
             (when handler
               (lambda (data)
                 (funcall handler obj (parse-mouse-event data))))
             :one-time one-time
             :cancel-event cancel-event
             :call-back-script mouse-event-script))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set-on-pointer-enter ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-pointer-enter (clog-obj on-pointer-enter-handler)
  (:documentation "Set the ON-POINTER-ENTER-HANDLER for CLOG-OBJ.
 If ON-POINTER-ENTER-HANDLER is nil unbind the event."))

(defmethod set-on-pointer-enter ((obj clog-obj) handler)
  (set-on-event obj "pointerenter" handler))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set-on-pointer-leave ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-pointer-leave (clog-obj on-pointer-leave-handler)
  (:documentation "Set the ON-POINTER-LEAVE-HANDLER for CLOG-OBJ.
 If ON-POINTER-LEAVE-HANDLER is nil unbind the event."))

(defmethod set-on-pointer-leave ((obj clog-obj) handler)
  (set-on-event obj "pointerleave" handler))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; set-on-pointer-over ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-pointer-over (clog-obj on-pointer-over-handler)
  (:documentation "Set the ON-POINTER-OVER-HANDLER for CLOG-OBJ.
 If ON-POINTER-OVER-HANDLER is nil unbind the event."))

(defmethod set-on-pointer-over ((obj clog-obj) handler)
  (set-on-event obj "pointerover" handler))

;;;;;;;;;;;;;;;;;;;;;;;;
;; set-on-pointer-out ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-pointer-out (clog-obj on-pointer-out-handler)
  (:documentation "Set the ON-POINTER-OUT-HANDLER for CLOG-OBJ.
 If ON-POINTER-OUT-HANDLER is nil unbind the event."))

(defmethod set-on-pointer-out ((obj clog-obj) handler)
  (set-on-event obj "pointerout" handler))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; set-on-pointer-down ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-pointer-down (clog-obj on-pointer-down-handler
                                 &key capture-pointer one-time cancel-event)
  (:documentation "Set the ON-POINTER-DOWN-HANDLER for CLOG-OBJ. If
ON-POINTER-DOWN-HANDLER is nil unbind the event. If cancel event is t the
even does not bubble."))

(defmethod set-on-pointer-down ((obj clog-obj) handler
                                &key (capture-pointer nil)
                                (one-time nil)
                                (cancel-event nil))
  (set-event obj "pointerdown"
             (when handler
               (lambda (data)
                 (funcall handler obj (parse-pointer-event data))))
             :post-eval (if capture-pointer
                            (format nil "; ~A.setPointerCapture(e.pointerId)"
                                    (script-id obj))
                            "")
             :one-time one-time
             :cancel-event cancel-event
             :call-back-script pointer-event-script))

;;;;;;;;;;;;;;;;;;;;;;;
;; set-on-pointer-up ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-pointer-up (clog-obj on-pointer-up-handler
                               &key one-time cancel-event)
  (:documentation "Set the ON-POINTER-UP-HANDLER for CLOG-OBJ. If
ON-POINTER-UP-HANDLER is nil unbind the event."))

(defmethod set-on-pointer-up ((obj clog-obj) handler
                              &key (one-time nil) (cancel-event nil))
  (set-event obj "pointerup"
             (when handler
               (lambda (data)
                 (funcall handler obj (parse-pointer-event data))))
             :post-eval (format nil "; ~A.releasePointerCapture(e.pointerId)"
                                (script-id obj))
             :one-time one-time
             :cancel-event cancel-event
             :call-back-script pointer-event-script))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set-on-pointer-cancel ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-pointer-cancel (clog-obj on-pointer-cancel-handler
                                   &key one-time cancel-event)
  (:documentation "Set the ON-POINTER-CANCEL-HANDLER for CLOG-OBJ. If
ON-POINTER-CANCEL-HANDLER is nil unbind the event."))

(defmethod set-on-pointer-cancel ((obj clog-obj) handler
                                  &key (one-time nil) (cancel-event nil))
  (set-event obj "pointercancel"
             (when handler
               (lambda (data)
                 (funcall handler obj (parse-pointer-event data))))
             :post-eval (format nil "; ~A.releasePointerCapture(e.pointerId)"
                                (script-id obj))
             :one-time one-time
             :cancel-event cancel-event
             :call-back-script pointer-event-script))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; set-on-pointer-move ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-pointer-move (clog-obj on-pointer-move-handler
                                 &key one-time cancel-event)
  (:documentation "Set the ON-POINTER-MOVE-HANDLER for CLOG-OBJ. If
ON-POINTER-MOVE-HANDLER is nil unbind the event."))

(defmethod set-on-pointer-move ((obj clog-obj) handler
                                &key (one-time nil) (cancel-event nil))
  (set-event obj "pointermove"
             (when handler
               (lambda (data)
                 (funcall handler obj (parse-pointer-event data))))
             :one-time one-time
             :cancel-event cancel-event
             :call-back-script pointer-event-script))

;;;;;;;;;;;;;;;;;;;;;;;;
;; set-on-touch-start ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-touch-start (clog-obj on-touch-start-handler
                                &key one-time cancel-event)
  (:documentation "Set the ON-TOUCH-START-HANDLER for CLOG-OBJ. If
ON-TOUCH-START-HANDLER is nil unbind the event."))

(defmethod set-on-touch-start ((obj clog-obj) handler
                               &key (one-time nil) (cancel-event nil))
  (set-event obj "touchstart"
             (when handler
               (lambda (data)
                 (funcall handler obj (parse-touch-event data))))
             :one-time one-time
             :cancel-event cancel-event
             :call-back-script touch-event-script))

;;;;;;;;;;;;;;;;;;;;;;;
;; set-on-touch-move ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-touch-move (clog-obj on-touch-move-handler
                               &key one-time cancel-event)
  (:documentation "Set the ON-TOUCH-MOVE-HANDLER for CLOG-OBJ. If
ON-TOUCH-MOVE-HANDLER is nil unbind the event."))

(defmethod set-on-touch-move ((obj clog-obj) handler
                              &key (one-time nil) (cancel-event nil))
  (set-event obj "touchmove"
             (when handler
               (lambda (data)
                 (funcall handler obj (parse-touch-event data))))
             :one-time one-time
             :cancel-event cancel-event
             :call-back-script touch-event-script))

;;;;;;;;;;;;;;;;;;;;;;
;; set-on-touch-end ;;
;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-touch-end (clog-obj on-touch-end-handler
                              &key one-time cancel-event)
  (:documentation "Set the ON-TOUCH-END-HANDLER for CLOG-OBJ. If
ON-TOUCH-END-HANDLER is nil unbind the event."))

(defmethod set-on-touch-end ((obj clog-obj) handler
                             &key (one-time nil) (cancel-event nil))
  (set-event obj "touchend"
             (when handler
               (lambda (data)
                 (declare (ignore data))
                 (funcall handler obj '(:event-type :touch))))
             :one-time one-time
             :cancel-event cancel-event))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; set-on-touch-cancel ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-touch-cancel (clog-obj on-touch-cancel-handler)
  (:documentation "Set the ON-TOUCH-CANCEL-HANDLER for CLOG-OBJ. If
ON-TOUCH-CANCEL-HANDLER is nil unbind the event."))

(defmethod set-on-touch-cancel ((obj clog-obj) handler)
  (set-event obj "touchcancel"
             (when handler
               (lambda (data)
                 (declare (ignore data))
                 (funcall handler obj)))))

;;;;;;;;;;;;;;;;;;;;;;
;; set-on-character ;;
;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-character (clog-obj on-character-handler
                              &key one-time disable-default)
  (:documentation "Set the ON-CHARACTER-HANDLER for CLOG-OBJ. If
ON-CHARACTER-HANDLER is nil unbind the event. If disable-default is t
default key bindings in browser will not occur. Setting this event to
nil will unbind on-key-press also."))

(defmethod set-on-character ((obj clog-obj) handler
                             &key (one-time nil) (disable-default nil))
  (set-event obj "keypress"
             (when handler
               (lambda (data)
                 (let ((f (parse-keyboard-event data)))
                   (funcall handler obj (code-char (getf f :char-code))))))
             :one-time one-time
             :cancel-event disable-default
             :call-back-script keyboard-event-script))

;;;;;;;;;;;;;;;;;;;;;
;; set-on-key-down ;;
;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-key-down (clog-obj on-key-down-handler
                             &key one-time disable-default)
  (:documentation "Set the ON-KEY-DOWN-HANDLER for CLOG-OBJ. If
disable-default is t default key bindings in browser will not occur.
If ON-KEY-DOWN-HANDLER is nil unbind the event."))

(defmethod set-on-key-down ((obj clog-obj) handler
                            &key (one-time nil) (disable-default nil))
  (set-event obj "keydown"
             (when handler
               (lambda (data)
                 (funcall handler obj (parse-keyboard-event data))))
             :one-time one-time
             :cancel-event disable-default
             :call-back-script keyboard-event-script))

;;;;;;;;;;;;;;;;;;;
;; set-on-key-up ;;
;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-key-up (clog-obj on-key-up-handler &key one-time cancel-event)
  (:documentation "Set the ON-KEY-UP-HANDLER for CLOG-OBJ. If
ON-KEY-UP-HANDLER is nil unbind the event."))

(defmethod set-on-key-up ((obj clog-obj) handler &key one-time cancel-event)
  (set-event obj "keyup"
             (when handler
               (lambda (data)
                 (funcall handler obj (parse-keyboard-event data))))
             :one-time one-time
             :cancel-event cancel-event
             :call-back-script keyboard-event-script))

;;;;;;;;;;;;;;;;;;;;;;
;; set-on-key-press ;;
;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-key-press (clog-obj on-key-press-handler
                              &key one-time disable-default)
  (:documentation "Set the ON-KEY-PRESS-HANDLER for CLOG-OBJ. If
ON-KEY-PRESS-HANDLER is nil unbind the event."))

(defmethod set-on-key-press ((obj clog-obj) handler
                             &key (one-time nil) (disable-default nil))
  (set-event obj "keypress"
             (when handler
               (lambda (data)
                 (funcall handler obj (parse-keyboard-event data))))
             :one-time one-time
             :cancel-event disable-default
             :call-back-script keyboard-event-script))

;;;;;;;;;;;;;;;;;
;; set-on-copy ;;
;;;;;;;;;;;;;;;;;

(defgeneric set-on-copy (clog-obj on-copy-handler)
  (:documentation "Set the ON-COPY-HANDLER for CLOG-OBJ. If ON-COPY-HANDLER
is nil unbind the event."))

(defmethod set-on-copy ((obj clog-obj) handler)
  (set-on-event obj "copy" handler))

;;;;;;;;;;;;;;;;
;; set-on-cut ;;
;;;;;;;;;;;;;;;;

(defgeneric set-on-cut (clog-obj on-cut-handler)
  (:documentation "Set the ON-CUT-HANDLER for CLOG-OBJ. If ON-CUT-HANDLER
is nil unbind the event."))

(defmethod set-on-cut ((obj clog-obj) handler)
  (set-on-event obj "cut" handler))

;;;;;;;;;;;;;;;;;;
;; set-on-paste ;;
;;;;;;;;;;;;;;;;;;

(defgeneric set-on-paste (clog-obj handler)
  (:documentation "Set the ON-PASTE-HANDLER for CLOG-OBJ. If ON-PASTE-HANDLER
is nil unbind the event."))

(defmethod set-on-paste ((obj clog-obj) handler)
  (set-on-event obj "paste" handler))
