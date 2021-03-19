;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG - The Common Lisp Omnificent GUI                                 ;;;;
;;;; (c) 2020-2021 David Botton                                            ;;;;
;;;; License BSD 3 Clause                                                  ;;;;
;;;;                                                                       ;;;;
;;;; clog-base.lisp                                                        ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:in-package :clog)


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
   (connection-data-mutex
    :reader connection-data-mutex
    :initform (bordeaux-threads:make-lock)))
  (:documentation "CLOG objects (clog-obj) encapsulate the connection between
lisp and the HTML DOM element."))

;;;;;;;;;;;;;;;;;;;
;; connection-id ;;
;;;;;;;;;;;;;;;;;;;

(defgeneric connection-id (clog-obj)
  (:documentation "Reader for connection-id slot. (Private)"))

;;;;;;;;;;;;;
;; html-id ;;
;;;;;;;;;;;;;

(defgeneric html-id (clog-obj)
  (:documentation "Internal html-id of CLOG-Obj. (Internal)"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; connection-data-mutex ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric connection-data-mutex (clog-obj)
  (:documentation "Reader for connection-data thread lock. (Private)"))

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
  (:documentation "Return the script id for OBJ based on the html-id set
during attachment. (Private)"))

(defmethod script-id ((obj clog-obj))
  (if (eql (html-id obj) 0)
      "'body'"
      (format nil "clog['~A']" (html-id obj))))

;;;;;;;;;;;;;;;;
;; js-execute ;;
;;;;;;;;;;;;;;;;

(defgeneric js-execute (clog-obj script)
  (:documentation "Execure SCRIPT on browser. (Internal)"))

(defmethod js-execute ((obj clog-obj) script)
  (cc:execute (connection-id obj) script))

;;;;;;;;;;;;;;
;; js-query ;;
;;;;;;;;;;;;;;

(defgeneric js-query (clog-obj script &key default-answer)
  (:documentation "Execure SCRIPT on browser and return result. (Internal)"))

(defmethod js-query ((obj clog-obj) script &key (default-answer nil))
  (cc:query (connection-id obj) script :default-answer default-answer))

;;;;;;;;;;;;
;; jquery ;;
;;;;;;;;;;;;

(defgeneric jquery (clog-obj)
  (:documentation "Return the jquery accessor for OBJ. (Private)"))

(defmethod jquery ((obj clog-obj))
  (format nil "$(~A)" (script-id obj)))

;;;;;;;;;;;;;;;;;;;;
;; jquery-execute ;;
;;;;;;;;;;;;;;;;;;;;

(defgeneric jquery-execute (clog-obj method)
  (:documentation "Execute the jquery METHOD on OBJ. Result is
dicarded. (Private)"))

(defmethod jquery-execute ((obj clog-obj) method)
  (js-execute obj (format nil "~A.~A" (jquery obj) method)))

;;;;;;;;;;;;;;;;;;
;; jquery-query ;;
;;;;;;;;;;;;;;;;;;

(defgeneric jquery-query (clog-obj method &key default-answer)
  (:documentation "Execute the jquery METHOD on OBJ and return
result or DEFAULT-ANSWER on time out. (Private)"))

(defmethod jquery-query ((obj clog-obj) method &key (default-answer nil))
  (js-query obj (format nil "~A.~A" (jquery obj) method)
	    :default-answer default-answer))

;;;;;;;;;;;;;
;; execute ;;
;;;;;;;;;;;;;

(defgeneric execute (clog-obj method)
  (:documentation "Execute the JavaScript METHOD on OBJ. Result is
dicarded. (Private)"))

(defmethod execute ((obj clog-obj) method)
  (js-execute obj (format nil "~A.~A" (script-id obj) method)))

;;;;;;;;;;;
;; query ;;
;;;;;;;;;;;

(defgeneric query (clog-obj method &key default-answer)
  (:documentation "Execute the JavaScript query METHOD on OBJ and return
result or if time out DEFAULT-ANSWER (Private)"))

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
  "+ (e.clientX - e.target.getBoundingClientRect().left) + ':' + 
     (e.clientY - e.target.getBoundingClientRect().top) + ':' + 
     e.screenX + ':' + e.screenY + ':' + e.which + ':' + e.altKey + ':' +
     e.ctrlKey + ':' + e.shiftKey + ':' + e.metaKey"
  "JavaScript to collect mouse event data from browser.")
;; e.buttons would be better but not supported currently outside
;; of firefox and would always return 0 on Mac so using e.which.
;; The use of offsetLeft and offsetTop is to correct the X and Y
;; to the actual X,Y of the target.

(defun parse-mouse-event (data)
  (let ((f (ppcre:split ":" data)))
    (list
     :event-type   :mouse
     :x            (parse-integer (nth 0 f) :junk-allowed t)
     :y            (parse-integer (nth 1 f) :junk-allowed t)
     :screen-x     (parse-integer (nth 2 f) :junk-allowed t)
     :screen-y     (parse-integer (nth 3 f) :junk-allowed t)
     :which-button (parse-integer (nth 4 f) :junk-allowed t)
     :alt-key      (js-true-p (nth 5 f))
     :ctrl-key     (js-true-p (nth 6 f))
     :shift-key    (js-true-p (nth 7 f))
     :meta-key     (js-true-p (nth 8 f)))))

;;;;;;;;;;;;;;;;;;;;;;;
;; parse-touch-event ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defparameter touch-event-script
  "+ (e.touches[0].clientX - e.touches[0].target.getBoundingClientRect().left) + ':' + 
     (e.touches[0].clientY - e.touches[0].target.getBoundingClientRect().top) + ':' + 
     e.touches[0].screenX + ':' + e.touches[0].screenY + ':' + e.touches.length + ':' +
     e.altKey + ':' +
     e.ctrlKey + ':' +
     e.shiftKey + ':' +
     e.metaKey"
    "JavaScript to collect touch event data from browser.")

(defun parse-touch-event (data)
  (let ((f (ppcre:split ":" data)))
    (list
     :event-type     :touch
     :x              (parse-integer (nth 0 f) :junk-allowed t)
     :y              (parse-integer (nth 1 f) :junk-allowed t)
     :screen-x       (parse-integer (nth 2 f) :junk-allowed t)
     :screen-y       (parse-integer (nth 3 f) :junk-allowed t)
     :number-fingers (parse-integer (nth 4 f) :junk-allowed t)
     :alt-key        (js-true-p (nth 5 f))
     :ctrl-key       (js-true-p (nth 6 f))
     :shift-key      (js-true-p (nth 7 f))
     :meta-key       (js-true-p (nth 8 f)))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; parse-pointer-event ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter pointer-event-script
  "+ (e.clientX - e.target.getBoundingClientRect().left) + ':' + 
     (e.clientY - e.target.getBoundingClientRect().top) + ':' + 
     e.screenX + ':' + e.screenY + ':' + e.which + ':' + e.altKey + ':' +
     e.ctrlKey + ':' + e.shiftKey + ':' + e.metaKey"
  "JavaScript to collect pointer event data from browser.")

(defun parse-pointer-event (data)
  (let ((f (ppcre:split ":" data)))
    (list
     :event-type   :pointer
     :x            (parse-integer (nth 0 f) :junk-allowed t)
     :y            (parse-integer (nth 1 f) :junk-allowed t)
     :screen-x     (parse-integer (nth 2 f) :junk-allowed t)
     :screen-y     (parse-integer (nth 3 f) :junk-allowed t)
     :which-button (parse-integer (nth 4 f) :junk-allowed t)
     :alt-key      (js-true-p (nth 5 f))
     :ctrl-key     (js-true-p (nth 6 f))
     :shift-key    (js-true-p (nth 7 f))
     :meta-key     (js-true-p (nth 8 f)))))

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
     :key-code   (parse-integer (nth 0 f) :junk-allowed t)
     :char-code  (parse-integer (nth 1 f) :junk-allowed t)
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
  "+ (e.clientX - e.target.getBoundingClientRect().left) + ':' + 
     (e.clientY - e.target.getBoundingClientRect().top) + ':' +
     encodeURIComponent(e.originalEvent.dataTransfer.getData('~A'))"
  "JavaScript to collect drop event data from browser.")

(defun parse-drop-event (data)
  (let ((f (ppcre:split ":" data)))
    (list
     :event-type   :drop
     :x            (parse-integer (nth 0 f) :junk-allowed t)
     :y            (parse-integer (nth 1 f) :junk-allowed t)
     :drag-data    (quri:url-decode (or (nth 2 f) "")))))

;;;;;;;;;;;;;;;
;; set-event ;;
;;;;;;;;;;;;;;;

(defgeneric set-event (clog-obj event handler
		       &key call-back-script
			 eval-script
			 post-eval
			 cancel-event
			 one-time)
  (:documentation "Create the hook for incoming events. (Private)"))

(defmethod set-event ((obj clog-obj) event handler
		      &key (call-back-script "")
			(eval-script "")
			(post-eval "")
			(cancel-event nil)
			(one-time nil))
  (let ((hook (format nil "~A:~A" (html-id obj) event)))
    (cond (handler
	   (bind-event-script
	    obj event (format nil "~Aws.send('E:~A '~A)~A~A~A"
			      eval-script
			      hook
			      call-back-script
			      post-eval
			      (if one-time
				  (format nil "; ~A.off('~A')"
					  (jquery obj)
					  event)
				  "")
			      (if cancel-event
				  "; return false"
				  "")))
	   (setf (gethash hook (connection-data obj)) handler))
	  (t
	   (unbind-event-script obj event)
	   (remhash hook (connection-data obj))))))

;;;;;;;;;;;;;;
;; property ;;
;;;;;;;;;;;;;;

(defgeneric property (clog-obj property-name &key default-answer)
  (:documentation "Get/Setf html property."))

(defmethod property ((obj clog-obj) property-name &key (default-answer nil))
  (jquery-query obj (format nil "prop('~A')" property-name)
		:default-answer default-answer))

(defgeneric set-property (clog-obj property-name value)
  (:documentation "Set html property."))

(defmethod set-property ((obj clog-obj) property-name value)
  (jquery-execute obj (format nil "prop('~A','~A')" property-name (escape-string value))))
(defsetf property set-property)

;;;;;;;;;;;;
;; height ;;
;;;;;;;;;;;;

(defgeneric height (clog-obj)
  (:documentation "Get/Setf html height in pixels."))

(defmethod height ((obj clog-obj))
  (parse-integer (jquery-query obj "height()") :junk-allowed t))

(defgeneric set-height (clog-obj value)
  (:documentation "Set height VALUE for CLOG-OBJ"))

(defmethod set-height ((obj clog-obj) value)
  (jquery-execute obj (format nil "height('~A')" (escape-string value))))
(defsetf height set-height)

;;;;;;;;;;;
;; width ;;
;;;;;;;;;;;

(defgeneric width (clog-obj)
  (:documentation "Get/Setf html width in pixels."))

(defmethod width ((obj clog-obj))
  (parse-integer (jquery-query obj "width()") :junk-allowed t))

(defgeneric set-width (clog-obj value)
  (:documentation "Set width VALUE for CLOG-OBJ"))

(defmethod set-width ((obj clog-obj) value)
  (jquery-execute obj (format nil "width('~A')" (escape-string value))))
(defsetf width set-width)

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
  (cc:validp (connection-id obj)))

;;;;;;;;;;;;;;;;;;;;;
;; connection-data ;;
;;;;;;;;;;;;;;;;;;;;;

(defgeneric connection-data (clog-obj)
  (:documentation "Get connection-data that is associated with
clog-obj that will persist regardless of thread. The event hooks
are stored in this string based hash in the format of:
\"html-id:event-name\" => #'event-handler. clog-* keys are reserved
for internal use of clog. The key \"clog-body\" is set to the
clog-body of this connection."))

(defmethod connection-data ((obj clog-obj))
  (cc:get-connection-data (connection-id obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; connection-data-item ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric connection-data-item (clog-obj item-name)
  (:documentation "Get/Setf from connection-data the item-name in hash."))

(defmethod connection-data-item ((obj clog-obj) item-name)
  (gethash item-name (connection-data obj)))

(defgeneric set-connection-data-item (clog-obj item-name value)
  (:documentation "Set connection-data the item-name in hash."))

(defmethod set-connection-data-item ((obj clog-obj) item-name value)
  (bordeaux-threads:with-lock-held ((connection-data-mutex obj))
    (setf (gethash item-name (connection-data obj)) value)))
(defsetf connection-data-item set-connection-data-item)

(defgeneric remove-connection-data-item (clog-obj item-name)
  (:documentation "Remove from connection-data the item-name in hash."))

(defmethod remove-connection-data-item ((obj clog-obj) item-name)
  (bordeaux-threads:with-lock-held ((connection-data-mutex obj))
    (remhash item-name (connection-data obj))))

;;;;;;;;;;;;;;;;;;
;; set-on-event ;;
;;;;;;;;;;;;;;;;;;

(defun clog-call (handler &rest arguments)
  "You can use it just like `funcall`, except it calls both functions and symbols"
  (apply
   (if (functionp handler)
       handler
       (symbol-function handler))
   arguments))

(defgeneric set-on-event (clog-obj event-name handler)
  (:documentation "Set a HANDLER for EVENT-NAME on CLOG-OBJ. If handler is
nil unbind all event handlers. (Private)"))

(defmethod set-on-event ((obj clog-obj) event-name handler)
  (set-event obj event-name
	     (when handler
	       (lambda (data)
		 (declare (ignore data))
		 (clog-call handler obj)))))


;;;;;;;;;;;;;;;;;;;
;; set-on-resize ;;
;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-resize (clog-obj on-resize-handler)
  (:documentation "Set the ON-RESIZE-HANDLER for CLOG-OBJ. If ON-RESIZE-HANDLER
is nil unbind the event."))

(defmethod set-on-resize ((obj clog-obj) handler)
  (set-on-event obj "resize" handler))


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
		 (clog-call handler obj)))
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
  (:documentation "Set the ON-DRAG-END-HANDLER for CLOG-OBJ. If ON-DRAG-END-HANDLER
is nil unbind the event."))

(defmethod set-on-drag-end ((obj clog-obj) handler)
  (set-on-event obj "dragend" handler))

;;;;;;;;;;;;;;;;;;;;;;;
;; set-on-drag-enter ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-drag-enter (clog-obj on-drag-enter-handler)
  (:documentation "Set the ON-DRAG-ENTER-HANDLER for CLOG-OBJ. If ON-DRAG-ENTER-HANDLER
is nil unbind the event."))

(defmethod set-on-drag-enter ((obj clog-obj) handler)
  (set-on-event obj "dragenter" handler))


;;;;;;;;;;;;;;;;;;;;;;;
;; set-on-drag-leave ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-drag-leave (clog-obj on-drag-leave-handler)
  (:documentation "Set the ON-DRAG-LEAVE-HANDLER for CLOG-OBJ. If ON-DRAG-LEAVE-HANDLER
is nil unbind the event."))

(defmethod set-on-drag-leave ((obj clog-obj) handler)
  (set-on-event obj "dragleave" handler))

;;;;;;;;;;;;;;;;;;;;;;
;; set-on-drag-over ;;
;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-drag-over (clog-obj on-drag-over-handler)
  (:documentation "Set the ON-DRAG-OVER-HANDLER for CLOG-OBJ. If ON-DRAG-OVER-HANDLER
is nil unbind the event."))

(defmethod set-on-drag-over ((obj clog-obj) handler)
  (set-event obj "dragover"
	     (when handler
	       (lambda (data)
		 (declare (ignore data))
		 (clog-call handler obj)))
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
		 (clog-call handler obj (parse-drop-event data))))
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
this even is bound, you must call the form reset manually."))

(defmethod set-on-reset ((obj clog-obj) handler)
  (set-event obj "reset"
	     (when handler
	       (lambda (data)
		 (declare (ignore data))
		 (clog-call handler obj)))
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
		 (clog-call handler obj)))
	     :cancel-event t))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; set-on-context-menu ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-context-menu (clog-obj on-context-menu-handler
				 &key one-time)
  (:documentation "Set the ON-CONTEXT-MENU-HANDLER for CLOG-OBJ. If
ON-CONTEXT-MENU-HANDLER is nil unbind the event. Setting
on-mouse-right-click will replace this handler. If :ONE-TIME unbind
event on right click."))

(defmethod set-on-context-menu ((obj clog-obj) handler &key (one-time nil))
  (set-event obj "contextmenu"
	     (when handler
	       (lambda (data)
		 (declare (ignore data))
		 (clog-call handler obj)))
	     :one-time one-time
	     :cancel-event t))

;;;;;;;;;;;;;;;;;;
;; set-on-click ;;
;;;;;;;;;;;;;;;;;;

(defgeneric set-on-click (clog-obj on-click-handler &key one-time)
  (:documentation "Set the ON-CLICK-HANDLER for CLOG-OBJ. If ON-CLICK-HANDLER
is nil unbind the event. Setting this event will replace an on-mouse click if
set. If :ONE-TIME unbind event on click."))

(defmethod set-on-click ((obj clog-obj) handler &key (one-time nil))
  (set-event obj "click"
	     (when handler
	       (lambda (data)
		 (declare (ignore data))
		 (clog-call handler obj)))
	     :one-time one-time))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; set-on-double-click ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-double-click (clog-obj on-double-click-handler
				 &key one-time)
  (:documentation "Set the ON-DOUBLE-CLICK-HANDLER for CLOG-OBJ. If
ON-DOUBLE-CLICK-HANDLER is nil unbind the event. Setting the
on-mouse-double-click event will replace this handler."))

(defmethod set-on-double-click ((obj clog-obj) handler &key (one-time nil))
  (set-event obj "dblclick"
	     (when handler
	       (lambda (data)
		 (declare (ignore data))
		 (clog-call handler obj)))
	     :one-time one-time))

;;;;;;;;;;;;;;;;;;;;;;;;
;; set-on-mouse-click ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-mouse-click (clog-obj on-mouse-click-handler &key one-time)
  (:documentation "Set the ON-MOUSE-CLICK-HANDLER for CLOG-OBJ. If
ON-MOUSE-CLICK-HANDLER is nil unbind the event. Setting this event will replace
on an on-click event."))

(defmethod set-on-mouse-click ((obj clog-obj) handler &key (one-time nil))
  (set-event obj "click"
	     (when handler
	       (lambda (data)
		 (clog-call handler obj (parse-mouse-event data))))
	     :one-time one-time
	     :call-back-script mouse-event-script))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set-on-mouse-double-click ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-mouse-double-click (clog-obj on-mouse-double-click-handler
				       &key one-time)
  (:documentation "Set the ON-MOUSE-DOUBLE-CLICK-HANDLER for CLOG-OBJ. If
ON-MOUSE-DOUBLE-CLICK-HANDLER is nil unbind the event. Setting this event will
replace on an on-double-click event."))

(defmethod set-on-mouse-double-click ((obj clog-obj) handler &key (one-time nil))
  (set-event obj "dblclick"
	     (when handler
	       (lambda (data)
		 (clog-call handler obj (parse-mouse-event data))))
	     :one-time one-time
	     :call-back-script mouse-event-script))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set-on-mouse-right-click ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-mouse-right-click (clog-obj on-mouse-right-click-handler
				       &key one-time)
  (:documentation "Set the ON-MOUSE-RIGHT-CLICK-HANDLER for CLOG-OBJ. If
ON-MOUSE-RIGHT-CLICK-HANDLER is nil unbind the event. Setting this event will
replace on an on-context-menu event."))

(defmethod set-on-mouse-right-click ((obj clog-obj) handler
				     &key (one-time nil))
  (set-event obj "contextmenu"
	     (when handler
	       (lambda (data)
		 (clog-call handler obj (parse-mouse-event data))))
	     :one-time one-time
	     :call-back-script mouse-event-script
     	     :cancel-event t))

;;;;;;;;;;;;;;;;;;;;;;;;
;; set-on-mouse-enter ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-mouse-enter (clog-obj on-mouse-enter-handler)
  (:documentation "Set the ON-MOUSE-ENTER-HANDLER for CLOG-OBJ. If ON-MOUSE-ENTER-HANDLER
is nil unbind the event."))

(defmethod set-on-mouse-enter ((obj clog-obj) handler)
  (set-on-event obj "mouseenter" handler))

;;;;;;;;;;;;;;;;;;;;;;;;
;; set-on-mouse-leave ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-mouse-leave (clog-obj on-mouse-leave-handler)
  (:documentation "Set the ON-MOUSE-LEAVE-HANDLER for CLOG-OBJ. If ON-MOUSE-LEAVE-HANDLER
is nil unbind the event."))

(defmethod set-on-mouse-leave ((obj clog-obj) handler)
  (set-on-event obj "mouseleave" handler))

;;;;;;;;;;;;;;;;;;;;;;;
;; set-on-mouse-over ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-mouse-over (clog-obj on-mouse-over-handler)
  (:documentation "Set the ON-MOUSE-OVER-HANDLER for CLOG-OBJ. If ON-MOUSE-OVER-HANDLER
is nil unbind the event."))

(defmethod set-on-mouse-over ((obj clog-obj) handler)
  (set-on-event obj "mouseover" handler))

;;;;;;;;;;;;;;;;;;;;;;
;; set-on-mouse-out ;;
;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-mouse-out (clog-obj on-mouse-out-handler)
  (:documentation "Set the ON-MOUSE-OUT-HANDLER for CLOG-OBJ. If ON-MOUSE-OUT-HANDLER
is nil unbind the event."))

(defmethod set-on-mouse-out ((obj clog-obj) handler)
  (set-on-event obj "mouseout" handler))

;;;;;;;;;;;;;;;;;;;;;;;
;; set-on-mouse-down ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-mouse-down (clog-obj on-mouse-down-handler &key one-time)
  (:documentation "Set the ON-MOUSE-DOWN-HANDLER for CLOG-OBJ. If
ON-MOUSE-DOWN-HANDLER is nil unbind the event."))

(defmethod set-on-mouse-down ((obj clog-obj) handler &key (one-time nil))
  (set-event obj "mousedown"
	     (when handler
	       (lambda (data)
		 (clog-call handler obj (parse-mouse-event data))))
	     :one-time one-time
	     :call-back-script mouse-event-script))

;;;;;;;;;;;;;;;;;;;;;
;; set-on-mouse-up ;;
;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-mouse-up (clog-obj on-mouse-up-handler)
  (:documentation "Set the ON-MOUSE-UP-HANDLER for CLOG-OBJ. If
ON-MOUSE-UP-HANDLER is nil unbind the event."))

(defmethod set-on-mouse-up ((obj clog-obj) handler)
  (set-event obj "mouseup"
	     (when handler
	       (lambda (data)
		 (clog-call handler obj (parse-mouse-event data))))
	     :call-back-script mouse-event-script))

;;;;;;;;;;;;;;;;;;;;;;;
;; set-on-mouse-move ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-mouse-move (clog-obj on-mouse-move-handler)
  (:documentation "Set the ON-MOUSE-MOVE-HANDLER for CLOG-OBJ. If
ON-MOUSE-MOVE-HANDLER is nil unbind the event."))

(defmethod set-on-mouse-move ((obj clog-obj) handler)
  (set-event obj "mousemove"
	     (when handler
	       (lambda (data)
		 (clog-call handler obj (parse-mouse-event data))))
	     :call-back-script mouse-event-script))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set-on-pointer-enter ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-pointer-enter (clog-obj on-pointer-enter-handler)
  (:documentation "Set the ON-POINTER-ENTER-HANDLER for CLOG-OBJ. If ON-POINTER-ENTER-HANDLER
is nil unbind the event."))

(defmethod set-on-pointer-enter ((obj clog-obj) handler)
  (set-on-event obj "pointerenter" handler))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set-on-pointer-leave ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-pointer-leave (clog-obj on-pointer-leave-handler)
  (:documentation "Set the ON-POINTER-LEAVE-HANDLER for CLOG-OBJ. If ON-POINTER-LEAVE-HANDLER
is nil unbind the event."))

(defmethod set-on-pointer-leave ((obj clog-obj) handler)
  (set-on-event obj "pointerleave" handler))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; set-on-pointer-over ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-pointer-over (clog-obj on-pointer-over-handler)
  (:documentation "Set the ON-POINTER-OVER-HANDLER for CLOG-OBJ. If ON-POINTER-OVER-HANDLER
is nil unbind the event."))

(defmethod set-on-pointer-over ((obj clog-obj) handler)
  (set-on-event obj "pointerover" handler))

;;;;;;;;;;;;;;;;;;;;;;;;
;; set-on-pointer-out ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-pointer-out (clog-obj on-pointer-out-handler)
  (:documentation "Set the ON-POINTER-OUT-HANDLER for CLOG-OBJ. If ON-POINTER-OUT-HANDLER
is nil unbind the event."))

(defmethod set-on-pointer-out ((obj clog-obj) handler)
  (set-on-event obj "pointerout" handler))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; set-on-pointer-down ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-pointer-down (clog-obj on-pointer-down-handler
				 &key capture-pointer one-time)
  (:documentation "Set the ON-POINTER-DOWN-HANDLER for CLOG-OBJ. If
ON-POINTER-DOWN-HANDLER is nil unbind the event."))

(defmethod set-on-pointer-down ((obj clog-obj) handler
				&key (capture-pointer nil) (one-time nil))
  (set-event obj "pointerdown"
	     (when handler
	       (lambda (data)
		 (clog-call handler obj (parse-pointer-event data))))
	     :post-eval (if capture-pointer
			    (format nil "; ~A.setPointerCapture(e.pointerId)"
				    (script-id obj))
			    "")
	     :one-time one-time
	     :call-back-script pointer-event-script))

;;;;;;;;;;;;;;;;;;;;;;;
;; set-on-pointer-up ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-pointer-up (clog-obj on-pointer-up-handler)
  (:documentation "Set the ON-POINTER-UP-HANDLER for CLOG-OBJ. If
ON-POINTER-UP-HANDLER is nil unbind the event."))

(defmethod set-on-pointer-up ((obj clog-obj) handler)
  (set-event obj "pointerup"
	     (when handler
	       (lambda (data)
		 (clog-call handler obj (parse-pointer-event data))))
	     :post-eval (format nil "; ~A.releasePointerCapture(e.pointerId)"
				(script-id obj))
	     :call-back-script pointer-event-script))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; set-on-pointer-move ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-pointer-move (clog-obj on-pointer-move-handler)
  (:documentation "Set the ON-POINTER-MOVE-HANDLER for CLOG-OBJ. If
ON-POINTER-MOVE-HANDLER is nil unbind the event."))

(defmethod set-on-pointer-move ((obj clog-obj) handler)
  (set-event obj "pointermove"
	     (when handler
	       (lambda (data)
		 (clog-call handler obj (parse-pointer-event data))))
	     :call-back-script pointer-event-script))

;;;;;;;;;;;;;;;;;;;;;;;;
;; set-on-touch-start ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-touch-start (clog-obj on-touch-start-handler &key one-time)
  (:documentation "Set the ON-TOUCH-START-HANDLER for CLOG-OBJ. If
ON-TOUCH-START-HANDLER is nil unbind the event."))

(defmethod set-on-touch-start ((obj clog-obj) handler &key (one-time nil))
  (set-event obj "touchstart"
	     (when handler
	       (lambda (data)
		 (clog-call handler obj (parse-touch-event data))))
	     :one-time one-time
	     :call-back-script touch-event-script))

;;;;;;;;;;;;;;;;;;;;;;;
;; set-on-touch-move ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-touch-move (clog-obj on-touch-move-handler)
  (:documentation "Set the ON-TOUCH-MOVE-HANDLER for CLOG-OBJ. If
ON-TOUCH-MOVE-HANDLER is nil unbind the event."))

(defmethod set-on-touch-move ((obj clog-obj) handler)
  (set-event obj "touchmove"
	     (when handler
	       (lambda (data)
		 (clog-call handler obj (parse-touch-event data))))
	     :call-back-script touch-event-script))

;;;;;;;;;;;;;;;;;;;;;;
;; set-on-touch-end ;;
;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-touch-end (clog-obj on-touch-end-handler)
  (:documentation "Set the ON-TOUCH-END-HANDLER for CLOG-OBJ. If
ON-TOUCH-END-HANDLER is nil unbind the event."))

(defmethod set-on-touch-end ((obj clog-obj) handler)
  (set-event obj "touchend"
	     (when handler
	       (lambda (data)
		 (clog-call handler obj (parse-touch-event data))))
	     :call-back-script touch-event-script))

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
		 (clog-call handler obj (parse-touch-event data))))
	     :call-back-script touch-event-script))

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
		 (clog-call handler obj (code-char (getf f :char-code))))))
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
		 (clog-call handler obj (parse-keyboard-event data))))
	     :one-time one-time
	     :cancel-event disable-default
	     :call-back-script keyboard-event-script))   

;;;;;;;;;;;;;;;;;;;
;; set-on-key-up ;;
;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-key-up (clog-obj on-key-up-handler &key one-time)
  (:documentation "Set the ON-KEY-UP-HANDLER for CLOG-OBJ. If
ON-KEY-UP-HANDLER is nil unbind the event."))

(defmethod set-on-key-up ((obj clog-obj) handler &key (one-time nil))
  (set-event obj "keyup"
	     (when handler
	       (lambda (data)
		 (clog-call handler obj (parse-keyboard-event data))))
	     :one-time one-time
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
		 (clog-call handler obj (parse-keyboard-event data))))
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
