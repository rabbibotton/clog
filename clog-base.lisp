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
    :initarg :html-id))
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
  (:documentation "Reader for html-id slot. (Private)"))

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
  (cc:execute (connection-id obj)
	      (format nil "~A.~A" (jquery obj) method)))

;;;;;;;;;;;;;;;;;;
;; jquery-query ;;
;;;;;;;;;;;;;;;;;;

(defgeneric jquery-query (clog-obj method)
  (:documentation "Execute the jquery METHOD on OBJ and return
result. (Private)"))

(defmethod jquery-query ((obj clog-obj) method)
  (cc:query (connection-id obj)
	    (format nil "~A.~A" (jquery obj) method)))

;;;;;;;;;;;;;
;; execute ;;
;;;;;;;;;;;;;

(defgeneric execute (clog-obj method)
  (:documentation "Execute the js METHOD on OBJ. Result is
dicarded. (Private)"))

(defmethod execute ((obj clog-obj) method)
  (cc:execute (connection-id obj)
	      (format nil "~A.~A" (script-id obj) method)))

;;;;;;;;;;;
;; query ;;
;;;;;;;;;;;

(defgeneric query (clog-obj method)
  (:documentation "Execute the js query METHOD on OBJ and return
result. (Private)"))

(defmethod query ((obj clog-obj) method)
  (cc:query (connection-id obj)
	    (format nil "~A.~A" (script-id obj) method)))

;;;;;;;;;;;;;;;;;;;;;;;
;; bind-event-script ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric bind-event-script (clog-obj event call-back &key cancel-event)
  (:documentation "Create the code client side for EVENT CALL-BACK. (Private)"))

(defmethod bind-event-script ((obj clog-obj) event call-back
			      &key (cancel-event nil))
  (if cancel-event
      (jquery-execute
       obj (format nil "on('~A',function (e, data){~A});return false"
		   event call-back))
      (jquery-execute
       obj (format nil "on('~A',function (e, data){~A})" event call-back))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; unbind-event-script ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric unbind-event-script (clog-obj event)
  (:documentation "Remove the client call back code for EVENT. (Private)"))

(defmethod unbind-event-script ((obj clog-obj) event)
  (jquery-execute obj (format nil "off(~A)" event)))

;;;;;;;;;;;;;;;;;;;;;;;
;; parse-mouse-event ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defparameter mouse-event-script
  "+ (e.clientX - e.target.getBoundingClientRect().left) + ':' + 
     (e.clientY - e.target.getBoundingClientRect().top) + ':' + 
     e.screenX + ':' + e.screenY + ':' + e.which + ':' + e.altKey + ':' +
     e.ctrlKey + ':' + e.shiftKey + ':' + e.metaKey")
;; e.buttons would be better but not supported currently outside
;; of firefox and would always return 0 on Mac so using e.which.
;; The use of offsetLeft and offsetTop is to correct the X and Y
;; to the actual X,Y of the target.

(defun parse-mouse-event (data)
  (let ((f (ppcre:split ":" data)))
    (list
     :x            (parse-integer (nth 0 f) :junk-allowed t)
     :y            (parse-integer (nth 1 f) :junk-allowed t)
     :screen-y     (parse-integer (nth 2 f) :junk-allowed t)
     :screen-x     (parse-integer (nth 3 f) :junk-allowed t)
     :which-button (parse-integer (nth 4 f) :junk-allowed t)
     :alt-key      (js-true-p (nth 5 f))
     :ctrl-key     (js-true-p (nth 6 f))
     :shift-key    (js-true-p (nth 7 f))
     :meta-key     (js-true-p (nth 8 f)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parse-keyboard-event ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter keyboard-event-script
  "+ e.keyCode + ':' + e.charCode + ':' + e.altKey + ':' + e.ctrlKey + ':' +
     e.shiftKey + ':' + e.metaKey")

(defun parse-keyboard-event (data)
  (let ((f (ppcre:split ":" data)))
    (list
     :key-code  (parse-integer (nth 0 f) :junk-allowed t)
     :char-code (parse-integer (nth 1 f) :junk-allowed t)
     :alt-key   (js-true-p (nth 2 f))
     :ctrl-key  (js-true-p (nth 3 f))
     :shift-key (js-true-p (nth 4 f))
     :meta-key  (js-true-p (nth 5 f)))))

;;;;;;;;;;;;;;;
;; set-event ;;
;;;;;;;;;;;;;;;

(defgeneric set-event (clog-obj event handler &key call-back-script)
  (:documentation "Create the hood for incoming events. (Private)"))

(defmethod set-event ((obj clog-obj) event handler &key (call-back-script ""))
  ;; meeds mutex
  (let ((hook (format nil "~A:~A" (html-id obj) event)))
    (cond (handler
	   (bind-event-script
	    obj event (format nil "ws.send('E:~A-'~A)" hook call-back-script))
	   (setf (gethash hook (connection-data obj)) handler))
	  (t
	   (unbind-event-script obj event)
	   (remhash hook (connection-data obj))))))

;;;;;;;;;;;;;;
;; property ;;
;;;;;;;;;;;;;;

(defgeneric property (clog-obj property-name)
  (:documentation "Get/Setf html property. (eg. draggable)"))

(defmethod property ((obj clog-obj) property-name)
  (jquery-query obj (format nil "prop('~A')" property-name)))

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
  (jquery-query obj "height()"))

(defgeneric set-height (clog-obj value)
  (:documentation "Set height VALUE for CLOG-OBJ"))

(defmethod set-height ((obj clog-obj) value)
  (jquery-execute obj (format nil "height('~A')" value)))
(defsetf height set-height)

;;;;;;;;;;;
;; width ;;
;;;;;;;;;;;

(defgeneric width (clog-obj)
  (:documentation "Get/Setf html width in pixels."))

(defmethod width ((obj clog-obj))
  (jquery-query obj "width()"))

(defgeneric set-width (clog-obj value)
  (:documentation "Set width VALUE for CLOG-OBJ"))

(defmethod set-width ((obj clog-obj) value)
  (jquery-execute obj (format nil "width('~A')" value)))
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

(defmethod focus ((obj clog-obj))
  (jquery-execute obj "blur()"))

;;;;;;;;;;;;
;; validp ;;
;;;;;;;;;;;;

(defgeneric validp (clog-obj)
  (:documentation "Returns true of connection is valid on this CLOG-OBJ."))

(defmethod validp ((obj clog-obj))
  (cc:validp (connection-id obj)))

;;;;;;;;;;;;;;;;;;;;;
;; connection-data ;;
;;;;;;;;;;;;;;;;;;;;;

(defgeneric connection-data (clog-obj)
  (:documentation "Get connection-data that is associated with
clog-obj that will persist regardless of thread. The event hooks
are stored in this string based hash in the format of:
\"html-id:event-name\" => event-handler."))

(defmethod connection-data ((obj clog-obj))
  (cc:get-connection-data (connection-id obj)))

;;;;;;;;;;;;;;;;;;;
;; set-on-resize ;;
;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-resize (clog-obj on-resize-handler)
  (:documentation "Set the ON-RESIZE-HANDLER for CLOG-OBJ. If ON-RESIZE-HANDLER
is nil unbind the event."))

(defmethod set-on-resize ((obj clog-obj) on-resize-handler)
  (let ((on-resize on-resize-handler))
    (set-event obj "resize"
	       (lambda (data)
		 (declare (ignore data))
		 (funcall on-resize)))))

;;;;;;;;;;;;;;;;;;
;; set-on-focus ;;
;;;;;;;;;;;;;;;;;;

(defgeneric set-on-focus (clog-obj on-focus-handler)
  (:documentation "Set the ON-FOCUS-HANDLER for CLOG-OBJ. If ON-FOCUS-HANDLER
is nil unbind the event."))

(defmethod set-on-focus ((obj clog-obj) on-focus-handler)
  (let ((on-focus on-focus-handler))      
    (set-event obj "focus"
	       (lambda (data)
		 (declare (ignore data))
		 (funcall on-focus)))))

;;;;;;;;;;;;;;;;;
;; set-on-blur ;;
;;;;;;;;;;;;;;;;;

(defgeneric set-on-blur (clog-obj on-blur-handler)
  (:documentation "Set the ON-BLUR-HANDLER for CLOG-OBJ. If ON-BLUR-HANDLER
is nil unbind the event."))

(defmethod set-on-blur ((obj clog-obj) on-blur-handler)
  (let ((on-blur on-blur-handler))      
    (set-event obj "blur"
	       (lambda (data)
		 (declare (ignore data))
		 (funcall on-blur)))))

;;;;;;;;;;;;;;;;;;;
;; set-on-change ;;
;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-change (clog-obj on-change-handler)
  (:documentation "Set the ON-CHANGE-HANDLER for CLOG-OBJ. If ON-CHANGE-HANDLER
is nil unbind the event."))

(defmethod set-on-change ((obj clog-obj) on-change-handler)
  (let ((on-change on-change-handler))      
    (set-event obj "change"
	       (lambda (data)
		 (declare (ignore data))
		 (funcall on-change)))))

;;;;;;;;;;;;;;;;;;;;;
;; set-on-focus-in ;;
;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-focus-in (clog-obj on-focus-in-handler)
  (:documentation "Set the ON-FOCUS-IN-HANDLER for CLOG-OBJ. If
ON-FOCUS-IN-HANDLER is nil unbind the event."))

(defmethod set-on-focus-in ((obj clog-obj) on-focus-in-handler)
  (let ((on-focus-in on-focus-in-handler))      
    (set-event obj "focusin"
	       (lambda (data)
		 (declare (ignore data))
		 (funcall on-focus-in)))))

;;;;;;;;;;;;;;;;;;;;;;
;; set-on-focus-out ;;
;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-focus-out (clog-obj on-focus-out-handler)
  (:documentation "Set the ON-FOCUS-OUT-HANDLER for CLOG-OBJ.
If ON-FOCUS-OUT-HANDLER is nil unbind the event."))

(defmethod set-on-focus-out ((obj clog-obj) on-focus-out-handler)
  (let ((on-focus-out on-focus-out-handler))      
    (set-event obj "focusout"
	       (lambda (data)
		 (declare (ignore data))
		 (funcall on-focus-out)))))

;;;;;;;;;;;;;;;;;;
;; set-on-reset ;;
;;;;;;;;;;;;;;;;;;

(defgeneric set-on-reset (clog-obj on-reset-handler)
  (:documentation "Set the ON-RESET-HANDLER for CLOG-OBJ. If ON-RESET-HANDLER
is nil unbind the event. This event is activated by using reset on a form. If
this even is bound, you must call the form reset manually."))

(defmethod set-on-reset ((obj clog-obj) on-reset-handler)
  (let ((on-reset on-reset-handler))      
    (set-event obj "reset"
	       (lambda (data)
		 (declare (ignore data))
		 (funcall on-reset)))))

;;;;;;;;;;;;;;;;;;;
;; set-on-search ;;
;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-search (clog-obj on-search-handler)
  (:documentation "Set the ON-SEARCH-HANDLER for CLOG-OBJ. If ON-SEARCH-HANDLER
is nil unbind the event."))

(defmethod set-on-search ((obj clog-obj) on-search-handler)
  (let ((on-search on-search-handler))      
    (set-event obj "search"
	       (lambda (data)
		 (declare (ignore data))
		 (funcall on-search)))))

;;;;;;;;;;;;;;;;;;;
;; set-on-select ;;
;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-select (clog-obj on-select-handler)
  (:documentation "Set the ON-SELECT-HANDLER for CLOG-OBJ. If ON-SELECT-HANDLER
is nil unbind the event.  This event is activated by using submit on a form. If
this even is bound, you must call the form submit manually."))

(defmethod set-on-select ((obj clog-obj) on-select-handler)
  (let ((on-select on-select-handler))      
    (set-event obj "select"
	       (lambda (data)
		 (declare (ignore data))
		 (funcall on-select)))))

;;;;;;;;;;;;;;;;;;;
;; set-on-submit ;;
;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-submit (clog-obj on-submit-handler)
  (:documentation "Set the ON-SUBMIT-HANDLER for CLOG-OBJ. If ON-SUBMIT-HANDLER
is nil unbind the event."))

(defmethod set-on-submit ((obj clog-obj) on-submit-handler)
  (let ((on-submit on-submit-handler))      
    (set-event obj "submit"
	       (lambda (data)
		 (declare (ignore data))
		 (funcall on-submit)))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; set-on-context-menu ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-context-menu (clog-obj on-context-menu-handler)
  (:documentation "Set the ON-CONTEXT-MENU-HANDLER for CLOG-OBJ. If
ON-CONTEXT-MENU-HANDLER is nil unbind the event. Setting
on-mouse-right-click will replace this handler."))

(defmethod set-on-context-menu ((obj clog-obj) on-context-menu-handler)
  (let ((on-context-menu on-context-menu-handler))      
    (set-event obj "contextmenu"
	       (lambda (data)
		 (declare (ignore data))
		 (funcall on-context-menu)))))

;;;;;;;;;;;;;;;;;;
;; set-on-click ;;
;;;;;;;;;;;;;;;;;;

(defgeneric set-on-click (clog-obj on-click-handler)
  (:documentation "Set the ON-CLICK-HANDLER for CLOG-OBJ. If ON-CLICK-HANDLER
is nil unbind the event. Setting this event will replace an on-mouse click if
set."))

(defmethod set-on-click ((obj clog-obj) on-click-handler)
  (let ((on-click on-click-handler))      
    (set-event obj "click"
	       (lambda (data)
		 (declare (ignore data))
		 (funcall on-click)))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; set-on-double-click ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-double-click (clog-obj on-double-click-handler)
  (:documentation "Set the ON-DOUBLE-CLICK-HANDLER for CLOG-OBJ. If
ON-DOUBLE-CLICK-HANDLER is nil unbind the event. Setting the
on-mouse-double-click event will replace this handler."))

(defmethod set-on-double-click ((obj clog-obj) on-double-click-handler)
  (let ((on-double-click on-double-click-handler))      
    (set-event obj "dblclick"
	       (lambda (data)
		 (declare (ignore data))
		 (funcall on-double-click)))))

;;;;;;;;;;;;;;;;;;;;;;;;
;; set-on-mouse-click ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-mouse-click (clog-obj on-mouse-click-handler)
  (:documentation "Set the ON-MOUSE-CLICK-HANDLER for CLOG-OBJ. If
ON-MOUSE-CLICK-HANDLER is nil unbind the event. Setting this event will replace
on an on-click event."))

(defmethod set-on-mouse-click ((obj clog-obj) on-mouse-click-handler)
  (let ((on-mouse-click on-mouse-click-handler))      
    (set-event obj "click"
	       (lambda (data)
		 (funcall on-mouse-click (parse-mouse-event data)))
	       :call-back-script mouse-event-script)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set-on-mouse-double-click ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-mouse-double-click (clog-obj on-mouse-double-click-handler)
  (:documentation "Set the ON-MOUSE-DOUBLE-CLICK-HANDLER for CLOG-OBJ. If
ON-MOUSE-DOUBLE-CLICK-HANDLER is nil unbind the event. Setting this event will
replace on an on-double-click event."))

(defmethod set-on-mouse-double-click ((obj clog-obj) on-mouse-double-click-handler)
  (let ((on-mouse-double-click on-mouse-double-click-handler))      
    (set-event obj "dblclick"
	       (lambda (data)
		 (funcall on-mouse-double-click (parse-mouse-event data)))
	       :call-back-script mouse-event-script)))	      

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set-on-mouse-right-click ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-mouse-right-click (clog-obj on-mouse-right-click-handler)
  (:documentation "Set the ON-MOUSE-RIGHT-CLICK-HANDLER for CLOG-OBJ. If
ON-MOUSE-RIGHT-CLICK-HANDLER is nil unbind the event. Setting this event will
replace on an on-context-menu event."))

(defmethod set-on-mouse-right-click ((obj clog-obj) on-mouse-right-click-handler)
  (let ((on-mouse-right-click on-mouse-right-click-handler))      
    (set-event obj "contextmenu"
	       (lambda (data)
		 (funcall on-mouse-right-click (parse-mouse-event data)))
	       :call-back-script mouse-event-script)))

;;;;;;;;;;;;;;;;;;;;;;;;
;; set-on-mouse-enter ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-mouse-enter (clog-obj on-mouse-enter-handler)
  (:documentation "Set the ON-MOUSE-ENTER-HANDLER for CLOG-OBJ. If ON-MOUSE-ENTER-HANDLER
is nil unbind the event."))

(defmethod set-on-mouse-enter ((obj clog-obj) on-mouse-enter-handler)
  (let ((on-mouse-enter on-mouse-enter-handler))      
    (set-event obj "mouseenter"
	       (lambda (data)
		 (declare (ignore data))
		 (funcall on-mouse-enter)))))

;;;;;;;;;;;;;;;;;;;;;;;;
;; set-on-mouse-leave ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-mouse-leave (clog-obj on-mouse-leave-handler)
  (:documentation "Set the ON-MOUSE-LEAVE-HANDLER for CLOG-OBJ. If ON-MOUSE-LEAVE-HANDLER
is nil unbind the event."))

(defmethod set-on-mouse-leave ((obj clog-obj) on-mouse-leave-handler)
  (let ((on-mouse-leave on-mouse-leave-handler))      
    (set-event obj "mouseleave"
	       (lambda (data)
		 (declare (ignore data))
		 (funcall on-mouse-leave)))))

;;;;;;;;;;;;;;;;;;;;;;;
;; set-on-mouse-over ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-mouse-over (clog-obj on-mouse-over-handler)
  (:documentation "Set the ON-MOUSE-OVER-HANDLER for CLOG-OBJ. If ON-MOUSE-OVER-HANDLER
is nil unbind the event."))

(defmethod set-on-mouse-over ((obj clog-obj) on-mouse-over-handler)
  (let ((on-mouse-over on-mouse-over-handler))      
    (set-event obj "mouseover"
	       (lambda (data)
		 (declare (ignore data))
		 (funcall on-mouse-over)))))

;;;;;;;;;;;;;;;;;;;;;;
;; set-on-mouse-out ;;
;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-mouse-out (clog-obj on-mouse-out-handler)
  (:documentation "Set the ON-MOUSE-OUT-HANDLER for CLOG-OBJ. If ON-MOUSE-OUT-HANDLER
is nil unbind the event."))

(defmethod set-on-mouse-out ((obj clog-obj) on-mouse-out-handler)
  (let ((on-mouse-out on-mouse-out-handler))      
    (set-event obj "mouseout"
	       (lambda (data)
		 (declare (ignore data))
		 (funcall on-mouse-out)))))

;;;;;;;;;;;;;;;;;;;;;;;
;; set-on-mouse-down ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-mouse-down (clog-obj on-mouse-down-handler)
  (:documentation "Set the ON-MOUSE-DOWN-HANDLER for CLOG-OBJ. If
ON-MOUSE-DOWN-HANDLER is nil unbind the event."))

(defmethod set-on-mouse-down ((obj clog-obj) on-mouse-down-handler)
  (let ((on-mouse-down on-mouse-down-handler))      
    (set-event obj "mousedown"
	       (lambda (data)
		 (funcall on-mouse-down (parse-mouse-event data)))
	       :call-back-script mouse-event-script)))

;;;;;;;;;;;;;;;;;;;;;
;; set-on-mouse-up ;;
;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-mouse-up (clog-obj on-mouse-up-handler)
  (:documentation "Set the ON-MOUSE-UP-HANDLER for CLOG-OBJ. If
ON-MOUSE-UP-HANDLER is nil unbind the event."))

(defmethod set-on-mouse-up ((obj clog-obj) on-mouse-up-handler)
  (let ((on-mouse-up on-mouse-up-handler))      
    (set-event obj "mouseup"
	       (lambda (data)
		 (funcall on-mouse-up (parse-mouse-event data)))
	       :call-back-script mouse-event-script)))	       

;;;;;;;;;;;;;;;;;;;;;;;
;; set-on-mouse-move ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-mouse-move (clog-obj on-mouse-move-handler)
  (:documentation "Set the ON-MOUSE-MOVE-HANDLER for CLOG-OBJ. If
ON-MOUSE-MOVE-HANDLER is nil unbind the event."))

(defmethod set-on-mouse-move ((obj clog-obj) on-mouse-move-handler)
  (let ((on-mouse-move on-mouse-move-handler))      
    (set-event obj "mousemove"
	       (lambda (data)
		 (funcall on-mouse-move (parse-mouse-event data)))
	       :call-back-script mouse-event-script)))	       

;;;;;;;;;;;;;;;;;;;;;;
;; set-on-character ;;
;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-character (clog-obj on-character-handler)
  (:documentation "Set the ON-CHARACTER-HANDLER for CLOG-OBJ. If
ON-CHARACTER-HANDLER is nil unbind the event. Setting this event
will replace a on-key-press"))

(defmethod set-on-character ((obj clog-obj) on-character-handler)
  (let ((on-character on-character-handler))      
    (set-event obj "keypress"
	       (lambda (data)
		 (let ((f (parse-keyboard-event data)))
		   (funcall on-character (code-char (getf f ':char-code)))))
	       :call-back-script keyboard-event-script)))

;;;;;;;;;;;;;;;;;;;;;
;; set-on-key-down ;;
;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-key-down (clog-obj on-key-down-handler)
  (:documentation "Set the ON-KEY-DOWN-HANDLER for CLOG-OBJ. If
ON-KEY-DOWN-HANDLER is nil unbind the event."))

(defmethod set-on-key-down ((obj clog-obj) on-key-down-handler)
  (let ((on-key-down on-key-down-handler))      
    (set-event obj "keydown"
	       (lambda (data)
		 (funcall on-key-down (parse-keyboard-event data)))
	       :call-back-script keyboard-event-script)))	       

;;;;;;;;;;;;;;;;;;;
;; set-on-key-up ;;
;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-key-up (clog-obj on-key-up-handler)
  (:documentation "Set the ON-KEY-UP-HANDLER for CLOG-OBJ. If
ON-KEY-UP-HANDLER is nil unbind the event."))

(defmethod set-on-key-up ((obj clog-obj) on-key-up-handler)
  (let ((on-key-up on-key-up-handler))      
    (set-event obj "keyup"
	       (lambda (data)
		 (funcall on-key-up (parse-keyboard-event data)))
	       :call-back-script keyboard-event-script)))	       

;;;;;;;;;;;;;;;;;;;;;;
;; set-on-key-press ;;
;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-key-press (clog-obj on-key-press-handler)
  (:documentation "Set the ON-KEY-PRESS-HANDLER for CLOG-OBJ. If
ON-KEY-PRESS-HANDLER is nil unbind the event."))

(defmethod set-on-key-press ((obj clog-obj) on-key-press-handler)
  (let ((on-key-press on-key-press-handler))      
    (set-event obj "keypress"
	       (lambda (data)
		 (funcall on-key-press (parse-keyboard-event data)))
	       :call-back-script keyboard-event-script)))	       

;;;;;;;;;;;;;;;;;
;; set-on-copy ;;
;;;;;;;;;;;;;;;;;

(defgeneric set-on-copy (clog-obj on-copy-handler)
  (:documentation "Set the ON-COPY-HANDLER for CLOG-OBJ. If ON-COPY-HANDLER
is nil unbind the event."))

(defmethod set-on-copy ((obj clog-obj) on-copy-handler)
  (let ((on-copy on-copy-handler))      
    (set-event obj "copy"
	       (lambda (data)
		 (declare (ignore data))
		 (funcall on-copy)))))

;;;;;;;;;;;;;;;;
;; set-on-cut ;;
;;;;;;;;;;;;;;;;

(defgeneric set-on-cut (clog-obj on-cut-handler)
  (:documentation "Set the ON-CUT-HANDLER for CLOG-OBJ. If ON-CUT-HANDLER
is nil unbind the event."))

(defmethod set-on-cut ((obj clog-obj) on-cut-handler)
  (let ((on-cut on-cut-handler))      
    (set-event obj "cut"
	       (lambda (data)
		 (declare (ignore data))
		 (funcall on-cut)))))

;;;;;;;;;;;;;;;;;;
;; set-on-paste ;;
;;;;;;;;;;;;;;;;;;

(defgeneric set-on-paste (clog-obj on-paste-handler)
  (:documentation "Set the ON-PASTE-HANDLER for CLOG-OBJ. If ON-PASTE-HANDLER
is nil unbind the event."))

(defmethod set-on-paste ((obj clog-obj) on-paste-handler)
  (let ((on-paste on-paste-handler))      
    (set-event obj "paste"
	       (lambda (data)
		 (declare (ignore data))
		 (funcall on-paste)))))
