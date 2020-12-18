;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG - The Common Lisp Omnificent GUI                                 ;;;;
;;;; (c) 2020-2021 David Botton                                            ;;;;
;;;; License BSD 3 Clause                                                  ;;;;
;;;;                                                                       ;;;;
;;;; clog.lisp                                                             ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exports - clog
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(mgl-pax:define-package :clog
  (:documentation "The Common List Omnificent GUI - Parent package")  
  (:local-nicknames (:cc :clog-connection))
  (:use #:cl #:mgl-pax))

(in-package :clog)

(defsection @clog-manual (:title "The CLOG manual")
  "The Common Lisp Omnificient GUI, CLOG for short, uses web technology
to produce graphical user interfaces for applications locally or
remotely. The CLOG package starts up the connectivity to the browser
or other websocket client (often a browser embedded in a native
application."

  (clog asdf:system)

  (@clog-system section)
  (@clog-objs section))

(defsection @clog-system (:title "CLOG System")

  "CLOG Startup and Shutdown"
  (initialize function)
  (shutdown   function)

  "CLOG Low Level binding functions"
  (attach           function)
  (create-with-html function)
    
  "CLOG utilities"
  (open-browser function))


(defsection @clog-objs (:title "CLOG Objects")
  "CLOG-Obj"
  (clog-obj class)

  "CLOG-Obj - General"
  (property     generic-function)
;;  ((setf property) generic-function)

  (style        generic-function)
;;  (set-style generic-function)
  
  (attribute     generic-function)
;;  (set-attribute generic-function)
  
  "CLOG-Obj - Low Level Creation"
  (create-child    generic-function)
  (attach-as-child generic-function)

  "CLOG-Obj - Placement"
  (place-after            generic-function)
  (place-before           generic-function)
  (place-inside-top-of    generic-function)
  (place-inside-bottom-of generic-function))
  

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
;; make-clog-obj ;;
;;;;;;;;;;;;;;;;;;;

(defun make-clog-obj (connection-id html-id)
  "Construct a new clog-obj. (Private)"
  (make-instance 'clog-obj :connection-id connection-id
			   :html-id html-id))

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

;;;;;;;;;;;;;;
;; property ;;
;;;;;;;;;;;;;;

(defgeneric property (clog-obj property-name)
  (:documentation "Return PROPERTY-NAME's value for CLOG-OBJ"))

(defmethod property ((obj clog-obj) property-name)
  (jquery-query obj (format nil "prop('~A')" property-name)))

;; (defgeneric (setf property) (value obj property-name)
;;  (:documentation "Set PROPERTY-NAME to VALUE for CLOG-OBJ"))

(defmethod (setf property) (value (obj clog-obj) property-name)
  (jquery-execute obj (format nil "prop('~A','~A')" property-name value)))

;;;;;;;;;;;
;; style ;;
;;;;;;;;;;;

(defgeneric style (clog-obj style-name)
  (:documentation "Return STYLE-NAME's value for CLOG-OBJ"))

(defmethod style ((obj clog-obj) style-name )
  (jquery-query obj (format nil "css('~A')" style-name)))

;;(defgeneric set-style (clog-obj style-name value)
;;  (:documentation "Set STYLE-NAME to VALUE for CLOG-OBJ"))

(defmethod (setf style) (value (obj clog-obj) style-name)
  (jquery-execute obj (format nil "css('~A','~A')" style-name value)))

;;;;;;;;;;;;;;;
;; attribute ;;
;;;;;;;;;;;;;;;

(defgeneric attribute (clog-obj attribute-name)
  (:documentation "Return ATTRIBUTE-NAME's value for CLOG-OBJ"))

(defmethod attribute ((obj clog-obj) attribute-name)
  (jquery-query obj (format nil "attr('~A')" attribute-name)))

;;(defgeneric set-attribute (clog-obj attribute-name value)
;;  (:documentation "Set ATTRIBUTE-NAME to VALUE for CLOG-OBJ"))

(defmethod (setf attribute) (value (obj clog-obj) attribute-name)
  (jquery-execute obj (format nil "attr('~A','~A')" attribute-name value)))

;;;;;;;;;;;;;;;;;;
;; create-child ;;
;;;;;;;;;;;;;;;;;;

(defgeneric create-child (clog-obj html &key auto-place)
  (:documentation "Create a new CLOG-OBJ from HTML element as child of OBJ and if :AUTO-PLACE (default t)
place-inside-bottom-of OBJ"))

(defmethod create-child ((obj clog-obj) html &key (auto-place t))
  (let ((child (create-with-html (connection-id obj) html)))
    (if auto-place
	(place-inside-bottom-of obj child)
	child)))

;;;;;;;;;;;;;;;;;;;;;
;; attach-as-child ;;
;;;;;;;;;;;;;;;;;;;;;

(defgeneric attach-as-child (clog-obj html-id)
  (:documentation "Create a new CLOG-OBJ and attach an existing element with HTML-ID. The
HTML-ID must be unique."))

(defmethod attach-as-child ((obj clog-obj) html-id)
  (cc:execute (connection-id obj) (format nil "clog['~A']=$('#~A')" html-id html-id))
  (make-clog-obj (connection-id obj) html-id))

;;;;;;;;;;;;;;;;;
;; place-after ;;
;;;;;;;;;;;;;;;;;

(defgeneric place-after (clog-obj next-obj)
  (:documentation "Places NEXT-OBJ after CLOG-OBJ in DOM"))

(defmethod place-after ((obj clog-obj) next-obj)
  (jquery-execute obj (format nil "after(~A)" (script-id next-obj)))
  next-obj)

;;;;;;;;;;;;;;;;;;
;; place-before ;;
;;;;;;;;;;;;;;;;;;

(defgeneric place-before (clog-obj next-obj)
  (:documentation "Places NEXT-OBJ before CLOG-OBJ in DOM"))

(defmethod place-before ((obj clog-obj) next-obj)
  (jquery-execute obj (format nil "before(~A)" (script-id next-obj)))
  next-obj)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; place-inside-top-of ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric place-inside-top-of (clog-obj next-obj)
  (:documentation "Places NEXT-OBJ inside top of CLOG-OBJ in DOM"))

(defmethod place-inside-top-of ((obj clog-obj) next-obj)
  (jquery-execute obj (format nil "prepend(~A)" (script-id next-obj)))
  next-obj)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; place-inside-bottom-of ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric place-inside-bottom-of (clog-obj next-obj)
  (:documentation "Places NEXT-OBJ inside bottom of CLOG-OBJ in DOM"))

(defmethod place-inside-bottom-of ((obj clog-obj) next-obj)
  (jquery-execute obj (format nil "append(~A)" (script-id next-obj)))
  next-obj)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;
;; initialize ;;
;;;;;;;;;;;;;;;;

(defvar *on-new-window* nil "Store the on-new-window handler")

(defun on-connect (id)
  (when cc:*verbose-output*
    (format t "Start new window handler on connection-id - ~A" id))
  (let ((body (make-clog-obj id 0)))
    (funcall *on-new-window* body)))
    
(defun initialize (on-new-window
		   &key
		     (host           "0.0.0.0")
		     (port           8080)
		     (boot-file      "/boot.html")
		     (static-root    #P"./static-files/"))
  "Inititalze CLOG on a socket using HOST and PORT to serve BOOT-FILE as 
the default route to establish web-socket connections and static files
located at STATIC-ROOT."
  (setf *on-new-window* on-new-window)
  
  (cc:initialize #'on-connect
		 :host host
		 :port port
		 :boot-file boot-file
		 :static-root static-root))

;;;;;;;;;;;;;;
;; shutdown ;;
;;;;;;;;;;;;;;

(defun shutdown ()
  "Shutdown CLOG."
  (cc:shutdown-clog))

;;;;;;;;;;;;
;; attach ;;
;;;;;;;;;;;;

(defun attach (connection-id html-id)
  "Create a new clog-obj and attach an existing element with HTML-ID on
CONNECTION-ID to it and then return it. The HTML-ID must be unique."
  (cc:execute connection-id (format nil "clog['~A']=$('#~A')" html-id html-id))
  (make-clog-obj connection-id html-id))

;;;;;;;;;;;;;;;;;;;;;;
;; create-with-html ;;
;;;;;;;;;;;;;;;;;;;;;;

(defun create-with-html (connection-id html)
  "Create a new clog-obj and attach it to HTML on CONNECTION-ID. There must be
a single outer block that will be set to an internal id. The returned clog-obj
requires placement or will not be visible, ie. place-after, etc"
  (let ((web-id (cc:generate-id)))
    (cc:execute
     connection-id
     (format nil "clog['~A']=$(\"~A\"); clog['~A'].first().prop('id','~A')"
	     web-id html web-id web-id))
    (make-clog-obj connection-id web-id)))

;;;;;;;;;;;;;;;;;;
;; open-browser ;;
;;;;;;;;;;;;;;;;;;

(defun open-browser (&key (url "http://127.0.0.1:8080"))
  "Open a web browser to URL."
  (trivial-open-browser:open-browser url))
