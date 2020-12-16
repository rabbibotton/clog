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

  (@clog-top-level section))

(defsection @clog-top-level (:title "CLOG Top level")

  "CLOG Startup and Shutdown"

  (initialize      function)
  (shutdown        function)
  (set-on-connect  function)  

  (clog class)
  
  "CLOG Low Level bindings"

  (attach           function)
  (create-with-html function)
  (place-after      function)
  
  "CLOG utilities"

  (alert-box function)
  
  (escape-string function)  
  (open-browser  function))


(defclass clog ()
  ((connection-id
    :accessor connection-id
    :initarg :connection-id)
   (html-id
    :accessor html-id
    :initarg :html-id)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;
;; initialize ;;
;;;;;;;;;;;;;;;;

(defun initialize (on-connect-handler
		   &key
		     (host           "0.0.0.0")
		     (port           8080)
		     (boot-file      "/boot.html")
		     (static-root    #P"./static-files/"))
  "Inititalze CLOG on a socket using HOST and PORT to serve BOOT-FILE as 
the default route to establish web-socket connections and static files
located at STATIC-ROOT."
  (cc:initialize on-connect-handler
		 :host host
		 :port port
		 :boot-file boot-file
		 :static-root static-root))

;;;;;;;;;;;;;;
;; shutdown ;;
;;;;;;;;;;;;;;

(defun shutdown ()
  "Shutdown CLOG."
  (cc:shutdown))

;;;;;;;;;;;;
;; attach ;;
;;;;;;;;;;;;

(defun attach (connection-id id)
  "Create a new clog object and attach an existing element with HTML-ID on
CONNECTION-ID to it. The HTML-ID must be unique."
  (make-instance 'clog :connection-id connection-id :html-id id))

;;;;;;;;;;;;;;;;;;;;;;
;; create-with-html ;;
;;;;;;;;;;;;;;;;;;;;;;

(defun create-with-html (connection-id html)
  "Create a new clog object and attach it to HTML on CONNECTION-ID. There must be
a single outer block that will be set to an internal id"
  (let ((web-id (cc:generate-id)))
    (cc:execute
     connection-id (format nil "clog['~A']=$(\"~A\"); clog['~A'].first().prop('id','~A');"
			   web-id html web-id web-id))
    (attach connection-id web-id)))

;;;;;;;;;;;;;;;;;
;; place-after ;;
;;;;;;;;;;;;;;;;;

(defun place-after (parent child)
  (let ((jq (if parent
		(format nil "$(clog['~A'])" (html-id parent))
		(format nil "$('body')"))))
    (cc:execute (connection-id child)
		(format nil "~A.after(clog['~A'])" jq (html-id child))))
  child)

;;;;;;;;;;;;;;;
;; alert-box ;;
;;;;;;;;;;;;;;;

(defun alert-box (id message)
  (cc:execute
   id (format nil "alert('~A');" (cc:escape-string message))))

;;;;;;;;;;;;;;;;;;
;; open-browser ;;
;;;;;;;;;;;;;;;;;;

(defun open-browser (&key (url "http://127.0.0.1:8080"))
  "Open a web browser to URL."
  (trivial-open-browser:open-browser url))
