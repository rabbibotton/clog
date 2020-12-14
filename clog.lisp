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
  (:use #:cl #:mgl-pax))

(in-package :clog)

(defsection @clog-manual (:title "The CLOG manual")
  "The Common Lisp Omnificient GUI, CLOG for short, uses web technology
to produce graphical user interfaces for applications locally or
remotely. The CLOG package starts up the connectivity to the browser
or other websocket client (often a browser embedded in a native
application."

  (clog asdf:system)

  (@clog-top-level  section))

(defsection @clog-top-level (:title "CLOG Top level")
  "CLOG system startup and shutdown"

  (*verbose-output* variable)
  
  (initialize      function)
  (shutdown        function)
  (set-on-connect  function)
  
  "CLOG utilities"
  
  (open-browser function))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *verbose-output* t "Verbose server output (default true)")

(defvar *app*                nil "Clack 'app' middle-ware")
(defvar *client-handler*     nil "Clack 'handler' for socket traffic")
(defvar *on-connect-handler* nil "New connection event handler.")

(defvar *new-id* 0 "Connection IDs")

(defvar *connections*    (make-hash-table) "Connections to IDs")
(defvar *connection-ids* (make-hash-table) "IDs to connections")

(defvar *connection-lock* (bordeaux-threads:make-lock)
  "Protect the connection hash tables")
(defvar *queries-lock*    (bordeaux-threads:make-lock)
  "Protect query hash tables")

(defvar *queries*        (make-hash-table) "Query ID to Answers")
(defvar *queries-sems*   (make-hash-table) "Query ID to semiphores")
(defvar *query-time-out* 3 "Number of seconds to timeout waiting for a query")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; generate-connection-id ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun generate-connection-id ()
  "Generate unique ids for use in connections and sripts. (Private)"
  (incf *new-id*))

;;;;;;;;;;;;;;;;;;;;
;; get-connection ;;
;;;;;;;;;;;;;;;;;;;;

(defun get-connection (connection-id)
  "Return the connection associated with CONNECITION-ID. (Private)"
  (gethash connection-id *connection-ids*))

;;;;;;;;;;;;;;;;
;; prep-query ;;
;;;;;;;;;;;;;;;;

(defun prep-query (id default-answer)
  "Setup up a query to be received from a script identified by ID an returning
with DEFAULT-ANSWER in case of a time out. (Private)"
  (bordeaux-threads:with-lock-held (*queries-lock*)
    (setf (gethash id *queries-sems*) (bordeaux-threads:make-semaphore))
    (setf (gethash id *queries*) default-answer)))

;;;;;;;;;;;;;;;;;;;;;
;; wait-for-answer ;;
;;;;;;;;;;;;;;;;;;;;;

(defun wait-for-answer (id &key (timeout *query-time-out*))
  "Block after prep-query and sending the query script with ID and TIMEOUT with
the default answer. (Private)"
  (bordeaux-threads:wait-on-semaphore (gethash id *queries-sems*) :timeout timeout)
  (let ((answer (gethash id *queries*)))
    (bordeaux-threads:with-lock-held (*queries-lock*)
      (remhash id *queries*)
      (remhash id *queries-sems*))
    answer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; handle-new-connection ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun handle-new-connection (connection id)
  (cond (id
	 (when *verbose-output*
	   (format t "Reconnection id - ~A to ~A~%" id connection))
	 (bordeaux-threads:with-lock-held (*connection-lock*)
	   (setf (gethash id *connection-ids*) connection)
	   (setf (gethash connection *connections*) id)))
	(t
	 (setf id (generate-connection-id))
	 (bordeaux-threads:with-lock-held (*connection-lock*)
	   (setf (gethash connection *connections*) id)
	   (setf (gethash id *connection-ids*) connection))
	 (when *verbose-output*
	   (format t "New connection id - ~A - ~A~%" id connection))
	 (websocket-driver:send connection
				(format nil "clog['connection_id']=~A" id))
	 (bordeaux-threads:make-thread
	  (lambda ()
	    (funcall *on-connect-handler* id))))))

;;;;;;;;;;;;;;;;;;;;
;; handle-message ;;
;;;;;;;;;;;;;;;;;;;;

(defun handle-message (connection message)
  (let ((id (gethash connection *connections*))
	(ml (ppcre:split ":" message :limit 2)))
    (cond ((equal (car ml) "0")
	   (when *verbose-output*
	     (format t "~A Ping ~A~%" id (car ml))))
	  (t
	   (when *verbose-output*
	     (format t "~A ~A = ~A~%" id (car ml) (cadr ml)))
	   (bordeaux-threads:with-lock-held (*queries-lock*)
	     (setf (gethash (parse-integer (car ml)) *queries*) (cadr ml)))
	   (bordeaux-threads:signal-semaphore
	    (gethash (parse-integer (car ml)) *queries-sems*))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; handle-close-connection ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun handle-close-connection (connection)
  (let ((id (gethash connection *connections*)))
    (when id
      (when *verbose-output*
	(format t "Connection id ~A has closed. ~A~%" id connection))
      (bordeaux-threads:with-lock-held (*connection-lock*)
	(remhash id *connection-ids*)
	(remhash connection *connections*)))))

;;;;;;;;;;;;;;;;;
;; clog-server ;;
;;;;;;;;;;;;;;;;;

(defun clog-server (env)
  (let ((ws (websocket-driver:make-server env)))
    (websocket-driver:on :open ws
                         (lambda ()
			   (let ((id (getf env :query-string)))
			     (when (typep id 'string)
			       (setf id (parse-integer id)))
			     (handle-new-connection ws id))))
    
    (websocket-driver:on :message ws
                         (lambda (msg) (handle-message ws msg)))

    (websocket-driver:on :close ws
                         (lambda (&key code reason)
                           (declare (ignore code reason))
                           (handle-close-connection ws)))
    (lambda (responder)
      (declare (ignore responder))
      (websocket-driver:start-connection ws))))

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
  (set-on-connect on-connect-handler)  
  (setf *app*
	(lack:builder
	 (:static :path (lambda (path)
			  (cond ((ppcre:scan "^(?:/clog$)" path) nil)
				((equal path "/") boot-file)
				(t path)))
		  :root static-root)
	 (lambda (env)
	   (clog-server env))))
  (setf *client-handler* (clack:clackup *app* :address host :port port))
  (when *verbose-output*
    (format t "HTTP listening on : ~A:~A~%" host port)
    (format t "HTML Root         : ~A~%"    static-root)
    (format t "Boot file default : ~A~%"    boot-file)))

;;;;;;;;;;;;;;
;; shutdown ;;
;;;;;;;;;;;;;;

(defun shutdown ()
  "Shutdown CLOG."
  (clack:stop *client-handler*)
  (bordeaux-threads:with-lock-held (*connection-lock*)
    (clrhash *connections*)
    (clrhash *connection-ids*))
  (setf *app* nil)
  (setf *client-handler* nil))

;;;;;;;;;;;;;;;;;;;;
;; set-on-connect ;;
;;;;;;;;;;;;;;;;;;;;

(defun set-on-connect (on-connect-handler)
  "Change the ON-CONNECTION-HANDLER set during Initialize."
  (setf *on-connect-handler* on-connect-handler))

;;;;;;;;;;;;;;;;;;
;; open-browser ;;
;;;;;;;;;;;;;;;;;;

(defun open-browser (&key (url "http://127.0.0.1:8080"))
  "Open a web browser to URL."
  (trivial-open-browser:open-browser url))
