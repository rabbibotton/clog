;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG - The Common Lisp Omnificent GUI                                 ;;;;
;;;; (c) 2020-2021 David Botton                                            ;;;;
;;;; License BSD 3 Clause                                                  ;;;;
;;;;                                                                       ;;;;
;;;; clog-connection.lisp                                                  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; clog-connection.lisp contains the clog-connection package that handles
;;; the low level connectivity between lisp and the browser.
;;;
;;; clog <-> clog-connection <->
;;;   clack <-> Hunchentoot (by default) <->
;;;     internet/localhost <-> browser
;;;
;;; Normally users of clog do not interact with the exports of clog-connection
;;; package. Shutdown and startup of clog are found in clog-system.lisp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exports - clog-connection
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(mgl-pax:define-package :clog-connection
  (:documentation "The Common List Omnificent GUI - Connection")
  (:use #:cl #:mgl-pax))

(in-package :clog-connection)

(defsection @clog-connection (:title "CLOG Connection")
  "Low level connectivity to the web client and boot file 
script."

  "CLOG system startup and shutdown"

  (*verbose-output* variable)

  (initialize          function)
  (shutdown-clog       function)
  (set-on-connect      function)
  (set-clog-path       function)
  (get-connection-data function)

  "CLOG system utilities"
  
  (escape-string function)
  
  "CLOG connections"
  
  (execute           function)
  (query             function)
  (validp            function)
  (cclose            function)
  (shutdown          function)
  (put               function)
  (put-line          function)
  (new-line          function)
  (alert-box         function)
  (generate-id       function)
  (debug-mode        function)
  (set-html-on-close function))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implemetation - clog-connection
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *verbose-output* nil "Verbose server output (default false)")

(defvar *app*            nil "Clack 'app' middle-ware")
(defvar *client-handler* nil "Clack 'handler' for socket traffic")

(defvar *on-connect-handler* nil "New connection event handler.")

(defvar *connections*     (make-hash-table) "Connections to IDs")
(defvar *connection-ids*  (make-hash-table) "IDs to connections")
(defvar *connection-data* (make-hash-table) "Connection based data")
(defvar *connection-lock* (bordeaux-threads:make-lock)
  "Protect the connection hash tables")

(defvar *new-id*   0 "Last issued connection or script IDs")
(defvar *id-lock*  (bordeaux-threads:make-lock)
  "Protect new-id variable.")

(defvar *queries*        (make-hash-table) "Query ID to Answers")
(defvar *queries-sems*   (make-hash-table) "Query ID to semiphores")
(defvar *queries-lock*   (bordeaux-threads:make-lock)
  "Protect query hash tables")
(defvar *query-time-out* 3 "Number of seconds to timeout waiting for a query")

(defvar *url-to-boot-file* (make-hash-table :test 'equalp) "URL to boot-file")

;;;;;;;;;;;;;;;;;
;; generate-id ;;
;;;;;;;;;;;;;;;;;

(defun generate-id ()
  "Generate unique ids for use in connections and sripts."
  (bordeaux-threads:with-lock-held (*id-lock*) (incf *new-id*)))

;;;;;;;;;;;;;;;;;;;;
;; get-connection ;;
;;;;;;;;;;;;;;;;;;;;

(defun get-connection (connection-id)
  "Return the connection associated with CONNECITION-ID. (Private)"
  (gethash connection-id *connection-ids*))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; get-connection-data ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-connection-data (connection-id)
  "Return the connecton data associated with the CONNECTION-ID a
hash test: #'equal."
  (gethash connection-id *connection-data*))

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
  (handler-case
      (progn
	(bordeaux-threads:wait-on-semaphore (gethash id *queries-sems*) :timeout timeout)
	(let ((answer (gethash id *queries*)))
	  (bordeaux-threads:with-lock-held (*queries-lock*)
	    (remhash id *queries*)
	    (remhash id *queries-sems*))
	  answer))
    (t (c)
      (format t "Condition caught in wait-for-answer - ~A.~&" c)
      (values 0 c))))    

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; handle-new-connection ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun handle-new-connection (connection id)
  (handler-case
      (cond (id
	   (format t "Reconnection id - ~A to ~A~%" id connection)
	   (bordeaux-threads:with-lock-held (*connection-lock*)
	     (setf (gethash id *connection-ids*) connection)
	     (setf (gethash connection *connections*) id)))
	    (t
	     (setf id (generate-id))
	     (bordeaux-threads:with-lock-held (*connection-lock*)
	       (setf (gethash connection *connections*) id)
	       (setf (gethash id *connection-ids*) connection)
	       (setf (gethash id *connection-data*) (make-hash-table :test #'equal))
	       (setf (gethash "connection-id" (get-connection-data id)) id))
	     (format t "New connection id - ~A - ~A~%" id connection)
	     (websocket-driver:send connection
				  (format nil "clog['connection_id']=~A" id))
	     (bordeaux-threads:make-thread
	      (lambda ()
		(funcall *on-connect-handler* id))
              :name (format nil "CLOG connection ~A"
                            id))))
    (t (c)
      (format t "Condition caught in handle-new-connection - ~A.~&" c)
      (values 0 c))))

;;;;;;;;;;;;;;;;;;;;
;; handle-message ;;
;;;;;;;;;;;;;;;;;;;;

(defun handle-message (connection message)
  (handler-case
      (let ((id (gethash connection *connections*))
	    (ml (ppcre:split ":" message :limit 2)))
	(cond ((equal (first ml) "0")
	       (when *verbose-output*
		 (format t "~A Ping~%" id)))
	      ((equal (first ml) "E")
	       (let* ((em (ppcre:split " " (second ml) :limit 2))
                      (event-id (first em))
                      (data (second em)))
		 (when *verbose-output*
		   (format t "Channel ~A Hook ~A Data ~A~%"
			   id event-id data))
		 (bordeaux-threads:make-thread
		  (lambda ()
		    (let* ((event-hash (get-connection-data id))
			   (event      (when event-hash
					 (gethash event-id event-hash))))
		      (when event
			(funcall event data))))
		  :name (format nil "CLOG event handler ~A"
				event-id))))
	      (t
	       (when *verbose-output*
		 (format t "~A ~A = ~A~%" id (first ml) (second ml)))
	       (bordeaux-threads:with-lock-held (*queries-lock*)
		 (setf (gethash (parse-integer (first ml)) *queries*) (second ml)))
	       (bordeaux-threads:signal-semaphore
		(gethash (parse-integer (first ml)) *queries-sems*)))))
    (t (c)
      (format t "Condition caught in handle-message - ~A.~&" c)
      (values 0 c))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; handle-close-connection ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun handle-close-connection (connection)
  (handler-case
      (let ((id (gethash connection *connections*)))
	(when id
	  (when *verbose-output*
	    (format t "Connection id ~A has closed. ~A~%" id connection))
	  (bordeaux-threads:with-lock-held (*connection-lock*)
	    (remhash id *connection-data*)
	    (remhash id *connection-ids*)
	    (remhash connection *connections*))))
    (t (c)
      (format t "Condition caught in handle-message - ~A.~&" c)
      (values 0 c))))

;;;;;;;;;;;;;;;;;
;; clog-server ;;
;;;;;;;;;;;;;;;;;

(defun clog-server (env)
  (handler-case
      (let ((ws (websocket-driver:make-server env)))
	(websocket-driver:on :open ws
                             (lambda ()
			   (let* ((query (getf env :query-string))
				  (items (when query
					   (quri:url-decode-params query)))
				  (id    (when items
					   (cdr (assoc "r" items
						       :test #'equalp)))))
			     (when (typep id 'string)
			       (setf id (parse-integer id :junk-allowed t)))
			     (handle-new-connection ws id))))
	
	(websocket-driver:on :message ws
                             (lambda (msg) (handle-message ws msg)))
	
	(websocket-driver:on :close ws
                             (lambda (&key code reason)
                               (declare (ignore code reason))
                               (handle-close-connection ws)))
	(lambda (responder)
	  (declare (ignore responder))
	  (websocket-driver:start-connection ws)))
    (t (c)
      (format t "Condition caught in clog-server - ~A.~&" c)
      (values 0 c))))


;;;;;;;;;;;;;;;;
;; initialize ;;
;;;;;;;;;;;;;;;;

(defun initialize (on-connect-handler
		   &key
		     (host             "0.0.0.0")
		     (port             8080)
		     (boot-file        "/boot.html")
		     (static-boot-js   nil)
		     (static-root      #P"./static-files/"))
  "Initialize CLOG on a socket using HOST and PORT to serve BOOT-FILE as 
the default route for '/' to establish web-socket connections and static files
located at STATIC-ROOT. If BOOT-FILE is nil no initial clog-path's will be
setup, use clog-path to add. The on-connect-handler needs to indentify the
path by querying the browser. See PATH-NAME (in CLOG-LOCATION). If
static-boot-js is nil then boot.js is served from the file /js/boot.js
instead of the compiled version."
  (set-on-connect on-connect-handler)
  (when boot-file
    (set-clog-path "/" boot-file))
  (setf *app*
	(lack:builder
	 (lambda (app)
	   (lambda (env)
	     ;; if not static-boot-js use internal compiled boot.js
	     (if (and (eq static-boot-js nil)
		      (equalp (getf env :path-info) "/js/boot.js"))
		 `(200 (:content-type "text/javascript")
		   (,(compiled-boot-js)))
		 (funcall app env))))
	 (lambda (app)
	   (lambda (env)
	     ;; Special handling of "clog paths"
	     (let ((clog-path (gethash (getf env :path-info)
				       *url-to-boot-file*)))
	       (cond (clog-path
		      (let ((file (uiop:subpathname static-root clog-path)))
			(with-open-file (stream file :direction :input
						     :if-does-not-exist nil)
			  (let ((page-data (make-string (file-length stream)))
				(post-data))
			    (read-sequence page-data stream)
			    ;; Check if post method response
			    (when (equal (getf env :content-type)
					 "application/x-www-form-urlencoded")
			      (setf post-data (make-string (getf env :content-length)))
			      (read-sequence post-data (getf env :raw-body)))
      			    `(200 (:content-type "text/html")
				  (,(if post-data
				      (concatenate 'string page-data
				        (format nil "<script>clog['post-data']='~A'</script>"
						post-data))
				      page-data)))))))
		     ;; Pass the handling on to next rule
		     (t (funcall app env))))))
	 (:static :path (lambda (path)
			  ;; Request is static path if not the websocket connection.
			  ;; Websocket url is /clog
			  (cond ((ppcre:scan "^(?:/clog$)" path) nil)
				(t path)))
		  :root static-root)
	 ;; Handle Websocket connection
	 (lambda (env)
	   (clog-server env))))
  (setf *client-handler* (clack:clackup *app* :address host :port port))
  (format t "HTTP listening on    : ~A:~A~%" host port)
  (format t "HTML Root            : ~A~%"    static-root)
  (format t "Boot js source       : ~A~%"    (if static-boot-js
						 "actual file"
						 "compiled in"))
  (format t "Boot file for path / : ~A~%"    boot-file))

;;;;;;;;;;;;;;;;;;;
;; shutdown-clog ;;
;;;;;;;;;;;;;;;;;;;

(defun shutdown-clog ()
  "Shutdown CLOG."
  (clack:stop *client-handler*)
  (bordeaux-threads:with-lock-held (*connection-lock*)
    (clrhash *connection-data*)
    (clrhash *connections*)
    (clrhash *connection-ids*))
  (clrhash *url-to-boot-file*)
  (setf *app* nil)
  (setf *client-handler* nil))

;;;;;;;;;;;;;;;;;;;;
;; set-on-connect ;;
;;;;;;;;;;;;;;;;;;;;

(defun set-on-connect (on-connect-handler)
  "Change the ON-CONNECTION-HANDLER set during Initialize."
  (setf *on-connect-handler* on-connect-handler))

;;;;;;;;;;;;;;;;;;;
;; set-clog-path ;;
;;;;;;;;;;;;;;;;;;;

(defun set-clog-path (path boot-file)
  (if boot-file
      (setf (gethash path *url-to-boot-file*)
	    ;; Make clog-path into a relative path of
	    ;; of site-root.
	    (if (eql (char boot-file 0) #\/)
		(concatenate 'string "." boot-file)
		boot-file))
      (remhash path *url-to-boot-file*)))

;;;;;;;;;;;;;;;;;;;
;; escape-string ;;
;;;;;;;;;;;;;;;;;;;

(defun escape-string (str)
  "Escape STR for sending to browser script."
  (let ((res))
    (setf res (ppcre:regex-replace-all "\\x22" str "\\x22"))
    (setf res (ppcre:regex-replace-all "\\x27" res "\\x27"))
    (setf res (ppcre:regex-replace-all "\\x0A" res "\\x0A"))
    (setf res (ppcre:regex-replace-all "\\x0D" res "\\x0D"))
    res))

;;;;;;;;;;;;;
;; execute ;;
;;;;;;;;;;;;;

(defun execute (connection-id message)
  "Execute SCRIPT on CONNECTION-ID, disregard return value."
  (let ((con (get-connection connection-id)))
    (when con
      (websocket-driver:send con message))))

;;;;;;;;;;;
;; query ;;
;;;;;;;;;;;

(defun query (connection-id script &key (default-answer nil))
  "Execute SCRIPT on CONNECTION-ID, return value. If times out answer
DEFAULT-ANSWER."
  (let ((uid (generate-id)))
    (prep-query uid default-answer)
    (execute connection-id
	     (format nil "ws.send (\"~A:\"+eval(\"~A\"));"
		     uid
		     (escape-string script)))
    (wait-for-answer uid)))

;;;;;;;;;;;;
;; validp ;;
;;;;;;;;;;;;

(defun validp (connection-id)
  "Check if CONNECTION-ID is valid."
  (if (get-connection connection-id)
      t
      nil))

;;;;;;;;;;;;
;; cclose ;;
;;;;;;;;;;;;

(defun cclose (connection-id)
  "Close connection to CONNECTION-ID. The boot file may try to reistablish
 connectivity."
  (execute connection-id "ws.close()"))

;;;;;;;;;;;;;;
;; shutdown ;;
;;;;;;;;;;;;;;

(defun shutdown (connection-id)
  "Shutdown connection to CONNECTION-ID. The boot file may not try to
reistablish connectivity."
  (execute connection-id "Shutdown_ws(event.reason='user')"))

;;;;;;;;;
;; put ;;
;;;;;;;;;

(defun put (connection-id text)
  "Write TEXT to document object of CONNECTION-ID with out new line."
  (execute connection-id
	   (format nil "document.write('~A');" (escape-string text))))

;;;;;;;;;;;;;;
;; put-line ;;
;;;;;;;;;;;;;;

(defun put-line (connection-id text)
  "Write TEXT to document object of CONNECTION-ID with new line and
HTML <br />."
  (execute connection-id
	   (format nil "document.writeln('~A<br />');" (escape-string text))))

;;;;;;;;;;;;;;
;; new-line ;;
;;;;;;;;;;;;;;

(defun new-line (connection-id)
  "Write a new line raw to document object of CONNECTION-ID with a <br />."
  (execute connection-id (format nil "document.writeln('<br />');")))

;;;;;;;;;;;;;;;
;; alert-box ;;
;;;;;;;;;;;;;;;

(defun alert-box (connection-id message)
  "Create an alert box on CONNECTION-ID with MESSAGE"
  (execute connection-id (format nil "alert('~A');" (escape-string message))))

;;;;;;;;;;;;;;;;
;; debug-mode ;;
;;;;;;;;;;;;;;;;

(defun debug-mode (connection-id)
  "Turn on javascript debug mode in the boot.js file"
  (execute connection-id "clog_debug = true"))

;;;;;;;;;;;;;;;;;;;;;;;
;; set-html-on-close ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defun set-html-on-close (connection-id html)
  "Set the client side variable clog['html_on_close'] to replace
the browser contents in case of connection loss."
  (execute connection-id (format nil "clog['html_on_close']='~A'"
				 (escape-string html))))

;;;;;;;;;;;;;;;;;;;;;;
;; compiled-boot-js ;;
;;;;;;;;;;;;;;;;;;;;;;

(defun compiled-boot-js ()
  "Returns a compiled version of current version of boot.js (private)"
"var ws;
var adr;
var clog={};
var pingerid;

if (typeof clog_debug == 'undefined') {
    clog_debug = false;
}

function Ping_ws() {
    if (ws.readyState == 1) {
        ws.send ('0');
    }
}

function Shutdown_ws(event) {
    if (ws != null) {
	ws.onerror = null;
	ws.onclose = null;
	ws.close ();
	ws = null;
    }
    clearInterval (pingerid);
    if (clog['html_on_close'] != '') {
        $(document.body).html(clog['html_on_close']);
    }
}

function Setup_ws() {
    ws.onmessage = function (event) {
        try {
            if (clog_debug == true) {
		console.log ('eval data = ' + event.data);
            }
            eval (event.data);
        } catch (e) {
            console.error (e.message);
        }
    }
    
    ws.onerror = function (event) {
        console.log ('onerror: reconnect');
        ws = null;
        ws = new WebSocket (adr  + '?r=' + clog['connection_id']);
        ws.onopen = function (event) {
            console.log ('onerror: reconnect successful');
            Setup_ws();
        }
        ws.onclose = function (event) {
            console.log ('onerror: reconnect failure');
            Shutdown_ws(event);
        }
    }
    
    ws.onclose = function (event) {
        console.log ('onclose: reconnect');
        ws = null;
        ws = new WebSocket (adr  + '?r=' + clog['connection_id']);
        ws.onopen = function (event) {
            console.log ('onclose: reconnect successful');
            Setup_ws();
        }
        ws.onclose = function (event) {
            console.log ('onclose: reconnect failure');
            Shutdown_ws(event);
        }
    }
}

$( document ).ready(function() {
    var s = document.location.search;
    var tokens;
    var r = /[?&]?([^=]+)=([^&]*)/g;

    clog['body']=document.body;
    clog['head']=document.head;
    clog['documentElement']=document.documentElement;
    clog['window']=window;
    clog['navigator']=navigator;
    clog['document']=window.document;
    clog['location']=window.location;
    
    if (location.protocol == 'https:') {
        adr = 'wss://' + location.hostname;
    } else {
        adr = 'ws://' + location.hostname;
    }
    
    if (location.port != '') { adr = adr + ':' + location.port; }
    adr = adr + '/clog';
    
    try {
        console.log ('connecting to ' + adr);
        ws = new WebSocket (adr);
    } catch (e) {
        console.log ('trying again, connecting to ' + adr);
        ws = new WebSocket (adr);
    }
    
    if (ws != null) {
        ws.onopen = function (event) {
            console.log ('connection successful');
            Setup_ws();
        }
        pingerid = setInterval (function () {Ping_ws ();}, 10000);
    } else {
        document.writeln ('If you are seeing this your browser or your connection to the internet is blocking websockets.');
    }
});")
