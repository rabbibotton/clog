;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG - The Common Lisp Omnificent GUI                                 ;;;;
;;;; (c) 2020-2022 David Botton                                            ;;;;
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
  (*break-on-error* variable)

  (initialize             function)
  (shutdown-clog          function)
  (set-on-connect         function)
  (set-clog-path          function)
  (get-connection-data    function)
  (delete-connection-data function)

  "CLOG system utilities"

  (escape-string      function)
  (compiled-boot-html function)

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

(defun make-hash-table* (&rest args)
  "Use native concurrent hash tables"
  ;; This covers sbcl ecl mazzano lw and ccl.
  ;; (lw and ccl default hash is synchronized)
  #+(or sbcl ecl mezzano)
  (apply #'make-hash-table :synchronized t args)
  #-(or sbcl ecl mezzano) (apply #'make-hash-table args))

(defvar *verbose-output* nil "Verbose server output (default false)")
(defvar *break-on-error* t   "Allow invoking debugger (default true)")

(defvar *app*            nil "Clack 'app' middle-ware")
(defvar *client-handler* nil "Clack 'handler' for socket traffic")

(defvar *on-connect-handler* nil "New connection event handler.")

(defvar *connections*     (make-hash-table*) "Connections to IDs")
(defvar *connection-ids*  (make-hash-table*) "IDs to connections")
(defvar *connection-data* (make-hash-table*) "Connection based data")

(defvar *new-id*   0 "Last issued connection or script IDs")
(defvar *id-lock*  (bordeaux-threads:make-lock)
  "Protect new-id variable.")

(defvar *queries*        (make-hash-table*) "Query ID to Answers")
(defvar *queries-sems*   (make-hash-table*) "Query ID to semiphores")
(defvar *query-time-out* 3
  "Number of seconds to timeout waiting for a query by default")

(defvar *url-to-boot-file* (make-hash-table* :test 'equalp) "URL to boot-file")

(defvar *long-poll-first* nil
  "Dynamic variable indicating to use html output instead of
   websocket for output at start if connection.")
(defvar *extended-long-poll* nil
  "Dynamic variable indicating to extend long polling beyond
   extablishing websocket for output.")
(defvar *long-poll-url* nil
  "Dynamic variable indicating the url path used.")

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; delete-connection-data ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun delete-connection-data (connection-id)
  "Delete CONNECTION-ID's data. (private)"
  (remhash connection-id *connection-data*))

;;;;;;;;;;;;;;;;
;; prep-query ;;
;;;;;;;;;;;;;;;;

(defun prep-query (id default-answer)
  "Setup up a query to be received from a script identified by ID an returning
with DEFAULT-ANSWER in case of a time out. (Private)"
  (setf (gethash id *queries-sems*) (bordeaux-threads:make-semaphore))
  (setf (gethash id *queries*) default-answer))

;;;;;;;;;;;;;;;;;;;;;
;; wait-for-answer ;;
;;;;;;;;;;;;;;;;;;;;;

(defun wait-for-answer (id &key (timeout *query-time-out*))
  "Block after prep-query and sending the query script with ID and TIMEOUT with
the default answer. (Private)"
  (handler-case
      (progn
        (bordeaux-threads:wait-on-semaphore (gethash id *queries-sems*)
                                            :timeout timeout)
        (let ((answer (gethash id *queries*)))
          (remhash id *queries*)
          (remhash id *queries-sems*)
          answer))
    (t (c)
      (format t "Condition caught in wait-for-answer - ~A.~&" c)
      (values 0 c))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; handle-new-connection ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun handle-new-connection (connection id)
  "Handle new incoming websocket CONNECTIONS with ID from boot page. (Private)"
  (handler-case
      (cond (id
             (format t "Reconnection id - ~A to ~A~%" id connection)
             (setf (gethash id *connection-ids*) connection)
             (setf (gethash connection *connections*) id))
            (t
             (setf id (+ (floor (/ (get-universal-time) 2) (generate-id))))
             (setf (gethash connection *connections*) id)
             (setf (gethash id *connection-ids*) connection)
             (setf (gethash id *connection-data*)
                   (make-hash-table* :test #'equal))
             (setf (gethash "connection-id" (get-connection-data id)) id)
             (format t "New connection id - ~A - ~A~%" id connection)
             (websocket-driver:send connection
                                    (format nil "clog['connection_id']=~A" id))
             (bordeaux-threads:make-thread
              (lambda ()
                (if *break-on-error*
                    (funcall *on-connect-handler* id)
                    (handler-case
                        (funcall *on-connect-handler* id)
                      (t (c)
                        (format t "Condition caught connection ~A - ~A.~&" id c)
                        (values 0 c)))))
              :name (format nil "CLOG connection ~A"
                            id))))
    (t (c)
      (format t "Condition caught in handle-new-connection - ~A.~&" c)
      (values 0 c))))

;;;;;;;;;;;;;;;;;;;;
;; handle-message ;;
;;;;;;;;;;;;;;;;;;;;

(defun handle-message (connection message)
  "Handle incoming websocket MESSAGE on CONNECTION. (Private)"
  (handler-case
      (let ((connection-id (gethash connection *connections*))
            (ml (ppcre:split ":" message :limit 2)))
        (cond ((equal (first ml) "0")
               ;; a ping
               (when *verbose-output*
                 (format t "Connection ~A    Ping~%" connection-id)))
              ((equal (first ml) "E")
               ;; an event
               (let* ((em (ppcre:split " " (second ml) :limit 2))
                      (event-id (first em))
                      (data (second em)))
                 (when *verbose-output*
                   (format t "Connection ~A    Hook = ~A    Data = ~A~%"
                           connection-id event-id data))
                 (bordeaux-threads:make-thread
                  (lambda ()
                    (if *break-on-error*
                        (let* ((event-hash (get-connection-data connection-id))
                               (event      (when event-hash
                                             (gethash event-id event-hash))))
                          (when event
                            (funcall event data)))
                        (handler-case
                            (let* ((event-hash (get-connection-data connection-id))
                                   (event      (when event-hash
                                                 (gethash event-id
                                                          event-hash))))
                              (when event
                                (funcall event data)))
                          (t (c)
                            (format t "Condition caught in handle-message for event - ~A.~&" c)
                            (values 0 c)))))
                  :name (format nil "CLOG event handler ~A"
                                event-id))))
              (t
               ;; a JavaScript execution result
               (let ((server-query-id (first ml))
                     (browser-returned-answer (second ml)))
                 (when *verbose-output*
                   (format t "Connection ~A    ~A = ~A    ~A = ~A~%"
                           connection-id
                           'server-query-id
                           server-query-id
                           'browser-returned-answer
                           browser-returned-answer))
                 (setf (gethash (parse-integer server-query-id) *queries*) browser-returned-answer)
                 (bordeaux-threads:signal-semaphore
                  (gethash (parse-integer server-query-id) *queries-sems*))))))
    (t (c)
      (format t "Condition caught in handle-message - ~A.~&" c)
      (values 0 c))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; handle-close-connection ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun handle-close-connection (connection)
  "Close websocket CONNECTION. (Private)"
  (handler-case
      (let ((id (gethash connection *connections*)))
        (when id
          (when *verbose-output*
            (format t "Connection id ~A has closed. ~A~%" id connection))
          (remhash id *connection-data*)
          (remhash id *connection-ids*)
          (remhash connection *connections*)))
    (t (c)
      (format t "Condition caught in handle-message - ~A.~&" c)
      (values 0 c))))

;;;;;;;;;;;;;;;;;
;; clog-server ;;
;;;;;;;;;;;;;;;;;

(defun clog-server (env)
  "Setup websocket server on ENV. (Private)"
  (handler-case
      (let ((ws (websocket-driver:make-server env)))
        (websocket-driver:on :open ws
                             (lambda ()
                               (handler-case
                                   (let* ((query (getf env :query-string))
                                          (items (when query
                                                   (quri:url-decode-params query)))
                                          (id    (when items
                                                   (cdr (assoc "r" items
                                                               :test #'equalp)))))
                                     (when (typep id 'string)
                                       (setf id (parse-integer id :junk-allowed t)))
                                     (handle-new-connection ws id))
                                 (t (c)
                                   (print env)
                                   (format t "Condition caught in clog-server :open - ~A.~&" c)
                                   (values 0 c)))))
        (websocket-driver:on :message ws
                             (lambda (msg)
                               (handler-case
                                   (handle-message ws msg)
                                 (t (c)
                                   (format t "Condition caught in clog-server :message - ~A.~&" c)
                                   (values 0 c)))))
        (websocket-driver:on :close ws
                             (lambda (&key code reason)
                               (declare (ignore code reason))
                               (handler-case
                                   (handle-close-connection ws)
                                 (t (c)
                                   (format t "Condition caught in clog-server :message - ~A.~&" c)
                                   (values 0 c)))))
        (lambda (responder)
          (declare (ignore responder))
          (websocket-driver:start-connection ws)))
    (t (c)
      (format t "Condition caught in clog-server start-up - ~A.~&" c)
      (values 0 c))))

;;;;;;;;;;;;;;;;
;; initialize ;;
;;;;;;;;;;;;;;;;

(defun initialize (on-connect-handler
                   &key
                     (host             "0.0.0.0")
                     (port             8080)
                     (server           :hunchentoot)
                     (extended-routing nil)
                     (long-poll-first  nil)
                     (boot-file        "/boot.html")
                     (boot-function    nil)
                     (static-boot-html nil)
                     (static-boot-js   nil)
                     (static-root      #P"./static-files/"))
  "Initialize CLOG on a socket using HOST and PORT to serve BOOT-FILE as the
default route for '/' to establish web-socket connections and static files
located at STATIC-ROOT. The webserver used with CLACK can be chosed with
:SERVER. If LONG-POLLING-FIRST is t, the output is sent as HTML instead of
websocket commands until the end of the on-new-window-handler, if
LONG-POLLING-FIRST is a number will keep long polling till that number of
queries to browser.  LONG-POLLING-FIRST is used in webserver applications to
enable crawling of your website. If BOOT-FILE is nil no initial clog-path's will
be setup, use clog-path to add. The on-connect-handler needs to indentify the
path by querying the browser. See PATH-NAME (in CLOG-LOCATION). If
EXTENDED-ROUTING is t routes will match even if extend with additional / and
additional paths. If static-boot-js is nil then boot.js is served from the file
/js/boot.js instead of the compiled version. If static-boot-html is t if
boot.html is not present will use compiled version. boot-function if set is
called with the url and the contents of boot-file and its return value replaces
the contents sent to the brower."
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
             (let* ((url-path  (getf env :path-info))
                    (clog-path (gethash url-path *url-to-boot-file*)))
               (unless clog-path
                 (when extended-routing
                   (maphash (lambda (k v)
                              (unless (equal k "/")
                                (when (ppcre:scan (format nil "^~A/" k)
                                                  url-path)
                                  (setf clog-path v))))
                            *url-to-boot-file*)))
               (cond (clog-path
                      (let ((file (uiop:subpathname static-root clog-path)))
                        (with-open-file (stream file :direction :input
                                                     :if-does-not-exist nil)
                          (let ((page-data (if stream
                                               (make-string (file-length stream))
                                               (if static-boot-html
                                                   ""
                                                   (compiled-boot-html nil nil))))
                                (post-data nil))
                            (when stream
                              (read-sequence page-data stream))
                            (when boot-function
                              (setf page-data (funcall boot-function
                                                       url-path
                                                       page-data)))
                            (when (search "multipart/form-data;"
                                          (getf env :content-type))
                              (let ((id  (get-universal-time))
                                    (req (lack.request:make-request env)))
                                (setf (gethash id *connection-data*)
                                      (lack.request:request-body-parameters req))
                                (setf post-data id)))
                            (when (equal (getf env :content-type)
                                         "application/x-www-form-urlencoded")
                              (setf post-data (make-string (getf env :content-length)))
                              (read-sequence post-data (getf env :raw-body)))
                            (cond (long-poll-first
                                   (let ((id (+ (floor (/ (get-universal-time) 2) (generate-id)))))
                                     (setf (gethash id *connection-data*) (make-hash-table* :test #'equal))
                                     (setf (gethash "connection-id" (get-connection-data id)) id)
                                     (format t "New html connection id - ~A~%" id)
                                     (lambda (responder)
                                       (let* ((writer               (funcall responder '(200 (:content-type "text/html"))))
                                              (stream               (lack.util.writer-stream:make-writer-stream writer))
                                              (*long-poll-url*      url-path)
                                              (*long-poll-first*    stream)
                                              (*extended-long-poll* (if (eq long-poll-first t)
                                                                        :extend
                                                                        long-poll-first)))
                                         (write-sequence page-data stream)
                                         (write-sequence
                                          (format nil "<script>clog['connection_id']=~A;Open_ws();</script>" id)
                                          stream)
                                         (when post-data
                                           (write-sequence
                                            (format nil "<script>clog['post-data']='~A'</script>"
                                                    post-data)
                                            stream))
                                         (if *break-on-error*
                                             (funcall *on-connect-handler* id)
                                             (handler-case
                                                 (funcall *on-connect-handler* id)
                                               (t (c)
                                                 (format t "Condition caught connection ~A - ~A.~&" id c)
                                                 (values 0 c))))
                                         (when *long-poll-first*
                                           (setf *long-poll-first* nil)
                                           (handler-case
                                               (finish-output stream)
                                             (t (c)
                                               (format t "Condition caught finish-output ~A - ~A.~&" id c)
                                               (values 0 c))))
                                         (format t "HTML connection closed - ~A~%" id)))))
                                  (t
                                   (lambda (responder)
                                     (let* ((writer (funcall responder '(200 (:content-type "text/html"))))
                                            (stream (lack.util.writer-stream:make-writer-stream writer)))
                                       (write-sequence page-data stream)
                                       (when post-data
                                         (write-sequence
                                          (format nil "<script>clog['post-data']='~A'</script>"
                                                  post-data)
                                          stream))
                                       (finish-output stream)))))))))
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
  (setf *client-handler* (clack:clackup *app* :server server :address host :port port))
  (format t "HTTP listening on    : ~A:~A~%" host port)
  (format t "HTML root            : ~A~%"    static-root)
  (format t "Long poll first      : ~A~%"    (if long-poll-first
                                                 "yes"
                                                 "no"))
  (format t "Boot function added  : ~A~%"    (if boot-function
                                                 "yes"
                                                 "no"))
  (format t "Boot html source use : ~A~%"    (if static-boot-html
                                                 "static file"
                                                 "compiled version, when no file"))
  (format t "Boot js source use   : ~A~%"    (if static-boot-js
                                                 "static file"
                                                 "compiled version"))
  (format t "Boot file for path / : ~A~%"    boot-file)
  *client-handler*)

;;;;;;;;;;;;;;;;;;;
;; shutdown-clog ;;
;;;;;;;;;;;;;;;;;;;

(defun shutdown-clog ()
  "Shutdown CLOG."
  (clack:stop *client-handler*)
  (clrhash *connection-data*)
  (clrhash *connections*)
  (clrhash *connection-ids*)
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
  "Associate URL path to BOOT-FILE"
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
    (setf res (ppcre:regex-replace-all "\\x5C" res "\\x5C")) ; \
    (setf res (ppcre:regex-replace-all "\\x22" str "\\x22")) ; "
    (setf res (ppcre:regex-replace-all "\\x27" res "\\x27")) ; '
    (setf res (ppcre:regex-replace-all "\\x0A" res "\\x0A")) ; \n
    (setf res (ppcre:regex-replace-all "\\x0D" res "\\x0D")) ; \r
    res))

;;;;;;;;;;;;;
;; execute ;;
;;;;;;;;;;;;;

(defun execute (connection-id message)
  "Execute SCRIPT on CONNECTION-ID, disregard return value."
  (if *long-poll-first*
      (write-sequence (format nil "<script>~A</script>~%" message)
                      *long-poll-first*)
      (let ((con (get-connection connection-id)))
        (when con
          (websocket-driver:send con message)))))

;;;;;;;;;;;
;; query ;;
;;;;;;;;;;;

(defun query (connection-id script &key (default-answer nil))
  "Execute SCRIPT on CONNECTION-ID, return value. If times out answer
DEFAULT-ANSWER."
  ;; Provide delay if needed to establish websocket connection for
  ;; response.
  (when *long-poll-first*
    (finish-output *long-poll-first*)
    (loop
      for n from 1 to 10 do
        (let ((con (get-connection connection-id)))
          (when con
            (unless (or (eq *extended-long-poll* :extend)
                        (> (decf *extended-long-poll*) 0))
              (format t "Closing long-poll for ~A~%" connection-id)
              (setf *long-poll-first* nil))
            (return))
          (format t "Awaiting websocket connection for ~A~%" connection-id)
          (sleep .1))))
  (let ((uid (generate-id)))
    (prep-query uid (when default-answer (format nil "~A" default-answer)))
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

;;;;;;;;;;;;;;;;;;;;;;;;
;; compiled-boot-html ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defun compiled-boot-html (path content)
  (declare (ignore path content))
  "Returns a compiled version version of boot.html. The compiled boot.html
uses the jQuery CDN instead of the static js files."
"<!doctype HTML>
<HTML>
   <HEAD>
      <meta http-equiv='Cache-Control' content='no-cache, no-store, must-revalidate' />
      <meta http-equiv='Pragma' content='no-cache' />
      <meta http-equiv='Expires' content='0' />
      <meta charset='utf-8'>
      <meta name='viewport' content='width=device-width, initial-scale=1'>
      <script src='https://code.jquery.com/jquery-3.6.0.min.js'
              integrity='sha256-/xUj+3OJU5yExlq6GSYGSHk7tPXikynS7ogEvDej/m4='
              crossorigin='anonymous'></script>
      <script src='/js/boot.js' type='text/javascript'></script>
      <noscript><%= (@ meta) %></noscript>
   </HEAD>
<BODY>
  <noscript><%= (@ body) %></noscript>
</BODY>
<noscript>Your browser must support JavaScript and be HTML 5 compilant to see this site.</noscript>
</HTML>")

;;;;;;;;;;;;;;;;;;;;;;
;; compiled-boot-js ;;
;;;;;;;;;;;;;;;;;;;;;;

(defun compiled-boot-js ()
  "Returns a compiled version of current version of boot.js (private)"
"
/*compiled version*/
var ws=null;
var adr; var adrc;
var clog={};
var pingerid;
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
        ws = new WebSocket (adr + '?r=' + clog['connection_id']);
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

function Open_ws() {
    if (location.protocol == 'https:') {
        adr = 'wss://' + location.hostname;
    } else {
        adr = 'ws://' + location.hostname;
    }

    if (location.port != '') { adr = adr + ':' + location.port; }
    adr = adr + '/clog';

    if (clog['connection_id']) {
      adrc = adr  + '?r=' + clog['connection_id'];
    } else { adrc = adr }

    try {
        console.log ('connecting to ' + adrc);
        ws = new WebSocket (adrc);
    } catch (e) {
        console.log ('trying again, connecting to ' + adrc);
        ws = new WebSocket (adrc);
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
}

$( document ).ready(function() {
    if (ws == null) { Open_ws(); }
});

")
