;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG - The Common Lisp Omnificent GUI                                 ;;;;
;;;; (c) David Botton                                                      ;;;;
;;;;                                                                       ;;;;
;;;; clog-connection-websockets.lisp                                       ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Specific implementation using websockets and long polling

(in-package :clog-connection)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implemetation - clog-connection-websockets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *app*            nil "Clack 'app' middle-ware")
(defvar *client-handler* nil "Clack 'handler' for socket traffic")

(defvar *long-poll-first* nil
  "Dynamic variable indicating to use html output instead of
   websocket for output at start if connection.")
(defvar *extended-long-poll* nil
  "Dynamic variable indicating to extend long polling beyond
   extablishing websocket for output.")
(defvar *long-poll-url* nil
  "Dynamic variable indicating the url path used.")

(defparameter *compiled-boot-js*
  (with-open-file (stream (merge-pathnames #P"static-files/js/boot.js"
                                           (asdf:system-source-directory :clog)))
    (let ((content (make-string (file-length stream))))
      (read-sequence content stream)
      content))
  "A compiled version of current version of boot.js (private)")

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; handle-new-connection ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun handle-new-connection (connection id)
  "Handle new incoming websocket CONNECTIONS with ID from boot page. (Private)"
  (handler-case
      (cond ((and id (gethash id *connection-data*))
             (format t "Reconnection id - ~A to ~A~%" id connection)
             (handler-case
                 (websocket-driver:close-connection (gethash id *connection-ids*)
                                                    "Aborting this old connection since receiving a reconnection request.")
               (t (c)
                 (when *verbose-output*
                   (format t "Failed to close the old connection when establishing reconnection. ~
                              This can be normal: The old connection could probably don't work for the client, ~
                              so the client is requesting to reconnect.~%Condition - ~A.~&"
                           c))))
             (setf (gethash id *connection-ids*) connection)
             (setf (gethash connection *connections*) id))
            (id
             (format t "Reconnection id ~A not found. Closing the connection.~%" id)
             (websocket-driver:close-connection connection)) ; Don't send the reason for better security.
            (t
             (setf id (random-hex-string))
             (setf (gethash connection *connections*) id)
             (setf (gethash id *connection-ids*) connection)
             (setf (gethash id *connection-data*)
                   (make-hash-table* :test #'equal))
             (setf (gethash "connection-id" (get-connection-data id)) id)
             (format t "New connection id - ~A - ~A~%" id connection)
             (websocket-driver:send connection
                                    (format nil "clog['connection_id']='~A'" id))
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
        (cond ((null connection-id)
               ;; a zombie connection
               (when *verbose-output*
                 (format t "A zombie connection ~A. CLOG doesn't remember its connection-id. Closing it.~%"
                         connection))
               (websocket-driver:close-connection connection)) ; don't send the reason for better security
              ((equal (first ml) "0")
               ;; a ping
               (when *browser-gc-on-ping*
                 ;; run browser gc
                 (execute connection-id
                        "Object.entries(clog).forEach(function(c,i,a)
                             {if ((c[1] !== null) && (typeof c[1] === 'object') && (c[1].nodeType===1))
                             {if (c[1].isConnected===false) {delete clog[c[0]]}}})"))
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
                            (let* ((debug-hook (gethash "clog-debug" event-hash)))
                              (if (and debug-hook (not *disable-clog-debugging*))
                                  (funcall debug-hook event data)
                                  (funcall event data)))))
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
      (format t "Condition caught in handle-close-connection - ~A.~&" c)
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
	(websocket-driver:on :error ws
			     (lambda (msg)
			       (format t "Websocket error - ~A~&" msg)))
        (websocket-driver:on :close ws
                             (lambda (&key code reason)
                               (declare (ignore code reason))
                               (handler-case
                                   (handle-close-connection ws)
                                 (t (c)
                                   (format t "Condition caught in clog-server :close - ~A.~&" c)
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
                     (lack-middleware-list nil)
                     (extended-routing nil)
                     (long-poll-first  nil)
                     (boot-file        "/boot.html")
                     (boot-function    nil)
                     (static-boot-html nil)
                     (static-boot-js   nil)
                     (static-root      #P"./static-files/")
                     (ssl              nil)
                     (ssl-key-file     nil)
                     (ssl-cert-file    nil))
  "Initialize CLOG on a socket using HOST and PORT to serve BOOT-FILE as the
default route for '/' to establish web-socket connections and static files
located at STATIC-ROOT. The webserver used with CLACK can be chosen with
:SERVER and middlewares prepended with :LACK-MIDDLEWARE-LIST,
NOT supporting LACK.BUILDER DSL.
If LONG-POLLING-FIRST is t, the output is sent as HTML instead of
websocket commands until the end of the on-new-window-handler, if
LONG-POLLING-FIRST is a number will keep long polling till that number of
queries to browser.  LONG-POLLING-FIRST is used in webserver applications to
enable crawling of your website. If BOOT-FILE is nil no initial clog-path's will
be setup, use clog-path to add. The on-connect-handler needs to indentify the
path by querying the browser. See PATH-NAME (in CLOG-LOCATION). If
EXTENDED-ROUTING is t routes will match even if extend with additional / and
additional paths. If static-boot-js is nil then boot.js is served from the file
/js/boot.js instead of the compiled version. If static-boot-html is t if
boot.html is not present will use compiled version otherwise if set to nil (default)
if a boot file not found returns returns a blank page, if it is set to :error will
signal an error and if set to a string will display the string. boot-function if set is
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
                       (,*compiled-boot-js*))
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
                                                   (cond ((eq static-boot-html t)
                                                          "")
                                                         ((eq static-boot-html :error)
                                                          (error (format nil "Can not open boot file - ~A"
                                                                         file)))
                                                         (t
                                                          static-boot-html))
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
                              (let ((id  (random-hex-string))
                                    (req (lack.request:make-request env)))
                                (setf (gethash id *connection-data*)
                                      (lack.request:request-body-parameters req))
                                (setf post-data id)))
                            (when (equal (getf env :content-type)
                                         "application/x-www-form-urlencoded")
                              (setf post-data (cond ((eq (class-name (class-of (getf env :raw-body)))
                                                         'circular-streams:circular-input-stream)
                                                     (let ((array-buffer (make-array (getf env :content-length)
                                                                                     :adjustable t
                                                                                     :fill-pointer t)))
                                                       (read-sequence array-buffer (getf env :raw-body))
                                                       (flex:octets-to-string array-buffer)))
                                                    (t
                                                     (let ((string-buffer (make-string (getf env :content-length))))
                                                       (read-sequence string-buffer (getf env :raw-body))
                                                       string-buffer)))))
                            (cond (long-poll-first
                                   (let ((id (random-hex-string)))
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
                                          (format nil "<script>clog['connection_id']='~A';Open_ws();</script>" id)
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
  ;; Wrap lack middlewares
  (setf *app* (reduce #'funcall
                      lack-middleware-list
                      :initial-value *app*
                      :from-end t))
  (setf *client-handler* (clack:clackup *app* :server server :address host :port port :ssl ssl :ssl-key-file ssl-key-file :ssl-cert-file ssl-cert-file))
  (format t "HTTP listening on    : ~A:~A~%" host port)
  (format t "HTML root            : ~A~%"    static-root)
  (format t "SSL                  : ~A~%"    (if ssl
                                                 "yes"
                                                 "no"))
  (format t "SSL Key File         : ~A~%"    ssl-key-file)
  (format t "SSL Cert File        : ~A~%"    ssl-cert-file)
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

;;;;;;;;;;;;;;;;;;;;;;;;;
;; shutdown-connection ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun shutdown-connection ()
  (clack:stop *client-handler*)
  (setf *app* nil)
  (setf *client-handler* nil))

;;;;;;;;;;;;
;; cclose ;;
;;;;;;;;;;;;

(defun cclose (connection-id)
  "Close connection to CONNECTION-ID. The boot file may try to reestablish
 connectivity."
  (execute connection-id "ws.close()"))

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
