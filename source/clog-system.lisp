;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG - The Common Lisp Omnificent GUI                                 ;;;;
;;;; (c) 2020-2022 David Botton                                            ;;;;
;;;; License BSD 3 Clause                                                  ;;;;
;;;;                                                                       ;;;;
;;;; clog-system.lisp                                                      ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:in-package :clog)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - CLOG System
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *url-to-on-new-window* (make-hash-table* :test 'equalp)
  "URL to on-new-window handlers (private)")

(defvar *clog-running* nil "If clog running.")

(defvar *overide-static-root* nil
  "Override the static-root settings. This is not normally a good idea, but if
trying to run the tutorials or demos and unable to have your local directory
the same as the clog directy this overides the relative paths used in them.")

(defvar *static-root* nil
  "Contains the static-root setting after initialization.")

(defvar *extended-routing* nil
  "If true extended routing is done.")

;;;;;;;;;;;;;;;;
;; initialize ;;
;;;;;;;;;;;;;;;;

(defun on-connect (connection-id)
  (when clog-connection:*verbose-output*
    (format t "Start new window handler on connection-id - ~A" connection-id))
  (let ((body (make-clog-body connection-id)))
    (let* ((path          (if clog-connection::*long-poll-url*
                              clog-connection::*long-poll-url*
                              (path-name (location body))))
           (on-new-window (gethash path *url-to-on-new-window*)))
      (unless on-new-window
        (when *extended-routing*
            (maphash (lambda (k v)
                       (unless (equal k "/")
                         (when (ppcre:scan (format nil "^~A/" k) path)
                           (setf on-new-window v))))
                     *url-to-on-new-window*)))
      (unless on-new-window
          (setf on-new-window (or (gethash :default *url-to-on-new-window*)
                                  (gethash "/" *url-to-on-new-window*))))
      (if on-new-window
          (progn
            (setf (connection-data-item body "clog-path") path)
            (setf (connection-data-item body "clog-body") body)
            (setf (connection-data-item body "clog-sync") (bordeaux-threads:make-lock))
            (funcall on-new-window body))
          (put-br (html-document body) "No route to on-new-window")))))

(defun initialize
    (on-new-window-handler
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
       (static-root      (merge-pathnames "./static-files/"
                            (asdf:system-source-directory :clog))))
  "Inititalize CLOG on a socket using HOST and PORT to serve BOOT-FILE
as the default route to establish web-socket connections and static
files located at STATIC-ROOT. The webserver used with CLACK can be
chosed with :SERVER. If EXTENDED-ROUTING is t routes will match even
if extend with additional / and additional paths. If
LONG-POLLING-FIRST is t, the output is sent as HTML instead of
websocket commands until on-new-window-handler ends, this should be
used in webserver applications to enable crawling of your website. If
CLOG was already initialized and not shut down, this function does the
same as set-on-new-window (does not change the static-root). If
ON-NEW-WINDOW-HANDLER is nil no handler is set and none is
removed. STATIC-ROOT by default is the \"directory CLOG is installed
in ./static-files\" If the variable clog:*overide-static-root* is set
STATIC-ROOT will be ignored. If BOOT-FILE is nil no default boot-file
will be set for root path, i.e. /. If static-boot-js is t then boot.js
is served from the file /js/boot.js instead of the compiled
version. If static-boot-html is nil if boot.html is not present will
use compiled version. boot-function if set is called with the url and
the contents of boot-file and its return value replaces the contents
sent to the brower, this allows adding content for search engine
optimization, see tutorial 12 for an example."
  (setf *extended-routing* extended-routing)
  (when on-new-window-handler
    (set-on-new-window on-new-window-handler :path "/" :boot-file boot-file))
  (unless *clog-running*
    (setf *clog-running* t)
    (setf *static-root* (truename (if *overide-static-root*
				      *overide-static-root*
				      static-root)))
    (clog-connection:initialize #'on-connect
                                :host             host
                                :port             port
                                :server           server
                                :long-poll-first  long-poll-first
                                :extended-routing extended-routing
                                :boot-file        boot-file
                                :boot-function    boot-function
                                :static-boot-html static-boot-html
                                :static-boot-js   static-boot-js
                                :static-root      *static-root*)))

;;;;;;;;;;;;;;;;;;;;;;;
;; set-on-new-window ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defun set-on-new-window (on-new-window-handler
                          &key (path "/") (boot-file "/boot.html"))
  "Set or change the ON-NEW-WINDOW-HANDLER for PATH using
BOOT_FILE. Paths should always begin with a forward slash '/'. If PATH
is set to :default any path without another route and there is no
static file matching the requested path ON-NEW-WINDOW-HANDLER and
BOOT-FILE will be used. If BOOT-FILE is nil path is removed."
  (clog-connection:set-clog-path path boot-file)
  (if boot-file
      (setf (gethash path *url-to-on-new-window*) on-new-window-handler)
      (remhash path *url-to-on-new-window*)))

;;;;;;;;;;;;;;;;;;
;; is-running-p ;;
;;;;;;;;;;;;;;;;;;

(defun is-running-p ()
  *clog-running*)

;;;;;;;;;;;;;;
;; shutdown ;;
;;;;;;;;;;;;;;

(defun shutdown ()
  "Shutdown CLOG."
  (when *clog-running*
    (clrhash *url-to-on-new-window*)
    (setf *clog-running* nil)
    (clog-connection:shutdown-clog)))

;;;;;;;;;;;;;;;;
;; debug-mode ;;
;;;;;;;;;;;;;;;;

(defun debug-mode (obj)
  "Turn on browser console debugging for OBJ's connection."
  (clog-connection:debug-mode (connection-id obj)))

;;;;;;;;;;;;;;;;;;
;; open-browser ;;
;;;;;;;;;;;;;;;;;;

(defun open-browser (&key (url "http://127.0.0.1:8080"))
  "Launch on os a web browser on local machine to URL. See OPEN-WINDOW
for openning windows on remote machines."
  (handler-case
      (trivial-open-browser:open-browser url)
    (error (c)
      (format t "Unable to open browser.~%~%~A" c))))
