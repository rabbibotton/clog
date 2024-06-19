;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG - The Common Lisp Omnificent GUI                                 ;;;;
;;;; (c) David Botton                                                      ;;;;
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

(defvar *clog-debug* nil
  "Set a debug hook that is called for every event with (event data)
that must be (funcall event data).")

(defvar *overide-static-root* nil
  "Override the static-root settings. This is not normally a good idea, but if
trying to run the tutorials or demos and unable to have your local directory
the same as the clog directy this overides the relative paths used in them.")

(defvar *static-root* nil
  "Contains the static-root setting after initialization.")

(defvar *extended-routing* nil
  "If true extended routing is done.")

(defparameter *clog-port* 8080
  "Port this instance of clog was started on")

;;;;;;;;;;;;;;;;
;; initialize ;;
;;;;;;;;;;;;;;;;

(defun on-connect (connection-id)
  (when clog-connection:*verbose-output*
    (format t "Start new window handler on connection-id - ~A~%" connection-id))
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
            (setf (connection-data-item body "clog-debug") *clog-debug*)
	    (set-on-event-with-data (window body) "gc"
                                    (lambda (obj data)
                                      (perform-gc obj data)))
            (if *clog-debug*
                (funcall *clog-debug* on-new-window body)
                (funcall on-new-window body)))
          (put-br (html-document body) "No route to on-new-window")))))

(defun initialize
    (on-new-window-handler
     &rest rest
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
       (static-root      (merge-pathnames "./static-files/"
                                          (asdf:system-source-directory :clog)))
       (ssl              nil)
       (ssl-key-file     nil)
       (ssl-cert-file    nil))
  "Inititalize CLOG on a socket using HOST and PORT to serve BOOT-FILE as the
default route to establish web-socket connections and static files located at
STATIC-ROOT. The webserver used with CLACK can be chosen with :SERVER and
middlewares prepended with :LACK-MIDDLEWARE-LIST, NOT supporting LACK.BUILDER
DSL.  If EXTENDED-ROUTING is t routes will match even if extend with additional
/ and additional paths. If LONG-POLLING-FIRST is t then long polling continues
until the on-new-window-handler ends, if LONG-POLLING-FIRST is a number
continues long polling until that number of queries to browser.
LONG-POLLING-FIRST is used in webserver applications to enable crawling of your
website. If CLOG was already initialized and not shut down, this function does
the same as set-on-new-window (does not change the static-root). If
ON-NEW-WINDOW-HANDLER is nil no handler is set and none is removed. STATIC-ROOT
by default is the \"directory CLOG is installed in ./static-files\" If the
variable clog:*overide-static-root* is set STATIC-ROOT will be ignored. If
BOOT-FILE is nil no default boot-file will be set for root path, i.e. /. If
static-boot-js is t then boot.js is served from the file /js/boot.js instead of
the compiled version. If static-boot-html is t if boot.html is not present will
use compiled version otherwise if set to nil (default) if a boot file not found
returns returns a blank page, if it is set to :error will signal an error and if
set to a string will display the string. boot-function if set is called with the
url and the contents of boot-file and its return value replaces the contents
sent to the brower, this allows adding content for search engine optimization,
see tutorial 12 for an example. If port is nil or 0 a random available port
number is chosen."
  (declare (ignorable host
                      port
                      server
                      lack-middleware-list
                      extended-routing
                      long-poll-first
                      boot-file
                      boot-function
                      static-boot-html
                      static-boot-js
                      static-root
                      ssl
                      ssl-cert-file
                      ssl-key-file))
  (setf *extended-routing* extended-routing)
  (when on-new-window-handler
    (set-on-new-window on-new-window-handler :path "/" :boot-file boot-file))
  (unless *clog-running*
    (setf *clog-running* t)
    (when (or (eql port 0) (eq port nil))
      (setf port (clog-connection:random-port)))
    (setf *clog-port* port)
    (setf *static-root* (truename (or *overide-static-root*
                                      static-root)))
    (apply #'clog-connection:initialize
           (append (list #'on-connect :static-root *static-root* :port *clog-port*)
                   rest))))

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
    (clog-connection:shutdown-clog)
    (setf *clog-running* nil)))

;;;;;;;;;;;;;;;;
;; debug-mode ;;
;;;;;;;;;;;;;;;;

(defun debug-mode (obj)
  "Turn on browser console debugging for OBJ's connection."
  (clog-connection:debug-mode (connection-id obj)))

;;;;;;;;;;;;;;;;;;;;;;;
;; open-file-with-os ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defun open-file-with-os (path)
  "Open PATH using OS"
  #+windows
  (uiop:launch-program (list "explorer.exe" (uiop:native-namestring path)))
  #+linux
  (uiop:launch-program (list "xdg-open" (uiop:native-namestring path)))
  #+darwin
  (uiop:launch-program (list "open" (uiop:native-namestring path))))

;;;;;;;;;;;;;;;;;;
;; open-browser ;;
;;;;;;;;;;;;;;;;;;

(defun open-browser (&key (url (format nil "http://127.0.0.1:~A" *clog-port*)))
  "Launch on os a web browser on local machine to URL. See OPEN-WINDOW
for openning windows on remote machines."
  #+windows
  (uiop:launch-program (list "rundll32" "url.dll,FileProtocolHandler" url))
  #+linux
  (uiop:launch-program (list "xdg-open" url))
  #+darwin
  (uiop:launch-program (list "open" url)))

