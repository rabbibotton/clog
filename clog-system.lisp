;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG - The Common Lisp Omnificent GUI                                 ;;;;
;;;; (c) 2020-2021 David Botton                                            ;;;;
;;;; License BSD 3 Clause                                                  ;;;;
;;;;                                                                       ;;;;
;;;; clog-system.lisp                                                      ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:in-package :clog)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - CLOG System
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *url-to-on-new-window* (make-hash-table :test 'equalp)
  "URL to on-new-window handlers")

(defvar *clog-running* nil "If clog running.")

(defvar *overide-static-root* nil "Overide the static-root settings.")

;;;;;;;;;;;;;;;;
;; initialize ;;
;;;;;;;;;;;;;;;;

(defun on-connect (connection-id)
  (when cc:*verbose-output*
    (format t "Start new window handler on connection-id - ~A" connection-id))
  (let ((body (make-clog-body connection-id)))
    (let* ((path          (path-name (location body)))
	   (on-new-window (or (gethash path *url-to-on-new-window*)
			      (gethash "default" *url-to-on-new-window*)
			      (gethash "/" *url-to-on-new-window*))))
      (if on-new-window  
	  (funcall on-new-window body)
	  (put-br (html-document body) "No route to on-new-window")))))

(defun initialize (on-new-window-handler
		   &key
		     (host           "0.0.0.0")
		     (port           8080)
		     (boot-file      "/boot.html")
		     (static-root    #P"./static-files/"))
  "Inititalize CLOG on a socket using HOST and PORT to serve BOOT-FILE as 
the default route to establish web-socket connections and static files
located at STATIC-ROOT. If CLOG was already initialized and not shut
down, this function does the same as set-on-new-window. If the variable
clog:*overide-static-root* is set STATIC-ROOT will be ignored. If BOOT-FILE
is nil no default boot-file will be set for /."

  (set-on-new-window on-new-window-handler :path "/" :boot-file boot-file)

  (unless *clog-running*
    (setf *clog-running* t)
    (cc:initialize #'on-connect
		   :host host
		   :port port
		   :boot-file boot-file
		   :static-root (if *overide-static-root*
				    *overide-static-root*
				    static-root))))

;;;;;;;;;;;;;;;;;;;;;;;
;; set-on-new-window ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defun set-on-new-window (on-new-window-handler
			  &key (path "/") (boot-file "/boot.html"))
  "Set or change the on-new-window handler or set a new one for PATH
using BOOT_FILE. Paths should always begin with a '/'. If PATH is set to
\"default\" will use boot-file when the route can not be determined, ie
a static html file including boot.js that has not been added with this
function. If BOOT-FILE is nil path is removed."
  (cc:set-clog-path path boot-file)
  (if boot-file
      (setf (gethash path *url-to-on-new-window*) on-new-window-handler)
      (remhash path *url-to-on-new-window*)))

;;;;;;;;;;;;;;
;; shutdown ;;
;;;;;;;;;;;;;;

(defun shutdown ()
  "Shutdown CLOG."
  (clrhash *url-to-on-new-window*)
  (setf *clog-running* nil)
  (cc:shutdown-clog))
