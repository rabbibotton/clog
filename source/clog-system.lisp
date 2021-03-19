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

(defvar *overide-static-root* nil
  "Overide the static-root settings. This is not normally a good idea, but if
trying to run the tutorials or demos and unable to have your local directy the
same as the clog directy this overides the relative paths used in them.")

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
	  (progn
	    (setf (connection-data-item body "clog-body") body)
	    (funcall
             (if (functionp on-new-window)
                 on-new-window
                 (symbol-function on-new-window))
             body))
	  (put-br (html-document body) "No route to on-new-window")))))

(defun initialize
    (on-new-window-handler
     &key
       (host           "0.0.0.0")
       (port           8080)
       (boot-file      "/boot.html")
       (static-boot-js nil)       
       (static-root    (merge-pathnames "./static-files/"
			  (asdf:system-source-directory :clog))))
  "Inititalize CLOG on a socket using HOST and PORT to serve BOOT-FILE
as the default route to establish web-socket connections and static
files located at STATIC-ROOT. If CLOG was already initialized and not
shut down, this function does the same as set-on-new-window (does not
change the static-root). STATIC-ROOT by default is the \"directory CLOG
is installed in ./static-files\" If the variable clog:*overide-static-root*
is set STATIC-ROOT will be ignored. If BOOT-FILE is nil no default
boot-file will be set for root path, i.e. /. If static-boot-js is t
then boot.js is served from the file /js/boot.js instead of the
compiled version."
  (set-on-new-window on-new-window-handler :path "/" :boot-file boot-file)
  (unless *clog-running*
    (setf *clog-running* t)
    (cc:initialize #'on-connect
		   :host           host
		   :port           port
		   :boot-file      boot-file
		   :static-boot-js static-boot-js
		   :static-root    (if *overide-static-root*
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

;;;;;;;;;;;;;;;;
;; debug-mode ;;
;;;;;;;;;;;;;;;;

(defun debug-mode (obj)
  "Turn on browser console debugging for OBJ's connection."
  (cc:debug-mode (connection-id obj)))

;;;;;;;;;;;;;;;;;;
;; open-browser ;;
;;;;;;;;;;;;;;;;;;

(defun open-browser (&key (url "http://127.0.0.1:8080"))
  "Open a web browser to URL."
  (trivial-open-browser:open-browser url))

