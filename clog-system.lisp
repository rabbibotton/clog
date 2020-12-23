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
