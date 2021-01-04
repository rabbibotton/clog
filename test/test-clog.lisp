(defpackage #:test-clog
  (:use #:cl #:clog)
  (:export test))

(in-package :test-clog)

(defvar *last-obj*)
(defvar *last-win*)

(defun on-new-window (win)
  (log-console (window win) "Message!")
  (log-error (window win) "Error Log")
  (put-br (html-document win) "test 1 2 3")
  (setf *last-win* win)
  (let ((tmp))
    (clog-connection:put-line (clog::connection-id win) "<button id='myid'>In html</button>")
    (setf tmp (attach-as-child win "myid"))
    (setf (style tmp "background-color") "red")
    (setf (draggablep tmp) t)
    (when (draggablep tmp)
      (setf (property tmp "innerHTML") "<h2>I am draggable</h2>"))
    (setf tmp (create-child win "<button>test</botton>"))
    (set-on-click tmp (lambda () (alert (window win) "clicked")))
    (setf (box-sizing tmp) :border-box)
    (setf (width tmp) 300)
    (setf (height tmp) 50)    
    (set-border (create-child win
			  (format nil "<H2>~A</H2>"
				  (gethash "connection-id" (connection-data win))))
	    "4px" :dotted "blue")
    (setf *last-obj* (create-child win "<button>********</button>"))
    (set-on-mouse-enter *last-obj*
			(lambda ()
			  (setf (property *last-obj* "innerHTML") "Inside")))
    (set-on-mouse-leave *last-obj*
			(lambda ()
			  (setf (property *last-obj* "innerHTML") "Outside")))
    (set-on-mouse-click *last-obj*
			(lambda (data)
			  (print data)))
    (set-on-mouse-move *last-obj*
		       (lambda (data)
			 (format t "x=~A Y=~A~%" (getf data ':x) (getf data ':y))))
    (set-on-character win
		      (lambda (data)
			(print data)))
    (setf (title (html-document win)) "CLOG Test App")
    (print (title (html-document win)))
    ))

(defun test ()
  (print "Init connection")
  (initialize #'on-new-window :boot-file "/debug.html")
  (print "Open browser")
  (open-browser))
