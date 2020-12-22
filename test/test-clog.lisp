(defpackage #:test-clog
  (:use #:cl #:clog)
  (:export test))

(in-package :test-clog)

(defvar *last-obj*)

(defun on-new-window (win)
  (let ((tmp))
    (clog-connection:put-line (clog::connection-id win) "<button id='myid'>In html</button>")
    (setf tmp (attach-as-child win "myid"))
    (setf (style tmp "background-color") "red")
    (setf (property tmp "draggable") "true")
    (when (equal (property tmp "draggable")
		 (setf (property tmp "innerHTML") "<h2>I am draggable</h2>")))
    (setf tmp (create-child win "<button>test</botton>"))
    (set-on-click tmp (lambda () (clog-connection:alert-box (clog::connection-id win) "clicked")))
    (setf (width tmp) 300)
    (setf (height tmp) 50)
    (create-child win (format nil "<H2>~A</H2>" (gethash "connection-id" (connection-data win))))
    (setf *last-obj* (create-child win "<button>a</button>"))))

(defun test ()
  (print "Init connection")
  (initialize #'on-new-window :boot-file "/debug.html")
  (print "Open browser")
  (open-browser))
