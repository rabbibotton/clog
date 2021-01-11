(defpackage #:clog-user
  (:use #:cl #:clog)
  (:export start-tutorial))

(in-package :clog-user)

(defun on-new-window (body)
  (let* ((t1 (create-button body :content "Tab1"))
	 (t2 (create-button body :content "Tab2"))
	 (t3 (create-button body :content "Tab3"))
	 (p1 (create-div body :content "Panel1 - Type here"))
	 (p2 (create-div body :content "Panel2 - Type here"))
	 (p3 (create-div body :content "Panel3 - Type here")))

    (place-after t3 (create-br body))

    (setf (width p1) 600)
    (setf (width p2) 600)
    (setf (width p3) 600)
    
    (setf (height p1) 300)
    (setf (height p2) 300)
    (setf (height p3) 300)

    (set-border p1 :thin :solid :black)
    (set-border p2 :thin :solid :black)
    (set-border p3 :thin :solid :black)

    (setf (editablep p1) t)
    (setf (editablep p2) t)
    (setf (editablep p3) t)

    (flet ((select-tab (obj)
	     (setf (hiddenp p1) t)
	     (setf (hiddenp p2) t)
	     (setf (hiddenp p3) t)
	     (setf (hiddenp obj) nil)
	     (focus obj)))

      (select-tab p1)
      
      (set-on-click t1 (lambda (obj)
			 (select-tab p1)))
      (set-on-click t2 (lambda (obj)
			 (select-tab p2)))
      (set-on-click t3 (lambda (obj)
			 (select-tab p3))))))

(defun start-tutorial ()
  "Start turtorial."

  (initialize #'on-new-window)
  (open-browser))
