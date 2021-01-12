(defpackage #:clog-user
  (:use #:cl #:clog)
  (:export start-tutorial))

(in-package :clog-user)

(defun on-new-window (body)
  (let* (last-tab
	 (t1 (create-button body :content "Tab1"))
	 (t2 (create-button body :content "Tab2"))
	 (t3 (create-button body :content "Tab3"))
	 (p1 (create-div body :content "Panel1 - Type here"))
	 (p2 (create-div body :content "Panel2 - Type here"))
	 (p3 (create-div body :content "Panel3 - Type here"))
	 (f1 (create-form p1))
	 (fe1 (create-form-element f1 :text :value "Stuff"))
	 (fe2 (create-form-element f1 :submit :value "OK"))
	 (fe3 (create-form-element f1 :reset :value "Start Again")))

    (place-after t3 (create-br body))
    (place-after fe1 (create-br body))

    (set-on-submit f1
		   (lambda (obj)
		     (setf (title (html-document body)) (attribute fe1 "value"))
		     (setf (hiddenp f1) t)
		     (create-span p1 "<br><b>Your form has been submitted</b>")))
    
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

	     (setf (background-color t1) :grey)
	     (setf (background-color t2) :grey)
	     (setf (background-color t3) :grey)
	     
	     (setf (background-color last-tab) :lightblue)
	     (setf (hiddenp obj) nil)
	     (focus obj)))

      (setf last-tab t1)
      (select-tab p1)
      
      (set-on-click t1 (lambda (obj)
			 (setf last-tab obj)
			 (select-tab p1)))
      (set-on-click t2 (lambda (obj)
			 (setf last-tab obj)
			 (select-tab p2)))
      (set-on-click t3 (lambda (obj)
			 (setf last-tab obj)
			 (select-tab p3))))))

(defun start-tutorial ()
  "Start turtorial."

  (initialize #'on-new-window)
  (open-browser))
