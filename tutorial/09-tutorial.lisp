(defpackage #:clog-user
  (:use #:cl #:clog)
  (:export start-tutorial))

(in-package :clog-user)

(defun on-new-window (body)
  (let* (last-tab
	 ;; Note: Since the there is no need to use the tmp objects
	 ;;       we reuse the same symbol name even though the
	 ;;       compiler can mark those for collection early
	 ;;       there is no issue as the element is created already
	 ;;       in the browser window.
	 
	 ;; Create tabs and panels
	 (t1  (create-button body :content "Tab1"))
	 (t2  (create-button body :content "Tab2"))
	 (t3  (create-button body :content "Tab3"))
	 (tmp (create-br body))
	 (p1  (create-div body))
	 (p2  (create-div body :content "Panel2 - Type here"))
	 (p3  (create-div body :content "Panel3 - Type here"))

	 ;; Create from for panel 1
	 (f1  (create-form p1))
	 (tmp (create-label f1 :content "Fill in blank:"))
	 (fe1 (create-form-element f1 :text :label tmp))
	 (tmp (create-br f1))
	 (tmp (create-label f1 :content "Pick a color:"))
	 (fe2 (create-form-element f1 :color :label tmp))
	 (tmp (create-br f1))
	 (tmp (create-form-element f1 :submit :value "OK"))
	 (tmp (create-form-element f1 :reset :value "Start Again")))
    
    (setf (place-holder fe1) "type here..")
    
    (set-on-submit f1
		   (lambda (obj)
		     (setf (title (html-document body)) (value fe1))
		     (setf (background-color p1) (value fe2))
		     (setf (hiddenp f1) t)
		     (create-span p1 "<br><b>Your form has been submitted</b>")))
    
    (setf (width p1) "100%")
    (setf (width p2) "100%")
    (setf (width p3) "100%")
    
    (setf (height p1) 400)
    (setf (height p2) 400)
    (setf (height p3) 400)

    (set-border p1 :thin :solid :black)
    (set-border p2 :thin :solid :black)
    (set-border p3 :thin :solid :black)

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
