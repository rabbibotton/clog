(defpackage #:clog-user
  (:use #:cl #:clog)
  (:export start-tutorial))

(in-package :clog-user)

(defun on-new-window (body)
  (let* (last-tab
	 ;; Note: Since the there is no need to use the tmp objects
	 ;;       we reuse the same symbol name (tmp) even though the
	 ;;       compiler can mark those for garbage collection early
	 ;;       this not an issue as the element is created already
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
	 (fe2 (create-form-element
	       f1 :color :value "#ffffff" :label tmp))
	 (tmp (create-br f1))
	 (tmp (create-form-element f1 :submit :value "OK"))
	 (tmp (create-form-element f1 :reset :value "Start Again"))
	 (dl))

    ;; Panel 1 contents
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    (setf (place-holder fe1) "type here..")
    (setf (requiredp fe1) t)
    (setf (size fe1) 60)

    (setf dl (create-data-list f1))
    (add-option dl "Cool Title")
    (add-option dl "Not So Cool Title")
    (add-option dl "Why Not Another Title")
    (set-data-list fe1 dl)
    
    (setf dl (create-data-list f1))
    (add-option dl "#ffffff")
    (add-option dl "#ff0000")
    (add-option dl "#00ff00")
    (add-option dl "#0000ff")
    (add-option dl "#ff00ff")
    (set-data-list fe2 dl)
    
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

    ;; Panel 2 contents
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (setf (editablep p2) t)

    ;; Panel 3 contents
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (setf (editablep p3) t)

    ;; Tab functionality
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (flet ((select-tab (obj)
	     (setf (hiddenp p1) t)
	     (setf (hiddenp p2) t)
	     (setf (hiddenp p3) t)

	     (setf (background-color t1) :lightgrey)
	     (setf (background-color t2) :lightgrey)
	     (setf (background-color t3) :lightgrey)
	     
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
			 (select-tab p3))))
    (run body)))

(defun start-tutorial ()
  "Start turtorial."

  (initialize #'on-new-window)
  (open-browser))
