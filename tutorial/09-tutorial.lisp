(defpackage #:clog-tut-9
  (:use #:cl #:clog)
  (:export start-tutorial))

(in-package :clog-tut-9)

(defun on-new-window (body)
  (setf (title (html-document body)) "Tutorial 9")
  (let* (last-tab
	 ;; Note: Since the there is no need to use the tmp objects
	 ;;       we reuse the same symbol name (tmp) even though the
	 ;;       compiler can mark those for garbage collection early
	 ;;       this not an issue as the element is created already
	 ;;       in the browser window. This is probably not the best
	 ;;       option for a production app though regardless.
	 ;;
	 ;; Create tabs and panels
	 (t1  (create-button body :content "Tab1"))
	 (t2  (create-button body :content "Tab2"))
	 (t3  (create-button body :content "Tab3"))
	 (tmp (create-br body))
	 (p1  (create-div body))
	 (p2  (create-div body))
	 (p3  (create-div body :content "Panel3 - Type here"))
	 ;; Create form for panel 1
	 (f1  (create-form p1))
	 (fe1 (create-form-element f1 :text
				   :label (create-label f1 :content "Fill in blank:")))
	 (tmp (create-br f1))
	 (fe2 (create-form-element f1 :color :value "#ffffff"
					     :label (create-label f1 :content "Pick a color:")))
	 (tmp (create-br f1))
	 (tmp (create-form-element f1 :submit :value "OK"))
	 (tmp (create-form-element f1 :reset :value "Start Again"))
	 ;; Create for for panel 2
	 (f2  (create-form p2))
	 (fs2 (create-fieldset f2 :legend "Stuff"))
	 (tmp (create-label fs2 :content "Please type here:"))
	 (ta1 (create-text-area fs2 :columns 60 :rows 8 :label tmp))
	 (tmp (create-br fs2))
	 (rd1 (create-form-element fs2 :radio :name "rd"))
	 (tmp (create-label fs2 :content "To Be" :label-for rd1))
	 (rd2 (create-form-element fs2 :radio :name "rd"))
	 (tmp (create-label fs2 :content "No to Be" :label-for rd2))
	 (tmp (create-br fs2))
	 (ck1 (create-form-element fs2 :checkbox :name "ck"))
	 (tmp (create-label fs2 :content "Here" :label-for ck1))
	 (ck2 (create-form-element fs2 :checkbox :name "ck"))
	 (tmp (create-label fs2 :content "There" :label-for ck2))
	 (tmp (create-br fs2))	 
	 (sl1 (create-select fs2 :label (create-label fs2 :content "Pick one:")))
	 (sl2 (create-select fs2 :label (create-label fs2 :content "Pick one:")))	 
	 (sl3 (create-select fs2 :multiple t
			     :label (create-label fs2 :content "Pick some:")))
	 (o1  (create-option sl3 :content "one"))
	 (o2  (create-option sl3 :content "two"))
	 (o3  (create-option sl3 :content "three"))
	 (og1 (create-optgroup sl3 :content "These are a group"))
	 (o4  (create-option og1 :content "four"))
	 (o5  (create-option og1 :content "five"))
	 (tmp (create-form-element f2 :submit :value "OK"))
	 (tmp (create-form-element f2 :reset :value "Start Again")))
    (declare (ignore tmp))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Panel 1 contents
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
    (setf (place-holder fe1) "type here..")
    (setf (requiredp fe1) t)
    (setf (size fe1) 60)    
    (make-data-list fe1 '("Cool Title"
			  "Not So Cool Title"
			  "Why Not, Another Title"))    
    (make-data-list fe2 '("#ffffff"
			  "#ff0000"
			  "#00ff00"
			  "#0000ff"
			  "#ff00ff"))    
    (set-on-submit f1
		   (lambda (obj)
		     (declare (ignore obj))
		     (setf (title (html-document body)) (value fe1))
		     (setf (background-color p1) (value fe2))
		     (setf (hiddenp f1) t)
		     (create-span p1 :content
				  "<br><b>Your form has been submitted</b>")))    
    (setf (width p1) "100%")
    (setf (width p2) "100%")
    (setf (width p3) "100%")    
    (setf (height p1) 400)
    (setf (height p2) 400)
    (setf (height p3) 400)
    (set-border p1 :thin :solid :black)
    (set-border p2 :thin :solid :black)
    (set-border p3 :thin :solid :black)
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Panel 2 contents
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (setf (vertical-align ta1) :top)
    (disable-resize ta1)
    (setf (vertical-align sl1) :top)
    (setf (vertical-align sl2) :top)
    (setf (vertical-align sl3) :top)
    (setf (size sl1) 3)
    (add-select-options sl1 '("one"
			      "two"
			      "three"
			      "four"
			      "five"))
    (add-select-options sl2 '("one"
			      "two"
			      "three"
			      "four"
			      "five"))
    (set-on-change sl3 (lambda (obj)
			 (declare (ignore obj))
			 (when (selectedp o5)
			   (alert (window body) "Selected 5"))))
    (set-on-submit f2
		   (lambda (obj)
		     (declare (ignore obj))
		     (setf (hiddenp f2) t)
		     (create-span p2 :content
		       (format nil "<br><b>Your form has been submitted:</b>
                                       <br>~A<hr>1 - ~A<br>2 - ~A<br>3 - ~A"
			       (value ta1)
			       (value sl1)
			       (value sl2)
			       (selectedp o2)))))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Panel 3 contents
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (setf (editablep p3) t)
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
  (initialize 'on-new-window)
  (open-browser))
