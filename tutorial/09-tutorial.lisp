(defpackage #:clog-tut-9
  (:use #:cl #:clog)
  (:export start-tutorial))

(in-package :clog-tut-9)

;; In this tutorial we demonstrate using forms and form controls
;; this is not traditonal HTML use, as we never leave the original
;; page. It is an interactive live application.

(defun on-new-window (body)
  (setf (title (html-document body)) "Tutorial 09")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Create tabs and panels - a simple file card interface
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (let* ((tab1   (create-button body :content "Tab1"))
         (tab2   (create-button body :content "Tab2"))
         (tab3   (create-button body :content "Tab3"))
         (tmp    (create-br body))
         (panel1 (create-div body))
         (panel2 (create-div body))
         ; styling can be done here also
         (panel3 (create-div body :content "Panel3 - Type here"
                             :style "width:100%;height:400px;border:thin solid black"))
         (tab-to-panel (list
                         (list tab1 panel1)
                         (list tab2 panel2)
                         (list tab3 panel3)))) ; an a-list of tabs to panels
    (declare (ignore tmp)) ; ignore warnings tmp never used
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Style the panels - you can program your styles
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; can set any value with width and height, as a number (ie pixels).
    ; or any valid number + unit as a string. They both return as a numbers
    ; the number of pixels.
    (setf (width panel1) "100%")
    (setf (height panel1) (unit :px 400)) ; work with numbers and set unit
    ; you can set many values at one with set-geometry, it's default unit
    ; is a pixel
    (set-geometry panel2 :width "100%" :height 400)
    ; CLOG contains programtic ways to handle most styles
    (set-border panel1 :thin :solid :black)
    ; You can also set styles with a list
    (set-styles panel2 '(("border" "thin solid black")))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Tab functionality
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (flet ((select-tab (new-tab) ; can be used as an event handler
             (mapcar (lambda (l)
                       (setf (background-color (first l)) :lightgrey)
                       (setf (hiddenp (second l)) t))
                     tab-to-panel)
             (setf (hiddenp (second (assoc new-tab tab-to-panel))) nil) ; not hidden
             (setf (background-color new-tab) :lightblue)
             (focus new-tab)))
      (set-on-click tab1 #'select-tab) ; You can not use 'select-tab the symbol
      (set-on-click tab2 #'select-tab) ; value as the symbol is not global. You
      (set-on-click tab3 #'select-tab) ; must set the actual function value #'
      (select-tab tab1))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Panel 1 contents
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (let* ((form (create-form panel1))
           (element1 (create-form-element form :text ; input text box
                                          :label (create-label form ; give it label
                                                               :content "Fill in blank:")))
           (tmp       (create-br form))
           (element2  (create-form-element form :color ; color picker
                                           :value "#ffffff"
                                           :label (create-label form :content "Pick a color:"))))
      (declare (ignore tmp))
      (create-br form)
      (create-form-element form :submit :value "OK")
      (create-form-element form :reset :value "Start Again")
      (setf (place-holder element1) "type here..")
      (setf (requiredp element1) t)
      (setf (size element1) 60)
      (make-data-list element1 '("Cool Title"
                                 "Not So Cool Title"
                                 "Why Not, Another Title")) ; completion list
      (make-data-list element2 '("#ffffff"
                                 "#ff0000"
                                 "#00ff00"
                                 "#0000ff"
                                 "#ff00ff")) ; default color palette
      (set-on-submit form
                     (lambda (obj)
                       (declare (ignore obj))
                       (setf (title (html-document body))
                             (value element1)) ; change page title
                       (setf (background-color panel1)
                             (value element2)) ; change panel background
                       (setf (hiddenp form) t) ; hide form and all its elements
                       ; To destroy the contents of tbe panel1 and so also the
                       ; form, you could also do (setf (text panel1) "")
                       (create-span panel1 :content
                                    "<br><b>Your form has been submitted</b>"))))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Panel 2 contents
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (let* ((form      (create-form panel2))
           (fset      (create-fieldset form :legend "Stuff"))
           (lbl       (create-label fset :content "Please type here:"))
           (text-area (create-text-area fset :columns 60 :rows 8 :label lbl))
           (tmp1      (create-br fset))
           (radio1    (create-form-element fset :radio :name "rd"))
           (tmp2      (create-label fset :content "To Be" :label-for radio1))
           (radio2    (create-form-element fset :radio :name "rd"))
           (tmp3      (create-label fset :content "No to Be" :label-for radio2))
           (tmp4      (create-br fset))
           (check1    (create-form-element fset :checkbox :name "ck"))
           (tmp5      (create-label fset :content "Here" :label-for check1))
           (check2    (create-form-element fset :checkbox :name "ck"))
           (tmp6      (create-label fset :content "There" :label-for check2))
           (tmp7      (create-br fset))
           (select1   (create-select fset :label (create-label fset :content "Pick one:")))
           (select2   (create-select fset :label (create-label fset :content "Pick one:")))
           (select3   (create-select fset :multiple t
                                     :label (create-label fset :content "Pick some:"))))
      (declare (ignore tmp1 tmp2 tmp3 tmp4 tmp5 tmp6 tmp7))
      ;; virticle aling controls with their labels
      (setf (vertical-align text-area) :top)
      (setf (vertical-align select1) :top)
      (setf (vertical-align select2) :top)
      (setf (vertical-align select3) :top)
      ;; add options to select1 as a list
      (setf (size select1) 3) ; turn select1 into a listbox size > 1
      (add-select-options select1 '("one"
                                    "two"
                                    "three"
                                    "four"
                                    "five"))
      ;; add options to select1 as a list
      ;; size defaults to 1 - so a dropdown list
      (add-select-options select2 '("one"
                                    "two"
                                    "three"
                                    "four"
                                    "five"))
  
      ;; add options to select3 programticly
      (create-option select3 :content "one")
      (create-option select3 :content "two")
      (create-option select3 :content "three")
      (let* ((group (create-optgroup select3 :content "These are a group"))
             (op4   (create-option group :content "four"))
             (op5   (create-option group :content "five")))
        (declare (ignore op4))
        (set-on-change select3 (lambda (obj) ; change event on a control
                                 (declare (ignore obj))
                                 (when (selectedp op5)
                                   (alert (window body) "Selected 5")))))
      ;; settings for text-area
      (disable-resize text-area)
      ;; add form buttons
      (create-form-element form :submit :value "OK")
      (create-form-element form :reset :value "Start Again")
      (set-on-submit form
                     (lambda (obj)
                       (declare (ignore obj))
                       (setf (hiddenp form) t)
                       (create-span panel2 :content
                                    (format nil "<br><b>Your form has been submitted:</b>
                                       <br>~A<hr>1 - ~A<br>2 - ~A<br>3 - ~A"
                                            (value text-area)
                                            (value select1)
                                            (value select2)
                                            (value select3))))))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Panel 3 contents
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (setf (editablep panel3) t))) ; turn panel 3 into an editable area

(defun start-tutorial ()
  "Start tutorial."
  (initialize 'on-new-window)
  (open-browser))
