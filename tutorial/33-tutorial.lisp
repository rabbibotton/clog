;; In this tutorial we redo Tutorial 9 using with-clog-create a far
;; more elegant way to set up GUIs in code using a declartive
;; syntax. The with-clog-create macro takes the place of using let*
;; and many temporary variables. This macro was designed by Mariano
;; Montone and illustrates nicely the advantage the programmable
;; language has at all levels of use to simplify complexity.
;;
;; Thanks Mariano!

(defpackage #:clog-tut-33
  (:use #:cl #:clog)
  (:export start-tutorial))

(in-package :clog-tut-33)

(defun on-new-window (body)
  (setf (title (html-document body)) "Tutorial 33")
  (with-connection-cache (body)
    (let* (last-tab)
      ;; Create tabs and panels Using with-clog-create allows you to
      ;; replace using "let" to create your code based gui layout.
      ;;
      ;; To use the macro you remove the create- from the create
      ;; functions. The clog-obj passed as the first parameter of the
      ;; macro is passed as the parent obj to the declared object,
      ;; after that nested levels of decleraton are used as the parent
      ;; clog-obj. To bind a variable to any created clog object using
      ;; :bind var
      (with-clog-create body
          (div ()
               (button (:bind t1 :content "Tab1"))
               (button (:bind t2 :content "Tab2"))
               (button (:bind t3 :content "Tab3"))
               (br ())

               ;; Panel 1
               (div (:bind p1)
                    ;; Create form for panel 1
                    (form (:bind f1)
                          (form-element (:bind fe1 :text :label (create-label f1 :content "Fill in blank:")))
                          (br ())
                          (form-element (:bind fe2 :color :value "#ffffff"
                                          :label (create-label f1 :content "Pick a color:")))
                          (br ())
                          (form-element (:submit :value "OK"))
                          (form-element (:reset :value "Start Again"))))

               ;; Panel 2
               (div (:bind p2)
                    ;; Create form for panel 2
                    (form (:bind f2)
                          (fieldset (:bind fs2 :legend "Stuff")
                                    (label (:bind lbl :content "Please type here:"))
                                    (text-area (:bind ta1 :columns 60 :rows 8 :label lbl))
                                    (br ())
                                    (form-element (:bind rd1 :radio :name "rd"))
                                    (label (:content "To Be" :label-for rd1))
                                    (form-element (:bind rd2 :radio :name "rd"))
                                    (label (:content "No to Be" :label-for rd2))
                                    (br ())
                                    (form-element (:bind ck1 :checkbox :name "ck"))
                                    (label (:content "Here" :label-for ck1))
                                    (form-element (:bind ck2 :checkbox :name "ck"))
                                    (label (:content "There" :label-for ck2))
                                    (br ())
                                    (select (:bind sl1 :label (create-label fs2 :content "Pick one:")))
                                    (select (:bind sl2 :label (create-label fs2 :content "Pick one:")))
                                    (select (:bind sl3 :multiple t :label (create-label fs2 :content "Pick some:"))
                                            (option (:content "one"))
                                            (option (:bind o2 :content "two"))
                                            (option (:content "three"))
                                            (optgroup (:content "These are a group")
                                                      (option (:content "four"))
                                                      (option (:bind o5 :content "five")))))
                          (form-element (:submit :value "OK"))
                          (form-element (:reset :value "Start Again"))))

               ;; Panel 3
               (div (:bind p3 :content "Panel3 - Type here")))
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
                             (select-tab p3))))))))

(defun start-tutorial ()
  "Start turtorial."
  (initialize 'on-new-window)
  (open-browser))
