(defpackage #:clog-tut-8
  (:use #:cl #:clog)
  (:export start-tutorial))

(in-package :clog-tut-8)

(defclass app-data ()
  ((drag-x
    :accessor drag-x
    :documentation "The location of the left side of the box relative to mouse during drag.")
   (drag-y
    :accessor drag-y
   :documentation "The location of the top of the box relative to mouse during drag."))
  (:documentation "App data specific to each instance of our tutorial 8 app"))

(defun stop-tracking (obj)
  (set-on-pointer-move obj nil)
  (set-on-pointer-up obj nil))

(defun on-mouse-down (obj data)
  (let* ((app (connection-data-item obj "app-data"))
         (mouse-x  (getf data :screen-x))  ; Use the screen coordinates not
         (mouse-y  (getf data :screen-y))  ; the coordinates relative to the obj
         (obj-top  (position-top obj))
         (obj-left (position-left obj)))
    (setf (drag-x app) (- mouse-x obj-left))
    (setf (drag-y app) (- mouse-y obj-top))
    (set-on-pointer-move obj 'on-mouse-move)
    (set-on-pointer-up obj 'on-mouse-up)))
  
(defun on-mouse-move (obj data)
  (let* ((app (connection-data-item obj "app-data"))
         (x   (getf data :screen-x))
         (y   (getf data :screen-y)))
    (setf (top obj) (unit :px (- y (drag-y app))))
    (setf (left obj) (unit :px (- x (drag-x app))))))

(defun on-mouse-up (obj data)
  (declare (ignore data))
  (stop-tracking obj))

(defun on-new-window (body)
  (let ((app (make-instance 'app-data)))                 ; Create our "App-Data" for this instance
    (setf (connection-data-item body "app-data") app))   ; of our App.
  (setf (title (html-document body)) "Tutorial 08")
  (let* ((div1 (create-div body))
         (div2 (create-div div1))
         (div3 (create-div div2))
         (dir  (create-div div1 :content "<b>Click and drag the boxes</b>")))
    ;; Absolute allows fixed positioning relative to parent
    (setf (positioning dir) :absolute)
    (setf (bottom dir) 0)
    ;; borders
    (set-border div1 :medium :solid :blue)
    (set-border div2 :thin :dotted :red)
    (set-border div3 :thick :dashed :green)
    ;; sizes
    (setf (width div1) 400)
    (setf (width div2) 300)
    (setf (width div3) 200)
    (setf (height div1) 400)
    (setf (height div2) 300)
    (setf (height div3) 200)
    ;; Fixed positioning allows direct positioning relative
    ;; to the entire window.
    (setf (positioning div1) :fixed)        ; Its location relative to window
    (setf (overflow div1) :hidden)          ; Clip the contents
    (setf (positioning div1) :fixed)        ; Its location relative to window
    (setf (positioning div2) :absolute)     ; Its location relative to its parent container
    (setf (overflow div2) :hidden)
    (setf (positioning div3) :absolute)
    ;; Setup mouse/touch/pointer events
    ;;
    ;; Since our divs are embedded on with in the other we use cancel-event so events do
    ;; not bubble up from one div to another
    ;;
    ;; We need to catch the on-touch-start event or touches will not be tracked
    ;; However once cought the pointer events will be same for mouse or touch
    (set-on-touch-start div1 (lambda (obj data) (declare (ignore obj data)) nil) :cancel-event t)
    (set-on-touch-start div2 (lambda (obj data) (declare (ignore obj data)) nil) :cancel-event t)
    (set-on-touch-start div3 (lambda (obj data) (declare (ignore obj data)) nil) :cancel-event t)
    (set-on-pointer-down div1 'on-mouse-down :cancel-event t :capture-pointer t)
    (set-on-pointer-down div2 'on-mouse-down :cancel-event t :capture-pointer t)
    (set-on-pointer-down div3 'on-mouse-down :cancel-event t :capture-pointer t)))

(defun start-tutorial ()
  "Start tutorial."
  (initialize 'on-new-window)
  (open-browser))
