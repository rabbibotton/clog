(defpackage #:clog-user
  (:use #:cl #:clog)
  (:export start-demo))

(in-package :clog-user)

(defclass app-data ()
  ((body
    :accessor body
    :documentation "Store top level access on new window")
   (drag-mutex
    :reader drag-mutex
    :initform (bordeaux-threads:make-lock)
    :documentation "Serialize access to the on-mouse-down event.")
   (in-drag
    :accessor in-drag-p
    :initform nil
    :documentation "Ensure only one box is dragged at a time.")
   (drag-x
    :accessor drag-x
    :documentation "The location of the left side of the box relative to mouse during drag.")
   (drag-y
    :accessor drag-y
   :documentation "The location of the top of the box relative to mouse during drag.")))

(defun on-mouse-down (obj data)
  (let ((app (connection-data-item obj "app-data")))
    (bordeaux-threads:with-lock-held ((drag-mutex app))
      (setf (z-index obj) 1)
      (unless (in-drag-p app)
	(setf (in-drag-p app) t)
      (let* ((mouse-x  (getf data ':screen-x))
	     (mouse-y  (getf data ':screen-y))
	     (obj-top  (parse-integer (top obj) :junk-allowed t))
	     (obj-left (parse-integer (left obj) :junk-allowed t)))	
	(setf (drag-x app) (- mouse-x obj-left))
	(setf (drag-y app) (- mouse-y obj-top))
	(if (eq (getf data ':event-type) :touch)
	    (progn
	      (set-on-touch-move obj 'on-mouse-move)
	      (set-on-touch-end obj 'stop-obj-grab)
	      (set-on-touch-cancel obj 'on-mouse-leave))
	    (progn
	      (set-on-mouse-move obj 'on-mouse-move)
	      (set-on-mouse-up obj 'stop-obj-grab)
	      (set-on-mouse-leave obj 'on-mouse-leave))))))))

(defun on-mouse-move (obj data)
  (let* ((app (connection-data-item obj "app-data"))
	 (x   (getf data ':screen-x))
	 (y   (getf data ':screen-y)))    
    (setf (top obj) (format nil "~Apx" (- y (drag-y app))))
    (setf (left obj) (format nil "~Apx" (- x (drag-x app))))))

(defun on-mouse-leave (obj)
  (let ((app (connection-data-item obj "app-data")))
    (setf (in-drag-p app) nil)
    (set-on-touch-move obj nil)
    (set-on-touch-end obj nil)
    (set-on-touch-cancel obj nil)
    (set-on-mouse-move obj nil)
    (set-on-mouse-up obj nil)
    (set-on-mouse-leave obj nil)))

(defun stop-obj-grab (obj data)
  (on-mouse-move obj data)
  (on-mouse-leave obj))

(defgeneric create-window (clog-obj title
			   &key html-id content left top width height)
  (:documentation "Create an html-window"))

(defmethod create-window ((obj clog-obj) title &key
						 (html-id nil)
						 (top-bar "")
						 (content "")
						 (left 60)
						 (top 60)
						 (width 400)
						 (height 300))
  (unless html-id
    (setf html-id (clog-connection:generate-id)))
  
  (let* ((app (connection-data-item obj "app-data"))
	 (win (create-child (body app)
	        (format nil
			  "<div style='position:absolute;top:~Apx;left:~Apx;width:~Apx;height:~Apx;'
	                               class='w3-card-4 w3-white w3-border'>
                             <div class='w3-container w3-black' style='cursor: move;'>
                               <span id='~A-title'>~A</span>
                               <span id='~A-close' class='w3-right' style='cursor: pointer;'>X</span>
                               ~A
                             </div>
                             <div id='~A-body' style='right:0;height:100%;margin: 0 auto;'>~A</div>
                           </div>"
			  top left width height html-id title html-id top-bar html-id content)
		:html-id html-id)))
    (set-on-click (attach-as-child obj (format nil "~A-close" html-id))
		  (lambda (obj)
		    (setf (hiddenp win) t)))
    win))

(defun do-ide-file-new (obj)
 (let* ((app (connection-data-item obj "app-data"))
	(win (create-window obj "New window"
				 :left (random 600)
				 :top (+ 40 (random 400)))))
	(create-child obj
		      (format nil
		      "<script>
                         var editor = ace.edit('~A-body');
                         editor.setTheme('ace/theme/xcode');
                         editor.session.setMode('ace/mode/lisp');
                      </script>"
		      (html-id win)))
   (set-on-touch-start win 'on-mouse-down)
   (set-on-mouse-down win 'on-mouse-down)))

(defun do-ide-help-about (obj)
  (let* ((app   (connection-data-item obj "app-data"))
	 (about (create-window (body app) "About"
			       :top-bar "<center><img src='/demo/clogwicon.png'></center>
	                                 <center>CLOG</center>
	                                 <center>The Common Lisp Omnificent GUI</center>"
			       :content "<p><center>Demo 3</center>
                                         <center>(c) 2021 - David Botton</center></p>"
			       :left (- (/ (width (body app)) 2) 100)
			       :width 200
			       :height 200)))
    (set-on-touch-start about 'on-mouse-down)
    (set-on-mouse-down about 'on-mouse-down)))
		    
(defun on-new-window (body)
  (let ((app (make-instance 'app-data)))
    (setf (connection-data-item body "app-data") app)
    (setf (body app) body)
    (set-on-click (attach-as-child body "ide-file-new") #'do-ide-file-new)
    (set-on-click (attach-as-child body "ide-help-about") #'do-ide-help-about)
    (set-on-click (attach-as-child body "ide-logo") #'do-ide-help-about)
    (run body)))

(defun start-demo ()
  "Start demo."
  (initialize #'on-new-window :boot-file "/demo/frame.html")
  (open-browser))
