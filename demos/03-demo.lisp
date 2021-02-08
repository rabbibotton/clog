(defpackage #:clog-user
  (:use #:cl #:clog)
  (:export start-demo))

(in-package :clog-user)

(defclass app-data ()
  ((body
    :accessor body
    :documentation "Top level access to browser window")
   (drag-mutex
    :reader drag-mutex
    :initform (bordeaux-threads:make-lock)
    :documentation "Serialize access to the on-ide-drag-down event.")
   (in-drag
    :accessor in-drag
    :initform nil
    :documentation "Ensure only one box is dragged at a time and type of drag.")
   (drag-x
    :accessor drag-x
    :documentation "The location of the left side of the box relative to pointer during drag.")
   (drag-y
    :accessor drag-y
   :documentation "The location of the top of the box relative to pointer during drag.")))

(defun on-ide-drag-down (obj data)
  (let ((app (connection-data-item obj "app-data")))
    (bordeaux-threads:with-lock-held ((drag-mutex app))
      (unless (in-drag app)
	(setf (in-drag app) (attribute obj "data-drag-type"))
	(let* ((id-drag   (attribute obj "data-drag-obj"))
	       (drag-obj  (attach-as-child obj id-drag))
	       (pointer-x (getf data ':screen-x))
	       (pointer-y (getf data ':screen-y))
	       (obj-top)
	       (obj-left))
	  (if (equalp (in-drag app) "m")
	      (progn
		(setf obj-top  (parse-integer (top drag-obj) :junk-allowed t))
		(setf obj-left (parse-integer (left drag-obj) :junk-allowed t)))
	      (progn
		(setf obj-top  (height drag-obj))
		(setf obj-left (width drag-obj))))
	  (setf (z-index drag-obj) 1)
	  (setf (drag-y app) (- pointer-y obj-top))
	  (setf (drag-x app) (- pointer-x obj-left))
	  (set-on-pointer-move obj 'on-ide-drag-move)
	  (set-on-pointer-up obj 'on-ide-drag-stop))))))

(defun on-ide-drag-move (obj data)
  (let* ((app (connection-data-item obj "app-data"))
	 (drag-obj (attach-as-child obj (attribute obj "data-drag-obj")))	 
	 (x   (getf data ':screen-x))
	 (y   (getf data ':screen-y)))
    (if (equalp (in-drag app) "m")
	(progn
	  (setf (top drag-obj) (format nil "~Apx" (- y (drag-y app))))
	  (setf (left drag-obj) (format nil "~Apx" (- x (drag-x app)))))
	(progn
	  (format t "y = ~A  x = ~A~%" y x)
	  (setf (height drag-obj) (format nil "~Apx" (- y (drag-y app))))
	  (setf (width drag-obj) (format nil "~Apx" (- x (drag-x app))))))))

(defun on-ide-drag-stop (obj data)
  (let ((app (connection-data-item obj "app-data")))
    (on-ide-drag-move obj data)
    (setf (in-drag app) nil)
    (set-on-pointer-move obj nil)
    (set-on-pointer-up obj nil)))

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
			  "<div style='position:fixed;top:~Apx;left:~Apx;width:~Apx;height:~Apx;'
	                               class='w3-card-4 w3-white w3-border'>
                             <div id='~A-title-bar' class='w3-container w3-black'
                                   style='flex-container;display:flex;align-items:stretch;'>
                               <span data-drag-obj='~A' data-drag-type='m' id='~A-title'
                                     style='flex-grow:9;user-select:none;cursor:move;'>~A</span>
                               <span id='~A-close'
                                     style='cursor:pointer;user-select:none;'>X</span>
                               ~A
                             </div>
                             <div id='~A-body' style='right:0;height:100%;margin: 0 auto;'>~A</div>
                             <div id='~A-size' style='user-select:none;cursor:se-resize;opacity:0'
                                  class='w3-right' data-drag-obj='~A' data-drag-type='s'>+</div>
                           </div>"
			  top left width height   ; outer div
			  html-id html-id html-id ; title bar
			  title html-id top-bar   ; title
			  html-id content         ; body
			  html-id html-id)        ; size
		:html-id html-id))
	 (title   (attach-as-child win (format nil "~A-title" html-id)))
	 (close-x (attach-as-child win (format nil "~A-close" html-id)))
	 (sizer   (attach-as-child win (format nil "~A-size" html-id))))
    (set-on-pointer-down title 'on-ide-drag-down :capture-pointer t)
    (set-on-pointer-down sizer 'on-ide-drag-down :capture-pointer t)    
    (set-on-click close-x (lambda (obj)
			    (setf (hiddenp win) t)))
    win))

(defun do-ide-file-new (obj)
 (let* ((app (connection-data-item obj "app-data"))
	(win (create-window obj "New window"
				 :left (random 600)
				 :top  (+ 40 (random 400)))))
	(create-child obj
		      (format nil
		      "<script>
                         var editor = ace.edit('~A-body');
                         editor.setTheme('ace/theme/xcode');
                         editor.session.setMode('ace/mode/lisp');
                      </script>"
		      (html-id win)))))

(defun do-ide-help-about (obj)
  (let* ((app   (connection-data-item obj "app-data"))
	 (about (create-window (body app) "About"
			       :top-bar "<center><img src='/demo/clogwicon.png'></center>
	                                 <center>CLOG</center>
	                                 <center>The Common Lisp Omnificent GUI</center>"
			       :content "<p><center>Demo 3</center>
                                         <center>(c) 2021 - David Botton</center></p>"
			       :left    (- (/ (width (body app)) 2) 100)
			       :width   200
			       :height  200)))))
		    
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
