(defpackage #:clog-user
  (:use #:cl #:clog)
  (:export start-demo))

(in-package :clog-user)

(defvar *last-z* -9999)

(defclass app-data ()
  ((body
    :accessor body
    :documentation "Top level access to browser window")
   (current-win
    :accessor current-win
    :initform nil
    :documentation "The current window at front.")
   (copy-buf
    :accessor copy-buf
    :initform ""
    :documentation "Copy buffer")
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
    :documentation "Location of the left side or width relative to pointer during drag.")
   (drag-y
    :accessor drag-y
   :documentation "Location of the top or height relative to pointer during drag.")))

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
		(setf (current-win app) drag-obj)
		(setf obj-top  (parse-integer (top drag-obj) :junk-allowed t))
		(setf obj-left (parse-integer (left drag-obj) :junk-allowed t)))
	      (progn
		(setf obj-top  (height drag-obj))
		(setf obj-left (width drag-obj))))
	  (setf (z-index drag-obj) (incf *last-z*))
	  (setf (drag-y app) (- pointer-y obj-top))
	  (setf (drag-x app) (- pointer-x obj-left))
	  (set-on-pointer-move obj 'on-ide-drag-move)
	  (set-on-pointer-up obj 'on-ide-drag-stop))))))

(defun on-ide-drag-move (obj data)
  (let* ((app (connection-data-item obj "app-data"))
	 (drag-obj (attach-as-child obj (attribute obj "data-drag-obj")))	 
	 (x        (getf data ':screen-x))
	 (y        (getf data ':screen-y))
	 (adj-y    (- y (drag-y app)))
	 (adj-x    (- x (drag-x app))))
    (when (and (> adj-x 0) (> adj-y 30))
      (if (equalp (in-drag app) "m")
	  (progn
	    (setf (top drag-obj) (format nil "~Apx" adj-y))
	    (setf (left drag-obj) (format nil "~Apx" adj-x)))
	  (progn
	    (js-execute drag-obj (format nil "editor_~A.resize()" (html-id drag-obj)))
	    (setf (height drag-obj) (format nil "~Apx" adj-y))
	    (setf (width drag-obj) (format nil "~Apx" adj-x)))))))

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
			  "<div style='position:fixed;top:~Apx;left:~Apx;width:~Apx;height:~Apx;
                                       flex-container;display:flex;flex-direction:column;z-index:~A'
	                               class='w3-card-4 w3-white w3-border'>
                             <div id='~A-title-bar' class='w3-container w3-black'
                                   style='flex-container;display:flex;align-items:stretch;'>
                               <span data-drag-obj='~A' data-drag-type='m' id='~A-title'
                                     style='flex-grow:9;user-select:none;cursor:move;'>~A</span>
                               <span id='~A-close'
                                     style='cursor:pointer;user-select:none;'>X</span>
                             </div>
                             <div id='~A-body' style='flex-grow:9;'>~A</div>
                             <div id='~A-size' style='user-select:none;height:1px;
                                                      cursor:se-resize;opacity:0'
                                  class='w3-right' data-drag-obj='~A' data-drag-type='s'>+</div>
                           </div>"
			  top left width height (incf *last-z*)  ; outer div
			  html-id html-id html-id                ; title bar
			  title html-id                          ; title
			  html-id content                        ; body
			  html-id html-id)                       ; size
		:html-id html-id))
	 (title   (attach-as-child win (format nil "~A-title" html-id)))
	 (close-x (attach-as-child win (format nil "~A-close" html-id)))
	 (sizer   (attach-as-child win (format nil "~A-size" html-id))))
    (set-on-pointer-down title 'on-ide-drag-down :capture-pointer t)
    (set-on-pointer-down sizer 'on-ide-drag-down :capture-pointer t)    
    (set-on-click close-x (lambda (obj)
			    (remove-from-dom win)))
    win))

(defun do-ide-file-new (obj)
  (let* ((app (connection-data-item obj "app-data"))
	 (win (create-window obj "New window"
			     :left (random 600)
			     :top  (+ 40 (random 400)))))
    (create-child obj
		  (format nil
			  "<script>
                         var editor_~A = ace.edit('~A-body');
                         editor_~A.setTheme('ace/theme/xcode');
                         editor_~A.session.setMode('ace/mode/lisp');
                         editor_~A.session.setTabSize(3);
                      </script>"
			  (html-id win) (html-id win)
			  (html-id win)
			  (html-id win)
			  (html-id win)))
    (setf (current-win app) win)))

(defun do-ide-help-about (obj)
  (let* ((app   (connection-data-item obj "app-data"))
	 (about (create-window (body app) "About"
			       :content "<div class='w3-black'>
                                         <center><img src='/demo/clogwicon.png'></center>
	                                 <center>CLOG</center>
	                                 <center>The Common Lisp Omnificent GUI</center></div>
			                 <div><p><center>Demo 3</center>
                                         <center>(c) 2021 - David Botton</center></p></div>"
			       :left    (- (/ (width (body app)) 2) 100)
			       :width   200
			       :height  200)))
    (setf (current-win app) about)))

(defun do-ide-file-open (obj)
  (do-ide-file-new obj)
  (let* ((app (connection-data-item obj "app-data")))
    (js-execute obj (format nil "editor_~A.setValue('~A')"
			    (html-id (current-win app))
			    (escape-string "(print \"hello!\")")))))
  
(defun do-ide-file-save (obj)
  (let* ((app (connection-data-item obj "app-data")))
    (print (js-query obj (format nil "editor_~A.getValue()"
				 (html-id (current-win app)))))))

(defun do-ide-file-save-as (obj)
  (do-ide-file-save obj))

(defun do-ide-edit-copy (obj)
  (let* ((app (connection-data-item obj "app-data")))
    (setf (copy-buf app) (js-query obj (format nil "editor_~A.getCopyText();"
					       (html-id (current-win app)))))))

(defun do-ide-edit-cut (obj)
  (do-ide-edit-copy obj)
  (let* ((app (connection-data-item obj "app-data")))
    (js-execute obj (format nil "editor_~A.execCommand('cut')"
			    (html-id (current-win app))))))

(defun do-ide-edit-paste (obj)
  (let* ((app (connection-data-item obj "app-data")))
    (js-execute obj (format nil "editor_~A.execCommand('paste', '~A')"
			    (html-id (current-win app))
			    (escape-string (copy-buf app))))))

(defun on-new-window (body)
  (let ((app (make-instance 'app-data)))
    (setf (connection-data-item body "app-data") app)
    (setf (body app) body)
    (set-on-click (attach-as-child body "ide-logo") #'do-ide-help-about)
    (set-on-click (attach-as-child body "ide-file-new") #'do-ide-file-new)
    (set-on-click (attach-as-child body "ide-file-open") #'do-ide-file-open)
    (set-on-click (attach-as-child body "ide-file-save") #'do-ide-file-save)
    (set-on-click (attach-as-child body "ide-file-save-as") #'do-ide-file-save-as)
    (set-on-click (attach-as-child body "ide-edit-copy") #'do-ide-edit-copy)
    (set-on-click (attach-as-child body "ide-edit-cut") #'do-ide-edit-cut)
    (set-on-click (attach-as-child body "ide-edit-paste") #'do-ide-edit-paste)    
    (set-on-click (attach-as-child body "ide-help-about") #'do-ide-help-about)
    (run body)))

(defun start-demo ()
  "Start demo."
  (initialize #'on-new-window :boot-file "/demo/frame.html")
  (open-browser))
