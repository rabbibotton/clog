;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG - The Common Lisp Omnificent GUI                                 ;;;;
;;;; (c) 2020-2021 David Botton                                            ;;;;
;;;; License BSD 3 Clause                                                  ;;;;
;;;;                                                                       ;;;;
;;;; clog-gui.lisp                                                         ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:in-package :clog)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-gui - Desktop GUI abstraction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-gui ()
  ((body
    :accessor body
    :documentation "Top level access to browser window")
   (current-win
    :accessor current-win
    :initform nil
    :documentation "The current window at front")
   (windows
    :accessor windows
    :initform (make-hash-table :test 'equalp)
    :documentation "Window collection")
   (last-z
    :accessor last-z
    :initform -9999
    :documentation "Top z-order for windows")
   (copy-buf
    :accessor copy-buf
    :initform ""
    :documentation "Copy buffer")
   (in-drag
    :accessor in-drag
    :initform nil
    :documentation "Drag window or Size window")
   (drag-obj
    :accessor drag-obj
    :initform nil
    :documentation "Drag target object")
   (drag-x
    :accessor drag-x
    :documentation "Location of the left side or width relative to pointer during drag")
   (drag-y
    :accessor drag-y
    :documentation "Location of the top or height relative to pointer during drag")
   (on-window-change
    :accessor on-window-change
    :initform nil
    :documentation "Fired when foreground window changed.")))

;;;;;;;;;;;;;;;;;;;;;
;; create-clog-gui ;;
;;;;;;;;;;;;;;;;;;;;;

(defun create-clog-gui (clog-body)
  "Create a clog-gui object and places it in CLOG-BODY's connection-data as
\"clog-gui\". (Private)"
  (let ((clog-gui (make-instance 'clog-gui)))
    (setf (connection-data-item clog-body "clog-gui") clog-gui)
    (setf (body clog-gui) clog-body)
    clog-gui))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; clog-gui-initialize ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun clog-gui-initialize (clog-body &key (w3-css-url "/css/w3.css")
					(jquery-ui-css "/css/jquery-ui.css")
					(jquery-ui "/js/jquery-ui.js"))
  "Initializes clog-gui loading w3.css from :W3-CSS-URL and installs a
clog-gui object on connection."
  (create-clog-gui clog-body)
  (load-script (html-document clog-body) jquery-ui)
  (load-css (html-document clog-body) jquery-ui-css)
  (load-css (html-document clog-body) w3-css-url))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - Menus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;
;; create-gui-menu-bar ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-gui-menu-bar (clog-div)()
  (:documentation "Menu bar"))

(defgeneric create-gui-menu-bar (clog-obj &key class html-id)
  (:documentation "Attached a menu bar to a CLOG-OBJ in general a
clog-body."))

(defmethod create-gui-menu-bar ((obj clog-obj)
				&key (class "w3-bar w3-black w3-card-4")
				  (html-id nil))
  (let ((div (create-div obj :class class :html-id html-id)))
    (change-class div 'clog-gui-menu-bar)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; create-gui-menu-drop-down ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-gui-menu-drop-down (clog-div)()
  (:documentation "Drop down menu"))

(defgeneric create-gui-menu-drop-down (clog-gui-menu-bar
				       &key content class html-id)
  (:documentation "Attached a menu bar drop-down to a CLOG-GUI-MENU-BAR"))

(defmethod create-gui-menu-drop-down ((obj clog-gui-menu-bar)
	          &key (content "")
		    (class "w3-dropdown-content w3-bar-block w3-card-4")
		    (html-id nil))
  (let* ((hover  (create-div obj :class "w3-dropdown-hover"))
	 (button (create-button hover :class "w3-button" :content content))
	 (div    (create-div hover :class class :html-id html-id)))
    (declare (ignore button))
    (change-class div 'clog-gui-menu-drop-down)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; create-gui-menu-item ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-gui-menu-item (clog-span)()
  (:documentation "Menu bar"))

(defgeneric create-gui-menu-item (clog-gui-menu-drop-down
				  &key content
				    on-click
				    class
				    html-id)
  (:documentation "Attached a menu item to a CLOG-GUI-MENU-DROP-DOWN"))

(defmethod create-gui-menu-item ((obj clog-obj)
				 &key (content "")
				   (on-click nil)
				   (class "w3-bar-item w3-button")
				   (html-id nil))
  (let ((span
	  (create-span obj :content content :class class :html-id html-id)))
    (set-on-click span on-click)
    (change-class span 'clog-gui-menu-item)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; create-gui-menu-full-screen ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric create-gui-menu-full-screen (clog-gui-menu-bar &key html-id)
  (:documentation "Add as last item in menu bar to allow for a full screen
icon ⤢ and full screen mode."))

(defmethod create-gui-menu-full-screen ((obj clog-gui-menu-bar)
					&key (html-id nil))
  (create-child obj
    	  " <span class='w3-bar-item w3-right' style='user-select:none;'
	     onClick='if (document.fullscreenElement==null) {
                         documentElement.requestFullscreen()
                      } else {document.exitFullscreen();}'>⤢</span>"
	  :html-id html-id
	  :clog-type 'clog-gui-menu-item))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; create-gui-menu-icon ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric create-gui-menu-icon (clog-gui-menu-bar &key image-url
						      on-click
						      class
						      html-id)
  (:documentation "Add icon as menu bar item."))

(defmethod create-gui-menu-icon ((obj clog-gui-menu-bar)
				 &key (image-url "/img/clogwicon.png")
				   (on-click nil)
				   (class "w3-button w3-bar-item")
				   (html-id nil))
  (set-on-click
   (create-child obj
		 (format nil "<button class='~A'>~
                                <img height=22 src='~A'></button>"
			 class
			 image-url)
		 :html-id html-id
		 :clog-type 'clog-gui-menu-item)
   on-click))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - Window System
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;
;; current-window ;;
;;;;;;;;;;;;;;;;;;;;

(defgeneric current-window (clog-obj)
  (:documentation "Get the current selected clog-gui-window"))

(defmethod current-window ((obj clog-obj))
  (let ((app (connection-data-item obj "clog-gui")))
    (current-win app)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set-on-window-change ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;
  
(defgeneric set-on-window-change (clog-obj handler)
  (:documentation "Set the on-window-change HANDLER.
The on-window-change clog-obj received is the new window"))

(defmethod set-on-window-change ((obj clog-obj) handler)
  (let ((app (connection-data-item obj "clog-gui")))
    (setf (on-window-change app) handler)))

(defmethod fire-on-window-change (obj app)
  "Fire handler if set. Change the value of current-win to clog-obj (Private)"
  (unless obj
    (let (new-order 
	  (order -9999))
      (maphash (lambda (key value)
		 (declare (ignore key))
		 (setf new-order (z-index value))
		 (when (>= new-order order)
		   (setf order new-order)
		   (setf obj value)))
	       (windows app))))
  (setf (current-win app) obj)
  (when (on-window-change app)
    (funcall (on-window-change app) obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - Individual Windows
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-gui-window (clog-element)
  ((win-title
    :accessor win-title
    :documentation "Window title clog-element")
   (title-bar
    :accessor title-bar
    :documentation "Window title-bar clog-element")
   (content
    :accessor content
    :documentation "Window body clog-element")
   (closer
    :accessor closer
    :documentation "Window closer clog-element")
   (sizer
    :accessor sizer
    :documentation "Window sizer clog-element")
   (on-window-can-close
    :accessor on-window-can-close
    :initform nil
    :documentation "Return t to allow close of window")
   (on-window-can-move
    :accessor on-window-can-move
    :initform nil
    :documentation "Return t to allow move of window")
   (on-window-can-size
    :accessor on-window-can-size
    :initform nil
    :documentation "Return t to allow close of window")
   (on-window-close
    :accessor on-window-close
    :initform nil
    :documentation "Fired on window closed")
   (on-window-move
    :accessor on-window-move
    :initform nil
    :documentation "Fired during move of window")
   (on-window-size
    :accessor on-window-size
    :initform nil
    :documentation "Fired during size change of window")
   (on-window-move-done
    :accessor on-window-move-done
    :initform nil
    :documentation "Fired after move of window")
   (on-window-size-done
    :accessor on-window-size-done
    :initform nil
    :documentation "Fired after size change of window")))

;;;;;;;;;;;;;;;;;;;;;;
;; on-gui-drag-down ;;
;;;;;;;;;;;;;;;;;;;;;;

(defun on-gui-drag-down (obj data)
  "Handle mouse down on drag items"
  (let ((app (connection-data-item obj "clog-gui")))
    (unless (in-drag app)
      (setf (in-drag app) (attribute obj "data-drag-type"))
      (let* ((target (gethash (attribute obj "data-drag-obj") (windows app)))
	     (pointer-x (getf data ':screen-x))
	     (pointer-y (getf data ':screen-y))
	     (obj-top)
	     (obj-left)
	     (perform-drag nil))
	(when target
	  (setf (drag-obj app) target)
	  (cond ((equalp (in-drag app) "m")
		 (setf obj-top
		       (parse-integer (top (drag-obj app)) :junk-allowed t))
		 (setf obj-left
		       (parse-integer (left (drag-obj app)) :junk-allowed t))
		 (setf perform-drag (fire-on-window-can-move (drag-obj app))))
		((equalp (in-drag app) "s")
		 (setf obj-top  (height (drag-obj app)))
		 (setf obj-left (width (drag-obj app)))
		 (setf perform-drag (fire-on-window-can-size (drag-obj app))))
		(t
		 (format t "Warning - invalid data-drag-type attribute")))
	  (setf (z-index (drag-obj app)) (incf (last-z app)))
	  (fire-on-window-change (drag-obj app) app)
	  (setf (drag-y app) (- pointer-y obj-top))
	  (setf (drag-x app) (- pointer-x obj-left)))
	(cond (perform-drag
	       (set-on-pointer-move obj 'on-gui-drag-move)
	       (set-on-pointer-up obj 'on-gui-drag-stop))
	      (t
	       (setf (in-drag app) nil)))))))

;;;;;;;;;;;;;;;;;;;;;;
;; on-gui-drag-move ;;
;;;;;;;;;;;;;;;;;;;;;;

(defun on-gui-drag-move (obj data)
  "Handle mouse tracking on drag object"
  (let* ((app (connection-data-item obj "clog-gui"))
	 (x        (getf data ':screen-x))
	 (y        (getf data ':screen-y))
	 (adj-y    (- y (drag-y app)))
	 (adj-x    (- x (drag-x app))))
    (when (and (> adj-x 0) (> adj-y 30))
      (cond ((equalp (in-drag app) "m")
	     (fire-on-window-move (drag-obj app))
	     (setf (top (drag-obj app)) (unit :px adj-y))
	     (setf (left (drag-obj app)) (unit :px adj-x)))
	    ((equalp (in-drag app) "s")
	     (fire-on-window-size (drag-obj app))
	     (setf (height (drag-obj app)) (unit :px adj-y))
	     (setf (width (drag-obj app)) (unit :px adj-x)))))))

;;;;;;;;;;;;;;;;;;;;;;
;; on-gui-drag-stop ;;
;;;;;;;;;;;;;;;;;;;;;;

(defun on-gui-drag-stop (obj data)
  "Handle end of drag object"
  (let ((app (connection-data-item obj "clog-gui")))
    (on-gui-drag-move obj data)
    (set-on-pointer-move obj nil)
    (set-on-pointer-up obj nil)
    (cond ((equalp (in-drag app) "m")
	   (fire-on-window-move-done (drag-obj app)))
	  ((equalp (in-drag app) "s")
	   (fire-on-window-size-done (drag-obj app))))
    (setf (in-drag app) nil)
    (setf (drag-obj app) nil)))

;;;;;;;;;;;;;;;;;;;;;;;
;; create-gui-window ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric create-gui-window (clog-obj &key title
					  content
					  left top width height
					  client-movement
					  html-id)
  (:documentation "Create a clog-gui-window. If client-movement is t then
use jquery-ui to move/resize. When client-movement is t no events will
be fired for size or movement of window that window."))

(defmethod create-gui-window ((obj clog-obj) &key (title "New Window")
					       (content "")
					       (left 60)
					       (top 60)
					       (width 400)
					       (height 300)
					       (client-movement nil)
					       (html-id nil))
  (unless html-id
    (setf html-id (clog-connection:generate-id)))
  
  (let* ((app (connection-data-item obj "clog-gui"))
	 (win (create-child (body app)
	        (format nil
			  "<div style='position:fixed;top:~Apx;left:~Apx;width:~Apx;height:~Apx;
                                       flex-container;display:flex;flex-direction:column;z-index:~A'
	                               class='w3-card-4 w3-white w3-border'>
                             <div id='~A-title-bar' class='w3-container w3-black'
                                   style='flex-container;display:flex;align-items:stretch;'>
                               <span data-drag-obj='~A' data-drag-type='m' id='~A-title'
                                     style='flex-grow:9;user-select:none;cursor:move;'>~A</span>
                               <span id='~A-closer'
                                     style='cursor:pointer;user-select:none;'>X</span>
                             </div>
                             <div id='~A-body' style='flex-grow:9;overflow:auto'>~A</div>
                             <div id='~A-sizer' style='user-select:none;height:3px;
                                                       cursor:se-resize;opacity:0'
                                  class='w3-right' data-drag-obj='~A' data-drag-type='s'>+</div>
                           </div>"
			  top left width height (incf (last-z app)) ; outer div
			  html-id html-id html-id                   ; title bar
			  title html-id                             ; title
			  html-id content                           ; body
			  html-id html-id)                          ; size
		:clog-type 'clog-gui-window
		:html-id html-id)))
    (setf (win-title win)
	  (attach-as-child win (format nil "~A-title" html-id)))
    (setf (title-bar win)
	  (attach-as-child win (format nil "~A-title-bar" html-id)))
    (setf (closer win) (attach-as-child win (format nil "~A-closer" html-id)))
    (setf (sizer win) (attach-as-child win (format nil "~A-sizer" html-id)))
    (setf (content win) (attach-as-child win (format nil "~A-body"  html-id)))
    (set-on-double-click (win-title win) (lambda (obj)
					   (setf (width win) (unit :px 800))
					   (setf (height win) (unit :px 600))))
    (set-on-click (closer win) (lambda (obj)
				 (declare (ignore obj))
				 (when (fire-on-window-can-close win)
				   (remhash (format nil "~A" html-id)
					    (windows app))
				   (remove-from-dom win)
				   (fire-on-window-change nil app)
				   (fire-on-window-close win))))
    (setf (gethash (format nil "~A" html-id) (windows app)) win)
    (fire-on-window-change win app)
    (cond (client-movement
	   (jquery-execute win (format nil "draggable({handle:'#~A-title-bar'})" html-id))
	   (jquery-execute win "resizable({handles:'se'})")
	   (set-on-pointer-down (win-title win)
				(lambda (obj data)
				  (setf (z-index win) (incf (last-z app)))
				  (fire-on-window-change win app)))
	   (set-on-pointer-down (sizer win)
				(lambda (obj data)
				  (print "sizer")
				  (setf (z-index win) (incf (last-z app)))
				  (fire-on-window-change win app))))
	  (t
	   (set-on-pointer-down
	    (win-title win) 'on-gui-drag-down :capture-pointer t)
	   (set-on-pointer-down
	    (sizer win) 'on-gui-drag-down :capture-pointer t)))
    win))

;;;;;;;;;;;;;;;;;;
;; window-title ;;
;;;;;;;;;;;;;;;;;;

(defgeneric window-title (clog-gui-window)
  (:documentation "Get/setf window title"))

(defmethod window-title ((obj clog-gui-window))
  (inner-html (win-title obj)))

(defgeneric set-window-title (clog-gui-window value)
  (:documentation "Set window title"))

(defmethod set-window-title ((obj clog-gui-window) value)
  (setf (inner-html (win-title obj)) value))
(defsetf window-title set-window-title)

;;;;;;;;;;;;;;;;;;;;
;; window-content ;;
;;;;;;;;;;;;;;;;;;;;

(defgeneric window-content (clog-gui-window)
  (:documentation "Get window content element."))

(defmethod window-content ((obj clog-gui-window))
  (content obj))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set-on-window-can-close ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-window-can-close (clog-gui-window handler)
  (:documentation "Set the on-window-can-close HANDLER"))

(defmethod set-on-window-can-close ((obj clog-gui-window) handler)
  (setf (on-window-can-close obj) handler))

(defgeneric fire-on-window-can-close (clog-gui-window)
  (:documentation "Fire handler if set. (Private)"))

(defmethod fire-on-window-can-close ((obj clog-gui-window))
  (if (on-window-can-close obj)
      (funcall (on-window-can-close obj) obj)
      t))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; set-on-window-close ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-window-close (clog-gui-window handler)
  (:documentation "Set the on-window-close HANDLER"))

(defmethod set-on-window-close ((obj clog-gui-window) handler)
  (setf (on-window-close obj) handler))

(defgeneric fire-on-window-close (clog-gui-window)
  (:documentation "Fire handler if set. (Private)"))

(defmethod fire-on-window-close ((obj clog-gui-window))
  (when (on-window-close obj)
    (funcall (on-window-close obj) obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set-on-window-can-size ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-window-can-size (clog-gui-window handler)
  (:documentation "Set the on-window-can-size HANDLER"))

(defmethod set-on-window-can-size ((obj clog-gui-window) handler)
  (setf (on-window-can-size obj) handler))

(defgeneric fire-on-window-can-size (clog-gui-window)
  (:documentation "Fire handler if set. (Private)"))

(defmethod fire-on-window-can-size ((obj clog-gui-window))
  (if (on-window-can-size obj)
      (funcall (on-window-can-size obj) obj)
      t))

;;;;;;;;;;;;;;;;;;;;;;;;
;; set-on-window-size ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-window-size (clog-gui-window handler)
  (:documentation "Set the on-window-size HANDLER"))

(defmethod set-on-window-size ((obj clog-gui-window) handler)
  (setf (on-window-size obj) handler))

(defgeneric fire-on-window-size (clog-gui-window)
  (:documentation "Fire handler if set. (Private)"))

(defmethod fire-on-window-size ((obj clog-gui-window))
  (when (on-window-size obj)
    (funcall (on-window-size obj) obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set-on-window-size-done ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-window-size-done (clog-gui-window handler)
  (:documentation "Set the on-window-size-done HANDLER"))

(defmethod set-on-window-size-done ((obj clog-gui-window) handler)
  (setf (on-window-size-done obj) handler))

(defmethod fire-on-window-size-done ((obj clog-gui-window))
  (when (on-window-size-done obj)
    (funcall (on-window-size-done obj) obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set-on-window-can-move ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-window-can-move (clog-gui-window handler)
  (:documentation "Set the on-window-can-move HANDLER"))

(defmethod set-on-window-can-move ((obj clog-gui-window) handler)
  (setf (on-window-can-move obj) handler))

(defgeneric fire-on-window-can-move (clog-gui-window)
  (:documentation "Fire handler if set. (Private)"))

(defmethod fire-on-window-can-move ((obj clog-gui-window))
  (if (on-window-can-move obj)
      (funcall (on-window-can-move obj) obj)
      t))

;;;;;;;;;;;;;;;;;;;;;;;;
;; set-on-window-move ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-window-move (clog-gui-window handler)
  (:documentation "Set the on-window-move HANDLER"))

(defmethod set-on-window-move ((obj clog-gui-window) handler)
  (setf (on-window-move obj) handler))

(defgeneric fire-on-window-move (clog-gui-window)
  (:documentation "Fire handler if set. (Private)"))

(defmethod fire-on-window-move ((obj clog-gui-window))
  (when (on-window-move obj)
    (funcall (on-window-move obj) obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set-on-window-move-done ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-window-move-done (clog-gui-window handler)
  (:documentation "Set the on-window-move-done HANDLER"))

(defmethod set-on-window-move-done ((obj clog-gui-window) handler)
  (setf (on-window-move-done obj) handler))

(defmethod fire-on-window-move-done ((obj clog-gui-window))
  (when (on-window-move-done obj)
    (funcall (on-window-move-done obj) obj)))
