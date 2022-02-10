;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG - The Common Lisp Omnificent GUI                                 ;;;;
;;;; (c) 2020-2022 David Botton                                            ;;;;
;;;; License BSD 3 Clause                                                  ;;;;
;;;;                                                                       ;;;;
;;;; clog-gui.lisp                                                         ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Like clog-web, clog-gui uses w3.css as the underlying framework. w3.css is
;;; a public domain css only framework for layouts, is fast and efficient and
;;; does not require additional components outside of the css file. In addition
;;; clog-gui uses jQueryUI and its default css file to provide client side
;;; movement when needed, if client side movement is not used it is possible
;;; to pass nil to the initilization function for both.

(mgl-pax:define-package :clog-gui
  (:documentation "CLOG-GUI a desktop GUI abstraction for CLOG")
  (:use #:cl #:parse-float #:clog #:mgl-pax))

(cl:in-package :clog-gui)

(defsection @clog-gui (:title "CLOG GUI Objects")
  "CLOG-GUI - Desktop GUI abstraction for CLOG"
  (clog-gui-initialize function)

  "CLOG-GUI - Menus"
  (clog-gui-menu-bar             class)
  (create-gui-menu-bar           generic-function)
  (clog-gui-menu-drop-down       class)
  (create-gui-menu-drop-down     generic-function)
  (clog-gui-menu-item            class)
  (create-gui-menu-item          generic-function)
  (create-gui-menu-window-select generic-function)
  (create-gui-menu-full-screen   generic-function)
  (create-gui-menu-icon          generic-function)

  "CLOG-GUI - Window System"
  (current-window              generic-function)
  (menu-bar                    generic-function)
  (menu-bar-height             generic-function)
  (window-collection           generic-function)
  (maximize-all-windows        generic-function)
  (normalize-all-windows       generic-function)
  (set-on-window-change        generic-function)

  "CLOG-GUI - Individual Windows"
  (clog-gui-window             class)
  (create-gui-window           generic-function)
  (window-title                generic-function)
  (window-content              generic-function)
  (window-focus                generic-function)
  (window-close                generic-function)
  (window-maximized-p          generic-function)
  (window-maximize             generic-function)
  (window-normalize            generic-function)
  (window-toggle-maximize      generic-function)
  (window-keep-on-top          generic-function)
  (window-make-modal           generic-function)
  (window-end-modal            generic-function)
  (window-center               generic-function)

  "CLOG-GUI - Individual Window Events"
  (set-on-window-focus         generic-function)
  (set-on-window-blur          generic-function)
  (set-on-window-can-close     generic-function)
  (set-on-window-close         generic-function)
  (set-on-window-can-maximize  generic-function)
  (set-on-window-can-normalize generic-function)
  (set-on-window-can-move      generic-function)
  (set-on-window-can-size      generic-function)
  (set-on-window-move          generic-function)
  (set-on-window-size          generic-function)
  (set-on-window-move-done     generic-function)
  (set-on-window-size-done     generic-function)

  "CLOG-GUI - Dialog Boxes"
  (alert-toast                 function)
  (alert-dialog                function)
  (input-dialog                function)
  (confirm-dialog              function)
  (form-dialog                 function)
  (server-file-dialog          function))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-gui - Desktop GUI abstraction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant top-bar-height 20 "Overlap on new windows with nil set for top")

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
    :initform (make-hash-table* :test 'equalp)
    :documentation "Window collection indexed by html-id")
   (last-z
    :accessor last-z
    :initform -9999
    :documentation "Top z-order for windows")
   (last-x
    :accessor last-x
    :initform 0
    :documentation "Last default open x point")
   (last-y
    :accessor last-y
    :initform 0
    :documentation "Last default open y point")
   (modal-background
    :accessor modal-background
    :initform nil
    :documentation "Modal Background")
   (modal-count
    :accessor modal-count
    :initform 0
    :documentation "Count of nested modal windows")
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
   (menu
    :accessor menu
    :initform nil
    :documentation "Installed menu bar if installed")
   (window-select
    :accessor window-select
    :initform nil
    :documentation "If installed a drop down that selects window to maximize")
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
  "Initializes clog-gui and installs a clog-gui object on connection.
If W3-CSS-URL has not been loaded before is installed unless is nil."
  (create-clog-gui clog-body)
  (set-on-full-screen-change (html-document clog-body)
			     (lambda (obj)
			       (when (current-window obj)
				 (when (window-maximized-p (current-window obj))
				   (window-normalize (current-window obj))
				   (window-maximize (current-window obj))))))
  (set-on-orientation-change (window clog-body)
			     (lambda (obj)
			       (when (current-window obj)
				 (when (window-maximized-p (current-window obj))
				   (window-normalize (current-window obj))
				   (window-maximize (current-window obj))))))
  (unless (connection-data-item clog-body "w3-css")
    (when w3-css-url
      (setf (connection-data-item clog-body "w3-css") t)
      (load-css (html-document clog-body) w3-css-url)))
  (when jquery-ui-css
    (load-css (html-document clog-body) jquery-ui-css))
  (when jquery-ui
    (load-script (html-document clog-body) jquery-ui)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - Menus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
;; menu-bar ;;
;;;;;;;;;;;;;;

(defgeneric menu-bar (clog-obj)
  (:documentation "Get/setf window menu-bar. This is set buy
create-gui-menu-bar."))

(defmethod menu-bar ((obj clog-obj))
  (let ((app (connection-data-item obj "clog-gui")))
    (menu app)))

(defgeneric set-menu-bar (clog-obj value)
  (:documentation "Set window menu-bar"))

(defmethod set-menu-bar ((obj clog-obj) value)
  (let ((app (connection-data-item obj "clog-gui")))
    (setf (menu app) value)))
(defsetf menu-bar set-menu-bar)

;;;;;;;;;;;;;;;;;;;;;
;; menu-bar-height ;;
;;;;;;;;;;;;;;;;;;;;;

(defgeneric menu-bar-height (clog-obj)
  (:documentation "Get menu-bar height"))

(defmethod menu-bar-height ((obj clog-obj))
  (let ((app (connection-data-item obj "clog-gui")))
    (if (menu app)
	(height (menu app))
	0)))

;;;;;;;;;;;;;;;;;;;;;;;
;; window-collection ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric window-collection (clog-obj)
  (:documentation "Get hash table of open windows"))

(defmethod window-collection ((obj clog-obj))
  (let ((app (connection-data-item obj "clog-gui")))
    (windows app)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; maximize-all-windows ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric maximize-all-windows (clog-obj)
  (:documentation "Maximize all windows"))

(defmethod maximize-all-windows ((obj clog-obj))
  (let ((app (connection-data-item obj "clog-gui")))
    (maphash (lambda (key value)
	       (declare (ignore key))
	       (window-maximize value))
	     (windows app))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; normalize-all-windows ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric normalize-all-windows (clog-obj)
  (:documentation "Normalize all windows"))

(defmethod normalize-all-windows ((obj clog-obj))
  (let ((app (connection-data-item obj "clog-gui")))
    (maphash (lambda (key value)
	       (declare (ignore key))
	       (window-normalize value))
	     (windows app))))

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
  (let ((div (create-div obj :class class :html-id html-id))
	(app (connection-data-item obj "clog-gui")))
    (change-class div 'clog-gui-menu-bar)
    (setf (menu app) div)
    div))

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
  (:documentation "Menu item"))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; create-gui-menu-item ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-gui-menu-item (clog-span)()
  (:documentation "Menu item"))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; create-gui-menu-window-select ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-gui-menu-window-select (clog-select)()
  (:documentation "Drop down containing windows. Selecting a window
will maximize it on top."))

(defgeneric create-gui-menu-window-select (clog-obj
					   &key class
					     html-id)
  (:documentation "Attached a clog-select as a menu item that auto updates
with open windows and maximizes them unless is a keep-on-top window or
on-window-can-maximize returns nil. Only one instance allowed."))

(defmethod create-gui-menu-window-select
    ((obj clog-obj)
     &key (class "w3-select")
       (html-id nil))
  (let ((window-select (create-select obj :class class :html-id html-id))
	(app           (connection-data-item obj "clog-gui")))
    (change-class window-select 'clog-gui-menu-window-select)
    (setf (window-select app) window-select)
    (set-on-change window-select (lambda (obj)
				   (let ((win (gethash (value obj) (windows app))))
				     (when win
				       (unless (keep-on-top win)
					 (window-maximize win))))))
    (create-option window-select :content "Select Window")
    window-select))

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
  (when (current-win app)
    (fire-on-window-blur (current-win app)))
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
    (funcall (on-window-change app) obj))
  (when obj
    (fire-on-window-focus obj)))

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
   (pinner
    :accessor pinner
    :initform nil
    :documentation "Window pinner clog-element if created with has-pinner")
   (closer
    :accessor closer
    :documentation "Window closer clog-element")
   (sizer
    :accessor sizer
    :documentation "Window sizer clog-element")
   (last-width
    :accessor last-width
    :initform nil
    :documentation "Last width before maximize")
   (last-height
    :accessor last-height
    :initform nil
    :documentation "Last heigth before maximize")
   (last-x
    :accessor last-x
    :initform nil
    :documentation "Last x before maximize")
   (last-y
    :accessor last-y
    :initform nil
    :documentation "Last y before maximize")
   (pinnedp
    :accessor pinnedp
    :initform nil
    :documentation "True if this window is pinned and nil otherwise")
   (keep-on-top
    :accessor keep-on-top
    :initform nil
    :documentation "If t don't change z-order")
   (window-select-item
    :accessor window-select-item
    :initform nil
    :documentation "Item in window select")
   (on-window-can-close
    :accessor on-window-can-close
    :initform nil
    :documentation "Return t to allow closing of window")
   (on-window-can-move
    :accessor on-window-can-move
    :initform nil
    :documentation "Return t to allow moving of window")
   (on-window-can-size
    :accessor on-window-can-size
    :initform nil
    :documentation "Return t to allow sizing of window")
   (on-window-can-maximize
    :accessor on-window-can-maximize
    :initform nil
    :documentation "Return t to allow maximizing of window")
   (on-window-can-normalize
    :accessor on-window-can-normalize
    :initform nil
    :documentation "Return t to allow normalizing of window")
   (on-window-focus
    :accessor on-window-focus
    :initform nil
    :documentation "Fired on window focused")
   (on-window-blur
    :accessor on-window-blur
    :initform nil
    :documentation "Fired on window blurred")
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
	  (unless (keep-on-top (drag-obj app))
	    (setf (z-index (drag-obj app)) (incf (last-z app))))
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
    (cond ((equalp (in-drag app) "m")
	   (fire-on-window-move (drag-obj app))
	   (setf (top (drag-obj app)) (unit :px adj-y))
	   (setf (left (drag-obj app)) (unit :px adj-x)))
	  ((equalp (in-drag app) "s")
	   (fire-on-window-size (drag-obj app))
	   (setf (height (drag-obj app)) (unit :px adj-y))
	   (setf (width (drag-obj app)) (unit :px adj-x))))))

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
					  maximize
					  has-pinner
					  keep-on-top
					  hidden
					  client-movement
					  html-id)
  (:documentation "Create a clog-gui-window. If client-movement is t then
use jquery-ui to move/resize and will not work on mobile. When client-movement
is t only on-window-move is fired once at start of drag and on-window-move-done
at end of drag and on-window-resize at start of resize and
on-window-resize-done at end of resize. If has-pinner a toggle wil appear on 
title bar to allow pinning the window in place, if keep-on-top t then when
pinned also will keep-on-top. If had-pinned is nil and keep-on-top t then
the window will be set to keep-on-top always."))

(defmethod create-gui-window ((obj clog-obj) &key (title "New Window")
					       (content "")
					       (left nil)
					       (top nil)
					       (width 300)
					       (height 200)
					       (maximize nil)
			                       (has-pinner nil)
					       (keep-on-top nil)
					       (hidden nil)
					       (client-movement nil)
					       (html-id nil))
  (let ((app (connection-data-item obj "clog-gui")))
    (unless html-id
      (setf html-id (clog-connection:generate-id)))
    (when (eql (hash-table-count (windows app)) 0)
      ;; If previously no open windows reset default position
      (setf (last-x app) 0)
      (setf (last-y app) 0))
    (unless left
      ;; Generate sensible initial x location
      (setf left (last-x app))
      (incf (last-x app) 10))
    (unless top
      ;; Generate sensible initial y location
      (when (eql (last-y app) 0)
	(setf (last-y app) (menu-bar-height obj)))
      (setf top (last-y app))
      (incf (last-y app) top-bar-height)
      (when (> top (- (inner-height (window (body app))) (last-y app)))
	(setf (last-y app) (menu-bar-height obj))))
    (let ((win (create-child (body app)
			    (format nil
	    "<div style='position:fixed;top:~Apx;left:~Apx;width:~Apx;height:~Apx;
                  z-index:~A;visibility:hidden'
                  class='w3-card-4 w3-white w3-border'>
                  <div id='~A-title-bar' class='w3-container w3-black'
                       style='position:absolute;top:0;right:0;left:0;height:25px'>
                    <span data-drag-obj='~A' data-drag-type='m' id='~A-title'
                      style='position:absolute;top:0;right:20px;left:5px;
                             user-select:none;cursor:move;'>~A</span>~A
                    <span id='~A-closer'
                      style='position:absolute;top:0;right:5px;cursor:pointer;user-select:none;'>&times;</span>
                  </div>
                  <div id='~A-body' style='position:absolute;top:25px;left:0;right:0;bottom:3px;overflow:auto'>~A</div>
                  <div id='~A-sizer' style='position:absolute;right:0;bottom:0;left:0;user-select:none;height:3px;
                       cursor:se-resize;opacity:0'
                       class='w3-right' data-drag-obj='~A' data-drag-type='s'>+</div>
             </div>"
	    top left width height (incf (last-z app))   ; outer div
	    html-id html-id html-id                     ; title bar
	    title                                       ; title
	    (if has-pinner                              ; pinner
	      (format nil "<span id='~A-pinner'
                 style='position:absolute;top:0;right:20px;
                        cursor:pointer;user-select:none;'>
                 ☐</span><span>&nbsp;&nbsp;&nbsp;</span>" html-id)
	      "")
	    html-id                                     ; closer
	    html-id content                             ; body
	    html-id html-id)                            ; size
			    :clog-type 'clog-gui-window
			    :html-id html-id)))
      (setf (win-title win)
	    (attach-as-child win (format nil "~A-title" html-id)))
      (setf (title-bar win)
	    (attach-as-child win (format nil "~A-title-bar" html-id)))
      (when has-pinner
	(setf (pinner win) (attach-as-child win (format nil "~A-pinner" html-id))))
      (setf (closer win) (attach-as-child win (format nil "~A-closer" html-id)))
      (setf (sizer win) (attach-as-child win (format nil "~A-sizer" html-id)))
      (setf (content win) (attach-as-child win (format nil "~A-body"  html-id)))
      (setf (gethash (format nil "~A" html-id) (windows app)) win)
      (if maximize
	  (window-maximize win)
	  (fire-on-window-change win app))
      (unless hidden
	(setf (visiblep win) t))
      (when (window-select app)
	(setf (window-select-item win) (create-option (window-select app)
						      :content title
						      :value html-id)))
      (set-on-double-click (win-title win) (lambda (obj)
					     (declare (ignore obj))
					     (window-toggle-maximize win)))
      (if has-pinner
	  (set-on-click (pinner win) (lambda (obj)
       				       (declare (ignore obj))
       				       (window-toggle-pin win keep-on-top)))
	  (when keep-on-top
	    (window-keep-on-top win)))      
      (set-on-click (closer win) (lambda (obj)
				   (declare (ignore obj))
				   (when (fire-on-window-can-close win)
				     (window-close win))))
      (cond (client-movement
	     (clog::jquery-execute win
			     (format nil "draggable({handle:'#~A-title-bar'})" html-id))
	     (clog::jquery-execute win "resizable({handles:'se'})")
	     (set-on-pointer-down (win-title win)
	    			  (lambda (obj data)
				    (declare (ignore obj) (ignore data))
	    			    (setf (z-index win) (incf (last-z app)))
	    			    (fire-on-window-change win app)))
	     (clog::set-on-event win "dragstart"
				 (lambda (obj)
				   (declare (ignore obj))
				   (fire-on-window-move win)))
	     (clog::set-on-event win "dragstop"
				 (lambda (obj)
				   (fire-on-window-move-done win)))
	     (clog::set-on-event win "resizestart"
				 (lambda (obj)
				   (declare (ignore obj))
				   (fire-on-window-size win)))
	     (clog::set-on-event win "resizestop"
				 (lambda (obj)
				   (declare (ignore obj))
				   (fire-on-window-size-done win))))
	    (t
	     (set-on-pointer-down
	      (win-title win) 'on-gui-drag-down :capture-pointer t)
	     (set-on-pointer-down
	      (sizer win) 'on-gui-drag-down :capture-pointer t)))
      win)))

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
  (when (window-select-item obj)
    (setf (inner-html (window-select-item obj)) value))
  (setf (inner-html (win-title obj)) value))
(defsetf window-title set-window-title)

;;;;;;;;;;;;;;;;;;;;
;; window-content ;;
;;;;;;;;;;;;;;;;;;;;

(defgeneric window-content (clog-gui-window)
  (:documentation "Get window content element."))

(defmethod window-content ((obj clog-gui-window))
  (content obj))

;;;;;;;;;;;;;;;;;;
;; window-focus ;;
;;;;;;;;;;;;;;;;;;

(defgeneric window-focus (clog-gui-window)
  (:documentation "Set CLOG-GUI-WINDOW as focused window."))

(defmethod window-focus ((obj clog-gui-window))
  (let ((app (connection-data-item obj "clog-gui")))
    (when (keep-on-top obj)
      (setf (keep-on-top obj) nil))
    (setf (z-index obj) (incf (last-z app)))
    (fire-on-window-change obj app)))

;;;;;;;;;;;;;;;;;;
;; window-close ;;
;;;;;;;;;;;;;;;;;;

(defgeneric window-close (clog-gui-window)
  (:documentation "Close CLOG-GUI-WINDOW. on-window-can-close is not called.
CLOG-GUI-WINDOW is removed from DOM but still present in the CLOG cache on
the browser."))

(defmethod window-close ((obj clog-gui-window))
  (let ((app (connection-data-item obj "clog-gui")))
    (remhash (format nil "~A" (html-id obj)) (windows app))
    (when (window-select app)
      (destroy (window-select-item obj)))
    (remove-from-dom obj)
    (fire-on-window-change nil app)
    (fire-on-window-close obj)))

;;;;;;;;;;;;;;;;;;;;;;;;
;; window-maximized-p ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric window-maximized-p (clog-gui-window)
  (:documentation "Set CLOG-GUI-WINDOW as maximized window."))

(defmethod window-maximized-p ((obj clog-gui-window))
  (last-width obj))

;;;;;;;;;;;;;;;;;;;;;
;; window-maximize ;;
;;;;;;;;;;;;;;;;;;;;;

(defgeneric window-maximize (clog-gui-window)
  (:documentation "Set CLOG-GUI-WINDOW as maximized window."))

(defmethod window-maximize ((obj clog-gui-window))
  (let ((app (connection-data-item obj "clog-gui")))
    (unless (keep-on-top obj)
      (window-focus obj))
    (when (fire-on-window-can-maximize obj)
      (unless (window-maximized-p obj)
	(setf (last-x obj) (left obj))
	(setf (last-y obj) (top obj))
	(setf (last-height obj) (height obj))
	(setf (last-width obj) (width obj)))
      (setf (top obj) (unit :px (menu-bar-height obj)))
      (setf (left obj) (unit :px 0))
      (setf (width obj) (unit :vw 100))
      (setf (height obj)
	    (- (inner-height (window (body app))) (menu-bar-height obj)))
      (fire-on-window-size-done obj))))

;;;;;;;;;;;;;;;;;;;;;;
;; window-normalize ;;
;;;;;;;;;;;;;;;;;;;;;;

(defgeneric window-normalize (clog-gui-window)
  (:documentation "Set CLOG-GUI-WINDOW as normalized window."))

(defmethod window-normalize ((obj clog-gui-window))
  (unless (keep-on-top obj)
    (window-focus obj))
  (when (fire-on-window-can-normalize obj)
    (when (window-maximized-p obj)
      (setf (width obj) (last-width obj))
      (setf (height obj) (last-height obj))
      (setf (top obj) (last-y obj))
      (setf (left obj) (last-x obj))
      (setf (last-width obj) nil)
      (fire-on-window-size-done obj))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; window-toggle-maximize ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric window-toggle-maximize (clog-gui-window)
  (:documentation "Toggle CLOG-GUI-WINDOW as maximize window."))

(defmethod window-toggle-maximize ((obj clog-gui-window))
  (if (window-maximized-p obj)
      (window-normalize obj)
      (window-maximize obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; window-toggle-pinned ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric window-toggle-pin (clog-gui-window keep-on-top)
  (:documentation "Toggle the pinned state of a CLOG-GUI-WINDOW. A pinned
window cannot be moved, closed, resized, maximized or normalized and if
keep-on-top t when pinned is always on top. A new window is always unpinned."))

(defmethod window-toggle-pin ((win clog-gui-window) keep-on-top)
  (if (pinnedp win)
      (progn
	(when (pinner win)
	  (setf (inner-html (pinner win)) "☐"))
	(when keep-on-top
	  (setf (keep-on-top win) nil)
	  (window-focus win))
	(setf (pinnedp win) nil)
	(set-on-window-can-close win nil)
	(set-on-window-can-size win nil)
	(set-on-window-can-move win nil)
	(set-on-window-can-maximize win nil)
	(set-on-window-can-normalize win nil))
      (flet ((no-op (obj) (declare (ignore obj))))
	(when (pinner win)
	  (setf (inner-html (pinner win)) "☑"))
	(when keep-on-top
	  (setf (keep-on-top win) t)
	  (setf (z-index win) 1))
	(setf (pinnedp win) t)
	(set-on-window-can-close win #'no-op)
	(set-on-window-can-size win #'no-op)
	(set-on-window-can-move win #'no-op)
	(set-on-window-can-maximize win #'no-op)
	(set-on-window-can-normalize win #'no-op))))

;;;;;;;;;;;;;;;;;;;;;;;;
;; window-keep-on-top ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric window-keep-on-top (clog-gui-window)
  (:documentation "Set CLOG-GUI-WINDOW to stay on top. Use window-focus to undue."))

(defmethod window-keep-on-top ((obj clog-gui-window))
  (setf (keep-on-top obj) t)
  (setf (z-index obj) 1))

;;;;;;;;;;;;;;;;;;;;;;;
;; window-make-modal ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric window-make-modal (clog-gui-window)
  (:documentation "Set CLOG-GUI-WINDOW to stay on top and prevent all other
interactions. Use window-end-modal to undo."))

(defmethod window-make-modal ((obj clog-gui-window))
  (let ((app (connection-data-item obj "clog-gui")))
    (when (= (modal-count app) 0)
      (setf (modal-background app) (create-div (body app) :class "w3-overlay"))
      (setf (display (modal-background app)) :block))
    (incf (modal-count app))
    (setf (keep-on-top obj) t)
    (setf (z-index obj) 4)))

;;;;;;;;;;;;;;;;;;;;;;
;; window-end-modal ;;
;;;;;;;;;;;;;;;;;;;;;;

(defgeneric window-end-modal (clog-gui-window)
  (:documentation "Set CLOG-GUI-WINDOW to end modal state."))

(defmethod window-end-modal ((obj clog-gui-window))
  (let ((app (connection-data-item obj "clog-gui")))
    (decf (modal-count app))
    (when (<= (modal-count app) 0)
      (destroy (modal-background app)))
    (window-focus obj)))

;;;;;;;;;;;;;;;;;;;
;; window-center ;;
;;;;;;;;;;;;;;;;;;;

(defgeneric window-center (clog-gui-window)
  (:documentation "Center CLOG-GUI-WINDOW in browser."))

(defmethod window-center ((obj clog-gui-window))
  (let ((body (connection-data-item obj "clog-body")))
    (setf (top obj) (unit :px (- (/ (inner-height (window body)) 2.0)
				 (/ (height obj) 2.0))))
    (setf (left obj) (unit :px (- (/ (inner-width (window body)) 2.0)
				  (/ (width obj) 2.0))))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; set-on-window-focus ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-window-focus (clog-gui-window handler)
  (:documentation "Set the on-window-focus HANDLER"))

(defmethod set-on-window-focus ((obj clog-gui-window) handler)
  (setf (on-window-focus obj) handler))

(defgeneric fire-on-window-focus (clog-gui-window)
  (:documentation "Fire handler if set. (Private)"))

(defmethod fire-on-window-focus ((obj clog-gui-window))
  (when (on-window-focus obj)
    (funcall (on-window-focus obj) obj)))

;;;;;;;;;;;;;;;;;;;;;;;;
;; set-on-window-blur ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-window-blur (clog-gui-window handler)
  (:documentation "Set the on-window-blur HANDLER"))

(defmethod set-on-window-blur ((obj clog-gui-window) handler)
  (setf (on-window-blur obj) handler))

(defgeneric fire-on-window-blur (clog-gui-window)
  (:documentation "Fire handler if set. (Private)"))

(defmethod fire-on-window-blur ((obj clog-gui-window))
  (when (on-window-blur obj)
    (funcall (on-window-blur obj) obj)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set-on-window-can-maximize ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-window-can-maximize (clog-gui-window handler)
  (:documentation "Set the on-window-can-maximize HANDLER"))

(defmethod set-on-window-can-maximize ((obj clog-gui-window) handler)
  (setf (on-window-can-maximize obj) handler))

(defgeneric fire-on-window-can-maximize (clog-gui-window)
  (:documentation "Fire handler if set. (Private)"))

(defmethod fire-on-window-can-maximize ((obj clog-gui-window))
  (if (on-window-can-maximize obj)
      (funcall (on-window-can-maximize obj) obj)
      t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set-on-window-can-normalize ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-window-can-normalize (clog-gui-window handler)
  (:documentation "Set the on-window-can-normalize HANDLER"))

(defmethod set-on-window-can-normalize ((obj clog-gui-window) handler)
  (setf (on-window-can-normalize obj) handler))

(defgeneric fire-on-window-can-normalize (clog-gui-window)
  (:documentation "Fire handler if set. (Private)"))

(defmethod fire-on-window-can-normalize ((obj clog-gui-window))
  (if (on-window-can-normalize obj)
      (funcall (on-window-can-normalize obj) obj)
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
  (if (< (parse-integer (top obj) :junk-allowed t)
	 (menu-bar-height obj))
      (setf (top obj) (unit "px" (menu-bar-height obj))))
  (when (on-window-move-done obj)
    (funcall (on-window-move-done obj) obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - Dialog Boxes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;
;; alert-toast ;;
;;;;;;;;;;;;;;;;;

(defun alert-toast (obj title content &key
					(color-class "w3-red")
					(time-out nil)
					(place-top nil)
					(html-id nil))
  "Create an alert toast with option :TIME-OUT. If place-top is t then alert
is placed in DOM at top of html body instead of bottom of html body."
  (unless html-id
      (setf html-id (clog-connection:generate-id)))
  (let* ((body   (connection-data-item obj "clog-body"))
	 (win    (create-child body
			     (format nil
"  <div class='w3-panel ~A w3-animate-right w3-display-container'>~
   <span id=~A-close class='w3-button w3-large w3-display-topright'>&times;</span>~
   <h3>~A</h3>~
   <p>~A</p>~
</div>"
                              color-class
			      html-id
			      title
			      content)
			     :html-id html-id
			     :auto-place nil)))
    (if place-top
	(place-inside-top-of body win)
	(place-inside-bottom-of body win))
    (set-on-click
     (attach-as-child obj (format nil "~A-close" html-id))
     (lambda (obj)
       (declare (ignore obj))
       (destroy win)))
    (when time-out
      (sleep time-out)
      (destroy win))))

;;;;;;;;;;;;;;;;;;
;; alert-dialog ;;
;;;;;;;;;;;;;;;;;;

(defun alert-dialog (obj content &key (modal t)
				   (title "About")
				   (left nil) (top nil)
				   (width 300) (height 200)
				   (client-movement nil)
				   (html-id nil))
  "Create an alert dialog box with CONTENT centered."
  (unless html-id
      (setf html-id (clog-connection:generate-id)))
  (let* ((body (connection-data-item obj "clog-body"))
	 (win  (create-gui-window obj
				  :title    title
				  :content  (format nil
"<div class='w3-panel'>
<center>~A<br><br>
<button class='w3-button w3-black' id='~A-btn'>OK</button>
</center>
</div>" content html-id)
				  :top             top
				  :left            left
				  :width           width
				  :height          height
				  :hidden          t
				  :client-movement client-movement
				  :html-id         html-id))
	 (btn  (attach-as-child win (format nil "~A-btn" html-id))))
    (unless top
      (setf (top win) (unit :px (- (/ (inner-height (window body)) 2.0)
				   (/ (height win) 2.0)))))
    (unless left
      (setf (left win) (unit :px (- (/ (inner-width (window body)) 2.0)
				    (/ (width win) 2.0)))))
    (setf (visiblep win) t)
    (when modal
      (window-make-modal win))
    (focus btn)
    (set-on-click btn (lambda (obj)
			(declare (ignore obj))
			(window-end-modal win)
			(window-close win)))
    (set-on-window-close win (lambda (obj)
			       (declare (ignore obj))
			       (when modal
				 (window-end-modal win))))))

;;;;;;;;;;;;;;;;;;
;; input-dialog ;;
;;;;;;;;;;;;;;;;;;

(defun input-dialog (obj content on-input &key (modal t)
					    (title "Input")
					    (size 20)
					    (rows 1)
					    (default-value "")
					    (left nil) (top nil)
					    (width 300) (height 200)
					    (client-movement nil)
					    (html-id nil))
  "Create an input dialog box with CONTENT centered and an input box.
Calls on-input with input box contents or nil if canceled."
  (unless html-id
      (setf html-id (clog-connection:generate-id)))
  (let* ((body (connection-data-item obj "clog-body"))
	 (inp  (if (eql rows 1)
		   (format nil "<input type='text' id='~A-input' size='~A' value='~A'>"
			   html-id
			   size
			   (escape-string default-value))
		   (format nil "<textarea id='~A-input' cols='~A' rows='~A'>~A</textarea>"
			   html-id
			   size
			   rows
			   (escape-string default-value))))
	 (win  (create-gui-window obj
				  :title          title
				  :content        (format nil
"<div class='w3-panel'>
<center>~A<br><br>
<form class='w3-container' onSubmit='return false;'>
~A<br><br>
<button class='w3-button w3-black' style='width:7em' id='~A-ok'>OK</button>
<button class='w3-button w3-black' style='width:7em' id='~A-cancel'>Cancel</button>
</form>
</center>
</div>"
      content
      inp
      html-id  ; ok
      html-id) ; cancel
				  :top             top
				  :left            left
				  :width           width
				  :height          height
				  :hidden          t
				  :client-movement client-movement
				  :html-id         html-id))
	 (input  (attach-as-child win (format nil "~A-input" html-id)
				  :clog-type 'clog:clog-form-element))
	 (ok     (attach-as-child win (format nil "~A-ok" html-id)))
	 (cancel (attach-as-child win (format nil "~A-cancel" html-id))))
    (unless top
      (setf (top win) (unit :px (- (/ (inner-height (window body)) 2.0)
				   (/ (height win) 2.0)))))
    (unless left
      (setf (left win) (unit :px (- (/ (inner-width (window body)) 2.0)
				    (/ (width win) 2.0)))))
    (setf (visiblep win) t)
    (when modal
      (window-make-modal win))
    (focus input)
    (set-on-click cancel (lambda (obj)
			   (declare (ignore obj))
			   (window-close win))
		  :one-time t)
    (set-on-click ok (lambda (obj)
		       (declare (ignore obj))
		       (set-on-window-close win nil)
		       (when modal
			 (window-end-modal win))
		       (window-close win)
		       (funcall on-input (value input)))
		  :one-time t)
    (set-on-window-close win (lambda (obj)
			       (declare (ignore obj))
			       (when modal
				 (window-end-modal win))
			       (funcall on-input nil)))))

;;;;;;;;;;;;;;;;;;;;
;; confirm-dialog ;;
;;;;;;;;;;;;;;;;;;;;

(defun confirm-dialog (obj content on-input &key (modal t)
					      (title "Confirm")
					      (ok-text "OK")
					      (cancel-text "Cancel")
					      (left nil) (top nil)
					      (width 300) (height 200)
					      (client-movement nil)
					      (html-id nil))
  "Create a confirmation dialog box with CONTENT centered.
Calls on-input with t if confirmed or nil if canceled."
  (unless html-id
      (setf html-id (clog-connection:generate-id)))
  (let* ((body (connection-data-item obj "clog-body"))
	 (win  (create-gui-window obj
				  :title          title
				  :content        (format nil
"<div class='w3-panel'>
<center>~A<br><br>
<form class='w3-container' onSubmit='return false;'>
<button class='w3-button w3-black' style='width:7em' id='~A-ok'>~A</button>
<button class='w3-button w3-black' style='width:7em' id='~A-cancel'>~A</button>
</form>
</center>
</div>" content
	html-id ok-text      ; ok
	html-id cancel-text) ; cancel
				  :top             top
				  :left            left
				  :width           width
				  :height          height
				  :hidden          t
				  :client-movement client-movement
				  :html-id         html-id))
	 (ok     (attach-as-child win (format nil "~A-ok" html-id)))
	 (cancel (attach-as-child win (format nil "~A-cancel" html-id))))
    (unless top
      (setf (top win) (unit :px (- (/ (inner-height (window body)) 2.0)
				   (/ (height win) 2.0)))))
    (unless left
      (setf (left win) (unit :px (- (/ (inner-width (window body)) 2.0)
				    (/ (width win) 2.0)))))
    (setf (visiblep win) t)
    (when modal
      (window-make-modal win))
    (focus ok)
    (set-on-click cancel (lambda (obj)
			   (declare (ignore obj))
			   (window-close win))
		  :one-time t)
    (set-on-click ok (lambda (obj)
		       (declare (ignore obj))
		       (set-on-window-close win nil)
		       (when modal
			 (window-end-modal win))
		       (window-close win)
		       (funcall on-input t))
		  :one-time t)
    (set-on-window-close win (lambda (obj)
			       (declare (ignore obj))
			       (when modal
				 (window-end-modal win))
			       (funcall on-input nil)))))

;;;;;;;;;;;;;;;;;
;; form-dialog ;;
;;;;;;;;;;;;;;;;;

(defun form-dialog (obj content fields on-input &key (modal t)
						  (title "Form")
						  (ok-text "OK")
						  (cancel-text "Cancel")
						  (left nil) (top nil)
						  (width 400) (height 500)
						  (client-movement nil)
						  (html-id nil))
  "Create a form dialog box with CONTENT followed by FIELDS.
FIELDS is a list of lists each list has:

    (1) Field name         - Used for (name attribute)
    (2) Field description  - Used for label
    (3) Field type         - Optional (defaults to :text)
    (4) Field type options - Optional

Special field types

   Field Type     Field Type Options
   =============  ==================
   :filename      default dir -- NOTE: This is _server_ side!
   :checkbox      t if checked
   :radiobox      a-list ((label name)) a third value can be added \"checked\"
   :select        a-list ((label name)) a third value can be added \"selected\"
   :text          value
     (any text input types also work :email, :tel, etc.
      see FORM-ELEMENT-TYPE)

Calls on-input after OK or Cancel with an a-list of field name to value
if confirmed or nil if canceled."
  (unless html-id
    (setf html-id (clog-connection:generate-id)))
  (let* ((body (connection-data-item obj "clog-body"))
	 (fls (format nil "~{~A~}"
		      (mapcar (lambda (l)
				(cond
				  ((eq (third l) :select)
				   (format nil
					   "<div><label class='w3-text-black'><b>~A</b></label>~
			       <select class='w3-select w3-border' name='~A-~A'>~A</select></div>"
					   (first l) html-id (second l)
					   (format nil "~{~A~}"
						   (mapcar (lambda (s)
							     (format nil
								     "<option value='~A' ~A>~A</option>"
								     (second s)
								     (if (third s)
									 (third s)
									 "")
								     (first s)))
							   (fourth l)))))
				  ((eq (third l) :radio)
				   (format nil
					   "<div><label class='w3-text-black'><b>~A</b></label>~A</div>"
					   (first l)
					   (format nil "~{~A~}"
						   (mapcar (lambda (s)
							     (format nil
			       "<div><input type=radio class='w3-radio' name='~A-~A'~
                                      id='~A-~A-~A' value='~A' ~A> ~
                                     <label for='~A-~A-~A'>~A</label></div>"
			                                      html-id (second l)
							      html-id (second l) (second s)
							      (second s)
							      (if (third s)
								  (third s)
								  "")
							      html-id (second l) (second s)
							      (first s)))
							   (fourth l)))))
				  ((eq (third l) :checkbox)
				   (format nil
					   "<div><input class='w3-check' type='checkbox' ~
                                                  name='~A-~A' id='~A-~A' ~A> ~
                                                  <label class='w3-text-black' for='~A-~A'>~
                                                  <b>~A</b></label>~
                                            </div>"
					   html-id (second l) html-id (second l)
					   (if (fourth l)
					       "checked"
					       "")
					   html-id (second l)
					   (first l)))
				  ((third l)
				   (format nil
					   "<div><label class='w3-text-black'><b>~A</b></label>~
                                                 <input class='w3-input w3-border' type='~A'~
                                                  name='~A-~A' id='~A-~A' value='~A'></div>"
                                   (first l) (third l)
				   html-id (second l) html-id (second l)
				   (if (fourth l)
				       (fourth l)
				       "")))
				  (t
				   (format nil
					    "<div><label class='w3-text-black'><b>~A</b></label>~
                               <input class='w3-input w3-border' type='text' name='~A-~A' id='~A-~A'></div>"
                                            (first l) html-id (second l) html-id (second l)))))
			      fields)))
	 (win  (create-gui-window obj
				  :title          title
				  :content        (format nil
"<div class='w3-panel'>
~A
<form class='w3-container' onSubmit='return false;'>
~A
<br><center>
<button class='w3-button w3-black' style='width:7em' id='~A-ok'>~A</button>
<button class='w3-button w3-black' style='width:7em' id='~A-cancel'>~A</button>
</center>
</form>
</div>" (if content
	    (format nil "<center>~A</center><br>" content)
	    "")
        fls
	html-id ok-text      ; ok
	html-id cancel-text) ; cancel
				  :top             top
				  :left            left
				  :width           width
				  :height          height
				  :hidden          t
				  :client-movement client-movement
				  :html-id         html-id))
	 (ok     (attach-as-child win (format nil "~A-ok" html-id)))
	 (cancel (attach-as-child win (format nil "~A-cancel" html-id))))
    (unless top
      (setf (top win) (unit :px (- (/ (inner-height (window body)) 2.0)
				   (/ (height win) 2.0)))))
    (unless left
      (setf (left win) (unit :px (- (/ (inner-width (window body)) 2.0)
				    (/ (width win) 2.0)))))
    (setf (visiblep win) t)
    (when modal
      (window-make-modal win))
    (mapcar (lambda (l)
	      (when (eq (third l) :filename)
		(let ((fld (attach-as-child body (format nil "~A-~A"
							    html-id
							    (second l))
					    :clog-type 'clog:clog-form-element)))
		  (set-on-click fld (lambda (obj)
				      (declare (ignore obj))
				      (server-file-dialog body (first l) (fourth l)
							  (lambda (fname)
							    (setf (value fld) fname))))))))
	    fields)
    (js-execute obj (format nil "$('[name=~A-~A]').focus()"
			    html-id
			    (cadar fields)))
    (set-on-click cancel (lambda (obj)
			   (declare (ignore obj))
			   (window-close win))
		  :one-time t)
    (set-on-click ok (lambda (obj)
		       (declare (ignore obj))
		       (set-on-window-close win nil)
		       (when modal
			 (window-end-modal win))
		       (let ((result (mapcar
				      (lambda (l)
					`(,(second l)
					  ,(let ((name (format nil "~A-~A" html-id (second l))))
					     (cond ((eq (third l) :select)
						    (select-value win name))
						   ((eq (third l) :radio)
						    (radio-value win name))
						   ((eq (third l) :checkbox)
						    (checkbox-value win name))
						   (t
						    (name-value win name))))))
				      fields)))
			 (window-close win)
			 (funcall on-input result)))
		  :one-time t)
    (set-on-window-close win (lambda (obj)
			       (declare (ignore obj))
			       (when modal
				 (window-end-modal win))
			       (funcall on-input nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;
;; server-file-dialog ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defun server-file-dialog (obj title initial-dir on-file-name
			   &key (modal t)
			     (left nil) (top nil) (width 390) (height 425)
			     (maximize nil)
			     (initial-filename nil)
			     (client-movement nil)
			     (html-id nil))
  "Create a local file dialog box called TITLE using INITIAL-DIR on server
machine, upon close ON-FILE-NAME called with filename or nil if failure."
  (let* ((body (connection-data-item obj "clog-body"))
	 (win  (create-gui-window obj
				  :title           title
				  :maximize        maximize
				  :top             top
				  :left            left
				  :width           width
				  :height          height
				  :hidden          t
				  :client-movement client-movement
				  :html-id         html-id))
	 (box    (create-div (window-content win) :class "w3-panel"))
	 (form   (create-form box))
	 (dirs   (create-select form))
	 (files  (create-select form))
	 (input  (create-form-element form :input :label
				     (create-label form :content "File Name:")))
	 (ok     (create-button form :content "OK"
				     :class "w3-button w3-black w3-margin"))
	 (cancel (create-button form :content "Cancel"
				     :class "w3-button w3-black w3-margin")))
    (unless top
      (setf (top win) (unit :px (- (/ (inner-height (window body)) 2.0)
				   (/ (height win) 2.0)))))
    (unless left
      (setf (left win) (unit :px (- (/ (inner-width (window body)) 2.0)
				    (/ (width win) 2.0)))))
    (setf (size dirs) 4)
    (setf (box-width dirs) "100%")
    (setf (size files) 8)
    (setf (box-width files) "100%")
    (setf (box-width input) "100%")
    (setf (width ok) "7em")
    (setf (width cancel) "7em")
    (setf (visiblep win) t)
    (when modal
      (window-make-modal win))
    (flet ((populate-dirs (dir)
	     (setf (inner-html dirs) "")
	     (add-select-option dirs (format nil "~A" dir) ".")
	     (setf (value input) (truename dir))
	     (unless (or (equalp dir "/") (equalp dir #P"/"))
	       (add-select-option dirs (format nil "~A../" dir) ".."))
	     (dolist (item (uiop:subdirectories dir))
	       (add-select-option dirs item item)))
	   (populate-files (dir)
	     (setf (inner-html files) "")
	     (dolist (item (uiop:directory-files dir))
	       (add-select-option files item (file-namestring item))))
	   (caret-at-end ()
	     (focus input)
	     (js-execute win (format nil "~A.setSelectionRange(~A.value.length,~A.value.length)"
				     (clog::script-id input)
				     (clog::script-id input)
				     (clog::script-id input)))))
      (populate-dirs initial-dir)
      (populate-files initial-dir)
      (when initial-filename
	(setf (value input) (truename initial-filename))
	(caret-at-end))
      (set-on-change files (lambda (obj)
			     (declare (ignore obj))
			     (setf (value input) (truename (value files)))
			     (caret-at-end)))
      (set-on-change dirs (lambda (obj)
			    (declare (ignore obj))
			    (setf (value input) (value dirs))
			    (caret-at-end)
			    (populate-files (value dirs))))
      (set-on-double-click dirs
			   (lambda (obj)
			     (declare (ignore obj))
			     (populate-dirs (truename (value dirs)))))
      (set-on-double-click files (lambda (obj)
				   (declare (ignore obj))
				   (click ok))))
    (set-on-window-close win (lambda (obj)
			       (declare (ignore obj))
			       (when modal
				 (window-end-modal win))
			       (funcall on-file-name nil)))
    (set-on-click cancel (lambda (obj)
			   (declare (ignore obj))
			   (window-close win))
		  :one-time t)
    (set-on-click ok (lambda (obj)
		       (declare (ignore obj))
		       (set-on-window-close win nil)
		       (when modal
			 (window-end-modal win))
		       (window-close win)
		       (funcall on-file-name (value input)))
		  :one-time t)))
