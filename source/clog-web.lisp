;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG - The Common Lisp Omnificent GUI                                 ;;;;
;;;; (c) 2020-2022 David Botton                                            ;;;;
;;;; License BSD 3 Clause                                                  ;;;;
;;;;                                                                       ;;;;
;;;; clog-web.lisp                                                         ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Like clog-gui, clog-web uses w3.css as the underlying framework. w3.css is
;;; a public domain css only framework for layouts, is fast and efficient and
;;; does not require additional components outside of the css file. The goal
;;; of clog-web is to help make it easier to create "webpage" style apps
;;; (page layout instead of a more direct layout around the browser window
;;; as in clog-gui the mimics a desktop environment) or actual webpages
;;; (traditional hyper-linking, submition of forms and minimal need for an
;;; active clog connection).

(mgl-pax:define-package :clog-web
  (:documentation "CLOG-WEB a web page style abstraction for CLOG")
  (:use #:cl #:parse-float #:clog #:mgl-pax))

(cl:in-package :clog-web)

(defsection @clog-web (:title "CLOG Web Objects")
  "CLOG-WEB - Web page abstraction for CLOG"
  (clog-web-initialize              function)
  (set-maximum-page-width-in-pixels function)

  "CLOG-WEB - General Containers"
  (clog-web-panel          class)
  (create-web-panel        generic-function)
  (clog-web-content        class)
  (create-web-content      generic-function)
  (clog-web-code           class)
  (create-web-code         generic-function)
  (clog-web-main           class)
  (create-web-main         generic-function)
  (clog-web-sidebar        class)
  (create-web-sidebar      generic-function)
  (clog-web-sidebar-item   class)
  (create-web-sidebar-item generic-function)
  (clog-web-sidebar-item   class)
  (create-web-sidebar-item generic-function)
  (clog-web-compositor     class)
  (create-web-compositor   generic-function)
  (web-padding-class-type  type)
  (composite-on-hover      generic-function)
  (composite-position      generic-function)
  (composite-top-middle    generic-function)
  (composite-top-left      generic-function)
  (composite-top-right     generic-function)
  (composite-bottom-middle generic-function)
  (composite-bottom-left   generic-function)
  (composite-bottom-right  generic-function)
  (composite-middle        generic-function)
  (composite-left          generic-function)
  (composite-right         generic-function)
  
  "CLOG-WEB - Auto Layout System"
  (clog-web-auto-row      class)
  (create-web-auto-row    generic-function)
  (clog-web-auto-column   class)
  (create-web-auto-column generic-function)
			
  "CLOG-WEB - 12 Column Grid Layout System"
  (clog-web-row         class)
  (create-web-row       generic-function)
  (clog-web-container   class)
  (create-web-container generic-function)

  "CLOG-WEB - Look and Feel"
  (add-card-look       generic-function)
  (add-hard-card-look  generic-function)
  
  "CLOG-WEB - Mobile"
  (full-row-on-mobile     generic-function)
  (hide-on-small-screens  generic-function)
  (hide-on-medium-screens generic-function)
  (hide-on-large-screens  generic-function)

  "CLOG-WEB - Menus"
  (clog-web-menu-bar             class)
  (create-web-menu-bar           generic-function)
  (web-menu-bar                  generic-function)
  (web-menu-bar-height           generic-function)  
  (clog-web-menu-drop-down       class)
  (create-web-menu-drop-down     generic-function)
  (clog-web-menu-item            class)
  (create-web-menu-item          generic-function)
  (create-web-menu-full-screen   generic-function)
  (create-web-menu-icon          generic-function)

  "CLOG-WEB - Interactions"
  (clog-web-alert         function)
  (clog-web-form          function))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-web - CLOG Web page abstraction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-web ()
  ((body
    :accessor body
    :documentation "Top level access to browser window")
   (web-menu
    :accessor web-menu
    :initform nil
    :documentation "Installed menu bar if installed")))

;;;;;;;;;;;;;;;;;;;;;
;; create-clog-web ;;
;;;;;;;;;;;;;;;;;;;;;

(defun create-clog-web (clog-body)
  "Create a clog-web object and places it in CLOG-BODY's connection-data as
\"clog-web\". (Private)"
  (let ((clog-web (make-instance 'clog-web)))
    (setf (connection-data-item clog-body "clog-web") clog-web)
    (setf (body clog-web) clog-body)
    clog-web))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; clog-web-initialize ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun clog-web-initialize (clog-body &key (w3-css-url "/css/w3.css"))
  "Initializes clog-web and installs a clog-web object on connection.
If W3-CSS-URL has not been loaded before is installed unless is nil."
  (create-clog-web clog-body)
  (unless (connection-data-item clog-body "w3-css")
    (when w3-css-url
      (setf (connection-data-item clog-body "w3-css") t)
      (load-css (html-document clog-body) w3-css-url))))

;;;;;;;;;;;;;;;;;;
;; web-menu-bar ;;
;;;;;;;;;;;;;;;;;;

(defgeneric web-menu-bar (clog-obj)
  (:documentation "Get/setf window web-menu-bar. This is set buy
create-web-menu-bar."))

(defmethod web-menu-bar ((obj clog-obj))
  (let ((app (connection-data-item obj "clog-web")))
    (web-menu app)))

(defgeneric set-web-menu-bar (clog-obj value)
  (:documentation "Set window web-menu-bar"))

(defmethod set-web-menu-bar ((obj clog-obj) value)
  (let ((app (connection-data-item obj "clog-web")))
    (setf (web-menu app) value)))
(defsetf web-menu-bar set-web-menu-bar)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; web-menu-bar-height ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric web-menu-bar-height (clog-obj)
  (:documentation "Get web-menu-bar height"))

(defmethod web-menu-bar-height ((obj clog-obj))
  (let ((app (connection-data-item obj "clog-web")))
    (if (web-menu app)
	(height (web-menu app))
	0)))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; create-web-menu-bar ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-web-menu-bar (clog-div)()
  (:documentation "Menu bar"))

(defgeneric create-web-menu-bar (clog-obj &key class html-id)
  (:documentation "Attached a menu bar to a CLOG-OBJ in general a
clog-body."))

(defmethod create-web-menu-bar ((obj clog-obj)
				&key (class "w3-bar w3-white")
				  (html-id nil))
  (let ((div (create-div obj :class class :html-id html-id))
	(app (connection-data-item obj "clog-web")))
    (change-class div 'clog-web-menu-bar)
    (setf (web-menu app) div)
    div))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; create-web-menu-drop-down ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-web-menu-drop-down (clog-div)()
  (:documentation "Drop down menu"))

(defgeneric create-web-menu-drop-down (clog-web-menu-bar
				       &key content class html-id)
  (:documentation "Attached a menu bar drop-down to a CLOG-WEB-MENU-BAR"))

(defmethod create-web-menu-drop-down ((obj clog-web-menu-bar)
	          &key (content "")
		    (class "w3-dropdown-content w3-bar-block")
		    (html-id nil))
  (let* ((hover  (create-div obj :class "w3-dropdown-hover"))
	 (button (create-button hover :class "w3-button" :content content))
	 (div    (create-div hover :class class :html-id html-id)))
    (declare (ignore button))
    (change-class div 'clog-web-menu-drop-down)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; create-web-menu-item ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-web-menu-item (clog-span)()
  (:documentation "Menu item"))

(defgeneric create-web-menu-item (clog-web-menu-drop-down
				  &key content
				    on-click
				    class
				    html-id)
  (:documentation "Attached a menu item to a CLOG-WEB-MENU-DROP-DOWN"))

(defmethod create-web-menu-item ((obj clog-obj)
				 &key (content "")
				   (on-click nil)
				   (class "w3-bar-item w3-button")
				   (html-id nil))
  (let ((span
	  (create-span obj :content content :class class :html-id html-id)))
    (set-on-click span on-click)
    (change-class span 'clog-web-menu-item)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; create-web-menu-item ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-web-menu-item (clog-span)()
  (:documentation "Menu item"))

(defgeneric create-web-menu-item (clog-web-menu-drop-down
				  &key content
				    on-click
				    class
				    html-id)
  (:documentation "Attached a menu item to a CLOG-WEB-MENU-DROP-DOWN"))

(defmethod create-web-menu-item ((obj clog-obj)
				 &key (content "")
				   (on-click nil)
				   (class "w3-bar-item w3-button")
				   (html-id nil))
  (let ((span
	  (create-span obj :content content :class class :html-id html-id)))
    (set-on-click span on-click)
    (change-class span 'clog-web-menu-item)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; create-web-menu-full-screen ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric create-web-menu-full-screen (clog-web-menu-bar &key html-id)
  (:documentation "Add as last item in menu bar to allow for a full screen
icon ⤢ and full screen mode."))

(defmethod create-web-menu-full-screen ((obj clog-web-menu-bar)
					&key (html-id nil))
  (create-child obj
    	  " <span class='w3-bar-item w3-right' style='user-select:none;'
	     onClick='if (document.fullscreenElement==null) {
                         documentElement.requestFullscreen()
                      } else {document.exitFullscreen();}'>⤢</span>"
	  :html-id html-id
	  :clog-type 'clog-web-menu-item))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; create-web-menu-icon ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric create-web-menu-icon (clog-web-menu-bar &key image-url
						      on-click
						      class
						      html-id)
  (:documentation "Add icon as menu bar item."))

(defmethod create-web-menu-icon ((obj clog-web-menu-bar)
				 &key (image-url "/img/clogicon.png")
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
		 :clog-type 'clog-web-menu-item)
   on-click))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set-maximum-page-width-in-pixels ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun set-maximum-page-width-in-pixels (clog-body width)
  "The default width is 980 pixels."
  (add-class clog-body "w3-content")
  (setf (maximum-width clog-body) (unit "px" width)))

;;;;;;;;;;;;;;;;;;;
;; add-card-look ;;
;;;;;;;;;;;;;;;;;;;

(defgeneric add-card-look (clog-element)
  (:documentation "Change clog-element to use 2px card look"))

(defmethod add-card-look ((obj clog-element))
  (add-class obj "w3-card-2"))

;;;;;;;;;;;;;;;;;;;;;;;;
;; add-hard-card-look ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric add-hard-card-look (clog-element)
  (:documentation "Change clog-element to use 4px card look"))

(defmethod add-hard-card-look ((obj clog-element))
  (add-class obj "w3-card-4"))

;;;;;;;;;;;;;;;;;;;;;;;;
;; full-row-on-mobile ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric full-row-on-mobile (clog-element)
  (:documentation "Change element to display:block, take up the full row, when
screen size smaller then 601 pixels DP"))

(defmethod full-row-on-mobile ((obj clog-element))
  (add-class obj "w3-mobile"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hide-on-small-screens ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric hide-on-small-screens (clog-element)
  (:documentation "Hide element on screens smaller then 601 pixels DP"))

(defmethod hide-on-small-screens ((obj clog-element))
  (add-class obj "w3-hide-small"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hide-on-medium-screens ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric hide-on-medium-screens (clog-element)
  (:documentation "Hide element on screens smaller then 993 pixels DP"))

(defmethod hide-on-medium-screens ((obj clog-element))
  (add-class obj "w3-hide-medium"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hide-on-large-screens ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric hide-on-large-screens (clog-element)
  (:documentation "Hide element on screens smaller then 993 pixels DP"))

(defmethod hide-on-large-screens ((obj clog-element))
  (add-class obj "w3-hide-large"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - General Containers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Container          -  Sample Uses
;; -----------------     --------------------------------------------
;; Content            -  Fixed size centered Content
;; Panel              -  Notes, Quote boxes, Notifications
;; Display-Container  -  Image text overlays
;; Code               -  Code blocks
;; Sidebar            -  Sidebar to main content, optional collapsable
;; Main               -  Mark main contact when using a sidebar

;;;;;;;;;;;;;;;;;;;;;;
;; create-web-panel ;;
;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-web-panel (clog-div)()
  (:documentation "Panel for web content"))

(defgeneric create-web-panel (clog-obj &key content hidden class html-id)
  (:documentation "Create a clog-web-panel. General container with 16px left
and right padding and 16x top and bottom margin. If hidden is t then then the
visiblep propetery will be set to nil on creation."))

(defmethod create-web-panel ((obj clog-obj) &key (content "")
		 				  (hidden nil)
						  (class nil)
						  (html-id nil))
  (let ((div (create-div obj :content content
			     :hidden t :class class :html-id html-id)))
    (add-class div "w3-panel")
    (unless hidden
      (setf (visiblep div) t))
    (change-class div 'clog-web-panel)))

;;;;;;;;;;;;;;;;;;;;;;;;
;; create-web-content ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-web-content (clog-div)()
  (:documentation "Content for web content"))

(defgeneric create-web-content (clog-obj &key content maximum-width
					   hidden class html-id)
  (:documentation "Create a clog-web-content. General container with 16px left
and right padding. If hidden is t then then the visiblep propetery will be set
to nil on creation."))

(defmethod create-web-content ((obj clog-obj) &key (content "")
						(maximum-width nil)
		 				(hidden nil)
						(class nil)
						(html-id nil))
  (let ((div (create-div obj :content content
			     :hidden t :class class :html-id html-id)))
    (add-class div "w3-content")
    (when maximum-width      
      (setf (maximum-width div) (unit "px" maximum-width)))
    (unless hidden
      (setf (visiblep div) t))
    (change-class div 'clog-web-content)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; create-web-compositor ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-web-compositor (clog-div)()
  (:documentation "Compositor for compositing layers of web content"))

(defgeneric create-web-compositor (clog-obj &key content hidden class html-id)
  (:documentation "Create a clog-web-compositor. Allows compositing of content
on top of other content. Content is added as children and then
composit-location is called on the object of that content. If hidden is t then
then the visiblep propetery will be set to nil on creation."))

(defmethod create-web-compositor ((obj clog-obj) &key (content "")
		 				  (hidden nil)
						  (class nil)
						  (html-id nil))
  (let ((div (create-div obj :content content
			     :hidden t :class class :html-id html-id)))
    (add-class div "w3-display-container")
    (unless hidden
      (setf (visiblep div) t))
    (change-class div 'clog-web-compositor)))

;;;;;;;;;;;;;;;;;
;; composite-* ;;
;;;;;;;;;;;;;;;;;

(deftype web-padding-class-type ()
  '(member :padding-small :padding :padding-large :padding-16 :padding-24
    :padding-32 :padding-48 :padding-64	:padding-top-64 :padding-top-48
    :padding-top-48 :padding-top-32))

(defgeneric composite-on-hover (clog-element)
  (:documentation "Composite CLOG-ELEMENT on on-hover."))

(defmethod composite-on-hover ((obj clog-element))
  (add-class obj "w3-display-hover"))

(defgeneric composite-position (clog-element &key top left padding-class)
  (:documentation "Composite CLOG-ELEMENT to coordinate top left."))

(defmethod composite-position ((obj clog-element)
			       &key
				 (top 0) (left 0)
				 (padding-class nil))
  (add-class obj
     (format nil "w3-display-position~A"
	     (if padding-class
		 (format nil " w3-~A" (string-downcase padding-class))
		 "")))
  (setf (top obj) (unit :px top))
  (setf (left obj) (unit :px left)))

(defgeneric composite-top-middle (clog-element &key padding-class)
  (:documentation "Composite CLOG-ELEMENT on top-middle."))

(defmethod composite-top-middle ((obj clog-element)
				    &key (padding-class nil))
  (add-class obj
     (format nil "w3-display-topmiddle~A"
	     (if padding-class
		 (format nil " w3-~A" (string-downcase padding-class))
		 ""))))

(defgeneric composite-bottom-middle (clog-element &key padding-class)
  (:documentation "Composite CLOG-ELEMENT on bottom-middle."))

(defmethod composite-bottom-middle ((obj clog-element)
				    &key (padding-class nil))
  (add-class obj
     (format nil "w3-display-bottommiddle~A"
	     (if padding-class
		 (format nil " w3-~A" (string-downcase padding-class))
		 ""))))

(defgeneric composite-bottom-right (clog-element &key padding-class)
  (:documentation "Composite CLOG-ELEMENT on bottom-right."))

(defmethod composite-bottom-right ((obj clog-element)
				    &key (padding-class nil))
  (add-class obj
     (format nil "w3-display-bottomright~A"
	     (if padding-class
		 (format nil " w3-~A" (string-downcase padding-class))
		 ""))))

(defgeneric composite-bottom-left (clog-element &key padding-class)
  (:documentation "Composite CLOG-ELEMENT on bottom-left."))

(defmethod composite-bottom-left ((obj clog-element)
				    &key (padding-class nil))
  (add-class obj
     (format nil "w3-display-bottomleft~A"
	     (if padding-class
		 (format nil " w3-~A" (string-downcase padding-class))
		 ""))))

(defgeneric composite-top-right (clog-element &key padding-class)
  (:documentation "Composite CLOG-ELEMENT on top-right."))

(defmethod composite-top-right ((obj clog-element)
				    &key (padding-class nil))
  (add-class obj
     (format nil "w3-display-topright~A"
	     (if padding-class
		 (format nil " w3-~A" (string-downcase padding-class))
		 ""))))

(defgeneric composite-top-left (clog-element &key padding-class)
  (:documentation "Composite CLOG-ELEMENT on top-left."))

(defmethod composite-top-left ((obj clog-element)
				    &key (padding-class nil))
  (add-class obj
     (format nil "w3-display-topleft~A"
	     (if padding-class
		 (format nil " w3-~A" (string-downcase padding-class))
		 ""))))

(defgeneric composite-left (clog-element &key padding-class)
  (:documentation "Composite CLOG-ELEMENT on left."))

(defmethod composite-left ((obj clog-element)
				    &key (padding-class nil))
  (add-class obj
     (format nil "w3-display-left~A"
	     (if padding-class
		 (format nil " w3-~A" (string-downcase padding-class))
		 ""))))

(defgeneric composite-middle (clog-element &key padding-class)
  (:documentation "Composite CLOG-ELEMENT on middle."))

(defmethod composite-middle ((obj clog-element)
				    &key (padding-class nil))
  (add-class obj
     (format nil "w3-display-middle~A"
	     (if padding-class
		 (format nil " w3-~A" (string-downcase padding-class))
		 ""))))

(defgeneric composite-right (clog-element &key padding-class)
  (:documentation "Composite CLOG-ELEMENT on right."))

(defmethod composite-right ((obj clog-element)
				    &key (padding-class nil))
  (add-class obj
     (format nil "w3-display-right~A"
	     (if padding-class
		 (format nil " w3-~A" (string-downcase padding-class))
		 ""))))

;;;;;;;;;;;;;;;;;;;;;
;; create-web-code ;;
;;;;;;;;;;;;;;;;;;;;;

(defclass clog-web-code (clog-div)()
  (:documentation "Code for web code"))

(defgeneric create-web-code (clog-obj &key content
					   hidden class html-id)
  (:documentation "Create a clog-web-code. Code content container.
If hidden is t then then the visiblep propetery will be set
to nil on creation."))

(defmethod create-web-code ((obj clog-obj) &key (content "")
		 				(hidden nil)
						(class nil)
						(html-id nil))
  (let ((div (create-div obj :content content
			     :hidden t :class class :html-id html-id)))
    (add-class div "w3-code")
    (unless hidden
      (setf (visiblep div) t))
    (change-class div 'clog-web-code)))

;;;;;;;;;;;;;;;;;;;;;
;; create-web-main ;;
;;;;;;;;;;;;;;;;;;;;;

(defclass clog-web-main (clog-div)()
  (:documentation "Main for web content"))

(defgeneric create-web-main (clog-obj &key content hidden class html-id)
  (:documentation "Create a clog-web-main. Container for main content
when using a collapsable sidebar or other whole page shifting
technique. If hidden is t then then the visiblep propetery will be set
to nil on creation."))

(defmethod create-web-main ((obj clog-obj) &key (content "")
		 				  (hidden nil)
						  (class nil)
						  (html-id nil))
  (let ((div (create-div obj :content content
			     :hidden t :class class :html-id html-id)))
    (add-class div "w3-main")
    (unless hidden
      (setf (visiblep div) t))
    (change-class div 'clog-web-main)))

;;;;;;;;;;;;;;;;;;;;;;;;
;; create-web-sidebar ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-web-sidebar (clog-div)()
  (:documentation "Sidebar for web content"))

(defgeneric create-web-sidebar (clog-obj &key content hidden class html-id)
  (:documentation "Create a clog-web-sidebar. Container for sidebar content.
sidebars are create with the display property set to :none if hidden it t
and :block if nil. In general the visiblep property is used in clog, however
in clog-web-sidebar the block property is needed to activate its animations
if used. If using a sidebar that will take space and not collapse, make sure
to set the sidebar's size and set a margin equal to the size on the main
container."))

(defmethod create-web-sidebar ((obj clog-obj) &key (content "")
		 				  (hidden nil)
						  (class nil)
						  (html-id nil))
  (let ((div (create-div obj :content content
			     :hidden t :class class :html-id html-id)))
    (setf (display div) :none)
    (setf (visiblep div) t)
    (add-class div "w3-sidebar w3-bar-block")
    (unless hidden
      (setf (display div) :block))
    (change-class div 'clog-web-sidebar)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; create-web-sidebar-item ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-web-sidebar-item (clog-button)()
  (:documentation "Sidebar-Item for web content"))

(defgeneric create-web-sidebar-item (clog-obj &key content hidden class html-id)
  (:documentation "Create a clog-web-sidebar-item. A sidebar menu bar item.
If hidden is t then then the visiblep propetery will be set to nil on
creation."))

(defmethod create-web-sidebar-item ((obj clog-obj) &key (content "")
		 				  (hidden nil)
						  (class nil)
						  (html-id nil))
  (let ((item (create-button obj :content content
				 :hidden t :class class :html-id html-id)))
    (add-class item "w3-bar-item w3-button")
    (unless hidden
      (setf (visiblep item) t))
    (change-class item 'clog-web-sidebar-item)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - Auto Layout
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Container          -  Sample Uses
;; -----------------     ----------------------------------------------------
;; Auto-Row           -  Container of Auto-Columns
;; Auto-Column        -  Columns size adjusts width to fix contents of all
;;                       columns to fill 100% and all heights equal

;;;;;;;;;;;;;;;;;;;;;;;;;
;; create-web-auto-row ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-web-auto-row (clog-div)()
  (:documentation "Content for web content"))

(defgeneric create-web-auto-row (clog-obj &key hidden class html-id)
  (:documentation "Create a clog-web-auto-row. Container for auto-columns
If hidden is t then then the visiblep propetery will be set to nil on
creation."))

(defmethod create-web-auto-row ((obj clog-obj) &key (hidden nil)
						 (class nil)
						 (html-id nil))
  (let ((div (create-div obj :hidden t :class class :html-id html-id)))
    (add-class div "w3-cell-row")
    (unless hidden
      (setf (visiblep div) t))
    (change-class div 'clog-web-auto-row)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; create-web-auto-column ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype web-vertical-align-type () '(member :top :middle :bottom))

(defclass clog-web-auto-column (clog-div)()
  (:documentation "Content for web content"))

(defgeneric create-web-auto-column (clog-obj &key content vertical-align
					   hidden class html-id)
  (:documentation "Create a clog-web-auto-column. Container for auto-columns
If hidden is t then then the visiblep propetery will be set to nil on
creation."))

(defmethod create-web-auto-column ((obj clog-obj) &key (content "")
						  (vertical-align nil)
		 				  (hidden nil)
						  (class nil)
						  (html-id nil))
  (let ((div (create-div obj :content content
			     :hidden t :class class :html-id html-id)))
    (add-class div "w3-cell")
    (when vertical-align
      (add-class div (format nil "w3-cell-~A"
			     (string-downcase vertical-align))))
    (unless hidden
      (setf (visiblep div) t))
    (change-class div 'clog-web-auto-column)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - Responsive 12 part grid
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Container          -  Sample Uses
;; -----------------     ----------------------------------------------------
;; Row                -  Container of grid columns (Containers)
;; Container          -  Headers, Footers, General, 12 part Grid Columns

;;;;;;;;;;;;;;;;;;;;
;; create-web-row ;;
;;;;;;;;;;;;;;;;;;;;

(defclass clog-web-row (clog-div)()
  (:documentation "Row to contain columns of web content in 12 column grid"))

(defgeneric create-web-row (clog-obj &key padding hidden class html-id)
  (:documentation "Create a clog-web-row. If padding is true 8px left and
right padding is addded. If hidden is t then then the visiblep propetery will
be set to nil on creation."))

(defmethod create-web-row ((obj clog-obj) &key (padding nil)
		 			    (hidden nil)
					    (class nil)
					    (html-id nil))
  (let ((div (create-div obj :hidden t :class class :html-id html-id)))
    (if padding
	(add-class div "w3-row-padding")
	(add-class div "w3-row"))
    (unless hidden
      (setf (visiblep div) t))
    (change-class div 'clog-web-row)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; create-web-container ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype web-container-size-type () '(member :half :third :twothird :quarter
				      :threequarter :rest :col))

(defclass clog-web-container (clog-div)()
  (:documentation "Container cells for web content in 12 column grid"))

(defgeneric create-web-container (clog-obj &key content
					     column-size
					     hidden class html-id)
  (:documentation "Create a clog-web-container. COLUMN-SIZE can be of type
container-size-type when to set size displayed on medium and large screens
or can use a string of \"s1-12 m1-12 l1-12\" s m or l followed by how many
columns this container uses on small, medium or large screens. Small screens
are always displayed full row. Total columns must add to 12 or one needs to
be of type :w3-rest to fill space. If hidden is t then then the visiblep
propetery will be set to nil on creation."))

(defmethod create-web-container ((obj clog-obj) &key (content "")
						  (column-size nil)
		 				  (hidden nil)
						  (class nil)
						  (html-id nil))
  (let ((div (create-div obj :content content
			     :hidden t :class class :html-id html-id)))
    (add-class div "w3-container")
    (when column-size
      (add-class div (format nil "w3-~A" (string-downcase column-size))))
    (unless hidden
      (setf (visiblep div) t))
    (change-class div 'clog-web-container)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-web Interactions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun clog-web-alert (obj title content &key
					   (color-class "w3-red")
					   (time-out nil)
					   (place-top nil)
					   (html-id nil))
  "Create an alert toast with option :TIME-OUT. If place-top is t then alert
is placed in DOM at top of OBJ instead of bottom of OBJ."
  (unless html-id
      (setf html-id (clog-connection:generate-id)))
  (let* ((panel    (create-child obj
				 (format nil
"  <div class='w3-panel ~A w3-animate-right w3-display-container'>~
   <span id='~A-closer' class='w3-button w3-large w3-display-topright'>&times;</span>~
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
	(place-inside-top-of obj panel)
	(place-inside-bottom-of obj panel))
    (set-on-click
     (attach-as-child obj (format nil "~A-closer" html-id))
     (lambda (obj)
       (declare (ignore obj))
       (destroy panel)))
    (when time-out
      (sleep time-out)
      (destroy panel))))

(defun clog-web-form (obj content fields on-input &key (modal nil)
						    (ok-text "OK")
						    (cancel-text "Cancel")
						    (html-id nil))
  "Create a form with CONTENT followed by FIELDS.
FIELDS is a list of lists each list has:

    (1) Field description  - Used for label
    (2) Field name         - Used for (name attribute)
    (3) Field type         - Optional (defaults to :text)
    (4) Field type options - Optional

Special field types

   Field Type     Field Type Options
   =============  ==================
   :checkbox      t if checked
   :radiobox      a-list ((label name)) a third value can be added \"checked\"
   :select        a-list ((label name)) a third value can be added \"selected\"
   :text          value
     (any text input types also work :email, :tel, etc.
      see FORM-ELEMENT-TYPE)

Calls on-input after OK or Cancel with an a-list of field name to value
if confirmed or nil if canceled. CANCEL-TEXT is only displayed if modal is t"
  (unless html-id
    (setf html-id (clog-connection:generate-id)))
  (let* ((fls (format nil "~{~A~}"
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
	 (win  (create-web-content obj
				  :content        (format nil
"<div class='w3-panel'>
~A
<form class='w3-container' onSubmit='return false;'>
~A
<br><center>
<button class='w3-button w3-black' style='width:7em' id='~A-ok'>~A</button>~A
</center>
</form>
</div>" (if content
	    (format nil "<center>~A</center><br>" content)
	    "")
        fls
	html-id ok-text ; ok
	(if modal
	    (format nil "&nbsp;<button class='w3-button w3-black' style='width:7em' id='~A-cancel'>~A</button>"
		    html-id cancel-text)
	    ""))
				  :hidden          t
				  :html-id         html-id))
	 (ok     (attach-as-child win (format nil "~A-ok" html-id)))
	 (cancel (if modal
		     (attach-as-child win (format nil "~A-cancel" html-id))
		     nil)))
    (declare (ignore cancel))
    (setf (visiblep win) t)
    (when modal
      (js-execute obj (format nil "$('[name=~A-~A]').focus()"
			      html-id
			      (cadar fields))))
    (set-on-click ok (lambda (obj)
		       (declare (ignore obj))
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
			 (funcall on-input result)))
		  :one-time nil)))
