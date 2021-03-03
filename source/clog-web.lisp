;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG - The Common Lisp Omnificent GUI                                 ;;;;
;;;; (c) 2020-2021 David Botton                                            ;;;;
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

  (full-row-on-mobile     generic-function)
  (hide-on-small-screens  generic-function)
  (hide-on-medium-screens generic-function)
  (hide-on-large-screens  generic-function))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-web - CLOG Web page abstraction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-web ()
  ((body
    :accessor body
    :documentation "Top level access to browser window")))

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
  "Initializes clog-web and installs a clog-web object on connection."
  (create-clog-web clog-body)
  (when w3-css-url
    (load-css (html-document clog-body) w3-css-url)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set-maximum-page-width-in-pixels ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun set-maximum-page-width-in-pixels (clog-body width)
  "The default width is 980 pixels."
  (add-class clog-body "w3-content")
  (setf (maximum-width clog-body) (unit "px" width)))

;;;;;;;;;;;;;;;;;;;;;;;;
;; full-row-on-mobile ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric full-row-on-mobiile (clog-element)
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
;; Main               -  Mark main contact when using a side-bar

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
  (:documentation "Create a clog-web-main. Container for main content when
using a collapsable sidebar  or other whole page shifting technique.
If hidden is t then then the visiblep propetery will be set to nil on
creation."))

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
If hidden is t then then the visiblep propetery will be set to nil on
creation."))

(defmethod create-web-sidebar ((obj clog-obj) &key (content "")
		 				  (hidden nil)
						  (class nil)
						  (html-id nil))
  (let ((div (create-div obj :content content
			     :hidden t :class class :html-id html-id)))
    (add-class div "w3-sidebar w3-bar-block")
    (unless hidden
      (setf (visiblep div) t))
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

