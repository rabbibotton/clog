;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG - The Common Lisp Omnificent GUI                                 ;;;;
;;;; (c) David Botton                                                      ;;;;
;;;;                                                                       ;;;;
;;;; clog-gui.lisp                                                         ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Like clog-web, clog-gui uses w3.css as the underlying framework. w3.css is
;;; a public domain css only framework for layouts, is fast and efficient and
;;; does not require additional components outside of the css file. In addition
;;; clog-gui uses jQueryUI and its default css file to provide client side
;;; movement when needed, if client side movement is not used it is possible
;;; to pass nil to the initilization function for both the jquery-ui-js and
;;;  jquery-ui-css options.

(mgl-pax:define-package :clog-gui
  (:documentation "CLOG-GUI a desktop GUI abstraction for CLOG")
  (:use #:cl #:clog #:mgl-pax))

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
  (window-to-top-by-title      generic-function)
  (window-to-top-by-param      generic-function)
  (window-by-title             generic-function)
  (window-by-param             generic-function)
  (reorient-all-windows        generic-function)
  (maximize-all-windows        generic-function)
  (normalize-all-windows       generic-function)
  (set-on-window-change        generic-function)

  "CLOG-GUI - Individual Windows"
  (clog-gui-window             class)
  (create-gui-window           generic-function)
  (window-title                generic-function)
  (window-icon-area            generic-function)
  (window-param                generic-function)
  (window-content              generic-function)
  (window-focus                generic-function)
  (window-close                generic-function)
  (window-valid-p              function)
  (window-maximized-p          generic-function)
  (window-maximize             generic-function)
  (window-normalize            generic-function)
  (window-toggle-maximize      generic-function)
  (window-toggle-title-bar     generic-function)
  (window-toggle-pinned        generic-function)
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
  (alert-toast        function)
  (alert-dialog       function)
  (input-dialog       function)
  (confirm-dialog     function)
  (prompt-dialog      function)
  (form-dialog        function)
  (server-file-dialog function)

  "CLOG-GUI - Debugger"
  (with-clog-debugger    macro)
  (one-of-dialog         function)
  (dialog-in-stream      class)
  (dialog-out-stream     class)
  (clog-break            function)
  (clog-probe            macro)
  (*probe*               variable)
  (*clog-debug-instance* variable)

  "CLOG-GUI - Look and Feel"
  (*menu-bar-class*           variable)
  (*menu-bar-drop-down-class* variable)
  (*menu-item-class*          variable)
  (*menu-window-select-class* variable)
  (*menu-full-screen-item*    variable)
  (*menu-icon-image-class*    variable)
  (*top-bar-height*           variable)
  (*default-icon*             variable)
  (*default-title-class*      variable)
  (*default-border-class*     variable))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Default Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CLOG GUI based ebugger settings
(defparameter *clog-debug-instance* nil
              "Default location to open debugger windows")
(defvar *probe* nil "Result value of a probe")

;; Menus
(defparameter *menu-bar-class* "w3-bar w3-black w3-card-4")
(defparameter *menu-bar-drop-down-class* "w3-dropdown-content w3-bar-block w3-card-4")
(defparameter *menu-item-class* "w3-bar-item w3-button")
(defparameter *menu-window-select-class* "w3-bar-item w3-button")
(defparameter *menu-full-screen-item* "⤢")
(defparameter *menu-icon-image-class* "w3-button w3-bar-item")

;; New Window placement
(defparameter *top-bar-height* 20
  "Overlap on new windows created with top set as nil")

;; Window treatements
(defparameter *default-title-class* "w3-black"
  "Window title bar class")
(defparameter *default-border-class* "w3-card-4 w3-white w3-border"
  "Window frame border")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-gui - Desktop GUI abstraction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-gui ()
  ((body
    :accessor body
    :documentation "The body of the main window")
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
   (body-left-offset
    :accessor body-left-offset
    :initform 0
    :documentation "Offset for maximize on left side")
   (body-right-offset
    :accessor body-right-offset
    :initform 0
    :documentation "Offset for maximize on right side")
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
    (setf (body clog-gui) (connection-body clog-body))
    clog-gui))

;;;;;;;;;;;;;;;;;;;;;;;;
;; with-clog-debugger ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro with-clog-debugger ((clog-obj &key title
                                             standard-output
                                             standard-input)
                              &body body)
  "body uses a clog-gui based debugger instead of the console"
  `(with-open-stream (out-stream (make-instance 'dialog-out-stream))
     (with-open-stream (in-stream (make-instance 'dialog-in-stream :clog-obj ,clog-obj :source out-stream))
       (labels ((my-debugger (condition encapsulation)
                  (handler-case
                      (let ((restart (one-of-dialog ,clog-obj condition (compute-restarts)
                                                    :title (format nil "Available Restarts~A"
                                                                   (if ,title
                                                                       (format nil " for ~A" ,title)
                                                                       "")))))
                        (when restart
                          (let ((*debugger-hook* encapsulation))
                            (invoke-restart-interactively restart))))
                    (end-of-file () ; no reset chosen
                                 nil))))
         (let* ((*standard-output* (or ,standard-output
                                       *standard-output*))
                (*standard-input* (or ,standard-input
                                      *standard-input*))
                (*query-io*        (make-two-way-stream in-stream out-stream))
                (*debugger-hook*   (if clog-connection:*disable-clog-debugging*
                                       *debugger-hook*
                                       #'my-debugger)))
           ,@body)))))

;;;;;;;;;;;;;;;;
;; clog-break ;;
;;;;;;;;;;;;;;;;

(defun clog-break (&key clog-body run (modal t))
  "Stop execution, funcall RUN with CLOG-BODY if set, if run returns :continue,
the execution continues. If CLOG-BODY not set use *clog-debug-instance*. Then
confirm continue execution on current thread or (break)."
  (unless clog-body
    (setf clog-body *clog-debug-instance*))
  (let ((continue (when run
                    (funcall run clog-body))))
    (when (and (validp clog-body)
               (not (eq continue :continue)))
      (confirm-dialog clog-body
                      (format nil "Continue thread ~A ?"
                              (bordeaux-threads:thread-name
                                (bordeaux-threads:current-thread)))
                      (lambda (result)
                        (unless result
                          (break)))
                      :width 400
                      :time-out 600
                      :modal modal
                      :title "clog-break in execution"))))

;;;;;;;;;;;;;;;;
;; clog-probe ;;
;;;;;;;;;;;;;;;;

(defmacro clog-probe (symbol &key clog-body
                                  (title "")
                                  (time-out 600)
                                  top left
                                  (width 400) (height 300)
                                  auto-probe
                                  save-value
                                  (modal t))
  "Pause thread of execution for time-out numnber of seconds or nil to not
block execution, display symbol's value, value is changed if OK pressed at
the moment pressed. When time-out is nil, :q quits the probe and cancel
repeats the probe with out changing value. When time-out is nil modal is
always nil. If auto-probe is set, modal and time-out is set to nil and the
probe is run again in auto-probe seconds. If not tile is set, the symbol is
used for title. If save-value is true clog-gui:*probe* is set to value of
symbol before any change is made by dialog."
  `(let ((body (or ,clog-body
                   *clog-debug-instance*))
         (title (if (equal ,title "")
                    (format nil "~s" ',symbol) 
                    ,title)))
     (when (validp body)
       (if (and ,time-out (not ,auto-probe))
           (let* ((ovalue ,symbol)
                  (value (escape-for-html ovalue)))
             (when ,save-value
               (setf clog-gui:*probe* ovalue))
             (input-dialog body
                           (format nil "Probe in thread ~A :<br><code>~A</code> New Value?"
                                   (bordeaux-threads:thread-name
                                     (bordeaux-threads:current-thread))
                                   value)
                           (lambda (result)
                             (when (and result
                                        (not (equal result "")))
                               (setf ,symbol (eval (read-from-string result)))))
                           :time-out ,time-out
                           :top ,top :left ,left
                           :width ,width
                           :height ,height
                           :modal ,modal
                           :title (format nil "clog-probe ~A" title)))
           (bordeaux-threads:make-thread
             (lambda ()
               (loop
                 (let* ((ovalue ,symbol)
                        (value (escape-for-html ovalue)))
                   (when (eq (input-dialog body
                                           (format nil "Probe result <code>~A</code> - New Value or :q to quit?"
                                                   value)
                                           (lambda (result)
                                             (when (and result
                                                        (not (equalp result "")))
                                               (if (equalp result ":q")
                                                   :q
                                                   (setf ,symbol (eval (read-from-string result))))))
                                           :time-out (or ,auto-probe 999)
                                           :top ,top :left ,left
                                           :width ,width
                                           :height ,height
                                           :modal nil
                                           :title (format nil "clog-probe ~A" title))
                             :q)
                     (when ,save-value
                       (setf clog-gui:*probe* ovalue))
                     (return)))))
             :name (format nil "clog-probe ~A" title))))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; clog-gui-initialize ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun clog-gui-initialize (clog-body &key
                                        (body-left-offset 0)
                                        (body-right-offset 0)
                                        (use-clog-debugger nil)
                                        (standard-output nil)
                                        (parent-desktop-obj nil)
                                        (w3-css-url "/css/w3.css")
                                        (jquery-ui-css "/css/jquery-ui.css")
                                        (jquery-ui "/js/jquery-ui.js"))
  "Initializes clog-gui and installs a clog-gui object on the connection body.
If W3-CSS-URL has not been loaded before it is installed unless set to nil.
clog-gui uses jQueryUI and its default css file to provide client side
movement when needed, if client side movement is not used it is possible
to pass nil to the initilization function for both the jquery-ui-js and
jquery-ui-css options and there is no need to deliver the jQueryUI it with your
application. BODY-LEFT-OFFSET and BODY-RIGHT-OFFSET limit width on maximize.
parent-desktop-obj is used if this window is a popup or otherwise a
slave of another clog-gui page.
If use-clog-debugger then a graphical debugger is set for all events. If
standard-output is set *standard-output* for every event is redirected
to it.
NOTE: use-clog-debugger should not be set for security issues
      on non-secure environments."
  (if parent-desktop-obj
    (let ((app (connection-data-item parent-desktop-obj "clog-gui")))
      (setf (connection-data-item clog-body "clog-gui") app))
    (let ((app (create-clog-gui clog-body)))
      (setf (body-left-offset app) body-left-offset)
      (setf (body-right-offset app) body-right-offset)))
  (set-on-full-screen-change (html-document clog-body) 'reorient-all-windows)
  (set-on-orientation-change (window clog-body) 'reorient-all-windows)
  (set-on-resize (window clog-body) 'reorient-all-windows)
  (unless (connection-data-item clog-body "w3-css")
    (when w3-css-url
      (setf (connection-data-item clog-body "w3-css") t)
      (load-css (html-document clog-body) w3-css-url)))
  (when jquery-ui-css
    (load-css (html-document clog-body) jquery-ui-css))
  (when jquery-ui
    (load-script (html-document clog-body) jquery-ui))
  (when (and use-clog-debugger (not clog-connection:*disable-clog-debugging*))
    (unless (or *clog-debug-instance*
                (when (and (typep *clog-debug-instance* 'clog-obj)
                           (validp *clog-debug-instance*))))
      (setf *clog-debug-instance* clog-body))
    (setf (connection-data-item clog-body "clog-debug")
          (lambda (event data)
            (with-clog-debugger (clog-body :standard-output standard-output)
              (funcall event data))))))

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

(defgeneric (setf menu-bar) (value clog-obj)
  (:documentation "Set window menu-bar"))

(defmethod (setf menu-bar) (value (obj clog-obj))
  (let ((app (connection-data-item obj "clog-gui")))
    (setf (menu app) value)))

;;;;;;;;;;;;;;;;;;;;;
;; menu-bar-height ;;
;;;;;;;;;;;;;;;;;;;;;

(defgeneric menu-bar-height (clog-obj)
  (:documentation "Get menu-bar height"))

(defmethod menu-bar-height ((obj clog-obj))
  (let ((app (connection-data-item obj "clog-gui")))
    (if (and app (menu app))
        (if (in-clog-popup-p obj)
            0
            (height (menu app)))
        0)))

;;;;;;;;;;;;;;;;;;;;;;;
;; window-collection ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric window-collection (clog-obj)
  (:documentation "Get hash table of open windows"))

(defmethod window-collection ((obj clog-obj))
  (window-clean-zombies obj)
  (let ((app (connection-data-item obj "clog-gui")))
    (windows app)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; window-to-top-by-title ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric window-to-top-by-title (clog-obj title)
  (:documentation "Bring window with TITLE to top and return
window or nil if not found"))

(defmethod window-to-top-by-title ((obj clog-obj) title)
  (window-clean-zombies obj)
  (when title
    (let ((app (connection-data-item obj "clog-gui"))
          (r   nil))
      (maphash (lambda (key value)
                 (declare (ignore key))
                 (when (window-valid-p value)
                   (when (and (equalp (window-title value) title)
                              (window-focus value)
                              (setf r value)))))
               (windows app))
      r)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; window-clean-zombies ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric window-clean-zombies (clog-obj &key use-select)
  (:documentation "Clean zombie references to windows that can
occur from browsers being closed or crashing. (private)"))

(defmethod window-clean-zombies ((obj clog-obj) &key use-select)
  (let ((app (connection-data-item obj "clog-gui")))
    (when use-select
      (setf (inner-html use-select) ""))
    (maphash (lambda (key value)
               (if (window-valid-p value)
                   (when use-select
                     (setf (window-select-item value)
                           (create-option use-select
                                          :content (window-title value)
                                          :selected t
                                          :value key)))
                   (remhash key (windows app))))
             (windows app))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; window-to-top-by-param ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric window-to-top-by-param (clog-obj param)
  (:documentation "Bring window with PARAM to top and return
window or nil if not found"))

(defmethod window-to-top-by-param ((obj clog-obj) param)
  (window-clean-zombies obj)
  (let ((app (connection-data-item obj "clog-gui"))
        (r   nil))
    (maphash (lambda (key value)
               (declare (ignore key))
               (when (window-valid-p value)
                 (when (equalp (win-param value) param)
                   (window-focus value)
                   (setf r value))))
             (windows app))
    r))

;;;;;;;;;;;;;;;;;;;;;
;; window-by-title ;;
;;;;;;;;;;;;;;;;;;;;;

(defgeneric window-by-title (clog-obj title)
  (:documentation "Bring window with TITLE to top and return
window or nil if not found"))

(defmethod window-by-title ((obj clog-obj) title)
  (window-clean-zombies obj)
  (let ((app (connection-data-item obj "clog-gui"))
        (r   nil))
    (maphash (lambda (key value)
               (declare (ignore key))
               (when (window-valid-p value)
                 (when (equalp (window-title value) title)
                   (setf r value))))
             (windows app))
    r))

;;;;;;;;;;;;;;;;;;;;;;
;; window-icon-area ;;
;;;;;;;;;;;;;;;;;;;;;;

(defgeneric window-icon-area (clog-obj)
  (:documentation "Return the clog-obj for the icon-area to allow adding
custom icons on the title bar to the right of the close icon"))

(defmethod window-icon-area ((obj clog-obj))
  (icon-area obj))

;;;;;;;;;;;;;;;;;;;;;
;; window-by-param ;;
;;;;;;;;;;;;;;;;;;;;;

(defgeneric window-by-param (clog-obj param)
  (:documentation "Bring window with PARAM to top and return
window or nil if not found"))

(defmethod window-by-param ((obj clog-obj) param)
  (window-clean-zombies obj)
  (let ((app (connection-data-item obj "clog-gui"))
        (r   nil))
    (maphash (lambda (key value)
               (declare (ignore key))
               (when (window-valid-p value)
                 (when (equalp (win-param value) param)
                   (setf r value))))
             (windows app))
    r))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; maximize-all-windows ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric maximize-all-windows (clog-obj)
  (:documentation "Maximize all windows"))

(defmethod maximize-all-windows ((obj clog-obj))
  (window-clean-zombies obj)
  (let ((app (connection-data-item obj "clog-gui")))
    (maphash (lambda (key value)
               (declare (ignore key))
               (when (window-valid-p value)
                 (window-maximize value)))
             (windows app))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; normalize-all-windows ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric normalize-all-windows (clog-obj)
  (:documentation "Normalize all windows"))

(defmethod normalize-all-windows ((obj clog-obj))
  (window-clean-zombies obj)
  (let ((app (connection-data-item obj "clog-gui")))
    (maphash (lambda (key value)
               (declare (ignore key))
               (when (window-valid-p value)
                 (window-normalize value)))
             (windows app))))

;;;;;;;;;;;;;;;;;;;;
;; make-in-bounds ;;
;;;;;;;;;;;;;;;;;;;;

(defun make-in-bounds (obj mbh bh bw)
  "Insure obj in bounds of gui (private)"
  (let* ((top-loc   (js-to-integer (top obj)))
	 (left-loc  (js-to-integer (left obj)))
	 (width-loc (width obj)))
    (if (< (+ left-loc width-loc) 25)
	(setf (left obj) (unit :px (- 25 width-loc))))
    (if (> left-loc bw)
	(setf (left obj) (unit :px (- bw 15))))
    (if (< top-loc mbh)
	(setf (top obj) (unit :px mbh)))
    (if (>= top-loc bh)
	(setf (top obj) (unit :px (- bh 15))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; reorient-all-windows ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric reorient-all-windows (clog-obj)
  (:documentation "Reorient all windows. Remaximized any maximize windows
and leave any normalized windows as normalized. This is called by default
in on-resize, on-full-screen-change and on-orientation-change events."))

(defmethod reorient-all-windows ((obj clog-obj))
  (window-clean-zombies obj)
  (let* ((app  (connection-data-item obj "clog-gui"))
	 (body (connection-body obj))
	 (mbh  (menu-bar-height obj))
	 (bh   (height (html-document body)))
	 (bw   (width  (html-document body)))
	 (cur  (current-window obj)))
    (maphash (lambda (key value)
               (declare (ignore key))
               (when (window-valid-p value)
                 (cond ((window-maximized-p value)
                        (window-maximize value :focus nil))
		       (t
		        (make-in-bounds value mbh bh bw)))))
             (windows app))
    (when cur
      (window-focus cur))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; create-gui-menu-bar ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-gui-menu-bar (clog-div)()
  (:documentation "Menu bar"))

(defgeneric create-gui-menu-bar (clog-obj &key class html-id main-menu)
  (:documentation "Attached a menu bar to a CLOG-OBJ in general a
clog-body. If main-menu add as main menu bar."))

(defmethod create-gui-menu-bar ((obj clog-obj)
                                &key (class *menu-bar-class*)
                                  (html-id nil)
                                  (main-menu t))
  (let* ((div (create-div obj :class class :html-id html-id))
         (blank (create-a div))
         (app (connection-data-item obj "clog-gui")))
    (declare (ignore blank))
    (change-class div 'clog-gui-menu-bar)
    (when main-menu
        (setf (menu app) div))
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
                    (class *menu-bar-drop-down-class*)
                    (right-align nil)
                    (html-id nil))
  (let* ((hover  (create-div obj :class (if right-align
                                            "w3-right w3-dropdown-hover"
                                            "w3-dropdown-hover")))
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
                                   (class *menu-item-class*)
                                   (html-id nil))
  (let ((span
          (create-span obj :content content :class class :html-id html-id)))
    (set-on-click span on-click)
    (change-class span 'clog-gui-menu-item)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; create-gui-menu-window-select ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-gui-menu-window-select (clog-select)()
  (:documentation "Drop down containing existing windows."))

(defgeneric create-gui-menu-window-select (clog-obj
                                           &key class
                                             content
                                             html-id)
  (:documentation "Attaches a clog-select as a menu item that auto updates
with open windows and focuses them unless is a keep-on-top window. The
first menu-window-select will receive change window notices only."))

(defmethod create-gui-menu-window-select ((obj clog-obj)
                                          &key (class *menu-window-select-class*)
                                            (content "Select Window")
                                            (html-id nil))
  (let ((window-select (create-select obj :html-id html-id :class class))
        (app           (connection-data-item obj "clog-gui")))
    (change-class window-select 'clog-gui-menu-window-select)
    (unless (window-select app)
      (setf (window-select app) window-select))
    ; on mac on-click after a refill doesn't work
    ; on pc mouse-enter fires as long as in the control, so..
    (flet ((refill (obj)
             (set-on-mouse-enter obj nil)
             (with-sync-event (obj)
               (window-clean-zombies obj :use-select window-select))
             (when content
               (setf (selectedp (create-option window-select :content content)) t))))
      (set-on-mouse-enter window-select (lambda (obj)
                                          (refill obj)))
      (set-on-mouse-leave window-select (lambda (obj)
                                          (declare (ignore obj))
                                          (sleep .5)
                                          (set-on-mouse-enter window-select (lambda (obj)
                                                                              (refill obj))))))
    (set-on-change window-select (lambda (obj)
                                   (let ((win (gethash (text-value obj) (windows app))))
                                     (when win
                                       (unless (keep-on-top win)
                                         (setf (hiddenp win) nil)
                                         (window-focus win))))))
    (when content
      (create-option window-select :content content))
    window-select))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; create-gui-menu-full-screen ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric create-gui-menu-full-screen (clog-gui-menu-bar &key html-id)
  (:documentation "Add as last item in menu bar to allow for a full screen
icon ⤢ (*menu-full-screen-item* default) and full screen mode."))

(defmethod create-gui-menu-full-screen ((obj clog-gui-menu-bar)
                                        &key (html-id nil))
  (create-child obj
                (format nil
                        " <span class='w3-bar-item w3-right' style='user-select:none;'
                            onClick='if (document.fullscreenElement==null) {
                              documentElement.requestFullscreen()
                            } else {document.exitFullscreen();}'>~A</span>"
                        *menu-full-screen-item*)
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
                                 &key (image-url *default-icon*)
                                   (on-click nil)
                                   (class *menu-icon-image-class*)
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
  "Fire handler if set. Change the value of current-win to obj (Private)"
  (unless (eq obj (current-win app))
    (when (current-win app)
      (fire-on-window-blur (current-win app)))
    (unless obj
      (let (new-order
            (order -9999))
        (maphash (lambda (key value)
                   (declare (ignore key))
                   (setf new-order (z-index value))
                   (when (window-valid-p value)
                     (when (and new-order
                                (>= new-order order))
                       (setf order new-order)
                       (setf obj value))))
                 (windows app))))
    (setf (current-win app) obj)
    (when (on-window-change app)
      (funcall (on-window-change app) obj))
    (when obj
      (fire-on-window-focus obj))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - Individual Windows
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-gui-window (clog-element)
  ((win-title
    :accessor win-title
    :documentation "Window title clog-element")
   (win-param
    :accessor win-param
    :initform nil
    :documentation "Window specific parameter")
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
   (icon-area
     :accessor icon-area
     :documentation "Window icon area for adding icons to menu bar")
   (closer
    :accessor closer
    :documentation "Window closer clog-element")
   (sizer
    :accessor sizer
    :initform nil
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
   (window-size-mutex
    :reader window-size-mutex
    :initform (bordeaux-threads:make-lock)
    :documentation "Sync maximize / normalize events")
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
    (handler-case
        (let* ((target (gethash (attribute obj "data-drag-obj") (windows app)))
               (pointer-x (getf data ':screen-x))
               (pointer-y (getf data ':screen-y))
               (obj-top)
               (obj-left)
               (perform-drag nil))
          (when target
            (setf (drag-obj app) target)
            (unless (keep-on-top (drag-obj app))
              (setf (z-index (drag-obj app)) (incf (last-z app))))
            (setf (in-drag app) (attribute obj "data-drag-type"))
            (cond ((equalp (in-drag app) "m")
                    (setf obj-top
                          (js-to-integer (top (drag-obj app))))
                    (setf obj-left
                          (js-to-integer (left (drag-obj app))))
                    (setf perform-drag (fire-on-window-can-move (drag-obj app))))
                  ((equalp (in-drag app) "s")
                   (setf obj-top  (height (drag-obj app)))
                   (setf obj-left (width (drag-obj app)))
                   (setf perform-drag (fire-on-window-can-size (drag-obj app))))
                  (t
                    (format t "Warning - invalid data-drag-type attribute")))
            (fire-on-window-change (drag-obj app) app)
            (setf (drag-y app) (- pointer-y obj-top))
            (setf (drag-x app) (- pointer-x obj-left))
            (cond (perform-drag
                    (set-on-pointer-move obj 'on-gui-drag-move)
                    (set-on-pointer-cancel obj
                      (lambda (obj data)
                        (setf (getf data ':screen-x) pointer-x)
                        (setf (getf data ':screen-y) pointer-y)
                        (on-gui-drag-stop obj data)))
                    (set-on-pointer-up obj 'on-gui-drag-stop))
                  (t
                    (setf (in-drag app) nil)))))
      (error ()
             (setf (in-drag app) nil)))))

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
    (set-on-pointer-cancel obj nil)
    (set-on-pointer-up obj nil)
    (when (drag-obj app)
      (cond ((window-maximized-p (drag-obj app))
              (window-maximize (drag-obj app) :focus nil))
            (t
              (let* ((body      (connection-body (drag-obj app)))
                     (mbh       (menu-bar-height (drag-obj app)))
                     (bh        (height (html-document body)))
                     (bw        (width  (html-document body))))
                (make-in-bounds (drag-obj app) mbh bh bw))))
      (cond ((equalp (in-drag app) "m")
              (fire-on-window-move-done (drag-obj app)))
            ((equalp (in-drag app) "s")
             (fire-on-window-size-done (drag-obj app)))))
    (setf (in-drag app) nil)
    (setf (drag-obj app) nil)))

;;;;;;;;;;;;;;;;;;;;;;;
;; create-gui-window ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric create-gui-window (clog-obj &key title
                                          content
                                          left top width height
                                          maximize
                                          hide-title-bar
                                          drag-client-area
                                          has-pinner
                                          closer-html
                                          keep-on-top
                                          window-param
                                          hidden
                                          client-movement
                                          no-sizer
                                          border-class
                                          title-class
                                          html-id)
  (:documentation "Create a clog-gui-window. If client-movement is t then
use jquery-ui to move/resize and will not work on mobile and touch events
are limitted to clicks. When client-movement is t only on-window-move is fired
once at start of drag and on-window-move-done at end of drag and
on-window-resize at start of resize and on-window-resize-done at end of resize.
If has-pinner a toggle will appear on title bar to allow pinning the window in
place, if keep-on-top t then when pinned also will keep-on-top. If had-pinned
is nil and keep-on-top t then the window will be set to keep-on-top always.
window-param is a general parameter for identifiying the window to use with
window-to-top-by-param or window-by-param."))

(defmethod create-gui-window ((obj clog-obj) &key (title "New Window")
                                               (content "")
                                               (left nil)
                                               (top nil)
                                               (width 300)
                                               (height 200)
                                               (maximize nil)
                                               (hide-title-bar nil)
                                               (drag-client-area nil)
                                               (has-pinner nil)
                                               (closer-html "&times;")
                                               (keep-on-top nil)
                                               (window-param nil)
                                               (hidden nil)
                                               (client-movement nil)
                                               (no-sizer nil)
                                               (border-class *default-border-class*)
                                               (title-class *default-title-class*)
                                               (html-id nil))
  (let ((app (connection-data-item obj "clog-gui"))
        (body (connection-body obj)))
    (if html-id
        (setf html-id (format nil "~A" html-id))
        (setf html-id (format nil "~A" (generate-id))))
    (when (eql (hash-table-count (windows app)) 0)
      ;; If previously no open windows reset default position
      (setf (last-x app) 0)
      (setf (last-y app) 0))
    (unless left
      ;; Generate sensible initial x location
      (setf left (last-x app))
      (when (> (last-x app) 600)
        (setf (last-x app) 0))
      (incf (last-x app) 10))
    (unless top
      ;; Generate sensible initial y location
      (when (> (last-y app) 400)
        (setf (last-y app) 0))
      (when (eql (last-y app) 0)
        (setf (last-y app) (menu-bar-height obj)))
      (setf top (last-y app))
      (incf (last-y app) *top-bar-height*)
      (when (> top (- (inner-height (window body)) (last-y app)))
        (setf (last-y app) (menu-bar-height obj))))
    (let ((win (create-child body
                             (format nil
            "<div style='position:fixed;top:~Apx;left:~Apx;width:~Apx;height:~Apx;
                  z-index:~A;visibility:hidden'
                  class='~A'>
                  <div id='~A-title-bar' class='w3-container ~A'
                       style='position:absolute;top:0;right:0;left:0;height:25px'>
                    <span data-drag-obj='~A' data-drag-type='m' id='~A-title'
                      style='position:absolute;top:0;right:20px;left:5px;
                             user-select:none;cursor:move;'>~A</span>
                    <span id='~a-icons' style='position:absolute;top:0;right:15px;
                        cursor:pointer;user-select:none;'></span>
                    <span id='~A-closer'
                      style='position:absolute;top:0;right:5px;cursor:pointer;user-select:none;'>~A</span>
                  </div>
                  <div id='~A-body' ~A style='position:absolute;top:25px;left:0;right:0;bottom:3px;overflow:auto'>~A</div>
                  <div id='~A-sizer' style='position:absolute;right:0;bottom:0;left:0;user-select:none;height:3px;
                       ~Aopacity:0'
                       class='w3-right' data-drag-obj='~A' data-drag-type='s'>+</div>
             </div>"
            top left width height (incf (last-z app))   ; outer div
            border-class
            html-id title-class html-id html-id         ; title bar
            title                                       ; title
            html-id                                     ; icons area
            html-id                                     ; closer
            closer-html
            html-id
            (if drag-client-area
                (format nil "data-drag-obj='~A' data-drag-type='m'" html-id)
                "")
            content                                     ; body
            html-id                                     ; sizer
            (if no-sizer
                ""
                "cursor:se-resize;")
            html-id)
                            :clog-type 'clog-gui-window
                            :html-id html-id)))
      (setf (win-title win)
            (attach-as-child win (format nil "~A-title" html-id)))
      (setf (win-param win) window-param)
      (setf (title-bar win)
            (attach-as-child win (format nil "~A-title-bar" html-id)))
      (when hide-title-bar
        (setf (hiddenp (title-bar win)) t))
      (setf (icon-area win) (attach-as-child win (format nil "~A-icons" html-id)))
      (when has-pinner
        (setf (pinner win) (create-span (icon-area win)
                                        :content (format nil "~A&nbsp;</span>"
                                                         (code-char 9744)))))
      (setf (closer win) (attach-as-child win (format nil "~A-closer" html-id)))
      (unless no-sizer
        (setf (sizer win) (attach-as-child win (format nil "~A-sizer" html-id))))
      (setf (content win) (attach-as-child win (format nil "~A-body"  html-id)))
      (setf (gethash html-id (windows app)) win)
      (set-on-click win (lambda (obj)
                          (declare (ignore obj))
                          (unless (> (modal-count app) 0)
                            (window-focus win))))
      (if maximize
          (window-maximize win)
          (fire-on-window-change win app))
      (unless hidden
        (setf (visiblep win) t))
      (when (window-select app)
        (setf (window-select-item win) (create-option (window-select app)
                                                      :content title
                                                      :selected t
                                                      :value html-id)))
      (set-on-double-click (win-title win) (lambda (obj)
                                             (declare (ignore obj))
                                             (window-toggle-maximize win)))
      (if has-pinner
          (set-on-click (pinner win) (lambda (obj)
                                       (declare (ignore obj))
                                       (window-toggle-pinned win :keep-on-top keep-on-top)))
          (when keep-on-top
            (window-keep-on-top win)))
      (set-on-click (closer win) (lambda (obj)
                                   (declare (ignore obj))
                                   (when (fire-on-window-can-close win)
                                     (window-close win))))
      (cond (client-movement
             (if drag-client-area
                 (progn
                   (jquery-execute win "draggable()")
                   (set-on-touch-start win (lambda (obj data)
                                             (declare (ignore obj data)) nil)
                                       :cancel-event t)
                   (set-on-pointer-down win
                                        (lambda (obj data)
                                          (declare (ignore obj data))
                                          (unless (keep-on-top win)
                                            (setf (z-index win) (incf (last-z app)))
                                            (fire-on-window-change win app)))
                                        :capture-pointer t))
                 (progn
                   (jquery-execute win
                                   (format nil "draggable({handle:'#~A-title-bar'})" html-id))
                   (set-on-touch-start (win-title win) (lambda (obj data)
                                                         (declare (ignore obj data)) nil)
                                       :cancel-event t)
                   (set-on-pointer-down (win-title win)
                                        (lambda (obj data)
                                          (declare (ignore obj data))
                                          (unless (keep-on-top win)
                                            (setf (z-index win) (incf (last-z app)))
                                            (fire-on-window-change win app)))
                                        :capture-pointer t)))
             (jquery-execute win "resizable({handles:'se'})")
             (set-on-event win "dragstart"
                           (lambda (obj)
                             (declare (ignore obj))
                             (fire-on-window-move win)))
             (set-on-event win "dragstop"
                           (lambda (obj)
                             (declare (ignore obj))
                             (fire-on-window-move-done win)))
             (set-on-event win "resizestart"
                           (lambda (obj)
                             (declare (ignore obj))
                             (fire-on-window-size win)))
             (set-on-event win "resizestop"
                           (lambda (obj)
                             (declare (ignore obj))
                             (fire-on-window-size-done win))))
            (t
             (set-on-touch-start (win-title win) (lambda (obj data) 
                                                   (declare (ignore obj data)) nil) 
                                 :cancel-event t)
             (set-on-pointer-down (win-title win) 'on-gui-drag-down :capture-pointer t)
             (if drag-client-area
                 (progn
                   (set-on-touch-start (content win) (lambda (obj data) 
                                                       (declare (ignore obj data)) nil)
                                       :cancel-event t)
                   (set-on-pointer-down (content win) 'on-gui-drag-down :capture-pointer t)))
             (unless no-sizer
               (set-on-touch-start (sizer win) (lambda (obj data)
                                                 (declare (ignore obj data)) nil)
                                   :cancel-event t)
               (set-on-pointer-down
                (sizer win) 'on-gui-drag-down :capture-pointer t))))
      win)))

;;;;;;;;;;;;;;;;;;
;; window-title ;;
;;;;;;;;;;;;;;;;;;

(defgeneric window-title (clog-gui-window)
  (:documentation "Get/setf window title"))

(defmethod window-title ((obj clog-gui-window))
  (inner-html (win-title obj)))

(defgeneric (setf window-title) (value clog-gui-window)
  (:documentation "Set window title"))

(defmethod (setf window-title) (value (obj clog-gui-window))
  (when (window-select-item obj)
    (setf (inner-html (window-select-item obj)) value))
  (setf (inner-html (win-title obj)) value))

;;;;;;;;;;;;;;;;;;
;; window-param ;;
;;;;;;;;;;;;;;;;;;

(defgeneric window-param (clog-gui-window)
  (:documentation "Get/setf window param"))

(defmethod window-param ((obj clog-gui-window))
  (win-param obj))

(defgeneric (setf window-param) (value clog-gui-window)
  (:documentation "Set window param"))

(defmethod (setf window-param) (value (obj clog-gui-window))
  (setf (win-param obj) value))

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
  (let ((app (connection-data-item obj "clog-gui"))
        (pop (connection-data-item obj "clog-popup")))
    (when app
      (unless (keep-on-top obj)
        (when (last-z app)
          (setf (z-index obj) (incf (last-z app)))))
      (when (window-select app)
        (when (window-select-item obj)
          (setf (selectedp (window-select-item obj)) t)))
      (when pop
        (focus pop))
      (fire-on-window-change obj app)))
  obj)

;;;;;;;;;;;;;;;;;;
;; window-close ;;
;;;;;;;;;;;;;;;;;;

(defgeneric window-close (clog-gui-window)
  (:documentation "Close CLOG-GUI-WINDOW. on-window-can-close is not called.
CLOG-GUI-WINDOW is removed from DOM but still present in the CLOG cache on
the browser."))

(defmethod window-close ((obj clog-gui-window))
  (let ((app (connection-data-item obj "clog-gui")))
    (when app
      (remhash (html-id obj) (windows app))
      (when (window-select app)
        (destroy (window-select-item obj)))
      (remove-from-dom obj)
      (fire-on-window-change nil app)
      (fire-on-window-close obj)))
  nil)

;;;;;;;;;;;;;;;;;;;;
;; window-valid-p ;;
;;;;;;;;;;;;;;;;;;;;

(defun window-valid-p (obj)
  "Returns t if is a valid clog-gui-window. An invalid state
can occur when a popup slave desktop is closed by the OS or the window has
been previously closed. If the obj exists in the internal hash of windows
it is removed."
  (when obj
    (when (typep obj 'clog-gui-window)
      (let* ((app (connection-data-item obj "clog-gui"))
             (win (when app
                    (gethash (html-id obj) (windows app)))))
        (when win
          (when (connection-data-item win "clog-gui")
                obj))))))

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

(defgeneric window-maximize (clog-gui-window &key focus)
  (:documentation "Set CLOG-GUI-WINDOW as maximized window and
:focus (default t)."))

(defmethod window-maximize ((obj clog-gui-window) &key (focus t))
  (when (window-valid-p obj)
    (bordeaux-threads:with-lock-held ((window-size-mutex obj)) ; prevent race condition of maximize/normalize
      (let ((app (connection-data-item obj "clog-gui")))
        (when focus
	  (unless (keep-on-top obj)
	    (window-focus obj)))
        (when (fire-on-window-can-maximize obj)
	  (unless (window-maximized-p obj)
            (setf (last-x obj) (left obj))
            (setf (last-y obj) (top obj))
            (setf (last-height obj) (height obj))
            (setf (last-width obj) (width obj)))
          (cond ((connection-data-item obj "clog-popup")
                 (setf (top obj) (unit :px 0))
                 (setf (height obj) (inner-height (window (connection-body obj)))))
                (t
                 (setf (top obj) (unit :px (menu-bar-height obj)))
	         (setf (height obj)
                       (- (inner-height (window (connection-body obj))) (menu-bar-height obj)))))
	  (setf (left obj) (unit :px 0))
	  (setf (width obj) (unit :vw 100))
	  (setf (left obj) (unit :px (body-left-offset app)))
	  (setf (width obj) (- (width obj)
                               (body-left-offset app)
                               (body-right-offset app)))
	  (fire-on-window-size-done obj))))))

;;;;;;;;;;;;;;;;;;;;;;
;; window-normalize ;;
;;;;;;;;;;;;;;;;;;;;;;

(defgeneric window-normalize (clog-gui-window &key focus)
  (:documentation "Set CLOG-GUI-WINDOW as normalized window an
:focus (default t)."))

(defmethod window-normalize ((obj clog-gui-window) &key (focus t))
  (bordeaux-threads:with-lock-held ((window-size-mutex obj)) ; prevent race condition of maximize/normalize
    (when focus
      (unless (keep-on-top obj)
	(window-focus obj)))
    (when (fire-on-window-can-normalize obj)
      (when (window-maximized-p obj)
	(setf (width obj) (last-width obj))
	(setf (height obj) (last-height obj))
	(setf (top obj) (last-y obj))
	(setf (left obj) (last-x obj))
	(setf (last-width obj) nil)
	(fire-on-window-size-done obj)))))

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

(defgeneric window-toggle-pinned (clog-gui-window &key state keep-on-top)
  (:documentation "Toggle the pinned state of a CLOG-GUI-WINDOW. A pinned
window cannot be moved, closed, resized, maximized or normalized. A new
window is always unpinned. If keep-on-top the keep-on-top state is toggled
to match the pinned state. :state forces state. Returns new state"))

(defmethod window-toggle-pinned ((win clog-gui-window) &key (state :toggle)
                                                         keep-on-top)
  (if (or (eq state nil)
          (and (eq state :toggle)
               (pinnedp win)))
      (progn
        (when (pinner win)
          (setf (inner-html (pinner win)) (format nil "~A&nbsp;" (code-char 9744))))
        (when keep-on-top
          (window-keep-on-top win :state nil))
        (setf (pinnedp win) nil)
        (set-on-window-can-close win nil)
        (set-on-window-can-size win nil)
        (set-on-window-can-move win nil)
        (set-on-window-can-maximize win nil)
        (set-on-window-can-normalize win nil)
        nil)
      (flet ((no-op (obj) (declare (ignore obj)) nil))
        (when (pinner win)
          (setf (inner-html (pinner win)) (format nil "~A&nbsp;" (code-char 9745))))
        (when keep-on-top
          (window-keep-on-top win))
        (setf (pinnedp win) t)
        (set-on-window-can-close win #'no-op)
        (set-on-window-can-size win #'no-op)
        (set-on-window-can-move win #'no-op)
        (set-on-window-can-maximize win #'no-op)
        (set-on-window-can-normalize win #'no-op)
        t)))

;;;;;;;;;;;;;;;;;;;;;;;;
;; window-keep-on-top ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric window-keep-on-top (clog-gui-window &key state)
  (:documentation "Set CLOG-GUI-WINDOW to stay on top based on state (default t)."))

(defmethod window-keep-on-top ((obj clog-gui-window) &key (state t))
  (cond (state
         (setf (keep-on-top obj) t)
         (setf (z-index obj) 1))
        (t
         (setf (keep-on-top obj) nil)
         (window-focus obj))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; window-toggle-title-bar ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric window-toggle-title-bar (clog-gui-window &key state)
  (:documentation "Set CLOG-GUI-WINDOW title bar to visible or not (default t)."))

(defmethod window-toggle-title-bar ((obj clog-gui-window) &key (state :toggle))
  (when (eq state :toggle)
    (if (hiddenp (title-bar obj))
        (setf state t)
        (setf state nil)))
  (cond (state
         (setf (hiddenp (title-bar obj)) nil))
        (t
         (setf (hiddenp (title-bar obj)) t)))
  state)

;;;;;;;;;;;;;;;;;;;;;;;
;; window-make-modal ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric window-make-modal (clog-gui-window)
  (:documentation "Set CLOG-GUI-WINDOW to stay on top and prevent all other
interactions. Use window-end-modal to undo."))

(defmethod window-make-modal ((obj clog-gui-window))
  (let ((app (connection-data-item obj "clog-gui")))
    (when (and app
	       (<= (modal-count app) 0))
      (setf (modal-background app) (create-div (connection-body obj) :class "w3-overlay"))
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
  (let ((body (connection-body obj)))
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
  (:documentation "Set the on-window-move-done HANDLER."))

(defmethod set-on-window-move-done ((obj clog-gui-window) handler)
  (setf (on-window-move-done obj) handler))

(defmethod fire-on-window-move-done ((obj clog-gui-window))
  (when (on-window-move-done obj)
    (funcall (on-window-move-done obj) obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - Dialog Boxes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;
;; alert-toast ;;
;;;;;;;;;;;;;;;;;

(defun alert-toast (obj title content &key (color-class "w3-red")
                                        (time-out nil)
                                        (place-top nil)
                                        (html-id nil))
  "Create an alert toast with option :TIME-OUT. If place-top is t then alert
is placed in DOM at top of html body instead of bottom of html body. Note,
when time-out alert-toast blocks and the toast is displayed for time-out or
until user closes the toast."
  (unless html-id
      (setf html-id (generate-id)))
  (let* ((sem    (when time-out (bordeaux-threads:make-semaphore)))
         (body   (connection-body obj))
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
       (destroy win)
       (setf win nil)
       (when sem
         (bordeaux-threads:signal-semaphore sem))))
    (when sem
      (bordeaux-threads:wait-on-semaphore sem :timeout time-out)
      (when win
        (destroy win)))))

;;;;;;;;;;;;;;;;;;
;; alert-dialog ;;
;;;;;;;;;;;;;;;;;;

(defun alert-dialog (obj content &key (modal t)
                                   (title "About")
                                   (time-out nil)
                                   (left nil) (top nil)
                                   (width 300) (height 200)
                                   (client-movement nil)
                                   (html-id nil))
  "Create an alert dialog box with CONTENT centered. If time-out
alert-dialog blocks till time-out reached or OK clicked."
  (unless html-id
      (setf html-id (generate-id)))
  (let* ((sem  (when time-out (bordeaux-threads:make-semaphore)))
         (body (connection-body obj))
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
                        (window-close win)))
    (set-on-window-close win (lambda (obj)
                               (declare (ignore obj))
                               (when modal
                                 (window-end-modal win)
                                 (setf win nil)
                                 (when sem
                                   (bordeaux-threads:signal-semaphore sem)))))
    (when sem
      (bordeaux-threads:wait-on-semaphore sem :timeout time-out)
      (when win
        (window-close win)))))


;;;;;;;;;;;;;;;;;;
;; input-dialog ;;
;;;;;;;;;;;;;;;;;;

(defun input-dialog (obj content on-input &key (modal t)
                                            (time-out nil)
                                            (title "Input")
                                            (size 20)
                                            (rows 1)
                                            (placeholder-value "")
                                            (default-value "")
                                            is-password
                                            (spell-check t)
                                            (left nil) (top nil)
                                            (width 300) (height 200)
                                            (client-movement nil)
                                            (html-id nil))
  "Create an input dialog box with CONTENT centered and an input box.
Calls on-input with input box contents or nil if canceled. If time-out
block time-out seconds for responce or cancels dialog box then returns
result of on-input."
  (unless html-id
      (setf html-id (generate-id)))
  (let* ((sem  (when time-out (bordeaux-threads:make-semaphore)))
         (result nil)
         (body (connection-body obj))
         (inp  (if (eql rows 1)
                   (format nil "<input type='~A' id='~A-input' size='~A' value='~A' placeholder='~A'>"
                           (if is-password
                               "password"
                               "text")
                           html-id
                           size
                           (escape-string default-value :html t)
                           (escape-string placeholder-value :html t))
                   (format nil "<textarea id='~A-input' cols='~A' rows='~A'>~A</textarea>"
                           html-id
                           size
                           rows
                           (escape-string default-value :html t))))
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
                                  :clog-type (if (eql rows 1)
                                                 `clog:clog-form-element
                                                 'clog:clog-text-area)))
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
    (unless spell-check
      (setf (spellcheckp input) nil))
    (focus input)
    (set-on-click cancel (lambda (obj)
                           (declare (ignore obj))
                           (window-close win))
                  :one-time t)
    (set-on-click ok (lambda (obj)
                       (declare (ignore obj))
                       (let ((r (text-value input)))
                         (set-on-window-close win nil)
                         (when modal
                           (window-end-modal win))
                         (window-close win)
                         (setf result (funcall on-input r))
                         (when sem
                           (bordeaux-threads:signal-semaphore sem))))
                  :one-time t)
    (set-on-window-close win (lambda (obj)
                               (declare (ignore obj))
                               (when modal
                                 (window-end-modal win))
                               (setf result (funcall on-input nil))
                               (when sem
                                 (bordeaux-threads:signal-semaphore sem))))
    (when sem
      (unless (bordeaux-threads:wait-on-semaphore sem :timeout time-out)
        (setf sem nil)
        (window-close win)))
    result))


;;;;;;;;;;;;;;;;;;;;
;; confirm-dialog ;;
;;;;;;;;;;;;;;;;;;;;

(defun confirm-dialog (obj content on-input &key (modal t)
                                              (title "Confirm")
                                              (ok-text "OK")
                                              (cancel-text "Cancel")
                                              (time-out nil)
                                              (left nil) (top nil)
                                              (width 300) (height 200)
                                              (client-movement nil)
                                              (html-id nil))
  "Create a confirmation dialog box with CONTENT centered.
Calls on-input with t if confirmed or nil if canceled."
  (unless html-id
      (setf html-id (generate-id)))
  (let* ((sem  (when time-out (bordeaux-threads:make-semaphore)))
         (result nil)
         (body (connection-body obj))
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
                       (setf result (funcall on-input t))
                       (when sem
                         (bordeaux-threads:signal-semaphore sem)))
                  :one-time t)
    (set-on-window-close win (lambda (obj)
                               (declare (ignore obj))
                               (when modal
                                 (window-end-modal win))
                               (setf result (funcall on-input nil))
                               (when sem
                                 (bordeaux-threads:signal-semaphore sem))))
    (when sem
      (unless (bordeaux-threads:wait-on-semaphore sem :timeout time-out)
        (setf sem nil)
        (window-close win)))
    result))

;;;;;;;;;;;;;;;;;;;
;; prompt-dialog ;;
;;;;;;;;;;;;;;;;;;;

(defun prompt-dialog (obj callback
                      &key
                        (title "Prompt")
                        (completion #'list)
                        (validation (constantly t))
                        (presentation (lambda (it) (format nil "~a" it)))
                        (initial-value "")
                        (modal t)
                        time-out
                        left top (width 390) (height 425)
                        maximize
                        client-movement
                        (keep-on-top t)
                        html-id)
  "Create a prompt dialog box with a selection of items generated by
the provided COMPLETION function.

COMPLETION is a function that takes the current string and returns
a list of options.

VALIDATION is a predicate that you can specify to prevent the user
from entering a malformed text input.

PRESENTATION is used if COMPLETION yields something other than strings.

Pressing <Tab> will replace the input field with the top completion,
as you might expect in an IDE.  Clicking an item will also put it in
the input field.

Pressing <Escape> will cancel the prompt."
  (let* ((sem (when time-out (bt2:make-semaphore)))
         (result nil)
         (body (connection-body obj))
         (win (create-gui-window obj
                                 :title title
                                 :maximize maximize
                                 :top top
                                 :left left
                                 :width width
                                 :height height
                                 :hidden t
                                 :client-movement client-movement
                                 :keep-on-top keep-on-top
                                 :html-id html-id))
         (form (create-form (window-content win)
                            :style "width:100%;display:flex;"))
         (input (create-form-element form :input :style "flex-grow:1;"))
         (ok (create-button form
                            :content "Okay"
                            :class "w3-button w3-black"))
         (cancel (create-button form
                                :content "Cancel"
                                :class "w3-button w3-black"))
         (items-list (create-unordered-list
                      (window-content win)
                      :class "w3-ul w3-hoverable"))
         (items '()))
    (unless top
      (setf (top win) (unit :px (- (/ (inner-height (window body)) 2.0)
                                   (/ (height win) 2.0)))))
    (unless left
      (setf (left win) (unit :px (- (/ (inner-width (window body)) 2.0)
                                    (/ (width win) 2.0)))))
    (setf (overflow items-list) :auto)
    (setf (visiblep win) t)
    (setf (value input) initial-value)
    (setf (attribute input "autocomplete") "off")
    (when modal
      (window-make-modal win))
    (flet ((refresh-completions ()
             (setf (inner-html items-list) "")
             (setf items '())
             (dolist (it (funcall completion (value input)))
               (setf items (append items (list it)))
               (let ((li (create-list-item
                          items-list
                          :content (funcall presentation it))))
                 (set-on-click li (lambda (obj)
                                    (declare (ignore obj))
                                    (setf (value input) it)
                                    (focus input)))))
             (focus input)))
      (refresh-completions)
      (set-on-window-close win (lambda (obj)
                                 (declare (ignore obj))
                                 (when modal
                                   (window-end-modal win))
                                 (when sem
                                   (bt2:signal-semaphore sem))))
      (set-on-click ok (lambda (obj)
                         (declare (ignore obj))
                         (when (funcall validation (value input))
                           (when modal
                             (window-end-modal win))
                           (setf result (funcall callback (value input)))
                           (window-close win)))
                    :one-time t)
      (set-on-click cancel (lambda (obj)
                             (declare (ignore obj))
                             (window-close win)))
      (set-on-key-down input (lambda (obj ev)
                               (declare (ignore obj))
                               (let ((key (getf ev :key)))
                                 (cond ((equal "Escape" key)
                                        (window-close win))
                                       ((equal "Tab" key)
                                        (setf (value input) (car items))
                                        )))
                               (refresh-completions)))
      (when sem
        (unless (bt2:wait-on-semaphore sem :timeout time-out)
          (setf sem nil)
          (window-close win)))
      result)))

;;;;;;;;;;;;;;;;;
;; form-dialog ;;
;;;;;;;;;;;;;;;;;

(defun form-dialog (obj content fields on-input &key (modal t)
                                                  (title "Form")
                                                  (ok-text "OK")
                                                  (cancel-text "Cancel")
                                                  (time-out nil)
                                                  (left nil) (top nil)
                                                  (width 400) (height 500)


                                                  (size 40) (rows 4)
                                                  (client-movement nil)
                                                  (html-id nil))
  "Create a form dialog box with CONTENT followed by FIELDS.
FIELDS is a list of lists each list has:

    (1) Field description  - Used for label
    (2) Field name         - Used for (name attribute)
    (3) Field type         - Optional (defaults to :text)
    (4) Field type options - Optional

Special field types

   Field Type     Field Type Options
   =============  ==================
   :filename      default dir -- NOTE: This is _server_ side!
   :checkbox      t if checked
   :radiobox      a-list ((label name)) a third value can be added \"checked\"
   :select        a-list ((label name)) a third value can be added \"selected\"
   :textarea      value
   :text          value
     (any text input types also work :email, :tel, etc.
      see FORM-ELEMENT-TYPE)

The size of any texarea field is controled by the size and rows parameters

Calls on-input after OK or Cancel with an a-list of field name to value
if confirmed or nil if canceled. If time-out is set the result of
on-input returned after either ok or cancel or time elapses."
  (unless html-id
    (setf html-id (generate-id)))
  (let* ((sem  (when time-out (bordeaux-threads:make-semaphore)))
         (result nil)
         (body (connection-body obj))
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
                                  ((eq (third l) :textarea)
                                   (format nil
                                           "<div><label class='w3-text-black'><b>~A</b></label>
                                                 <textarea
                                                  name='~A-~A' id='~A-~A' cols='~A' rows='~A'>~A</textarea></div>"
                                           (first l)
                                           html-id
                                           (second l)
                                           html-id
                                           (second l)
                                           size
                                           rows
                                           (if (fourth l)
                                               (fourth l)
                                               "")))
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
                                                          html-id ok-text ; ok
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
                                                            (setf (text-value fld) fname))))))))
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
                       (setf result (mapcar
                                     (lambda (l)
                                       `(,(second l)
                                         ,(let ((name (format nil "~A-~A" html-id (second l))))
                                            (cond ((eq (third l) :select)
                                                   (select-value win name))
                                                  ((eq (third l) :radio)
                                                   (radio-value win name))
                                                  ((eq (third l) :checkbox)
                                                   (checkbox-value win name))
                                                  ((eq (third l) :textarea)
                                                   (textarea-value win name))
                                                  (t
                                                   (name-value win name))))))
                                     fields))
                       (window-close win)
                       (setf result (funcall on-input result))
                       (when sem
                         (bordeaux-threads:signal-semaphore sem)))
                  :one-time t)
    (set-on-window-close win (lambda (obj)
                               (declare (ignore obj))
                               (when modal
                                 (window-end-modal win))
                               (setf result (funcall on-input nil))
                               (when sem
                                 (bordeaux-threads:signal-semaphore sem))))
    (when sem
      (unless (bordeaux-threads:wait-on-semaphore sem :timeout time-out)
        (setf sem nil)
        (window-close win)))
    result))

;;;;;;;;;;;;;;;;;;;;;;;;
;; server-file-dialog ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defun server-file-dialog (obj title initial-dir on-file-name
                           &key (modal t)
                             (time-out nil)
                             (left nil) (top nil) (width 390) (height 425)
                             (maximize nil)
                             (initial-filename nil)
                             (client-movement nil)
                             (html-id nil))
  "Create a local file dialog box called TITLE using INITIAL-DIR on server
machine, upon close ON-FILE-NAME called with filename or nil if failure.
If time-out return result of on-file-name, cancels dialog if time runs out."
  (let* ((sem  (when time-out (bordeaux-threads:make-semaphore)))
         (result nil)
         (body (connection-body obj))
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
         (form   (create-form (window-content win)))
         (fname  (file-namestring initial-dir))
         (dirs   (create-select form))
         (files  (create-select form))
         (input  (create-form-element form :input))
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
    (setf (positioning dirs) :absolute)
    (setf (positioning files) :absolute)
    (setf (positioning input) :absolute)
    (setf (positioning ok) :absolute)
    (setf (positioning cancel) :absolute)
    (setf (size dirs) 4)
    (setf (size files) 4)
    (set-geometry dirs :left 5 :right 5 :top 5 :height 75)
    (set-geometry files :left 5 :right 5 :top 85 :bottom 90)
    (set-geometry input :left 5 :right 5 :bottom 65 :height 20)
    (set-geometry ok :right 95 :bottom 0  :width 85)
    (set-geometry cancel :right 5 :bottom 0 :width 85)
    (setf (overflow files) :auto)
    (setf (overflow dirs) :auto)
    (setf (visiblep win) t)
    (when modal
      (window-make-modal win))
    (flet ((populate-dirs (d)
             (let ((dir (directory-namestring d)))
               (setf (inner-html dirs) "")
               (add-select-option dirs (format nil "~A" dir) ".")
               (setf (text-value input) (if (equal fname "")
                                            (truename dir)
                                            (format nil "~A~A" (truename dir) fname)))
               (unless (or (equalp dir "/") (equalp dir #P"/"))
                 (add-select-option dirs (format nil "~A../" dir) ".."))
               (dolist (item (uiop:subdirectories dir))
                 (add-select-option dirs item item))))
           (populate-files (dir)
             (setf (inner-html files) "")
             (dolist (item (uiop:directory-files (directory-namestring dir)))
               (add-select-option files item (file-namestring item))))
           (caret-at-end ()
             (focus input)
             (js-execute win (format nil "~A.setSelectionRange(~A.value.length,~A.value.length)"
                                     (script-id input)
                                     (script-id input)
                                     (script-id input)))))
      (populate-dirs initial-dir)
      (populate-files initial-dir)
      (when initial-filename
        (ignore-errors
         (setf (text-value input) (truename initial-filename)))
        (caret-at-end))
      (set-on-change files (lambda (obj)
                             (declare (ignore obj))
                             (setf (text-value input) (truename (text-value files)))
                             (caret-at-end)))
      (set-on-change dirs (lambda (obj)
                            (declare (ignore obj))
                            (setf (text-value input) (text-value dirs))
                            (caret-at-end)
                            (populate-files (text-value dirs))))
      (set-on-double-click dirs
                           (lambda (obj)
                             (declare (ignore obj))
                             (populate-dirs (truename (text-value dirs)))))
      (set-on-double-click files (lambda (obj)
                                   (declare (ignore obj))
                                   (click ok))))
    (set-on-window-close win (lambda (obj)
                               (declare (ignore obj))
                               (when modal
                                 (window-end-modal win))
                               (setf result (funcall on-file-name nil))
                               (when sem
                                 (bordeaux-threads:signal-semaphore sem))))
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
                       (setf result (funcall on-file-name (text-value input)))
                       (when sem
                         (bordeaux-threads:signal-semaphore sem)))
                  :one-time t)
    (when sem
      (unless (bordeaux-threads:wait-on-semaphore sem :timeout time-out)
        (setf sem nil)
        (window-close win)))
    result))



;;;;;;;;;;;;;;;;;;;;;;
;; dialog-in-stream ;;
;;;;;;;;;;;;;;;;;;;;;;

(defclass dialog-in-stream (trivial-gray-streams:fundamental-character-input-stream)
  ((clog-obj :reader   obj       :initarg  :clog-obj)
   (outbuf   :reader   outbuf    :initarg  :source)
   (buffer   :accessor buffer-of :initform "")
   (index    :accessor index     :initform 0))
  (:documentation "dialog-in-stream and dialog-out-stream can be combined with
make-two-way-stream to provide a *query-io* using a clog-gui instead of console)"))

(defmethod trivial-gray-streams:stream-read-char ((stream dialog-in-stream))
  (when (eql (index stream) (length (buffer-of stream)))
    (setf (buffer-of stream) "")
    (setf (index stream) 0))
  (when (eql (index stream) 0)
    (input-dialog (obj stream) (prompt (outbuf stream)) (lambda (result)
                                                          (add-line stream result))
                  :time-out 999
                  :modal nil))
  (when (< (index stream) (length (buffer-of stream)))
    (prog1
        (char (buffer-of stream) (index stream))
      (incf (index stream)))))

(defmethod trivial-gray-streams:stream-unread-char ((stream dialog-in-stream) character)
  (decf (index stream)))

(defmethod trivial-gray-streams:stream-line-column ((stream dialog-in-stream))
  nil)

(defmethod add-line ((stream dialog-in-stream) text)
  (setf (buffer-of stream) (format nil "~A~A~%" (buffer-of stream) text)))

;;;;;;;;;;;;;;;;;;;;;;;
;; dialog-out-stream ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defclass dialog-out-stream (trivial-gray-streams:fundamental-character-output-stream)
  ((buffer :accessor buffer-of :initform ""))
  (:documentation "dialog-in-stream and dialog-out-stream can be combined with
make-two-way-stream to provide a *query-io* using a clog-gui instead of console)"))

(defmethod trivial-gray-streams:stream-write-char ((stream dialog-out-stream) character)
  (setf (buffer-of stream) (format nil "~A~A" (buffer-of stream) character)))

(defmethod trivial-gray-streams:stream-line-column ((stream dialog-out-stream))
  nil)

(defmethod prompt ((stream dialog-out-stream))
  (prog1
      (buffer-of stream)
    (setf (buffer-of stream) "")))

;;;;;;;;;;;;
;; one-of ;;
;;;;;;;;;;;;

(defun one-of-dialog (obj intro choices &key (title "Please choose one") (prompt "Choice"))
  "Prompt a dialog box with TITLE and INTRO using list of CHOICES and PROMPT"
  (let ((q  (format nil "<pre>~A</pre><p style='text-align:left'>" (escape-for-html intro)))
        (n (length choices)) (i))
    (do ((c choices (cdr c)) (i 1 (+ i 1)))
        ((null c))
      (setf q (format nil "~A~&[~D] ~A~%<br>" q i (escape-for-html (car c)))))
    (do () ((typep i `(integer 1 ,n)))
      (let ((trc (make-array '(0) :element-type 'base-char
                             :fill-pointer 0 :adjustable t)))
        (with-output-to-string (s trc)
          (uiop:print-condition-backtrace intro :stream s))
        (when trc
          (format t "~%~A~%" trc)))
      (setf q (format nil "~A~&~A:" q prompt))
      (setq i (read-from-string (input-dialog obj q (lambda (result)
                                                      (cond ((or (eq result nil)
                                                                 (equal result ""))
                                                              (format nil "~A" n))
                                                            (t
                                                              result)))
                                              :title title
                                              :placeholder-value (format nil "~A" n)
                                              :time-out 999
                                              :modal nil
                                              :width 640
                                              :height 480))))
    (nth (- i 1) choices)))

(defparameter *default-icon*
  "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAACAAAAAcCAYAAAAAwr0iAAAAAXNSR0IArs4c6QAAAKZlWElmTU0A
KgAAAAgABwEGAAMAAAABAAIAAAESAAMAAAABAAEAAAEaAAUAAAABAAAAYgEbAAUAAAABAAAAagEo
AAMAAAABAAIAAAExAAIAAAAVAAAAcodpAAQAAAABAAAAiAAAAAAAAABIAAAAAQAAAEgAAAABUGl4
ZWxtYXRvciBQcm8gMi4wLjUAAAACoAIABAAAAAEAAAAgoAMABAAAAAEAAAAcAAAAAMSXmL0AAAAJ
cEhZcwAACxMAAAsTAQCanBgAAAQRaVRYdFhNTDpjb20uYWRvYmUueG1wAAAAAAA8eDp4bXBtZXRh
IHhtbG5zOng9ImFkb2JlOm5zOm1ldGEvIiB4OnhtcHRrPSJYTVAgQ29yZSA2LjAuMCI+CiAgIDxy
ZGY6UkRGIHhtbG5zOnJkZj0iaHR0cDovL3d3dy53My5vcmcvMTk5OS8wMi8yMi1yZGYtc3ludGF4
LW5zIyI+CiAgICAgIDxyZGY6RGVzY3JpcHRpb24gcmRmOmFib3V0PSIiCiAgICAgICAgICAgIHht
bG5zOmV4aWY9Imh0dHA6Ly9ucy5hZG9iZS5jb20vZXhpZi8xLjAvIgogICAgICAgICAgICB4bWxu
czp4bXA9Imh0dHA6Ly9ucy5hZG9iZS5jb20veGFwLzEuMC8iCiAgICAgICAgICAgIHhtbG5zOnRp
ZmY9Imh0dHA6Ly9ucy5hZG9iZS5jb20vdGlmZi8xLjAvIj4KICAgICAgICAgPGV4aWY6Q29sb3JT
cGFjZT4xPC9leGlmOkNvbG9yU3BhY2U+CiAgICAgICAgIDxleGlmOlBpeGVsWERpbWVuc2lvbj4z
MjwvZXhpZjpQaXhlbFhEaW1lbnNpb24+CiAgICAgICAgIDxleGlmOlBpeGVsWURpbWVuc2lvbj4y
ODwvZXhpZjpQaXhlbFlEaW1lbnNpb24+CiAgICAgICAgIDx4bXA6Q3JlYXRvclRvb2w+UGl4ZWxt
YXRvciBQcm8gMi4wLjU8L3htcDpDcmVhdG9yVG9vbD4KICAgICAgICAgPHhtcDpNZXRhZGF0YURh
dGU+MjAyMS0wMi0wNFQwMzo0MDoxOVo8L3htcDpNZXRhZGF0YURhdGU+CiAgICAgICAgIDx0aWZm
OlJlc29sdXRpb25Vbml0PjI8L3RpZmY6UmVzb2x1dGlvblVuaXQ+CiAgICAgICAgIDx0aWZmOlBo
b3RvbWV0cmljSW50ZXJwcmV0YXRpb24+MjwvdGlmZjpQaG90b21ldHJpY0ludGVycHJldGF0aW9u
PgogICAgICAgICA8dGlmZjpDb21wcmVzc2lvbj4xPC90aWZmOkNvbXByZXNzaW9uPgogICAgICAg
ICA8dGlmZjpPcmllbnRhdGlvbj4xPC90aWZmOk9yaWVudGF0aW9uPgogICAgICAgICA8dGlmZjpY
UmVzb2x1dGlvbj43MjAwMDAvMTAwMDA8L3RpZmY6WFJlc29sdXRpb24+CiAgICAgICAgIDx0aWZm
OllSZXNvbHV0aW9uPjcyMDAwMC8xMDAwMDwvdGlmZjpZUmVzb2x1dGlvbj4KICAgICAgPC9yZGY6
RGVzY3JpcHRpb24+CiAgIDwvcmRmOlJERj4KPC94OnhtcG1ldGE+CjH2KYwAAAMuSURBVEgN7Za7
a1RBFId3N3GjUQMJNuIDGwVFRW0sFBTRNpWVhYKFWtko2PgH2NgpgtgERK1EUQSxsBIMYpAIgsRX
o4WFYNS8k/X7zc7vOrmbZO+uYOWBb8+ZM3PmzOvObKn0X/7hCtRqtXI+XYMj3+BvyySt0kcFpqEG
y2J5slwuz2G3L3RegW7ozPeCr2shv9tRV4ZK2ytAsGYywyw0qxLlbtQkyD+LXzOWfx1qF2yGLbAc
blP/BB2WRrolodOqEig5sg0q2GOgxBMwjW8TnKTjfuiADzAIK+AudWfQrQuBXY7Cvg6/4BFslx+9
D87CeTgCvW5vje8ODINWq7gQEJKjte+PIZVvFC7DQTgEG9OeKetMhIToAfgEPWmbJW0a6zRrhr3w
HCQzMA5acstFd4SjB1ZHrTOieA1O8gJ0HpoLDZ18DfYrkEyBBiCZg4lg1X9uovryPeM7ACMguQfN
t8CN0H2gfZNMwmyw6smjmQ1I5ddwAnZDP1yDH2C5mh9gQ5mW3vOV2EMxUjN18uiap9LtUMUoaIUs
jj3VkNAOWnbCqqT8NEYredpZdDcoJfH2pJVegS84t7r/TOOsQjgscmLr5D4Aifbcow+OAj8ahFZE
KFYHVnIlS2oDp+7rTCgfgzcgUXCryUNg8vM92h/RG5QIXb9TMMKVjD4KN+AlWLTk7Sb3rL30urT2
x+R/vgCcYfZofRqpqIMie57G2M4fxhEq9sTkHdhh0vlXbFwNkAnQt69G7TxYUzF+Fn0JhmCYN+Id
ifUuzGGHRyw/gPdUSpR03pkI3mI/Tq4Ex0l0y2Fx1lly+3UYvAWHsSW+1Vpd/nTZLygBfek/g57N
QJY0NVTpMrYP4E/sVkSD9cDvJ/2Fa9zlRTXBXoWd2J65Tq1m5TLmgqI2Y7FG+7xWidDFHhuPigAP
Qvf3W7BoALr/jS4lobJnjVkbhPAMo7P/De6/kCbQg9Cnchr0bDaTrzQ4B47NH+5Fc2d7n7ZQR5ze
7B8rZd3be2EHrAe9E/rP9xmewUPaj6LDoUtj5WtLSKpTG2ZUpAO1VUyRtmmbQgGxYw8mXCCxE8c3
fttpliXs3y+7fSKpo8d7AAAAAElFTkSuQmCC")
