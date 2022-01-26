;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG Builder - UI Design tool for CLOG                                ;;;;
;;;; (c) 2020-2022 David Botton                                            ;;;;
;;;; License BSD 3 Clause                                                  ;;;;
;;;;                                                                       ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :clog-tools)

;; Per instance app data

(defclass builder-app-data ()
  ((copy-buf
    :accessor copy-buf
    :initform ""
    :documentation "Copy buffer")
   (next-panel-id
    :accessor next-panel-id
    :initform 0
    :documentation "Next new panel id")
   (current-control
    :accessor current-control
    :initform nil
    :documentation "Current selected control")
   (select-tool
    :accessor select-tool
    :initform nil
    :documentation "Select tool")
   (control-lists
    :accessor control-lists
    :initform (make-hash-table :test #'equalp)
    :documentation "Panel to Control List hash table")
   (properties-list
    :accessor properties-list
    :initform nil
    :documentation "Property list in properties window")
   (control-properties-win
    :accessor control-properties-win
    :initform nil
    :documentation "Current control properties window")
   (events-list
    :accessor events-list
    :initform nil
    :documentation "Property list in events window")
   (control-events-win
    :accessor control-events-win
    :initform nil
    :documentation "Current control events window")
   (control-list-win
    :accessor control-list-win
    :initform nil
    :documentation "Current control list window")
   (control-pallete-win
    :accessor control-pallete-win
    :initform nil
    :documentation "Current control pallete window")))

;; Cross page syncing

(defparameter *app-sync-hash* (make-hash-table :test #'equal)
  "Exchange app instance with new external pages")

;; Control-List utilities

(defun init-control-list (app panel-id)
  "Initialize new control list for PANEL-ID on instance of APP."
  (setf (gethash panel-id (control-lists app)) (make-hash-table :test #'equalp)))

(defun destroy-control-list (app panel-id)
  "Destroy the control-list on PANEL-ID"
  (remhash panel-id (control-lists app)))

(defun get-control-list (app panel-id)
  "Rerieve the control-list hash table on PANEL-ID"
  (gethash panel-id (control-lists app)))

(defun add-to-control-list (app panel-id control)
  "Add a CONTROL on to control-list on PANEL-ID"
  (let ((html-id (format nil "~A" (html-id control))))
    (setf (gethash html-id (get-control-list app panel-id)) control)))

(defun get-from-control-list (app panel-id html-id)
  "Get control identified my HTML-ID from control-list on PANEL-ID"
  (gethash html-id (get-control-list app panel-id)))

(defun remove-from-control-list (app panel-id html-id)
  "Remove a control identified by HTML-ID from control-list on PANEL-ID"
  (remhash html-id (get-control-list app panel-id)))

;; Handle per content next-id counts

(defun next-id (content)
  (parse-integer (attribute content "data-clog-next-id") :junk-allowed t))

(defun setf-next-id (content id)
  (setf (attribute content "data-clog-next-id") (format nil "~A" id)))

(defun incf-next-id (content)
  (setf-next-id content (1+ (next-id content))))


;; Local file utilities

(defun read-file (infile)
  "Read local file"
  (with-open-file (instream infile :direction :input :if-does-not-exist nil)
    (when instream
      (let ((string (make-string (file-length instream))))
        (read-sequence string instream)
        string))))

(defun write-file (string outfile &key (action-if-exists :rename))
  "Write local file"
   (check-type action-if-exists (member nil :error :new-version :rename :rename-and-delete
					    :overwrite :append :supersede))
   (with-open-file (outstream outfile :direction :output :if-exists action-if-exists)
     (write-sequence string outstream)))

;; Lisp code evaluation utilities

(defun capture-eval (form &key (eval-in-package "clog-user"))
  "Capture lisp evaluaton of FORM"
  (let ((result (make-array '(0) :element-type 'base-char
				 :fill-pointer 0 :adjustable t))
	(eval-result))
    (with-output-to-string (stream result)
      (let* ((*standard-output* stream)
	     (*error-output* stream)
	     (*package* (find-package (string-upcase eval-in-package))))
	(setf eval-result (eval (read-from-string (format nil "(progn ~A)" form))))))
    (format nil "~A~%=>~A~%" result eval-result)))

;; Control utilities

(defun control-info (control-type-name)
  "Return control informaton record for CONTROL-TYPE-NAME from the *supported-controls* list."
  (if (equal control-type-name "clog-data")
       '(:name           "clog-data"
	 :description    "Panel Properties"
	 :events         nil
	 :properties     ((:name "in-package"
			   :attr "data-in-package")))
      (find-if (lambda (x) (equal (getf x :name) control-type-name)) *supported-controls*)))

(defun create-control (parent content control-record uid)
  "Return a new control based on CONTROL-RECORD as a child of PARENT"
  (let* ((create-type       (getf control-record :create-type))
	 (control-type-name (getf control-record :name))
	 (control           (cond ((eq create-type :base)
				   (funcall (getf control-record :create) parent
					    :html-id uid))
				  ((eq create-type :element)
				   (funcall (getf control-record :create) parent
					    :html-id uid
					    :content (if (equal (getf control-record :create-content) "")
							 ""
							 (format nil "~A-~A"
								 (getf control-record :create-content)
								 (next-id content)))))
				  ((eq create-type :form)
				   (funcall (getf control-record :create) parent
					    (getf control-record :create-param)
					    :html-id uid
					    :value (if (equal (getf control-record :create-value) "")
						       ""
						       (format nil "~A-~A"
							       (getf control-record :create-value)
							       (next-id content)))))
				  ((eq create-type :textarea)
				   (funcall (getf control-record :create) parent
					    :html-id uid
					    :value (getf control-record :create-value)))
				  (t nil))))
    (when control
      (setf (attribute control "data-clog-type") control-type-name)
      (when (getf control-record :setup)
	(funcall (getf control-record :setup) control content control-record)))
    control))

(defun drop-new-control (app content data &key win)
  "Create new control droppend at event DATA on CONTENT of WIN)"
  ;; any click on panel directly will focus window
  (when win
    (window-focus win))
  ;; create control
  (let* ((control-record    (control-info (value (select-tool app))))
	 (control-type-name (getf control-record :name))
	 (positioning       (if (getf data :ctrl-key)
				:static
				:absolute))
	 (parent            (when (getf data :shift-key)
			      (current-control app)))
	 (control           (create-control (if parent
						parent
						content)
					    content
					    control-record
					    (format nil "B~A~A"
						    (get-universal-time)
						    (next-id content)))))
    (cond (control
	   ;; panel directly clicked with a control type selected
	   ;; setup control
	   (setf (attribute control "data-clog-name")
		 (format nil "~A-~A" control-type-name (next-id content)))
	   (setf (value (select-tool app)) 0)
	   (setf (box-sizing control) :content-box)
	   (setf (positioning control) positioning)
	   (set-geometry control
			 :left (getf data :x)
			 :top (getf data :y))
	   (setup-control content control :win win)
	   (select-control control)
	   (on-populate-control-list-win content)
	   t)
	  (t
	   ;; panel directly clicked with select tool or no control type to add
	   (deselect-current-control app)
	   (on-populate-control-properties-win content :win win)
	   (on-populate-control-list-win content)
	   nil))))

(defun setup-control (content control &key win)
  "Setup CONTROL by creating pacer and setting up events for manipulation"
  (let ((app      (connection-data-item content "builder-app-data"))
	(panel-id (html-id content))
	(placer   (create-div control :auto-place nil :html-id (format nil "p-~A" (html-id control)))))
    (add-to-control-list app panel-id control)
    ;; setup placer
    (set-geometry placer :top (position-top control)
			 :left (position-left control)
			 :width (client-width control)
			 :height (client-height control))
    (place-after control placer)
    (setf (box-sizing placer) :content-box)
    (setf (positioning placer) :absolute)
    (clog::jquery-execute placer "draggable().resizable()")
    ;; setup control events
    (set-on-focus control (lambda (obj)
			    (declare (ignore obj))
			    ;; set focus is bound in case control
			    ;; is set to static or reached using
			    ;; tab selection
			    (select-control obj)))
    ;; setup placer events
    (set-on-mouse-down placer
		       (lambda (obj data)
			 (declare (ignore obj) (ignore data))
			 (select-control control)
			 (when win
			   (window-focus win)))
		       :cancel-event t)
    (clog::set-on-event placer "resizestop"
			(lambda (obj)
			  (set-geometry control :units ""
						:width (width placer)
						:height (height placer))
			  (set-geometry placer :units ""
					       :width (client-width control)
					       :height (client-height control))
			  (on-populate-control-properties-win content :win win)))
    (clog::set-on-event placer "dragstop"
			(lambda (obj)
			  (set-geometry control :units ""
						:top (top placer)
						:left (left placer))
			  (set-geometry placer :top (top control)
					       :left (left control))
			  (on-populate-control-properties-win content :win win)))))

;; Control selection utilities

(defun get-placer (control)
  "Get placer for CONTROL. A placer is a div placed on top of the control and
access to it and allows manipulation of location, size etc of the control."
  (when control
    (attach-as-child control (format nil "p-~A" (html-id control)))))

(defun deselect-current-control (app)
  "Remove selection on current control and remove visual ques on its placer."
  (when (current-control app)
    (set-border (get-placer (current-control app)) (unit "px" 0) :none :blue)
    (setf (current-control app) nil)))

(defun delete-current-control (app panel-id html-id)
  (remove-from-control-list app panel-id html-id)
  (destroy (get-placer (current-control app)))
  (destroy (current-control app))
  (setf (current-control app) nil))

(defun select-control (control)
  "Select CONTROL as the current control and highlight its placer.
The actual original clog object used for creation must be used and
not a temporary attached one when using select-control."
  (let ((app    (connection-data-item control "builder-app-data"))
	(placer (get-placer control)))
    (deselect-current-control app)
    (set-geometry placer :top (top control)
			 :left (left control)
			 :width (client-width control)
			 :height (client-height control))
    (setf (current-control app) control)
    (set-border placer (unit "px" 2) :solid :blue)
    (on-populate-control-properties-win control)))

(defun add-sub-controls (parent content &key win)
  "Setup html imported in to CONTENT starting with PARENT for use with Builder"
  (let ((app       (connection-data-item content "builder-app-data"))
	(panel-uid (get-universal-time))
	(panel-id  (html-id content)))
    ;; Assign any elements with no id, an id, name and type
    (let ((tmp (format nil
		       "var clog_id=~A; var clog_nid=1;~
      $(~A).find('*').each(function() {var e=$(this);~
        var t=e.prop('tagName').toLowerCase(); var p=e.attr('data-clog-type');~
        if((e.attr('id') === undefined) && (e.attr('data-clog-name') === undefined))~
           {e.attr('id','A'+clog_id++);~
            e.attr('data-clog-name','none-'+t+'-'+clog_nid++)}~
        if(e.attr('data-clog-name') === undefined){e.attr('data-clog-name',e.attr('id'))}~
        ~{~A~}~
        if(e.attr('data-clog-type') === undefined){e.attr('data-clog-type','span')}})"
		       panel-uid
		       (clog::jquery parent)
		       (mapcar (lambda (l)
				 (format nil "if(p === undefined && t=='~A'){e.attr('data-clog-type','~A')}"
					 (getf l :tag) (getf l :control)))
			       *import-types*))))
      (clog::js-execute parent tmp))
    (let* ((data (first-child content))
	   (name    (attribute data "data-clog-title"))
	   (next-id (attribute data "data-clog-next-id"))
	   (package (attribute data "data-in-package")))
      (when name
	(unless (equalp next-id "undefined")
	  (setf-next-id content next-id))
	(unless (equalp package "undefined")
	  (setf (attribute content "data-in-package") package))
	(unless (equalp name "undefined")
	  (setf (attribute content "data-clog-name") name)
	  (destroy data))))
    (labels ((add-siblings (control)
	       (let (dct)
		 (loop
		   (when (equal (html-id control) "undefined") (return))
		   (setf dct (attribute control "data-clog-type"))
		   (unless (equal dct "undefined")
		     (change-class control (getf (control-info dct) :clog-type))
		     (setup-control content control :win win)
		     (add-siblings (first-child control)))
		   (setf control (next-sibling control))))))
      (add-siblings (first-child parent)))))

;; Population of utility windows

(defun on-populate-control-events-win (obj)
  "Populate the control events for the current control"
  ;; obj if current-control is nil must be content
  (let* ((app       (connection-data-item obj "builder-app-data"))
	 (event-win (control-events-win app))
	 (control   (if (current-control app)
		        (current-control app)
		        obj))
	 (table     (events-list app)))
    (when event-win
      (setf (inner-html table) "")
      (let ((info  (control-info (attribute control "data-clog-type")))
	    events)
	(dolist (event (reverse (getf info :events)))
	  (let ((attr (format nil "data-~A" (getf event :name))))
	    (push `(,(getf event :name) ,(attribute control attr) ,(getf event :parameters)
		    ,(getf event :setup)
		    ,(lambda (obj)
		       (let ((txt (text obj)))
			 (if (or (equal txt "")
				 (equalp txt "undefined"))
			     (remove-attribute control attr)
			     (setf (attribute control attr) (text obj))))))
		  events)))
	(dolist (item events)
	  (let* ((tr  (create-table-row table))
		 (td1 (create-table-column tr :content (first item)))
		 (td2 (if (second item)
			  (create-table-column tr :content (second item))
			  (create-table-column tr))))
	    (set-border td1 "1px" :dotted :black)
	    (setf (spellcheckp td2) nil)
	    (setf (advisory-title td1) (format nil "params: panel ~A" (third item)))
	    (cond ((fourth item)
		   (setf (editablep td2) (funcall (fourth item) control td1 td2)))
		  (t
		   (setf (editablep td2) t)))
	    (set-on-blur td2
			 (lambda (obj)
			   (funcall (fifth item) obj)))))))))

(defun on-populate-control-properties-win (obj &key win)
  "Populate the control properties for the current control"
  ;; obj if current-control is nil must be content
  (on-populate-control-events-win obj)
  (let* ((app      (connection-data-item obj "builder-app-data"))
	 (prop-win (control-properties-win app))
	 (control  (if (current-control app)
		       (current-control app)
		       obj))
	 (placer   (when control
		     (get-placer control)))
	 (table    (properties-list app)))
    (when prop-win
      (setf (inner-html table) "")
      (let ((info  (control-info (attribute control "data-clog-type")))
	    props)
	(dolist (prop (reverse (getf info :properties)))
	  (cond ((eq (third prop) :style)
		 (push `(,(getf prop :name) ,(style control (getf prop :style)) ,(getf prop :setup)
			 ,(lambda (obj)
			    (setf (style control (getf prop :style)) (text obj))))
		       props))
		((or (eq (third prop) :get)
		     (eq (third prop) :set)
		     (eq (third prop) :setup))
		 (push `(,(getf prop :name) ,(when (getf prop :get)
					       (funcall (getf prop :get) control))
			 ,(getf prop :setup)
			 ,(lambda (obj)
			    (when (getf prop :set)
			      (funcall (getf prop :set) control obj))))
		       props))
		((eq (third prop) :setf)
		 (push `(,(getf prop :name) ,(funcall (getf prop :setf) control) ,(getf prop :setup)
			 ,(lambda (obj)
			    (funcall (find-symbol (format nil "SET-~A" (getf prop :setf)) :clog) control (text obj))))
		       props))
		((eq (third prop) :prop)
		 (push `(,(getf prop :name) ,(property control (getf prop :prop)) ,(getf prop :setup)
			 ,(lambda (obj)
			    (setf (property control (getf prop :prop)) (text obj))))
		       props))
		((eq (third prop) :attr)
		 (push `(,(getf prop :name) ,(attribute control (getf prop :attr)) ,(getf prop :setup)
			 ,(lambda (obj)
			    (setf (attribute control (getf prop :attr)) (text obj))))
		       props))
		(t (print "Configuration error."))))
	(when (current-control app)
	    (push
	     `("parent"  ,(attribute (parent-element control) "data-clog-name")
			 nil
			 ,(lambda (obj)
			    (place-inside-bottom-of
			     (attach-as-child control
					      (clog::js-query
					       control
					       (format nil "$(\"[data-clog-name='~A']\").attr('id')"
						       (text obj))))
			     control)
			    (place-after control placer)))
	     props))
	(push
	 `("name"    ,(attribute control "data-clog-name")
		      nil
		      ,(lambda (obj)
			 (setf (attribute control "data-clog-name") (text obj))
			 (when (equal (getf info :name) "clog-data")
			   (setf (window-title win) (text obj)))))
	 props)
	(dolist (item props)
	  (let* ((tr  (create-table-row table))
		 (td1 (create-table-column tr :content (first item)))
		 (td2 (if (second item)
			  (create-table-column tr :content (second item))
			  (create-table-column tr))))
	    (set-border td1 "1px" :dotted :black)
	    (cond ((third item)
		   (unless (eq (third item) :read-only)
		     (setf (editablep td2) (funcall (third item) control td1 td2))))
		  (t
		   (setf (editablep td2) t)))
	    (set-on-blur td2
			 (lambda (obj)
			   (funcall (fourth item) obj)
			   (when placer
			     (set-geometry placer :top (position-top control)
						  :left (position-left control)
						  :width (client-width control)
						  :height (client-height control)))))))))))

(defun on-populate-loaded-window (content &key win)
  "Setup html imported in to CONTENT for use with Builder"
  (add-sub-controls content content :win win))

(defun on-populate-control-list-win (content)
  "Populate the control-list-window to allow drag and drop adjust of order
of controls and double click to select control."
  (let ((app      (connection-data-item content "builder-app-data"))
	(panel-id (html-id content))
	(last-ctl nil))
    (when (control-list-win app)
      (let ((win (window-content (control-list-win app))))
	(setf (inner-html win) "")
	(labels ((add-siblings (control sim)
		   (let (dln)
		     (loop
		       (when (equal (html-id control) "undefined") (return))
		       (setf dln (attribute control "data-clog-name"))
		       (unless (equal dln "undefined")
			 (let ((list-item (create-div win :content (format nil "&#8597; ~A~A" sim dln)))
			       (status    (hiddenp (get-placer control))))
			   (if status
			       (setf (background-color list-item) :gray)
			       (setf (background-color list-item) :lightgray))
			   (setf (draggablep list-item) t)
			   (setf (attribute list-item "data-clog-control") (html-id control))
			   ;; click to select item
			   (set-on-mouse-down list-item
					 (lambda (obj data)
					   (let* ((html-id (attribute obj "data-clog-control"))
						  (control (get-from-control-list app
										  panel-id
										  html-id)))
					     (cond ((or (getf data :shift-key)
						       (getf data :ctrl-key))
						    (when (drop-new-control app content data)
						      (incf-next-id content)))
						   (t
						    (when last-ctl
						      (set-border last-ctl "0px" :dotted :blue))
						    (set-border list-item "2px" :dotted :blue)
						    (setf last-ctl list-item)
						    (select-control control))))))
			   (set-on-double-click list-item
						(lambda (obj)
						  (let* ((html-id (attribute obj "data-clog-control"))
							 (control (get-from-control-list app
											 panel-id
											 html-id))
							 (placer  (get-placer control))
							 (state   (hiddenp placer)))
						    (setf (hiddenp placer) (not state))
						    (select-control control))))
			   ;; drag and drop to change
			   (set-on-drag-over list-item (lambda (obj)(declare (ignore obj))()))
			   (set-on-drop list-item
					(lambda (obj data)
					  (let* ((id       (attribute obj "data-clog-control"))
						 (control1 (get-from-control-list app
										  panel-id
										  id))
						 (control2 (get-from-control-list app
										  panel-id
										  (getf data :drag-data)))
						 (placer1  (get-placer control1))
						 (placer2  (get-placer control2)))
						      (if (getf data :shift-key)
							  (place-inside-bottom-of control1 control2)
							  (place-before control1 control2))
						      (place-after control2 placer2)
						      (set-geometry placer1 :top (position-top control1)
									    :left (position-left control1)
									    :width (client-width control1)
									    :height (client-height control1))
						      (set-geometry placer2 :top (position-top control2)
									    :left (position-left control2)
									    :width (client-width control2)
									    :height (client-height control2))
						      (on-populate-control-list-win content))))
			   (set-on-drag-start list-item (lambda (obj)(declare (ignore obj))())
					      :drag-data (html-id control))
			   (add-siblings (first-child control) (format nil "~A&#8594;" sim))))
		       (setf control (next-sibling control))))))
	  (add-siblings (first-child content) ""))))))

;; Menu handlers

(defun do-ide-edit-copy (obj)
  "Copy to clipboard in to app data and browser's host OS"
  (let ((cw (current-window obj)))
    (when cw
      (let ((app (connection-data-item obj "builder-app-data")))
	(setf (copy-buf app) (js-query obj
		    (format nil "editor_~A.execCommand('copy');~
                                 navigator.clipboard.writeText(editor_~A.getCopyText());~
                                 editor_~A.getCopyText();"
			    (html-id cw) (html-id cw) (html-id cw))))))))

(defun do-ide-edit-undo (obj)
  "Undo typing in editor"
  (let ((cw (current-window obj)))
    (when cw
      (do-ide-edit-copy obj)
      (js-execute obj (format nil "editor_~A.execCommand('undo')"
			      (html-id cw))))))

(defun do-ide-edit-redo (obj)
  "Redo typing in editor"
  (let ((cw (current-window obj)))
    (when cw
      (js-execute obj (format nil "editor_~A.execCommand('redo')"
			      (html-id cw))))))

(defun do-ide-edit-cut (obj)
  "Cut to clipboard it to app data and browser's host OS"
  (let ((cw (current-window obj)))
    (when cw
      (do-ide-edit-copy obj)
      (js-execute obj (format nil "editor_~A.execCommand('cut')"
			      (html-id cw))))))

(defun do-ide-edit-paste (obj)
  "Paste from browser's host OS clip buffer"
  (let ((cw (current-window obj)))
    (when cw
      ;; Note this methods uses the global clip buffer and not (copy-buf app)
      ;; on copy and paste we set both the global and local buffer.
      (js-execute obj (format nil "navigator.clipboard.readText().then(function(text) {~
                                        editor_~A.execCommand('paste', text)~
                                     })"
			      (html-id cw))))))

(defun do-eval (obj &key cname (package "clog-user"))
  "Do lisp eval of editor contents"
  (let ((cw (current-window obj)))
    (when cw
      (let* ((form-string (js-query obj (format nil "editor_~A.getValue()"
						(html-id (current-window obj)))))
	     (result      (capture-eval (if cname
					    (format nil "~A~% (clog:set-on-new-window~
                                               (lambda (body)~
                                                 (clog-gui:clog-gui-initialize body)~
                                                 (clog-web:clog-web-initialize body :w3-css-url nil)~
                                                 (create-~A body)) :path \"/test\")~
                                                 (clog:open-browser :url \"http://127.0.0.1:8080/test\")"
						    form-string cname))
					:eval-in-package package)))
	(alert-dialog obj result :title "Eval Result")))))

(defun on-show-layout-code (obj &key package cname)
  "Show a lisp editor"
  (let* ((win         (create-gui-window obj :title  "Layout Code"
					     :height 400
					     :width  650))
	 (box         (create-panel-box-layout (window-content win)
					       :left-width 0 :right-width 9
					       :top-height 30 :bottom-height 0))
	 (file-name   "")
	 (center      (center-panel box))
	 (center-id   (html-id center))
	 (tool-bar    (top-panel box))
	 (btn-save    (create-button tool-bar :content "Save"))
	 (btn-eval    (create-button tool-bar :content "Eval"))
	 (btn-run     (create-button tool-bar :content "Test")))
    (setf (background-color tool-bar) :silver)
    (set-on-click btn-eval (lambda (obj)
			     (do-eval obj :package package)))
    (set-on-click btn-run (lambda (obj)
			     (do-eval obj :package package :cname cname)))
    (set-on-click btn-save (lambda (obj)
			     (server-file-dialog obj "Save As.." file-name
						 (lambda (fname)
						   (window-focus win)
						   (when fname
						     (setf (window-title win) fname)
						     (setf file-name fname)
						     (write-file (js-query obj (format nil "editor_~A.getValue()"
										       (html-id win)))
								 fname)))
						 :initial-filename file-name)))
    (set-on-window-size win (lambda (obj)
			      (js-execute obj
					  (format nil "editor_~A.resize()" (html-id win)))))
    (set-on-window-size-done win (lambda (obj)
				   (js-execute obj
					       (format nil "editor_~A.resize()" (html-id win)))))
    (create-child win
		  (format nil
			  "<script>
                            var editor_~A = ace.edit('~A');
                            editor_~A.setTheme('ace/theme/xcode');
                            editor_~A.session.setMode('ace/mode/lisp');
                            editor_~A.session.setTabSize(3);
                            editor_~A.focus();
                           </script>"
			(html-id win) center-id
			(html-id win)
			(html-id win)
			(html-id win)
			(html-id win)))
    win))

(defun on-show-control-properties-win (obj)
  "Show control properties window"
  (let ((app (connection-data-item obj "builder-app-data")))
    (if (control-properties-win app)
	(window-focus (control-properties-win app))
	(let* ((win          (create-gui-window obj :title "Control Properties"
						    :left 630
						    :top 40
						    :height 510 :width 400
						    :has-pinner t))
	       (content      (window-content win))
	       (control-list (create-table content)))
	  (setf (control-properties-win app) win)
	  (setf (properties-list app) control-list)
	  (set-on-window-close win (lambda (obj) (setf (control-properties-win app) nil)))
	  (setf (positioning control-list) :absolute)
	  (set-geometry control-list :left 0 :top 0 :bottom 0 :right 0)))))

(defun on-show-control-events-win (obj)
  "Show control events window"
  (let ((app (connection-data-item obj "builder-app-data")))
    (if (control-events-win app)
	(window-focus (control-events-win app))
	(let* ((win          (create-gui-window obj :title "Control Events"
						    :left 220
						    :top 350
						    :height 200 :width 400
						    :has-pinner t))
	       (content      (window-content win))
	       (control-list (create-table content)))
	  (setf (control-events-win app) win)
	  (setf (events-list app) control-list)
	  (set-on-window-close win (lambda (obj) (setf (control-events-win app) nil)))
	  (setf (positioning control-list) :absolute)
	  (set-geometry control-list :left 0 :top 0 :bottom 0 :right 0)))))

(defun on-show-control-pallete-win (obj)
  "Show control pallete"
  (let ((app (connection-data-item obj "builder-app-data")))
    (if (control-pallete-win app)
	(window-focus (control-pallete-win app))
	(let* ((win          (create-gui-window obj :title "Control Pallete"
						    :top 40
						    :left 0
						    :height 300 :width 200 :has-pinner t))
	       (content      (window-content win))
	       (control-list (create-select content)))
	  (setf (control-pallete-win app) win)
	  (set-on-window-close win (lambda (obj) (setf (control-pallete-win app) nil)))
	  (setf (positioning control-list) :absolute)
	  (setf (size control-list) 2)
	  (set-geometry control-list :units "" :left 0 :top 0 :bottom 0 :width "100%")
	  (setf (advisory-title control-list) (format nil "<ctrl> place static~%<shift> child to selected"))
	  (setf (select-tool app) control-list)
	  (dolist (control *supported-controls*)
	    (add-select-option control-list (getf control :name) (getf control :description)))))))

(defun on-show-control-list-win (obj)
  "Show control list for selecting and manipulating controls by name"
  (let ((app (connection-data-item obj "builder-app-data")))
    (if (control-list-win app)
	(window-focus (control-list-win app))
	(let* ((win (create-gui-window obj :title "Control List"
					   :top 350
					   :left 0
					   :width 200 :has-pinner t)))
	  (setf (control-list-win app) win)
	  (setf (advisory-title (window-content win))
		(format nil "Drag and drop order~%Double click non-focusable~%~
                             <ctrl> place static~%<shift> child to selected"))
	  (set-on-window-close win (lambda (obj) (setf (control-list-win app) nil)))))))

(defun render-clog-code (content win hide-loc)
  "Render panel to clog code and add tp CW window"
  (let* ((app      (connection-data-item content "builder-app-data"))
	 (panel-id (html-id content))
	 (package  (attribute content "data-in-package"))
	 (cname    (attribute content "data-clog-name"))
	 (cw       (on-show-layout-code win :cname cname :package package))
	 cmembers vars events)
    (maphash (lambda (html-id control)
	       (place-inside-bottom-of hide-loc
				       (get-placer control))
	       (let ((vname (attribute control "data-clog-name")))
		 (unless (and (>= (length vname) 5)
			      (equalp (subseq vname 0 5) "none-"))
		   (push (format nil
				 "\(~A :reader ~A\)"
				 vname
				 vname)
			 cmembers)
		   (push (format nil
				 "\(setf (slot-value panel '~A\) ~
                                    \(attach-as-child clog-obj \"~A\" :clog-type \'~A\ :new-id t)\)~%"
				 vname
				 html-id
				 (format nil "CLOG:~A" (type-of control)))
			 vars)
		   (let ((info  (control-info (attribute control "data-clog-type"))))
		     (dolist (event (getf info :events))
		       (let ((handler (attribute control (format nil "data-~A" (getf event :name)))))
			 (unless (or (equalp handler "undefined")
				     (equal handler ""))
			   (push (format nil
					 "\(set-~A \(~A panel\) \(lambda \(~A\) \(declare \(ignorable ~A\)\) ~A\)\)~%"
					 (getf event :name)
					 vname
					 (getf event :parameters)
					 (getf event :parameters)
					 handler)
				 events))))))))
	     (get-control-list app panel-id))
    (let ((result (format nil
			  "\(in-package \"~A\"\)
\(defclass ~A \(clog:clog-div\)
  \(~{~A~}\)\)
\(defun create-~A \(clog-obj\)
  \(let \(\(panel \(change-class \(clog:create-div clog-obj :content \"~A\"\) \'~A\)\)\)
~{~A~}
~{~A~}
    panel\)\)"
			  (string-upcase package)
			  cname     ;;defclass
			  cmembers
			  cname     ;;defun
			  (escape-string
			   (ppcre:regex-replace-all "\\x22"
						    (inner-html content)
						    "\\\\\\\""))
			  cname
			  vars
			  events)))
      (js-execute cw (format nil
			     "editor_~A.setValue('~A');editor_~A.moveCursorTo(0,0);"
			     (html-id cw)
			     (escape-string result)
			     (html-id cw))))
    (maphash (lambda (html-id control)
	       (declare (ignore html-id))
	       (place-after control (get-placer control)))
	     (get-control-list app panel-id))))

(defun save-panel (fname content panel-id hide-loc)
  (let ((app (connection-data-item content "builder-app-data")))
    (maphash
     (lambda (html-id control)
       (declare (ignore html-id))
       (place-inside-bottom-of hide-loc
			       (get-placer control)))
     (get-control-list app panel-id))
    (let ((data
	    (create-child content "<data />"
			  :html-id (format nil "I~A" (get-universal-time)))))
      (place-inside-top-of content data)
      (setf (attribute data "data-in-package")
	    (attribute content "data-in-package"))
      (setf (attribute data "data-clog-next-id")
	    (attribute content "data-clog-next-id"))
      (setf (attribute data "data-clog-title")
	    (attribute content "data-clog-name"))
      (write-file (inner-html content) fname)
      (destroy data))
    (maphash
     (lambda (html-id control)
       (declare (ignore html-id))
       (place-after control (get-placer control)))
     (get-control-list app panel-id))))

(defun on-new-builder-panel (obj)
  "Open new panel"
  (let* ((app (connection-data-item obj "builder-app-data"))
	 (win (create-gui-window obj :top 40 :left 220
				     :width 400 :height 300
				     :client-movement t))
	 (box (create-panel-box-layout (window-content win)
				       :left-width 0 :right-width 0
				       :top-height 30 :bottom-height 0))
	 (tool-bar  (top-panel box))
	 (btn-del   (create-button tool-bar :content "Del"))
	 (btn-sim   (create-button tool-bar :content "Simulate"))
	 (btn-rndr  (create-button tool-bar :content "Render"))
	 (btn-save  (create-button tool-bar :content "Save"))
	 (btn-load  (create-button tool-bar :content "Load"))
	 (content   (center-panel box))
	 (in-simulation nil)
	 (file-name "")
	 (panel-uid (get-universal-time)) ;; unique id for panel
	 (panel-id  (html-id content)))
    (setf-next-id content 1)
    (setf (overflow content) :auto)
    (init-control-list app panel-id)
    ;; setup panel window
    (let ((panel-name (format nil "panel-~A" (incf (next-panel-id app)))))
      (setf (window-title win) panel-name)
      (setf (attribute content "data-clog-name") panel-name))
    (setf (attribute content "data-clog-type") "clog-data")
    (setf (attribute content "data-in-package") "clog-user")
    (setf (background-color tool-bar) :silver)
    ;; activate associated windows on open
    (on-populate-control-properties-win content :win win)
    (on-populate-control-list-win content)
    ;; setup window events
    (set-on-window-focus win
			 (lambda (obj)
			   (declare (ignore obj))
			   (on-populate-control-properties-win content :win win)
			   (on-populate-control-list-win content)))
    (set-on-window-close win
			 (lambda (obj)
			   (declare (ignore obj))
			   ;; clear associated windows on close
			   (setf (current-control app) nil)
			   (destroy-control-list app panel-id)
			   (on-populate-control-properties-win content :win win)
			   (on-populate-control-list-win content)))
    ;; setup tool bar events
    (set-on-click btn-del (lambda (obj)
			    (declare (ignore obj))
			    (when (current-control app)
			      (delete-current-control app panel-id (html-id (current-control app)))
			      (on-populate-control-properties-win content :win win)
			      (on-populate-control-list-win content))))
    (set-on-click btn-sim (lambda (obj)
			    (declare (ignore obj))
			    (cond (in-simulation
				   (setf (text btn-sim) "Simulate")
				   (setf in-simulation nil)
				   (maphash (lambda (html-id control)
					      (declare (ignore html-id))
					      (setf (hiddenp (get-placer control)) nil))
					    (get-control-list app panel-id)))
				  (t
				   (setf (text btn-sim) "Develop")
				   (deselect-current-control app)
				   (on-populate-control-properties-win content :win win)
				   (setf in-simulation t)
				   (maphash (lambda (html-id control)
					      (declare (ignore html-id))
					      (setf (hiddenp (get-placer control)) t))
					    (get-control-list app panel-id))
				   (focus (first-child content))))))
    (set-on-click btn-load (lambda (obj)
			     (server-file-dialog obj "Load Panel" file-name
						 (lambda (fname)
						   (window-focus win)
						   (when fname
						     (setf file-name fname)
						     (setf (inner-html content)
							   (escape-string (read-file fname)))
						     (clrhash (get-control-list app panel-id))
						     (on-populate-loaded-window content :win win)
						     (setf (window-title win) (attribute content "data-clog-name")))))))
    (set-on-click btn-save (lambda (obj)
			     (server-file-dialog obj "Save Panel As.." file-name
						 (lambda (fname)
						   (window-focus win)
						   (when fname
						     (setf file-name fname)
						     (save-panel fname content panel-id (bottom-panel box)))
						   :initial-filename file-name))))
    (set-on-click btn-rndr
		  (lambda (obj)
		      (render-clog-code content win (bottom-panel box))))
    (set-on-mouse-down content
		       (lambda (obj data)
			 (declare (ignore obj))
			 (unless in-simulation
			   (when (drop-new-control app content data :win win)
			     (incf-next-id content)))))))

(defun on-attach-builder-page (body)
  "New builder page has attached"
  (let* ((params        (form-get-data body))
	 (panel-uid     (form-data-item params "bid"))
	 (app           (gethash panel-uid *app-sync-hash*))
	 win
	 (box           (create-panel-box-layout body
						 :left-width 0 :right-width 0
						 :top-height 0 :bottom-height 0))
	 (content       (center-panel box))
	 (in-simulation nil)
	 (file-name     "")
	 (panel-id      (html-id content)))
    ;; sync new window with app
    (setf (connection-data-item body "builder-app-data") app)
    (remhash panel-uid *app-sync-hash*)
    (funcall (gethash (format nil "~A-link" panel-uid) *app-sync-hash*) content)
    (setf win (gethash (format nil "~A-win" panel-uid) *app-sync-hash*))
    (remhash (format nil "~A-win" panel-uid) *app-sync-hash*)

    ;; setup window and page
    (setf-next-id content 1)
    (let ((panel-name (format nil "page-~A" (incf (next-panel-id app)))))
      (setf (title (html-document body)) panel-name)
      (setf (window-title win) panel-name)
      (setf (attribute content "data-clog-name") panel-name))
    (setf (attribute content "data-clog-type") "clog-data")
    (setf (attribute content "data-in-package") "clog-user")
    (setf (overflow content) :auto)
    (set-on-focus (window body)
		  (lambda (obj)
		    (declare (ignore obj))
		    (setf (title (html-document body)) (attribute content "data-clog-name"))))
    ;; setup close of page
    (set-on-before-unload (window body)
			  (lambda (obj)
			    (declare (ignore obj))
			    (window-close win)))
    ;; activate associated windows on open
    (deselect-current-control app)
    (on-populate-control-properties-win content :win win)
    (on-populate-control-list-win content)
    ;; setup window events
    (set-on-window-focus win
			 (lambda (obj)
			   (declare (ignore obj))
			   (on-populate-control-properties-win content :win win)
			   (on-populate-control-list-win content)))
    (set-on-window-close win
			 (lambda (obj)
			   (declare (ignore obj))
			   ;; clear associated windows on close
			   (setf (current-control app) nil)
			   (destroy-control-list app panel-id)
			   (close-window (window body))))

    (clog-gui-initialize body)
    (clog-web-initialize body :w3-css-url nil)
    (init-control-list app panel-id)
    (let* ((pbox     (create-panel-box-layout (window-content win)
					 :left-width 0 :right-width 0
					 :top-height 30 :bottom-height 0))
	   (tool-bar (top-panel pbox))
	   (btn-del  (create-button tool-bar :content "Del"))
	   (btn-sim  (create-button tool-bar :content "Simulate"))
	   (btn-rndr (create-button tool-bar :content "Render"))
	   (btn-save (create-button tool-bar :content "Save"))
	   (btn-load (create-button tool-bar :content "Load"))
	   (wcontent  (center-panel pbox)))
      (create-div wcontent :content
		  "<br><center>Drop and work with controls on it's window.</center>")
      (setf (background-color tool-bar) :silver)
      ;; setup tool bar events
      (set-on-click btn-del (lambda (obj)
			      (declare (ignore obj))
			      (when (current-control app)
				(delete-current-control app panel-id (html-id (current-control app)))
				(on-populate-control-properties-win content :win win)
				(on-populate-control-list-win content))))
      (set-on-click btn-sim (lambda (obj)
			      (declare (ignore obj))
			      (cond (in-simulation
				     (setf (text btn-sim) "Simulate")
				     (setf in-simulation nil)
				     (maphash (lambda (html-id control)
						(declare (ignore html-id))
						(setf (hiddenp (get-placer control)) nil))
					      (get-control-list app panel-id)))
				    (t
				     (setf (text btn-sim) "Develop")
				     (deselect-current-control app)
				     (on-populate-control-properties-win content :win win)
				     (setf in-simulation t)
				     (maphash (lambda (html-id control)
						(declare (ignore html-id))
						(setf (hiddenp (get-placer control)) t))
					      (get-control-list app panel-id))
				     (focus (first-child content))))))
      (set-on-click btn-load (lambda (obj)
			       (server-file-dialog win "Load Panel" file-name
						   (lambda (fname)
						     (window-focus win)
						     (when fname
						       (setf file-name fname)
						       (setf (inner-html content)
							     (escape-string (read-file fname)))
						       (clrhash (get-control-list app panel-id))
						       (on-populate-loaded-window content :win win)
						       (setf (title (html-document body)) (attribute content "data-clog-name"))
						       (setf (window-title win) (attribute content "data-clog-name")))))))
      (set-on-click btn-save (lambda (obj)
			       (server-file-dialog obj "Save Page As.." file-name
						   (lambda (fname)
						     (window-focus win)
						     (when fname
						       (setf file-name fname)
						       (save-panel fname content panel-id (bottom-panel box)))
						     :initial-filename file-name))))
      (set-on-click btn-rndr
		    (lambda (obj)
		      (render-clog-code content win (bottom-panel box)))))
    (set-on-mouse-down content
		       (lambda (obj data)
			 (declare (ignore obj))
			 (unless in-simulation
			   (when (drop-new-control app content data :win win)
			     (incf-next-id content)))))))

(defun on-new-builder-page (obj)
  "Open new page"
  (let* ((app (connection-data-item obj "builder-app-data"))
	 (win (create-gui-window obj :top 40 :left 220 :width 400))
	 (panel-uid  (format nil "~A" (get-universal-time))) ;; unique id for panel
	 (link       (format nil "http://127.0.0.1:8080/builder-page?bid=~A" panel-uid))
	 (page-link  (create-a (window-content win)
			       :target "_blank"
			       :content "<br><center><button>
                                   Click if browser does not open new page shortly.
                                   </button></center>"
			       :link link))
	 content panel-id)
    (setf (gethash panel-uid *app-sync-hash*) app)
    (setf (gethash (format nil "~A-win" panel-uid) *app-sync-hash*) win)
    (setf (gethash (format nil "~A-link" panel-uid) *app-sync-hash*)
	  (lambda (obj)
	    (setf content obj)
	    (setf panel-id (html-id content))
	    (destroy page-link)
	    (remhash (format nil "~A-link" panel-uid) *app-sync-hash*)))
    (open-browser :url link)))

(defun on-help-about-builder (obj)
  "Open about box"
  (let ((about (create-gui-window obj
				  :title   "About"
				  :content "<div class='w3-black'>
                                         <center><img src='/img/clogwicon.png'></center>
	                                 <center>CLOG</center>
	                                 <center>The Common Lisp Omnificent GUI</center></div>
			                 <div><p><center>CLOG Builder</center>
                                         <center>(c) 2021 - David Botton</center></p></div>"
				  :width   200
				  :height  215
				  :hidden  t)))
    (window-center about)
    (setf (visiblep about) t)
    (set-on-window-can-size about (lambda (obj)
				    (declare (ignore obj))()))))

(defun on-new-builder (body)
  "Launch instance of the CLOG Builder"
  (set-html-on-close body "Connection Lost")
  (let ((app (make-instance 'builder-app-data)))
    (setf (connection-data-item body "builder-app-data") app)
    (setf (title (html-document body)) "CLOG Builder")
    (clog-gui-initialize body)
    (load-script (html-document body) "https://pagecdn.io/lib/ace/1.4.12/ace.js")
    (add-class body "w3-blue-grey")
    (let* ((menu  (create-gui-menu-bar body))
	   (icon  (create-gui-menu-icon menu :on-click #'on-help-about-builder))
	   (file  (create-gui-menu-drop-down menu :content "Builder"))
	   (edit  (create-gui-menu-drop-down menu :content "Edit"))
	   (tools (create-gui-menu-drop-down menu :content "Tools"))
	   (win   (create-gui-menu-drop-down menu :content "Window"))
	   (help  (create-gui-menu-drop-down menu :content "Help")))
      (declare (ignore icon))
      (create-gui-menu-item file  :content "New CLOG GUI Panel" :on-click 'on-new-builder-panel)
      (create-gui-menu-item file  :content "New CLOG WEB Page"  :on-click 'on-new-builder-page)
      (create-gui-menu-item tools :content "Control Pallete"    :on-click 'on-show-control-pallete-win)
      (create-gui-menu-item tools :content "Control Properties" :on-click 'on-show-control-properties-win)
      (create-gui-menu-item tools :content "Control Events"     :on-click 'on-show-control-events-win)
      (create-gui-menu-item tools :content "Control List"       :on-click 'on-show-control-list-win)
      (create-gui-menu-item edit  :content "Undo"               :on-click #'do-ide-edit-undo)
      (create-gui-menu-item edit  :content "Redo"               :on-click #'do-ide-edit-redo)
      (create-gui-menu-item edit  :content "Copy"               :on-click #'do-ide-edit-copy)
      (create-gui-menu-item edit  :content "Cut"                :on-click #'do-ide-edit-cut)
      (create-gui-menu-item edit  :content "Paste"              :on-click #'do-ide-edit-paste)
      (create-gui-menu-item win   :content "Maximize All"       :on-click #'maximize-all-windows)
      (create-gui-menu-item win   :content "Normalize All"      :on-click #'normalize-all-windows)
      (create-gui-menu-window-select win)
      (create-gui-menu-item help  :content "About"              :on-click #'on-help-about-builder)
      (create-gui-menu-full-screen menu))
    (on-show-control-pallete-win body)
    (on-show-control-list-win body)
    (on-show-control-events-win body)
    (on-show-control-properties-win body)
    (on-new-builder-panel body)
    (set-on-before-unload (window body) (lambda(obj)
					  (declare (ignore obj))
					  ;; return empty string to prevent nav off page
					  ""))
    (run body)))

(defun clog-builder ()
  "Start clog-builder."
  (initialize nil)
  (set-on-new-window 'on-new-builder :path "/builder")
  (set-on-new-window 'on-attach-builder-page :path "/builder-page")
  (open-browser :url "http://127.0.0.1:8080/builder"))
