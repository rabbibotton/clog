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
    :initform nil
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
   (properties-list
    :accessor properties-list
    :initform nil
    :documentation "Property list in properties window")
   (events-list
    :accessor events-list
    :initform nil
    :documentation "Property list in events window")
   (properties-lock
    :accessor properties-lock
    :initform (bordeaux-threads:make-lock)
    :documentation "Sync refres properties and event window")
   (control-properties-win
    :accessor control-properties-win
    :initform nil
    :documentation "Current control properties window")
   (control-events-win
    :accessor control-events-win
    :initform nil
    :documentation "Current control events window")
   (control-list-win
    :accessor control-list-win
    :initform nil
    :documentation "Current control list window")
   (control-list-win-lock
    :accessor control-list-win-lock
    :initform (bordeaux-threads:make-lock)
    :documentation "Sync refresh control-list-win")
   (control-pallete-win
    :accessor control-pallete-win
    :initform nil
    :documentation "Current control pallete window")
   (new-control-lock
    :accessor new-control-lock
    :initform (bordeaux-threads:make-lock)
    :documentation "Sync creating new controls")
   (control-lists
    :accessor control-lists
    :initform (make-hash-table* :test #'equalp)
    :documentation "Panel to Control List hash table")))

;; Cross page syncing

(defvar *app-sync-hash* (make-hash-table* :test #'equal)
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

(defun remove-deleted-from-control-list (app panel-id)
  "Remove any deleted control from control-list"
  (maphash (lambda (html-id control)
	     (when (equalp (clog:js-query control (format nil "$.contains(document.documentElement, ~A)"
							  (clog::script-id control))) "false")
	       (remove-from-control-list app panel-id html-id)))
	   (get-control-list app panel-id)))

;; Handle per content next-id counts

(defun next-id (content)
  "Get next id for CONTENT"
  (parse-integer (attribute content "data-clog-next-id") :junk-allowed t))

(defun setf-next-id (content id)
  "Store ID on CONTENT"
  (setf (attribute content "data-clog-next-id") (format nil "~A" id)))

(defun incf-next-id (content)
  "Increment next id and store it in CONTENT"
  (setf-next-id content (1+ (next-id content))))

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

;; Local file utilities

(defun read-file (infile)
  "Read local file named INFILE"
  (with-open-file (instream infile :direction :input :if-does-not-exist nil)
    (when instream
      (let ((string (make-string (file-length instream))))
        (read-sequence string instream)
        string))))

(defun write-file (string outfile &key (action-if-exists :rename))
  "Write local file named OUTFILE"
   (check-type action-if-exists (member nil :error :new-version :rename :rename-and-delete
					    :overwrite :append :supersede))
   (with-open-file (outstream outfile :direction :output :if-exists action-if-exists)
     (write-sequence string outstream)))

(defun save-panel (fname content panel-id hide-loc)
  "Save panel to FNAME"
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
      (setf (attribute data "data-custom-slots")
	    (attribute content "data-custom-slots"))
      (setf (attribute data "data-clog-next-id")
	    (attribute content "data-clog-next-id"))
      (setf (attribute data "data-clog-title")
	    (attribute content "data-clog-name"))
      (write-file (js-query content
			    (format nil
				    "var z=~a.clone();~
                 z.find('*').each(function(){if($(this).attr('id') !== undefined && ~
                 $(this).attr('id').substring(0,5)=='CLOGB'){$(this).removeAttr('id')}});~
                 z.html()"
                                    (clog::jquery content)))
		  fname)
      (destroy data))
    (maphash
     (lambda (html-id control)
       (declare (ignore html-id))
       (place-after control (get-placer control)))
     (get-control-list app panel-id))))

;; Template Utilities

(defun walk-files-and-directories (path process)
  "Walk PATH and apply PROCESS on each (path and file)"
    (let* ((flist (uiop:directory-files path))
	   (dlist (uiop:subdirectories path)))
      (dolist (f flist)
	(funcall process path (file-namestring f)))
      (dolist (d dlist)
	(walk-files-and-directories d process))))

(defun template-copy (sys-name start-dir filename &key panel)
  "Copy START-DIR to FILENAME processing .lt files as cl-template files,
if PANEL each copy produces a <b>source</b> to destination added as
create-div's"
  (walk-files-and-directories
   start-dir
   (lambda (path file)
     (let* ((tmpl-ext "lt")
	    (src-file (format nil "~A~A"
			      path file))
	    (out-dir  (format nil "~A/~A/~A"
			      filename
			      sys-name
			      (subseq (format nil "~A" path)
				      (length start-dir))))
	    (out-file (format nil "~A~A"
			      out-dir
			      file)))
       (ensure-directories-exist out-dir)
       (cond ((equalp (pathname-type file) tmpl-ext)
	      (let* ((nfile (pathname-name file))
		     (afile (if (equalp (pathname-name nfile) "tmpl")
				(format nil "~A~A.~A" out-dir sys-name (pathname-type nfile))
				(format nil "~A~A" out-dir nfile))))
		(write-file (funcall (cl-template:compile-template (read-file src-file))
				     (list :sys-name sys-name))
			    afile)
		(when panel
		  (create-div panel
			      :content (format nil "<b>~A</b> -> ~A"
					       src-file afile)))))
	     (t
	      (uiop:copy-file src-file out-file)
	      (when panel
		(create-div panel
			    :content (format nil "<b>~A</b> -> ~A"
					     src-file out-file)))))))))

;; Control utilities

(defun control-info (control-type-name)
  "Return control informaton record for CONTROL-TYPE-NAME from the *supported-controls* list."
  (if (equal control-type-name "clog-data")
       `(:name           "clog-data"
	 :description    "Panel Properties"
	 :events         nil
	 :properties     ((:name "in-package"
			   :attr "data-in-package")
			  (:name "custom slots"
			   :attr "data-custom-slots")
			  (:name "width"
			   :get  ,(lambda (control) (width control))
			   :setup :read-only)
			  (:name "height"
			   :setup :read-only
			   :get  ,(lambda (control) (height control)))))
      (find-if (lambda (x) (equal (getf x :name) control-type-name)) *supported-controls*)))

(defun create-control (parent content control-record uid &key custom-query)
  "Return a new control based on CONTROL-RECORD as a child of PARENT"
  (let* ((create-type       (getf control-record :create-type))
	 (control-type-name (getf control-record :name))
	 (control           (cond ((eq create-type :base)
				   (funcall (getf control-record :create) parent
					    :html-id uid))
				  ((eq create-type :custom)
				   (funcall (getf control-record :create) parent
					    (getf control-record :create-content)
					    :html-id uid))
				  ((eq create-type :custom-query)
				   (funcall (getf control-record :create) parent
					    custom-query
					    :html-id uid))
				  ((eq create-type :paste)
				   (let ((c (funcall (getf control-record :create) parent
						     custom-query
						     :html-id uid)))
				     (setf control-type-name (attribute c "data-clog-type"))
				     (change-class c (getf (control-info control-type-name) :clog-type))
				     c))
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
  "Create new control droppend at event DATA location on CONTENT of WIN"
  ;; any click on panel directly will focus window
  (when win
    (window-focus win))
  (let* ((control-record    (control-info (value (select-tool app))))
	 (control-type-name (getf control-record :create-type)))
    (if (eq control-type-name :custom-query)
	(input-dialog win "Enter html (must have an outer element):"
		      (lambda (custom-query)
			(when custom-query
			  (do-drop-new-control
			    app content data
			    :win win
			    :custom-query custom-query)))
		      :width 500
		      :height 300
		      :rows 5
		      :size 40
		      :title "Custom HTML Control"
		      :default-value (getf control-record :create-content))
	(do-drop-new-control app content data :win win))))

(defun do-drop-new-control (app content data &key win custom-query)
  "Create new control droppend at event DATA on CONTENT of WIN)"
  ;; create control
  (bordeaux-threads:with-lock-held ((new-control-lock app))
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
					      (format nil "CLOGB~A~A"
						      (get-universal-time)
						      (next-id content))
					      :custom-query custom-query)))
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
	     (add-sub-controls control content :win win)
	     (on-populate-control-list-win content)
	     t)
	    (t
	     ;; panel directly clicked with select tool or no control type to add
	     (deselect-current-control app)
	     (on-populate-control-properties-win content :win win)
	     (on-populate-control-list-win content)
	     nil)))))

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
prevents access to use or activate the control directylu and allows
manipulation of the control's location and size."
  (when control
    (attach-as-child control (format nil "p-~A" (html-id control)))))

(defun deselect-current-control (app)
  "Remove selection on current control and remove visual ques on its placer."
  (when (current-control app)
    (set-border (get-placer (current-control app)) (unit "px" 0) :none :blue)
    (setf (current-control app) nil)))

(defun delete-current-control (app panel-id html-id)
  "Delete the current control"
  (bordeaux-threads:with-lock-held ((new-control-lock app))
    (remove-from-control-list app panel-id html-id)
    (destroy (get-placer (current-control app)))
    (destroy (current-control app))
    (setf (current-control app) nil)
    (remove-deleted-from-control-list app panel-id)))

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

(defun add-sub-controls (parent content &key win paste)
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
           {e.attr('id','CLOGB'+clog_id++);~
            e.attr('data-clog-name','none-'+t+'-'+clog_nid++)}~
        if(e.attr('id') === undefined){e.attr('id','CLOGB'+clog_id++)}~
        if(e.attr('data-clog-name') === undefined){e.attr('data-clog-name',e.attr('id'))}~
        ~A ~
        ~{~A~}~
        if(e.attr('data-clog-type') === undefined){e.attr('data-clog-type','span')}})"
		       panel-uid
		       (clog::jquery parent)
		       (if paste
			   (prog1
			       (format nil "e.attr('data-clog-name', e.attr('data-clog-name')+'-'+~A);"
				       (next-id content))
			     (incf-next-id content))
			   "")
		       (mapcar (lambda (l)
				 (format nil "if(p === undefined && t=='~A'){e.attr('data-clog-type','~A')}"
					 (getf l :tag) (getf l :control)))
			       *import-types*))))
      (clog::js-execute parent tmp))
    (let* ((data (first-child content))
	   (name    (attribute data "data-clog-title"))
	   (next-id (attribute data "data-clog-next-id"))
	   (slots   (attribute data "data-custom-slots"))
	   (package (attribute data "data-in-package")))
      (unless (equalp next-id "undefined")
	(setf-next-id content next-id))
      (unless (equalp package "undefined")
	(setf (attribute content "data-in-package") package))
      (unless (equalp slots "undefined")
	(setf (attribute content "data-custom-slots") slots))
      (unless (equalp name "undefined")
	(setf (attribute content "data-clog-name") name)
	(destroy data)))
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

;; Code rendering utlities

(defun render-clog-code (content win hide-loc)
  "Render panel to clog code and add tp CW window"
  (let* ((app      (connection-data-item content "builder-app-data"))
	 (panel-id (html-id content))
	 (package  (attribute content "data-in-package"))
	 (slots    (attribute content "data-custom-slots"))
	 (cname    (attribute content "data-clog-name"))
	 cmembers vars events)
    (unless (or (equal slots "")
		(equal slots "undefined"))
      (push slots cmembers))
    (maphash (lambda (html-id control)
	       (place-inside-bottom-of hide-loc
				       (get-placer control))
	       (let ((vname (attribute control "data-clog-name")))
		 (unless (and (>= (length vname) 5)
			      (equalp (subseq vname 0 5) "none-"))
		   (push (format nil
				 "    \(~A :reader ~A\)~%"
				 vname
				 vname)
			 cmembers)
		   (push (format nil
				 "    \(setf (slot-value panel '~A\) ~
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
					 "    \(set-~A \(~A panel\) \(lambda \(~A\) \(declare \(ignorable ~A\)\) ~A\)\)~%"
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
\(defun create-~A \(clog-obj &key \(hidden nil\) \(class nil\) \(html-id nil\) \(auto-place t\)\)
  \(let \(\(panel \(change-class \(clog:create-div clog-obj :content \"~A\"
         :hidden hidden :class class :html-id html-id :auto-place auto-place\) \'~A\)\)\)
~{~A~}~{~A~}    panel\)\)~%"
			  (string-upcase package)
			  cname     ;;defclass
			  cmembers
			  cname     ;;defun
			  (ppcre:regex-replace-all "\""
						    (js-query content
							      (format nil
								      "var z=~a.clone();~
    z.find('*').each(function(){for(n in $(this).get(0).dataset){delete $(this).get(0).dataset[n]}});~
    z.html()"
								      (clog::jquery content)))
						    "\\\"")
			  cname
			  vars
			  events)))
      (maphash (lambda (html-id control)
		 (declare (ignore html-id))
		 (place-after control (get-placer control)))
	       (get-control-list app panel-id))
      result)))

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
	    (push `(,(getf event :name)
		    ,(let ((txt (attribute control attr)))
		       (if (equalp txt "undefined")
			   ""
			   txt))
		    ,(getf event :parameters)
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
	    (setf (width td1) "30%")
	    (setf (width td2) "70%")
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
  (let ((app (connection-data-item obj "builder-app-data")))
    (bordeaux-threads:with-lock-held ((properties-lock app))
      (on-populate-control-events-win obj)
      (let* ((prop-win (control-properties-win app))
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
		(setf (width td1) "30%")
		(setf (width td2) "70%")
		(setf (spellcheckp td2) nil)
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
						      :height (client-height control)))))))))))))

(defun on-populate-loaded-window (content &key win)
  "Setup html imported in to CONTENT for use with Builder"
  (add-sub-controls content content :win win))

(defun on-populate-control-list-win (content)
  "Populate the control-list-window to allow drag and drop adjust of order
of controls and double click to select control."
  (let ((app (connection-data-item content "builder-app-data")))
    (bordeaux-threads:with-lock-held ((control-list-win-lock app))
      (let ((panel-id (html-id content))
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
							(select-control control)
							(on-populate-control-list-win content))))
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
	      (add-siblings (first-child content) ""))))))))

;; Menu handlers

(defun do-eval (obj form-string cname &key (package "clog-user") custom-boot)
  "Render, evalute and run code for panel"
  (let* ((result (capture-eval (format nil "~A~% (clog:set-on-new-window~
                                               (lambda (body)~
                                                 ~A
                                                 (create-~A body)) ~A:path \"/test\")~
                                                 (clog:open-browser :url \"http://127.0.0.1:8080/test\")"
				       form-string
				       (if custom-boot
					   ""
					   "(clog-gui:clog-gui-initialize body)
                                            (clog-web:clog-web-initialize body :w3-css-url nil)")
				       cname
				       (if custom-boot
					   (format nil ":boot-file \"~A\" " custom-boot)
					   ""))
			       :eval-in-package package)))
    (alert-dialog obj result :title "Eval Result")))

(defun on-show-control-properties-win (obj)
  "Show control properties window"
  (let ((app (connection-data-item obj "builder-app-data")))
    (if (control-properties-win app)
	(window-focus (control-properties-win app))
	(let* ((win          (create-gui-window obj :title "Control Properties"
						    :left 630
						    :top 40
						    :height 510 :width 400
						    :has-pinner t :client-movement t))
	       (content      (window-content win))
	       (control-list (create-table content)))
	  (setf (control-properties-win app) win)
	  (setf (properties-list app) control-list)
	  (set-on-window-close win (lambda (obj) (setf (control-properties-win app) nil)))
	  (setf (positioning control-list) :absolute)
	  (set-geometry control-list :units "" :left 0 :top 0 :bottom 0 :width "100%")))))

(defun on-show-control-events-win (obj)
  "Show control events window"
  (let ((app (connection-data-item obj "builder-app-data")))
    (if (control-events-win app)
	(window-focus (control-events-win app))
	(let* ((win          (create-gui-window obj :title "Control Events"
						    :left 220
						    :top 350
						    :height 200 :width 400
						    :has-pinner t :client-movement t))
	       (content      (window-content win))
	       (control-list (create-table content)))
	  (setf (control-events-win app) win)
	  (setf (events-list app) control-list)
	  (set-on-window-close win (lambda (obj) (setf (control-events-win app) nil)))
	  (setf (positioning control-list) :absolute)
	  (set-geometry control-list :units "" :left 0 :top 0 :bottom 0 :width "100%")))))

(defun on-show-control-pallete-win (obj)
  "Show control pallete"
  (let ((app (connection-data-item obj "builder-app-data")))
    (if (control-pallete-win app)
	(window-focus (control-pallete-win app))
	(let* ((win          (create-gui-window obj :title "Control Pallete"
						    :top 40
						    :left 0
						    :height 300 :width 200
						    :has-pinner t :client-movement t))
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
	    (if (equal (getf control :name) "group")
		(add-select-optgroup control-list (getf control :description))
		(add-select-option control-list (getf control :name) (getf control :description))))))))

(defun on-show-control-list-win (obj)
  "Show control list for selecting and manipulating controls by name"
  (let ((app (connection-data-item obj "builder-app-data")))
    (if (control-list-win app)
	(window-focus (control-list-win app))
	(let* ((win (create-gui-window obj :title "Control List"
					   :top 350
					   :left 0
					   :width 200
					   :has-pinner t :client-movement t)))
	  (setf (control-list-win app) win)
	  (setf (advisory-title (window-content win))
		(format nil "Drag and drop order~%Double click non-focusable~%~
                             <ctrl> place static~%<shift> child to selected"))
	  (set-on-window-close win (lambda (obj) (setf (control-list-win app) nil)))))))

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
	 (btn-copy  (create-button tool-bar :content "Copy"))
	 (btn-paste (create-button tool-bar :content "Paste"))
	 (btn-sim   (create-button tool-bar :content "Sim"))
	 (btn-test  (create-button tool-bar :content "Run"))
	 (btn-rndr  (create-button tool-bar :content "Rndr"))
	 (btn-save  (create-button tool-bar :content "Save"))
	 (btn-load  (create-button tool-bar :content "Load"))
	 (content   (center-panel box))
	 (in-simulation nil)
	 (file-name        "")
	 (render-file-name "")
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
    (setf (attribute content "data-custom-slots") "")
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
    (set-on-window-size-done win
			     (lambda (obj)
			       (on-populate-control-properties-win content :win win)))
    ;; setup tool bar events
    (set-on-click btn-copy (lambda (obj)
			     (declare (ignore obj))
			     (when (current-control app)
			       (maphash
				(lambda (html-id control)
				  (declare (ignore html-id))
				  (place-inside-bottom-of (bottom-panel box)
							  (get-placer control)))
				(get-control-list app panel-id))
			       (setf (copy-buf app)
				     (js-query content
					       (format nil
						       "var z=~a.clone(); z=$('<div />').append(z);~
     z.find('*').each(function(){if($(this).attr('id') !== undefined && ~
     $(this).attr('id').substring(0,5)=='CLOGB'){$(this).removeAttr('id')}});~
     z.html()"
						       (clog::jquery (current-control app)))))
			       (maphash
				(lambda (html-id control)
				  (declare (ignore html-id))
				  (place-after control (get-placer control)))
				(get-control-list app panel-id)))))
  (set-on-click btn-paste (lambda (obj)
			    (declare (ignore obj))
			    (bordeaux-threads:with-lock-held ((new-control-lock app))
			      (when (copy-buf app)
				(let ((control (create-control content content
							       `(:name "custom"
								 :clog-type      clog:clog-element
								 :create         clog:create-child
								 :create-type    :paste)
							       (format nil "CLOGB~A" (get-universal-time))
							       :custom-query (copy-buf app))))
				  (setf (attribute control "data-clog-name")
					(format nil "~A-~A" "copy" (next-id content)))
				  (incf-next-id content)
				  (setup-control content control :win win)
				  (select-control control)
				  (add-sub-controls control content :win win :paste t)
				  (on-populate-control-list-win content))))))
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
    (set-on-click btn-test
		  (lambda (obj)
		      (do-eval obj (render-clog-code content win (bottom-panel box))
			(attribute content "data-clog-name")
			:package (attribute content "data-in-package"))))
    (set-on-click btn-rndr
		  (lambda (obj)
		    (server-file-dialog obj "Save As.." file-name
					(lambda (fname)
					  (window-focus win)
					  (when fname
					    (setf render-file-name fname)
					    (write-file (render-clog-code content win (bottom-panel box))
							fname)))
					:initial-filename render-file-name)))
    (set-on-mouse-down content
		       (lambda (obj data)
			 (declare (ignore obj))
			 (unless in-simulation
			   (when (drop-new-control app content data :win win)
			     (incf-next-id content)))))))

(defun on-attach-builder-custom (body)
  "New custom builder page has attached"
  (let* ((params (form-get-data body))
	 (curl   (form-data-item params "curl")))
    (on-attach-builder-page body :custom-boot curl)))

(defun on-attach-builder-page (body &key custom-boot)
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
	 (file-name        "")
	 (render-file-name "")
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
    (setf (attribute content "data-custom-slots") "")
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
    ;; setup jquery and jquery-ui
    (cond (custom-boot
	   (load-css (html-document body) "/css/jquery-ui.css")
	   (load-script (html-document body) "/js/jquery-ui.js"))
	  (t
	   (clog-gui-initialize body)
	   (clog-web-initialize body :w3-css-url nil)))
    ;; init builder
    (init-control-list app panel-id)
    (let* ((pbox      (create-panel-box-layout (window-content win)
					 :left-width 0 :right-width 0
					 :top-height 30 :bottom-height 0))
	   (tool-bar  (top-panel pbox))
	   (btn-del   (create-button tool-bar :content "Del"))
	   (btn-copy  (create-button tool-bar :content "Copy"))
	   (btn-paste (create-button tool-bar :content "Paste"))
	   (btn-sim   (create-button tool-bar :content "Sim"))
	   (btn-test  (create-button tool-bar :content "Run"))
	   (btn-rndr  (create-button tool-bar :content "Rndr"))
	   (btn-save  (create-button tool-bar :content "Save"))
	   (btn-load  (create-button tool-bar :content "Load"))
	   (btn-exp   (create-button tool-bar :content "Export"))
	   (wcontent  (center-panel pbox)))
      (create-div wcontent :content
		  "<br><center>Drop and work with controls on it's window.</center>")
      (setf (background-color tool-bar) :silver)
      ;; setup tool bar events
      (set-on-click btn-exp (lambda (obj)
			      (server-file-dialog obj "Export as Boot HTML" "./"
						  (lambda (filename)
						    (when filename
						      (maphash
						       (lambda (html-id control)
							 (declare (ignore html-id))
							 (place-inside-bottom-of (bottom-panel box)
										 (get-placer control)))
						       (get-control-list app panel-id))
						      (save-body-to-file filename :body body :if-exists :rename)
						      (maphash
						       (lambda (html-id control)
							 (declare (ignore html-id))
							 (place-after control (get-placer control)))
						       (get-control-list app panel-id)))))))
      (set-on-click btn-copy (lambda (obj)
			       (declare (ignore obj))
			       (when (current-control app)
				 (maphash
				  (lambda (html-id control)
				    (declare (ignore html-id))
				    (place-inside-bottom-of (bottom-panel box)
							    (get-placer control)))
				  (get-control-list app panel-id))
				 (setf (copy-buf app)
				       (js-query content
						 (format nil
							 "var z=~a.clone(); z=$('<div />').append(z);~
       z.find('*').each(function(){if($(this).attr('id') !== undefined && ~
       $(this).attr('id').substring(0,5)=='CLOGB'){$(this).removeAttr('id')}});~
       z.html()"
							 (clog::jquery (current-control app)))))
				 (maphash
				  (lambda (html-id control)
				    (declare (ignore html-id))
				    (place-after control (get-placer control)))
				  (get-control-list app panel-id)))))
      (set-on-click btn-paste (lambda (obj)
				(declare (ignore obj))
				(bordeaux-threads:with-lock-held ((new-control-lock app))
				  (when (copy-buf app)
				    (let ((control (create-control content content
								   `(:name "custom"
								     :clog-type      clog:clog-element
								     :create         clog:create-child
								     :create-type    :paste)
								   (format nil "CLOGB~A" (get-universal-time))
								   :custom-query (copy-buf app))))
				      (setf (attribute control "data-clog-name")
					    (format nil "~A-~A" "copy" (next-id content)))
				      (incf-next-id content)
				      (setup-control content control :win win)
				      (select-control control)
				      (add-sub-controls control content :win win :paste t)
				      (on-populate-control-list-win content))))))
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
			       (declare (ignore obj))
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
      (set-on-click btn-test
		    (lambda (obj)
		      (do-eval obj (render-clog-code content win (bottom-panel box))
			(attribute content "data-clog-name")
			:package (attribute content "data-in-package")
			:custom-boot custom-boot)))
      (set-on-click btn-rndr
		    (lambda (obj)
		      (server-file-dialog obj "Save As.." file-name
					  (lambda (fname)
					    (window-focus win)
					    (when fname
					      (setf render-file-name fname)
					      (write-file (render-clog-code content win (bottom-panel box))
							  fname)))
					  :initial-filename render-file-name))))
    (set-on-mouse-down content
		       (lambda (obj data)
			 (declare (ignore obj))
			 (unless in-simulation
			   (when (drop-new-control app content data :win win)
			     (incf-next-id content)))))))

(defun on-new-builder-basic-page (obj)
  "Menu item to open new basic HTML page"
  (set-on-new-window 'on-attach-builder-custom :boot-file "/boot.html" :path "/builder-custom")
  (on-new-builder-page obj :custom-boot "/boot.html" :url-launch nil))

(defun on-new-builder-bst-page (obj)
  "Menu item to open new boostrap 5 page"
  (set-on-new-window 'on-attach-builder-custom :boot-file "/bootstrap.html" :path "/builder-custom")
  (on-new-builder-page obj :custom-boot "/bootstrap.html" :url-launch nil))

(defun on-new-builder-launch-page (obj)
  "Menu item to open new page"
  (on-new-builder-page obj :url-launch t))

(defun on-new-builder-custom (obj)
  "Open custom boot page"
  (let ((custom-boot "/boot.html"))
    (input-dialog obj "Boot File Name:"
		  (lambda (answer)
		    (when answer
		      (setf custom-boot answer)
		      (set-on-new-window 'on-attach-builder-custom
					 :boot-file custom-boot :path "/builder-custom")
		      (on-new-builder-page obj :custom-boot custom-boot :url-launch t)))
		  :default-value custom-boot :modal t)))

(defun on-new-builder-page (obj &key custom-boot url-launch)
  "Open new page"
  (let* ((app (connection-data-item obj "builder-app-data"))
	 (win (create-gui-window obj :top 40 :left 220 :width 400 :client-movement t))
	 (panel-uid  (format nil "~A" (get-universal-time))) ;; unique id for panel
	 (boot-loc   (if custom-boot
			 "builder-custom"
			 "builder-page"))
	 (curl       (if custom-boot
			 (format nil "&curl=~A" (quri:url-encode custom-boot))
			 ""))
	 (link       (format nil "http://127.0.0.1:8080/~A?bid=~A~A" boot-loc panel-uid curl))
	 (btn-txt    (if url-launch
			 "Click to launch default browser or copy URL."
			 "Click if browser does not open new page shortly."))
	 (txt-area   (create-div (window-content win)))
	 (page-link  (create-a txt-area
			       :target "_blank"
			       :content (format nil "<br><center><button>
                                   ~A
                                   </button></center>" btn-txt)
			       :link link))
	 (txt-link   (create-div txt-area
				 :content (format nil "<br><center>~A</center>" link)))
	 content panel-id)
    (setf (gethash panel-uid *app-sync-hash*) app)
    (setf (gethash (format nil "~A-win" panel-uid) *app-sync-hash*) win)
    (setf (gethash (format nil "~A-link" panel-uid) *app-sync-hash*)
	  (lambda (obj)
	    (setf content obj)
	    (setf panel-id (html-id content))
	    (destroy txt-area)
	    (remhash (format nil "~A-link" panel-uid) *app-sync-hash*)))
    (unless url-launch
      (open-browser :url link))))

(defun on-help-about-builder (obj)
  "Open about box"
  (let ((about (create-gui-window obj
				  :title   "About"
				  :content "<div class='w3-black'>
                                         <center><img src='/img/clogwicon.png'></center>
	                                 <center>CLOG</center>
	                                 <center>The Common Lisp Omnificent GUI</center></div>
			                 <div><p><center>
                                           <a target=_blank href='https://github.com/sponsors/rabbibotton'>CLOG Builder</a>
                                           </center>
                                         <center>(c) 2022 - David Botton</center></p></div>"
				  :width   200
				  :height  215
				  :hidden  t)))
    (window-center about)
    (setf (visiblep about) t)
    (set-on-window-can-size about (lambda (obj)
				    (declare (ignore obj))()))))

(defun on-new-app-template (obj)
  "Menu option to create new project from template"
  (let* ((win (create-gui-window obj :title "New Application Template"
				     :width 500 :height 400))
	 (ct  (create-clog-templates (window-content win))))
    (window-center win)
    (setf (win ct) win)
    (dolist (tmpl *supported-templates*)
      (add-select-option (template-box ct) (getf tmpl :code) (getf tmpl :name)))))

(defun fill-button-clicked (panel)
  "Template fill botton clicked"
  (let* ((tmpl-rec  (find-if (lambda (x)
			       (equal (getf x :code)
				      (value (template-box panel))))
			     *supported-templates*))
	 (start-dir (format nil "~A~A"
			    (asdf:system-source-directory :clog)
			    (getf tmpl-rec :loc)))
	 (www-dir   (format nil "~A~A"
			    (asdf:system-source-directory :clog)
			    (getf tmpl-rec :www))))
    (setf (hiddenp panel) t)
    (input-dialog
     (win panel) "Enter new system name:"
     (lambda (sys-name)
       (cond (sys-name
	      (server-file-dialog
	       (win panel) "Output Directory" "~/common-lisp/"
	       (lambda (filename)
		 (cond (filename
			(template-copy sys-name start-dir filename :panel (window-content (win panel)))
			(when (getf tmpl-rec :www)
			  (template-copy sys-name www-dir filename :panel (window-content (win panel))))
			(asdf:clear-source-registry)
			(create-div (window-content (win panel)) :content "<hr><b>done.</b>"))
		       (t
			(window-close (win panel)))))))
	     (t
	      (window-close (win panel))))))))

(defun on-new-builder (body)
  "Launch instance of the CLOG Builder"
  (set-html-on-close body "Connection Lost")
  (let ((app (make-instance 'builder-app-data)))
    (setf (connection-data-item body "builder-app-data") app)
    (setf (title (html-document body)) "CLOG Builder")
    (clog-gui-initialize body)
    (add-class body "w3-blue-grey")
    (setf (z-index (create-panel body :positioning :fixed
				      :bottom 0 :right 0
				      :content (format nil "static-root: ~A" clog::*static-root*)))
	  -9999)
    (let* ((menu  (create-gui-menu-bar body))
	   (icon  (create-gui-menu-icon menu :on-click #'on-help-about-builder))
	   (file  (create-gui-menu-drop-down menu :content "Builder"))
	   (tools (create-gui-menu-drop-down menu :content "Tools"))
	   (win   (create-gui-menu-drop-down menu :content "Window"))
	   (help  (create-gui-menu-drop-down menu :content "Help")))
      (declare (ignore icon))
      (create-gui-menu-item file  :content "New CLOG-GUI Panel"        :on-click 'on-new-builder-panel)
      (create-gui-menu-item file  :content "New CLOG-WEB Page"         :on-click 'on-new-builder-page)
      (create-gui-menu-item file  :content "New Basic HTML Page"       :on-click 'on-new-builder-basic-page)
      (create-gui-menu-item file  :content "New Bootstrap Page"        :on-click 'on-new-builder-bst-page)
      (create-gui-menu-item file  :content "New CLOG-WEB Delay Launch" :on-click 'on-new-builder-launch-page)
      (create-gui-menu-item file  :content "New Custom Boot Page"      :on-click 'on-new-builder-custom)
      (create-gui-menu-item file  :content "New Application Template"  :on-click 'on-new-app-template)
      (create-gui-menu-item tools :content "Control Pallete"    :on-click 'on-show-control-pallete-win)
      (create-gui-menu-item tools :content "Control Properties" :on-click 'on-show-control-properties-win)
      (create-gui-menu-item tools :content "Control Events"     :on-click 'on-show-control-events-win)
      (create-gui-menu-item tools :content "Control List"       :on-click 'on-show-control-list-win)
      (create-gui-menu-item win   :content "Maximize All"       :on-click #'maximize-all-windows)
      (create-gui-menu-item win   :content "Normalize All"      :on-click #'normalize-all-windows)
      (create-gui-menu-window-select win)
      (create-gui-menu-item help  :content "CLOG Manual"          :on-click
			    (lambda (obj)
			      (declare (ignore obj))
			      (open-window (window body) "https://rabbibotton.github.io/clog/clog-manual.html")))
      (create-gui-menu-item help  :content "Lisp in Y Minutes"    :on-click
			    (lambda (obj)
			      (declare (ignore obj))
			      (open-window (window body) "https://learnxinyminutes.com/docs/common-lisp/")))
      (create-gui-menu-item help  :content "Simplified Reference" :on-click
			    (lambda (obj)
			      (declare (ignore obj))
			      (open-window (window body) "https://jtra.cz/stuff/lisp/sclr/index.html")))
      (create-gui-menu-item help  :content "Common Lisp Manual"   :on-click
			    (lambda (obj)
			      (declare (ignore obj))
			      (open-window (window body) "http://clhs.lisp.se/")))
      (create-gui-menu-item help  :content "W3.CSS Manual"        :on-click
			    (lambda (obj)
			      (declare (ignore obj))
			      (open-window (window body) "https://www.w3schools.com/w3css/")))
      (create-gui-menu-item help  :content "Bootstrap 5.1 Manual" :on-click
			    (lambda (obj)
			      (declare (ignore obj))
			      (open-window (window body) "https://getbootstrap.com/docs/5.1/getting-started/introduction/")))
      (create-gui-menu-item help  :content "About CLOG Builder"   :on-click #'on-help-about-builder)
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

(defun clog-builder (&key static-root)
  "Start clog-builder."
  (if static-root
      (initialize nil :static-root static-root)
      (initialize nil))
  (set-on-new-window 'on-new-builder :path "/builder")
  (set-on-new-window 'on-attach-builder-page :path "/builder-page")
  (open-browser :url "http://127.0.0.1:8080/builder"))
