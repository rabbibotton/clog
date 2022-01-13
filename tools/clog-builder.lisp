(in-package :clog-tools)
(export 'clog-builder)

(defparameter supported-controls
  (list
   '(:name          "select"
     :description   "Selection Tool"
     :create         nil
     :create-type    nil
     :properties     nil
     :events         nil)
   '(:name           "label"
     :description    "Text Label"
     :create         clog:create-label
     :create-type    :label
     :create-content "label"
     :properties     ((:name "color"
		       :prop  clog:color)
		      (:name "background-color"
		       :prop  clog:background-color)))
   '(:name            "button"
     :description     "Button"
     :create          clog:create-form-element
     :create-type     :form
     :create-param    :button
     :create-value    "button"
     :properties      ((:name "color"
			 :prop  clog:color)
			(:name "background-color"
			 :prop  clog:background-color)))
   '(:name            "input"
     :description     "Text Input"
     :create          clog:create-form-element
     :create-type     :form
     :create-param    :input
     :create-value    ""
     :properties      ((:name "color"
			:prop  clog:color)
		       (:name "background-color"
			:prop  clog:background-color)))))

(defun control-info (control-name)
  "Return control informaton record for CONTROL-NAME"
  (find-if (lambda (x) (equal (getf x :name) control-name)) supported-controls))

(defclass builder-app-data ()
  ((copy-buf
    :accessor copy-buf
    :initform ""
    :documentation "Copy buffer")
   (next-panel-id
    :accessor next-pannel-id
    :initform 0
    :documentation "Next new pannel id")
   (current-control
    :accessor current-control
    :initform nil
    :documentation "Current selected control")
   (selected-tool
    :accessor selected-tool
    :initform nil
    :documentation "Currently selected tool")
   (properties-list
    :accessor properties-list
    :initform nil
    :documentation "Property list in properties window")
   (control-properties-win
    :accessor control-properties-win
    :initform nil
    :documentation "Current control properties window")
   (control-list-win
    :accessor control-list-win
    :initform nil
    :documentation "Current control list window")
   (control-pallete-win
    :accessor control-pallete-win
    :initform nil
    :documentation "Current control pallete window")))

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

(defun capture-eval (form)
  "Capture lisp evaluaton of FORM"
  (let ((result (make-array '(0) :element-type 'base-char
				 :fill-pointer 0 :adjustable t))
	(eval-result))
    (with-output-to-string (stream result)
      (let ((*standard-output* stream)
	    (*error-output* stream))
	(setf eval-result (eval (read-from-string (format nil "(progn ~A)" form))))))
    (format nil "~A~%=>~A~%" result eval-result)))

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

(defun do-eval (obj)
  "Do lisp eval of editor contents"
  (let ((cw (current-window obj)))
    (when cw
      (let* ((form-string (js-query obj (format nil "editor_~A.getValue()"
						(html-id (current-window obj)))))
	     (result      (capture-eval form-string)))
	(alert-dialog obj result :title "Eval Result")))))

(defun on-show-layout-code (obj)
  "Show a lisp editor"
  (let* ((win         (create-gui-window obj :title  "Layout Code"
					     :height 400
					     :width  650))
	 (box         (create-panel-box-layout (window-content win)
					       :left-width 0 :right-width 9
					       :top-height 30 :bottom-height 0))
	 (file-name   ".")
	 (center      (center-panel box))
	 (center-id   (html-id center))
	 (tool-bar    (top-panel box))
	 (btn-save    (create-button tool-bar :content "Save"))
	 (btn-eval    (create-button tool-bar :content "Run")))
    (setf (background-color tool-bar) :silver)
    (set-on-click btn-eval (lambda (obj)
			     (do-eval obj)))
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

(defun on-populate-control-properties-win (obj)
  "Populate the control properties win for the current control"
  (let* ((app     (connection-data-item obj "builder-app-data"))
	 (win     (control-properties-win app))
	 (control (current-control app))
	 (placer  (get-placer control))
	 (table   (properties-list app)))
    (when win
      (setf (inner-html table) ""))
    (when (and win control)
      (let ((info  (control-info (attribute control "data-clog-type")))
	    (props `(("id"      ,(html-id control) nil)
		     ("name"    ,(attribute control "data-clog-name") t
				,(lambda (obj)
				   (setf  (attribute control "data-clog-name") (text obj))))
		     ("top"     ,(top control) t ,(lambda (obj)
						  (setf (top control) (text obj))))
		     ("left"    ,(left control) t ,(lambda (obj)
						   (setf (left control) (text obj))))
		     ("width"   ,(width control) t ,(lambda (obj)
						     (setf (width control) (text obj))))
		     ("height"  ,(height control) t ,(lambda (obj)
						       (setf (height control) (text obj))))
		     ,(if (typep control 'clog:clog-form-element)
			  `("value"  ,(value control) t ,(lambda (obj)
							   (setf (value control) (text obj))))
			  `("text"   ,(text control) t ,(lambda (obj)
							  (setf (text control) (text obj))))))))
	(when info
	  (let (col)
	    (dolist (prop (reverse (getf info :properties)))
	      (push `(,(getf prop :name) ,(funcall (getf prop :prop) control) t
		      ,(lambda (obj)
			 (funcall (find-symbol (format nil "SET-~A" (getf prop :prop)) :clog) control (text obj))))
		    col))
	    (alexandria:appendf props col)))
	(dolist (item props)
	  (let* ((tr (create-table-row table))
		 (td1 (create-table-column tr :content (first item)))
		 (td2 (create-table-column tr :content (second item))))
	    (set-border td1 "1px" :dotted :black)
	    (when (third item)
	      (setf (editablep td2) t)
	      (set-on-blur td2
			   (lambda (obj)
			     (funcall (fourth item) obj)
			     (when control
			       (set-geometry placer :units ""
						    :top (top control)
						    :left (left control)
						    :width (client-width control)
						    :height (client-height control))))))))))))

(defun on-show-control-properties-win (obj)
  (let ((app (connection-data-item obj "builder-app-data")))
    (if (control-properties-win app)
	(window-focus (control-properties-win app))
	(let* ((win          (create-gui-window obj :title "Control Properties"
						    :left 220
						    :top 250
						    :height 300 :width 400
						    :has-pinner t))
	       (content      (window-content win))
	       (control-list (create-table content)))
	  (setf (control-properties-win app) win)
	  (setf (properties-list app) control-list)
	  (set-on-window-close win (lambda (obj) (setf (control-properties-win app) nil)))
	  (setf (positioning control-list) :absolute)
	  (set-geometry control-list :left 0 :top 0 :bottom 0 :right 0)))))

(defun on-show-control-pallete-win (obj)
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
	  (set-geometry control-list :left 0 :top 0 :bottom 0 :width 190)
	  (set-on-change control-list (lambda (obj)
					(setf (selected-tool app) (control-info (value control-list)))))
	  (set-on-focus control-list (lambda (obj)
				       (setf (selected-tool app) (control-info (value control-list)))))
	  (dolist (control supported-controls)
	    (add-select-option control-list (getf control :name) (getf control :description)))))))

(defun on-show-control-list-win (obj)
  (let ((app (connection-data-item obj "builder-app-data")))
    (if (control-list-win app)
	(window-focus (control-list-win app))
	(let* ((win (create-gui-window obj :title "Control List"
					   :top 350
					   :left 0
					   :width 200 :has-pinner t)))
	  (setf (control-list-win app) win)
	  (set-on-window-close win (lambda (obj) (setf (control-list-win app) nil)))))))

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

(defun select-control (control)
  "Select CONTROL as the current control and highlight its placer."
  (let ((app    (connection-data-item control "builder-app-data"))
	(placer (get-placer control)))
    (deselect-current-control app)
    (setf (current-control app) control)
    (set-border placer (unit "px" 2) :solid :blue)
    (on-populate-control-properties-win control)))

(defun on-populate-control-list-win (content)
  "Populate the control-list-window to allow drag and drop adjust of order
of controls and double click to select control."
  (let* ((app (connection-data-item content "builder-app-data")))
    (when (control-list-win app)
      (let* ((win     (window-content (control-list-win app)))
	     (control (first-child content))
	     dln)
	(setf (inner-html win) "")
	;; iterate through controls in content
	(loop
	  (when (equal (html-id control) "undefined") (return))
	  (setf dln (attribute control "data-clog-name"))
	  (unless (equal dln "undefined")
	    (let ((list-item (create-div win :content (format nil "&#8597; ~A" dln))))
	      (setf (background-color list-item) :lightgray)
	      (setf (draggablep list-item) t)
	      (setf (attribute list-item "data-clog-control") (html-id control))
	      ;; double click to select item
	      (set-on-double-click list-item (lambda (obj)
				       (let* ((id      (attribute obj "data-clog-control"))
					      (element (attach-as-child obj id)))
					 (select-control element))))
	      ;; drag and drop to change
	      (set-on-drag-over list-item (lambda (obj)(declare (ignore obj))()))
	      (set-on-drop list-item (lambda (obj data)
			       (declare (ignore obj))
			       (let* ((id (attribute obj "data-clog-control"))
				      (c1 (attach-as-child obj id))
				      (c2 (attach-as-child obj (getf data :drag-data)))
				      (placer (get-placer c2)))
				 (place-before c1 c2)
				 (place-after c2 placer)
				 (on-populate-control-list-win content))))
	      (set-on-drag-start list-item (lambda (obj)
				     (declare (ignore obj))())
				 :drag-data (html-id control))))
	  (setf control (next-sibling control)))))))

;; These templates are here due to compiler or slime bug,
;; I don't have time to hunt down at moment.
(defparameter *builder-template1* "\(in-package :clog-user)~%~
\(set-on-new-window \(lambda \(body)~%
                      \(let* \(\(~A \"~A\")~%
                            \(panel (create-div body :content ~A))~{~A~})~%
                       ))~%~
   :path \"/form_~A\")~%~
\(open-browser :url \"http://127.0.0.1:8080/form_~A\")~%")

(defparameter *builder-template2*
  "~%                            (~A (attach-as-child body \"~A\" :clog-type '~A))")

(defun on-new-builder-window (obj)
  "Open new panel"
  (let* ((app (connection-data-item obj "builder-app-data"))
	 (win (create-gui-window obj :top 40 :left 220 :width 400))
	 (box (create-panel-box-layout (window-content win)
				       :left-width 0 :right-width 9
				       :top-height 30 :bottom-height 0))
	 (tool-bar (top-panel box))
	 (btn-del  (create-button tool-bar :content "Delete"))
	 (btn-sim  (create-button tool-bar :content "Simulate"))
	 (btn-rndr (create-button tool-bar :content "Render"))
	 (btn-prop (create-button tool-bar :content "Properties"))
	 (btn-save (create-button tool-bar :content "Save"))
	 (content  (center-panel box))
	 (in-simulation nil)
	 (file-name  ".")
	 (panel-name (format nil "panel-~A" (incf (next-pannel-id app))))
	 (next-id  0)
	 control-list)
    ;; setup panel window
    (setf (background-color tool-bar) :silver)
    (setf (attribute content "data-clog-name") panel-name)
    (setf (window-title win) panel-name)
    ;; activate associated windows on open
    (on-populate-control-list-win content)
    ;; setup window events
    (set-on-window-focus win (lambda (obj) (declare (ignore obj)) (on-populate-control-list-win content)))
    (set-on-window-close win
			 (lambda (obj)
			   (declare (ignore obj))
			   ;; clear associated windows on close
			   (setf (current-control app) nil)
			   (on-populate-control-properties-win win)
			   (on-populate-control-list-win content)))
    ;; setup tool bar events
    (set-on-click btn-del (lambda (obj)
			    (declare (ignore obj))
			    (when (current-control app)
			      (alexandria:removef control-list (current-control app))
			      (destroy (get-placer (current-control app)))
			      (destroy (current-control app))
			      (setf (current-control app) nil)
			      (on-populate-control-properties-win win)
			      (on-populate-control-list-win content))))
    (set-on-click btn-sim (lambda (obj)
			    (declare (ignore obj))
			    (cond (in-simulation
				   (setf (text btn-sim) "Simulate")
				   (setf in-simulation nil)
				   (dolist (control control-list)
				     (setf (hiddenp (get-placer control)) nil)))
				  (t
				   (setf (text btn-sim) "Develop")
				   (deselect-current-control app)
				   (on-populate-control-properties-win win)
				   (setf in-simulation t)
				   (dolist (control control-list)
				     (setf (hiddenp (get-placer control)) t))
				   (focus (first-child content))))))
    (set-on-click btn-save (lambda (obj)
			     (server-file-dialog obj "Save Panel As.." file-name
						 (lambda (fname)
						   (window-focus win)
						   (when fname
						     (setf file-name fname)
						     (dolist (control control-list)
						       (place-inside-bottom-of (bottom-panel box) (get-placer control)))
						     (write-file (inner-html content) fname))
						   (dolist (control control-list)
						     (place-after control (get-placer control))))
						 :initial-filename file-name)))
    (set-on-click btn-rndr (lambda (obj)
			     (dolist (control control-list)
			       (place-inside-bottom-of (bottom-panel box) (get-placer control)))
			     (let* ((cw     (on-show-layout-code obj))
				    (result (format nil
						    *builder-template1*
						    panel-name
						    (escape-string
						     (ppcre:regex-replace-all "\\x22"
									      (inner-html content)
									      "\\\\\\\""))
						    panel-name
						    (mapcar (lambda (e)
							      (let ((vname (attribute e "data-clog-name")))
								(when vname
								  (format nil *builder-template2*
									  vname
									  (html-id e)
									  (format nil "CLOG:~A" (type-of e))))))
							    control-list)
						    (html-id cw)
						    (html-id cw))))
			       (js-execute obj (format nil
						       "editor_~A.setValue('~A');editor_~A.moveCursorTo(0,0);"
						       (html-id cw)
						       (escape-string result)
						       (html-id cw))))
			     (dolist (control control-list)
			       (place-after control (get-placer control)))))
    (set-on-click btn-prop
		  (lambda (obj)
		    (input-dialog obj "Panel Name"
				  (lambda (result)
				    (when result
				      (setf panel-name result)
				      (setf (attribute content "data-clog-name") panel-name)
				      (setf (window-title win) panel-name)))
				  :default-value panel-name
				  :title "Panel Properties")))
    ;; setup adding and manipulating controls
    (set-on-mouse-down content
		       (lambda (obj data)
			 (unless in-simulation
			   (let* ((control-record (selected-tool app))
				  (create-type    (getf control-record :create-type))
				  (control        (cond ((eq create-type :label)
							 (funcall (getf control-record :create) content
								  :content (getf control-record :create-content)))
							((eq create-type :form)
							 (funcall (getf control-record :create) content
								  (getf control-record :create-param)
								  :value (getf control-record :create-value)))
							(t nil)))
				  (placer         (when control
						    (create-div obj :html-id (format nil "p-~A" (html-id control))))))
			     (window-focus win)
			     (unless control
			       (deselect-current-control app)
			       (on-populate-control-list-win content))
			     (when control
			       (push control control-list)
			       (setf (attribute control "data-clog-name")
				     (format nil "~A-~A" (getf control-record :name) (incf next-id)))
			       (setf (attribute control "data-clog-type") (getf control-record :name))
			       (setf (box-sizing control) :content-box)
			       (setf (box-sizing placer) :content-box)
			       (set-on-mouse-down placer (lambda (obj data)
							   (declare (ignore obj) (ignore data))
							   (select-control control)
							   (window-focus win))
						  :cancel-event t)
			       (setf (selected-tool app) nil)
			       (setf (positioning control) :absolute)
			       (set-geometry control
					     :left (getf data :x)
					     :top (getf data :y))
			       (setf (positioning placer) :absolute)
			       (clog::jquery-execute placer "draggable().resizable()")
			       (clog::set-on-event placer "resizestop"
						   (lambda (obj)
						     (declare (ignore obj))
						     (set-geometry control :units ""
									   :width (width placer)
									   :height (height placer))
						     (set-geometry placer :units ""
									  :width (client-width control)
									  :height (client-height control))
						     (on-populate-control-properties-win win)))
			       (clog::set-on-event placer "dragstop"
						   (lambda (obj)
						     (declare (ignore obj))
						     (set-geometry control :units ""
									   :top (top placer)
									   :left (left placer))
						     (on-populate-control-properties-win win)))
			       (set-geometry placer
					     :left (getf data :x)
					     :top (getf data :y))
			       (set-geometry placer :units ""
						    :width (client-width control)
						    :height (client-height control))
			       (select-control control)
			       (on-populate-control-list-win content))))))))

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
      (create-gui-menu-item file  :content "New Panel"          :on-click 'on-new-builder-window)
      (create-gui-menu-item tools :content "Control Pallete"    :on-click 'on-show-control-pallete-win)
      (create-gui-menu-item tools :content "Control Properties" :on-click 'on-show-control-properties-win)
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
    (on-show-control-properties-win body)
    (on-new-builder-window body)
    (set-on-before-unload (window body) (lambda(obj)
					  (declare (ignore obj))
					  ;; return empty string to prevent nav off page
					  ""))
    (run body)))

(defun clog-builder ()
  "Start clog-builder."
  (initialize nil)
  (set-on-new-window 'on-new-builder :path "/builder")
  (open-browser :url "http://127.0.0.1:8080/builder"))
