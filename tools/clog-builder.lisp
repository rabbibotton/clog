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
  (find-if (lambda (x) (equal (getf x :name) control-name)) supported-controls))

(defclass builder-app-data ()
  ((copy-buf
    :accessor copy-buf
    :initform ""
    :documentation "Copy buffer")
   (current-placer
    :accessor current-placer
    :initform nil
    :documentation "Current selected placer")
   (current-control
    :accessor current-control
    :initform nil
    :documentation "Current selected control")
   (control-properties
    :accessor control-properties
    :initform nil
    :documentation "Current control properties window")
   (control-list-win
    :accessor control-list-win
    :initform nil
    :documentation "Current control list window")
   (properties-list
    :accessor properties-list
    :initform nil
    :documentation "Property list")
   (control-pallete
    :accessor control-pallete
    :initform nil
    :documentation "Current control pallete window")
   (selected-tool
    :accessor selected-tool
    :initform nil
    :documentation "Currently selected tool")))

(defun read-file (infile)
  (with-open-file (instream infile :direction :input :if-does-not-exist nil)
    (when instream
      (let ((string (make-string (file-length instream))))
        (read-sequence string instream)
        string))))

(defun write-file (string outfile &key (action-if-exists :rename))
   (check-type action-if-exists (member nil :error :new-version :rename :rename-and-delete
                                        :overwrite :append :supersede))
   (with-open-file (outstream outfile :direction :output :if-exists action-if-exists)
     (write-sequence string outstream)))

(defun capture-eval (form)
  (let ((result (make-array '(0) :element-type 'base-char
				 :fill-pointer 0 :adjustable t))
	(eval-result))
    (with-output-to-string (stream result)
      (let ((*standard-output* stream)
	    (*error-output* stream))
	(setf eval-result (eval (read-from-string (format nil "(progn ~A)" form))))))
    (format nil "~A~%=>~A~%" result eval-result)))

(defun do-ide-edit-copy (obj)
  (let ((cw (current-window obj)))
    (when cw
      (let ((app (connection-data-item obj "builder-app-data")))
	(setf (copy-buf app) (js-query obj
		    (format nil "editor_~A.execCommand('copy');~
                                 navigator.clipboard.writeText(editor_~A.getCopyText());~
                                 editor_~A.getCopyText();"
			    (html-id cw) (html-id cw) (html-id cw))))))))

(defun do-ide-edit-undo (obj)
  (let ((cw (current-window obj)))
    (when cw
      (do-ide-edit-copy obj)
      (js-execute obj (format nil "editor_~A.execCommand('undo')"
			      (html-id cw))))))

(defun do-ide-edit-redo (obj)
  (let ((cw (current-window obj)))
    (when cw
      (js-execute obj (format nil "editor_~A.execCommand('redo')"
			      (html-id cw))))))

(defun do-ide-edit-cut (obj)
  (let ((cw (current-window obj)))
    (when cw
      (do-ide-edit-copy obj)
      (js-execute obj (format nil "editor_~A.execCommand('cut')"
			      (html-id cw))))))

(defun do-ide-edit-paste (obj)
  (let ((cw (current-window obj)))
    (when cw
      ;; Note this methods uses the global clip buffer and not (copy-buf app)
      ;; on copy and paste we set both the global and local buffer.
      (js-execute obj (format nil "navigator.clipboard.readText().then(function(text) {~
                                        editor_~A.execCommand('paste', text)~
                                     })"
			      (html-id cw))))))

(defun do-eval (obj)
  (let ((cw (current-window obj)))
    (when cw
      (let* ((form-string (js-query obj (format nil "editor_~A.getValue()"
						(html-id (current-window obj)))))
	     (result      (capture-eval form-string)))
	(alert-dialog obj result :title "Eval Result")))))

(defun on-show-layout-code (obj)
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

(defun on-populate-control-properties (obj)
  (let* ((app     (connection-data-item obj "builder-app-data"))
	 (win     (control-properties app))
	 (control (current-control app))
	 (placer  (current-placer app))
	 (table   (properties-list app)))
    (when win
      (setf (inner-html table) ""))
    (when (and win control)
      (let ((info  (control-info (attribute control "data-clog-type")))
	    (props `(("id"      ,(html-id control) nil)
		     ("name"    ,(attribute control "data-lisp-name") t
				,(lambda (obj)
				   (setf  (attribute control "data-lisp-name") (text obj))))
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

(defun on-show-control-properties (obj)
  (let ((app (connection-data-item obj "builder-app-data")))
    (if (control-properties app)
	(window-focus (control-properties app))
	(let* ((win          (create-gui-window obj :title "Control Properties"
						    :left 220
						    :top 250
						    :height 300 :width 400
						    :has-pinner t))
	       (content      (window-content win))
	       (control-list (create-table content)))
	  (setf (control-properties app) win)
	  (setf (properties-list app) control-list)
	  (set-on-window-close win (lambda (obj) (setf (control-properties app) nil)))
	  (setf (positioning control-list) :absolute)
	  (set-geometry control-list :left 0 :top 0 :bottom 0 :right 0)))))

(defun on-show-control-pallete (obj)
  (let ((app (connection-data-item obj "builder-app-data")))
    (if (control-pallete app)
	(window-focus (control-pallete app))
	(let* ((win          (create-gui-window obj :title "Control Pallete"
						    :top 40
						    :left 0
						    :height 300 :width 200 :has-pinner t))
	       (content      (window-content win))
	       (control-list (create-select content)))
	  (setf (control-pallete app) win)
	  (set-on-window-close win (lambda (obj) (setf (control-pallete app) nil)))
	  (setf (positioning control-list) :absolute)
	  (setf (size control-list) 2)
	  (set-geometry control-list :left 0 :top 0 :bottom 0 :width 190)
	  (set-on-change control-list (lambda (obj)
					(setf (selected-tool app) (control-info (value control-list)))))
	  (set-on-focus control-list (lambda (obj)
				       (setf (selected-tool app) (control-info (value control-list)))))
	  (dolist (control supported-controls)
	    (add-select-option control-list (getf control :name) (getf control :description)))))))

(defun on-show-control-list (obj)
  (let ((app (connection-data-item obj "builder-app-data")))
    (if (control-list-win app)
	(window-focus (control-list-win app))
	(let* ((win (create-gui-window obj :title "Control List"
					   :top 350
					   :left 0
					   :width 200)))
	  (setf (control-list-win app) win)
	  (set-on-window-close win (lambda (obj) (setf (control-list-win app) nil)))))))

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
	 (panel-name (format nil "panel-~A" (clog-connection:generate-id)))
	 (file-name  ".")
	 control-list
	 placer-list)
    (labels ((populate-control-list-win ()
	       (when (control-list-win app)
		 (let* ((c (control-list-win app))
			(w (window-content c))
			(p (first-child content))
			dln)
		   (setf (inner-html w) "")
		   (loop
		     (when (equal (html-id p) "undefined") (return))
		     (setf dln (attribute p "data-lisp-name"))
		     (unless (equal dln "undefined")
		       (let ((n (create-div w :content (format nil "&#8597; ~A" dln))))
			 (setf (background-color n) :lightgray)
			 (setf (draggablep n) t)
			 (setf (attribute n "data-clog-control") (html-id p))
			 (set-on-drag-over n (lambda (obj)(declare (ignore obj))()))
			 (set-on-drop n (lambda (obj data)
					  (declare (ignore obj))
					  (let ((id (attribute n "data-clog-control")))
					    (place-before
					     (attach-as-child n id)
					     (attach-as-child n
							      (getf data :drag-data)))
					    (populate-control-list-win))))
			 (set-on-drag-start n (lambda (obj)
						(declare (ignore obj))())
					    :drag-data (html-id p))))
		     (setf p (next-sibling p)))))))
      (setf (background-color tool-bar) :silver)
      (setf (attribute content "data-lisp-name") panel-name)
      (setf (window-title win) panel-name)
      (set-on-click btn-del (lambda (obj)
			      (declare (ignore obj))
			      (when (current-control app)
				(alexandria:removef control-list (current-control app))
				(alexandria:removef placer-list (current-placer app))
				(destroy (current-placer app))
				(destroy (current-control app))
				(setf (current-control app) nil)
				(setf (current-placer app) nil)
				(on-populate-control-properties win)
				(populate-control-list-win))))
      (set-on-click btn-sim (lambda (obj)
			      (declare (ignore obj))
			      (cond (in-simulation
				     (setf (text btn-sim) "Simulate")
				     (setf in-simulation nil)
				     (dolist (placer placer-list)
				       (setf (hiddenp placer) nil)))
				    (t
				     (setf (text btn-sim) "Develop")
				     (when (current-control app)
				       (set-border (current-placer app) (unit "px" 0) :none :blue)
				       (setf (current-control app) nil)
				       (setf (current-placer app) nil)
				       (on-populate-control-properties win))
				     (setf in-simulation t)
				     (dolist (placer placer-list)
				       (setf (hiddenp placer) t))
				     (focus (first-child content))))))
      (set-on-click btn-save (lambda (obj)
			       (server-file-dialog obj "Save Panel As.." file-name
						   (lambda (fname)
						     (window-focus win)
						     (when fname
						       (setf file-name fname)
						       (dolist (placer placer-list)
							 (place-inside-bottom-of (bottom-panel box) placer))
						       (write-file (inner-html content) fname))
						     (dolist (placer placer-list)
						       (place-inside-bottom-of content placer)))
						   :initial-filename file-name)))
      (set-on-click btn-rndr (lambda (obj)
			       (dolist (placer placer-list)
				 (place-inside-bottom-of (bottom-panel box) placer))
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
								(let ((vname (attribute e "data-lisp-name")))
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
			       (dolist (placer placer-list)
				 (place-inside-bottom-of content placer))))
      (set-on-click btn-prop
		    (lambda (obj)
		      (input-dialog obj
				    "Panel Name"
				    (lambda (result)
				      (when result
					(setf panel-name result)
					(setf (attribute content "data-lisp-name") panel-name)
					(setf (window-title win) panel-name)))
				    :default-value panel-name
				    :title "Panel Properties")))
      (set-on-window-close win
			   (lambda (obj)
			     (declare (ignore obj))
			     (setf (current-control app) nil)
			     (setf (current-placer app) nil)
			     (on-populate-control-properties win)
			     (populate-control-list-win)))
      (set-on-mouse-down content
			 (lambda (obj data)
			   (unless in-simulation
			     (let* ((control     (selected-tool app))
				    (create-type (getf control :create-type))
				    (element     (cond ((eq create-type :label)
							(funcall (getf control :create) content
								 :content (getf control :create-content)))
						       ((eq create-type :form)
							(funcall (getf control :create) content
								 (getf control :create-param)
								 :value (getf control :create-value)))
						       (t nil)))
				    (placer      (when element
						   (create-div obj))))
			       (unless element
				 (when (current-placer app)
				   (set-border (current-placer app) (unit "px" 0) :none :blue))
				 (setf (current-control app) nil)
				 (setf (current-placer app) nil)
				 (on-populate-control-properties win))
			       (when element
				 (setf (current-control app) element)
				 (push element control-list)
				 (push placer placer-list)
				 (setf (attribute element "data-lisp-name")
				       (format nil "~A-~A" (getf control :name) (html-id element)))
				 (setf (attribute element "data-clog-type") (getf control :name))
				 (setf (box-sizing element) :content-box)
				 (setf (box-sizing placer) :content-box)
				 (set-on-mouse-down placer (lambda (obj data)
							     (declare (ignore obj) (ignore data))
							     (when (current-placer app)
							       (set-border (current-placer app) (unit "px" 0) :none :blue))
							     (setf (current-control app) element)
							     (setf (current-placer app) placer)
							     (set-border placer (unit "px" 2) :solid :blue)
							     (on-populate-control-properties win))
						    :cancel-event t)
				 (setf (selected-tool app) nil)
				 (setf (positioning element) :absolute)
				 (set-geometry element
					       :left (getf data :x)
					       :top (getf data :y))
				 (setf (positioning placer) :absolute)
				 (when (current-placer app)
				   (set-border (current-placer app) (unit "px" 0) :none :blue))
				 (set-border placer (unit "px" 2) :solid :blue)
				 (setf (current-placer app) placer)
				 (clog::jquery-execute placer "draggable().resizable()")
				 (set-geometry placer
					       :left (getf data :x)
					       :top (getf data :y))
				 (if (> (client-width element) 0)
				     (set-geometry placer :units ""
							  :width (client-width element)
							  :height (client-height element))
				     (set-geometry placer :units ""
							  :width (width element)
							  :height (height element)))
				 (on-populate-control-properties win)
				 (clog::set-on-event placer "resizestop"
						     (lambda (obj)
						       (declare (ignore obj))
						       (set-geometry element :units ""
									     :width (width placer)
									     :height (height placer))
						       (set-geometry placer :units ""
									    :width (client-width element)
									    :height (client-height element))
						       (on-populate-control-properties win)))
				 (clog::set-on-event placer "dragstop"
						     (lambda (obj)
						       (declare (ignore obj))
						       (set-geometry element :units ""
									     :top (top placer)
									     :left (left placer))
						       (on-populate-control-properties win)))
				 (populate-control-list-win)))))))))

(defun on-help-about-builder (obj)
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
      (create-gui-menu-item tools :content "Control Pallete"    :on-click 'on-show-control-pallete)
      (create-gui-menu-item tools :content "Control Properties" :on-click 'on-show-control-properties)
      (create-gui-menu-item tools :content "Control List"       :on-click 'on-show-control-list)
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
    (on-show-control-pallete body)
    (on-show-control-list body)
    (on-show-control-properties body)
    (on-new-builder-window body)
    (set-on-before-unload (window body) (lambda(obj)
					  (declare (ignore obj))
					  ;; return empty string to prevent nav off page
					  ""))
    (run body)))

(defun clog-builder ()
  "Start clog-builder."
  (initialize nil)
  (set-on-new-window 'on-new-builder :boot-file "/debug.html" :path "/builder")
  (open-browser :url "http://127.0.0.1:8080/builder"))
