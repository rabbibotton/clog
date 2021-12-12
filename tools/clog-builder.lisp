(defpackage #:clog-tools
  (:use #:cl #:clog #:clog-gui)
  (:export clog-builder))

(in-package :clog-tools)

(defvar supported-controls
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
     :properties     (list
		      (:name "text"
		       :prop  clog:text)
		      (:name "background-color"
		       :prop  clog:background-color)))
   '(:name            "button"
     :description     "Button"
     :create          clog:create-form-element
     :create-type     :form
     :create-param    :button
     :create-value    "button"
     :properties      (list
		       (:name "value"
			:prop  clog:value)
		       (:name "background-color"
			:prop  clog:background-color)))
   '(:name            "input"
     :description     "Text Input"
     :create          clog:create-form-element
     :create-type     :form
     :create-param    :input
     :create-value    ""
     :properties      (list
		       (:name "value"
			:prop  clog:value)
		       (:name "background-color"
			:prop  clog:background-color)))))

(defun control-info (control-name)
  (find-if (lambda (x) (equal (getf x :name) control-name)) supported-controls))

(defclass builder-app-data ()
  ((copy-buf
    :accessor copy-buf
    :initform ""
    :documentation "Copy buffer")
   (current-control
    :accessor current-control
    :initform nil
    :documentation "Current selected control")
   (control-properties
    :accessor control-properties
    :initform nil
    :documentation "Current control properties window")
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

(defun on-show-code (obj)
  (let* ((win         (create-gui-window obj :title  "Code"
					     :height 400
					     :width  650))
	 (box         (create-panel-box-layout (window-content win)
					       :left-width 0 :right-width 9
					       :top-height 30 :bottom-height 0))
	 (center      (center-panel box))
	 (center-id   (html-id center))
	 ;;top panel
	 (event-drop  (create-select (top-panel box)
			 :label (create-label (top-panel box)
					      :content " Event: "))))
    (setf (width event-drop) (unit :px 200))
    (add-select-options event-drop '("on-focus" "on-blur"))
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
			(html-id win)))))

(defun on-show-layout-code (obj)
  (let* ((win         (create-gui-window obj :title  "Layout Code"
					     :height 400
					     :width  650))
	 (box         (create-panel-box-layout (window-content win)
					       :left-width 0 :right-width 9
					       :top-height 30 :bottom-height 0))
	 (center      (center-panel box))
	 (center-id   (html-id center))
	 (tool-bar (top-panel box))
	 (btn-save (create-button tool-bar :content "Save"))
	 (btn-eval (create-button tool-bar :content "Run")))
    (setf (background-color tool-bar) :silver)
    (set-on-click btn-eval (lambda (obj)
			     (do-eval obj)))
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
	 (parent  (when control (parent-element control))))
    (when (and win control)
      (setf (text (properties-list app)) "")
      (add-select-options (properties-list app)
			  `(,(format nil "name    : ~A" (html-id control))
			    ,(format nil "top     : ~A" (top parent))
			    ,(format nil "left    : ~A" (left parent))
			    ,(format nil "bottom  : ~A" (bottom parent))
			    ,(format nil "right   : ~A" (right parent))
			    ,(format nil "value   : ~A" (value control)))))))

(defun on-show-properties (obj)
  (let ((app (connection-data-item obj "builder-app-data")))
    (if (control-properties app)
	(window-focus (control-properties app))	
	(let* ((win          (create-gui-window obj :title "Properties"
						    :height 300 :width 200
						    :has-pinner t))
	       (content      (window-content win))
	       (control-list (create-select content)))
	  (setf (control-properties app) win)
	  (setf (properties-list app) control-list)
	  (set-on-window-close win (lambda (obj) (setf (control-properties app) nil)))
	  (setf (positioning control-list) :absolute)
	  (setf (size control-list) 2)
	  (set-geometry control-list :left 0 :top 0 :bottom 0 :width 190)
	  (on-populate-control-properties obj)))))

(defun on-show-control-pallete (obj)
  (let ((app (connection-data-item obj "builder-app-data")))
    (if (control-pallete app)
	(window-focus (control-pallete app))
	(let* ((win          (create-gui-window obj :title "Controls" :height 300 :width 200 :has-pinner t))
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

(defun on-new-builder-window (obj)
  (let* ((app (connection-data-item obj "builder-app-data"))
	 (win (create-gui-window obj :title "New Panel"))
	 (box (create-panel-box-layout (window-content win)
				       :left-width 0 :right-width 9
				       :top-height 30 :bottom-height 0))
	 (tool-bar (top-panel box))
	 (btn-save (create-button tool-bar :content "Render"))
	 (content  (center-panel box)))
    (setf (background-color tool-bar) :silver)
    (set-on-click btn-save (lambda (obj)
			     (declare (ignore obj))
			     (let* ((cw     (on-show-layout-code obj))
				    (result (format nil
						    "(defvar *form_~A* \"~A\")~%~
(clog:set-on-new-window (lambda (body) (clog:create-div body :content *form_~A*)) :path \"/form_~A\")~%~
(clog:open-browser :url \"http://127.0.0.1:8080/form_~A\")~%"
						    (html-id cw)
						    (escape-string
						     (ppcre:regex-replace-all "\\x22"
									      (inner-html content)
									      "\\\\\\\""))
						    (html-id cw)
						    (html-id cw)
						    (html-id cw))))
			       (js-execute obj (format nil
						       "editor_~A.setValue('~A');editor_~A.moveCursorTo(0,0);"
						       (html-id cw)
						       (escape-string result)
						       (html-id cw))))))
    (set-on-window-close win
			 (lambda (obj)
			   (setf (current-control app) nil)))
    (set-on-mouse-up content
		     (lambda (obj data)
		       (let* ((control     (selected-tool app))
			      (create-type (getf control :create-type))
			      (handle      (create-div obj))
			      (element     (cond ((eq create-type :label)
						  (funcall (getf control :create) handle
							   :content (getf control :create-content)))
						 ((eq create-type :form)				 
						  (funcall (getf control :create) handle
							   (getf control :create-param)
							   :value (getf control :create-value)))
						 (t nil))))
			 (when element
			   (setf (current-control app) element)
			   (setf (box-sizing element) :content-box)
			   (setf (box-sizing handle) :content-box)
			   (set-padding handle "0px" "16px" "0px" "0px")							
			   (set-on-focus-in element (lambda (obj)
			   			      (declare (ignore obj))
						      (setf (current-control app) element)
						      (on-populate-control-properties win)
						      (let ((x (position-left handle))
							    (y (position-top handle)))
							(set-geometry handle :left (- x 12) :top (- y 12))
							(set-border handle "12px" :solid :blue))))
			   (set-on-focus-out element (lambda (obj)
			   			       (declare (ignore obj))
						       (let ((x (position-left handle))
							     (y (position-top handle)))
							 (set-border handle "initial" "" "")
							 (set-geometry handle :left (+ x 12) :top (+ y 12)))))
			   (setf (selected-tool app) nil)
			   (clog::jquery-execute handle "draggable().resizable()")
			   (set-geometry element :units "%" :width 100 :height 100)
			   (setf (positioning handle) :absolute)
			   (set-geometry handle
					 :left (getf data :x)
					 :top (getf data :y))))))))

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
      (create-gui-menu-item file  :content "Open Panel"      :on-click 'on-new-builder-window)
      (create-gui-menu-item tools :content "Control Pallete" :on-click 'on-show-control-pallete)
      (create-gui-menu-item tools :content "Properties"      :on-click 'on-show-properties)
      (create-gui-menu-item tools :content "Code"            :on-click 'on-show-code)
      (create-gui-menu-item tools :content "Eval Code"       :on-click 'do-eval)
      (create-gui-menu-item edit  :content "Undo"            :on-click #'do-ide-edit-undo)
      (create-gui-menu-item edit  :content "Redo"            :on-click #'do-ide-edit-redo)
      (create-gui-menu-item edit  :content "Copy"            :on-click #'do-ide-edit-copy)
      (create-gui-menu-item edit  :content "Cut"             :on-click #'do-ide-edit-cut)
      (create-gui-menu-item edit  :content "Paste"           :on-click #'do-ide-edit-paste)
      (create-gui-menu-item win   :content "Maximize All"    :on-click #'maximize-all-windows)
      (create-gui-menu-item win   :content "Normalize All"   :on-click #'normalize-all-windows)
      (create-gui-menu-window-select win)
      (create-gui-menu-item help  :content "About"           :on-click #'on-help-about-builder)
      (create-gui-menu-full-screen menu))
    (on-show-control-pallete body)
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
