(defpackage #:clog-tools
  (:use #:cl #:clog #:clog-gui)
  (:export clog-builder))

(in-package :clog-tools)

(defclass builder-app-data ()
  ((body
    :accessor body
    :documentation "Top level access to browser window")))

(defun on-show-code (obj)
  (let* ((win       (create-gui-window obj :title  "Code"
					   :height 400
					   :width  650))
	 (box       (create-panel-box-layout (window-content win)))
	 (center    (center-panel box))
	 (center-id (html-id center)))
    (set-on-window-size win (lambda (obj)
			      (js-execute obj
					  (format nil "editor_~A.resize()" (html-id win)))))
    (set-on-window-size-done win (lambda (obj)
				   (js-execute obj
					       (format nil "editor_~A.resize()" (html-id win)))))
    (js-execute center
		(format nil
			"var editor_~A = ace.edit('~A');
                         editor_~A.setTheme('ace/theme/xcode');
                         editor_~A.session.setMode('ace/mode/lisp');
                         editor_~A.session.setTabSize(3);
                         editor_~A.focus();"
			(html-id win) center-id
			(html-id win)
			(html-id win)
			(html-id win)
			(html-id win)))))

(defun on-show-properties (obj)
  (let* ((win          (create-gui-window obj :title "Properties" :height 300 :width 200 :has-pinner t))
	 (content      (window-content win))
	 (control-list (create-select content)))
    (setf (positioning control-list) :absolute)
    (setf (size control-list) 2)
    (set-geometry control-list :left 0 :top 0 :bottom 0 :width 190)
    (add-select-options control-list '("name    : var-1"
				       "visible : true"
				       "text    : ''"))))
  
(defun on-show-control-pallete (obj)
  (let* ((win          (create-gui-window obj :title "Controls" :height 300 :width 200 :has-pinner t))
	 (content      (window-content win))
	 (control-list (create-select content)))
    (setf (positioning control-list) :absolute)
    (setf (size control-list) 2)
    (set-geometry control-list :left 0 :top 0 :bottom 0 :width 190)
    (add-select-options control-list '("select"
				       "label"
				       "text input"))))

(defun on-new-builder-window (obj)
  ;; add menu items for save, etc
  ;; add tool bar
  ;; add on close to remove hooks
  ;; add hooks to drop controls,add actions
  (let* ((win (create-gui-window obj :title "New Panel"))
	 (content (window-content win)))
    (set-on-mouse-up content
		     (lambda (obj data)
		       ;; check what tool
		       ;; apply tool or add control
		       
		       (let* ((element (create-form-element obj :input)))
			 (setf (positioning element) :absolute)
			 (set-geometry element
				       :left (getf data ':x)
				       :top (getf data ':y)))))))

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
    (setf (body app) body)
    (setf (title (html-document body)) "CLOG Builder")
    (clog-gui-initialize body) 
    (load-script (html-document body) "https://pagecdn.io/lib/ace/1.4.12/ace.js")   
    (add-class body "w3-blue-grey")  
    (let* ((menu  (create-gui-menu-bar body))
	   (icon  (create-gui-menu-icon menu :on-click #'on-help-about-builder))
	   (file  (create-gui-menu-drop-down menu :content "Builder"))
	   (tools (create-gui-menu-drop-down menu :content "Tools"))
	   (win   (create-gui-menu-drop-down menu :content "Window"))
	   (help  (create-gui-menu-drop-down menu :content "Help")))
      (declare (ignore icon))
      (create-gui-menu-item file :content "Open Panel" :on-click 'on-new-builder-window)
      (create-gui-menu-item tools :content "Control Pallete" :on-click 'on-show-control-pallete)
      (create-gui-menu-item tools :content "Properties" :on-click 'on-show-properties)
      (create-gui-menu-item tools :content "Code" :on-click 'on-show-code)
      (create-gui-menu-item win :content "Maximize All" :on-click #'maximize-all-windows)
      (create-gui-menu-item win :content "Normalize All" :on-click #'normalize-all-windows)
      (create-gui-menu-window-select win)
      (create-gui-menu-item help :content "About" :on-click #'on-help-about-builder)
      (create-gui-menu-full-screen menu))    
    (set-on-before-unload (window body) (lambda(obj)
					  (declare (ignore obj))
					  ;; return empty string to prevent nav off page
					  ""))    
    (run body)))

(defun clog-builder ()
  "Start clog-builder."
  (initialize 'on-new-builder)
  (open-browser))
