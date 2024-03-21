;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG Builder - UI Design tool for CLOG                                ;;;;
;;;; (c) 2020-2024 David Botton                                            ;;;;
;;;; License BSD 3 Clause                                                  ;;;;
;;;;                                                                       ;;;;
;;;; clog-buider.lisp                                                      ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :clog-tools)

;; Control Record Utilities / Plugin API for controls

(defun control-info (control-type-name)
  "Return the control-record for CONTROL-TYPE-NAME from supported controls. (Exported)"
  (if (equal control-type-name "clog-data")
       `(:name           "clog-data"
         :description    "Panel Properties"
         :events         nil
         :properties     ((:name "panel name"
                           :attr "data-clog-name")
                          (:name "in-package"
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

(defun add-supported-controls (control-records)
  "Add a list of control-records to builder's supported controls. If control exists it is
replaced. (Exported)"
  (dolist (r control-records)
    (setf *supported-controls*
          (append (remove-if (lambda (x)
                               (unless (equalp (getf x :name) "group")
                                 (equal (getf x :name) (getf r :name))))
                             *supported-controls*)
                  (list r)))))

(defun reset-control-pallete (panel)
  (let* ((app (connection-data-item panel "builder-app-data"))
         (pallete (select-tool app)))
    (setf (inner-html pallete) "")
    (dolist (control *supported-controls*)
      (if (equal (getf control :name) "group")
          (add-select-optgroup pallete (getf control :description))
          (add-select-option pallete (getf control :name) (getf control :description))))))

;; Global Internal Config

(defparameter *app-mode* nil
  "If *app-mode* is t terminates the clog-builder process on exit of the first
clog-builder window.")

(defparameter *start-project* nil "Set the project to start with")
(defparameter *start-dir* nil "Set the directory the dir win should start with")
(defparameter *client-side-movement* nil "Use javascript for window movement")

;; Per instance app data

(defclass builder-app-data ()
  ((copy-buf
    :accessor copy-buf
    :initform nil
    :documentation "Copy buffer")
   (copy-history-win
    :accessor copy-history-win
    :initform nil
    :documentation "Copy history window")
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
   (current-project
    :accessor current-project
    :initform *start-project*
    :documentation "Current Project")
   (current-project-dir
    :accessor current-project-dir
    :initform ""
    :documentation "Current Project")
   (project-win
    :accessor project-win
    :initform nil
    :documentation "Project window")
   (right-panel
    :accessor right-panel
    :initform nil
    :documentation "Right panel")
   (left-panel
    :accessor left-panel
    :initform nil
    :documentation "Left panel")
   (control-properties-win
    :accessor control-properties-win
    :initform nil
    :documentation "Current control properties window")
   (events-list
    :accessor events-list
    :initform nil
    :documentation "Event list in events window")
   (event-editor
    :accessor event-editor
    :initform nil
    :documentation "Editor in events window")
   (events-js-list
    :accessor events-js-list
    :initform nil
    :documentation "JS Event list in events window")
   (event-js-editor
    :accessor event-js-editor
    :initform nil
    :documentation "JS Editor in events window")
   (events-ps-list
    :accessor events-ps-list
    :initform nil
    :documentation "ParenScript Event list in events window")
   (event-ps-editor
    :accessor event-ps-editor
    :initform nil
    :documentation "PS Editor in events window")
   (auto-complete-configured
    :accessor auto-complete-configured
    :initform nil
    :documentation "Auto complete is setup once per instance")
   (current-editor-is-lisp
    :accessor current-editor-is-lisp
    :initform nil
    :documentation "Turn or off swank autocomplete")
   (control-events-win
    :accessor control-events-win
    :initform nil
    :documentation "Current control events window")
   (control-js-events-win
    :accessor control-js-events-win
    :initform nil
    :documentation "Current control events window")
   (control-ps-events-win
    :accessor control-ps-events-win
    :initform nil
    :documentation "Current control events window")
   (control-list-win
    :accessor control-list-win
    :initform nil
    :documentation "Current control list window")
   (control-pallete-win
    :accessor control-pallete-win
    :initform nil
    :documentation "Current control pallete window")
   (control-lists
    :accessor control-lists
    :initform (make-hash-table* :test #'equalp)
    :documentation "Panel -> Control List - hash table")))

;; Show windows

(defun on-show-copy-history-win (obj)
  "Create and show copy/but history"
  (let ((app (connection-data-item obj "builder-app-data")))
    (if (copy-history-win app)
        (progn
          (setf (hiddenp (copy-history-win app)) nil)
          (window-focus (copy-history-win app)))
        (let* ((win          (create-gui-window obj :title "Copy History"
                                                    :left 225
                                                    :top 480
                                                    :height 400 :width 600
                                                    :has-pinner t :client-movement *client-side-movement*)))
          (window-center win)
          (setf (hiddenp win) t)
          (setf (overflow (window-content win)) :scroll)
          (setf (copy-history-win app) win)
          (set-on-window-can-close win (lambda (obj)
                                         (declare (ignore obj))
                                         (setf (hiddenp win) t)
                                         nil))))))

(defun panel-mode (obj bool)
  "Set the status for display or hiding the side panels."
  (let ((app (connection-data-item obj "builder-app-data")))
    (setf (hiddenp (right-panel app)) (not bool))
    (setf (hiddenp (left-panel app)) (not bool))))

(defun on-help-about-builder (obj)
  "Open about box"
  (let ((about (create-gui-window obj
                                  :title   "About"
                                  :content (format nil "<div class='w3-black'>
                                         <center><img src='~A'></center>
                                         <center>CLOG</center>
                                         <center>The Common Lisp Omnificent GUI</center></div>
                                         <div><p><center>
                                           <a target=_blank href='https://github.com/sponsors/rabbibotton'>CLOG Builder</a>
                                           </center>
                                         <center>(c) 2022-2024 - David Botton</center></p></div>"
                                                   img-clog-icon)
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
      (if (eq (getf tmpl :code) :group)
          (add-select-optgroup (template-box ct) (getf tmpl :name))
          (add-select-option (template-box ct) (getf tmpl :code) (getf tmpl :name))))))

(defun on-image-to-data (obj)
  "Menu option to create new project from template"
  (let* ((win (create-gui-window obj :title "Convert Images to Data"
                                     :width 450 :height 200)))
    (create-image-to-data (window-content win))
    (window-center win)))

(defun on-convert-image (body)
  "Convert image from form input from on-image-to-data"
  (let ((params (form-multipart-data body)))
    (create-div body :content params)
    (destructuring-bind (stream fname content-type)
        (form-data-item params "filename")
      (create-div body :content (format nil "filename = ~A - (contents printed in REPL)" fname))
      (let ((s        (flexi-streams:make-flexi-stream stream))
            (pic-data ""))
        (setf pic-data (format nil "data:~A;base64,~A" content-type
                               (with-output-to-string (out)
                                 (s-base64:encode-base64 s out))))
        (create-img body :url-src pic-data)
        (create-br body)
        (create-div body :content "User the following as a url source:")
        (set-geometry (create-text-area body :value pic-data) :width 500 :height 400)
        (create-br body)
        (create-div body :content (format nil "For example:<br>(create-img body :url-src \"~A\")" pic-data))))))

(defun on-quick-start (obj)
  "Open quick start help"
  (let* ((win (create-gui-window obj :title "Quick Start"
                                     :top 40 :left 225
                                     :width 600 :height 400
                                     :client-movement *client-side-movement*)))
    (create-quick-start (window-content win))))

(defun on-show-thread-viewer (obj)
  "Open thread views"
  (let* ((win (create-gui-window obj :title "Thread Viewer"
                                     :top 40 :left 225
                                     :width 600 :height 400
                                     :client-movement *client-side-movement*)))
    (create-thread-list (window-content win))))


(defun on-repl (obj)
  "Open a REPL"
  (let* ((win (create-gui-window obj :title "CLOG Builder REPL"
                                     :top 40 :left 225
                                     :width 600 :height 400
                                     :client-movement *client-side-movement*)))
    (set-geometry (create-clog-builder-repl (window-content win))
                  :units "%" :width 100 :height 100)))

(defun on-show-callers (body)
  "Open callers window"
  (input-dialog body "Enter package:function-name :"
                (lambda (result)
                  (when result
                    (handler-case
                        (on-open-file body :title (format nil "Callers of ~A" result)
                                           :title-class "w3-orange"
                                           :text (swank::list-callers (read-from-string result)))
                      (t (c)
                        (on-open-file body :title "Error - Callers"
                                           :title-class "w3-red"
                                           :text c)))))))

(defun on-show-callees (body)
  "Open callees window"
  (input-dialog body "Enter package:function-name :"
                (lambda (result)
                  (when result
                    (handler-case
                        (on-open-file body :title (format nil "Callees of ~A" result)
                                           :title-class "w3-orange"
                                           :text (swank::list-callees (read-from-string result)))
                      (t (c)
                        (on-open-file body :title "Error - Callees"
                                           :title-class "w3-red"
                                           :text c)))))))

(defun on-dir-win (obj &key dir top left)
  "Open dir window"
  (let* ((win (create-gui-window obj :title "Directory Window"
                                     :top top :left left
                                     :width 600 :height 400
                                     :client-movement *client-side-movement*))
         (d   (create-dir-view (window-content win))))
    (set-geometry d :units "%" :width 100 :height 100)
    (when *open-external*
      (setf (checkedp (open-file-ext d)) t))
    (when dir
      (populate-dir-win d dir))))

(defun on-open-file-window (body)
  (on-new-builder body))

(defun on-open-panel-window (body)
  (on-new-builder body))

(defun on-new-builder (body)
  "Launch instance of the CLOG Builder"
  (set-html-on-close body "Connection Lost")
  (let ((app        (make-instance 'builder-app-data))
        (open-file  (form-data-item (form-get-data body) "open-file"))
        (open-panel (form-data-item (form-get-data body) "open-panel")))
    (setf (connection-data-item body "builder-app-data") app)
    (setf (title (html-document body)) "CLOG Builder")
    (clog-gui-initialize body)
    (add-class body "w3-blue-grey")
    (setf (z-index (create-panel body :positioning :fixed
                                      :bottom 0 :left 222
                                      :content (format nil "static-root: ~A" clog::*static-root*)))
          -9999)
    (let* ((menu  (create-gui-menu-bar body))
           (icon  (create-gui-menu-icon menu :image-url img-clog-icon
                                             :on-click  #'on-help-about-builder))
           (file  (create-gui-menu-drop-down menu :content "Builder"))
           (src   (create-gui-menu-drop-down menu :content "Project"))
           (tools (create-gui-menu-drop-down menu :content "Tools"))
           (win   (create-gui-menu-drop-down menu :content "Window"))
           (help  (create-gui-menu-drop-down menu :content "Help")))
      (declare (ignore icon))
      (add-class menu "w3-small")
      (create-gui-menu-item file  :content "New CLOG Panel"                        :on-click 'on-new-builder-panel)
      (create-gui-menu-item file  :content "New CLOG Panel External Edit"          :on-click 'on-new-builder-page)
      (create-gui-menu-item file  :content "New HTML Panel External Edit"          :on-click 'on-new-builder-basic-page)
      (create-gui-menu-item file  :content "New Application Template"              :on-click 'on-new-app-template)
      (create-gui-menu-item src   :content "Project Window"               :on-click 'on-show-project)
      (create-gui-menu-item src   :content "Directory Window"             :on-click 'on-dir-win)
      (create-gui-menu-item src   :content "New Source Editor"            :on-click 'on-open-file)
      (create-gui-menu-item src   :content "New Source Editor in New Tab" :on-click
                            (lambda (obj)
                              (declare (ignore obj))
                              (open-window (window body) "/source-editor?open-file=%20")))
      (create-gui-menu-item src   :content "New System Browser"           :on-click 'on-new-sys-browser)
      (create-gui-menu-item src   :content "New ASDF System Browser"      :on-click 'on-new-asdf-browser)
      (create-gui-menu-item tools :content "List Callers"                :on-click 'on-show-callers)
      (create-gui-menu-item tools :content "List Callees"                :on-click 'on-show-callees)
      (create-gui-menu-item tools :content "Thread Viewer"               :on-click 'on-show-thread-viewer)
      (create-gui-menu-item tools :content "CLOG Builder REPL"           :on-click 'on-repl)
      (create-gui-menu-item tools :content "Copy/Cut History"            :on-click 'on-show-copy-history-win)
      (unless *app-mode*
        (create-gui-menu-item tools :content "Image to HTML Data"        :on-click 'on-image-to-data))
      (create-gui-menu-item tools :content "Launch DB Admin"           :on-click
                            (lambda (obj)
                              (declare (ignore obj))
                              (open-window (window body) "/dbadmin")))
      (create-gui-menu-item win   :content "Maximize"           :on-click
			    (lambda (obj)
			      (when (current-window obj)
			        (window-maximize (current-window obj)))))
      (create-gui-menu-item win   :content "Normalize"          :on-click
			    (lambda (obj)
			      (when (current-window obj)
			        (window-normalize (current-window obj)))))
      (create-gui-menu-item win   :content "Maximize All"       :on-click #'maximize-all-windows)
      (create-gui-menu-item win   :content "Normalize All"      :on-click #'normalize-all-windows)
      (create-gui-menu-window-select win)
      (create-gui-menu-item help  :content "CLOG Quick Start"     :on-click 'on-quick-start)
      (create-gui-menu-item help  :content "CLOG Manual"          :on-click
                            (lambda (obj)
                              (declare (ignore obj))
                              (open-window (window body) "https://rabbibotton.github.io/clog/clog-manual.html")))
      (create-gui-menu-item help  :content "CLOG Tutorials"       :on-click
                            (lambda (obj)
                              (declare (ignore obj))
                              (open-window (window body) "https://github.com/rabbibotton/clog/blob/main/LEARN.md")))
      (create-gui-menu-item help  :content "ParenScript Reference" :on-click
                            (lambda (obj)
                              (declare (ignore obj))
                              (open-window (window body) "https://parenscript.common-lisp.dev/")))
      (create-gui-menu-item help  :content "L1sp Search"       :on-click
                            (lambda (obj)
                              (declare (ignore obj))
                              (open-window (window body) "http://l1sp.org/html/")))
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
      (create-gui-menu-item help  :content "About CLOG Builder"   :on-click #'on-help-about-builder)
      (create-gui-menu-full-screen menu))
    (on-show-control-properties-win body)
    (on-show-control-list-win body)
    (on-show-copy-history-win body)
    (cond
      (open-panel
       (setf (title (html-document body)) open-panel)
       (on-new-builder-panel body :open-file open-panel))
      (open-file
       (setf (title (html-document body)) open-file)
       (on-open-file body :open-file open-file :maximized t))   
      (*start-dir*
       (on-dir-win body :dir *start-dir* :top 60 :left 232))
      (t
        (on-show-project body :project *start-project*)))
    (set-on-before-unload (window body) (lambda(obj)
                                          (declare (ignore obj))
                                          ;; return empty string to prevent nav off page
                                          "")))
  (when *app-mode*
    (incf *app-mode*))
  (run body)
  (when *app-mode*
    (decf *app-mode*)
    (when (<= *app-mode* 0)
      (clog:shutdown)
      (uiop:quit))))

(defun clog-builder (&key (port 8080) (start-browser t)
                       app project dir static-root system clogframe)
  "Start clog-builder. When PORT is 0 choose a random port. When APP is
t, shutdown applicatoin on termination of first window. If APP eq :BATCH then
must specific default project :PROJECT and it will be batch rerendered
and shutdown application. You can set the specific STATIC-ROOT or set SYSTEM
to use that asdf system's static root. if DIR then the directory window
instead of the project window will be displayed."
  (load (format nil "~A/preferences.lisp"
                (merge-pathnames "./tools/"
                                 (asdf:system-source-directory :clog)))
        :if-does-not-exist nil
        :verbose t)
  (if project
      (setf *start-project* (string-downcase (format nil "~A" project)))
      (setf *start-project* nil))
  (setf *start-dir* dir)
  (when system
    (setf static-root (merge-pathnames "./www/"
                                       (asdf:system-source-directory system))))
  (when app
    (unless *app-mode*
      (setf *app-mode* 0)))
  (if static-root
      (initialize nil :port port :static-root static-root)
      (initialize nil :port port))
  (setf port clog:*clog-port*)
  (set-on-new-window 'on-new-builder :path "/builder")
  (set-on-new-window 'on-new-db-admin :path "/dbadmin")
  (set-on-new-window 'on-convert-image :path "/image-to-data")
  (set-on-new-window 'on-open-panel-window :path "/panel-editor")
  (set-on-new-window 'on-open-file-window :path "/source-editor")
  (enable-clog-popup)
  (when clogframe
    (uiop:run-program (list "./clogframe"
                            "CLOG Builder"
                            (format nil "~A/builder" port)
                            (format nil "~A" 1280) (format nil "~A" 840))))
  (when start-browser
    (format t "~%If browser does not start go to http://127.0.0.1:~A/builder~%~%" port)
    (open-browser :url (format nil "http://127.0.0.1:~A/builder" port))))
