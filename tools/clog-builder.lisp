;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG Builder - UI Design tool for CLOG                                ;;;;
;;;; (c) 2020-2024 David Botton                                            ;;;;
;;;; License BSD 3 Clause                                                  ;;;;
;;;;                                                                       ;;;;
;;;; clog-buider.lisp                                                      ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :clog-tools)

;; Global Internal Config

(defparameter *app-mode* nil
  "If *app-mode* is t terminates the clog-builder process on exit of the all
clog-builder window.")
(defparameter *clogframe-mode* nil
              "If *clogframe-mode* is t no popup or tabs possible.")
(defparameter *preferances-file* nil "Location of the preferance file")

(defparameter *start-project* nil "Set the project to start with")
(defparameter *start-dir* nil "Set the directory the dir win should start with")
(defparameter *client-side-movement* nil "Use javascript for window movement")

;; Per instance app data

(defclass builder-app-data ()
  ((stdout
     :accessor stdout
     :initform nil
     :documentation "The standard-output for this instance")
   (copy-buf
     :accessor copy-buf
     :initform nil
     :documentation "Copy buffer")
   (copy-history-win
     :accessor copy-history-win
     :initform nil
     :documentation "Copy history window")
   (console-win
     :accessor console-win
     :initform nil
     :documentation "Console window")
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
   (project-tree-win
     :accessor project-tree-win
     :initform nil
     :documentation "Project Tree window")
   (project-win
     :accessor project-win
     :initform nil
     :documentation "ASD Project window")
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
   (controls-win
     :accessor controls-win
     :initform nil
     :documentation "Current controls window")
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
  (let ((app (connection-data-item obj "builder-app-data"))
        (*default-title-class*      *builder-title-class*)
        (*default-border-class*     *builder-border-class*))
    (if (copy-history-win app)
        (progn
          (setf (hiddenp (copy-history-win app)) nil)
          (window-focus (copy-history-win app)))
        (let* ((win (create-gui-window obj :title "Copy History"
                                       :height 400 :width 600
                                       :has-pinner t
                                       :client-movement *client-side-movement*)))
          (window-center win)
          (setf (hiddenp win) t)
          (setf (overflow (window-content win)) :scroll)
          (setf (copy-history-win app) win)
          (set-on-window-can-close win (lambda (obj)
                                         (declare (ignore obj))
                                         (setf (hiddenp win) t)
                                         nil))))))

(defun on-help-about-builder (obj)
  "Open about box"
  (let ((*default-title-class*      *builder-title-class*)
        (*default-border-class*     *builder-border-class*)
        (about (create-gui-window obj
                                  :title   "About"
                                  :content (format nil "<div class='w3-black'>
                                         <center><img src='~A'></center>
                                         <center>CLOG</center>
                                         <center>The Common Lisp Omnificent GUI</center></div>
                                         <div><p><center>
                                           <a target=_blank href='https://github.com/rabbibotton/clog'>CLOG Builder</a>
                                           </center>
                                         <center>(c) 2022-2024 - David Botton</center></p>
                                                 <p>
                                                 <center>
                                           <a target=_blank href='https://github.com/sponsors/rabbibotton'>Sponsor CLOG</a>
                                           </center>
                                                 </div>"
                                                   img-clog-icon)
                                  :width   400
                                  :height  250
                                  :hidden  t)))
    (add-class about "w3-animate-opacity")
    (window-center about)
    (setf (visiblep about) t)
    (set-on-window-can-size about (lambda (obj)
                                    (declare (ignore obj))()))))

(defun on-new-app-template (obj)
  "Menu option to create new project from template"
  (let* ((*default-title-class*      *builder-title-class*)
         (*default-border-class*     *builder-border-class*)
         (win (create-gui-window obj :title "New Application Template"
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
  (let* ((*default-title-class*      *builder-title-class*)
         (*default-border-class*     *builder-border-class*)
         (win (create-gui-window obj :title "Convert Images to Data"
                                 :width 450 :height 200)))
    (create-image-to-data (window-content win))
    (window-center win)))

(defun on-convert-image (body)
  "Convert image from form input from on-image-to-data"
  (let ((*default-title-class*      *builder-title-class*)
        (*default-border-class*     *builder-border-class*)
        (params (form-multipart-data body)))
    (create-div body :content params)
    (destructuring-bind (stream fname content-type)
                        (form-data-item params "filename")
      (create-div body :content (format nil "filename = ~A - " fname))
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

(defun on-show-thread-viewer (obj)
  "Open thread views"
  (let* ((*default-title-class*      *builder-title-class*)
         (*default-border-class*     *builder-border-class*)
         (win (create-gui-window obj :title "Thread Viewer"
                                 :top 40 :left 225
                                 :width 600 :height 400
                                 :client-movement *client-side-movement*)))
    (create-thread-list (window-content win))))

(defun on-show-callers (body)
  "Open callers window"
  (let ((*default-title-class*      *builder-title-class*)
        (*default-border-class*     *builder-border-class*))
    (input-dialog body "Enter package:function-name :"
                  (lambda (result)
                    (when result
                      (handler-case
                          (on-open-file body :title (format nil "Callers of ~A" result)
                                        :title-class *builder-show-callers-class*
                                        :text (swank::list-callers (read-from-string result)))
                        (t (c)
                          (on-open-file body :title "Error - Callers"
                                        :title-class "w3-red"
                                        :text c))))))))

(defun on-show-callees (body)
  "Open callees window"
  (let ((*default-title-class*      *builder-title-class*)
        (*default-border-class*     *builder-border-class*))
    (input-dialog body "Enter package:function-name :"
                  (lambda (result)
                    (when result
                      (handler-case
                          (on-open-file body :title (format nil "Callees of ~A" result)
                                        :title-class *builder-show-callees-class*
                                        :text (swank::list-callees (read-from-string result)))
                        (t (c)
                          (on-open-file body :title "Error - Callees"
                                        :title-class "w3-red"
                                        :text c))))))))

(defun on-opts-edit (body)
  (let ((pref (read-file (format nil "~A.sample" *preferances-file*))))
    (unless pref
      (setf pref ";; No sample preferances file found"))
    (on-open-file body :open-file *preferances-file*
                  :lisp-package "clog-tools"
                  :text pref
                  :title *preferances-file*)))

(defun on-update-clog (body)
  (if *app-mode*
      (alert-dialog body
                    "Results of update will apear when completed.
                     You may need to press ENTER on OS console window."
                    :title "CLOG Update")
      (alert-toast body
                   "CLOG Update"
                   "Results of update will apear when completed."
                   :color-class "w3-green" :time-out 2))
  (let ((results (capture-eval "(ql:update-all-dists :prompt nil)" :clog-obj body)))
    (if *app-mode*
        (on-open-file body :title "CLOG Updated - Close builder, rerun make/update and rerun."
                      :title-class "w3-green w3-animate-top"
                      :text results)
        (on-open-file body :title "CLOG Updated - Close builder, rerun (ql:quickload :clog/tools)(clog-tools:clog-builder)"
                      :title-class "w3-green w3-animate-top"
                      :text results))))

(defun on-open-file-window (body)
  (on-new-builder body))

(defun on-open-panel-window (body)
  (on-new-builder body))

(defun on-new-builder (body)
  "Launch instance of the CLOG Builder"
  (set-html-on-close body "Connection Lost")
  (indentify:load-templates +common-lisp-templates+
                            +asdf-templates+
                            +uiop-templates+
                            +alexandria-templates+)
  (let ((app        (make-instance 'builder-app-data))
        (*menu-bar-class*           *builder-menu-bar-class*)
        (*menu-bar-drop-down-class* *builder-menu-bar-drop-down-class*)
        (*menu-item-class*          *builder-menu-item-class*)
        (*menu-window-select-class* *builder-menu-window-select-class*)
        (*default-title-class*      *builder-title-class*)
        (*default-border-class*     *builder-border-class*)
        (open-file  (form-data-item (form-get-data body) "open-file"))
        (open-panel (form-data-item (form-get-data body) "open-panel"))
        (open-ext   (form-data-item (form-get-data body) "open-ext")))
    (setf (connection-data-item body "builder-app-data") app)
    (setf (title (html-document body)) "CLOG Builder")
    (setf (stdout app) (if clog-connection:*disable-clog-debugging*
                           *standard-output*
                           (make-instance 'console-out-stream :clog-obj body)))
    (clog-gui-initialize body :use-clog-debugger t :standard-output (stdout app))
    (add-class body *builder-window-desktop-class*)
    (with-clog-debugger (body :standard-output (stdout app))
                        (when *builder-window-show-static-root-class*
                          (setf (z-index (create-panel body :positioning :fixed
                                                       :bottom 0 :right 0
                                                       :class *builder-window-show-static-root-class*
                                                       :content (format nil "static-root: ~A" clog:*static-root*)))
                                -9999))
                        (let* ((menu  (create-gui-menu-bar body))
                               (icon  (create-gui-menu-icon menu :image-url img-clog-icon
                                                            :on-click  #'on-help-about-builder))
                               (file  (create-gui-menu-drop-down menu :content "Builder"))
                               (src   (create-gui-menu-drop-down menu :content "Project"))
                               (tools (create-gui-menu-drop-down menu :content "Tools"))
                               (opts  (create-gui-menu-drop-down menu :content "Options"))
                               (win   (create-gui-menu-drop-down menu :content "Window"))
                               (help  (create-gui-menu-drop-down menu :content "Help")))
                          (declare (ignore icon))
                          ;; Menu -> File
                          (let ((exter (create-button file :content "-" :class *builder-menu-button-class*)))
                            (flet ((exter-text ()
                                     (if *open-external*
                                         "open external tab"
                                         "open this tab")))
                              (setf (text-value exter) (exter-text))
                              (set-on-click exter (lambda (obj)
                                                    (declare (ignore obj))
                                                    (setf *open-external* (not *open-external*))
                                                    (setf (text-value exter) (exter-text)))))
                            (create-gui-menu-item file  :content "New Source Editor"                       :on-click
                                                  (lambda (obj)
                                                    (if *open-external*
                                                        (on-open-file-ext obj)
                                                        (on-open-file obj))))
                            (create-gui-menu-item file  :content "New CLOG Panel Editor"                   :on-click
                                                  (lambda (obj)
                                                    (if *open-external*
                                                        (on-new-builder-panel-ext obj)
                                                        (on-new-builder-panel obj))))
                            (create-gui-menu-item file  :content "New CLOG Panel Popup Editor"             :on-click 'on-new-builder-page)
                            (create-gui-menu-item file  :content "New HTML Panel Popup Editor"             :on-click 'on-new-builder-basic-page)
                            (create-gui-menu-item file  :content "New Custom Boot Panel External Editor"   :on-click 'on-new-builder-custom-page)
                            (create-gui-menu-item file  :content "New CLOG Builder Window"                 :on-click
                                                  (lambda (obj)
                                                    (declare (ignore obj))
                                                    (open-window (window body) "/builder"))))
                          ;; Menu -> Project
                          (create-gui-menu-item src   :content "Project Tree"                   :on-click 'on-project-tree)
                          (create-gui-menu-item src   :content "ASD Project Window"             :on-click 'on-show-project)
                          (create-gui-menu-item src   :content "New Project from template"      :on-click 'on-new-app-template)
                          (create-gui-menu-item src   :content "New OS Directory Browser"       :on-click 'on-dir-win)
                          (create-gui-menu-item src   :content "New System Source Browser"      :on-click 'on-new-sys-browser)
                          (create-gui-menu-item src   :content "New Loaded ASDF System Browser" :on-click 'on-new-asdf-browser)
                          ;; Menu -> Tools
                          (create-gui-menu-item tools :content "List Callers"                :on-click 'on-show-callers)
                          (create-gui-menu-item tools :content "List Callees"                :on-click 'on-show-callees)
                          (create-gui-menu-item tools :content "Thread Viewer"               :on-click 'on-show-thread-viewer)
                          (create-gui-menu-item tools :content "CLOG Builder REPL"           :on-click 'on-repl)
                          (create-gui-menu-item tools :content "CLOG Builder Console"        :on-click 'on-open-console)
                          (create-gui-menu-item tools :content "OS Pseudo Shell"                    :on-click 'on-shell)
                          (create-gui-menu-item tools :content "Copy/Cut History"            :on-click 'on-show-copy-history-win)
                          (unless *clogframe-mode*
                            (create-gui-menu-item tools :content "Image to HTML Data"        :on-click 'on-image-to-data))
                          (create-gui-menu-item tools :content "Launch DB Admin"           :on-click
                                                (lambda (obj)
                                                  (declare (ignore obj))
                                                  (open-window (window body) "/dbadmin")))
                          ;; Menu -> Options
                          (create-gui-menu-item opts :content "Edit preferences.lisp"      :on-click 'on-opts-edit)
                          (let ((exter (create-button opts :content "-" :class *builder-menu-button-class*)))
                            (flet ((exter-text ()
                                     (if *open-external-with-emacs*
                                         "open external files in emacs"
                                         "open all files in builder")))
                              (setf (text-value exter) (exter-text))
                              (set-on-click exter (lambda (obj)
                                                    (declare (ignore obj))
                                                    (setf *open-external-with-emacs* (not *open-external-with-emacs*))
                                                    (setf (text-value exter) (exter-text))))))
                          (create-gui-menu-item opts :content "Start SWANK Server Once"        :on-click
                                                (lambda (obj)
                                                  (let ((*default-title-class*      *builder-title-class*)
                                                        (*default-border-class*     *builder-border-class*))
                                                    (input-dialog obj "Port ID" 
                                                                  (lambda (result)
                                                                    (swank:create-server :port (js-to-integer result) :dont-close nil)
                                                                    (let ((*default-title-class*      *builder-title-class*)
                                                                          (*default-border-class*     *builder-border-class*))
                                                                      (alert-dialog obj (format nil "Use slime-connect on emacs or on a REPL to port ~A"
                                                                                                result)
                                                                                    :title "SWANK Started (One time connect)")))
                                                                  :placeholder-value "4005" :default-value "4005" :title "Start SWANK Server"))))
                          (create-gui-menu-item opts :content "Update CLOG Builder"       :on-click 'on-update-clog)
                          ;; Menu -> Windows
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
                          ;; Menu -> Help
                          (create-gui-menu-item help  :content "CLOG Manual"          :on-click
                                                (lambda (obj)
                                                  (declare (ignore obj))
                                                  (open-window (window body) "https://rabbibotton.github.io/clog/clog-manual.html")))
                          (create-gui-menu-item help  :content "Learn CLOG"           :on-click
                                                (lambda (obj)
                                                  (declare (ignore obj))
                                                  (open-window (window body) "https://github.com/rabbibotton/clog/blob/main/LEARN.md")))
                          (create-gui-menu-item help  :content "Tutorials DIR"  :on-click
                                                (lambda (obj)
                                                  (on-dir-win obj :dir (merge-pathnames "./tutorial/"
                                                                                        (asdf:system-source-directory :clog)))))
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
                          (create-gui-menu-window-select menu)
                          (create-gui-menu-full-screen menu))
                        (on-show-copy-history-win body)
                        (cond
                          (open-panel
                            (if (equal open-panel " ")
                                (setf open-panel nil)
                                (setf (title (html-document body)) (file-namestring open-panel)))
                            (cond ((equalp open-ext "t")
                                    (setf open-ext t))
                                  ((equalp open-ext "custom")
                                    (setf open-ext :custom)))
                            (on-new-builder-panel body :open-file open-panel :open-ext open-ext))
                          (open-file
                            (if (equal open-file " ")
                                (setf open-file nil)
                                (setf (title (html-document body)) (file-namestring open-file)))
                            (on-open-file body :open-file open-file :maximized t))
                          (t
                            (when *start-project*
                              (projects-load *start-project*))
                            (on-project-tree body :project *start-project*)
                            (when *start-dir*
                              (when *start-project*
                                (set-geometry (current-window body) :top 38 :left 5 :right "" :height "" :bottom 22)
                                (set-geometry (current-window body) :height (height (current-window body))
                                              :bottom (bottom (current-window body))))
                              (handler-case
                                  (on-dir-win body :dir *start-dir*)
                                (error (msg)
                                  (alert-toast body "Directory Error" (format nil "Unable to open directory ~A. ~A" *start-dir* msg))
                                  (setf *start-dir* nil)))
                              (set-geometry (current-window body) :top 38 :left "" :right 5 :height "" :bottom 22)
                              (set-geometry (current-window body) :height (height (current-window body))
                                            :bottom (bottom (current-window body))))))
                        (set-on-before-unload (window body) (lambda(obj)
                                                              (declare (ignore obj))
                                                              ;; return empty string to prevent nav off page
                                                              "")))
    (when *app-mode*
      (incf *app-mode*))
    (run body))
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
  (setf *preferances-file*
        (format nil "~A/preferences.lisp"
                (merge-pathnames "tools"
                                 (asdf:system-source-directory :clog))))
  (load *preferances-file*
        :if-does-not-exist nil
        :verbose t)
  (setf *start-project* nil)
  (setf *start-dir* nil)
  (if project
      (progn
        (setf *start-project* (string-downcase (format nil "~A" project)))
        (setf *start-dir* (format nil "~A" (asdf:system-source-directory project)))
        (setf static-root (merge-pathnames "./www/" *start-dir*)))
      (setf *start-project* nil))
  (when dir
    (setf *start-dir* dir))
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
  (set-on-new-window 'on-open-panel-window :path "/panel-editor")
  (set-on-new-window 'on-open-file-window :path "/source-editor")
  (set-on-new-window 'on-convert-image :path "/image-to-data")
  (set-on-new-window 'on-new-db-admin :path "/dbadmin")
  (enable-clog-popup)
  (setf *clogframe-mode* clogframe)
  (when clogframe
    (setf *open-external-with-emacs* nil)
    (setf *open-external-source-in-popup* nil)
    (setf *open-panels-as-popups* nil)
    (setf *open-external-panels-in-popup* nil)
    (setf *open-external* nil)
    (handler-case
        (uiop:run-program (list "./clogframe"
                                "CLOG Builder"
                                (format nil "~A/builder" port)
                                (format nil "~A" 1280) (format nil "~A" 840)))
      (error ()
        (format t "~%Unable to load ./clogframe, trying clogframe.exe also.")
        (ignore-errors
          (uiop:run-program (list "clogframe.exe"
                                  "CLOG Builder"
                                  (format nil "~A/builder" port)
                                  (format nil "~A" 1280) (format nil "~A" 840)))))))
  (when start-browser
    (format t "~%If browser does not start go to http://127.0.0.1:~A/builder~%~%" port)
    (open-browser :url (format nil "http://127.0.0.1:~A/builder" port))))
    
(in-package #:quicklisp-client)

;; patch, if-exists of :rename-and-delete does not work well on windows
(defun make-system-index (pathname)
  "Create a system index file for all system files under
PATHNAME. Current format is one native namestring per line."
  (setf pathname (truename pathname))
  (with-open-file (stream (system-index-file pathname)
                          :direction :output
                          :if-exists :overwrite)
    (dolist (system-file (local-project-system-files pathname))
      (let ((system-path (enough-namestring system-file pathname)))
        (write-line (native-namestring system-path) stream)))
    (probe-file stream)))

