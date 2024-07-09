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
(defparameter *no-quicklisp* nil "Do not use quicklisp")

(defvar *scope* nil "The last evaluated scope object")

;; Per instance app data

(defclass builder-app-data ()
  ((stdout
     :accessor stdout
     :initform nil
     :documentation "The standard-output for this instance")
   (stdin
     :accessor stdin
     :initform nil
     :documentation "The standard-input for this instance")
   (static-root-display
     :accessor static-root-display
     :initform nil
     :documentation "Display area for static-root")
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
   (probe-win
     :accessor probe-win
     :initform nil
     :documentation "Probe window")
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
   (last-panel-editor
     :accessor last-panel-editor
     :initform nil
     :documentation "Last editor panel focused, to avoid recalculations")
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
                                           </center><br>
                                           <center class='w3-tiny'>Programming is a pain, so let's make it easier on the programmers<br>
                                           --- this is at the very heart of Lisp philosophy.<br>
                                           (\"Lisp philosophy\" TANAKA Tomoyuki)</center>
                                                 </div>"
                                                   img-clog-icon)
                                  :width   400
                                  :height  300
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
  (let ((pref (read-file (format nil "~A/preferences.lisp.sample"
                                 (merge-pathnames "tools"
                                                  (asdf:system-source-directory :clog))))))
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
                     You will need to close and update the build when done."
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

(defun hide-panel-windows (app)
  (when (control-events-win app)
    (setf (hiddenp (control-events-win app)) t))
  (when (control-js-events-win app)
    (setf (hiddenp (control-js-events-win app)) t))
  (when (control-ps-events-win app)
    (setf (hiddenp (control-ps-events-win app)) t))
  (when (controls-win app)
    (setf (hiddenp (controls-win app)) t))
  (when (control-properties-win app)
    (setf (hiddenp (control-properties-win app)) t)))

(defun show-panel-windows (app)
  (when (control-events-win app)
    (setf (hiddenp (control-events-win app)) nil))
  (when (control-js-events-win app)
    (setf (hiddenp (control-js-events-win app)) nil))
  (when (control-ps-events-win app)
    (setf (hiddenp (control-ps-events-win app)) nil))
  (when (controls-win app)
    (setf (hiddenp (controls-win app)) nil))
  (when (control-properties-win app)
    (setf (hiddenp (control-properties-win app)) nil)))

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
        (safe t)
        (open-file  (form-data-item (form-get-data body) "open-file"))
        (open-panel (form-data-item (form-get-data body) "open-panel"))
        (open-ext   (form-data-item (form-get-data body) "open-ext")))
    (setf (connection-data-item body "builder-app-data") app)
    (setf (title (html-document body)) "CLOG Builder")
    (setf (stdout app) (if clog-connection:*disable-clog-debugging*
                           *standard-output*
                           (make-instance 'console-out-stream :clog-obj body)))
    (setf (stdin app) (if clog-connection:*disable-clog-debugging*
                          *standard-input*
                          (make-instance 'console-in-stream :clog-obj body)))
    (clog-gui-initialize body :use-clog-debugger t :standard-output (stdout app))
    (add-class body *builder-window-desktop-class*)
    (load-script (html-document body) "/builder-js/beautify.js" :load-only-once t)
    (load-script (html-document body) "/builder-js/beautify-css.js" :load-only-once t)
    (load-script (html-document body) "/builder-js/beautify-html.js" :load-only-once t)
    (when *password-protect*
      (input-dialog body "Enter password:" (lambda (result)
                                             (unless (equal result (if (functionp *password-protect*)
                                                                       (funcall *password-protect* body)
                                                                       *password-protect*))
                                               (setf safe nil)
                                               (close-connection (window body))))
                    :time-out 360 :title "Password"))
    (when safe
      (with-clog-debugger (body :standard-output (stdout app))
                          (when *builder-window-show-static-root-class*
                            (setf (static-root-display app)
                                  (create-panel body :positioning :fixed
                                                :bottom 0 :right 0
                                                :class *builder-window-show-static-root-class*
                                                :content (format nil "static-root: ~A" *static-root*)))
                            (setf (z-index (static-root-display app)) -9999))
                          (let* ((menu  (create-gui-menu-bar body))
                                 (icon  (create-gui-menu-icon menu :image-url img-clog-icon
                                                              :on-click  #'on-help-about-builder))
                                 (file  (create-gui-menu-drop-down menu :content "Builder"))
                                 (src   (create-gui-menu-drop-down menu :content "Project"))
                                 (tools (create-gui-menu-drop-down menu :content "Tools"))
                                 (opts  (create-gui-menu-drop-down menu :content "Options"))
                                 (win   (create-gui-menu-drop-down menu :content "Window"))
                                 (help  (create-gui-menu-drop-down menu :content "Help"))
                                 search
                                 sysbrw)
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
                            (create-gui-menu-item src   :content "New Directory Tree"             :on-click 'on-dir-tree)
                            (create-gui-menu-item src   :content "New Project from template"      :on-click 'on-new-app-template)
                            (create-gui-menu-item src   :content "New System Source Browser"      :on-click
                                                  (lambda (obj) (on-new-sys-browser obj :doc-maximize t)))
                            (create-gui-menu-item src   :content "New Loaded ASDF System Browser" :on-click 'on-new-asdf-browser)
                            ;; Menu -> Tools
                            (create-gui-menu-item tools :content "CLOG Builder REPL"           :on-click 'on-repl)
                            (create-gui-menu-item tools :content "CLOG Builder Console"        :on-click 'on-open-console)
                            (create-gui-menu-item tools :content "CLOG Probe Panel"            :on-click 'on-probe-panel)
                            (create-gui-menu-item tools :content "CLOG Object Scope"           :on-click 'on-object-scope)
                            (create-gui-menu-item tools :content "OS Pseudo Shell"             :on-click 'on-shell)
                            (create-gui-menu-item tools :content "Regex File Search"           :on-click 'on-file-search)
                            (create-gui-menu-item tools :content "List Callers"                :on-click 'on-show-callers)
                            (create-gui-menu-item tools :content "List Callees"                :on-click 'on-show-callees)
                            (create-gui-menu-item tools :content "Thread Viewer"               :on-click 'on-show-thread-viewer)
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
                            (create-gui-menu-item opts :content "Set *clog-debug-instance*"
                                                  :on-click (lambda (obj)
                                                              (setf *clog-debug-instance* (connection-body obj))
                                                              (alert-toast obj "*clog-debug-instance*"
                                                                           "This window will be used for future default debug alerts."
                                                                           :color-class "w3-green"
                                                                           :time-out 2)))
                            (unless *no-quicklisp*
                              (create-gui-menu-item opts :content "Update CLOG Builder"       :on-click 'on-update-clog))
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
                            (create-gui-menu-item win   :content "Hide Panel Editor Panels" :on-click
                                                  (lambda (obj)
                                                    (declare (ignore obj))
                                                    (hide-panel-windows app)))
                            (create-gui-menu-item win   :content "Show Panel Editor Panels" :on-click
                                                  (lambda (obj)
                                                    (declare (ignore obj))
                                                    (show-panel-windows app)))
                            ;; Menu -> Help
                            (create-gui-menu-item help  :content "CLOG Manual"          :on-click
                                                  (lambda (obj)
                                                    (declare (ignore obj))
                                                    (open-window (window body) "https://rabbibotton.github.io/clog/clog-manual.html")))
                            (create-gui-menu-item help  :content "Learn Lisp & CLOG"   :on-click
                                                  (lambda (obj)
                                                    (declare (ignore obj))
                                                    (open-window (window body) "https://github.com/rabbibotton/clog/blob/main/LEARN.md")))
                            (create-gui-menu-item help  :content "Tutorials DIR"  :on-click
                                                  (lambda (obj)
                                                    (on-dir-tree obj :dir (merge-pathnames "./tutorial/"
                                                                                           (asdf:system-source-directory :clog)))))
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
                            (create-gui-menu-item help  :content "ParenScript Reference" :on-click
                                                  (lambda (obj)
                                                    (declare (ignore obj))
                                                    (open-window (window body) "https://parenscript.common-lisp.dev/")))
                            (create-gui-menu-item help  :content "About CLOG Builder"   :on-click #'on-help-about-builder)
                            (create-gui-menu-window-select menu)
                            (create-gui-menu-full-screen menu)
                            (setf search  (create-gui-menu-drop-down menu :content
                                                                     "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
                                                                     &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
                                                                     &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
                                                                     &#x1F441;" :right-align t))
                            (setf sysbrw (create-form-element search :search :class *builder-menu-search-class* :size 35))
                            (setf (place-holder sysbrw) "system browse.. (alt-period)")
                            (set-on-change sysbrw (lambda (obj)
                                                    (on-new-sys-browser obj :search (text-value obj) :doc-maximize t)
                                                    (setf (text-value obj) "")))
                            (setf sysbrw (create-form-element search :search :class *builder-menu-search-class*))
                            (setf (place-holder sysbrw) "regex search.. (alt-comma)")
                            (set-on-change sysbrw (lambda (obj)
                                                    (on-file-search obj :search (text-value obj) :doc-maximize t)
                                                    (setf (text-value obj) ""))))
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
                              (on-project-tree body :project *start-project*)
                              (when *start-dir*
                                (handler-case
                                    (on-dir-tree body :dir *start-dir*)
                                  (error (msg)
                                    (alert-toast body "Directory Error" (format nil "Unable to open directory ~A. ~A" *start-dir* msg))
                                    (setf *start-dir* nil))))))
                          (set-on-before-unload (window body) (lambda(obj)
                                                                (declare (ignore obj))
                                                                ;; return empty string to prevent nav off page
                                                                "")))
      (when *app-mode*
        (incf *app-mode*))
      (run body)))
  (when (eq *clog-debug-instance* body)
    (setf *clog-debug-instance* nil))
  (when *app-mode*
    (decf *app-mode*)
    (when (<= *app-mode* 0)
      (clog:shutdown)
      (uiop:quit))))

(defun clog-open (&optional open-file)
  (unless (is-running-p)
    (clog-builder :app t :port 0 :start-browser nil)
    (sleep 1))
  (let* ((open-loc (if (and (> (length open-file) 5)
                        (equal (subseq open-file (- (length open-file) 5)) ".clog"))
                       "/panel-editor?open-panel"
                       "/source-editor?open-file"))
         (open-url  (format nil "~A=~A"
                            open-loc (if (or (eq open-file nil)
                                          (equal open-file ""))
                                         "%20"
                                         open-file))))
    (format t "~%If browser does not start go to http://127.0.0.1:~A~A" clog:*clog-port* open-url)
    (open-browser :url (format nil "http://127.0.0.1:~A~A" clog:*clog-port* open-url))))

(defun clog-builder (&key (host "0.0.0.0") (port 8080) (start-browser t)
                     app project dir static-root system clogframe
                     (new-template "ncp") no-quicklisp)
  "Start clog-builder.
  :PROJECT     - load ASDF Project, start its static root and set as current
  :DIR         - Start with directory tree set to dir
  :PORT        - default 8080, use 0 for random open port
  :APP         - start in app mode shutdown application on termination
                 If APP eq :BATCH then must specify the default project :PROJECT
                   and it will be batch rerendered and shutdown after.
  :STATIC-ROOT - set static-root dir manually.
  :SYSTEM      - Use projects's asdf system's static root."
  (declare (ignorable new-template))
  #-quicklisp
  (progn
    (setf no-quicklisp t)
    (unless project
      (setf project (car (last (pathname-directory (uiop:getcwd))))))
    (let ((fname (format nil "~A~A.asd" (uiop:getcwd) project)))
      (format t "Starting non-quicklisp dir based system - ~A~%" project)
      (unless (uiop:file-exists-p fname)
        (format t "New System - Creating Project ~A~%" fname)
        (fill-template new-template (uiop:getcwd) project))))
  (setf *preferances-file*
        (if project
            (format nil "~Apreferences.lisp"
                    (asdf:system-source-directory project))
            (format nil "~A/preferences.lisp"
                    (merge-pathnames "tools"
                                     (asdf:system-source-directory :clog)))))
  (clog-connection:add-plugin-path "^/builder-js/"
                                   (merge-pathnames "./static-files/"
                                                    (asdf:system-source-directory :clog)))
  (load *preferances-file*
        :if-does-not-exist nil
        :verbose t)
  (setf *start-dir* nil)
  (when no-quicklisp
    (setf *no-quicklisp* (or project no-quicklisp)))
  (if project
      (progn
        (setf *start-project* (string-downcase (format nil "~A" project)))
        (projects-load *start-project*)
        (setf static-root (merge-pathnames "./www/" (format nil "~A" (asdf:system-source-directory project)))))
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
      (initialize nil :host host :port port :static-root static-root)
      (initialize nil :host host :port port))
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

#-quicklisp
(defpackage #:ql
  (:use #:cl)
  (:export :*local-project-directories*))

#-quicklisp
(defvar ql:*local-project-directories* (list (uiop:getcwd)))

#+(and windows quicklisp)
(in-package #:quicklisp-client)

;; patch, if-exists of :rename-and-delete does not work well on windows
#+(and windows quicklisp)
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
