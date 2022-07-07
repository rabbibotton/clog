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
    :documentation "Current control pallete window")
   (control-lists
    :accessor control-lists
    :initform (make-hash-table* :test #'equalp)
    :documentation "Panel -> Control List - hash table")))

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
             (when (equalp (js-query control (format nil "$.contains(document.documentElement, ~A)"
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

(defun panel-snap-shot (content panel-id hide-loc)
  "Take a snap shot of panel"
  (with-sync-event (content)
    (let (snap
	  (app (connection-data-item content "builder-app-data")))
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
	(setf snap (js-query content
                             (format nil
                                     "var z=~a.clone();~
                 z.find('*').each(function(){~
                   if($(this).attr('data-clog-composite-control') == 't'){$(this).text('')}~
                   if($(this).attr('id') !== undefined && ~
                     $(this).attr('id').substring(0,5)=='CLOGB'){$(this).removeAttr('id')}});~
                 z.html()"
                                     (jquery content))))
	(destroy data))
      (maphash
       (lambda (html-id control)
	 (declare (ignore html-id))
	 (place-after control (get-placer control)))
       (get-control-list app panel-id))
      snap)))


(defun save-panel (fname content panel-id hide-loc)
  "Save panel to FNAME"
  (write-file (panel-snap-shot content panel-id hide-loc) fname))

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
                     (afile (cond ((equalp (pathname-name nfile) "tmpl")
                                    (format nil "~A~A.~A" out-dir sys-name (pathname-type nfile)))
				   ((equalp (pathname-name nfile) "tmpl-tools")
                                    (format nil "~A~A-tools.~A" out-dir sys-name (pathname-type nfile)))
                                   (t
				    (format nil "~A~A" out-dir nfile)))))
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
  "Return the control-record for CONTROL-TYPE-NAME from supported controls."
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

(defun add-supported-controls (control-records)
  "Add a list of control-records to builder's supported controls. If control exists it is
replaced."
  (dolist (r control-records)
    (setf *supported-controls*
	  (append (remove-if (lambda (x)
			       (unless (equalp (getf x :name) "group")
				 (equal (getf x :name) (getf r :name))))
			     *supported-controls*)
		  (list r)))))

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
                                   (let ((c (create-child parent custom-query
							  :html-id uid)))
				     (setf control-type-name (attribute c "data-clog-type"))
				     (when (equalp control-type-name "undefined")
                                       (setf (attribute c "data-clog-type") "div")
                                       (setf control-type-name "div"))
                                     (let ((cr (control-info control-type-name)))
                                       (change-class c (getf cr :clog-type)))
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
  "Create new control dropped at event DATA location on CONTENT of WIN"
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
  "Create new control dropped at event DATA on CONTENT of WIN)"
  ;; create control
  (let* ((control-record    (control-info (value (select-tool app))))
         (control-type-name (getf control-record :name))
         (positioning       (cond ((or (getf data :ctrl-key)
				       (getf data :meta-key))
                                   :static)
				  ((getf control-record :positioning)
				   (getf control-record :positioning))
				  (t
                                   :absolute)))
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
           (setf (value (select-tool app)) "")
           (setf (box-sizing control) :content-box)
           (setf (positioning control) positioning)
           (set-geometry control
                         :left (getf data :x)
                         :top (getf data :y))
	   (unless (equalp (attribute control "data-clog-composite-control") "t")
             (add-sub-controls control content :win win))
           (setup-control content control :win win)
           (select-control control)
           (on-populate-control-list-win content :win win)
	   (jquery-execute (get-placer content) "trigger('clog-builder-snap-shot')")
           t)
          (t
           ;; panel directly clicked with select tool or no control type to add
           (deselect-current-control app)
           (on-populate-control-properties-win content :win win)
           (on-populate-control-list-win content :win win)
           nil))))

(defun setup-control (content control &key win)
  "Setup CONTROL by creating pacer and setting up events for manipulation"
  (let ((app      (connection-data-item content "builder-app-data"))
        (panel-id (html-id content))
        (placer   (create-div control :auto-place nil
                                      :class "placer"
                                      :html-id (format nil "p-~A" (html-id control)))))
    (add-to-control-list app panel-id control)
    ;; setup placer
    (set-geometry placer :top (position-top control)
                         :left (position-left control)
                         :width (client-width control)
                         :height (client-height control))
    (place-after control placer)
    (setf (box-sizing placer) :content-box)
    (setf (positioning placer) :absolute)
    (jquery-execute placer (format nil "draggable({snap:'.placer',snapMode:'inner',cursor:'crosshair'})~
                                        .resizable({alsoResize:'#~A',autoHide:true})"
                                   (html-id control)))
    ;; setup control events
    (set-on-focus control (lambda (obj)
                            ;; set focus is bound in case control
                            ;; is set to static or reached using
                            ;; tab selection
                            (select-control obj)))
    ;; setup placer events
    (setf (tab-index placer) "-1") ; must have a tab-index to accept keyboard input
    (focus placer)
    (set-on-key-down placer
		     (lambda (obj data)
		       (declare (ignore obj))
		       (let ((key   (getf data :key))
			     (ctrl  (getf data :ctrl-key))
			     (meta  (getf data :meta-key))
			     (shift (getf data :shift-key)))
                         (cond ((equal key "ArrowUp")
                                (if shift
                                    (set-geometry control :height (1- (height control)))
                                    (set-geometry control :top (1- (position-top control)))))
                               ((equal key "ArrowDown")
                                (if shift
                                    (set-geometry control :height (1+ (height control)))
                                    (set-geometry control :top (1+ (position-top control)))))
                               ((equal key "ArrowRight")
                                (if shift
                                    (set-geometry control :width (1+ (width control)))
                                    (set-geometry control :left (1+ (position-left control)))))
                               ((equal key "ArrowLeft")
                                (if shift
                                    (set-geometry control :width (1- (width control)))
                                    (set-geometry control :left (1- (position-left control)))))
                               ((and (equal key "c")
                                     (or meta ctrl))
                                (blur placer))
                               ((and (equal key "v")
                                     (or meta ctrl))
                                (blur placer))
                               ((and (equal key "x")
                                     (or meta ctrl))
                                (blur placer)))
                         (set-geometry placer :top (position-top control)
                                              :left (position-left control)
                                              :width (client-width control)
                                              :height (client-height control))
                         (jquery-execute placer "trigger('clog-builder-snap-shot')")
                         (set-properties-after-geomentry-change control))))
    (set-on-mouse-down placer
                       (lambda (obj data)
                         (declare (ignore obj))
                         (let ((last  (current-control app))
                               (shift (getf data :shift-key)))
                           (if (not (equal (value (select-tool app)) ""))
                               (when (do-drop-new-control app content data :win win)
                                 (incf-next-id content)))
                           (cond ((and last
                                       shift)
                                  (let* ((control1 last)
                                         (control2 control)
                                         (placer1  (get-placer control1))
                                         (placer2  (get-placer control2)))
                                    (place-inside-bottom-of control1 control2)
                                    (place-after control2 placer2)
                                              (place-after control2 placer2)
                                              (set-geometry placer1 :top (position-top control1)
                                                                    :left (position-left control1)
                                                                    :width (client-width control1)
                                                                    :height (client-height control1))
                                              (set-geometry placer2 :top (position-top control2)
                                                                    :left (position-left control2)
                                                                    :width (client-width control2)
                                                                    :height (client-height control2)))
                                  (on-populate-control-properties-win content :win win)
                                  (on-populate-control-list-win content :win win))
                                 (t
                                  (select-control control)))
                           (when win
                             (window-focus win))))
                       :cancel-event t)
    (set-on-mouse-double-click placer
                               (lambda (obj data)
                                 (setf (hiddenp placer) t)
                                 (on-populate-control-list-win content :win win)))
    (set-on-event placer "resize"
        (lambda (obj)
          (set-properties-after-geomentry-change obj)))
    (set-on-event placer "resizestop"
        (lambda (obj)
          (set-properties-after-geomentry-change obj)
	  (jquery-execute placer "trigger('clog-builder-snap-shot')"))
        :cancel-event t)
    (set-on-event placer "drag"
                  (lambda (obj)
                    (declare (ignore obj))
                    (set-geometry control :units ""
                                          :top (top placer)
                                          :left (left placer))
		    (set-properties-after-geomentry-change control)))
    (set-on-event placer "dragstop"
                  (lambda (obj)
                    (declare (ignore obj))
                    (set-geometry control :units ""
                                          :top (top placer)
                                          :left (left placer))
                    (set-geometry placer :top (top control)
                                         :left (left control))
		    (jquery-execute placer "trigger('clog-builder-snap-shot')")
                    (set-properties-after-geomentry-change control)))))

(defun set-property-display (control property value)
  "Set property in the currently displayed property panel"
  (js-execute control (format nil "$('.clog-prop-~A').text('~A')"
                              property value)))

(defun set-properties-after-geomentry-change (control)
  "Set properties window geometry setting"
    (set-property-display control "top" (top control))
    (set-property-display control "left" (left control))
    (set-property-display control "right" (right control))
    (set-property-display control "bottom" (bottom control))
    (set-property-display control "width" (client-width control))
    (set-property-display control "height" (client-height control)))


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
  (remove-from-control-list app panel-id html-id)
  (destroy (get-placer (current-control app)))
  (destroy (current-control app))
  (setf (current-control app) nil)
  (remove-deleted-from-control-list app panel-id))

(defun select-control (control)
  "Select CONTROL as the current control and highlight its placer.
The actual original clog object used for creation must be used and
not a temporary attached one when using select-control."
    (let ((app    (connection-data-item control "builder-app-data"))
          (placer (get-placer control)))
      (unless (eq control (current-control app))
        (deselect-current-control app)
        (set-geometry placer :top (position-top control)
                             :left (position-left control)
                             :width (client-width control)
                             :height (client-height control))
        (setf (current-control app) control)
        (set-border placer (unit "px" 2) :solid :blue)
        (on-populate-control-properties-win control))))

(defun add-sub-controls (parent content &key win paste)
  "Setup html imported in to CONTENT starting with PARENT for use with Builder"
  (let ((panel-uid (get-universal-time)))
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
                       (1+ panel-uid)
                       (jquery parent)
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
      (js-execute parent tmp))
    (unless paste
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
          (destroy data))))
    (labels ((add-siblings (control)
               (let (dct)
                 (loop
                   (when (equal (html-id control) "undefined") (return))
                   (setf dct (attribute control "data-clog-type"))
                   (unless (equal dct "undefined")
                     (change-class control (getf (control-info dct) :clog-type))
                     (when (getf (control-info dct) :on-load)
                       (funcall (getf (control-info dct) :on-load) control (control-info dct)))
                     (setup-control content control :win win)
                     (add-siblings (first-child control)))
                   (setf control (next-sibling control))))))
      (add-siblings (first-child parent)))))

;; Code rendering utlities

(defun render-clog-code (content hide-loc)
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
               (declare (ignore html-id))
               (place-inside-bottom-of hide-loc
                                       (get-placer control)))
             (get-control-list app panel-id))
    ;; crawl tree
    ;; Insure that on-setup/on-create follow order in tree
    (labels ((add-siblings (control)
               (let (dct)
                 (loop
                   (when (equal (html-id control) "undefined") (return))
                   (setf dct (attribute control "data-clog-name"))
                   (unless (equal dct "undefined")
                     (setf control (get-from-control-list app panel-id (html-id control)))
                     (let ((vname (attribute control "data-clog-name"))
                           (control-record (control-info (attribute control "data-clog-type"))))
                       (unless (and (>= (length vname) 5)
                                    (equalp (subseq vname 0 5) "none-"))
                         ;; Add to members of the panel's class for each control
                         (push (format nil
                                       "    \(~A :reader ~A\)~%"
                                       vname
                                       vname)
                               cmembers)
                         ;; On instance of class, set member value for each control
                         (push (format nil
                                       "    \(setf (slot-value panel '~A\) ~
                                            \(attach-as-child clog-obj \"~A\" :clog-type \'~A\ :new-id t)\)~%"
                                       vname
                                       (html-id control)
                                       (format nil "~S" (getf control-record :clog-type)))
                               vars)
                         ;; On instance of class, set handers defined for each control
                         (dolist (event (getf control-record :events))
                           ;; Set regular handlers
                           (let ((handler (attribute control (format nil "data-~A" (getf event :name)))))
                             (unless (or (equalp handler "undefined")
                                         (equal handler ""))
                               (unless (equalp (getf event :name) "on-create")
                                 (let ((event-package (or (getf event :package) "clog")))
                                   (push (format nil
                                                 "    \(~A:set-~A \(~A panel\) \(lambda \(~A\) \(declare \(ignorable ~A\)\) ~A\)\)~%"
                                                 event-package
                                                 (getf event :name)
                                                 vname
                                                 (getf event :parameters)
                                                 (getf event :parameters)
                                                 handler)
                                         events))))))
                         ;; Set on-create (from user in builder) and on-setup (from control-record)
                         (let ((handler (attribute control "data-on-create")))
                           (when (equalp handler "undefined")
                             (setf handler ""))
                           (when (getf control-record :on-setup)
                             (setf handler (format nil "~A~A"
                                                   (funcall (getf control-record :on-setup)
                                                            control control-record)
                                                   handler)))
                           (unless (equal handler "")
                             (push (format nil
                                           "    \(let \(\(target \(~A panel\)\)\) ~
                                             \(declare \(ignorable target\)\) ~
                                              ~A\)~%"
                                           vname
                                           handler)
                                   events)))))
                     (add-siblings (first-child control)))
                   (setf control (next-sibling control))))))
      (add-siblings (first-child content)))
    (let ((result (format nil
                          "\(in-package \"~A\"\)
\(defclass ~A \(clog:clog-panel\)
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
    z.find('*').each(function(){~
      var m=$(this).attr('data-clog-name');
      if($(this).attr('data-clog-composite-control') == 't'){$(this).text('')}~
      for(n in $(this).get(0).dataset){delete $(this).get(0).dataset[n]}~
      if(m){$(this).attr('data-clog-name', m);}~
    });~
    z.html()"
                                                                      (jquery content)))
                                                    "\\\"")
                          cname
                          vars
                          (reverse events)))) ; Insure that on-setup/on-create follow order in tree
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
		       (let ((txt (text-value obj)))
                         (if (or (equal txt "")
                                 (equalp txt "undefined"))
                             (remove-attribute control attr)
                             (setf (attribute control attr) (text-value obj))))))
                  events)))
        (dolist (item events)
          (let* ((tr     (create-table-row table))
                 (td1    (create-table-column tr :content (first item)))
                 (td2    (create-table-column tr))
                 (editor nil))
            (setf (width td1) "30%")
            (setf (width td2) "70%")
            (set-border td1 "1px" :dotted :black)
            (setf (advisory-title td1) (format nil "params: panel ~A" (third item)))
            (cond ((fourth item)
                   (setf editor td2)
                   (setf (editablep td2) (funcall (fourth item) control td1 td2)))
                  (t
                   (setf editor (create-text-area td2))
                   (setf (spellcheckp editor) nil)
                   (setf (width editor) "95%"))) ; leave space for scroll bar
            (setf (text-value editor) (second item))
            (set-on-blur editor
                         (lambda (obj)
			   (declare (ignore obj))
                           (funcall (fifth item) obj)
			   (jquery-execute (get-placer control) "trigger('clog-builder-snap-shot')")))))))))

(defun on-populate-control-properties-win (obj &key win)
  "Populate the control properties for the current control"
  ;; obj if current-control is nil must be content
  (with-sync-event (obj)
    (let ((app (connection-data-item obj "builder-app-data")))
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
          (let ((info (control-info (attribute control "data-clog-type")))
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
                                                (js-query
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
			      (when win
				(setf (window-title win) (text obj))))))
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
				 (jquery-execute placer "trigger('clog-builder-snap-shot')")
                                 (set-geometry placer :top (position-top control)
                                                      :left (position-left control)
                                                      :width (client-width control)
                                                      :height (client-height control)))))))))))))

(defun on-populate-loaded-window (content &key win)
  "Setup html imported in to CONTENT for use with Builder"
  (with-sync-event (content)
    (add-sub-controls content content :win win)))

(defun on-populate-control-list-win (content &key win)
  "Populate the control-list-window to allow drag and drop adjust of order
of controls and double click to select control."
  (with-sync-event (content)
    (let ((app (connection-data-item content "builder-app-data")))
      (let ((panel-id (html-id content))
            (last-ctl nil))
	(when (control-list-win app)
          (let ((lwin (control-list-win app)))
            (setf (inner-html lwin) "")
	    (set-on-mouse-click (create-div lwin :content (attribute content "data-clog-name"))
				(lambda (obj data)
				  (deselect-current-control app)
				  (on-populate-control-properties-win content :win win)
				  (on-populate-control-list-win content :win win)))
            (labels ((add-siblings (control sim)
                       (let (dln dcc)
			 (loop
                           (when (equal (html-id control) "undefined") (return))
			   (setf dcc (attribute control "data-clog-composite-control"))
                           (setf dln (attribute control "data-clog-name"))
                           (unless (equal dln "undefined")
                             (let ((list-item (create-div lwin :content (format nil "&#8597; ~A~A" sim dln)))
                                   (status    (hiddenp (get-placer control))))
                               (if status
                                   (setf (color list-item) :darkred)
                                   (setf (background-color list-item) :grey))
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
								 (getf data :ctrl-key)
								 (getf data :meta-key))
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
							(on-populate-control-list-win content :win win))))
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
						(on-populate-control-properties-win content :win win)
						(on-populate-control-list-win content :win win))))
                               (set-on-drag-start list-item (lambda (obj)(declare (ignore obj))())
                                                  :drag-data (html-id control))
			       (when (equal dcc "undefined") ; when t is not a composite control
				 (add-siblings (first-child control) (format nil "~A&#8594;" sim)))))
                           (setf control (next-sibling control))))))
              (add-siblings (first-child content) ""))))))))

;; Menu handlers

(defun do-eval (obj form-string cname &key (package "clog-user") custom-boot)
  "Render, evalute and run code for panel"
  (let* ((result (capture-eval (format nil "~A~% (clog:set-on-new-window~
                                               (lambda (body)~
                                                 (clog:debug-mode body)~
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
  (let* ((app (connection-data-item obj "builder-app-data"))
	 (is-hidden  nil)
	 (panel  (create-panel (connection-body obj) :positioning :fixed
						     :width 400
						     :top 40
						     :right 0 :bottom 0
						     :class "w3-border"))
	 (content (create-panel panel :width 390 :top 0 :right 0 :bottom 0))
         (side-panel (create-panel panel :top 0 :left 0 :bottom 0 :width 10))
         (control-list (create-table content)))
    (setf (background-color side-panel) :black)
    (setf (background-color content) :gray)
    (setf (control-properties-win app) content)
    (setf (properties-list app) control-list)
    (set-on-click side-panel (lambda (obj)
			       (cond (is-hidden
				      (setf (width panel) "400px")
				      (setf is-hidden nil))
				     (t
				      (setf (width panel) "10px")
				      (setf is-hidden t)))))
    (setf (overflow content) :auto)
    (setf (positioning control-list) :absolute)
    (set-geometry control-list :left 0 :top 0 :right 0)))

(defun on-show-control-events-win (obj)
  "Show control events window"
  (let ((app (connection-data-item obj "builder-app-data")))
    (if (control-events-win app)
        (window-focus (control-events-win app))
        (let* ((win          (create-gui-window obj :title "Control Events"
                                                    :left 225
                                                    :top 480
                                                    :height 200 :width 600
                                                    :has-pinner t :client-movement t))
               (content      (window-content win))
               (control-list (create-table content)))
          (setf (control-events-win app) win)
          (setf (events-list app) control-list)
          (set-on-window-close win (lambda (obj)
                                     (declare (ignore obj))
                                     (setf (control-events-win app) nil)))
          (setf (positioning control-list) :absolute)
          (set-geometry control-list :units "" :left 0 :top 0 :bottom 0 :width "100%")))))

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
                                                    :has-pinner t :client-movement t)))
	  (window-center win)
	  (setf (hiddenp win) t)
	  (setf (overflow (window-content win)) :scroll)
          (setf (copy-history-win app) win)
          (set-on-window-can-close win (lambda (obj)
					 (declare (ignore obj))
					 (setf (hiddenp win) t)
					 nil))))))

(defun on-show-control-list-win (obj)
  "Show control list for selecting and manipulating controls by name"
  (let* ((app          (connection-data-item obj "builder-app-data"))
	 (is-hidden    nil)
	 (content      (create-panel (connection-body obj) :positioning :fixed
						       :width 220
						       :top 40
						       :left 0 :bottom 0
						       :class "w3-border"))
         (side-panel   (create-panel content :top 0 :right 0 :bottom 0 :width 10))
	 (sheight      (floor (/ (height content) 2)))
	 (divider      (create-panel content :top sheight :height 10 :left 0 :right 10))
	 (control-list (create-panel content :height (- sheight 10) :left 0 :bottom 0 :right 10))
         (pallete      (create-select content))
	 (adj-size     0))
    (set-geometry pallete :left 0 :top 0 :height sheight :right 10)
    (setf (background-color divider) :black)
    (setf (tab-index divider) "-1")
    (setf (cursor divider) :ns-resize)
    (setf (background-color content) :gray)
    (setf (background-color pallete) :gray)
    (setf (color pallete) :white)
    (setf (positioning pallete) :absolute)
    (setf (size pallete) 2)
    (setf (advisory-title pallete) (format nil "<ctrl/cmd> place static~%<shift> child to current selection"))
    (setf (select-tool app) pallete)
    (dolist (control *supported-controls*)
      (if (equal (getf control :name) "group")
          (add-select-optgroup pallete (getf control :description))
          (add-select-option pallete (getf control :name) (getf control :description))))
    (setf (overflow control-list) :auto)
    (setf (control-list-win app) control-list)
    (setf (advisory-title content)
          (format nil "Drag and drop order~%Double click non-focusable~%~
                             <ctrl/cmd> place as static~%<shift> child to current selection"))
    (setf (background-color side-panel) :black)
    (flet ((on-size (obj)
	     (declare (ignore obj))
	     (setf sheight (floor (/ (height content) 2)))
	     (when (and (> (- sheight adj-size) 5)
			(> (+ (- sheight 10) adj-size) 5))
	       (set-geometry pallete :height (- sheight adj-size))
	       (set-geometry divider :top (- sheight adj-size))
	       (set-geometry control-list :height (+ (- sheight 10) adj-size)))))
      (set-on-resize (window (connection-body obj)) #'on-size)
      (set-on-full-screen-change (html-document (connection-body obj)) #'on-size)
      (set-on-orientation-change (window (connection-body obj)) #'on-size)
      (set-on-pointer-down divider (lambda (obj data)
				     (setf (getf data :client-y) (+ adj-size
								    (getf data :client-y)))
				     (set-on-pointer-up (connection-body obj)
							(lambda (obj data)
							  (declare (ignore data))
							  (set-on-pointer-up (connection-body obj) nil)
							  (set-on-pointer-move (connection-body obj) nil)))
				     (set-on-pointer-move (connection-body obj)
							  (lambda (obj new-data)
							    (setf adj-size (- (getf data :client-y)
									      (getf new-data :client-y)))
							    (on-size obj))))
			   :capture-pointer t))
    (set-on-click side-panel (lambda (obj)
			       (declare (ignore obj))
			       (cond (is-hidden
				      (setf (width content) "220px")
				      (setf is-hidden nil))
				     (t
				      (setf (width content) "10px")
				      (setf is-hidden t)))))))

(defun on-new-builder-panel (obj)
  "Open new panel"
  (let* ((app (connection-data-item obj "builder-app-data"))
         (win (create-gui-window obj :top 40 :left 225
                                     :width 600 :height 430
                                     :client-movement t))
         (box (create-panel-box-layout (window-content win)
                                       :left-width 0 :right-width 0
                                       :top-height 33 :bottom-height 0))
         (tool-bar  (create-div (top-panel box) :class "w3-center"))
	 (btn-class "w3-button w3-white w3-border w3-border-black w3-ripple")
         (btn-copy  (create-img tool-bar :alt-text "copy"     :url-src img-btn-copy  :class btn-class))
         (btn-paste (create-img tool-bar :alt-text "paste"    :url-src img-btn-paste :class btn-class))
         (btn-cut   (create-img tool-bar :alt-text "cut"      :url-src img-btn-cut   :class btn-class))
         (btn-del   (create-img tool-bar :alt-text "delete"   :url-src img-btn-del   :class btn-class))
         (btn-undo  (create-img tool-bar :alt-text "undo"     :url-src img-btn-undo  :class btn-class))
         (btn-redo  (create-img tool-bar :alt-text "redo"     :url-src img-btn-redo  :class btn-class))
         (btn-test  (create-img tool-bar :alt-text "test"     :url-src img-btn-test  :class btn-class))
         (btn-rndr  (create-img tool-bar :alt-text "render"   :url-src img-btn-rndr  :class btn-class))
         (btn-save  (create-img tool-bar :alt-text "save"     :url-src img-btn-save  :class btn-class))
         (btn-load  (create-img tool-bar :alt-text "load"     :url-src img-btn-load  :class btn-class))
         (content   (center-panel box))
         (in-simulation    nil)
	 (undo-chain       nil)
	 (redo-chain       nil)
         (file-name        "")
         (render-file-name "")
         (panel-id  (html-id content)))
    (setf (background-color (top-panel box)) :black)
    (setf (advisory-title btn-copy) "copy")
    (setf (advisory-title btn-paste) "paste")
    (setf (advisory-title btn-cut) "cut")
    (setf (advisory-title btn-del) "delete")
    (setf (advisory-title btn-undo) "undo")
    (setf (advisory-title btn-redo) "redo")
    (setf (advisory-title btn-test) "test")
    (setf (advisory-title btn-rndr) "render to lisp")
    (setf (advisory-title btn-save) "save")
    (setf (advisory-title btn-load) "load")
    (setf (height btn-copy) "12px")
    (setf (height btn-paste) "12px")
    (setf (height btn-cut) "12px")
    (setf (height btn-del) "12px")
    (setf (height btn-undo) "12px")
    (setf (height btn-redo) "12px")
    (setf (height btn-test) "12px")
    (setf (height btn-rndr) "12px")
    (setf (height btn-save) "12px")
    (setf (height btn-load) "12px")
    (setf-next-id content 1)
    (setf (overflow content) :auto)
    (init-control-list app panel-id)
    ;; Setup panel window
    (let ((panel-name (format nil "panel-~A" (incf (next-panel-id app)))))
      (setf (window-title win) panel-name)
      (setf (attribute content "data-clog-name") panel-name))
    (setf (attribute content "data-clog-type") "clog-data")
    (setf (attribute content "data-in-package") "clog-user")
    (setf (attribute content "data-custom-slots") "")
    ;; activate associated windows on open
    (on-populate-control-properties-win content :win win)
    (on-populate-control-list-win content :win win)
    ;; setup window events
    (set-on-window-focus win
                         (lambda (obj)
                           (declare (ignore obj))
                           (on-populate-control-properties-win content :win win)
                           (on-populate-control-list-win content :win win)))
    (set-on-window-close win
                         (lambda (obj)
                           (declare (ignore obj))
                           ;; clear associated windows on close
                           (setf (current-control app) nil)
                           (destroy-control-list app panel-id)
                           (on-populate-control-properties-win content :win win)
                           (on-populate-control-list-win content :win win)))
    (set-on-window-size-done win
                             (lambda (obj)
                               (declare (ignore obj))
                               (on-populate-control-properties-win content :win win)))
    ;; setup tool bar events
    (flet (;; copy
           (copy (obj)
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
     z.find('*').each(function(){~
       if($(this).attr('data-clog-composite-control') == 't'){$(this).text('')}~
       if($(this).attr('id') !== undefined && ~
         $(this).attr('id').substring(0,5)=='CLOGB'){$(this).removeAttr('id')}});~
     z.html()"
                                       (jquery (current-control app)))))
               (system-clipboard-write obj (copy-buf app))
	       (let ((c (create-text-area (window-content (copy-history-win app))
					  :value (copy-buf app)
					  :auto-place nil)))
                 (place-inside-top-of (window-content (copy-history-win app)) c)
                 (setf (width c) "100%"))
               (maphash
                (lambda (html-id control)
                  (declare (ignore html-id))
                  (place-after control (get-placer control)))
                (get-control-list app panel-id))))
           ;; paste
           (paste (obj)
             (let ((buf (or (system-clipboard-read obj)
                            (copy-buf app))))
               (when buf
                 (let ((control (create-control content content
                                                `(:name "custom"
                                                  :create-type :paste)
                                                (format nil "CLOGB~A~A"
                                                        (get-universal-time)
                                                        (next-id content))
                                                :custom-query buf)))
                   (setf (attribute control "data-clog-name")
                         (format nil "~A-~A" "copy" (next-id content)))
                   (incf-next-id content)
                   (add-sub-controls control content :win win :paste t)
                   (let ((cr (control-info (attribute control "data-clog-type"))))
                     (when (getf cr :on-load)
                       (funcall (getf cr :on-load) control cr)))
                   (setup-control content control :win win)
                   (select-control control)
                   (on-populate-control-list-win content :win win)
                   (jquery-execute (get-placer content) "trigger('clog-builder-snap-shot')")))))
           ;; delete
           (del (obj)
             (declare (ignore obj))
             (when (current-control app)
               (delete-current-control app panel-id (html-id (current-control app)))
               (on-populate-control-properties-win content :win win)
               (on-populate-control-list-win content :win win)
               (jquery-execute (get-placer content) "trigger('clog-builder-snap-shot')"))))
      ;; set up del/cut/copy/paste handlers
      (set-on-copy content #'copy)
      (set-on-click btn-copy #'copy)
      (set-on-paste content #'paste)
      (set-on-click btn-paste #'paste)
      (set-on-click btn-del #'del)
      (set-on-cut content (lambda (obj)
                            (copy obj)
                            (del obj)))
      (set-on-click btn-cut (lambda (obj)
                              (copy obj)
                              (del obj))))
    (set-on-click btn-undo (lambda (obj)
                             (declare (ignore obj))
                             (when undo-chain
                               (setf (inner-html content)
                                     (let ((val (pop undo-chain)))
                                       (push val redo-chain)
                                       val))
                               (clrhash (get-control-list app panel-id))
                               (on-populate-loaded-window content :win win)
                               (setf (window-title win) (attribute content "data-clog-name"))
                               (on-populate-control-properties-win content :win win)
                               (on-populate-control-list-win content :win win))))
    (set-on-event content "clog-builder-snap-shot"
                  (lambda (obj)
                    (declare (ignore obj))
                    (setf redo-chain nil)
                    (push (panel-snap-shot content panel-id (bottom-panel box)) undo-chain)
                    (when (current-control app)
                      (focus (get-placer (current-control app))))))
    (set-on-click btn-redo (lambda (obj)
                             (declare (ignore obj))
                             (when redo-chain
                               (setf (inner-html content)
                                     (let ((val (pop redo-chain)))
                                       (push val undo-chain)
                                       val))
                               (clrhash (get-control-list app panel-id))
                               (on-populate-loaded-window content :win win)
                               (setf (window-title win) (attribute content "data-clog-name"))
                               (on-populate-control-properties-win content :win win)
                               (on-populate-control-list-win content :win win))))
    (set-on-click btn-load (lambda (obj)
                             (server-file-dialog obj "Load Panel" (directory-namestring file-name)
                                                 (lambda (fname)
                                                   (window-focus win)
                                                   (when fname
                                                     (setf file-name fname)
						     (setf render-file-name "")
                                                     (setf (inner-html content)
                                                           (read-file fname))
                                                     (clrhash (get-control-list app panel-id))
                                                     (on-populate-loaded-window content :win win)
                                                     (setf (window-title win) (attribute content "data-clog-name"))
                                                     (on-populate-control-list-win content :win win))))))
    (set-on-click btn-save (lambda (obj)
                             (when (equal file-name "")
                               (setf file-name (format nil "~A.clog" (attribute content "data-clog-name"))))
                             (server-file-dialog obj "Save Panel As.." file-name
                                                 (lambda (fname)
                                                   (window-focus win)
                                                   (when fname
                                                     (setf file-name fname)
                                                     (save-panel fname content panel-id (bottom-panel box)))
                                                   :initial-filename file-name))))
    (set-on-click btn-test
                  (lambda (obj)
                      (do-eval obj (render-clog-code content (bottom-panel box))
                        (attribute content "data-clog-name")
                        :package (attribute content "data-in-package"))))
    (set-on-click btn-rndr
                  (lambda (obj)
                    (when (equal render-file-name "")
                      (if (equal file-name "")
                          (setf render-file-name (format nil "~A.lisp" (attribute content "data-clog-name")))
                          (setf render-file-name (format nil "~A~A.lisp"
                                (directory-namestring file-name)
                                (pathname-name file-name)))))
                    (server-file-dialog obj "Render As.." render-file-name
                                        (lambda (fname)
                                          (window-focus win)
                                          (when fname
                                            (setf render-file-name fname)
                                            (write-file (render-clog-code content (bottom-panel box))
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
         (undo-chain       nil)
         (redo-chain       nil)
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
    (on-populate-control-list-win content :win win)
    ;; setup window events
    (set-on-window-focus win
                         (lambda (obj)
                           (declare (ignore obj))
                           (on-populate-control-properties-win content :win win)
                           (on-populate-control-list-win content :win win)))
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
                                         :top-height 33 :bottom-height 0))
           (tool-bar  (create-div (top-panel pbox) :class "w3-center"))
           (btn-class "w3-button w3-white w3-border w3-border-black w3-ripple")
           (btn-copy  (create-img tool-bar :alt-text "copy"     :url-src img-btn-copy  :class btn-class))
           (btn-paste (create-img tool-bar :alt-text "paste"    :url-src img-btn-paste :class btn-class))
           (btn-cut   (create-img tool-bar :alt-text "cut"      :url-src img-btn-cut   :class btn-class))
           (btn-del   (create-img tool-bar :alt-text "delete"   :url-src img-btn-del   :class btn-class))
           (btn-undo  (create-img tool-bar :alt-text "undo"     :url-src img-btn-undo  :class btn-class))
           (btn-redo  (create-img tool-bar :alt-text "redo"     :url-src img-btn-redo  :class btn-class))
           (btn-sim   (create-img tool-bar :alt-text "simulate" :url-src img-btn-sim   :class btn-class))
           (btn-test  (create-img tool-bar :alt-text "test"     :url-src img-btn-test  :class btn-class))
           (btn-rndr  (create-img tool-bar :alt-text "render"   :url-src img-btn-rndr  :class btn-class))
           (btn-save  (create-img tool-bar :alt-text "save"     :url-src img-btn-save  :class btn-class))
           (btn-load  (create-img tool-bar :alt-text "load"     :url-src img-btn-load  :class btn-class))
           (btn-exp   (create-img tool-bar :alt-text "export"   :url-src img-btn-exp   :class btn-class))
           (wcontent  (center-panel pbox)))
      (setf (background-color (top-panel pbox)) :black)
      (setf (advisory-title btn-copy) "copy")
      (setf (advisory-title btn-paste) "paste")
      (setf (advisory-title btn-cut) "cut")
      (setf (advisory-title btn-del) "delete")
      (setf (advisory-title btn-undo) "undo")
      (setf (advisory-title btn-redo) "redo")
      (setf (advisory-title btn-test) "test")
      (setf (advisory-title btn-rndr) "render to lisp")
      (setf (advisory-title btn-save) "save")
      (setf (advisory-title btn-load) "load")
      (setf (advisory-title btn-sim) "start simulation")
      (setf (advisory-title btn-exp) "export as boot page")
      (setf (height btn-copy) "12px")
      (setf (height btn-paste) "12px")
      (setf (height btn-cut) "12px")
      (setf (height btn-del) "12px")
      (setf (height btn-undo) "12px")
      (setf (height btn-redo) "12px")
      (setf (height btn-sim) "12px")
      (setf (height btn-test) "12px")
      (setf (height btn-rndr) "12px")
      (setf (height btn-save) "12px")
      (setf (height btn-load) "12px")
      (setf (height btn-exp) "12px")
      (create-div wcontent :content
                  "<br><center>Drop and work with controls on it's window.</center>")
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
      (flet (;; copy
             (copy (obj)
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
     z.find('*').each(function(){~
       if($(this).attr('data-clog-composite-control') == 't'){$(this).text('')}~
       if($(this).attr('id') !== undefined && ~
         $(this).attr('id').substring(0,5)=='CLOGB'){$(this).removeAttr('id')}});~
     z.html()"
                                         (jquery (current-control app)))))
                 (system-clipboard-write obj (copy-buf app))
		 (let ((c (create-text-area (window-content (copy-history-win app))
					    :value (copy-buf app)
					    :auto-place nil)))
		   (place-inside-top-of (window-content (copy-history-win app)) c)
		   (setf (width c) "100%"))
                 (maphash
                  (lambda (html-id control)
                    (declare (ignore html-id))
                    (place-after control (get-placer control)))
                  (get-control-list app panel-id))))
             ;; paste
             (paste (obj)
               (let ((buf (or (system-clipboard-read obj)
                              (copy-buf app))))
                 (when buf
                   (let ((control (create-control content content
                                                  `(:name "custom"
                                                    :create-type :paste)
                                                  (format nil "CLOGB~A~A"
                                                          (get-universal-time)
                                                          (next-id content))
                                                  :custom-query buf)))
                     (setf (attribute control "data-clog-name")
                           (format nil "~A-~A" "copy" (next-id content)))
                     (incf-next-id content)
                     (add-sub-controls control content :win win :paste t)
                     (let ((cr (control-info (attribute control "data-clog-type"))))
                       (when (getf cr :on-load)
                         (funcall (getf cr :on-load) control cr)))
                     (setup-control content control :win win)
                     (select-control control)
                     (on-populate-control-list-win content :win win)
		     (jquery-execute (get-placer content) "trigger('clog-builder-snap-shot')")))))
             ;; delete
             (del (obj)
               (declare (ignore obj))
               (when (current-control app)
                 (delete-current-control app panel-id (html-id (current-control app)))
                 (on-populate-control-properties-win content :win win)
                 (on-populate-control-list-win content :win win)
		 (jquery-execute (get-placer content) "trigger('clog-builder-snap-shot')"))))
        ;; set up del/cut/copy/paste handlers
        (set-on-copy content #'copy)
        (set-on-click btn-copy #'copy)
        (set-on-paste content #'paste)
        (set-on-click btn-paste #'paste)
        (set-on-click btn-del #'del)
        (set-on-cut content (lambda (obj)
                              (copy obj)
                              (del obj)))
        (set-on-click btn-cut (lambda (obj)
                                (copy obj)
                                (del obj))))
      (set-on-click btn-sim (lambda (obj)
                              (declare (ignore obj))
                              (cond (in-simulation
                                     (setf (url-src btn-sim) img-btn-sim)
				     (setf (advisory-title btn-sim) "start simulation")
                                     (setf in-simulation nil)
                                     (maphash (lambda (html-id control)
                                                (declare (ignore html-id))
                                                (setf (hiddenp (get-placer control)) nil))
                                              (get-control-list app panel-id)))
                                    (t
                                     (setf (url-src btn-sim) img-btn-cons)
				     (setf (advisory-title btn-sim) "construction mode")
                                     (deselect-current-control app)
                                     (on-populate-control-properties-win content :win win)
                                     (setf in-simulation t)
                                     (maphash (lambda (html-id control)
                                                (declare (ignore html-id))
                                                (setf (hiddenp (get-placer control)) t))
                                              (get-control-list app panel-id))
                                     (focus (first-child content))))))
      (set-on-click btn-undo (lambda (obj)
			       (declare (ignore obj))
			       (when undo-chain
				 (setf (inner-html content)
				       (let ((val (pop undo-chain)))
							(push val redo-chain)
							val))
				 (clrhash (get-control-list app panel-id))
				 (on-populate-loaded-window content :win win)
				 (setf (window-title win) (attribute content "data-clog-name"))
				 (on-populate-control-properties-win content :win win)
				 (on-populate-control-list-win content :win win))))
      (set-on-event content "clog-builder-snap-shot"
		    (lambda (obj)
		      (declare (ignore obj))
		      (setf redo-chain nil)
		      (push (panel-snap-shot content panel-id (bottom-panel box)) undo-chain)
		      (when (current-control app)
			(focus (get-placer (current-control app))))))
      (set-on-click btn-redo (lambda (obj)
			       (declare (ignore obj))
			       (when redo-chain
				 (setf (inner-html content)
				       (let ((val (pop redo-chain)))
							(push val undo-chain)
							val))
				 (clrhash (get-control-list app panel-id))
				 (on-populate-loaded-window content :win win)
				 (setf (window-title win) (attribute content "data-clog-name"))
				 (on-populate-control-properties-win content :win win)
				 (on-populate-control-list-win content :win win))))
      (set-on-click btn-load (lambda (obj)
                               (declare (ignore obj))
                               (server-file-dialog win "Load Panel" (directory-namestring file-name)
                                                   (lambda (fname)
                                                     (window-focus win)
                                                     (when fname
                                                       (setf file-name fname)
						       (setf render-file-name "")
                                                       (setf (inner-html content)
                                                             (escape-string (read-file fname)))
                                                       (clrhash (get-control-list app panel-id))
                                                       (on-populate-loaded-window content :win win)
                                                       (setf (title (html-document body)) (attribute content "data-clog-name"))
                                                       (setf (window-title win) (attribute content "data-clog-name"))
						       (on-populate-control-list-win content :win win))))))
      (set-on-click btn-save (lambda (obj)
			       (when (equal file-name "")
				 (setf file-name (format nil "~A.clog" (attribute content "data-clog-name"))))
                               (server-file-dialog obj "Save Page As.." file-name
                                                   (lambda (fname)
                                                     (window-focus win)
                                                     (when fname
                                                       (setf file-name fname)
                                                       (save-panel fname content panel-id (bottom-panel box)))
                                                     :initial-filename file-name))))
      (set-on-click btn-test
                    (lambda (obj)
                      (do-eval obj (render-clog-code content (bottom-panel box))
                        (attribute content "data-clog-name")
                        :package (attribute content "data-in-package")
                        :custom-boot custom-boot)))
      (set-on-click btn-rndr
                    (lambda (obj)
		      (when (equal render-file-name "")
			(if (equal file-name "")
			    (setf render-file-name (format nil "~A.lisp" (attribute content "data-clog-name")))
			    (setf render-file-name (format nil "~A~A.lisp"
							   (directory-namestring file-name)
							   (pathname-name file-name)))))
                      (server-file-dialog obj "Render As.." render-file-name
                                          (lambda (fname)
                                            (window-focus win)
                                            (when fname
                                              (setf render-file-name fname)
                                              (write-file (render-clog-code content (bottom-panel box))
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
         (win (create-gui-window obj :top 40 :left 225 :width 600 :client-movement t))
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
         content)
         (declare (ignore page-link txt-link))
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
                                  :content (format nil "<div class='w3-black'>
                                         <center><img src='~A'></center>
                                         <center>CLOG</center>
                                         <center>The Common Lisp Omnificent GUI</center></div>
                                         <div><p><center>
                                           <a target=_blank href='https://github.com/sponsors/rabbibotton'>CLOG Builder</a>
                                           </center>
                                         <center>(c) 2022 - David Botton</center></p></div>"
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

(defun on-image-to-data (obj)
  "Menu option to create new project from template"
  (let* ((win (create-gui-window obj :title "Convert Images to Data"
                                     :width 450 :height 200)))
    (create-image-to-data (window-content win))
    (window-center win)))

(defun on-quick-start (obj)
  "Open quick start"
  (let* ((app (connection-data-item obj "builder-app-data"))
         (win (create-gui-window obj :title "Quick Start"
				     :top 40 :left 225
				     :width 600 :height 400
				     :client-movement t)))
    (create-quick-start (window-content win))))

(defun on-new-builder (body)
  "Launch instance of the CLOG Builder"
  (set-html-on-close body "Connection Lost")
  (let ((app (make-instance 'builder-app-data)))
    (setf (connection-data-item body "builder-app-data") app)
    (setf (title (html-document body)) "CLOG Builder")
    (clog-gui-initialize body :body-left-offset 10 :body-right-offset 10)
    (add-class body "w3-blue-grey")
    (setf (z-index (create-panel body :positioning :fixed
                                      :bottom 0 :left 222
                                      :content (format nil "static-root: ~A" clog::*static-root*)))
          -9999)
    (let* ((menu  (create-gui-menu-bar body))
           (icon  (create-gui-menu-icon menu :image-url img-clog-icon
					     :on-click  #'on-help-about-builder))
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
      (create-gui-menu-item file  :content "Launch DB Admin"           :on-click
                            (lambda (obj)
                              (declare (ignore obj))
                              (open-window (window body) "/dbadmin")))
      (create-gui-menu-item tools :content "Control Events"     :on-click 'on-show-control-events-win)
      (create-gui-menu-item tools :content "Copy/Cut History"   :on-click 'on-show-copy-history-win)
      (create-gui-menu-item tools :content "Image to Data"      :on-click 'on-image-to-data)
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
    (on-show-control-properties-win body)
    (on-show-control-list-win body)
    (on-show-control-events-win body)
    (on-show-copy-history-win body)
    (on-new-builder-panel body)
    (set-on-before-unload (window body) (lambda(obj)
                                          (declare (ignore obj))
                                          ;; return empty string to prevent nav off page
                                          ""))))
(defun on-convert-image (body)
  (let ((params (form-multipart-data body)))
    (create-div body :content params)
    (destructuring-bind (stream fname content-type)
	(form-data-item params "filename")
      (create-div body :content (format nil "filename = ~A - (contents printed in REPL)" fname))
      (let ((s (flexi-streams:make-flexi-stream stream))
	    (b (make-string 1000))
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

(defun clog-builder (&key (port 8080) static-root system)
  "Start clog-builder."
  (if system
      (setf static-root (merge-pathnames "./www/"
					 (asdf:system-source-directory system))))
  (if static-root
      (initialize nil :port port :static-root static-root)
      (initialize nil :port port))
  (set-on-new-window 'on-new-builder :path "/builder")
  (set-on-new-window 'on-new-db-admin :path "/dbadmin")
  (set-on-new-window 'on-attach-builder-page :path "/builder-page")
  (set-on-new-window 'on-convert-image :path "/image-to-data")
  (open-browser :url (format nil "http://127.0.0.1:~A/builder" port)))
