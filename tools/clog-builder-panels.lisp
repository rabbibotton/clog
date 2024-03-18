(in-package :clog-tools)

;; Cross page syncing

(defvar *app-sync-hash* (make-hash-table* :test #'equal)
  "Exchange app instance with new external pages")

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

;; Snap-shots

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
  (write-file (panel-snap-shot content panel-id hide-loc) fname :clog-obj content))

;; Add controls to panels

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
                                  ((eq create-type :custom-block)
                                   (let ((c (funcall (getf control-record :create) parent
                                                     :content custom-query
                                                     :html-id uid)))
                                     (setf (attribute c "data-original-html") custom-query)
                                     c))
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
    (cond ((eq control-type-name :custom-query)
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
                         :default-value (getf control-record :create-content)))
          ((eq control-type-name :custom-block)
           (input-dialog win "Enter html to create control:"
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
                         :title "Custom HTML Block"
                         :default-value (getf control-record :create-content)))
          (t
           (do-drop-new-control app content data :win win)))))

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
           (when (equalp (attribute control "data-clog-composite-control") "undefined")
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
	(touch-x  0)
	(touch-y  0)
        (placer   (create-div control :auto-place nil
                                      :class "placer"
                                      :html-id (format nil "p-~A" (html-id control)))))
    (add-to-control-list app panel-id control)
    (setf (attribute placer "data-panel-id") panel-id)
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
                                    (set-geometry control :height (+ (height control) 2))
                                    (set-geometry control :top (+ (position-top control) 2))))
                               ((equal key "ArrowRight")
                                (if shift
                                    (set-geometry control :width (+ (width control) 2))
                                    (set-geometry control :left (+ (position-left control) 2))))
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
    (set-on-touch-start placer (lambda (obj data)
				 (declare (ignore obj))
				 (setf touch-x (getf data :X))
				 (setf touch-y (getf data :Y))))
    (set-on-touch-move placer (lambda (obj data)
				(declare (ignore obj))
				(set-geometry control :top (+ (position-top control)
							      (- (getf data :y) touch-y))
						      :left (+ (position-left control)
							       (- (getf data :x) touch-x)))
				(setf touch-x (getf data :X))
				(setf touch-y (getf data :Y))))
    (set-on-touch-end placer (lambda (obj data)
			       (declare (ignore obj data))
			       (set-geometry placer :units ""
						    :top (top control)
						    :left (left control))
			       (select-control control)
			       (jquery-execute placer "trigger('clog-builder-snap-shot')")
			       (set-properties-after-geomentry-change control)))
    (set-on-mouse-up placer (lambda (obj data)
			      (declare (ignore obj data))
			      (set-geometry control :units ""
						    :top (top placer)
						    :left (left placer))
			      (set-geometry placer :units ""
						   :top (top control)
						   :left (left control))
			      (select-control control)
			      (jquery-execute placer "trigger('clog-builder-snap-shot')")
			      (set-properties-after-geomentry-change control)))
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
				  (select-control control)
				  (on-populate-control-properties-win content :win win)
				  (on-populate-control-list-win content :win win))
                                 (t
                                  (select-control control)))
                           (when win
                             (window-focus win))))
		       :cancel-event t)
    (set-on-mouse-double-click placer
                               (lambda (obj data)
                                 (declare (ignore obj data))
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
		    (set-properties-after-geomentry-change control)))))

(defun on-populate-loaded-window (content &key win)
  "Setup html imported in to CONTENT for use with Builder"
  (with-sync-event (content)
    (add-sub-controls content content :win win)))

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
  "Get placer for CONTROL. A placer is a div placed on top of CONTROL and
prevents access to use or activate the control directy and allows
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
not a temporarily attached one when using select-control."
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
                     (unless (equal dct "block")
                       (add-siblings (first-child control))))
                   (setf control (next-sibling control))))))
      (add-siblings (first-child parent)))))
