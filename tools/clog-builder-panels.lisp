(in-package :clog-tools)

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
  (when (select-tool app)
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
              (do-drop-new-control app content data :win win))))))

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
        (placer   (create-div control
                              :class "placer"
                              :style "position:absolute;box-sizing:content-box;tabindex:0"
                              :html-id (format nil "p-~A" (html-id control)))))
    (add-to-control-list app panel-id control)
    (setf (attribute placer "data-panel-id") panel-id)
    ;; setup placer
    (set-geometry placer :top (position-top control)
                  :left (position-left control)
                  :width (client-width control)
                  :height (client-height control))
    (place-after control placer)
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
                           (if (and (select-tool app)
                                (not (equal (value (select-tool app)) "")))
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
    (clog::make-clog-element (clog::connection-id control)
                             (format nil "p-~A" (html-id control))
                             :clog-type 'clog-element)))

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
    (let ((dom (list-of-children parent))
          dct)
      (labels ((tr (control)
                 (unless (equal (html-id control) "undefined")
                   (setf dct (attribute control "data-clog-type"))
                   (unless (equal dct "undefined")
                     (change-class control (getf (control-info dct) :clog-type)))
                   (when (getf (control-info dct) :on-load)
                     (funcall (getf (control-info dct) :on-load) control (control-info dct)))
                   (setup-control content control :win win)))
               (ll (lst)
                   (mapcar (lambda (l)
                             (if (listp l)
                                 (if (listp (first l))
                                     (tr (first l))
                                     (ll l))
                                 (tr l)))
                           lst)))
        (ll dom)))))

;; Panel Windows

(defun on-new-builder-panel-ext (obj &key open-file popup open-ext)
  (open-window (window (connection-body obj))
               (if open-file
                   (format nil "/panel-editor?open-panel=~A~A"
                           open-file (if open-ext
                                         (format nil "&open-ext=~A" open-ext)
                                         ""))
                   (format nil "/panel-editor?open-panel=%20~A"
                           (if open-ext
                               (format nil "&open-ext=~A" open-ext)
                               "")))
               :specs (if (or popup *open-external-panels-in-popup*)
                          "width=1280,height=700"
                          "")
               :name "_blank"))

(defun on-new-builder-panel (obj &key (open-file nil) open-ext)
  "Open new panel"
  (unless (and open-file
               (window-to-top-by-param obj open-file))
    (let* ((app (connection-data-item obj "builder-app-data"))
           (*menu-bar-class*           *builder-menu-bar-class*)
           (*menu-bar-drop-down-class* *builder-menu-bar-drop-down-class*)
           (*menu-item-class*          *builder-menu-item-class*)
           (*menu-window-select-class* *builder-menu-window-select-class*)
           (*default-title-class*      *builder-title-class*)
           (*default-border-class*     *builder-border-class*)
           ext-panel
           (win (create-gui-window obj :top 40 :left 225
                                   :width 645 :height 430
                                   :client-movement *client-side-movement*))
           (box (create-panel-box-layout (window-content win)
                                         :left-width 0 :right-width 0
                                         :top-height 70 :bottom-height 0))
           (menu     (create-gui-menu-bar (top-panel box) :main-menu nil))
           (m-file   (create-gui-menu-drop-down menu :content "File"))
           (m-load   (create-gui-menu-item m-file :content "load"))
           (m-save   (create-gui-menu-item m-file :content "save (cmd/ctrl-s)"))
           (m-saveas (create-gui-menu-item m-file :content "save as.."))
           (m-reopnp (create-gui-menu-item m-file :content "save, close and reopen as panel"))
           (m-reopn  (create-gui-menu-item m-file :content "save, close and popup this panel"))
           (m-reopnb (create-gui-menu-item m-file :content "save, close and popup this panel custom boot file"))
           (m-reopnh (create-gui-menu-item m-file :content "save, close and popup this panel no css"))
           (m-reopns (create-gui-menu-item m-file :content "save, close and reopen as text source"))
           (m-edit   (create-gui-menu-drop-down menu :content "Edit"))
           ;;(m-undo   (create-gui-menu-item m-edit :content "undo"))
           ;;(m-redo   (create-gui-menu-item m-edit :content "redo"))
           (m-copy   (create-gui-menu-item m-edit :content "copy"))
           (m-paste  (create-gui-menu-item m-edit :content "paste"))
           (m-cut    (create-gui-menu-item m-edit :content "cut"))
           (m-del    (create-gui-menu-item m-edit :content "delete"))
           (m-lisp   (create-gui-menu-drop-down menu :content "Lisp"))
           (m-rndr   (create-gui-menu-item m-lisp :content "render form to lisp"))
           (m-rndras (create-gui-menu-item m-lisp :content "render form to lisp as..."))
           (m-rndr   (create-gui-menu-item m-lisp :content "render form to lisp"))
           (m-eval   (create-gui-menu-item m-lisp :content "evaluate"))
           (m-test   (create-gui-menu-item m-lisp :content "evaluate and test"))
           (m-events (create-gui-menu-drop-down menu :content "controls"))
           (tmp      (create-gui-menu-item m-events :content "show control properties"  :on-click 'on-show-control-properties-win))
           (tmp      (create-gui-menu-item m-events :content "show controls window"     :on-click 'on-show-control-list-win))
           (tmp      (create-gui-menu-item m-events :content "show CLOG events"         :on-click 'on-show-control-events-win))
           (tmp      (create-gui-menu-item m-events :content "show JavaScript events"   :on-click 'on-show-control-js-events-win))
           (tmp      (create-gui-menu-item m-events :content "show ParenScript events"  :on-click 'on-show-control-ps-events-win))
           (m-help   (create-gui-menu-drop-down menu :content "Help"))
           (m-helpk  (create-gui-menu-item m-help :content "quick start"))
           (tool-bar  (create-div (top-panel box) :class *builder-title-class*))
           (btn-class *builder-icons-class*)
           (btn-copy  (create-img tool-bar :alt-text "copy"     :url-src img-btn-copy  :class btn-class))
           (btn-paste (create-img tool-bar :alt-text "paste"    :url-src img-btn-paste :class btn-class))
           (btn-cut   (create-img tool-bar :alt-text "cut"      :url-src img-btn-cut   :class btn-class))
           (btn-del   (create-img tool-bar :alt-text "delete"   :url-src img-btn-del   :class btn-class))
           ;;(btn-undo  (create-img tool-bar :alt-text "undo"     :url-src img-btn-undo  :class btn-class))
           ;;(btn-redo  (create-img tool-bar :alt-text "redo"     :url-src img-btn-redo  :class btn-class))
           (btn-test  (create-img tool-bar :alt-text "test"     :url-src img-btn-test  :class btn-class))
           (btn-rndr  (create-img tool-bar :alt-text "render"   :url-src img-btn-rndr  :class btn-class))
           (btn-save  (create-img tool-bar :alt-text "save"     :url-src img-btn-save  :class btn-class))
           (btn-load  (create-img tool-bar :alt-text "load"     :url-src img-btn-load  :class btn-class))
           (cbox      (create-form-element tool-bar :checkbox :class "w3-margin-left"))
           (cbox-lbl  (create-label tool-bar :content "&nbsp;auto render" :label-for cbox))
           (spacer    (create-span tool-bar :content "&nbsp;&nbsp;&nbsp;"))
           (btn-help  (create-span tool-bar :content "?" :class "w3-tiny w3-ripple"))
           (content   (center-panel box))
           (in-simulation    nil)
           (undo-chain       nil)
           (redo-chain       nil)
           (is-dirty         nil)
           (last-date        nil)
           (file-name        "")
           (render-file-name "")
           (panel-id  (html-id content)))
      (declare (ignore spacer))
      (add-class menu "w3-small")
      (setf (overflow (top-panel box)) :visible) ; let menus leave the top panel
      (add-class (top-panel box) *builder-title-class*)
      (setf (background-color (top-panel box)) :black)
      (setf (checkedp cbox) t)
      (setf (advisory-title btn-copy) "copy")
      (setf (advisory-title btn-paste) "paste")
      (setf (advisory-title btn-cut) "cut")
      (setf (advisory-title btn-del) "delete")
      ;;(setf (advisory-title btn-undo) "undo")
      ;;(setf (advisory-title btn-redo) "redo")
      (setf (advisory-title btn-test) "test")
      (setf (advisory-title btn-rndr) "render to lisp - shift-click render as...")
      (setf (advisory-title btn-save) "save - shift-click save as...")
      (setf (advisory-title btn-load) "load")
      (setf (advisory-title cbox) "when checked render on save")
      (setf (advisory-title cbox-lbl) "when checked render on save")
      (setf (height btn-copy) "12px")
      (setf (height btn-paste) "12px")
      (setf (height btn-cut) "12px")
      (setf (height btn-del) "12px")
      ;;(setf (height btn-undo) "12px")
      ;;(setf (height btn-redo) "12px")
      (setf (height btn-test) "12px")
      (setf (height btn-rndr) "12px")
      (setf (height btn-save) "12px")
      (setf (height btn-load) "12px")
      (setf (height btn-help) "12px")
      (when (or open-ext
                *open-panels-as-popups*)
        (multiple-value-bind (pop pop-win)
                             (if (typep open-ext 'string)
                                 (progn
                                   (enable-clog-popup :path "/customboot" :boot-file open-ext)
                                   (open-clog-popup obj :path "/customboot"
                                                    :specs "width=640,height=480"))
                                 (open-clog-popup obj :specs "width=640,height=480"))
          (when pop
            (let ((msg (create-button content :content "Panel is external. Click to bring to front.")))
              (set-geometry msg :units "%" :height 100 :width 100)
              (set-on-click content
                            (lambda (obj)
                              (declare (ignore obj))
                              (focus pop-win))))
            (setf ext-panel pop)
            (cond ((eq open-ext :custom)
                    (load-css (html-document pop) "/css/jquery-ui.css")
                    (load-script (html-document pop) "/js/jquery-ui.js"))
                  (t
                    (clog-gui-initialize pop)
                    (clog-web-initialize pop :w3-css-url nil)))
            (setf (connection-data-item pop "builder-app-data") app)
            (let ((nbox (create-panel-box-layout pop
                                                 :left-width 0 :right-width 0
                                                 :top-height 0 :bottom-height 0)))
              (setf box nbox)
              (setf content (center-panel nbox))
              (setf panel-id (html-id content))
              (set-on-focus (window pop)
                            (lambda (obj)
                              (declare (ignore obj))
                              (setf (title (html-document pop)) (attribute content "data-clog-name"))))
              (set-on-before-unload (window pop)
                                    (lambda (obj)
                                      (declare (ignore obj))
                                      (deselect-current-control app)
                                      (on-populate-control-events-win content)
                                      (on-populate-control-list-win content :win win :clear t)
                                      (on-populate-control-properties-win content :win win :clear t)
                                      (setf content nil)
                                      (setf ext-panel nil)
                                      (Window-close win)))
              (set-on-click (create-gui-menu-item m-file :content "export as a boot html")
                            (lambda (obj)
                              (server-file-dialog obj "Export as a Boot HTML" "./"
                                                  (lambda (filename)
                                                    (when filename
                                                      (maphash
                                                        (lambda (html-id control)
                                                          (declare (ignore html-id))
                                                          (place-inside-bottom-of (bottom-panel box)
                                                                                  (get-placer control)))
                                                        (get-control-list app panel-id))
                                                      ;; needs to clear data attrs
                                                      (save-body-to-file filename :body pop :if-exists :rename)
                                                      (maphash
                                                        (lambda (html-id control)
                                                          (declare (ignore html-id))
                                                          (place-after control (get-placer control)))
                                                        (get-control-list app panel-id)))))))
              (focus pop-win)))))
      (setf-next-id content 1)
      (setf (css-class-name content) *builder-panel-class*)
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
      (on-show-control-events-win win)
      (on-show-control-properties-win win)
      (on-show-control-list-win win)
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
                             (deselect-current-control app)
                             (on-populate-control-events-win content)
                             (on-populate-control-list-win content :win win :clear t)
                             (on-populate-control-properties-win content :win win :clear t)
                             (setf (current-control app) nil)
                             (destroy-control-list app panel-id)
                             (when ext-panel
                               (close-window (window ext-panel)))))
      (set-on-window-size-done win
                               (lambda (obj)
                                 (declare (ignore obj))
                                 (on-populate-control-properties-win content :win win)))
      (set-on-event content "clog-builder-snap-shot"
                    (lambda (obj)
                      (declare (ignore obj))
                      (setf is-dirty t)
                      (setf redo-chain nil)
                      (push (panel-snap-shot content panel-id (bottom-panel box)) undo-chain)
                      (when (current-control app)
                        (focus (get-placer (current-control app))))))
      ;; setup tool bar events
      (set-on-click btn-help 'on-quick-start)
      (set-on-click m-helpk 'on-quick-start)
      (labels (;; copy
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
                                              :class "w3-input")))
                     (place-inside-top-of (window-content (copy-history-win app)) c))
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
                   (jquery-execute (get-placer content) "trigger('clog-builder-snap-shot')")))
               (cut (obj)
                 (copy obj)
                 (del obj))
               (undo (obj)
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
                   (on-populate-control-list-win content :win win)))
               (redo (obj)
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
        ;; set up del/cut/copy/paste handlers
        ;;(set-on-click btn-undo #'undo)
        ;;(set-on-click m-undo #'undo)
        ;;(set-on-click btn-redo #'redo)
        ;;(set-on-click m-redo #'redo)
        (set-on-copy content #'copy)
        (set-on-click btn-copy #'copy)
        (set-on-click m-copy #'copy)
        (set-on-paste content #'paste)
        (set-on-click btn-paste #'paste)
        (set-on-click m-paste #'paste)
        (set-on-click btn-del #'del)
        (set-on-click m-del #'del)
        (set-on-cut content #'cut)
        (set-on-click btn-cut #'cut)
        (set-on-click m-cut #'cut))
      (labels ((open-file-name (fname)
                 (setf file-name fname)
                 (setf last-date (file-write-date fname))
                 (setf render-file-name (format nil "~A~A.lisp"
                                                (directory-namestring file-name)
                                                (pathname-name file-name)))
                 (setf (inner-html content)
                       (or (read-file fname :clog-obj obj)
                           ""))
                 (setf is-dirty nil)
                 (clrhash (get-control-list app panel-id))
                 (on-populate-loaded-window content :win win)
                 (setf (window-title win) (attribute content "data-clog-name"))
                 (when ext-panel
                   (setf (title (html-document ext-panel)) (attribute content "data-clog-name")))
                 (setf (window-param win) fname)
                 (on-populate-control-list-win content :win win))
               (load-file (obj)
                 (server-file-dialog obj "Load Panel" (directory-namestring (if (equal file-name "")
                                                                                (current-project-dir app)
                                                                                file-name))
                                     (lambda (fname)
                                       (window-focus win)
                                       (when fname
                                         (open-file-name fname))))))
        (when open-file
          (open-file-name open-file))
        (set-on-click btn-load #'load-file)
        (set-on-click m-load #'load-file))
      (labels ((do-save (obj fname data)
                 (declare (ignore data))
                 (setf file-name fname)
                 (setf render-file-name (format nil "~A~A.lisp"
                                                (directory-namestring file-name)
                                                (pathname-name file-name)))
                 (add-class btn-save "w3-animate-top")
                 (save-panel fname content panel-id (bottom-panel box))
                 (setf last-date (file-write-date fname))
                 (when (checkedp cbox)
                   (add-class btn-rndr "w3-animate-top")
                   (write-file (render-clog-code content (bottom-panel box))
                               render-file-name :clog-obj obj)
                   (sleep .5)
                   (remove-class btn-rndr "w3-animate-top"))
                 (sleep .5)
                 (remove-class btn-save "w3-animate-top")
                 (cond ((eq is-dirty :close)
                         (setf is-dirty nil)
                         (window-close win))
                       (t
                         (setf is-dirty nil))))
               (save (obj data &key save-as)
                 (cond ((or (equal file-name "")
                            save-as
                            (getf data :shift-key))
                         (when (equal file-name "")
                           (setf file-name (format nil "~A~A.clog"
                                                   (current-project-dir app)
                                                   (attribute content "data-clog-name"))))
                         (server-file-dialog obj "Save Panel As.." file-name
                                             (lambda (fname)
                                               (window-focus win)
                                               (when fname
                                                 (setf file-name fname)
                                                 (do-save obj fname data)))
                                             :initial-filename file-name))
                       (t
                         (if (eql last-date (file-write-date file-name))
                             (do-save obj file-name data)
                             (confirm-dialog obj "Panel changed on file system. Save?"
                                             (lambda (result)
                                               (when result
                                                 (do-save obj file-name data))))))))
               (eval-test (obj &key (test t))
                 (do-eval obj (render-clog-code content (bottom-panel box))
                          (attribute content "data-clog-name")
                          :test test
                          :package (attribute content "data-in-package")))
               (render (obj data &key save-as)
                 (cond ((or (equal render-file-name "")
                            save-as
                            (getf data :shift-key))
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
                                                 (add-class btn-rndr "w3-animate-top")
                                                 (write-file (render-clog-code content (bottom-panel box))
                                                             fname :clog-obj obj)
                                                 (sleep .5)
                                                 (remove-class btn-rndr "w3-animate-top")))
                                             :initial-filename render-file-name))
                       (t
                         (add-class btn-rndr "w3-animate-top")
                         (write-file (render-clog-code content (bottom-panel box))
                                     render-file-name :clog-obj obj)
                         (sleep .5)
                         (remove-class btn-rndr "w3-animate-top")))))
        (set-on-window-can-close win
                                 (lambda (obj)
                                   (cond (is-dirty
                                           (confirm-dialog win "Save panel?"
                                                           (lambda (result)
                                                             (cond (result
                                                                     (setf is-dirty :close)
                                                                     (save obj nil))
                                                                   (t
                                                                     (setf is-dirty nil)
                                                                     (window-close win))))
                                                           :ok-text "Yes" :cancel-text "No")
                                           nil)
                                         (t
                                           t))))
        (set-on-mouse-click btn-save
                            (lambda (obj data)
                              (save obj data)))
        (set-on-click m-save (lambda (obj)
                               (save obj nil)))
        (set-on-click m-saveas (lambda (obj)
                                 (save obj nil :save-as t)))
        (set-on-click m-reopn (lambda (obj)
                                (when is-dirty
                                  (save obj nil))
                                (window-close win)
                                (on-new-builder-panel obj :open-file file-name :open-ext t)))
        (set-on-click m-reopnh (lambda (obj)
                                 (when is-dirty
                                   (save obj nil))
                                 (window-close win)
                                 (on-new-builder-panel obj :open-file file-name :open-ext :custom)))
        (set-on-click m-reopns (lambda (obj)
                                 (when is-dirty
                                   (save obj nil))
                                 (window-close win)
                                 (on-open-file obj :open-file file-name)))
        (set-on-click m-reopnb (lambda (obj)
                                 (input-dialog obj "Boot file Name?"
                                               (lambda (file)
                                                 (when file
                                                   (when is-dirty
                                                     (save obj nil))
                                                   (window-close win)
                                                   (on-new-builder-panel obj :open-file file-name :open-ext file))))))
        (set-on-click m-reopnp (lambda (obj)
                                 (when is-dirty
                                   (save obj nil))
                                 (window-close win)
                                 (on-new-builder-panel obj :open-file file-name)))
        (set-on-click m-eval (lambda (obj)
                               (eval-test obj :test nil)))
        (set-on-click btn-test #'eval-test)
        (set-on-click m-test #'eval-test)
        (set-on-mouse-click btn-rndr (lambda (obj data) (render obj data)))
        (set-on-click m-rndr (lambda (obj) (render obj nil)))
        (set-on-click m-rndras (lambda (obj) (render obj nil :save-as t))))
      (set-on-mouse-down content
                         (lambda (obj data)
                           (declare (ignore obj))
                           (when (drop-new-control app content data :win win)
                             (setf is-dirty t)
                             (incf-next-id content)))))))

(defun on-new-builder-page (obj)
  "Open new page"
  (if *open-external*
      (on-new-builder-panel-ext obj :open-ext t)
      (on-new-builder-panel obj :open-ext t)))

(defun on-new-builder-basic-page (obj)
  "Menu item to open new basic HTML page"
  (if *open-external*
      (on-new-builder-panel-ext obj :open-ext :custom)
      (on-new-builder-panel obj :open-ext :custom)))

(defun on-new-builder-custom-page (obj)
  (input-dialog obj "Boot file Name?"
                (lambda (file)
                  (when file
                    (if *open-external*
                        (on-new-builder-panel-ext obj :open-ext file)
                        (on-new-builder-panel obj :open-ext file))))))

(defun on-quick-start (obj)
  "Open quick start help"
  (let* ((*default-title-class*      *builder-title-class*)
         (*default-border-class*     *builder-border-class*)
         (win (create-gui-window obj :title "Quick Start"
                                 :width 600 :height 400
                                 :client-movement *client-side-movement*)))
    (create-quick-start (window-content win))))
