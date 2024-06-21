(in-package :clog-tools)

;; Control-List utilities

(defun init-control-list (app panel-id)
  "Initialize new control list for PANEL-ID on instance of APP."
  (setf (gethash panel-id (control-lists app)) (make-hash-table :test #'equalp)))

(defun destroy-control-list (app panel-id)
  "Destroy the control-list on PANEL-ID"
  (remhash panel-id (control-lists app)))

(defun get-control-list (app panel-id)
  "Rerieve the control-list hash table on PANEL-ID"
  (let ((h (gethash panel-id (control-lists app))))
    (if h
        h
        (make-hash-table* :test #'equalp)))) ;; return empty hash to avoid map fails

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

;; Contorl List Window

(defun on-show-control-list-win (obj)
  "Show control list for selecting and manipulating controls by name"
  (let* ((app          (connection-data-item obj "builder-app-data")))
    (unless (controls-win app)
      (let* ((*default-title-class*      *builder-title-class*)
             (*default-border-class*     *builder-border-class*)
             (win (create-gui-window obj :title "Controls"
                                     :has-pinner t
                                     :keep-on-top t
                                     :width 220))
             (content (window-content win))
             (sheight      (floor (/ (height content) 2)))
             (swidth       (floor (width content)))
             (divider      (create-panel content :top sheight :height 10 :left 0 :right 10
                                                 :class *builder-title-class*))
             (control-list (create-panel content :height (- sheight 10) :left 0 :bottom 0 :right 10))
             (pallete      (create-select content :class *builder-pallete-class*))
             (adj-size     0))
        (add-class content *builder-pallete-class*)
        (setf (controls-win app) win)
        (setf (control-list-win app) control-list)
        (setf (select-tool app) pallete)
        (set-on-window-close win (lambda (obj)
                                   (setf (controls-win app) nil)
                                   (setf (select-tool app) nil)
                                   (setf (control-list-win app) nil)))
        (reset-control-pallete pallete)
        (window-toggle-pinned win :state nil)
        (set-geometry win :top (menu-bar-height win) :left 0 :height "" :bottom 5 :right "")
        (set-geometry pallete :left 0 :top 0 :height sheight :right 0);:width (- swidth 10))
        (setf (tab-index divider) "-1")
        (setf (cursor divider) :ns-resize)
        (setf (positioning pallete) :absolute)
        (setf (size pallete) 2)
        (setf (advisory-title pallete) (format nil "<ctrl/cmd> place static~%<shift> child to current selection"))
        (setf (overflow control-list) :auto)
        (reset-control-pallete obj)
        (setf (advisory-title content)
              (format nil "Drag and drop order~%Double click non-focusable~%~
                             <ctrl/cmd> place as static~%<shift> child to current selection"))
        (flet ((on-size (obj)
                 (declare (ignore obj))
                 (setf sheight (floor (/ (height content) 2)))
                 (when (and (> (- sheight adj-size) 5)
                            (> (+ (- sheight 10) adj-size) 5))
                   (set-geometry pallete :height (- sheight adj-size))
                   (set-geometry divider :top (- sheight adj-size))
                   (set-geometry control-list :height (+ (- sheight 10) adj-size)))))
          (set-on-resize (window (connection-body obj)) #'on-size)
          (set-on-window-size win #'on-size)
          (set-on-window-move win (lambda (obj)
                                    (setf (height obj) (height obj))
                                    (on-size obj)))
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
                               :capture-pointer t)
          (on-size win))))
    (window-focus (controls-win app))))

(defun on-populate-control-list-win (content &key win clear)
  "Populate the control-list-window to allow drag and drop adjust of order
of controls and double click to select control."
  (when content
    (let ((app (connection-data-item content "builder-app-data")))
      (if clear
          (when (control-list-win app)
            (setf (inner-html (control-list-win app)) "")
            (browser-gc content))
          (with-sync-event (content)
            (let ((panel-id (html-id content))
                  (last-ctl nil))
              (when (control-list-win app)
                (let ((lwin (control-list-win app)))
                  (setf (inner-html lwin) "")
                  (browser-gc content)
                  (set-on-mouse-click (create-div lwin :content (attribute content "data-clog-name"))
                                      (lambda (obj data)
                                        (declare (ignore obj data))
                                        (deselect-current-control app)
                                        (on-populate-control-properties-win content :win win)
                                        (on-populate-control-list-win content :win win)))
                  (labels ((add-siblings (control sim)
                             (let (dln dcc)
                               (loop
                                 (when (equalp (html-id control) "undefined")
                                   (return))
                                 (setf dln (attribute control "data-clog-name"))
                                 (unless (or (equal dln "undefined")
                                             (eq dln nil))
                                  (setf dcc (attribute control "data-clog-composite-control"))
                                   (let ((list-item (create-div lwin :content (format nil "&#8597; ~A~A" sim dln)))
                                         (status    (hiddenp (get-placer control))))
                                     (if status
                                         (setf (color list-item) :darkred)
                                         (setf (css-class-name list-item) *builder-pallete-class*))
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
                                       (add-siblings (first-child control :no-attach t) (format nil "~A&#8594;" sim)))))
                                 (setf control (next-sibling control :no-attach t))))))
                    (add-siblings (first-child content :no-attach t) ""))))))))))
