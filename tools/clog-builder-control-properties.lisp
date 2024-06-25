(in-package :clog-tools)

(defun on-show-control-properties-win (obj)
  "Show control properties window"
  (let ((app (connection-data-item obj "builder-app-data")))
    (unless (control-properties-win app)
      (let* ((*default-title-class*      *builder-title-class*)
             (*default-border-class*     *builder-border-class*)
             (win (create-gui-window obj :title "Properties"
                                     :has-pinner t
                                     :keep-on-top t
                                     :width 400))
             (content (window-content win))
             (control-list (create-table content)))
        (add-class content *builder-pallete-class*)
        (set-on-window-close win (lambda (obj)
                                   (setf (control-properties-win app) nil)))
        (set-on-window-move win (lambda (obj)
                                  (setf (height obj) (height obj))))
        (window-toggle-pinned win :state nil)
        (setf (control-properties-win app) win)
        (setf (properties-list app) control-list)
        (setf (overflow content) :auto)
        (setf (positioning control-list) :absolute)
        (set-geometry win :top (menu-bar-height win) :left "" :height "" :bottom 5 :right 0)
        (set-geometry control-list :left 0 :top 0 :right 0)))
    (window-focus (control-properties-win app))))

(defun on-populate-control-properties-win (obj &key win clear)
  "Populate the control properties for the current control"
  ;; obj if current-control is nil must be content
  (when obj
    (let ((app (connection-data-item obj "builder-app-data")))
      (if clear
          (progn
            (setf (inner-html (properties-list app)) "")
            (browser-gc obj))
          (with-sync-event (obj)
            (bordeaux-threads:make-thread (lambda () (on-populate-control-events-win obj)))
            (let* ((prop-win (control-properties-win app))
                   (control  (if (current-control app)
                                 (current-control app)
                                 obj))
                   (placer   (when control
                               (get-placer control)))
                   (panel-id (attribute placer "data-panel-id"))
                   (panel    (if (current-control app)
                                 (attach-as-child obj panel-id)
                                 obj))
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
                  (when (equal (getf info :name) "clog-data")
                    (push
                      `("panel name"    ,(attribute control "data-clog-name")
                                        nil
                                        ,(lambda (obj)
                                           (let ((vname (text obj)))
                                             (unless (equal vname "")
                                               (when (equal (subseq vname 0 1) "(")
                                                 (setf vname (format nil "|~A|" vname)))
                                               (setf (attribute control "data-clog-name") vname)
                                               (on-populate-control-list-win panel :win win)
                                               (when win
                                                 (setf (window-title win) vname))))))
                      props))
                  (when (current-control app)
                    (let* (panel-controls
                           (cname    (attribute control "data-clog-name"))
                           (ctype    (attribute control "data-clog-type")))
                      (maphash (lambda (k v)
                                 (declare (ignore k))
                                 (let ((n (attribute v "data-clog-name"))
                                       (p (attribute (parent-element v) "data-clog-name")))
                                   (unless (or (equal cname n)
                                               (equal cname p))
                                     (push n panel-controls))))
                               (get-control-list app panel-id))
                      (push (attribute panel "data-clog-name") panel-controls)
                      (push
                       `("parent"  nil
                                   ,(lambda (control td1 td2)
                                      (declare (ignore td1))
                                      (let ((dd (create-select td2))
                                            (v  (attribute (parent-element control) "data-clog-name")))
                                        (set-geometry dd :width "100%")
                                        (add-select-options dd panel-controls)
                                        (setf (value dd) v)
                                        (set-on-change dd
                                                       (lambda (obj)
                                                         (place-inside-bottom-of
                                                          (attach-as-child control
                                                                           (js-query
                                                                            control
                                                                            (format nil "$(\"[data-clog-name='~A']\").attr('id')"
                                                                                    (value obj))))
                                                          control)
                                                         (place-after control placer)
                                                         (on-populate-control-list-win panel :win win))))
                                      nil)
                                   nil)
                       props)
                      (push
                       `("type"  ,ctype
                                 :read-only
                                 nil
                                 nil)
                       props)
                      (push
                       `("name"    ,cname
                                   nil
                                   ,(lambda (obj)
                                      (let ((vname (text obj)))
                                        (unless (equal vname "")
                                          (when (equal (subseq vname 0 1) "(")
                                            (setf vname (format nil "|~A|" vname)))
                                          (setf (attribute control "data-clog-name") vname)
                                          (on-populate-control-list-win panel :win win)
                                          (when (equal (getf info :name) "clog-data")
                                            (when win
                                              (setf (window-title win) vname)))))))
                       props)))
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
                      (when (fourth item)
                        (set-on-blur td2
                                     (lambda (obj)
                                       (funcall (fourth item) obj)
                                       (when placer
                                         (jquery-execute placer "trigger('clog-builder-snap-shot')")
                                         (set-geometry placer :top (position-top control)
                                                              :left (position-left control)
                                                              :width (client-width control)
                                                              :height (client-height control))))))))))))))))
