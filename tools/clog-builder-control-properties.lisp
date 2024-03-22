(in-package :clog-tools)

(defun on-show-control-properties-win (obj)
  "Show control properties window"
  (let* ((app (connection-data-item obj "builder-app-data"))
         (is-hidden  nil)
         (auto-mode  nil)
         (panel  (create-panel (connection-body obj) :positioning :fixed
                                                     :width 400
                                                     :top 40
                                                     :right 0 :bottom 0
                                                     :class "w3-border-left"))
         (content (create-panel panel :width 390 :top 0 :right 0 :bottom 0))
         (side-panel (create-panel panel :top 0 :left 0 :bottom 0 :width 10))
         (pin        (create-div side-panel :content "☑" :class "w3-small"))
         (control-list (create-table content)))
    (setf (background-color side-panel) :black)
    (setf (background-color content) :gray)
    (setf (right-panel app) panel)
    (setf (hiddenp (right-panel app)) t)
    (setf (control-properties-win app) content)
    (setf (properties-list app) control-list)
    (set-on-click side-panel (lambda (obj)
                               (declare (ignore obj))
                               (cond (auto-mode
                                      (setf auto-mode nil)
                                      (setf (text-value pin) "☑")
                                      (setf (width panel) "400px")
                                      (setf is-hidden nil))
                                     (t
                                      (setf auto-mode t)
                                      (setf (text-value pin) "☐")
                                      (setf (width panel) "400px")
                                      (setf is-hidden nil)))))
    (set-on-mouse-leave side-panel (lambda (obj)
                                     (declare (ignore obj))
                                     (when auto-mode
                                       (cond (is-hidden
                                              (setf (width panel) "400px")
                                              (setf is-hidden nil))
                                             (t
                                              (setf (width panel) "10px")
                                              (setf is-hidden t))))))
    (setf (overflow content) :auto)
    (setf (positioning control-list) :absolute)
    (set-geometry control-list :left 0 :top 0 :right 0)))

(defun on-populate-control-properties-win (obj &key win)
  "Populate the control properties for the current control"
  ;; obj if current-control is nil must be content
  (with-sync-event (obj)
    (bordeaux-threads:make-thread (lambda () (on-populate-control-events-win obj)))
    (let ((app (connection-data-item obj "builder-app-data")))
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
              (let* (panel-controls
                     (cname    (attribute control "data-clog-name"))
                     (ctype    (attribute control "data-clog-type"))
                     (panel-id (attribute placer "data-panel-id"))
                     (panel    (attach-as-child obj panel-id)))
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
                                                        :height (client-height control))))))))))))))
