;;;; CLOG Builder generated code - modify original .clog file and rerender
(in-package :clog-tools)
(defclass project-dir (clog:clog-panel)
          ((other-button :reader other-button)
           (select-button :reader select-button)
           (project-list :reader project-list) (frame :reader frame)
           (on-done :accessor on-done :initform nil)))
(defun create-project-dir
       (clog-obj &key hidden class style html-id (auto-place t))
  (let ((panel
         (change-class
          (clog:create-div clog-obj :content
                           "<div style=\"display: flex; flex-direction: column; box-sizing: content-box; position: absolute; inset: 10px; justify-content: normal; align-items: center;\"
    id=\"CLOGB3930207069\" data-clog-name=\"frame\">
    <select size=\"4\"
        style=\"box-sizing: content-box; position: static; width: 90%; height: 80%;\"
        id=\"CLOGB3930207070\" data-clog-name=\"project-list\"></select><button
        style=\"box-sizing: content-box; position: static; width: 220px;\"
        class=\"w3-margin\" id=\"CLOGB3930207071\"
        data-clog-name=\"select-button\">Select
        Project Directory</button><button
        style=\"box-sizing: content-box; position: static; left: 31px; top: 3px; width: 220px; height: 22px;\"
        class=\"\" id=\"CLOGB3930207072\" data-clog-name=\"other-button\">Add
        Alternative Directory</button>
</div>"
                           :hidden hidden :class class :style style :html-id
                           html-id :auto-place auto-place)
          'project-dir)))
    (setf (slot-value panel 'other-button)
            (attach-as-child clog-obj "CLOGB3930207072" :clog-type
             'clog:clog-button :new-id t))
    (setf (slot-value panel 'select-button)
            (attach-as-child clog-obj "CLOGB3930207071" :clog-type
             'clog:clog-button :new-id t))
    (setf (slot-value panel 'project-list)
            (attach-as-child clog-obj "CLOGB3930207070" :clog-type
             'clog:clog-select :new-id t))
    (setf (slot-value panel 'frame)
            (attach-as-child clog-obj "CLOGB3930207069" :clog-type
             'clog:clog-div :new-id t))
    (let ((target (project-list panel)))
      (declare (ignorable target))
      (add-select-options (project-list panel) (projects-local-directories))
      (setf (value (project-list panel)) (car (projects-local-directories))))
    (clog:set-on-double-click (project-list panel)
                              (lambda (target)
                                (declare (ignorable target))
                                (funcall (on-done panel) panel)))
    (clog:set-on-click (select-button panel)
                       (lambda (target)
                         (declare (ignorable target))
                         (funcall (on-done panel) panel)))
    (clog:set-on-click (other-button panel)
                       (lambda (target)
                         (declare (ignorable target))
                         (let ((*default-title-class* *builder-title-class*)
                               (*default-border-class* *builder-border-class*))
                           (input-dialog panel "Add Project Directory"
                            (lambda (result)
                              (when result
                                (pushnew result
                                         quicklisp-client:*local-project-directories*
                                         :test #'equalp)
                                (add-select-option (project-list panel) result
                                 result :selected t)
                                (let ((*default-title-class*
                                       *builder-title-class*)
                                      (*default-border-class*
                                       *builder-border-class*))
                                  (alert-dialog panel
                                   (format nil
                                           "~A added to ql:*local-project-directories* temporarily.~% ~
                                                   Use the generated run-ql or run-ocicl scripts in the created project ~%
                                                   or add to Options -> Edit preferences.lisp to work with your project in ~%
                                                   future."
                                           result)
                                   :width 500 :height 250))))))))
    panel))