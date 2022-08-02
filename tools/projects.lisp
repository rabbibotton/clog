(in-package "CLOG-TOOLS")
(defclass projects (clog:clog-panel)
          ((runtime-list :reader runtime-list) (edit-asd :reader edit-asd)
           (runtime-add-lisp :reader runtime-add-lisp)
           (designtime-add-lisp :reader designtime-add-lisp)
           (designtime-add-clog :reader designtime-add-clog)
           (label-10 :reader label-10)
           (designtime-list :reader designtime-list) (label-7 :reader label-7)
           (new-project-button :reader new-project-button)
           (unload-project-button :reader unload-project-button)
           (project-list :reader project-list)
           (projects-label :reader projects-label)))
(defun create-projects
       (clog-obj &key (hidden nil) (class nil) (html-id nil) (auto-place t))
  (let ((panel
         (change-class
          (clog:create-div clog-obj :content
                           "<label for=\"undefined\" style=\"box-sizing: content-box; position: absolute; left: 5px; top: 6.99858px;\" id=\"CLOGB3868392584\" data-clog-name=\"projects-label\">Current Project</label><select style=\"box-sizing: content-box; position: absolute; left: 5px; top: 35px; width: 386.54px; height: 22px; bottom: 309.041px;\" id=\"CLOGB3868392585\" data-clog-name=\"project-list\"></select><button style=\"box-sizing: content-box; position: absolute; left: 520px; top: 44.9957px; width: 100px; height: 22px; bottom: 309.041px;\" id=\"CLOGB3868392586\" data-clog-name=\"unload-project-button\">Unload</button><button style=\"box-sizing: content-box; position: absolute; left: 520px; top: 9.99574px; width: 100px; height: 22px;\" id=\"CLOGB3868392587\" data-clog-name=\"new-project-button\">New</button><label for=\"\" style=\"box-sizing: content-box; position: absolute; left: 5px; top: 64.9943px;\" id=\"CLOGB3868392588\" data-clog-name=\"label-7\">Runtime System</label><select size=\"4\" style=\"box-sizing: content-box; position: absolute; left: 290.007px; top: 96px; width: 265px; height: 195.545px;\" id=\"CLOGB3868392589\" data-clog-name=\"designtime-list\"></select><label for=\"\" style=\"box-sizing: content-box; position: absolute; left: 290.007px; top: 66.9986px;\" id=\"CLOGB3868392590\" data-clog-name=\"label-10\">Design Time System (/tools)</label><button style=\"box-sizing: content-box; position: absolute; left: 290px; top: 300px;\" id=\"CLOGB3868392591\" data-clog-name=\"designtime-add-clog\">Add .clog</button><button style=\"box-sizing: content-box; position: absolute; left: 385px; top: 300px;\" id=\"CLOGB3868392592\" data-clog-name=\"designtime-add-lisp\">Add .lisp</button><button style=\"box-sizing: content-box; position: absolute; left: 5px; top: 299.997px;\" id=\"CLOGB3868392593\" data-clog-name=\"runtime-add-lisp\">Add .lisp</button><button style=\"box-sizing: content-box; position: absolute; left: 400px; top: 32px; height: 22px; width: 90px;\" id=\"CLOGB3868392594\" data-clog-name=\"edit-asd\">Edit .asd</button><select size=\"4\" style=\"box-sizing: content-box; position: absolute; left: 5px; top: 95px; width: 265px; height: 196px;\" id=\"CLOGB3868392595\" data-clog-name=\"runtime-list\"></select>"
                           :hidden hidden :class class :html-id html-id
                           :auto-place auto-place)
          'projects)))
    (setf (slot-value panel 'runtime-list)
            (attach-as-child clog-obj "CLOGB3868392595" :clog-type
             'clog:clog-select :new-id t))
    (setf (slot-value panel 'edit-asd)
            (attach-as-child clog-obj "CLOGB3868392594" :clog-type
             'clog:clog-button :new-id t))
    (setf (slot-value panel 'runtime-add-lisp)
            (attach-as-child clog-obj "CLOGB3868392593" :clog-type
             'clog:clog-button :new-id t))
    (setf (slot-value panel 'designtime-add-lisp)
            (attach-as-child clog-obj "CLOGB3868392592" :clog-type
             'clog:clog-button :new-id t))
    (setf (slot-value panel 'designtime-add-clog)
            (attach-as-child clog-obj "CLOGB3868392591" :clog-type
             'clog:clog-button :new-id t))
    (setf (slot-value panel 'label-10)
            (attach-as-child clog-obj "CLOGB3868392590" :clog-type
             'clog:clog-label :new-id t))
    (setf (slot-value panel 'designtime-list)
            (attach-as-child clog-obj "CLOGB3868392589" :clog-type
             'clog:clog-select :new-id t))
    (setf (slot-value panel 'label-7)
            (attach-as-child clog-obj "CLOGB3868392588" :clog-type
             'clog:clog-label :new-id t))
    (setf (slot-value panel 'new-project-button)
            (attach-as-child clog-obj "CLOGB3868392587" :clog-type
             'clog:clog-button :new-id t))
    (setf (slot-value panel 'unload-project-button)
            (attach-as-child clog-obj "CLOGB3868392586" :clog-type
             'clog:clog-button :new-id t))
    (setf (slot-value panel 'project-list)
            (attach-as-child clog-obj "CLOGB3868392585" :clog-type
             'clog:clog-select :new-id t))
    (setf (slot-value panel 'projects-label)
            (attach-as-child clog-obj "CLOGB3868392584" :clog-type
             'clog:clog-label :new-id t))
    (let ((target (projects-label panel)))
      (declare (ignorable target))
      (setf (attribute target "for")
              (clog:js-query target
                             "$('[data-clog-name=\\'projects-list\\']').attr('id')")))
    (let ((target (project-list panel)))
      (declare (ignorable target))
      (projects-setup panel))
    (let ((target (label-7 panel)))
      (declare (ignorable target))
      nil)
    (let ((target (label-10 panel)))
      (declare (ignorable target))
      nil)
    (clog:set-on-change (project-list panel)
                        (lambda (target)
                          (declare (ignorable target))
                          (projects-populate panel)))
    (clog:set-on-click (unload-project-button panel)
                       (lambda (target)
                         (declare (ignorable target))
                         (let ((sel (text-value (project-list panel))))
                           (asdf/system-registry:clear-system sel)
                           (setf (text-value (project-list panel)) "None")
                           (projects-populate panel))))
    (clog:set-on-click (new-project-button panel)
                       (lambda (target)
                         (declare (ignorable target))
                         (on-new-app-template panel)))
    (clog:set-on-double-click (designtime-list panel)
                              (lambda (target)
                                (declare (ignorable target))
                                (open-projects-component target
                                 (format nil "~A/tools"
                                         (text-value (project-list panel)))
                                 target)))
    (clog:set-on-click (edit-asd panel)
                       (lambda (target)
                         (declare (ignorable target))
                         (let ((sel (text-value (project-list panel))))
                           (on-open-file panel :open-file
                            (asdf/system:system-source-file
                             (asdf/system:find-system sel))))))
    (clog:set-on-double-click (runtime-list panel)
                              (lambda (target)
                                (declare (ignorable target))
                                (open-projects-component target
                                 (text-value (project-list panel)) target)))
    panel))
