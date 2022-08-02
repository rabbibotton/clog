
(in-package "CLOG-TOOLS")
(defclass projects (clog:clog-panel)
          ((unload-project-button :reader unload-project-button)
           (new-project-button :reader new-project-button)
           (designtime-list :reader designtime-list)
           (runtime-list :reader runtime-list)
           (designtime-label :reader designtime-label)
           (runtime-label :reader runtime-label) (edit-asd :reader edit-asd)
           (project-list :reader project-list)
           (projects-label :reader projects-label)))
(defun create-projects
       (clog-obj &key (hidden nil) (class nil) (html-id nil) (auto-place t))
  (let ((panel
         (change-class
          (clog:create-div clog-obj :content
                           "<label for=\"undefined\" style=\"box-sizing: content-box; position: absolute; left: 5px; top: 6.99858px;\" id=\"CLOGB3868394887\" data-clog-name=\"projects-label\">Current Project</label><select style=\"box-sizing: content-box; position: absolute; left: 5px; top: 35px; width: 386.54px; height: 22px; bottom: 309.041px;\" id=\"CLOGB3868394888\" data-clog-name=\"project-list\"></select><button style=\"box-sizing: content-box; position: absolute; left: 400px; top: 32px; height: 22px; width: 90px;\" id=\"CLOGB3868394889\" data-clog-name=\"edit-asd\">Edit .asd</button><label for=\"CLOGB3868393710\" style=\"box-sizing: content-box; position: absolute; left: 5px; top: 64.9943px;\" id=\"CLOGB3868394890\" data-clog-name=\"runtime-label\">Runtime System</label><label for=\"CLOGB3868393704\" style=\"box-sizing: content-box; position: absolute; left: 290.007px; top: 66.9986px;\" id=\"CLOGB3868394891\" data-clog-name=\"designtime-label\">Design Time System (/tools)</label><select size=\"4\" style=\"box-sizing: content-box; position: absolute; left: 5px; top: 95px; width: 265px; height: 196px;\" id=\"CLOGB3868394892\" data-clog-name=\"runtime-list\"></select><select size=\"4\" style=\"box-sizing: content-box; position: absolute; left: 290.007px; top: 96px; width: 265px; height: 195.545px;\" id=\"CLOGB3868394893\" data-clog-name=\"designtime-list\"></select><button style=\"box-sizing: content-box; position: absolute; left: 520px; top: 9.99574px; width: 100px; height: 22px;\" id=\"CLOGB3868394899\" data-clog-name=\"new-project-button\">New</button><button style=\"box-sizing: content-box; position: absolute; left: 520px; top: 44.9957px; width: 100px; height: 22px; bottom: 309.041px;\" id=\"CLOGB3868394900\" data-clog-name=\"unload-project-button\">Unload</button>"
                           :hidden hidden :class class :html-id html-id
                           :auto-place auto-place)
          'projects)))
    (setf (slot-value panel 'unload-project-button)
            (attach-as-child clog-obj "CLOGB3868394900" :clog-type
             'clog:clog-button :new-id t))
    (setf (slot-value panel 'new-project-button)
            (attach-as-child clog-obj "CLOGB3868394899" :clog-type
             'clog:clog-button :new-id t))
    (setf (slot-value panel 'designtime-list)
            (attach-as-child clog-obj "CLOGB3868394893" :clog-type
             'clog:clog-select :new-id t))
    (setf (slot-value panel 'runtime-list)
            (attach-as-child clog-obj "CLOGB3868394892" :clog-type
             'clog:clog-select :new-id t))
    (setf (slot-value panel 'designtime-label)
            (attach-as-child clog-obj "CLOGB3868394891" :clog-type
             'clog:clog-label :new-id t))
    (setf (slot-value panel 'runtime-label)
            (attach-as-child clog-obj "CLOGB3868394890" :clog-type
             'clog:clog-label :new-id t))
    (setf (slot-value panel 'edit-asd)
            (attach-as-child clog-obj "CLOGB3868394889" :clog-type
             'clog:clog-button :new-id t))
    (setf (slot-value panel 'project-list)
            (attach-as-child clog-obj "CLOGB3868394888" :clog-type
             'clog:clog-select :new-id t))
    (setf (slot-value panel 'projects-label)
            (attach-as-child clog-obj "CLOGB3868394887" :clog-type
             'clog:clog-label :new-id t))
    (let ((target (projects-label panel)))
      (declare (ignorable target))
      (setf (attribute target "for")
              (clog:js-query target
                             "$('[data-clog-name=\\'projects-list\\']').attr('id')")))
    (let ((target (project-list panel)))
      (declare (ignorable target))
      (projects-setup panel))
    (let ((target (runtime-label panel)))
      (declare (ignorable target))
      (setf (attribute target "for")
              (clog:js-query target
                             "$('[data-clog-name=\\'runtime-list\\']').attr('id')")))
    (let ((target (designtime-label panel)))
      (declare (ignorable target))
      (setf (attribute target "for")
              (clog:js-query target
                             "$('[data-clog-name=\\'designtime-list\\']').attr('id')")))
    (clog:set-on-change (project-list panel)
                        (lambda (target)
                          (declare (ignorable target))
                          (projects-populate panel)))
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
    (clog:set-on-double-click (designtime-list panel)
                              (lambda (target)
                                (declare (ignorable target))
                                (open-projects-component target
                                 (format nil "~A/tools"
                                         (text-value (project-list panel)))
                                 target)))
    (clog:set-on-click (new-project-button panel)
                       (lambda (target)
                         (declare (ignorable target))
                         (on-new-app-template panel)))
    (clog:set-on-click (unload-project-button panel)
                       (lambda (target)
                         (declare (ignorable target))
                         (let ((sel (text-value (project-list panel))))
                           (asdf/system-registry:clear-system sel)
                           (setf (text-value (project-list panel)) "None")
                           (projects-populate panel))))
    panel))