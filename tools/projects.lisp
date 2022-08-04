(in-package "CLOG-TOOLS")
(defclass projects (clog:clog-panel)
          ((design-plugin :reader design-plugin)
           (design-del-dep :reader design-del-dep)
           (design-add-dep :reader design-add-dep)
           (runtime-del-dep :reader runtime-del-dep)
           (runtime-add-dep :reader runtime-add-dep)
           (design-deps :reader design-deps)
           (design-deps-label :reader design-deps-label)
           (runtime-deps :reader runtime-deps)
           (runtime-deps-label :reader runtime-deps-label)
           (reload-project-button :reader reload-project-button)
           (unload-project-button :reader unload-project-button)
           (new-project-button :reader new-project-button)
           (designtime-delete :reader designtime-delete)
           (designtime-add-lisp :reader designtime-add-lisp)
           (designtime-add-clog :reader designtime-add-clog)
           (runtime-delete :reader runtime-delete)
           (runtime-add-lisp :reader runtime-add-lisp)
           (designtime-list :reader designtime-list)
           (runtime-list :reader runtime-list) (dbl-click2 :reader dbl-click2)
           (designtime-label :reader designtime-label)
           (dbl-click1 :reader dbl-click1)
           (runtime-label :reader runtime-label) (edit-asd :reader edit-asd)
           (project-list :reader project-list)
           (projects-label :reader projects-label)))
(defun create-projects
       (clog-obj &key (hidden nil) (class nil) (html-id nil) (auto-place t))
  (let ((panel
         (change-class
          (clog:create-div clog-obj :content
                           "<label for=\"undefined\" style=\"box-sizing: content-box; position: absolute; left: 5px; top: 6.99858px;\" id=\"CLOGB3868634927\" data-clog-name=\"projects-label\">Current Project</label><select style=\"box-sizing: content-box; position: absolute; left: 5px; top: 35px; width: 386.54px; height: 22px; bottom: 309.041px;\" id=\"CLOGB3868634928\" data-clog-name=\"project-list\"></select><button style=\"box-sizing: content-box; position: absolute; left: 400px; top: 32px; height: 22px; width: 90px;\" id=\"CLOGB3868634929\" data-clog-name=\"edit-asd\">Edit .asd</button><label for=\"CLOGB3868393710\" style=\"box-sizing: content-box; position: absolute; left: 5px; top: 65px;\" id=\"CLOGB3868634930\" data-clog-name=\"runtime-label\">Runtime System</label><label for=\"CLOGB3868452429\" style=\"box-sizing: content-box; position: absolute; left: 5px; top: 85px;\" id=\"CLOGB3868634931\" data-clog-name=\"dbl-click1\">(double click to launch)</label><label for=\"CLOGB3868393704\" style=\"box-sizing: content-box; position: absolute; left: 290.007px; top: 65px;\" id=\"CLOGB3868634932\" data-clog-name=\"designtime-label\">Design Time System (/tools)</label><label for=\"CLOGB3868452430\" style=\"box-sizing: content-box; position: absolute; left: 290px; top: 85px;\" id=\"CLOGB3868634933\" data-clog-name=\"dbl-click2\">(double click to launch)</label><select size=\"4\" style=\"box-sizing: content-box; position: absolute; left: 5px; top: 115px; width: 260px; height: 196px;\" id=\"CLOGB3868634934\" data-clog-name=\"runtime-list\"></select><select size=\"4\" style=\"box-sizing: content-box; position: absolute; left: 290px; top: 115px; width: 265px; height: 195.545px;\" id=\"CLOGB3868634935\" data-clog-name=\"designtime-list\"></select><button style=\"box-sizing: content-box; position: absolute; left: 5px; top: 320px;\" id=\"CLOGB3868634936\" data-clog-name=\"runtime-add-lisp\">Add .lisp</button><button style=\"box-sizing: content-box; position: absolute; left: 95px; top: 320px; width: 65px; height: 22px;\" id=\"CLOGB3868634937\" data-clog-name=\"runtime-delete\">Remove</button><button style=\"box-sizing: content-box; position: absolute; left: 290px; top: 320px;\" id=\"CLOGB3868634938\" data-clog-name=\"designtime-add-clog\">Add .clog</button><button style=\"box-sizing: content-box; position: absolute; left: 386px; top: 320px;\" id=\"CLOGB3868634939\" data-clog-name=\"designtime-add-lisp\">Add .lisp</button><button style=\"box-sizing: content-box; position: absolute; left: 478px; top: 320px; width: 65px; height: 22px;\" id=\"CLOGB3868634940\" data-clog-name=\"designtime-delete\">Remove</button><button style=\"box-sizing: content-box; position: absolute; left: 520px; top: 8px; width: 100px; height: 22px;\" id=\"CLOGB3868634941\" data-clog-name=\"new-project-button\">New</button><button style=\"box-sizing: content-box; position: absolute; left: 520px; top: 43px; width: 100px; height: 22px; bottom: 309.041px;\" id=\"CLOGB3868634942\" data-clog-name=\"unload-project-button\">Unload</button><button style=\"box-sizing: content-box; position: absolute; left: 521px; top: 79px; width: 100px; height: 22px; bottom: 309.041px;\" id=\"CLOGB3868634943\" data-clog-name=\"reload-project-button\">Reload</button><label for=\"CLOGB3868619266\" style=\"box-sizing: content-box; position: absolute; left: 5px; top: 365px;\" id=\"CLOGB3868634944\" data-clog-name=\"runtime-deps-label\">Runtime Dependecies</label><select size=\"4\" style=\"box-sizing: content-box; position: absolute; left: 5px; top: 395px; width: 260px; height: 75px;\" id=\"CLOGB3868634945\" data-clog-name=\"runtime-deps\"></select><label for=\"CLOGB3868619268\" style=\"box-sizing: content-box; position: absolute; left: 290px; top: 365px;\" id=\"CLOGB3868634946\" data-clog-name=\"design-deps-label\">Design Dependecies</label><select size=\"4\" style=\"box-sizing: content-box; position: absolute; left: 290px; top: 395px; width: 263.358px; height: 75px;\" id=\"CLOGB3868634947\" data-clog-name=\"design-deps\"></select><button style=\"box-sizing: content-box; position: absolute; left: 5px; top: 480px; width: 65px;\" id=\"CLOGB3868634948\" data-clog-name=\"runtime-add-dep\">Add</button><button style=\"box-sizing: content-box; position: absolute; left: 95px; top: 480px; width: 65px; height: 22px;\" id=\"CLOGB3868634949\" data-clog-name=\"runtime-del-dep\">Remove</button><button style=\"box-sizing: content-box; position: absolute; left: 290px; top: 480px; width: 65px; height: 22px;\" id=\"CLOGB3868634950\" data-clog-name=\"design-add-dep\">Add</button><button style=\"box-sizing: content-box; position: absolute; left: 380px; top: 480px; width: 65px; height: 22px;\" id=\"CLOGB3868634951\" data-clog-name=\"design-del-dep\">Remove</button><button style=\"box-sizing: content-box; position: absolute; left: 470px; top: 480px; width: 65px; height: 22px;\" id=\"CLOGB3868634952\" data-clog-name=\"design-plugin\">Plugin</button>"
                           :hidden hidden :class class :html-id html-id
                           :auto-place auto-place)
          'projects)))
    (setf (slot-value panel 'design-plugin)
            (attach-as-child clog-obj "CLOGB3868634952" :clog-type
             'clog:clog-button :new-id t))
    (setf (slot-value panel 'design-del-dep)
            (attach-as-child clog-obj "CLOGB3868634951" :clog-type
             'clog:clog-button :new-id t))
    (setf (slot-value panel 'design-add-dep)
            (attach-as-child clog-obj "CLOGB3868634950" :clog-type
             'clog:clog-button :new-id t))
    (setf (slot-value panel 'runtime-del-dep)
            (attach-as-child clog-obj "CLOGB3868634949" :clog-type
             'clog:clog-button :new-id t))
    (setf (slot-value panel 'runtime-add-dep)
            (attach-as-child clog-obj "CLOGB3868634948" :clog-type
             'clog:clog-button :new-id t))
    (setf (slot-value panel 'design-deps)
            (attach-as-child clog-obj "CLOGB3868634947" :clog-type
             'clog:clog-select :new-id t))
    (setf (slot-value panel 'design-deps-label)
            (attach-as-child clog-obj "CLOGB3868634946" :clog-type
             'clog:clog-label :new-id t))
    (setf (slot-value panel 'runtime-deps)
            (attach-as-child clog-obj "CLOGB3868634945" :clog-type
             'clog:clog-select :new-id t))
    (setf (slot-value panel 'runtime-deps-label)
            (attach-as-child clog-obj "CLOGB3868634944" :clog-type
             'clog:clog-label :new-id t))
    (setf (slot-value panel 'reload-project-button)
            (attach-as-child clog-obj "CLOGB3868634943" :clog-type
             'clog:clog-button :new-id t))
    (setf (slot-value panel 'unload-project-button)
            (attach-as-child clog-obj "CLOGB3868634942" :clog-type
             'clog:clog-button :new-id t))
    (setf (slot-value panel 'new-project-button)
            (attach-as-child clog-obj "CLOGB3868634941" :clog-type
             'clog:clog-button :new-id t))
    (setf (slot-value panel 'designtime-delete)
            (attach-as-child clog-obj "CLOGB3868634940" :clog-type
             'clog:clog-button :new-id t))
    (setf (slot-value panel 'designtime-add-lisp)
            (attach-as-child clog-obj "CLOGB3868634939" :clog-type
             'clog:clog-button :new-id t))
    (setf (slot-value panel 'designtime-add-clog)
            (attach-as-child clog-obj "CLOGB3868634938" :clog-type
             'clog:clog-button :new-id t))
    (setf (slot-value panel 'runtime-delete)
            (attach-as-child clog-obj "CLOGB3868634937" :clog-type
             'clog:clog-button :new-id t))
    (setf (slot-value panel 'runtime-add-lisp)
            (attach-as-child clog-obj "CLOGB3868634936" :clog-type
             'clog:clog-button :new-id t))
    (setf (slot-value panel 'designtime-list)
            (attach-as-child clog-obj "CLOGB3868634935" :clog-type
             'clog:clog-select :new-id t))
    (setf (slot-value panel 'runtime-list)
            (attach-as-child clog-obj "CLOGB3868634934" :clog-type
             'clog:clog-select :new-id t))
    (setf (slot-value panel 'dbl-click2)
            (attach-as-child clog-obj "CLOGB3868634933" :clog-type
             'clog:clog-label :new-id t))
    (setf (slot-value panel 'designtime-label)
            (attach-as-child clog-obj "CLOGB3868634932" :clog-type
             'clog:clog-label :new-id t))
    (setf (slot-value panel 'dbl-click1)
            (attach-as-child clog-obj "CLOGB3868634931" :clog-type
             'clog:clog-label :new-id t))
    (setf (slot-value panel 'runtime-label)
            (attach-as-child clog-obj "CLOGB3868634930" :clog-type
             'clog:clog-label :new-id t))
    (setf (slot-value panel 'edit-asd)
            (attach-as-child clog-obj "CLOGB3868634929" :clog-type
             'clog:clog-button :new-id t))
    (setf (slot-value panel 'project-list)
            (attach-as-child clog-obj "CLOGB3868634928" :clog-type
             'clog:clog-select :new-id t))
    (setf (slot-value panel 'projects-label)
            (attach-as-child clog-obj "CLOGB3868634927" :clog-type
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
    (let ((target (dbl-click1 panel)))
      (declare (ignorable target))
      (setf (attribute target "for")
              (clog:js-query target
                             "$('[data-clog-name=\\'runtime-list\\']').attr('id')")))
    (let ((target (designtime-label panel)))
      (declare (ignorable target))
      (setf (attribute target "for")
              (clog:js-query target
                             "$('[data-clog-name=\\'designtime-list\\']').attr('id')")))
    (let ((target (dbl-click2 panel)))
      (declare (ignorable target))
      (setf (attribute target "for")
              (clog:js-query target
                             "$('[data-clog-name=\\'designtime-list\\']').attr('id')")))
    (let ((target (runtime-deps-label panel)))
      (declare (ignorable target))
      (setf (attribute target "for")
              (clog:js-query target
                             "$('[data-clog-name=\\'runtime-deps\\']').attr('id')")))
    (let ((target (design-deps-label panel)))
      (declare (ignorable target))
      (setf (attribute target "for")
              (clog:js-query target
                             "$('[data-clog-name=\\'design-deps\\']').attr('id')")))
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
    (clog:set-on-click (runtime-add-lisp panel)
                       (lambda (target)
                         (declare (ignorable target))
                         (let ((sys (text-value (project-list panel))))
                           (projects-add-lisp panel sys))))
    (clog:set-on-click (runtime-delete panel)
                       (lambda (target)
                         (declare (ignorable target))
                         (let ((sys (text-value (project-list panel)))
                               (file (select-text (runtime-list panel))))
                           (unless (equal file "")
                             (setf file (subseq file 0 (- (length file) 5)))
                             (remove-file-from-defsystem sys file :file)
                             (projects-populate panel)))))
    (clog:set-on-click (designtime-add-clog panel)
                       (lambda (target)
                         (declare (ignorable target))
                         (let ((sys (text-value (project-list panel))))
                           (projects-add-clog panel sys))))
    (clog:set-on-click (designtime-add-lisp panel)
                       (lambda (target)
                         (declare (ignorable target))
                         (let ((sys
                                (format nil "~A/tools"
                                        (text-value (project-list panel)))))
                           (projects-add-lisp panel sys))))
    (clog:set-on-click (designtime-delete panel)
                       (lambda (target)
                         (declare (ignorable target))
                         (let ((sys
                                (format nil "~A/tools"
                                        (text-value (project-list panel))))
                               (file (select-text (designtime-list panel)))
                               ext)
                           (unless (equal file "")
                             (setf ext (subseq file (- (length file) 5)))
                             (setf file (subseq file 0 (- (length file) 5)))
                             (remove-file-from-defsystem sys file
                              (if (equalp ext ".clog")
                                  :clog-file
                                  :file))
                             (projects-populate panel)))))
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
    (clog:set-on-click (reload-project-button panel)
                       (lambda (target)
                         (declare (ignorable target))
                         (let ((sel (text-value (project-list panel))))
                           (asdf/system-registry:clear-system sel)
                           (projects-populate panel))))
    (clog:set-on-click (runtime-add-dep panel)
                       (lambda (target)
                         (declare (ignorable target))
                         (let ((sys (text-value (project-list panel))))
                           (projects-add-dep panel sys))))
    (clog:set-on-click (runtime-del-dep panel)
                       (lambda (target)
                         (declare (ignorable target))
                         (let ((sys (text-value (project-list panel)))
                               (file (select-text (runtime-deps panel))))
                           (remove-dep-from-defsystem sys file)
                           (projects-populate panel))))
    (clog:set-on-click (design-add-dep panel)
                       (lambda (target)
                         (declare (ignorable target))
                         (let ((sys
                                (format nil "~A/tools"
                                        (text-value (project-list panel)))))
                           (projects-add-dep panel sys))))
    (clog:set-on-click (design-del-dep panel)
                       (lambda (target)
                         (declare (ignorable target))
                         (let ((sys
                                (format nil "~A/tools"
                                        (text-value (project-list panel))))
                               (file (select-text (design-deps panel))))
                           (remove-dep-from-defsystem sys file)
                           (projects-populate panel))))
    (clog:set-on-click (design-plugin panel)
                       (lambda (target)
                         (declare (ignorable target))
                         (let ((sys (text-value (project-list panel))))
                           (projects-add-plugin panel sys))))
    panel))
