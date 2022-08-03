
(in-package "CLOG-TOOLS")
(defclass projects (clog:clog-panel)
          ((reload-project-button :reader reload-project-button)
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
                           "<label for=\"undefined\" style=\"box-sizing: content-box; position: absolute; left: 5px; top: 6.99858px;\" id=\"CLOGB3868552713\" data-clog-name=\"projects-label\">Current Project</label><select style=\"box-sizing: content-box; position: absolute; left: 5px; top: 35px; width: 386.54px; height: 22px; bottom: 309.041px;\" id=\"CLOGB3868552714\" data-clog-name=\"project-list\"></select><button style=\"box-sizing: content-box; position: absolute; left: 400px; top: 32px; height: 22px; width: 90px;\" id=\"CLOGB3868552715\" data-clog-name=\"edit-asd\">Edit .asd</button><label for=\"CLOGB3868393710\" style=\"box-sizing: content-box; position: absolute; left: 5px; top: 65px;\" id=\"CLOGB3868552716\" data-clog-name=\"runtime-label\">Runtime System</label><label for=\"CLOGB3868452429\" style=\"box-sizing: content-box; position: absolute; left: 5px; top: 85px;\" id=\"CLOGB3868552717\" data-clog-name=\"dbl-click1\">(double click to launch)</label><label for=\"CLOGB3868393704\" style=\"box-sizing: content-box; position: absolute; left: 290.007px; top: 65px;\" id=\"CLOGB3868552718\" data-clog-name=\"designtime-label\">Design Time System (/tools)</label><label for=\"CLOGB3868452430\" style=\"box-sizing: content-box; position: absolute; left: 290px; top: 85px;\" id=\"CLOGB3868552719\" data-clog-name=\"dbl-click2\">(double click to launch)</label><select size=\"4\" style=\"box-sizing: content-box; position: absolute; left: 5px; top: 115px; width: 265px; height: 196px;\" id=\"CLOGB3868552720\" data-clog-name=\"runtime-list\"></select><select size=\"4\" style=\"box-sizing: content-box; position: absolute; left: 290px; top: 115px; width: 265px; height: 195.545px;\" id=\"CLOGB3868552721\" data-clog-name=\"designtime-list\"></select><button style=\"box-sizing: content-box; position: absolute; left: 5px; top: 320px;\" id=\"CLOGB3868552722\" data-clog-name=\"runtime-add-lisp\">Add .lisp</button><button style=\"box-sizing: content-box; position: absolute; left: 92.9801px; top: 320px; width: 65px; height: 22px;\" id=\"CLOGB3868552723\" data-clog-name=\"runtime-delete\">Remove</button><button style=\"box-sizing: content-box; position: absolute; left: 290px; top: 320px;\" id=\"CLOGB3868552724\" data-clog-name=\"designtime-add-clog\">Add .clog</button><button style=\"box-sizing: content-box; position: absolute; left: 385px; top: 320px;\" id=\"CLOGB3868552725\" data-clog-name=\"designtime-add-lisp\">Add .lisp</button><button style=\"box-sizing: content-box; position: absolute; left: 473px; top: 320px; width: 65px; height: 22px;\" id=\"CLOGB3868552726\" data-clog-name=\"designtime-delete\">Remove</button><button style=\"box-sizing: content-box; position: absolute; left: 520px; top: 8px; width: 100px; height: 22px;\" id=\"CLOGB3868552727\" data-clog-name=\"new-project-button\">New</button><button style=\"box-sizing: content-box; position: absolute; left: 520px; top: 43px; width: 100px; height: 22px; bottom: 309.041px;\" id=\"CLOGB3868552728\" data-clog-name=\"unload-project-button\">Unload</button><button style=\"box-sizing: content-box; position: absolute; left: 521px; top: 79px; width: 100px; height: 22px; bottom: 309.041px;\" id=\"CLOGB3868552729\" data-clog-name=\"reload-project-button\">Reload</button>"
                           :hidden hidden :class class :html-id html-id
                           :auto-place auto-place)
          'projects)))
    (setf (slot-value panel 'reload-project-button)
            (attach-as-child clog-obj "CLOGB3868552729" :clog-type
             'clog:clog-button :new-id t))
    (setf (slot-value panel 'unload-project-button)
            (attach-as-child clog-obj "CLOGB3868552728" :clog-type
             'clog:clog-button :new-id t))
    (setf (slot-value panel 'new-project-button)
            (attach-as-child clog-obj "CLOGB3868552727" :clog-type
             'clog:clog-button :new-id t))
    (setf (slot-value panel 'designtime-delete)
            (attach-as-child clog-obj "CLOGB3868552726" :clog-type
             'clog:clog-button :new-id t))
    (setf (slot-value panel 'designtime-add-lisp)
            (attach-as-child clog-obj "CLOGB3868552725" :clog-type
             'clog:clog-button :new-id t))
    (setf (slot-value panel 'designtime-add-clog)
            (attach-as-child clog-obj "CLOGB3868552724" :clog-type
             'clog:clog-button :new-id t))
    (setf (slot-value panel 'runtime-delete)
            (attach-as-child clog-obj "CLOGB3868552723" :clog-type
             'clog:clog-button :new-id t))
    (setf (slot-value panel 'runtime-add-lisp)
            (attach-as-child clog-obj "CLOGB3868552722" :clog-type
             'clog:clog-button :new-id t))
    (setf (slot-value panel 'designtime-list)
            (attach-as-child clog-obj "CLOGB3868552721" :clog-type
             'clog:clog-select :new-id t))
    (setf (slot-value panel 'runtime-list)
            (attach-as-child clog-obj "CLOGB3868552720" :clog-type
             'clog:clog-select :new-id t))
    (setf (slot-value panel 'dbl-click2)
            (attach-as-child clog-obj "CLOGB3868552719" :clog-type
             'clog:clog-label :new-id t))
    (setf (slot-value panel 'designtime-label)
            (attach-as-child clog-obj "CLOGB3868552718" :clog-type
             'clog:clog-label :new-id t))
    (setf (slot-value panel 'dbl-click1)
            (attach-as-child clog-obj "CLOGB3868552717" :clog-type
             'clog:clog-label :new-id t))
    (setf (slot-value panel 'runtime-label)
            (attach-as-child clog-obj "CLOGB3868552716" :clog-type
             'clog:clog-label :new-id t))
    (setf (slot-value panel 'edit-asd)
            (attach-as-child clog-obj "CLOGB3868552715" :clog-type
             'clog:clog-button :new-id t))
    (setf (slot-value panel 'project-list)
            (attach-as-child clog-obj "CLOGB3868552714" :clog-type
             'clog:clog-select :new-id t))
    (setf (slot-value panel 'projects-label)
            (attach-as-child clog-obj "CLOGB3868552713" :clog-type
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
    panel))