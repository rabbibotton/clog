(in-package "CLOG-TOOLS")
(defclass asdf-systems (clog:clog-panel)
          ((asd-label :reader asd-label) (reload-button :reader reload-button)
           (remove-button :reader remove-button)
           (load-new-button :reader load-new-button)
           (reset-list-button :reader reset-list-button)
           (button-panel :reader button-panel)
           (source-file :reader source-file) (files :reader files)
           (files-label :reader files-label) (deps :reader deps)
           (deps-label :reader deps-label)
           (loaded-systems :reader loaded-systems)
           (sys-label :reader sys-label)))
(defun create-asdf-systems
       (clog-obj &key (hidden nil) (class nil) (html-id nil) (auto-place t))
  (let ((panel
         (change-class
          (clog:create-div clog-obj :content
                           "<label for=\"CLOGB38680930412\" style=\"box-sizing: content-box; position: absolute; left: 10px; top: 8px;\" id=\"CLOGB3868299109\" data-clog-name=\"sys-label\">Loaded Systems:</label><select size=\"4\" style=\"box-sizing: content-box; position: absolute; left: 10px; top: 38px; width: 239.716px; height: 261.341px;\" id=\"CLOGB3868299110\" data-clog-name=\"loaded-systems\"></select><label for=\"CLOGB38680988074\" style=\"box-sizing: content-box; position: absolute; left: 265px; top: 8px; width: 281.814px; height: 22.5px;\" class=\"\" id=\"CLOGB3868299111\" data-clog-name=\"deps-label\">Depends On: (double click to switch)</label><select size=\"4\" style=\"box-sizing: content-box; position: absolute; left: 265px; top: 39.9858px; width: 310.361px; height: 76.3494px;\" id=\"CLOGB3868299112\" data-clog-name=\"deps\"></select><label for=\"\" style=\"box-sizing: content-box; position: absolute; left: 265px; top: 124px; width: 236.104px; height: 21.4986px;\" id=\"CLOGB3868299113\" data-clog-name=\"files-label\">Files: (double click to launch)</label><select size=\"4\" style=\"box-sizing: content-box; position: absolute; left: 265px; top: 151.991px; width: 311.562px; height: 146.932px;\" id=\"CLOGB3868299114\" data-clog-name=\"files\"></select><input type=\"TEXT\" value=\"\" style=\"box-sizing: content-box; position: absolute; left: 10px; top: 332px; width: 560.727px; height: 22.5px;\" id=\"CLOGB3868299115\" data-clog-name=\"source-file\"><div style=\"box-sizing: content-box; position: absolute; left: 1.0015px; top: 368.991px; width: 442.912px; height: 32.486px;\" id=\"CLOGB3868299116\" data-clog-name=\"button-panel\"><button style=\"box-sizing: content-box; position: absolute; left: 10px; top: 0px; width: 85px; height: 22px;\" id=\"CLOGB3868299117\" data-clog-name=\"reset-list-button\">Reset List</button><button style=\"box-sizing: content-box; position: absolute; left: 119.991px; top: 0px; width: 85px; height: 22px;\" class=\"\" id=\"CLOGB3868299118\" data-clog-name=\"load-new-button\">Load New</button><button style=\"box-sizing: content-box; position: absolute; left: 342px; top: 0px; width: 85px; height: 22px;\" id=\"CLOGB3868299119\" data-clog-name=\"remove-button\">Remove</button><button style=\"box-sizing: content-box; position: absolute; left: 230.996px; top: 0px; width: 85px; height: 22px;\" id=\"CLOGB3868299120\" data-clog-name=\"reload-button\">Reload</button></div><label for=\"\" style=\"box-sizing: content-box; position: absolute; left: 10px; top: 304.996px;\" id=\"CLOGB3868299121\" data-clog-name=\"asd-label\">ASD Project: (double click to edit)</label>"
                           :hidden hidden :class class :html-id html-id
                           :auto-place auto-place)
          'asdf-systems)))
    (setf (slot-value panel 'asd-label)
            (attach-as-child clog-obj "CLOGB3868299121" :clog-type
             'clog:clog-label :new-id t))
    (setf (slot-value panel 'reload-button)
            (attach-as-child clog-obj "CLOGB3868299120" :clog-type
             'clog:clog-button :new-id t))
    (setf (slot-value panel 'remove-button)
            (attach-as-child clog-obj "CLOGB3868299119" :clog-type
             'clog:clog-button :new-id t))
    (setf (slot-value panel 'load-new-button)
            (attach-as-child clog-obj "CLOGB3868299118" :clog-type
             'clog:clog-button :new-id t))
    (setf (slot-value panel 'reset-list-button)
            (attach-as-child clog-obj "CLOGB3868299117" :clog-type
             'clog:clog-button :new-id t))
    (setf (slot-value panel 'button-panel)
            (attach-as-child clog-obj "CLOGB3868299116" :clog-type
             'clog:clog-div :new-id t))
    (setf (slot-value panel 'source-file)
            (attach-as-child clog-obj "CLOGB3868299115" :clog-type
             'clog:clog-form-element :new-id t))
    (setf (slot-value panel 'files)
            (attach-as-child clog-obj "CLOGB3868299114" :clog-type
             'clog:clog-select :new-id t))
    (setf (slot-value panel 'files-label)
            (attach-as-child clog-obj "CLOGB3868299113" :clog-type
             'clog:clog-label :new-id t))
    (setf (slot-value panel 'deps)
            (attach-as-child clog-obj "CLOGB3868299112" :clog-type
             'clog:clog-select :new-id t))
    (setf (slot-value panel 'deps-label)
            (attach-as-child clog-obj "CLOGB3868299111" :clog-type
             'clog:clog-label :new-id t))
    (setf (slot-value panel 'loaded-systems)
            (attach-as-child clog-obj "CLOGB3868299110" :clog-type
             'clog:clog-select :new-id t))
    (setf (slot-value panel 'sys-label)
            (attach-as-child clog-obj "CLOGB3868299109" :clog-type
             'clog:clog-label :new-id t))
    (let ((target (sys-label panel)))
      (declare (ignorable target))
      (setf (attribute target "for")
              (clog:js-query target
                             "$('[data-clog-name=\\'loaded-systems\\']').attr('id')")))
    (let ((target (loaded-systems panel)))
      (declare (ignorable target))
      (asdf-browser-reset panel))
    (let ((target (deps-label panel)))
      (declare (ignorable target))
      (setf (attribute target "for")
              (clog:js-query target
                             "$('[data-clog-name=\\'deps\\']').attr('id')")))
    (let ((target (files-label panel)))
      (declare (ignorable target))
      nil)
    (let ((target (asd-label panel)))
      (declare (ignorable target))
      nil)
    (clog:set-on-change (loaded-systems panel)
                        (lambda (target)
                          (declare (ignorable target))
                          (asdf-browser-populate panel)))
    (clog:set-on-double-click (deps panel)
                              (lambda (target)
                                (declare (ignorable target))
                                (setf (text-value (loaded-systems panel))
                                        (text-value target))
                                (asdf-browser-populate panel)))
    (clog:set-on-double-click (files panel)
                              (lambda (target)
                                (declare (ignorable target))
                                (let ((disp (select-text target))
                                      (item (text-value target)))
                                  (cond
                                   ((equal (subseq item (1- (length item)))
                                           "/")
                                    (setf (inner-html (files panel)) "")
                                    (dolist
                                        (n
                                         (asdf/component:module-components
                                          (asdf/component:find-component
                                           (asdf/system:find-system
                                            (text-value
                                             (loaded-systems panel)))
                                           (subseq disp 0
                                                   (1- (length disp))))))
                                      (let ((name
                                             (asdf/component:component-relative-pathname
                                              n))
                                            (path
                                             (asdf/component:component-pathname
                                              n)))
                                        (add-select-option (files panel) path
                                         name))))
                                   ((and (> (length item) 5)
                                         (equal
                                          (subseq item (- (length item) 5))
                                          ".clog"))
                                    (on-new-builder-panel panel :open-file
                                     item))
                                   (t (on-open-file panel :open-file item))))))
    (clog:set-on-double-click (source-file panel)
                              (lambda (target)
                                (declare (ignorable target))
                                (on-open-file panel :open-file
                                 (text-value target))))
    (clog:set-on-click (reset-list-button panel)
                       (lambda (target)
                         (declare (ignorable target))
                         (asdf-browser-reset panel)))
    (clog:set-on-click (load-new-button panel)
                       (lambda (target)
                         (declare (ignorable target))
                         (clog-gui:input-dialog panel "Load New System:"
                                                (lambda (fname)
                                                  (quicklisp-client:quickload
                                                   fname)
                                                  (asdf-browser-reset panel)
                                                  (setf (text-value
                                                         (loaded-systems
                                                          panel))
                                                          fname)
                                                  (asdf-browser-populate
                                                   panel))
                                                :title "Quickload")))
    (clog:set-on-click (remove-button panel)
                       (lambda (target)
                         (declare (ignorable target))
                         (asdf/system-registry:clear-system
                          (text-value (loaded-systems panel)))
                         (asdf-browser-reset panel)))
    (clog:set-on-click (reload-button panel)
                       (lambda (target)
                         (declare (ignorable target))
                         (let ((fname (text-value (loaded-systems panel))))
                           (quicklisp-client:quickload fname)
                           (setf (text-value (loaded-systems panel)) fname)
                           (asdf-browser-populate panel))))
    panel))
