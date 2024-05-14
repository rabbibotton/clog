;;;; CLOG Builder generated code - modify original .clog file and rerender
(in-package :clog-tools)
(defclass asdf-systems (clog:clog-panel)
          ((pop-open-clog-label :reader pop-open-clog-label)
           (pop-open-clog :reader pop-open-clog)
           (ext-open-source-label :reader ext-open-source-label)
           (ext-open-source :reader ext-open-source)
           (asd-label :reader asd-label) (dir-button :reader dir-button)
           (remove-button :reader remove-button)
           (reload-button :reader reload-button)
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
                           "<label for=\"CLOGB38680930412\" style=\"box-sizing: content-box; position: absolute; left: 10px; top: 8px;\" id=\"CLOGB3920365575\" data-clog-name=\"sys-label\">Loaded Systems:</label><select size=\"4\" style=\"box-sizing: content-box; position: absolute; left: 10px; top: 38px; width: 239.716px; height: 261.341px;\" id=\"CLOGB3920365576\" data-clog-name=\"loaded-systems\"></select><label for=\"CLOGB38680988074\" style=\"box-sizing: content-box; position: absolute; left: 265px; top: 8px; width: 281.814px; height: 22.5px;\" class=\"\" id=\"CLOGB3920365577\" data-clog-name=\"deps-label\">Depends On: (double click to switch)</label><select size=\"4\" style=\"box-sizing: content-box; position: absolute; left: 265px; top: 39.9858px; width: 310.361px; height: 76.3494px;\" id=\"CLOGB3920365578\" data-clog-name=\"deps\"></select><label for=\"\" style=\"box-sizing: content-box; position: absolute; left: 265px; top: 124px; width: 236.104px; height: 21.4986px;\" id=\"CLOGB3920365579\" data-clog-name=\"files-label\">Files: (double click to launch)</label><select size=\"4\" style=\"box-sizing: content-box; position: absolute; left: 265px; top: 151px; width: 311.562px; height: 146.932px;\" id=\"CLOGB3920365580\" data-clog-name=\"files\"></select><input type=\"TEXT\" value=\"\" style=\"box-sizing: content-box; position: absolute; left: 10px; top: 332px; width: 560.727px; height: 22.5px;\" id=\"CLOGB3920365581\" data-clog-name=\"source-file\"><div style=\"box-sizing: content-box; position: absolute; left: 1.0015px; top: 368.991px; width: 572.898px; height: 32.4844px;\" id=\"CLOGB3920365582\" data-clog-name=\"button-panel\"><button style=\"box-sizing: content-box; position: absolute; left: 10px; top: 0px; width: 85px; height: 22px;\" id=\"CLOGB3920365583\" data-clog-name=\"reset-list-button\">Reset List</button><button style=\"box-sizing: content-box; position: absolute; left: 127px; top: 0px; width: 85px; height: 22px;\" class=\"\" id=\"CLOGB3920365584\" data-clog-name=\"load-new-button\">Load New</button><button style=\"box-sizing: content-box; position: absolute; left: 243px; top: 0px; width: 85px; height: 22px;\" id=\"CLOGB3920365585\" data-clog-name=\"reload-button\">Reload</button><button style=\"box-sizing: content-box; position: absolute; left: 360px; top: 0px; width: 85px; height: 22px;\" id=\"CLOGB3920365586\" data-clog-name=\"remove-button\">Unload</button><button style=\"box-sizing: content-box; position: absolute; left: 477px; top: 0px; width: 85px;\" id=\"CLOGB3920365587\" data-clog-name=\"dir-button\">View Dir</button></div><label for=\"\" style=\"box-sizing: content-box; position: absolute; left: 10px; top: 304.996px;\" id=\"CLOGB3920365588\" data-clog-name=\"asd-label\">ASD Project: (double click to edit)</label><input type=\"CHECKBOX\" value=\"\" id=\"CLOGB392036561317\" style=\"box-sizing: content-box; position: absolute; left: 292px; top: 308px;\" data-clog-name=\"ext-open-source\"><label for=\"CLOGB392036561317\" id=\"CLOGB392036564519\" style=\"box-sizing: content-box; position: absolute; left: 310px; top: 302px;\" data-clog-name=\"ext-open-source-label\">open external</label><input type=\"CHECKBOX\" value=\"\" id=\"CLOGB392036562618\" style=\"box-sizing: content-box; position: absolute; left: 426px; top: 308px;\" data-clog-name=\"pop-open-clog\"><label for=\"undefined\" id=\"CLOGB392036565720\" style=\"box-sizing: content-box; position: absolute; left: 445px; top: 302px;\" data-clog-name=\"pop-open-clog-label\">popup panels</label>"
                           :hidden hidden :class class :html-id html-id
                           :auto-place auto-place)
          'asdf-systems)))
    (setf (slot-value panel 'pop-open-clog-label)
            (attach-as-child clog-obj "CLOGB392036565720" :clog-type
             'clog:clog-label :new-id t))
    (setf (slot-value panel 'pop-open-clog)
            (attach-as-child clog-obj "CLOGB392036562618" :clog-type
             'clog:clog-form-element :new-id t))
    (setf (slot-value panel 'ext-open-source-label)
            (attach-as-child clog-obj "CLOGB392036564519" :clog-type
             'clog:clog-label :new-id t))
    (setf (slot-value panel 'ext-open-source)
            (attach-as-child clog-obj "CLOGB392036561317" :clog-type
             'clog:clog-form-element :new-id t))
    (setf (slot-value panel 'asd-label)
            (attach-as-child clog-obj "CLOGB3920365588" :clog-type
             'clog:clog-label :new-id t))
    (setf (slot-value panel 'dir-button)
            (attach-as-child clog-obj "CLOGB3920365587" :clog-type
             'clog:clog-button :new-id t))
    (setf (slot-value panel 'remove-button)
            (attach-as-child clog-obj "CLOGB3920365586" :clog-type
             'clog:clog-button :new-id t))
    (setf (slot-value panel 'reload-button)
            (attach-as-child clog-obj "CLOGB3920365585" :clog-type
             'clog:clog-button :new-id t))
    (setf (slot-value panel 'load-new-button)
            (attach-as-child clog-obj "CLOGB3920365584" :clog-type
             'clog:clog-button :new-id t))
    (setf (slot-value panel 'reset-list-button)
            (attach-as-child clog-obj "CLOGB3920365583" :clog-type
             'clog:clog-button :new-id t))
    (setf (slot-value panel 'button-panel)
            (attach-as-child clog-obj "CLOGB3920365582" :clog-type
             'clog:clog-div :new-id t))
    (setf (slot-value panel 'source-file)
            (attach-as-child clog-obj "CLOGB3920365581" :clog-type
             'clog:clog-form-element :new-id t))
    (setf (slot-value panel 'files)
            (attach-as-child clog-obj "CLOGB3920365580" :clog-type
             'clog:clog-select :new-id t))
    (setf (slot-value panel 'files-label)
            (attach-as-child clog-obj "CLOGB3920365579" :clog-type
             'clog:clog-label :new-id t))
    (setf (slot-value panel 'deps)
            (attach-as-child clog-obj "CLOGB3920365578" :clog-type
             'clog:clog-select :new-id t))
    (setf (slot-value panel 'deps-label)
            (attach-as-child clog-obj "CLOGB3920365577" :clog-type
             'clog:clog-label :new-id t))
    (setf (slot-value panel 'loaded-systems)
            (attach-as-child clog-obj "CLOGB3920365576" :clog-type
             'clog:clog-select :new-id t))
    (setf (slot-value panel 'sys-label)
            (attach-as-child clog-obj "CLOGB3920365575" :clog-type
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
    (let ((target (ext-open-source-label panel)))
      (declare (ignorable target))
      (setf (attribute target "for")
              (clog:js-query target
                             "$('[data-clog-name=\\'ext-open-source\\']').attr('id')")))
    (let ((target (pop-open-clog-label panel)))
      (declare (ignorable target))
      (setf (attribute target "for")
              (clog:js-query target
                             "$('[data-clog-name=\\'pop-open-clog-label\\']').attr('id')")))
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
                                (asdf-files-double-click panel target)))
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
                                                  (projects-load fname)
                                                  (asdf-browser-reset panel)
                                                  (setf (text-value
                                                         (loaded-systems
                                                          panel))
                                                          fname)
                                                  (asdf-browser-populate
                                                   panel))
                                                :title "Quickload")))
    (clog:set-on-click (reload-button panel)
                       (lambda (target)
                         (declare (ignorable target))
                         (let ((fname (text-value (loaded-systems panel))))
                           (projects-load fname)
                           (setf (text-value (loaded-systems panel)) fname)
                           (asdf-browser-populate panel))))
    (clog:set-on-click (dir-button panel)
                       (lambda (target)
                         (declare (ignorable target))
                         (on-dir-tree panel :dir
                          (asdf:system-source-directory
                           (text-value (loaded-systems panel))))))
    panel))
