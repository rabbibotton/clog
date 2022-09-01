;;;; CLOG Builder generated code - modify original clog file
(in-package :clog-tools)
(defclass dir-view (clog:clog-panel)
          ((files :reader files) (divider :reader divider)
           (folders :reader folders)))
(defun create-dir-view
       (clog-obj &key (hidden nil) (class nil) (html-id nil) (auto-place t))
  (let ((panel
         (change-class
          (clog:create-div clog-obj :content
                           "<select size=\"4\" style=\"box-sizing: content-box; position: absolute; left: 10px; top: 10px; right: 10px; height: 150px;\" id=\"CLOGB3871033758\" data-clog-name=\"folders\"></select><div style=\"box-sizing: content-box; position: absolute; left: 10px; height: 5px; background-attachment: scroll; background-color: rgb(0, 0, 0); right: 10px; top: 166px;\" id=\"CLOGB3871033756\" data-clog-name=\"divider\"></div><select id=\"CLOGB38710339135\" size=\"4\" style=\"box-sizing: content-box; position: absolute; inset: 175px 10px 10px;\" data-clog-name=\"files\"></select>"
                           :hidden hidden :class class :html-id html-id
                           :auto-place auto-place)
          'dir-view)))
    (setf (slot-value panel 'files)
            (attach-as-child clog-obj "CLOGB38710339135" :clog-type
             'clog:clog-select :new-id t))
    (setf (slot-value panel 'divider)
            (attach-as-child clog-obj "CLOGB3871033756" :clog-type
             'clog:clog-div :new-id t))
    (setf (slot-value panel 'folders)
            (attach-as-child clog-obj "CLOGB3871033758" :clog-type
             'clog:clog-select :new-id t))
    (let ((target (folders panel)))
      (declare (ignorable target))
      (populate-dir-win panel "./"))
    (clog:set-on-change (folders panel)
                        (lambda (target)
                          (declare (ignorable target))
                          (populate-dir-win panel (value target))))
    (clog:set-on-double-click (files panel)
                              (lambda (target)
                                (declare (ignorable target))
                                (on-select-dir-win panel)))
    panel))