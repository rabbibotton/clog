;;;; CLOG Builder generated code - modify original clog file
(in-package :clog-tools)
(defclass project-dir (clog:clog-panel)
          ((select-button :reader select-button)
           (project-list :reader project-list) (frame :reader frame)
           (on-done :accessor on-done :initform nil)))
(defun create-project-dir
       (clog-obj &key (hidden nil) (class nil) (html-id nil) (auto-place t))
  (let ((panel
         (change-class
          (clog:create-div clog-obj :content
                           "<div style=\"display: flex; flex-direction: column; box-sizing: content-box; position: absolute; inset: 10px 10px 10px 12px; justify-content: normal; align-items: center;\" id=\"CLOGB3871639549\" data-clog-name=\"frame\"><select size=\"4\" style=\"box-sizing: content-box; position: static; width: 400px; height: 200px;\" id=\"CLOGB3871639550\" data-clog-name=\"project-list\"></select><button style=\"box-sizing: content-box; position: static;\" class=\"w3-margin\" id=\"CLOGB3871639551\" data-clog-name=\"select-button\">Select Project Directory</button></div>"
                           :hidden hidden :class class :html-id html-id
                           :auto-place auto-place)
          'project-dir)))
    (setf (slot-value panel 'select-button)
            (attach-as-child clog-obj "CLOGB3871639551" :clog-type
             'clog:clog-button :new-id t))
    (setf (slot-value panel 'project-list)
            (attach-as-child clog-obj "CLOGB3871639550" :clog-type
             'clog:clog-select :new-id t))
    (setf (slot-value panel 'frame)
            (attach-as-child clog-obj "CLOGB3871639549" :clog-type
             'clog:clog-div :new-id t))
    (let ((target (project-list panel)))
      (declare (ignorable target))
      (add-select-options (project-list panel)
       (projects-local-directories))
      (setf (value (project-list panel))
              (car (projects-local-directories))))
    (clog:set-on-double-click (project-list panel)
                              (lambda (target)
                                (declare (ignorable target))
                                (funcall (on-done panel) panel)))
    (clog:set-on-click (select-button panel)
                       (lambda (target)
                         (declare (ignorable target))
                         (funcall (on-done panel) panel)))
    panel))
