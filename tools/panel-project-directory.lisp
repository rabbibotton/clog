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
          (clog:create-div clog-obj :content "<div data-clog-name=\"frame\"
    style=\"display: flex; flex-direction: column; box-sizing: content-box; position: absolute; inset: 10px; justify-content: normal; align-items: center;\"
    id=\"CLOGB3930736269\">

    <select size=\"4\" data-clog-name=\"project-list\"
        style=\"box-sizing: content-box; position: static; width: 90%; height: 80%;\"
        id=\"CLOGB3930736270\"></select>

    <button data-clog-name=\"select-button\"
        style=\"box-sizing: content-box; position: static; width: 220px;\"
        class=\"w3-margin\" id=\"CLOGB3930736271\">Select
        Project Directory</button>

    <button data-clog-name=\"other-button\"
        style=\"box-sizing: content-box; position: static; left: 31px; top: 3px; width: 220px; height: 22px;\"
        class=\"\" id=\"CLOGB3930736272\">Add Alternative Directory</button>
</div>"
                           :hidden hidden :class class :style style :html-id
                           html-id :auto-place auto-place)
          'project-dir)))
    (setf (slot-value panel 'other-button)
            (attach-as-child clog-obj "CLOGB3930736272" :clog-type
             'clog:clog-button :new-id t))
    (setf (slot-value panel 'select-button)
            (attach-as-child clog-obj "CLOGB3930736271" :clog-type
             'clog:clog-button :new-id t))
    (setf (slot-value panel 'project-list)
            (attach-as-child clog-obj "CLOGB3930736270" :clog-type
             'clog:clog-select :new-id t))
    (setf (slot-value panel 'frame)
            (attach-as-child clog-obj "CLOGB3930736269" :clog-type
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
                         (add-template-dir panel target)))
    panel))