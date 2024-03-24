;;;; CLOG Builder generated code - modify original .clog file and rerender
(in-package :clog-tools)
(defclass dir-view (clog:clog-panel)
          ((pop-clog-lavel :reader pop-clog-lavel) (pop-clog :reader pop-clog)
           (open-ext-label :reader open-ext-label)
           (open-file-ext :reader open-file-ext)
           (rename-button :reader rename-button)
           (del-button :reader del-button) (open-button :reader open-button)
           (rename-dir-button :reader rename-dir-button)
           (del-dir-button :reader del-dir-button)
           (new-dir-button :reader new-dir-button)
           (open-dir-button :reader open-dir-button) (files :reader files)
           (divider :reader divider) (folders :reader folders)
           (current-dir :accessor current-dir :initform ".")))
(defun create-dir-view
       (clog-obj &key (hidden nil) (class nil) (html-id nil) (auto-place t))
  (let ((panel
         (change-class
          (clog:create-div clog-obj :content
                           "<select size=\"4\" style=\"box-sizing: content-box; position: absolute; left: 10px; top: 10px; right: 10px; height: 115px; overflow: auto;\" id=\"CLOGB3920294017\" data-clog-name=\"folders\"></select><div style=\"box-sizing: content-box; position: absolute; left: 10px; height: 5px; background-attachment: scroll; background-color: rgb(0, 0, 0); right: 10px; top: 166px;\" id=\"CLOGB3920294018\" data-clog-name=\"divider\"></div><select size=\"4\" style=\"box-sizing: content-box; position: absolute; inset: 175px 10px 40px; overflow: auto;\" id=\"CLOGB3920294019\" data-clog-name=\"files\"></select><input type=\"BUTTON\" value=\"Open\" style=\"box-sizing: content-box; position: absolute; left: 10px; top: 132px; width: 70px;\" id=\"CLOGB3920294020\" data-clog-name=\"open-dir-button\"><input type=\"BUTTON\" value=\"New\" style=\"box-sizing: content-box; position: absolute; left: 100px; top: 132px; width: 70px;\" id=\"CLOGB3920294021\" data-clog-name=\"new-dir-button\"><input type=\"BUTTON\" value=\"Delete\" style=\"box-sizing: content-box; position: absolute; left: 190px; top: 132px; width: 70px;\" id=\"CLOGB3920294022\" data-clog-name=\"del-dir-button\"><input type=\"BUTTON\" value=\"Rename\" style=\"box-sizing: content-box; position: absolute; left: 280px; top: 132px; width: 70px;\" id=\"CLOGB3920294023\" data-clog-name=\"rename-dir-button\"><input type=\"BUTTON\" value=\"Open\" style=\"box-sizing: content-box; position: absolute; left: 10px; bottom: 5px; width: 70px;\" id=\"CLOGB3920294024\" data-clog-name=\"open-button\"><input type=\"BUTTON\" value=\"Delete\" style=\"box-sizing: content-box; position: absolute; left: 100px; bottom: 5px; width: 70px;\" id=\"CLOGB3920294025\" data-clog-name=\"del-button\"><input type=\"BUTTON\" value=\"Rename\" style=\"box-sizing: content-box; position: absolute; left: 190px; bottom: 5px; width: 70px;\" id=\"CLOGB3920294026\" data-clog-name=\"rename-button\"><input type=\"CHECKBOX\" value=\"\" style=\"box-sizing: content-box; position: absolute; bottom: 12px; left: 290px;\" id=\"CLOGB3920294027\" data-clog-name=\"open-file-ext\"><label for=\"CLOGB3918824377\" style=\"box-sizing: content-box; position: absolute; left: 308px; bottom: 9px;\" id=\"CLOGB3920294028\" data-clog-name=\"open-ext-label\">open external</label><input type=\"CHECKBOX\" value=\"\" style=\"box-sizing: content-box; position: absolute; left: 420px; bottom: 12px;\" id=\"CLOGB3920294029\" data-clog-name=\"pop-clog\"><label for=\"CLOGB392029139824\" style=\"box-sizing: content-box; position: absolute; left: 440px; bottom: 9px;\" id=\"CLOGB3920294030\" data-clog-name=\"pop-clog-lavel\">popup panels</label>"
                           :hidden hidden :class class :html-id html-id
                           :auto-place auto-place)
          'dir-view)))
    (setf (slot-value panel 'pop-clog-lavel)
            (attach-as-child clog-obj "CLOGB3920294030" :clog-type
             'clog:clog-label :new-id t))
    (setf (slot-value panel 'pop-clog)
            (attach-as-child clog-obj "CLOGB3920294029" :clog-type
             'clog:clog-form-element :new-id t))
    (setf (slot-value panel 'open-ext-label)
            (attach-as-child clog-obj "CLOGB3920294028" :clog-type
             'clog:clog-label :new-id t))
    (setf (slot-value panel 'open-file-ext)
            (attach-as-child clog-obj "CLOGB3920294027" :clog-type
             'clog:clog-form-element :new-id t))
    (setf (slot-value panel 'rename-button)
            (attach-as-child clog-obj "CLOGB3920294026" :clog-type
             'clog:clog-form-element :new-id t))
    (setf (slot-value panel 'del-button)
            (attach-as-child clog-obj "CLOGB3920294025" :clog-type
             'clog:clog-form-element :new-id t))
    (setf (slot-value panel 'open-button)
            (attach-as-child clog-obj "CLOGB3920294024" :clog-type
             'clog:clog-form-element :new-id t))
    (setf (slot-value panel 'rename-dir-button)
            (attach-as-child clog-obj "CLOGB3920294023" :clog-type
             'clog:clog-form-element :new-id t))
    (setf (slot-value panel 'del-dir-button)
            (attach-as-child clog-obj "CLOGB3920294022" :clog-type
             'clog:clog-form-element :new-id t))
    (setf (slot-value panel 'new-dir-button)
            (attach-as-child clog-obj "CLOGB3920294021" :clog-type
             'clog:clog-form-element :new-id t))
    (setf (slot-value panel 'open-dir-button)
            (attach-as-child clog-obj "CLOGB3920294020" :clog-type
             'clog:clog-form-element :new-id t))
    (setf (slot-value panel 'files)
            (attach-as-child clog-obj "CLOGB3920294019" :clog-type
             'clog:clog-select :new-id t))
    (setf (slot-value panel 'divider)
            (attach-as-child clog-obj "CLOGB3920294018" :clog-type
             'clog:clog-div :new-id t))
    (setf (slot-value panel 'folders)
            (attach-as-child clog-obj "CLOGB3920294017" :clog-type
             'clog:clog-select :new-id t))
    (let ((target (folders panel)))
      (declare (ignorable target))
      (on-setup-dir-win panel))
    (let ((target (open-ext-label panel)))
      (declare (ignorable target))
      (setf (attribute target "for")
              (clog:js-query target
                             "$('[data-clog-name=\\'open-file-ext\\']').attr('id')")))
    (let ((target (pop-clog-lavel panel)))
      (declare (ignorable target))
      (setf (attribute target "for")
              (clog:js-query target
                             "$('[data-clog-name=\\'pop-clog\\']').attr('id')")))
    (clog:set-on-mouse-double-click (folders panel)
                                    (lambda (target data)
                                      (declare (ignorable target data))
                                      (populate-dir-win panel (value target))))
    (clog:set-on-double-click (files panel)
                              (lambda (target)
                                (declare (ignorable target))
                                (on-select-dir-win panel)))
    (clog:set-on-click (open-dir-button panel)
                       (lambda (target)
                         (declare (ignorable target))
                         (populate-dir-win panel (value (folders panel)))))
    (clog:set-on-click (new-dir-button panel)
                       (lambda (target)
                         (declare (ignorable target))
                         (on-new-dir-dir-win panel)))
    (clog:set-on-click (del-dir-button panel)
                       (lambda (target)
                         (declare (ignorable target))
                         (on-delete-dir-dir-win panel
                          (value (folders panel)))))
    (clog:set-on-click (rename-dir-button panel)
                       (lambda (target)
                         (declare (ignorable target))
                         (on-rename-dir-dir-win panel
                          (value (folders panel)))))
    (clog:set-on-click (open-button panel)
                       (lambda (target)
                         (declare (ignorable target))
                         (on-select-dir-win panel)))
    (clog:set-on-click (del-button panel)
                       (lambda (target)
                         (declare (ignorable target))
                         (on-delete-dir-win panel)))
    (clog:set-on-click (rename-button panel)
                       (lambda (target)
                         (declare (ignorable target))
                         (on-rename-dir-win panel)))
    panel))