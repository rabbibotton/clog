;;;; CLOG Builder generated code - modify original .clog file and rerender
(in-package :clog-tools)
(defclass dir-view (clog:clog-panel)
          ((open-ext-label :reader open-ext-label)
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
                           "<select size=\"4\" style=\"box-sizing: content-box; position: absolute; left: 10px; top: 10px; right: 10px; height: 115px; overflow: auto;\" id=\"CLOGB3918824367\" data-clog-name=\"folders\"></select><div style=\"box-sizing: content-box; position: absolute; left: 10px; height: 5px; background-attachment: scroll; background-color: rgb(0, 0, 0); right: 10px; top: 166px;\" id=\"CLOGB3918824368\" data-clog-name=\"divider\"></div><select size=\"4\" style=\"box-sizing: content-box; position: absolute; inset: 175px 10px 40px; overflow: auto;\" id=\"CLOGB3918824369\" data-clog-name=\"files\"></select><input type=\"BUTTON\" value=\"Open\" style=\"box-sizing: content-box; position: absolute; left: 10px; top: 132px; width: 70px;\" id=\"CLOGB3918824370\" data-clog-name=\"open-dir-button\"><input type=\"BUTTON\" value=\"New\" style=\"box-sizing: content-box; position: absolute; left: 100px; top: 132px; width: 70px;\" id=\"CLOGB3918824371\" data-clog-name=\"new-dir-button\"><input type=\"BUTTON\" value=\"Delete\" style=\"box-sizing: content-box; position: absolute; left: 190px; top: 132px; width: 70px;\" id=\"CLOGB3918824372\" data-clog-name=\"del-dir-button\"><input type=\"BUTTON\" value=\"Rename\" style=\"box-sizing: content-box; position: absolute; left: 280px; top: 132px; width: 70px;\" id=\"CLOGB3918824373\" data-clog-name=\"rename-dir-button\"><input type=\"BUTTON\" value=\"Open\" style=\"box-sizing: content-box; position: absolute; left: 10px; bottom: 5px; width: 70px;\" id=\"CLOGB3918824374\" data-clog-name=\"open-button\"><input type=\"BUTTON\" value=\"Delete\" style=\"box-sizing: content-box; position: absolute; left: 100px; bottom: 5px; width: 70px;\" id=\"CLOGB3918824375\" data-clog-name=\"del-button\"><input type=\"BUTTON\" value=\"Rename\" style=\"box-sizing: content-box; position: absolute; left: 190px; bottom: 5px; width: 70px;\" id=\"CLOGB3918824376\" data-clog-name=\"rename-button\"><input type=\"CHECKBOX\" value=\"\" style=\"box-sizing: content-box; position: absolute; bottom: 12px; left: 290px;\" id=\"CLOGB3918824377\" data-clog-name=\"open-file-ext\"><label for=\"CLOGB3918824377\" style=\"box-sizing: content-box; position: absolute; left: 308px; bottom: 7px;\" id=\"CLOGB3918824378\" data-clog-name=\"open-ext-label\">Open External</label>"
                           :hidden hidden :class class :html-id html-id
                           :auto-place auto-place)
          'dir-view)))
    (setf (slot-value panel 'open-ext-label)
            (attach-as-child clog-obj "CLOGB3918824378" :clog-type
             'clog:clog-label :new-id t))
    (setf (slot-value panel 'open-file-ext)
            (attach-as-child clog-obj "CLOGB3918824377" :clog-type
             'clog:clog-form-element :new-id t))
    (setf (slot-value panel 'rename-button)
            (attach-as-child clog-obj "CLOGB3918824376" :clog-type
             'clog:clog-form-element :new-id t))
    (setf (slot-value panel 'del-button)
            (attach-as-child clog-obj "CLOGB3918824375" :clog-type
             'clog:clog-form-element :new-id t))
    (setf (slot-value panel 'open-button)
            (attach-as-child clog-obj "CLOGB3918824374" :clog-type
             'clog:clog-form-element :new-id t))
    (setf (slot-value panel 'rename-dir-button)
            (attach-as-child clog-obj "CLOGB3918824373" :clog-type
             'clog:clog-form-element :new-id t))
    (setf (slot-value panel 'del-dir-button)
            (attach-as-child clog-obj "CLOGB3918824372" :clog-type
             'clog:clog-form-element :new-id t))
    (setf (slot-value panel 'new-dir-button)
            (attach-as-child clog-obj "CLOGB3918824371" :clog-type
             'clog:clog-form-element :new-id t))
    (setf (slot-value panel 'open-dir-button)
            (attach-as-child clog-obj "CLOGB3918824370" :clog-type
             'clog:clog-form-element :new-id t))
    (setf (slot-value panel 'files)
            (attach-as-child clog-obj "CLOGB3918824369" :clog-type
             'clog:clog-select :new-id t))
    (setf (slot-value panel 'divider)
            (attach-as-child clog-obj "CLOGB3918824368" :clog-type
             'clog:clog-div :new-id t))
    (setf (slot-value panel 'folders)
            (attach-as-child clog-obj "CLOGB3918824367" :clog-type
             'clog:clog-select :new-id t))
    (let ((target (folders panel)))
      (declare (ignorable target))
      (populate-dir-win panel "./"))
    (let ((target (open-ext-label panel)))
      (declare (ignorable target))
      (setf (attribute target "for")
              (clog:js-query target
                             "$('[data-clog-name=\\'open-file-ext\\']').attr('id')")))
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