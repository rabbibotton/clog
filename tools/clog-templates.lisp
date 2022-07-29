(in-package "CLOG-TOOLS")
(defclass clog-templates (clog:clog-panel)
          ((fill-button :reader fill-button)
           (template-box :reader template-box) (win :accessor win)))
(defun create-clog-templates
       (clog-obj &key (hidden nil) (class nil) (html-id nil) (auto-place t))
  (let ((panel
         (change-class
          (clog:create-div clog-obj :content
                           "<form action=\"#\" onsubmit=\"return false;\" target=\"_self\" style=\"box-sizing: content-box; position: static; left: 162px; top: 216px;\" id=\"CLOGB3868051553\" data-clog-name=\"none-form\"><label for=\"\" style=\"box-sizing: content-box; position: absolute; left: 7px; top: 6px; text-align: start; cursor: default; font: bold 15px / 22.5px Verdana, sans-serif&nbsp;; bottom: 211.5px;\" id=\"CLOGB3868051554\" data-clog-name=\"none-label-2\">Available Templates:</label><div style=\"box-sizing: content-box; position: absolute; inset: 35px 7px 45px;\" id=\"CLOGB3868051555\" data-clog-name=\"none-div-1\"><select size=\"4\" style=\"box-sizing: content-box; position: static; inset: 40px 5px 5px; width: 100%; height: 100%;\" id=\"CLOGB3868051556\" data-clog-name=\"template-box\"></select></div><button class=\"\" style=\"box-sizing: content-box; position: absolute; left: 7px; bottom: 5px;\" id=\"CLOGB3868051557\" data-clog-name=\"fill-button\">Fill Template</button></form>"
                           :hidden hidden :class class :html-id html-id
                           :auto-place auto-place)
          'clog-templates)))
    (setf (slot-value panel 'fill-button)
            (attach-as-child clog-obj "CLOGB3868051557" :clog-type
             'clog:clog-button :new-id t))
    (setf (slot-value panel 'template-box)
            (attach-as-child clog-obj "CLOGB3868051556" :clog-type
             'clog:clog-select :new-id t))
    (clog:set-on-click (fill-button panel)
                       (lambda (target)
                         (declare (ignorable target))
                         (fill-button-clicked panel)))
    panel))
