(in-package "CLOG-TOOLS")
(defclass sys-browser (clog:clog-panel)
          ((file-name :reader file-name)
           (eval-sel-button :reader eval-sel-button)
           (eval-button :reader eval-button) (save-button :reader save-button)
           (search-box :reader search-box) (class-only :reader class-only)
           (label-class-only :reader label-class-only)
           (status-box :reader status-box) (src-box :reader src-box)
           (pac-box :reader pac-box) (doc-box :reader doc-box)
           (class-box :reader class-box) (package-box :reader package-box)
           (type-box :reader type-box) (classes :accessor classes)
           (fname :accessor fname) (state :accessor state :initform t)))
(defun create-sys-browser
       (clog-obj &key (hidden nil) (class nil) (html-id nil) (auto-place t))
  (let ((panel
         (change-class
          (clog:create-div clog-obj :content
                           "<select style=\"box-sizing: content-box; position: absolute; left: 5px; top: 10px; width: 190px; height: 20px;\" id=\"CLOGB3868648221\" data-clog-name=\"type-box\"></select><select style=\"box-sizing: content-box; position: absolute; left: 205px; top: 10px; width: 300px; height: 20px; bottom: 335.028px;\" id=\"CLOGB3868648222\" data-clog-name=\"package-box\"></select><select size=\"4\" style=\"box-sizing: content-box; position: absolute; left: 5px; top: 40px; right: 5px; height: 125px;\" class=\"w3-small\" id=\"CLOGB3868648223\" data-clog-name=\"class-box\"></select><textarea name=\"\" cols=\"20\" rows=\"2\" style=\"box-sizing: content-box; position: absolute; right: 5px; height: 50px; resize: none; min-width: 0px; top: 175px; left: 5px;\" class=\"&nbsp;w3-small\" id=\"CLOGB3868648224\" data-clog-name=\"doc-box\"></textarea><input type=\"TEXT\" value=\"clog-user\" style=\"box-sizing: content-box; position: absolute; left: 5px; top: 239px; right: 5px; height: 18px;\" placeholder=\"Current Package\" class=\"w3-small\" id=\"CLOGB3868648225\" data-clog-name=\"pac-box\"><div class=\"ace_editor ace_hidpi ace-xcode ace-tm\" style=\"border: thin solid black; box-sizing: content-box; position: absolute; inset: 273px 5px 50px;\" id=\"CLOGB3868648226\" data-clog-name=\"src-box\"></div><div style=\"box-sizing: content-box; position: absolute; left: 5px; bottom: 5px; right: 5px;\" class=\"w3-tiny w3-border\" id=\"CLOGB3868648227\" data-clog-name=\"status-box\">status</div><label for=\"CLOGB386795982312\" style=\"box-sizing: content-box; position: absolute; left: 535px; top: 9px;\" class=\"\" id=\"CLOGB3868648228\" data-clog-name=\"label-class-only\">pkg only</label><input type=\"CHECKBOX\" value=\"\" style=\"box-sizing: content-box; position: absolute; left: 516px; top: 15px;\" checked=\"checked\" id=\"CLOGB3868648229\" data-clog-name=\"class-only\"><input type=\"TEXT\" value=\"\" style=\"box-sizing: content-box; position: absolute; inset: 7px 5px 332.045px 605px; height: 22px;\" placeholder=\"search\" name=\"\" id=\"CLOGB3868648230\" data-clog-name=\"search-box\"><input type=\"BUTTON\" value=\"Save\" style=\"box-sizing: content-box; position: absolute; width: 50px; height: 15px; right: 5px; bottom: 25px;\" class=\"w3-small\" disabled=\"disabled\" id=\"CLOGB3868648231\" data-clog-name=\"save-button\"><input type=\"BUTTON\" value=\"Eval File\" style=\"box-sizing: content-box; position: absolute; width: 50px; height: 15px; right: 75px; bottom: 25px;\" class=\"w3-small\" disabled=\"disabled\" id=\"CLOGB3868648232\" data-clog-name=\"eval-button\"><input type=\"BUTTON\" value=\"Eval Sel\" style=\"box-sizing: content-box; position: absolute; width: 50px; height: 15px; right: 145px; bottom: 25px;\" class=\"w3-small\" placeholder=\"\" disabled=\"disabled\" id=\"CLOGB3868648233\" data-clog-name=\"eval-sel-button\"><div style=\"box-sizing: content-box; position: absolute; left: 5px; bottom: 27px; right: 220px;\" class=\"w3-small\" id=\"CLOGB3868648234\" data-clog-name=\"file-name\">&nbsp;</div>"
                           :hidden hidden :class class :html-id html-id
                           :auto-place auto-place)
          'sys-browser)))
    (setf (slot-value panel 'file-name)
            (attach-as-child clog-obj "CLOGB3868648234" :clog-type
             'clog:clog-div :new-id t))
    (setf (slot-value panel 'eval-sel-button)
            (attach-as-child clog-obj "CLOGB3868648233" :clog-type
             'clog:clog-form-element :new-id t))
    (setf (slot-value panel 'eval-button)
            (attach-as-child clog-obj "CLOGB3868648232" :clog-type
             'clog:clog-form-element :new-id t))
    (setf (slot-value panel 'save-button)
            (attach-as-child clog-obj "CLOGB3868648231" :clog-type
             'clog:clog-form-element :new-id t))
    (setf (slot-value panel 'search-box)
            (attach-as-child clog-obj "CLOGB3868648230" :clog-type
             'clog:clog-form-element :new-id t))
    (setf (slot-value panel 'class-only)
            (attach-as-child clog-obj "CLOGB3868648229" :clog-type
             'clog:clog-form-element :new-id t))
    (setf (slot-value panel 'label-class-only)
            (attach-as-child clog-obj "CLOGB3868648228" :clog-type
             'clog:clog-label :new-id t))
    (setf (slot-value panel 'status-box)
            (attach-as-child clog-obj "CLOGB3868648227" :clog-type
             'clog:clog-div :new-id t))
    (setf (slot-value panel 'src-box)
            (attach-as-child clog-obj "CLOGB3868648226" :clog-type
             'clog-ace:clog-ace-element :new-id t))
    (setf (slot-value panel 'pac-box)
            (attach-as-child clog-obj "CLOGB3868648225" :clog-type
             'clog:clog-form-element :new-id t))
    (setf (slot-value panel 'doc-box)
            (attach-as-child clog-obj "CLOGB3868648224" :clog-type
             'clog:clog-text-area :new-id t))
    (setf (slot-value panel 'class-box)
            (attach-as-child clog-obj "CLOGB3868648223" :clog-type
             'clog:clog-select :new-id t))
    (setf (slot-value panel 'package-box)
            (attach-as-child clog-obj "CLOGB3868648222" :clog-type
             'clog:clog-select :new-id t))
    (setf (slot-value panel 'type-box)
            (attach-as-child clog-obj "CLOGB3868648221" :clog-type
             'clog:clog-select :new-id t))
    (let ((target (type-box panel)))
      (declare (ignorable target))
      (add-select-options target
       '(alien-type callable class compiler-macro condition constant
         declaration definition function generic-function global-definition
         ir1-convert macro method method-combination optimizer setf-expander
         source-transform special-operator structure symbol-macro transform
         type type-definition variable vop))
      (setf (value target) "GLOBAL-DEFINITION"))
    (let ((target (package-box panel)))
      (declare (ignorable target))
      (add-select-option target "All" "All")
      (dolist
          (p
           (sort (list-all-packages)
                 (lambda (a b)
                   (string-lessp (package-name a) (package-name b)))))
        (add-select-option target (package-name p) (package-name p)))
      (setf (value target) "All")
      (sys-browser-populate panel))
    (let ((target (src-box panel)))
      (declare (ignorable target))
      (clog-ace:attach-clog-ace target)
      (setf (clog-ace:theme target) "ace/theme/xcode")
      (setf (clog-ace:mode target) "ace/mode/lisp")
      (setf (clog-ace:tab-size target) 2)
      (setup-lisp-ace target (status-box panel)))
    (let ((target (label-class-only panel)))
      (declare (ignorable target))
      (setf (attribute target "for")
              (clog:js-query target
                             "$('[data-clog-name=\\'class-only\\']').attr('id')")))
    (clog:set-on-change (type-box panel)
                        (lambda (target)
                          (declare (ignorable target))
                          (sys-browser-populate panel)))
    (clog:set-on-change (package-box panel)
                        (lambda (target)
                          (declare (ignorable target))
                          (sys-browser-populate panel)))
    (clog:set-on-change (class-box panel)
                        (lambda (target)
                          (declare (ignorable target))
                          (sys-browser-select panel target)))
    (clog:set-on-input (src-box panel)
                       (lambda (target)
                         (declare (ignorable target))
                         (unless (state panel)
                           (when (fname panel)
                             (setf (state panel) t)
                             (setf (disabledp (save-button panel)) nil)))))
    (clog:set-on-change (class-only panel)
                        (lambda (target)
                          (declare (ignorable target))
                          (sys-browser-populate panel)))
    (clog:set-on-key-up (search-box panel)
                        (lambda (target data)
                          (declare (ignorable target data))
                          (cond
                           ((equal (text-value (package-box panel)) "All")
                            (when (equalp "enter" (getf data :key))
                              (sys-browser-populate panel)))
                           (t (sys-browser-populate panel)))))
    (clog:set-on-click (save-button panel)
                       (lambda (target)
                         (declare (ignorable target))
                         (when (fname panel)
                           (write-file (text-value (src-box panel))
                            (fname panel))
                           (setf (state panel) nil)
                           (setf (disabledp (save-button panel)) t))))
    (clog:set-on-click (eval-button panel)
                       (lambda (target)
                         (declare (ignorable target))
                         (let ((pac (text-value (pac-box panel)))
                               (val (clog-ace:selected-text (src-box panel))))
                           (unless (equal val "")
                             (let ((result
                                    (capture-eval val :clog-obj panel
                                     :eval-in-package
                                     (text-value (package-box panel)))))
                               (clog-web-alert (connection-body panel) "Result"
                                (format nil "~&result: ~A" result) :color-class
                                "w3-green" :time-out 3))))))
    (clog:set-on-click (eval-sel-button panel)
                       (lambda (target)
                         (declare (ignorable target))
                         (let ((pac (text-value (pac-box panel)))
                               (val (clog-ace:selected-text (src-box panel))))
                           (unless (equal val "")
                             (let ((result
                                    (capture-eval val :clog-obj panel
                                     :eval-in-package pac)))
                               (clog-web-alert (connection-body panel) "Result"
                                (format nil "~&result: ~A" result) :color-class
                                "w3-green" :time-out 3))))))
    panel))
