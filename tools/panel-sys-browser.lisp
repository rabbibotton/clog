;;;; CLOG Builder generated code - modify original .clog file and rerender
(in-package :clog-tools)
(defclass sys-browser (clog:clog-panel)
          ((status-box :reader status-box) (save-button :reader save-button)
           (eval-button :reader eval-button)
           (eval-sel-button :reader eval-sel-button)
           (eval-form-button :reader eval-form-button)
           (file-name :reader file-name) (src-box :reader src-box)
           (pac-box :reader pac-box) (doc-box :reader doc-box)
           (class-box :reader class-box) (search-box :reader search-box)
           (class-only :reader class-only)
           (label-class-only :reader label-class-only)
           (package-box :reader package-box) (type-box :reader type-box)
           (classes :accessor classes) (fname :accessor fname)
           (state :accessor state :initform t)))
(defun create-sys-browser
       (clog-obj &key (hidden nil) (class nil) (html-id nil) (auto-place t))
  (let ((panel
         (change-class
          (clog:create-div clog-obj :content
                           "<select style=\"box-sizing: content-box; position: absolute; left: 5px; top: 10px; width: 190px; height: 20px;\" id=\"CLOGB3921103967\" data-clog-name=\"type-box\"></select><select style=\"box-sizing: content-box; position: absolute; left: 205px; top: 10px; width: 300px; height: 20px; bottom: 335.028px;\" id=\"CLOGB3921103968\" data-clog-name=\"package-box\"></select><label for=\"CLOGB386795982312\" style=\"box-sizing: content-box; position: absolute; left: 535px; top: 9px;\" class=\"\" id=\"CLOGB3921103969\" data-clog-name=\"label-class-only\">pkg only</label><input type=\"CHECKBOX\" value=\"\" style=\"box-sizing: content-box; position: absolute; left: 516px; top: 15px;\" checked=\"checked\" id=\"CLOGB3921103970\" data-clog-name=\"class-only\"><input type=\"TEXT\" value=\"\" style=\"box-sizing: content-box; position: absolute; inset: 7px 5px 332.045px 605px; height: 22px;\" placeholder=\"search\" name=\"\" id=\"CLOGB3921103971\" data-clog-name=\"search-box\"><select size=\"4\" style=\"box-sizing: content-box; position: absolute; left: 5px; top: 40px; right: 5px; height: 125px;\" class=\"w3-small\" id=\"CLOGB3921103972\" data-clog-name=\"class-box\"></select><textarea name=\"\" cols=\"20\" rows=\"2\" style=\"box-sizing: content-box; position: absolute; right: 5px; height: 50px; resize: none; min-width: 0px; top: 175px; left: 5px;\" class=\"&nbsp;w3-small\" id=\"CLOGB3921103973\" data-clog-name=\"doc-box\"></textarea><input type=\"TEXT\" value=\"clog-user\" style=\"box-sizing: content-box; position: absolute; left: 5px; top: 239px; right: 5px; height: 18px;\" placeholder=\"Current Package\" class=\"w3-small\" id=\"CLOGB3921103974\" data-clog-name=\"pac-box\"><div class=\"ace_editor ace_hidpi ace-xcode ace-tm\" style=\"border: thin solid black; box-sizing: content-box; position: absolute; inset: 273px 5px 50px;\" id=\"CLOGB3921103975\" data-clog-name=\"src-box\"></div><input type=\"button\" style=\"box-sizing: content-box; position: absolute; left: 5px; bottom: 24px; right: 290px; text-align: left;\" class=\"w3-small\" id=\"CLOGB3921103976\" data-clog-name=\"file-name\"><input type=\"BUTTON\" value=\"Eval Form\" style=\"box-sizing: content-box; position: absolute; width: 50px; height: 15px; right: 215px; bottom: 24px;\" class=\"w3-tiny\" disabled=\"disabled\" id=\"CLOGB3921103977\" data-clog-name=\"eval-form-button\"><input type=\"BUTTON\" value=\"Eval Sel\" style=\"box-sizing: content-box; position: absolute; width: 50px; height: 15px; right: 145px; bottom: 25px;\" class=\"w3-tiny\" placeholder=\"\" disabled=\"disabled\" id=\"CLOGB3921103978\" data-clog-name=\"eval-sel-button\"><input type=\"BUTTON\" value=\"Eval File\" style=\"box-sizing: content-box; position: absolute; width: 50px; height: 15px; right: 75px; bottom: 25px;\" class=\"w3-tiny\" disabled=\"disabled\" id=\"CLOGB3921103979\" data-clog-name=\"eval-button\"><input type=\"BUTTON\" value=\"Save\" style=\"box-sizing: content-box; position: absolute; width: 50px; height: 15px; right: 5px; bottom: 25px;\" class=\"w3-tiny\" disabled=\"disabled\" id=\"CLOGB3921103980\" data-clog-name=\"save-button\"><div style=\"box-sizing: content-box; position: absolute; left: 5px; bottom: 5px; right: 5px;\" class=\"w3-tiny w3-border\" id=\"CLOGB3921103981\" data-clog-name=\"status-box\">status</div>"
                           :hidden hidden :class class :html-id html-id
                           :auto-place auto-place)
          'sys-browser)))
    (setf (slot-value panel 'status-box)
            (attach-as-child clog-obj "CLOGB3921103981" :clog-type
             'clog:clog-div :new-id t))
    (setf (slot-value panel 'save-button)
            (attach-as-child clog-obj "CLOGB3921103980" :clog-type
             'clog:clog-form-element :new-id t))
    (setf (slot-value panel 'eval-button)
            (attach-as-child clog-obj "CLOGB3921103979" :clog-type
             'clog:clog-form-element :new-id t))
    (setf (slot-value panel 'eval-sel-button)
            (attach-as-child clog-obj "CLOGB3921103978" :clog-type
             'clog:clog-form-element :new-id t))
    (setf (slot-value panel 'eval-form-button)
            (attach-as-child clog-obj "CLOGB3921103977" :clog-type
             'clog:clog-form-element :new-id t))
    (setf (slot-value panel 'file-name)
            (attach-as-child clog-obj "CLOGB3921103976" :clog-type
             'clog:clog-form-element :new-id t))
    (setf (slot-value panel 'src-box)
            (attach-as-child clog-obj "CLOGB3921103975" :clog-type
             'clog-ace:clog-ace-element :new-id t))
    (setf (slot-value panel 'pac-box)
            (attach-as-child clog-obj "CLOGB3921103974" :clog-type
             'clog:clog-form-element :new-id t))
    (setf (slot-value panel 'doc-box)
            (attach-as-child clog-obj "CLOGB3921103973" :clog-type
             'clog:clog-text-area :new-id t))
    (setf (slot-value panel 'class-box)
            (attach-as-child clog-obj "CLOGB3921103972" :clog-type
             'clog:clog-select :new-id t))
    (setf (slot-value panel 'search-box)
            (attach-as-child clog-obj "CLOGB3921103971" :clog-type
             'clog:clog-form-element :new-id t))
    (setf (slot-value panel 'class-only)
            (attach-as-child clog-obj "CLOGB3921103970" :clog-type
             'clog:clog-form-element :new-id t))
    (setf (slot-value panel 'label-class-only)
            (attach-as-child clog-obj "CLOGB3921103969" :clog-type
             'clog:clog-label :new-id t))
    (setf (slot-value panel 'package-box)
            (attach-as-child clog-obj "CLOGB3921103968" :clog-type
             'clog:clog-select :new-id t))
    (setf (slot-value panel 'type-box)
            (attach-as-child clog-obj "CLOGB3921103967" :clog-type
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
    (let ((target (label-class-only panel)))
      (declare (ignorable target))
      (setf (attribute target "for")
              (clog:js-query target
                             "$('[data-clog-name=\\'class-only\\']').attr('id')")))
    (let ((target (src-box panel)))
      (declare (ignorable target))
      (clog-ace:attach-clog-ace target)
      (setf (clog-ace:theme target) "ace/theme/xcode")
      (setf (clog-ace:mode target) "ace/mode/lisp")
      (setf (clog-ace:tab-size target) 2)
      (setup-lisp-ace target (status-box panel)))
    (clog:set-on-change (type-box panel)
                        (lambda (target)
                          (declare (ignorable target))
                          (sys-browser-populate panel)))
    (clog:set-on-change (package-box panel)
                        (lambda (target)
                          (declare (ignorable target))
                          (sys-browser-populate panel)))
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
    (clog:set-on-click (class-box panel)
                       (lambda (target)
                         (declare (ignorable target))
                         (sys-browser-select panel target)))
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
    (clog:set-on-click (file-name panel)
                       (lambda (target)
                         (declare (ignorable target))
                         (on-open-file target :open-file (text-value target))))
    (clog:set-on-click (eval-form-button panel)
                       (lambda (target)
                         (declare (ignorable target))
                         (let ((p
                                (parse-integer
                                 (js-query panel
                                  (format nil
                                          "~A.session.doc.positionToIndex (~A.selection.getCursor(), 0);"
                                          (clog-ace::js-ace (src-box panel))
                                          (clog-ace::js-ace (src-box panel))))
                                 :junk-allowed t))
                               (tv (text-value (src-box panel)))
                               (pk (text-value (pac-box panel)))
                               (lf nil)
                               (cp 0))
                           (loop
                            (setf (values lf cp)
                                    (read-from-string tv nil nil :start cp))
                            (unless lf (return nil))
                            (when (> cp p) (return lf)))
                           (when lf
                             (let ((result
                                    (capture-eval lf :clog-obj
                                     (connection-body panel) :eval-in-package
                                     (format nil "~A" pk))))
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
    (clog:set-on-click (save-button panel)
                       (lambda (target)
                         (declare (ignorable target))
                         (when (fname panel)
                           (write-file (text-value (src-box panel))
                            (fname panel))
                           (setf (state panel) nil)
                           (setf (disabledp (save-button panel)) t))))
    panel))