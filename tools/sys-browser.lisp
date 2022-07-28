(in-package "CLOG-TOOLS")
(defclass sys-browser (clog:clog-panel)
  (    (file-name :reader file-name)
    (eval-sel-button :reader eval-sel-button)
    (eval-button :reader eval-button)
    (save-button :reader save-button)
    (search-box :reader search-box)
    (class-only :reader class-only)
    (label-class-only :reader label-class-only)
    (status-box :reader status-box)
    (src-box :reader src-box)
    (doc-box :reader doc-box)
    (class-box :reader class-box)
    (package-box :reader package-box)
    (type-box :reader type-box)
(classes :accessor classes) (fname :accessor fname) (state :accessor state :initform t)))
(defun create-sys-browser (clog-obj &key (hidden nil) (class nil) (html-id nil) (auto-place t))
  (let ((panel (change-class (clog:create-div clog-obj :content "<select style=\"box-sizing: content-box; position: absolute; left: 5px; top: 10px; width: 190px; height: 20px;\" id=\"CLOGB3868018625\" data-clog-name=\"type-box\"></select><select style=\"box-sizing: content-box; position: absolute; left: 205px; top: 10px; width: 300px; height: 20px; bottom: 335.028px;\" id=\"CLOGB3868018626\" data-clog-name=\"package-box\"></select><select size=\"4\" style=\"box-sizing: content-box; position: absolute; left: 5px; top: 45px; width: 350px; height: 145px;\" class=\"w3-small\" id=\"CLOGB3868018627\" data-clog-name=\"class-box\"></select><textarea name=\"\" cols=\"20\" rows=\"2\" style=\"box-sizing: content-box; position: absolute; inset: 45px 5px 176.051px 365px; height: 139.989px; resize: none; min-width: 0px;\" class=\"&nbsp;w3-small\" id=\"CLOGB3868018628\" data-clog-name=\"doc-box\"></textarea><div class=\"ace_editor ace_hidpi ace-xcode ace-tm\" style=\"border: thin solid black; box-sizing: content-box; position: absolute; inset: 198.991px 5px 50px;\" id=\"CLOGB3868018629\" data-clog-name=\"src-box\"></div><div style=\"box-sizing: content-box; position: absolute; left: 5px; bottom: 5px; right: 5px;\" class=\"w3-tiny w3-border\" id=\"CLOGB3868018630\" data-clog-name=\"status-box\">status</div><label for=\"CLOGB386795982312\" style=\"box-sizing: content-box; position: absolute; left: 535px; top: 9px;\" class=\"\" id=\"CLOGB3868018631\" data-clog-name=\"label-class-only\">class only</label><input type=\"CHECKBOX\" value=\"\" style=\"box-sizing: content-box; position: absolute; left: 516px; top: 15px;\" checked=\"checked\" id=\"CLOGB3868018632\" data-clog-name=\"class-only\"><input type=\"TEXT\" value=\"\" style=\"box-sizing: content-box; position: absolute; left: 620px; top: 7px; height: 22px; right: 5px;\" placeholder=\"search\" name=\"\" id=\"CLOGB3868018633\" data-clog-name=\"search-box\"><input type=\"BUTTON\" value=\"Save\" style=\"box-sizing: content-box; position: absolute; width: 50px; height: 15px; right: 5px; bottom: 25px;\" class=\"w3-small\" disabled=\"disabled\" id=\"CLOGB3868018634\" data-clog-name=\"save-button\"><input type=\"BUTTON\" value=\"Eval File\" style=\"box-sizing: content-box; position: absolute; width: 50px; height: 15px; right: 75px; bottom: 25px;\" class=\"w3-small\" disabled=\"disabled\" id=\"CLOGB3868018635\" data-clog-name=\"eval-button\"><input type=\"BUTTON\" value=\"Eval Sel\" style=\"box-sizing: content-box; position: absolute; width: 50px; height: 15px; right: 145px; bottom: 25px;\" class=\"w3-small\" placeholder=\"\" disabled=\"disabled\" id=\"CLOGB3868018636\" data-clog-name=\"eval-sel-button\"><div style=\"box-sizing: content-box; position: absolute; left: 5px; bottom: 25px; right: 220px;\" class=\"w3-small\" id=\"CLOGB3868018637\" data-clog-name=\"file-name\">&nbsp;</div>"
         :hidden hidden :class class :html-id html-id :auto-place auto-place) 'sys-browser)))
    (setf (slot-value panel 'file-name) (attach-as-child clog-obj "CLOGB3868018637" :clog-type 'CLOG:CLOG-DIV :new-id t))
    (setf (slot-value panel 'eval-sel-button) (attach-as-child clog-obj "CLOGB3868018636" :clog-type 'CLOG:CLOG-FORM-ELEMENT :new-id t))
    (setf (slot-value panel 'eval-button) (attach-as-child clog-obj "CLOGB3868018635" :clog-type 'CLOG:CLOG-FORM-ELEMENT :new-id t))
    (setf (slot-value panel 'save-button) (attach-as-child clog-obj "CLOGB3868018634" :clog-type 'CLOG:CLOG-FORM-ELEMENT :new-id t))
    (setf (slot-value panel 'search-box) (attach-as-child clog-obj "CLOGB3868018633" :clog-type 'CLOG:CLOG-FORM-ELEMENT :new-id t))
    (setf (slot-value panel 'class-only) (attach-as-child clog-obj "CLOGB3868018632" :clog-type 'CLOG:CLOG-FORM-ELEMENT :new-id t))
    (setf (slot-value panel 'label-class-only) (attach-as-child clog-obj "CLOGB3868018631" :clog-type 'CLOG:CLOG-LABEL :new-id t))
    (setf (slot-value panel 'status-box) (attach-as-child clog-obj "CLOGB3868018630" :clog-type 'CLOG:CLOG-DIV :new-id t))
    (setf (slot-value panel 'src-box) (attach-as-child clog-obj "CLOGB3868018629" :clog-type 'CLOG-ACE:CLOG-ACE-ELEMENT :new-id t))
    (setf (slot-value panel 'doc-box) (attach-as-child clog-obj "CLOGB3868018628" :clog-type 'CLOG:CLOG-TEXT-AREA :new-id t))
    (setf (slot-value panel 'class-box) (attach-as-child clog-obj "CLOGB3868018627" :clog-type 'CLOG:CLOG-SELECT :new-id t))
    (setf (slot-value panel 'package-box) (attach-as-child clog-obj "CLOGB3868018626" :clog-type 'CLOG:CLOG-SELECT :new-id t))
    (setf (slot-value panel 'type-box) (attach-as-child clog-obj "CLOGB3868018625" :clog-type 'CLOG:CLOG-SELECT :new-id t))
    (let ((target (type-box panel))) (declare (ignorable target)) (add-select-options target '(ALIEN-TYPE
                             CALLABLE
                             CLASS
                             COMPILER-MACRO
                             CONDITION
                             CONSTANT
                             DECLARATION
                             DEFINITION
                             FUNCTION
                             GENERIC-FUNCTION
                             GLOBAL-DEFINITION
                             IR1-CONVERT
                             MACRO
                             METHOD
                             METHOD-COMBINATION
                             OPTIMIZER
                             SETF-EXPANDER
                             SOURCE-TRANSFORM
                             SPECIAL-OPERATOR
                             STRUCTURE
                             SYMBOL-MACRO
                             TRANSFORM
                             TYPE
                             TYPE-DEFINITION
                             VARIABLE
                             VOP))
(setf (value target) "CALLABLE")
                             )
    (let ((target (package-box panel))) (declare (ignorable target)) (dolist (p (sort (list-all-packages) (lambda (a b)
                                       (string-lessp (package-name a)
                                                     (package-name b)))))
  (add-select-option target (package-name p)
                            (package-name p)))
(setf (value target) "CLOG-USER")
(sys-browser-populate panel))
    (let ((target (src-box panel))) (declare (ignorable target)) (clog-ace:attach-clog-ace target)
(setf (clog-ace:theme target) "ace/theme/xcode")
(setf (clog-ace:mode target) "ace/mode/lisp")
(setf (clog-ace:tab-size target) 2)(setup-lisp-ace target (status-box panel)))
    (let ((target (label-class-only panel))) (declare (ignorable target)) (setf (attribute target "for") (clog:js-query target "$('[data-clog-name=\\'class-only\\']').attr('id')")))
    (clog:set-on-change (type-box panel) (lambda (target) (declare (ignorable target)) (sys-browser-populate panel)))
    (clog:set-on-change (package-box panel) (lambda (target) (declare (ignorable target)) (sys-browser-populate panel)))
    (clog:set-on-change (class-box panel) (lambda (target) (declare (ignorable target)) (let* ((item (nth (parse-integer (text-value (class-box panel))) (classes panel))))
  (setf (fname panel) (getf (definitions:source-location item) :file))
  (setf (text-value (doc-box panel))
    (or (definitions:documentation item)
        "No documentation"))
  (cond ((fname panel)
         (setf (text-value (src-box panel)) (read-file (fname panel)))
         (setf (text-value (file-name panel)) (fname panel))
         (setf (disabledp (eval-button panel)) nil)
         (setf (disabledp (eval-sel-button panel)) nil)
         (setf (state panel) nil)
         (js-execute target (format nil "~A.find('~A')" (clog-ace::js-ace (src-box panel)) (definitions:name item))))
        (t
         (setf (text-value (file-name panel)) "")
         (setf (disabledp (eval-button panel)) t)
         (setf (disabledp (eval-sel-button panel)) t)
         (setf (disabledp (save-button panel)) t)
         (setf (state panel) t)
         (setf (text-value (src-box panel)) "No file information"))))))
    (clog:set-on-input (src-box panel) (lambda (target) (declare (ignorable target)) (unless (state panel)
  (when (fname panel)
    (setf (state panel) t)
    (setf (disabledp (save-button panel)) nil)))))
    (clog:set-on-change (class-only panel) (lambda (target) (declare (ignorable target)) (sys-browser-populate panel)))
    (clog:set-on-key-up (search-box panel) (lambda (target data) (declare (ignorable target data)) (sys-browser-populate panel)))
    (clog:set-on-click (save-button panel) (lambda (target) (declare (ignorable target)) (when (fname panel)
  (write-file (text-value (src-box panel)) (fname panel))
  (setf (state panel) nil)
  (setf (disabledp (save-button panel)) t))))
    (clog:set-on-click (eval-button panel) (lambda (target) (declare (ignorable target)) (let ((val (text-value (src-box panel))))
  (unless (equal val "")
    (let ((result (capture-eval val :clog-obj panel
                                    :eval-in-package (text-value (package-box panel)))))
      (clog-web-alert (connection-body clog-obj) "Result"
                      (format nil "~&result: ~A" result)
                      :color-class "w3-green"
                      :time-out 3))))))
    (clog:set-on-click (eval-sel-button panel) (lambda (target) (declare (ignorable target)) (let ((val (clog-ace:selected-text (src-box panel))))
  (unless (equal val "")
    (let ((result (capture-eval val :clog-obj panel
                                    :eval-in-package (text-value (package-box panel)))))
      (clog-web-alert (connection-body clog-obj) "Result"
                      (format nil "~&result: ~A" result)
                      :color-class "w3-green"
                      :time-out 3))))))
    panel))
