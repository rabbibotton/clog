(in-package "CLOG-TOOLS")
(defclass sys-browser (clog:clog-panel)
  (    (class-only :reader class-only)
    (label-class-only :reader label-class-only)
    (status-box :reader status-box)
    (src-box :reader src-box)
    (doc-box :reader doc-box)
    (class-box :reader class-box)
    (package-box :reader package-box)
    (type-box :reader type-box)
(classes :accessor classes)))
(defun create-sys-browser (clog-obj &key (hidden nil) (class nil) (html-id nil) (auto-place t))
  (let ((panel (change-class (clog:create-div clog-obj :content "<select style=\"box-sizing: content-box; position: absolute; left: 5px; top: 10px; width: 190px; height: 20px;\" id=\"CLOGB3867963405\" data-clog-name=\"type-box\"></select><select style=\"box-sizing: content-box; position: absolute; left: 205px; top: 10px; width: 300px; height: 20px; bottom: 335.028px;\" id=\"CLOGB3867963406\" data-clog-name=\"package-box\"></select><select size=\"4\" style=\"box-sizing: content-box; position: absolute; left: 5px; top: 45px; width: 350px; height: 145px;\" class=\"w3-small\" id=\"CLOGB3867963407\" data-clog-name=\"class-box\"></select><textarea name=\"\" cols=\"20\" rows=\"2\" style=\"box-sizing: content-box; position: absolute; inset: 45px 5px 176.051px 365px; height: 139.989px; resize: none; min-width: 0px;\" class=\"&nbsp;w3-small\" id=\"CLOGB3867963408\" data-clog-name=\"doc-box\"></textarea><div class=\"ace_editor ace_hidpi ace-xcode ace-tm\" style=\"border: thin solid black; box-sizing: content-box; position: absolute; inset: 199.991px 5px 30px;\" id=\"CLOGB3867963409\" data-clog-name=\"src-box\"></div><div style=\"box-sizing: content-box; position: absolute; left: 5px; bottom: 5px; right: 5px;\" class=\"w3-tiny w3-border\" id=\"CLOGB3867963410\" data-clog-name=\"status-box\">status</div><label for=\"CLOGB386795982312\" style=\"box-sizing: content-box; position: absolute; left: 535px; top: 9px;\" class=\"\" id=\"CLOGB3867963411\" data-clog-name=\"label-class-only\">class only</label><input type=\"CHECKBOX\" value=\"\" style=\"box-sizing: content-box; position: absolute; left: 516px; top: 15px;\" checked=\"checked\" id=\"CLOGB3867963412\" data-clog-name=\"class-only\">"
         :hidden hidden :class class :html-id html-id :auto-place auto-place) 'sys-browser)))
    (setf (slot-value panel 'class-only) (attach-as-child clog-obj "CLOGB3867963412" :clog-type 'CLOG:CLOG-FORM-ELEMENT :new-id t))
    (setf (slot-value panel 'label-class-only) (attach-as-child clog-obj "CLOGB3867963411" :clog-type 'CLOG:CLOG-LABEL :new-id t))
    (setf (slot-value panel 'status-box) (attach-as-child clog-obj "CLOGB3867963410" :clog-type 'CLOG:CLOG-DIV :new-id t))
    (setf (slot-value panel 'src-box) (attach-as-child clog-obj "CLOGB3867963409" :clog-type 'CLOG-ACE:CLOG-ACE-ELEMENT :new-id t))
    (setf (slot-value panel 'doc-box) (attach-as-child clog-obj "CLOGB3867963408" :clog-type 'CLOG:CLOG-TEXT-AREA :new-id t))
    (setf (slot-value panel 'class-box) (attach-as-child clog-obj "CLOGB3867963407" :clog-type 'CLOG:CLOG-SELECT :new-id t))
    (setf (slot-value panel 'package-box) (attach-as-child clog-obj "CLOGB3867963406" :clog-type 'CLOG:CLOG-SELECT :new-id t))
    (setf (slot-value panel 'type-box) (attach-as-child clog-obj "CLOGB3867963405" :clog-type 'CLOG:CLOG-SELECT :new-id t))
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
    (clog:set-on-change (class-box panel) (lambda (target) (declare (ignorable target)) (let* ((item (nth (parse-integer (text-value (class-box panel))) (classes panel)))
       (fname (getf (definitions:source-location item) :file)))
  (setf (text-value (doc-box panel))
    (or (definitions:documentation item)
        "No documentation"))
  (cond (fname
         (setf (text-value (src-box panel)) (read-file fname))
         (js-execute target (format nil "~A.find('~A')" (clog-ace::js-ace (src-box panel)) (definitions:name item))))
        (t
         (setf (text-value (src-box panel)) "No file information"))))))
    (clog:set-on-change (class-only panel) (lambda (target) (declare (ignorable target)) (sys-browser-populate panel)))
    panel))
