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
           (classes :accessor classes)
           (search-js :accessor search-js :initform nil)
           (fname :accessor fname) (state :accessor state :initform t)))
(defun create-sys-browser
       (clog-obj &key hidden class style html-id (auto-place t))
  (let ((panel
         (change-class
          (clog:create-div clog-obj :content
                           "<select data-clog-name=\"type-box\"
    style=\"box-sizing: content-box; position: absolute; left: 5px; top: 10px; width: 190px; height: 20px;\"
    id=\"CLOGB3930618984\"></select>

<select data-clog-name=\"package-box\"
    style=\"box-sizing: content-box; position: absolute; left: 205px; top: 10px; width: 300px; height: 20px; bottom: 335.028px;\"
    id=\"CLOGB3930618985\"></select><label for=\"CLOGB386795982312\"
    data-clog-name=\"label-class-only\"
    style=\"box-sizing: content-box; position: absolute; left: 535px; top: 9px;\"
    class=\"\" id=\"CLOGB3930618986\">pkg only</label>

<input type=\"CHECKBOX\" value=\"\" data-clog-name=\"class-only\"
    style=\"box-sizing: content-box; position: absolute; left: 516px; top: 15px;\"
    checked=\"checked\" id=\"CLOGB3930618987\">

<input type=\"TEXT\" value=\"\" data-clog-name=\"search-box\"
    style=\"box-sizing: content-box; position: absolute; inset: 7px 5px 332.045px 605px; height: 22px;\"
    placeholder=\"search\" name=\"\" id=\"CLOGB3930618988\">

<select size=\"4\" data-clog-name=\"class-box\"
    style=\"box-sizing: content-box; position: absolute; left: 5px; top: 40px; right: 5px; height: 125px;\"
    class=\"w3-small\" id=\"CLOGB3930618989\"></select>

<textarea name=\"\" cols=\"20\" rows=\"2\" data-clog-name=\"doc-box\"
    style=\"box-sizing: content-box; position: absolute; right: 5px; height: 50px; resize: none; min-width: 0px; top: 175px; left: 5px;\"
    class=\" w3-small\" id=\"CLOGB3930618990\"></textarea>

<input type=\"TEXT\" value=\"clog-user\" data-clog-name=\"pac-box\"
    style=\"box-sizing: content-box; position: absolute; left: 5px; top: 239px; right: 5px; height: 18px;\"
    placeholder=\"Current Package\" class=\"w3-small\" id=\"CLOGB3930618991\">

<div class=\"ace_editor ace_hidpi ace-xcode ace-tm\" data-clog-name=\"src-box\"
    style=\"border: thin solid black; box-sizing: content-box; position: absolute; inset: 273px 5px 50px;\"
    id=\"CLOGB3930618992\"></div>

<input type=\"button\" data-clog-name=\"file-name\"
    style=\"box-sizing: content-box; position: absolute; left: 5px; bottom: 24px; right: 290px; text-align: left; height: 18px;\"
    class=\"w3-small\" id=\"CLOGB3930618993\">

<input type=\"BUTTON\" value=\"Eval Form\" data-clog-name=\"eval-form-button\"
    style=\"box-sizing: content-box; position: absolute; width: 50px; height: 15px; right: 215px; bottom: 24px;\"
    class=\"w3-tiny\" disabled=\"disabled\" id=\"CLOGB3930618994\">

<input type=\"BUTTON\" value=\"Eval Sel\" data-clog-name=\"eval-sel-button\"
    style=\"box-sizing: content-box; position: absolute; width: 50px; height: 15px; right: 145px; bottom: 25px;\"
    class=\"w3-tiny\" placeholder=\"\" disabled=\"disabled\" id=\"CLOGB3930618995\">

<input type=\"BUTTON\" value=\"Eval File\" data-clog-name=\"eval-button\"
    style=\"box-sizing: content-box; position: absolute; width: 50px; height: 15px; right: 75px; bottom: 25px;\"
    class=\"w3-tiny\" disabled=\"disabled\" id=\"CLOGB3930618996\">

<input type=\"BUTTON\" value=\"Save\" data-clog-name=\"save-button\"
    style=\"box-sizing: content-box; position: absolute; width: 50px; height: 15px; right: 5px; bottom: 25px;\"
    class=\"w3-tiny\" disabled=\"disabled\" id=\"CLOGB3930618997\">

<div data-clog-name=\"status-box\"
    style=\"box-sizing: content-box; position: absolute; left: 5px; bottom: 5px; right: 5px;\"
    class=\"w3-tiny w3-border\" id=\"CLOGB3930618998\">status</div>"
                           :hidden hidden :class class :style style :html-id
                           html-id :auto-place auto-place)
          'sys-browser)))
    (setf (slot-value panel 'status-box)
            (attach-as-child clog-obj "CLOGB3930618998" :clog-type
             'clog:clog-div :new-id t))
    (setf (slot-value panel 'save-button)
            (attach-as-child clog-obj "CLOGB3930618997" :clog-type
             'clog:clog-form-element :new-id t))
    (setf (slot-value panel 'eval-button)
            (attach-as-child clog-obj "CLOGB3930618996" :clog-type
             'clog:clog-form-element :new-id t))
    (setf (slot-value panel 'eval-sel-button)
            (attach-as-child clog-obj "CLOGB3930618995" :clog-type
             'clog:clog-form-element :new-id t))
    (setf (slot-value panel 'eval-form-button)
            (attach-as-child clog-obj "CLOGB3930618994" :clog-type
             'clog:clog-form-element :new-id t))
    (setf (slot-value panel 'file-name)
            (attach-as-child clog-obj "CLOGB3930618993" :clog-type
             'clog:clog-form-element :new-id t))
    (setf (slot-value panel 'src-box)
            (attach-as-child clog-obj "CLOGB3930618992" :clog-type
             'clog-ace:clog-ace-element :new-id t))
    (setf (slot-value panel 'pac-box)
            (attach-as-child clog-obj "CLOGB3930618991" :clog-type
             'clog:clog-form-element :new-id t))
    (setf (slot-value panel 'doc-box)
            (attach-as-child clog-obj "CLOGB3930618990" :clog-type
             'clog:clog-text-area :new-id t))
    (setf (slot-value panel 'class-box)
            (attach-as-child clog-obj "CLOGB3930618989" :clog-type
             'clog:clog-select :new-id t))
    (setf (slot-value panel 'search-box)
            (attach-as-child clog-obj "CLOGB3930618988" :clog-type
             'clog:clog-form-element :new-id t))
    (setf (slot-value panel 'class-only)
            (attach-as-child clog-obj "CLOGB3930618987" :clog-type
             'clog:clog-form-element :new-id t))
    (setf (slot-value panel 'label-class-only)
            (attach-as-child clog-obj "CLOGB3930618986" :clog-type
             'clog:clog-label :new-id t))
    (setf (slot-value panel 'package-box)
            (attach-as-child clog-obj "CLOGB3930618985" :clog-type
             'clog:clog-select :new-id t))
    (setf (slot-value panel 'type-box)
            (attach-as-child clog-obj "CLOGB3930618984" :clog-type
             'clog:clog-select :new-id t))
    (let ((target (type-box panel)))
      (declare (ignorable target))
      (sys-browser-type-box-create panel target))
    (let ((target (package-box panel)))
      (declare (ignorable target))
      (sys-browser-package-box panel target))
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
                          (sys-browser-search-box-key-up panel target data)))
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
                         (sys-browser-src-box-on-input panel target)))
    (clog:set-on-click (file-name panel)
                       (lambda (target)
                         (declare (ignorable target))
                         (sys-browser-file-name-on-click panel target)))
    (clog:set-on-click (eval-form-button panel)
                       (lambda (target)
                         (declare (ignorable target))
                         (sys-browser-eval-form-button-on-click panel target)))
    (clog:set-on-click (eval-sel-button panel)
                       (lambda (target)
                         (declare (ignorable target))
                         (sys-browser-eval-sel-button-on-click panel target)))
    (clog:set-on-click (eval-button panel)
                       (lambda (target)
                         (declare (ignorable target))
                         (sys-browser-eval-button-on-click panel target)))
    (clog:set-on-click (save-button panel)
                       (lambda (target)
                         (declare (ignorable target))
                         (sys-browser-save-button-on-click panel target)))
    panel))