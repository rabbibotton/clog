;;;; CLOG Builder generated code - modify original .clog file and rerender
(in-package :clog-tools)
(defclass panel-search (clog:clog-panel)
          ((search-button :reader search-button)
           (subdir-label :reader subdir-label)
           (subdir-check :reader subdir-check)
           (name-regex-input :reader name-regex-input)
           (grep-input :reader grep-input) (dir-input :reader dir-input)
           (result-box :reader result-box) (result-grid :reader result-grid)
           (search-form :reader search-form)))
(defun create-panel-search
       (clog-obj &key hidden class style html-id (auto-place t))
  (let ((panel
         (change-class
          (clog:create-div clog-obj :content
                           "<form action=\"#\" onsubmit=\"return false;\" id=\"CLOGB3929199710\"
    data-clog-name=\"search-form\">
    <div style=\"display: grid; box-sizing: content-box; position: absolute; inset: 5px; gap: 5px; grid-template: &quot;a a a a a&quot; 1fr &quot;d d d d d&quot; 28px &quot;b n o l c&quot; 28px / 1fr 100px 20px 70px 80px;\"
        class=\"\" id=\"CLOGB3929199711\" data-clog-name=\"result-grid\"><select
            size=\"4\"
            style=\"box-sizing: content-box; position: static; grid-area: a;\"
            class=\"w3-small\" tabindex=\"1\" id=\"CLOGB3929199712\"
            data-clog-name=\"result-box\"></select><input type=\"TEXT\" value=\"\"
            style=\"box-sizing: content-box; position: static; grid-area: d;\"
            placeholder=\"directory to search\" tabindex=\"2\" id=\"CLOGB3929199713\"
            data-clog-name=\"dir-input\"><input type=\"TEXT\" value=\"\"
            style=\"box-sizing: content-box; position: static; left: 50px; top: 8px; grid-area: b;\"
            placeholder=\"search regex\" tabindex=\"3\" id=\"CLOGB3929199714\"
            data-clog-name=\"grep-input\">
        <input type=\"TEXT\" value=\"(.*\\.lisp$)\"
            style=\"box-sizing: content-box; position: static; left: 43px; top: 11px; grid-area: n;\"
            placeholder=\"file regex\" id=\"CLOGB3929199715\" tabindex=\"4\"
            data-clog-name=\"name-regex-input\"><input type=\"CHECKBOX\" value=\"\"
            style=\"box-sizing: content-box; position: static; left: 68px; top: 5px; grid-area: o;\"
            id=\"CLOGB3929199716\" tabindex=\"5\" checked=\"checked\"
            data-clog-name=\"subdir-check\"><label for=\"CLOGB3929199716\"
            style=\"box-sizing: content-box; position: static; left: 70px; top: 11px; grid-area: l; font: 15px / 22.5px sans-serif; visibility: visible; vertical-align: middle; text-align: center;\"
            class=\"w3-tiny\" id=\"CLOGB3929199717\"
            data-clog-name=\"subdir-label\">subdirectories</label><input
            type=\"submit\" value=\"Search\"
            style=\"box-sizing: content-box; position: static; height: 22px; grid-area: c;\"
            class=\"\" tabindex=\"6\" id=\"CLOGB3929199718\"
            data-clog-name=\"search-button\">
    </div>
</form>"
                           :hidden hidden :class class :style style :html-id
                           html-id :auto-place auto-place)
          'panel-search)))
    (setf (slot-value panel 'search-button)
            (attach-as-child clog-obj "CLOGB3929199718" :clog-type
             'clog:clog-form-element :new-id t))
    (setf (slot-value panel 'subdir-label)
            (attach-as-child clog-obj "CLOGB3929199717" :clog-type
             'clog:clog-label :new-id t))
    (setf (slot-value panel 'subdir-check)
            (attach-as-child clog-obj "CLOGB3929199716" :clog-type
             'clog:clog-form-element :new-id t))
    (setf (slot-value panel 'name-regex-input)
            (attach-as-child clog-obj "CLOGB3929199715" :clog-type
             'clog:clog-form-element :new-id t))
    (setf (slot-value panel 'grep-input)
            (attach-as-child clog-obj "CLOGB3929199714" :clog-type
             'clog:clog-form-element :new-id t))
    (setf (slot-value panel 'dir-input)
            (attach-as-child clog-obj "CLOGB3929199713" :clog-type
             'clog:clog-form-element :new-id t))
    (setf (slot-value panel 'result-box)
            (attach-as-child clog-obj "CLOGB3929199712" :clog-type
             'clog:clog-select :new-id t))
    (setf (slot-value panel 'result-grid)
            (attach-as-child clog-obj "CLOGB3929199711" :clog-type
             'clog:clog-div :new-id t))
    (setf (slot-value panel 'search-form)
            (attach-as-child clog-obj "CLOGB3929199710" :clog-type
             'clog:clog-form :new-id t))
    (let ((target (grep-input panel)))
      (declare (ignorable target))
      (setf (attribute target "autofocus") "true")
      (focus target))
    (let ((target (subdir-label panel)))
      (declare (ignorable target))
      (setf (attribute target "for")
              (clog:js-query target
                             "$('[data-clog-name=\\'subdir-check\\']').attr('id')")))
    (clog:set-on-change (dir-input panel)
                        (lambda (target)
                          (declare (ignorable target))
                          (panel-search-dir-change panel target)))
    (clog:set-on-click (search-button panel)
                       (lambda (target)
                         (declare (ignorable target))
                         (panel-search-on-click panel target)))
    panel))