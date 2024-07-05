;;;; CLOG Builder generated code - modify original .clog file and rerender
(in-package :clog-tools)
(defclass panel-search (clog:clog-panel)
          ((search-button :reader search-button)
           (subdir-label :reader subdir-label)
           (subdir-check :reader subdir-check) (grep-input :reader grep-input)
           (dir-input :reader dir-input) (result-box :reader result-box)
           (result-grid :reader result-grid) (search-form :reader search-form)))
(defun create-panel-search
       (clog-obj &key hidden class style html-id (auto-place t))
  (let ((panel
         (change-class
          (clog:create-div clog-obj :content
                           "<form action=\"#\" onsubmit=\"return false;\" id=\"CLOGB3929127594\"
    data-clog-name=\"search-form\">
    <div style=\"display: grid; box-sizing: content-box; position: absolute; inset: 5px; gap: 5px; grid-template: &quot;a a a a&quot; 1fr &quot;d d d d&quot; 28px &quot;b o l c&quot; 28px / 1fr 20px 70px 80px;\"
        class=\"\" id=\"CLOGB3929127595\" data-clog-name=\"result-grid\"><select
            size=\"4\"
            style=\"box-sizing: content-box; position: static; grid-area: a;\"
            class=\"w3-small\" tabindex=\"2\" id=\"CLOGB3929127596\"
            data-clog-name=\"result-box\"></select><input type=\"TEXT\" value=\"\"
            style=\"box-sizing: content-box; position: static; grid-area: d;\"
            placeholder=\"directory to search\" tabindex=\"3\" id=\"CLOGB3929127597\"
            data-clog-name=\"dir-input\"><input type=\"TEXT\" value=\"\"
            style=\"box-sizing: content-box; position: static; left: 50px; top: 8px; grid-area: b;\"
            placeholder=\"search regex\" tabindex=\"4\" id=\"CLOGB3929127598\"
            data-clog-name=\"grep-input\">
        <input type=\"CHECKBOX\" value=\"\" id=\"CLOGB39291277016\"
            style=\"box-sizing: content-box; position: static; left: 68px; top: 5px; grid-area: o;\"
            data-clog-name=\"subdir-check\"><label for=\"undefined\"
            id=\"CLOGB39291277397\"
            style=\"box-sizing: content-box; position: static; left: 70px; top: 11px; grid-area: l;\"
            class=\"\" data-clog-name=\"subdir-label\">sub dirs</label><input
            type=\"submit\" value=\"Search\"
            style=\"box-sizing: content-box; position: static; height: 22px; grid-area: c;\"
            class=\"\" tabindex=\"1\" id=\"CLOGB3929127599\"
            data-clog-name=\"search-button\">
    </div>
</form>"
                           :hidden hidden :class class :style style :html-id
                           html-id :auto-place auto-place)
          'panel-search)))
    (setf (slot-value panel 'search-button)
            (attach-as-child clog-obj "CLOGB3929127599" :clog-type
             'clog:clog-form-element :new-id t))
    (setf (slot-value panel 'subdir-label)
            (attach-as-child clog-obj "CLOGB39291277397" :clog-type
             'clog:clog-label :new-id t))
    (setf (slot-value panel 'subdir-check)
            (attach-as-child clog-obj "CLOGB39291277016" :clog-type
             'clog:clog-form-element :new-id t))
    (setf (slot-value panel 'grep-input)
            (attach-as-child clog-obj "CLOGB3929127598" :clog-type
             'clog:clog-form-element :new-id t))
    (setf (slot-value panel 'dir-input)
            (attach-as-child clog-obj "CLOGB3929127597" :clog-type
             'clog:clog-form-element :new-id t))
    (setf (slot-value panel 'result-box)
            (attach-as-child clog-obj "CLOGB3929127596" :clog-type
             'clog:clog-select :new-id t))
    (setf (slot-value panel 'result-grid)
            (attach-as-child clog-obj "CLOGB3929127595" :clog-type
             'clog:clog-div :new-id t))
    (setf (slot-value panel 'search-form)
            (attach-as-child clog-obj "CLOGB3929127594" :clog-type
             'clog:clog-form :new-id t))
    (let ((target (grep-input panel)))
      (declare (ignorable target))
      (setf (attribute target "autofocus") "true")
      (focus target))
    (let ((target (subdir-label panel)))
      (declare (ignorable target))
      (setf (attribute target "for")
              (clog:js-query target
                             "$('[data-clog-name=\\'sundir-check\\']').attr('id')")))
    (clog:set-on-change (dir-input panel)
                        (lambda (target)
                          (declare (ignorable target))
                          (panel-search-dir-change panel target)))
    (clog:set-on-click (search-button panel)
                       (lambda (target)
                         (declare (ignorable target))
                         (panel-search-on-click panel target)))
    panel))