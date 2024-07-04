;;;; CLOG Builder generated code - modify original .clog file and rerender
(in-package :clog-tools)
(defclass panel-search (clog:clog-panel)
          ((search-button :reader search-button)
           (grep-input :reader grep-input) (dir-input :reader dir-input)
           (result-box :reader result-box) (result-grid :reader result-grid)))
(defun create-panel-search
       (clog-obj &key hidden class style html-id (auto-place t))
  (let ((panel
         (change-class
          (clog:create-div clog-obj :content
                           "<div style=\"display: grid; box-sizing: content-box; position: absolute; inset: 5px; grid-template: &quot;a a&quot; 1fr &quot;d d&quot; 28px &quot;b c&quot; 28px / 1fr 80px; gap: 5px;\"
    class=\"\" id=\"CLOGB3929109399\" data-clog-name=\"result-grid\"><select size=\"4\"
        style=\"box-sizing: content-box; position: static; grid-area: a;\"
        class=\"w3-small\" tabindex=\"2\" id=\"CLOGB3929109400\"
        data-clog-name=\"result-box\"></select><input type=\"TEXT\" value=\"\"
        style=\"box-sizing: content-box; position: static; grid-area: d;\"
        placeholder=\"directory to search\" tabindex=\"3\" id=\"CLOGB3929109401\"
        data-clog-name=\"dir-input\"><input type=\"TEXT\" value=\"\"
        style=\"box-sizing: content-box; position: static; left: 50px; top: 8px; grid-area: b;\"
        placeholder=\"search regex\" tabindex=\"4\" id=\"CLOGB3929109402\"
        data-clog-name=\"grep-input\"><button
        style=\"box-sizing: content-box; position: static; height: 22px; grid-area: c;\"
        class=\"\" tabindex=\"1\" id=\"CLOGB3929109403\"
        data-clog-name=\"search-button\">Search</button>
</div>"
                           :hidden hidden :class class :style style :html-id
                           html-id :auto-place auto-place)
          'panel-search)))
    (setf (slot-value panel 'search-button)
            (attach-as-child clog-obj "CLOGB3929109403" :clog-type
             'clog:clog-button :new-id t))
    (setf (slot-value panel 'grep-input)
            (attach-as-child clog-obj "CLOGB3929109402" :clog-type
             'clog:clog-form-element :new-id t))
    (setf (slot-value panel 'dir-input)
            (attach-as-child clog-obj "CLOGB3929109401" :clog-type
             'clog:clog-form-element :new-id t))
    (setf (slot-value panel 'result-box)
            (attach-as-child clog-obj "CLOGB3929109400" :clog-type
             'clog:clog-select :new-id t))
    (setf (slot-value panel 'result-grid)
            (attach-as-child clog-obj "CLOGB3929109399" :clog-type
             'clog:clog-div :new-id t))
    (let ((target (search-button panel)))
      (declare (ignorable target))
      (setf (attribute target "autofocus") "true")
      (focus target))
    (clog:set-on-change (dir-input panel)
                        (lambda (target)
                          (declare (ignorable target))
                          (panel-search-dir-change panel target)))
    (clog:set-on-click (search-button panel)
                       (lambda (target)
                         (declare (ignorable target))
                         (panel-search-on-click panel target)))
    panel))