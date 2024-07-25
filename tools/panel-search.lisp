;;;; CLOG Builder generated code - modify original .clog file and rerender
(in-package :clog-tools)
(defclass panel-search (clog:clog-panel)
          ((status-bar :reader status-bar) (save-btn :reader save-btn)
           (eval-all-btn :reader eval-all-btn)
           (eval-sel-btn :reader eval-sel-btn)
           (eval-form-btn :reader eval-form-btn)
           (preview-buttons :reader preview-buttons)
           (preview-panel :reader preview-panel)
           (preview-ace :reader preview-ace) (preview-div :reader preview-div)
           (pac-line :reader pac-line) (preview-grid :reader preview-grid)
           (search-button :reader search-button)
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
                           "<form action=\"#\" onsubmit=\"return false;\" data-clog-name=\"search-form\"
    id=\"CLOGB3930901992\">

    <div data-clog-name=\"result-grid\"
        style=\"display: grid; box-sizing: content-box; position: absolute; inset: 5px; gap: 5px; grid-template: &quot;a a a a a p&quot; 1fr &quot;d d d d d r&quot; 28px &quot;b n o l c r&quot; 28px / 1fr 100px 20px 70px 80px 50%;\"
        class=\"\" id=\"CLOGB3930901993\">

        <select size=\"4\" data-clog-name=\"result-box\"
            style=\"box-sizing: content-box; position: static; grid-area: a;\"
            class=\"w3-small\" tabindex=\"1\" id=\"CLOGB3930901994\"></select>

        <input type=\"TEXT\" value=\"\" data-clog-name=\"dir-input\"
            style=\"box-sizing: content-box; position: static; grid-area: d;\"
            placeholder=\"directory to search\" tabindex=\"2\" id=\"CLOGB3930901995\">

        <input type=\"TEXT\" value=\"\" data-clog-name=\"grep-input\"
            style=\"box-sizing: content-box; position: static; left: 50px; top: 8px; grid-area: b;\"
            placeholder=\"search regex\" tabindex=\"3\" id=\"CLOGB3930901996\">

        <input type=\"TEXT\" value=\"(.*\\.lisp$)\"
            data-clog-name=\"name-regex-input\"
            style=\"box-sizing: content-box; position: static; left: 43px; top: 11px; grid-area: n;\"
            placeholder=\"file regex\" tabindex=\"4\" id=\"CLOGB3930901997\">

        <input type=\"CHECKBOX\" value=\"\" data-clog-name=\"subdir-check\"
            style=\"box-sizing: content-box; position: static; left: 68px; top: 5px; grid-area: o;\"
            tabindex=\"5\" checked=\"checked\" id=\"CLOGB3930901998\"><label
            for=\"CLOGB3929199716\" data-clog-name=\"subdir-label\"
            style=\"box-sizing: content-box; position: static; left: 70px; top: 11px; grid-area: l; font: 15px / 22.5px sans-serif; visibility: visible; vertical-align: middle; text-align: center;\"
            class=\"w3-tiny\" id=\"CLOGB3930901999\">subdirectories</label>

        <input type=\"submit\" value=\"Search\" data-clog-name=\"search-button\"
            style=\"box-sizing: content-box; position: static; height: 22px; grid-area: c;\"
            class=\"\" tabindex=\"6\" id=\"CLOGB3930902000\">

        <div id=\"CLOGB393090262523\" data-clog-name=\"preview-grid\"
            style=\"display: grid; box-sizing: content-box; position: static; left: 46px; top: 0px; grid-area: p; grid-template-areas: &quot;a&quot; &quot;b&quot;; grid-template-rows: 28px 1fr; row-gap: 5px;\">

            <input type=\"TEXT\" value=\"\" id=\"CLOGB393090238222\"
                data-clog-name=\"pac-line\"
                style=\"box-sizing: content-box; position: static; left: 77px; top: 8px; grid-area: a;\">

            <div data-clog-name=\"preview-div\"
                style=\"box-sizing: content-box; position: static; left: 81px; top: 5px; grid-area: b;\"
                id=\"CLOGB3930902001\">

                <div style=\"border: thin solid black; box-sizing: content-box; position: relative; width: 100%; height: 100%;\"
                    class=\" ace_editor ace_hidpi ace-xcode ace-tm\"
                    data-clog-name=\"preview-ace\" id=\"CLOGB3930902002\"></div>
            </div>
        </div>

        <div data-clog-name=\"preview-panel\"
            style=\"display: grid; box-sizing: content-box; position: static; left: 58px; top: 5px; grid-area: r; grid-template-areas: &quot;button&quot; &quot;status&quot;; row-gap: 5px;\"
            id=\"CLOGB3930902003\">

            <div data-clog-name=\"preview-buttons\"
                style=\"display: grid; box-sizing: content-box; position: static; left: 88px; top: 10px; grid-auto-flow: column; column-gap: 5px;\"
                class=\"w3-small\" id=\"CLOGB3930902004\">

                <button data-clog-name=\"eval-form-btn\"
                    style=\"box-sizing: content-box; position: static; left: 93px; top: 7px;\"
                    id=\"CLOGB3930902005\">Eval
                    Form</button>

                <button data-clog-name=\"eval-sel-btn\"
                    style=\"box-sizing: content-box; position: static; left: 111px; top: 4px;\"
                    id=\"CLOGB3930902006\">Eval
                    Sel</button>

                <button data-clog-name=\"eval-all-btn\"
                    style=\"box-sizing: content-box; position: static; left: 103px; top: 18px;\"
                    id=\"CLOGB3930902007\">Eval
                    File</button>

                <button data-clog-name=\"save-btn\"
                    style=\"box-sizing: content-box; position: static; left: 118px; top: 8px;\"
                    id=\"CLOGB3930902008\">Save</button>
            </div>

            <input type=\"TEXT\" value=\"\" data-clog-name=\"status-bar\"
                style=\"box-sizing: content-box; position: static; left: 77px; top: 7px;\"
                class=\"w3-border w3-tiny\" readonly=\"readonly\"
                id=\"CLOGB3930902009\">
        </div>
    </div>
</form>"
                           :hidden hidden :class class :style style :html-id
                           html-id :auto-place auto-place)
          'panel-search)))
    (setf (slot-value panel 'status-bar)
            (attach-as-child clog-obj "CLOGB3930902009" :clog-type
             'clog:clog-form-element :new-id t))
    (setf (slot-value panel 'save-btn)
            (attach-as-child clog-obj "CLOGB3930902008" :clog-type
             'clog:clog-button :new-id t))
    (setf (slot-value panel 'eval-all-btn)
            (attach-as-child clog-obj "CLOGB3930902007" :clog-type
             'clog:clog-button :new-id t))
    (setf (slot-value panel 'eval-sel-btn)
            (attach-as-child clog-obj "CLOGB3930902006" :clog-type
             'clog:clog-button :new-id t))
    (setf (slot-value panel 'eval-form-btn)
            (attach-as-child clog-obj "CLOGB3930902005" :clog-type
             'clog:clog-button :new-id t))
    (setf (slot-value panel 'preview-buttons)
            (attach-as-child clog-obj "CLOGB3930902004" :clog-type
             'clog:clog-div :new-id t))
    (setf (slot-value panel 'preview-panel)
            (attach-as-child clog-obj "CLOGB3930902003" :clog-type
             'clog:clog-div :new-id t))
    (setf (slot-value panel 'preview-ace)
            (attach-as-child clog-obj "CLOGB3930902002" :clog-type
             'clog-ace:clog-ace-element :new-id t))
    (setf (slot-value panel 'preview-div)
            (attach-as-child clog-obj "CLOGB3930902001" :clog-type
             'clog:clog-div :new-id t))
    (setf (slot-value panel 'pac-line)
            (attach-as-child clog-obj "CLOGB393090238222" :clog-type
             'clog:clog-form-element :new-id t))
    (setf (slot-value panel 'preview-grid)
            (attach-as-child clog-obj "CLOGB393090262523" :clog-type
             'clog:clog-div :new-id t))
    (setf (slot-value panel 'search-button)
            (attach-as-child clog-obj "CLOGB3930902000" :clog-type
             'clog:clog-form-element :new-id t))
    (setf (slot-value panel 'subdir-label)
            (attach-as-child clog-obj "CLOGB3930901999" :clog-type
             'clog:clog-label :new-id t))
    (setf (slot-value panel 'subdir-check)
            (attach-as-child clog-obj "CLOGB3930901998" :clog-type
             'clog:clog-form-element :new-id t))
    (setf (slot-value panel 'name-regex-input)
            (attach-as-child clog-obj "CLOGB3930901997" :clog-type
             'clog:clog-form-element :new-id t))
    (setf (slot-value panel 'grep-input)
            (attach-as-child clog-obj "CLOGB3930901996" :clog-type
             'clog:clog-form-element :new-id t))
    (setf (slot-value panel 'dir-input)
            (attach-as-child clog-obj "CLOGB3930901995" :clog-type
             'clog:clog-form-element :new-id t))
    (setf (slot-value panel 'result-box)
            (attach-as-child clog-obj "CLOGB3930901994" :clog-type
             'clog:clog-select :new-id t))
    (setf (slot-value panel 'result-grid)
            (attach-as-child clog-obj "CLOGB3930901993" :clog-type
             'clog:clog-div :new-id t))
    (setf (slot-value panel 'search-form)
            (attach-as-child clog-obj "CLOGB3930901992" :clog-type
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
    (let ((target (preview-ace panel)))
      (declare (ignorable target))
      (clog-ace:attach-clog-ace target)
      (setf (clog-ace:theme target) "ace/theme/iplastic")
      (setf (clog-ace:mode target) "ace/mode/lisp")
      (setf (clog-ace:tab-size target) 2))
    (clog:set-on-change (dir-input panel)
                        (lambda (target)
                          (declare (ignorable target))
                          (panel-search-dir-change panel target)))
    (clog:set-on-click (search-button panel)
                       (lambda (target)
                         (declare (ignorable target))
                         (panel-search-on-click panel target)))
    panel))