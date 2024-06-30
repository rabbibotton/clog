;;;; CLOG Builder generated code - modify original .clog file and rerender
(in-package :clog-tools)
(defclass asdf-systems (clog:clog-panel)
          ((dir-button :reader dir-button)
           (remove-button :reader remove-button)
           (reload-button :reader reload-button)
           (load-new-button :reader load-new-button)
           (reset-list-button :reader reset-list-button)
           (button-panel :reader button-panel)
           (source-file :reader source-file)
           (pop-open-clog-label :reader pop-open-clog-label)
           (pop-open-clog :reader pop-open-clog)
           (ext-open-source-label :reader ext-open-source-label)
           (ext-open-source :reader ext-open-source)
           (check-grid :reader check-grid) (asd-label :reader asd-label)
           (files :reader files) (files-label :reader files-label)
           (deps :reader deps) (deps-label :reader deps-label)
           (loaded-systems :reader loaded-systems)
           (sys-label :reader sys-label) (sysgrid :reader sysgrid)))
(defun create-asdf-systems
       (clog-obj &key (hidden nil) (class nil) (html-id nil) (auto-place t))
  (let ((panel
         (change-class
          (clog:create-div clog-obj :content "
<div id=\"CLOG97061\" style=\"display: grid; box-sizing: content-box; position: absolute; grid-template-areas: &quot;l1 l2&quot; &quot;d1 d2&quot; &quot;d1 l3&quot; &quot;d1 d3&quot; &quot;l4 l5&quot; &quot;lo lo&quot; &quot;ba ba&quot;; gap: 5px; grid-area: ba; inset: 5px; grid-auto-columns: auto;\" data-clog-name=\"sysgrid\">
  <label for=\"CLOG97059\" style=\"box-sizing: content-box; position: static; grid-area: l1;\" id=\"CLOG97060\" data-clog-name=\"sys-label\">Loaded Systems:
  </label>
  <select size=\"4\" style=\"box-sizing: content-box; position: static; grid-area: d1;\" id=\"CLOG97059\" data-clog-name=\"loaded-systems\">
  </select>
  <label for=\"CLOG97057\" style=\"box-sizing: content-box; position: static; width: 281.814px; height: 22.5px; grid-area: l2;\" class=\"\" id=\"CLOG97058\" data-clog-name=\"deps-label\">Depends On: (double click to switch)
  </label>
  <select size=\"4\" style=\"box-sizing: content-box; position: static; height: 76.3494px; grid-area: d2;\" id=\"CLOG97057\" data-clog-name=\"deps\">
  </select>
  <label for=\"\" style=\"box-sizing: content-box; position: static; width: 236.104px; height: 21.4986px; grid-area: l3;\" id=\"CLOG97056\" data-clog-name=\"files-label\">Files: (double click to launch)
  </label>
  <select size=\"4\" style=\"box-sizing: content-box; position: static; grid-area: d3;\" id=\"CLOG97055\" data-clog-name=\"files\">
  </select>
  <label for=\"\" style=\"box-sizing: content-box; position: static;\" id=\"CLOG97054\" data-clog-name=\"asd-label\">ASD Project: (double click to edit)
  </label>
  <div id=\"CLOG97053\" style=\"width: 309.987px; height: 9.9905px; display: grid; box-sizing: content-box; position: static; left: 503.996px; top: 208px; grid-template-areas: &quot;c1 l1 c2 l2&quot;; grid-area: l5;\" data-clog-name=\"check-grid\">
    <input type=\"CHECKBOX\" value=\"\" style=\"box-sizing: content-box; position: static;\" id=\"CLOG97052\" data-clog-name=\"ext-open-source\">
      <label for=\"CLOG97052\" style=\"box-sizing: content-box; position: static;\" id=\"CLOG97051\" data-clog-name=\"ext-open-source-label\">open external
      </label>
      <input type=\"CHECKBOX\" value=\"\" style=\"box-sizing: content-box; position: static;\" id=\"CLOG97050\" data-clog-name=\"pop-open-clog\">
	<label for=\"CLOG97049\" style=\"box-sizing: content-box; position: static;\" id=\"CLOG97049\" data-clog-name=\"pop-open-clog-label\">popup panels
	</label>
  </div>
  <input type=\"TEXT\" value=\"\" style=\"box-sizing: content-box; position: static; grid-area: lo; left: 0px; right: 0px;\" id=\"CLOG97048\" data-clog-name=\"source-file\">
    <div style=\"box-sizing: content-box; position: static; width: 100%; height: 32.4844px; right: 90.0758px; grid-area: ba; display: grid; grid-auto-flow: column; justify-items: normal; column-gap: 5px;\" id=\"CLOG97047\" data-clog-name=\"button-panel\">
      <button style=\"box-sizing: content-box; position: static;\" id=\"CLOG97046\" data-clog-name=\"reset-list-button\">Reset List
      </button>
      <button style=\"box-sizing: content-box; position: static;\" class=\"\" id=\"CLOG97045\" data-clog-name=\"load-new-button\">Load New
      </button>
      <button style=\"box-sizing: content-box; position: static;\" id=\"CLOG97044\" data-clog-name=\"reload-button\">Reload
      </button>
      <button style=\"box-sizing: content-box; position: static;\" id=\"CLOG97043\" data-clog-name=\"remove-button\">Unload
      </button>
      <button style=\"box-sizing: content-box; position: static;\" id=\"CLOG97042\" data-clog-name=\"dir-button\">View Dir
      </button>
    </div>
</div>
"
                           :hidden hidden :class class :html-id html-id
                           :auto-place auto-place)
          'asdf-systems)))
    (setf (slot-value panel 'dir-button)
            (attach-as-child clog-obj "CLOG97042" :clog-type 'clog:clog-button
             :new-id t))
    (setf (slot-value panel 'remove-button)
            (attach-as-child clog-obj "CLOG97043" :clog-type 'clog:clog-button
             :new-id t))
    (setf (slot-value panel 'reload-button)
            (attach-as-child clog-obj "CLOG97044" :clog-type 'clog:clog-button
             :new-id t))
    (setf (slot-value panel 'load-new-button)
            (attach-as-child clog-obj "CLOG97045" :clog-type 'clog:clog-button
             :new-id t))
    (setf (slot-value panel 'reset-list-button)
            (attach-as-child clog-obj "CLOG97046" :clog-type 'clog:clog-button
             :new-id t))
    (setf (slot-value panel 'button-panel)
            (attach-as-child clog-obj "CLOG97047" :clog-type 'clog:clog-div
             :new-id t))
    (setf (slot-value panel 'source-file)
            (attach-as-child clog-obj "CLOG97048" :clog-type
             'clog:clog-form-element :new-id t))
    (setf (slot-value panel 'pop-open-clog-label)
            (attach-as-child clog-obj "CLOG97049" :clog-type 'clog:clog-label
             :new-id t))
    (setf (slot-value panel 'pop-open-clog)
            (attach-as-child clog-obj "CLOG97050" :clog-type
             'clog:clog-form-element :new-id t))
    (setf (slot-value panel 'ext-open-source-label)
            (attach-as-child clog-obj "CLOG97051" :clog-type 'clog:clog-label
             :new-id t))
    (setf (slot-value panel 'ext-open-source)
            (attach-as-child clog-obj "CLOG97052" :clog-type
             'clog:clog-form-element :new-id t))
    (setf (slot-value panel 'check-grid)
            (attach-as-child clog-obj "CLOG97053" :clog-type 'clog:clog-div
             :new-id t))
    (setf (slot-value panel 'asd-label)
            (attach-as-child clog-obj "CLOG97054" :clog-type 'clog:clog-label
             :new-id t))
    (setf (slot-value panel 'files)
            (attach-as-child clog-obj "CLOG97055" :clog-type 'clog:clog-select
             :new-id t))
    (setf (slot-value panel 'files-label)
            (attach-as-child clog-obj "CLOG97056" :clog-type 'clog:clog-label
             :new-id t))
    (setf (slot-value panel 'deps)
            (attach-as-child clog-obj "CLOG97057" :clog-type 'clog:clog-select
             :new-id t))
    (setf (slot-value panel 'deps-label)
            (attach-as-child clog-obj "CLOG97058" :clog-type 'clog:clog-label
             :new-id t))
    (setf (slot-value panel 'loaded-systems)
            (attach-as-child clog-obj "CLOG97059" :clog-type 'clog:clog-select
             :new-id t))
    (setf (slot-value panel 'sys-label)
            (attach-as-child clog-obj "CLOG97060" :clog-type 'clog:clog-label
             :new-id t))
    (setf (slot-value panel 'sysgrid)
            (attach-as-child clog-obj "CLOG97061" :clog-type 'clog:clog-div
             :new-id t))
    (let ((target (sys-label panel)))
      (declare (ignorable target))
      (setf (attribute target "for")
              (clog:js-query target
                             "$('[data-clog-name=\\'loaded-systems\\']').attr('id')")))
    (let ((target (loaded-systems panel)))
      (declare (ignorable target))
      (asdf-browser-reset panel))
    (let ((target (deps-label panel)))
      (declare (ignorable target))
      (setf (attribute target "for")
              (clog:js-query target
                             "$('[data-clog-name=\\'deps\\']').attr('id')")))
    (let ((target (files-label panel)))
      (declare (ignorable target))
      nil)
    (let ((target (asd-label panel)))
      (declare (ignorable target))
      nil)
    (let ((target (ext-open-source-label panel)))
      (declare (ignorable target))
      (setf (attribute target "for")
              (clog:js-query target
                             "$('[data-clog-name=\\'ext-open-source\\']').attr('id')")))
    (let ((target (pop-open-clog-label panel)))
      (declare (ignorable target))
      (setf (attribute target "for")
              (clog:js-query target
                             "$('[data-clog-name=\\'pop-open-clog-label\\']').attr('id')")))
    (clog:set-on-change (loaded-systems panel)
                        (lambda (target)
                          (declare (ignorable target))
                          (asdf-browser-populate panel)))
    (clog:set-on-double-click (deps panel)
                              (lambda (target)
                                (declare (ignorable target))
                                (setf (text-value (loaded-systems panel))
                                        (text-value target))
                                (asdf-browser-populate panel)))
    (clog:set-on-double-click (files panel)
                              (lambda (target)
                                (declare (ignorable target))
                                (asdf-files-double-click panel target)))
    (clog:set-on-double-click (source-file panel)
                              (lambda (target)
                                (declare (ignorable target))
                                (on-open-file panel :open-file
                                 (text-value target))))
    (clog:set-on-click (reset-list-button panel)
                       (lambda (target)
                         (declare (ignorable target))
                         (asdf-browser-reset panel)))
    (clog:set-on-click (load-new-button panel)
                       (lambda (target)
                         (declare (ignorable target))
                         (clog-gui:input-dialog panel "Load New System:"
                                                (lambda (fname)
                                                  (projects-load fname)
                                                  (asdf-browser-reset panel)
                                                  (setf (text-value
                                                         (loaded-systems
                                                          panel))
                                                          fname)
                                                  (asdf-browser-populate
                                                   panel))
                                                :title "Quickload")))
    (clog:set-on-click (reload-button panel)
                       (lambda (target)
                         (declare (ignorable target))
                         (let ((fname (text-value (loaded-systems panel))))
                           (projects-load fname)
                           (setf (text-value (loaded-systems panel)) fname)
                           (asdf-browser-populate panel))))
    (clog:set-on-click (dir-button panel)
                       (lambda (target)
                         (declare (ignorable target))
                         (on-dir-tree panel :dir
                          (asdf/system:system-source-directory
                           (text-value (loaded-systems panel))))))
    panel))