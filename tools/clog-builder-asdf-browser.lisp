(in-package :clog-tools)

(defun on-new-asdf-browser (obj &key (project nil))
  (let* ((win (create-gui-window obj :title "ASDF System Browser"
                                     :top 40 :left 225
                                     :width 592 :height 435
                                     :client-movement t))
         (panel (create-asdf-systems (window-content win))))
    (when project
      (setf (text-value (loaded-systems panel)) (string-downcase project))
      (asdf-browser-populate panel))))

(defun asdf-browser-reset (panel)
  (let* ((app (connection-data-item panel "builder-app-data")))
    (setf (inner-html (loaded-systems panel)) "")
    (dolist (n (sort (asdf:already-loaded-systems) #'string-lessp))
      (add-select-option (loaded-systems panel) n n))
    (if (current-project app)
        (setf (text-value (loaded-systems panel)) (current-project app))
        (setf (text-value (loaded-systems panel)) "clog"))
    (asdf-browser-populate panel)))

(Defun asdf-browser-populate (panel)
  (setf (text-value (source-file panel))
        (asdf:system-source-file
         (asdf:find-system (text-value (loaded-systems panel)))))
  (setf (inner-html (deps panel)) "")
  (dolist (n (asdf:system-depends-on
              (asdf:find-system (text-value (loaded-systems panel)))))
    (add-select-option (deps panel) n n))
  (setf (inner-html (files panel)) "")
  (dolist (n (asdf:module-components
              (asdf:find-system (text-value (loaded-systems panel)))))
    (let ((name (asdf:component-relative-pathname n))
          (path (asdf:component-pathname n)))
      (add-select-option (files panel) path name))))


