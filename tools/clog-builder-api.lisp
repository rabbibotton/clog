(in-package :clog-tools)

;; Control Record Utilities / Plugin APIs

(defun control-info (control-type-name)
  "Return the control-record for CONTROL-TYPE-NAME from supported controls. (Exported)"
  (if (equal control-type-name "clog-data")
       `(:name           "clog-data"
         :description    "Panel Properties"
         :events         nil
         :properties     ((:name "panel name"
                           :attr "data-clog-name")
                          (:name "in-package"
                           :attr "data-in-package")
                          (:name "custom slots"
                           :attr "data-custom-slots")
                          (:name "width"
                           :get  ,(lambda (control) (width control))
                           :setup :read-only)
                          (:name "height"
                           :setup :read-only
                           :get  ,(lambda (control) (height control)))))
      (find-if (lambda (x) (equal (getf x :name) control-type-name)) *supported-controls*)))

(defun add-supported-controls (control-records)
  "Add a list of control-records to builder's supported controls. If control exists it is
replaced. (Exported)"
  (dolist (r control-records)
    (setf *supported-controls*
          (append (remove-if (lambda (x)
                               (unless (equalp (getf x :name) "group")
                                 (equal (getf x :name) (getf r :name))))
                             *supported-controls*)
                  (list r)))))

(defun add-inspector (name func)
  "Add a custom inspector with NAME and (FUNC object title value clog-obj)"
  (setf *inspectors* (remove-if (lambda (x)
                                  (equalp name (getf x :name)))
                                *inspectors*))
  (push (list :name name :func func) *inspectors*))

(defun reset-control-pallete (panel)
  (let* ((app (connection-data-item panel "builder-app-data"))
         (pallete (select-tool app)))
    (when pallete
      (setf (inner-html pallete) "")
      (dolist (control *supported-controls*)
        (if (equal (getf control :name) "group")
            (add-select-optgroup pallete (getf control :description))
            (add-select-option pallete (getf control :name) (getf control :description)))))))