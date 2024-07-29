(in-package :clog-tools)

;; Control Record Utilities / Plugin APIs

(defun control-info (control-type-name)
  "Return the control-record for CONTROL-TYPE-NAME from supported controls. (Exported)"
  (if (equal control-type-name "clog-data")
       `(:name           "clog-data"
         :description    "Panel Properties"
         :events         nil
         :properties     ((:name "in-package"
                           :attr "data-in-package")
                          (:name "custom slots"
                           :setup ,(lambda (control td1 td2)
                                     (declare (ignore td1))
                                     (setf (advisory-title td2) "double click for external text editor")
                                     (set-on-double-click td2 (lambda (obj)
                                                                (let ((*default-title-class*      *builder-title-class*)
                                                                      (*default-border-class*     *builder-border-class*))
                                                                  (input-dialog obj "Enter slots:"
                                                                                (lambda (result)
                                                                                  (when result
                                                                                    (setf (attribute control "data-custom-slots") result)
                                                                                    (setf (text td2) result)))
                                                                                :default-value (attribute control "data-custom-slots")
                                                                                :width 800
                                                                                :height 420
                                                                                :size 80
                                                                                :rows 10)))))
                           :get  ,(lambda (control) (attribute control "data-custom-slots"))
                           :set  ,(lambda (control obj) (setf (attribute control "data-custom-slots") (text obj))))
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

(defun adjust-control-placer (control)
  "If changing a property potentialy can change the size of a control function should be called. (Exported)"
  (let ((placer (get-placer control)))
    (adjust-placer control placer)))

(defun add-inspector (name func)
  "Add a custom inspector with NAME and (FUNC object title value clog-obj). (Exported)"
  (setf *inspectors* (remove-if (lambda (x)
                                  (equalp name (getf x :name)))
                                *inspectors*))
  (push (list :name name :func func) *inspectors*))

(defun add-file-extension (name func)
  "Add a custom file extension with NAME and (FUNC file dir project clog-obj). (Exported)"
  (setf *file-extensions* (remove-if (lambda (x)
                                  (equalp name (getf x :name)))
                                *file-extensions*))
  (push (list :name name :func func) *file-extensions*))

(defun reset-control-pallete (panel)
  (let* ((app (connection-data-item panel "builder-app-data"))
         (pallete (select-tool app)))
    (when pallete
      (setf (inner-html pallete) "")
      (dolist (control *supported-controls*)
        (if (equal (getf control :name) "group")
            (add-select-optgroup pallete (getf control :description))
            (add-select-option pallete (getf control :name) (getf control :description)))))))
