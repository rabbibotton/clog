(in-package :clog-tools)

;; Template Utilities

(defun walk-files-and-directories (path process)
  "Walk PATH and apply PROCESS on each (path and file)"
    (let* ((flist (uiop:directory-files path))
           (dlist (uiop:subdirectories path)))
      (dolist (f flist)
        (funcall process path (file-namestring f)))
      (dolist (d dlist)
        (walk-files-and-directories d process))))

(defun template-copy (sys-name start-dir filename &key panel)
  "Copy START-DIR to FILENAME processing .lt files as cl-template files,
if PANEL each copy produces a <b>source</b> to destination added as
create-div's"
  (walk-files-and-directories
   start-dir
   (lambda (path file)
     (let* ((tmpl-ext "lt")
            (src-file (format nil "~A~A"
                              path file))
            (out-dir  (format nil "~A/~A/~A"
                              filename
                              sys-name
                              (subseq (format nil "~A" path)
                                      (length start-dir))))
            (out-file (format nil "~A~A"
                              out-dir
                              file)))
       (ensure-directories-exist out-dir)
       (cond ((equalp (pathname-type file) tmpl-ext)
              (let* ((nfile (pathname-name file))
                     (afile (cond ((equalp (pathname-name nfile) "tmpl")
                                    (format nil "~A~A.~A" out-dir sys-name (pathname-type nfile)))
                                   ((equalp (pathname-name nfile) "tmpl-tools")
                                    (format nil "~A~A-tools.~A" out-dir sys-name (pathname-type nfile)))
                                   (t
                                    (format nil "~A~A" out-dir nfile)))))
                (write-file (funcall (cl-template:compile-template (read-file src-file :clog-obj panel))
                                     (list :sys-name sys-name))
                            afile :clog-obj panel)
                (when panel
                  (create-div panel
                              :content (format nil "<b>~A</b> -> ~A"
                                               src-file afile)))))
             (t
              (uiop:copy-file src-file out-file)
              (when panel
                (create-div panel
                            :content (format nil "<b>~A</b> -> ~A"
                                             src-file out-file)))))))))

;; Handle panel-clog-templates events

(defun fill-button-clicked (panel)
  "Template fill botton clicked"
  (let* ((app (connection-data-item panel "builder-app-data"))
         (*default-title-class*      *builder-title-class*)
         (*default-border-class*     *builder-border-class*)
         (tmpl-rec  (find-if (lambda (x)
                               (equal (getf x :code)
                                      (value (template-box panel))))
                             *supported-templates*))
         (start-dir (format nil "~A~A"
                            (asdf:system-source-directory :clog)
                            (getf tmpl-rec :loc)))
         (www-dir   (format nil "~A~A"
                            (asdf:system-source-directory :clog)
                            (getf tmpl-rec :www))))
    (setf (hiddenp (win panel)) t)
    (input-dialog
     (win panel) "Enter new system name:"
     (lambda (sys-name)
       (cond (sys-name
              (let* ((*default-title-class*      *builder-title-class*)
                     (*default-border-class*     *builder-border-class*)
                     (pwin (create-gui-window panel :title "Local Project Directory"
                                              :width 500 :height 250))
                     (prjs (create-project-dir (window-content pwin))))
                (window-center pwin)
                (setf (on-done prjs)
                      (lambda (obj)
                        (declare (ignore obj))
                        (let ((filename (value (project-list prjs))))
                          (window-close pwin)
                          (cond (filename
                                 (cond ((uiop:directory-exists-p (format nil "~A~A" filename sys-name))
                                        (clog-gui:alert-toast (win panel) "Cancel" "Canceled - Project directory exists")
                                        (window-close (win panel)))
                                       (t
                                        (template-copy sys-name start-dir filename :panel (window-content (win panel)))
                                        (when (getf tmpl-rec :www)
                                          (template-copy sys-name www-dir filename :panel (window-content (win panel))))
                                        (asdf:clear-source-registry)
                                        (when (project-win app)
                                          (window-close (project-win app)))
                                         (on-show-project panel :project sys-name)
                                        (when (project-tree-win app)
                                          (window-close (project-tree-win app))
                                          (on-project-tree panel))
                                        (create-div (window-content (win panel)) :content "<hr><b>done.</b>"))))
                                (t
                                 (window-close (win panel)))))))))
             (t
              (window-close (win panel))))))))
