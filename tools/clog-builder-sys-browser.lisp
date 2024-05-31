(in-package :clog-tools)

(defun on-new-sys-browser (obj &key (search nil))
  (let* ((*default-title-class*      *builder-title-class*)
         (*default-border-class*     *builder-border-class*)
         (win (create-gui-window obj :title "System Browser"
                                     :top 40 :left 225
                                     :width 685 :height 530
                                     :client-movement *client-side-movement*))
         (panel (create-sys-browser (window-content win))))
    (when search
      (setf (text-value (search-box panel)) search)
      (sys-browser-populate panel))
    (set-on-click (create-span (window-icon-area win)
                               :content (format nil "~A&nbsp;" (code-char #x26F6))
                               :auto-place :top)
                  (lambda (obj)
                    (declare (ignore obj))
                    (set-geometry win
                                  :top (menu-bar-height win)
                                  :left 300
                                  :height "" :width ""
                                  :bottom 5 :right 0)
                    (set-on-window-move win nil)
                    (set-on-window-move win (lambda (obj)
                                              (setf (width obj) (width obj))
                                              (setf (height obj) (height obj))))))
    (set-on-window-size-done win (lambda (obj)
                                   (declare (ignore obj))
                                   (clog-ace:resize (src-box panel))))))

(defun sys-browser-type-box-create (panel target)
  (declare (ignore panel))
  (add-select-options target '(ALIEN-TYPE
                               CALLABLE
                               CLASS
                               COMPILER-MACRO
                               CONDITION
                               CONSTANT
                               DECLARATION
                               DEFINITION
                               FUNCTION
                               GENERIC-FUNCTION
                               GLOBAL-DEFINITION
                               IR1-CONVERT
                               MACRO
                               METHOD
                               METHOD-COMBINATION
                               OPTIMIZER
                               SETF-EXPANDER
                               SOURCE-TRANSFORM
                               SPECIAL-OPERATOR
                               STRUCTURE
                               SYMBOL-MACRO
                               TRANSFORM
                               TYPE
                               TYPE-DEFINITION
                               VARIABLE
                               VOP))
  (setf (value target) "GLOBAL-DEFINITION"))

(defun sys-browser-package-box (panel target)
  (add-select-option target "All"
                     "All")
  (dolist (p (sort (list-all-packages) (lambda (a b)
                                         (string-lessp (package-name a)
                                                       (package-name b)))))
    (add-select-option target (package-name p)
                       (package-name p)))
  (setf (value target) "All")
  (sys-browser-populate panel))

(defun sys-browser-search-box-key-up (panel target data)
  (declare (ignore target))
  (cond ((equal (text-value (package-box panel)) "All")
         (when (equalp "enter" (getf data :key))
           (sys-browser-populate panel)))
        (t
         (sys-browser-populate panel))))

(defun sys-browser-src-box-on-input (panel target)
  (declare (ignore target))
  (unless (state panel)
    (when (fname panel)
      (setf (state panel) t)
      (setf (disabledp (save-button panel)) nil))))

(defun sys-browser-file-name-on-click (panel target)
  (declare (ignore panel))
  (unless (equal (text-value target) "")
    (on-open-file target :open-file (text-value target) :regex (search-js panel))))

(defun sys-browser-eval-form-button-on-click (panel target)
  (declare (ignore target))
  (let ((p  (parse-integer 
             (js-query panel
                       (format nil "~A.session.doc.positionToIndex (~A.selection.getCursor(), 0);"
                               (clog-ace::js-ace (src-box panel))
                               (clog-ace::js-ace (src-box panel))))
             :junk-allowed t))
        (tv (text-value (src-box panel)))
        (pk (text-value (pac-box panel)))
        (lf nil)
        (cp 0))
    (loop
      (setf (values lf cp) (read-from-string tv nil nil :start cp))
      (unless lf (return nil))
      (when (> cp p) (return lf)))
    (when lf
      (let ((result (capture-eval lf
                                  :clog-obj (connection-body panel)
                                  :eval-in-package (format nil "~A" pk))))
        (clog-web-alert (connection-body panel) "Result"
                        (format nil "~&result: ~A" result)
                        :color-class "w3-green"
                        :time-out 3)))))

(defun sys-browser-eval-sel-button-on-click (panel target)
  (declare (ignore target))
  (let ((pac (text-value (pac-box panel)))
        (val (clog-ace:selected-text (src-box panel))))
    (unless (equal val "")
      (let ((result (capture-eval val :clog-obj panel
                                      :eval-in-package pac)))
        (clog-web-alert (connection-body panel) "Result"
                        (format nil "~&result: ~A" result)
                        :color-class "w3-green"
                        :time-out 3)))))

(defun sys-browser-eval-button-on-click (panel target)
  (declare  (ignore target))
  (let ((pac (text-value (pac-box panel)))
        (val (clog-ace:text-value (src-box panel))))
    (unless (equal val "")
      (let ((result (capture-eval val :clog-obj panel
                                      :eval-in-package pac)))
        (clog-web-alert (connection-body panel) "Result"
                        (format nil "~&result: ~A" result)
                        :color-class "w3-green"
                        :time-out 3)))))

(defun sys-browser-save-button-on-click (panel target)
  (when (fname panel)
    (write-file (text-value (src-box panel)) (fname panel))
    (setf (state panel) nil)
    (setf (disabledp (save-button panel)) t)))

(defun sys-browser-populate (panel)
  (ignore-errors ; ignore invalid searches
   (setf (inner-html (class-box panel)) "")
   (setf (text-value (src-box panel)) "")
   (setf (text-value (doc-box panel)) "")
   (setf (text-value (file-name panel)) "")
   (setf (fname panel) nil)
   (let* ((filter (text-value (search-box panel)))
          (has-pac    (position #\: filter :test #'equal))
          (class-only (checkedp (class-only panel)))
          (pac        (text-value (package-box panel))))
     (when has-pac
       (setf pac (string-upcase (subseq filter 0 has-pac)))
       (setf (text-value (package-box panel)) pac)
       (unless (equalp (text-value (package-box panel)) pac)
         (setf (text-value (package-box panel)) "All")
         (setf pac "All"))
       (setf filter (subseq filter (+ has-pac 1)))
       (when (and (> (length filter) 1)
                  (eql (char filter 0) #\:))
         (setf filter (subseq filter 1)))
       (setf (text-value (search-box panel)) filter))
     (setf (window-title (current-window panel))
           (format nil "System Browser - ~A" filter))
     (cond ((equalp pac "All")
            (setf class-only nil)
            (setf (classes panel) nil)
            (unless (equal filter "")
              (setf (classes panel) (definitions:apropos-definitions
                                     filter
                                     :type (find-symbol
                                            (text-value (type-box panel))
                                            (find-package :definitions))))))
           (t
            (setf (classes panel) (definitions:find-definitions
                                   (text-value (package-box panel))
                                   :package (find-package :key)
                                   :type (find-symbol
                                          (text-value (type-box panel))
                                          (find-package :definitions))))))
     (let ((i 0))
       (dolist (c (classes panel))
         (let ((name (format nil "~A" (definitions:designator c))))
           (if (or (equal filter "")
                   (search filter name :test #'char-equal))
               (if class-only
                   (if (equalp (package-name (definitions:package c))
                               pac)
                       (add-select-option (class-box panel) i
                                          (format nil "~A - ~A"
                                                  name
                                                  (definitions:type c))))
                   (add-select-option (class-box panel) i
                                      (format nil "~A:~A - ~A"
                                              (package-name (definitions:package c))
                                              name
                                              (definitions:type c)))))
           (incf i)))))))

(defun sys-browser-select (panel target)
  (ignore-errors
   (let* ((item (nth (parse-integer (text-value (class-box panel))) (classes panel))))
     (setf (fname panel) (getf (definitions:source-location item) :file))
     (setf (text-value (doc-box panel))
           (or (definitions:documentation item)
               "No documentation"))
     (cond ((fname panel)
            (let ((c (read-file (fname panel))))
              (setf (text-value (src-box panel)) c)
              (setf (text-value (pac-box panel)) (get-package-from-string c)))
            (setf (text-value (file-name panel)) (fname panel))
            (setf (disabledp (eval-button panel)) nil)
            (setf (disabledp (eval-sel-button panel)) nil)
            (setf (disabledp (eval-form-button panel)) nil)
            (setf (state panel) nil)
            (let* ((type (type-of item))
                   (name (format nil "~A" (definitions:designator item))))
              (setf name (ppcre:regex-replace-all "\\\\" name "\\x5C\\x5C"))
              (setf name (ppcre:regex-replace-all "\\\(" name "\\x5C("))
              (setf name (ppcre:regex-replace-all "\\\)" name "\\x5C)"))
              (setf name (ppcre:regex-replace-all "\\\*" name "\\x5C*"))
              (setf (search-js panel) (cond ((eq type 'definitions:generic-function)
                                             (format nil "defgeneric\\\\s+~A(\\\\s+|\\\\(|$)" name))
                                            ((eq type 'definitions:method)
                                             (format nil "defmethod\\\\s+~A(\\\\s+|\\\\(|$)" name))
                                            ((eq type 'definitions:function)
                                             (format nil "defun\\\\s+~A(\\\\s+|\\\\(|$)" name))
                                            ((eq type 'definitions:macro)
                                             (format nil "defmacro\\\\s+~A(\\\\s+|\\\\(|$)" name))
                                            ((eq type 'definitions:class)
                                             (format nil "defclass\\\\s+~A(\\\\s+|\\\\(|$)" name))
                                            ((eq type 'definitions:compiler-macro)
                                             (format nil "define-compiler-macro\\\\s+~A(\\\\s+|\\\\(|$)" name))
                                            ((eq type 'definitions:condition)
                                             (format nil "define-condition\\\\s+~A(\\\\s+|\\\\(|$)" name))
                                            ((eq type 'definitions:alien-type)
                                             (format nil "define-alien-type ~A(\\\\s+|\\\\(|$)" name))
                                            ((eq type 'definitions:constant)
                                             (format nil "defconstant\\\\s+~A(\\\\s+|\\\\(|$)" name))
                                            ((eq type 'definitions:package)
                                             (format nil "defpackage\\\\s+~A(\\\\s+|\\\\(|$)" name))
                                            ((eq type 'definitions:special-variable)
                                             (format nil "(defsection|defparameter|defvar)\\\\s+~A(\\\\s+|\\\\(|$)" name))
                                            ((eq type 'definitions:vop)
                                             (format nil "define-type-vop\\\\s+~A(\\\\s+|\\\\(|$)" name))
                                            ((eq type 'definitions:structure)
                                             (format nil "defstruct\\\\s*\\\\(\\\\s*~A(\\\\s+|\\\\(|$)" name))
                                            ((eq type 'definitions:setf-expander)
                                             (format nil "(defsetf|def)\\\\s+~A(\\\\s+|\\\\(|$)" name))
                                            ((eq type 'definitions:optimizer)
                                             (format nil "defoptimizer\\\\s*\\\\(\\\\s*~A(\\\\s+|\\\\(|$)" name))
                                            ((eq type 'definitions:ir1-convert)
                                             (format nil "def-ir1-translator\\\\s+~A(\\\\s+|\\\\(|$)" name))
                                            (t
                                             name)))
              (js-execute target (format nil "~A.find('~A',{caseSensitive:false,regExp:true})"
                                         (clog-ace::js-ace (src-box panel)) (search-js panel)))))
           (t
            (setf (search-js panel) nil)
            (setf (text-value (file-name panel)) "")
            (setf (disabledp (eval-button panel)) t)
            (setf (disabledp (eval-sel-button panel)) t)
            (setf (disabledp (eval-form-button panel)) t)
            (setf (disabledp (save-button panel)) t)
            (setf (state panel) t)
            (setf (text-value (src-box panel)) "No file information"))))))
