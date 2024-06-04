(in-package :clog-tools)

(defun on-object-scope (obj &key object title)
    (let* ((*default-title-class*      *builder-title-class*)
           (*default-border-class*     *builder-border-class*)
           (win         (create-gui-window obj :title "CLOG Object Scope"
                                           :width 640
                                           :height 480
                                           :has-pinner t
                                           :keep-on-top t
                                           :client-movement *client-side-movement*))
           (root-obj    (create-form-element (window-content win) :text))
           class
           (tree        (create-panel (window-content win)
                                      :class "w3-small"
                                      :overflow :scroll
                                      :top 30 :bottom 0 :left 0 :right 0)))
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
      (if title
          (setf (text-value root-obj) title)
          (setf (text-value root-obj) (format nil "~a" object)))
      (setf (positioning root-obj) :absolute)
      (set-geometry root-obj :height 27 :width "100%" :top 0 :left 0 :right 0)
      (labels ((escape-lisp (object)
                 (let ((value (format nil "~A" object)))
                   (setf value (ppcre:regex-replace-all "<" value "&lt;"))
                   (setf value (ppcre:regex-replace-all ">" value "&gt;"))))
               (add-class (node class object)
                 (let* ((is-root    (typep node 'clog-panel))
                        (class-tree (create-clog-tree (if is-root
                                                          node
                                                          (tree-root node))
                                                      :visible is-root
                                                      :indent-level (if is-root
                                                                        0
                                                                        (1+ (indent-level node)))
                                                      :content (format nil "<b>Class: ~A</b> : Object Value ~A"
                                                                       (escape-lisp (class-name class)) (escape-lisp object)))))
                   (create-clog-tree (tree-root class-tree)
                                     :node-html "&#x1F46A;"
                                     :content "Precedence List"
                                     :visible is-root
                                     :indent-level (1+ (indent-level class-tree))
                                     :fill-function (lambda (obj)
                                                      (on-precedences obj class object)))
                   (create-clog-tree (tree-root class-tree)
                                     :node-html "&#x1F56E;"
                                     :content "Slots"
                                     :visible is-root
                                     :indent-level (1+ (indent-level class-tree))
                                     :fill-function (lambda (obj)
                                                      (on-slots obj class object)))
                   (create-clog-tree (tree-root class-tree)
                                     :node-html "&#x1F528;"
                                     :content "Direct Generic Functions"
                                     :visible is-root
                                     :indent-level (1+ (indent-level class-tree))
                                     :fill-function (lambda (obj)
                                                      (on-generic obj class)))
                   (create-clog-tree (tree-root class-tree)
                                     :node-html "&#x1F45E;"
                                     :content "Direct Methods"
                                     :visible is-root
                                     :indent-level (1+ (indent-level class-tree))
                                     :fill-function (lambda (obj)
                                                      (on-method obj class)))
                   ))
               (on-generic (obj class)
                 (mapcar (lambda (item)
                           (create-clog-tree (tree-root obj)
                                             :indent-level (1+ (indent-level obj))
                                             :node-html "&#x1F527;"
                                             :visible nil
                                             :content (format nil "<b>~A</b> ~A"
                                                              (escape-lisp (closer-mop:generic-function-name item))
                                                              (escape-lisp (closer-mop:generic-function-lambda-list item)))))
                         (closer-mop:specializer-direct-generic-functions class)))
               (on-method (obj class)
                 (mapcar (lambda (item)
                           (create-clog-tree (tree-root obj)
                                             :indent-level (1+ (indent-level obj))
                                             :node-html "&#x1F45F;"
                                             :visible nil
                                             :content (format nil "<b>~A</b> ~A"
                                                              (escape-lisp (closer-mop:generic-function-name (closer-mop:method-generic-function item)))
                                                              (escape-lisp (closer-mop:method-lambda-list item)))))
                         (closer-mop:specializer-direct-methods class)))
               (on-precedences (obj class object)
                 (mapcar (lambda (item)
                           (unless (eq item class)
                             (add-class obj item object)))
                         (closer-mop:class-precedence-list class)))
               (on-slots (obj class object)
                 (mapcar (lambda (slot)
                           (create-clog-tree (tree-root obj)
                                             :indent-level (1+ (indent-level obj))
                                             :node-html "&#x1F588;"
                                             :visible nil
                                             :fill-function (lambda (obj)
                                                              (let* ((object (slot-value object (closer-mop:slot-definition-name slot)))
                                                                     (class (class-of object)))
                                                                (add-class obj class object)))
                                             :content (format nil "<b>~A</b> Object Value = ~A"
                                                              (escape-lisp (closer-mop:slot-definition-name slot))
                                                              (escape-lisp (slot-value object (closer-mop:slot-definition-name slot))))))
                         (closer-mop:class-slots class)))
               (on-change (object)
                 (setf (text tree) "")
                 (setf class (class-of object))
                 (add-class tree class object)))
        (set-on-change root-obj (lambda (obj)
                                  (declare (ignore obj))
                                  (on-change (eval (read-from-string (text-value root-obj))))))
        (on-change object))))