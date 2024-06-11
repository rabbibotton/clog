(in-package :clog-tools)

(defun clog-builder-scope (object &key title clog-body)
  "Load a new CLOG Object Scope with OBJECT, use TITLE if set for the object
name. If CLOG-BODY not set use *clog-debug-instance*"
  (unless clog-body
    (setf clog-body *clog-debug-instance*))
  (on-object-scope clog-body :object object :title title))

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
         (pac-line    (create-form-element (window-content win) :text :value "clog-user"))
         class
         (tree        (create-panel (window-content win)
                                    :class "w3-small"
                                    :overflow :scroll
                                    :top 60 :bottom 0 :left 0 :right 0)))
    (set-on-window-close win (lambda (obj)
                               (browser-gc obj)))
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
    (setf (place-holder root-obj) "Enter a form to evaluate")
    (setf (spellcheckp root-obj) nil)
    (create-div tree :class "w3-tiny w3-center"
                :content "or use CLOG-TOOLS:CLOG-BUILDER-SCOPE to create new scope")
    (when object
      (if title
          (setf (text-value root-obj) title)
          (setf (text-value root-obj) (format nil "~a" object))))
    (setf (positioning root-obj) :absolute)
    (setf (positioning pac-line) :absolute)
    (set-geometry root-obj :height 27 :width "100%" :top 0 :left 0 :right 0)
    (set-geometry pac-line :height 27 :width "100%" :top 27 :left 0 :right 0)
    (labels ((escape-lisp (object)
               (let ((value (format nil "~A" object)))
                 (setf value (ppcre:regex-replace-all "<" value "&lt;"))
                 (setf value (ppcre:regex-replace-all ">" value "&gt;"))))
             (get-package (sym)
               (handler-case
                   (escape-lisp (package-name (symbol-package sym)))
                 (error ()
                   (escape-lisp sym))))
             (add-list (node lst)
               (mapcar (lambda (object)
                         (add-class node (class-of object) object))
                       lst))
             (add-hash (node hsh)
               (maphash (lambda (k v)
                          (create-clog-tree-item (tree-root node) :content "Key/Value:" :node-html "")
                          (add-class node (class-of k) k)
                          (add-class node (class-of v) v))
                        hsh))
             (add-class (node class object)
               (let* ((is-root    (typep node 'clog-panel))
                      (class-tree (create-clog-tree (if is-root
                                                        node
                                                        (tree-root node))
                                                    :node-html "<span style='color:orange'>&#9673;</a>"
                                                    :visible is-root
                                                    :indent-level (if is-root
                                                                      0
                                                                      (1+ (indent-level node)))
                                                    :on-context-menu (lambda (obj)
                                                                       (on-new-sys-browser obj
                                                                                           :search (get-name (class-name class))))
                                                    :content (format nil "<b>Class: ~A</b>~A"
                                                                     (get-name (class-name class))
                                                                     (if (typep object class)
                                                                         (format nil " : Object Value ~A" (escape-lisp object))
                                                                         "")))))
                 (when (consp object)
                   (create-clog-tree-item (tree-root class-tree) :content "<u>List</u>" :node-html "")
                   (add-list class-tree object))
                 (when (hash-table-p object)
                   (create-clog-tree-item (tree-root class-tree) :content "<u>Hash Table</u>" :node-html "")
                   (add-hash class-tree object))
                 (create-clog-tree (tree-root class-tree)
                                   :node-html "<span style='color:red'>&#9282;</a>"
                                   :content "Precedence List"
                                   :visible nil
                                   :indent-level (1+ (indent-level class-tree))
                                   :fill-function (lambda (obj)
                                                    (on-precedences obj class object)))
                 (create-clog-tree (tree-root class-tree)
                                   :node-html "<span style='color:red'>&#9282;</a>"
                                   :content "Superclass List"
                                   :visible nil
                                   :indent-level (1+ (indent-level class-tree))
                                   :fill-function (lambda (obj)
                                                    (on-super obj class object)))
                 (create-clog-tree (tree-root class-tree)
                                   :node-html "<span style='color:red'>&#9282;</a>"
                                   :content "Subclass List"
                                   :visible nil
                                   :indent-level (1+ (indent-level class-tree))
                                   :fill-function (lambda (obj)
                                                    (on-sub obj class object)))
                 (create-clog-tree (tree-root class-tree)
                                   :node-html "<span style='color:red'>&#9776;</a>"
                                   :content "Direct Slots"
                                   :visible nil
                                   :indent-level (1+ (indent-level class-tree))
                                   :fill-function (lambda (obj)
                                                    (on-slots obj class object)))
                 (create-clog-tree (tree-root class-tree)
                                   :node-html "<span style='color:red'>&#9689;</a>"
                                   :content "Direct Generic Functions"
                                   :visible nil
                                   :indent-level (1+ (indent-level class-tree))
                                   :fill-function (lambda (obj)
                                                    (on-generic obj class)))
                 (create-clog-tree (tree-root class-tree)
                                   :node-html "<span style='color:red'>&#9649;</a>"
                                   :content "Direct Methods"
                                   :visible nil
                                   :indent-level (1+ (indent-level class-tree))
                                   :fill-function (lambda (obj)
                                                    (on-method obj class)))
                 (when is-root
                   (create-clog-tree (tree-root class-tree)
                                     :node-html "<b><span style='color:red'>&#9776;</span></b>"
                                     :content "<b>Class Slots</b>"
                                     :visible nil
                                     :indent-level (1+ (indent-level class-tree))
                                     :fill-function (lambda (obj)
                                                      (on-calc-slots obj class object)))
                   (create-clog-tree (tree-root class-tree)
                                     :node-html "<b><span style='color:red'>&#9649;</span></b>"
                                     :content "<b>Class Methods</b>"
                                     :visible nil
                                     :indent-level (1+ (indent-level class-tree))
                                     :fill-function (lambda (obj)
                                                      (on-calc-methods obj class))))))
             (get-name (sym)
               (escape-lisp (if (typep sym 'cons)
                                (format nil "(~A ~A:~A)"
                                        (first sym)
                                        (get-package (second sym))
                                        (second sym))
                                (format nil "~A:~A"
                                        (get-package sym)
                                        sym))))
             (get-name-cons (sym)
               (if (typep sym 'cons)
                   (list (format nil "(~A ~A)"
                                 (first sym)
                                 (second sym))
                         (format nil "~A" (get-package (second sym))))
                   (list (format nil "~A" sym)
                         (format nil "~A" (get-package sym)))))
             (on-generic (obj class)
               (mapcar (lambda (item)
                         (create-clog-tree (tree-root obj)
                                           :indent-level (1+ (indent-level obj))
                                           :node-html "<span style='color:black'>&#9688;</a>"
                                           :visible nil
                                           :on-context-menu (lambda (obj)
                                                              (let ((sys (get-name-cons
                                                                           (closer-mop:generic-function-name item))))
                                                                (on-new-sys-browser obj
                                                                                    :search (first sys)
                                                                                    :package (second sys))))
                                           :content (format nil "<b>~A</b> ~A"
                                                            (get-name (closer-mop:generic-function-name item))
                                                            (escape-lisp (closer-mop:generic-function-lambda-list item)))))
                       (closer-mop:specializer-direct-generic-functions class)))
             (on-calc-methods (obj class)
               (dolist (dclass (closer-mop:class-precedence-list class))
                 (when (or (equalp (get-name (class-name dclass)) "common-lisp:standard-object")
                           (equalp (get-name (class-name dclass)) "common-lisp:structure-object"))
                   (return))
                 (create-div (tree-root obj) :content (format nil "&nbsp;&nbsp;<u>~A</u>" (get-name (class-name dclass)))
                             :class "w3-tiny")
                 (mapcar (lambda (item)
                           (create-clog-tree (tree-root obj)
                                             :indent-level (1+ (indent-level obj))
                                             :node-html "<span style='color:black'>&#9648;</a>"
                                             :visible nil
                                             :on-context-menu (lambda (obj)
                                                                (let ((sys (get-name-cons
                                                                             (closer-mop:generic-function-name
                                                                               (closer-mop:method-generic-function item)))))
                                                                  (on-new-sys-browser obj
                                                                                      :search (first sys)
                                                                                      :package (second sys))))
                                             :content (format nil "<b>~A</b> ~A"
                                                              (get-name (closer-mop:generic-function-name (closer-mop:method-generic-function item)))
                                                              (escape-lisp (closer-mop:method-lambda-list item)))))
                         (closer-mop:specializer-direct-methods dclass))))
             (on-method (obj class)
               (mapcar (lambda (item)
                         (create-clog-tree (tree-root obj)
                                           :indent-level (1+ (indent-level obj))
                                           :node-html "<span style='color:black'>&#9648;</a>"
                                           :visible nil
                                           :on-context-menu (lambda (obj)
                                                              (let ((sys (get-name-cons
                                                                           (closer-mop:generic-function-name
                                                                             (closer-mop:method-generic-function item)))))
                                                                (on-new-sys-browser obj
                                                                                    :search (first sys)
                                                                                    :package (second sys))))
                                           :content (format nil "<b>~A</b> ~A"
                                                            (get-name (closer-mop:generic-function-name (closer-mop:method-generic-function item)))
                                                            (escape-lisp (closer-mop:method-lambda-list item)))))
                       (closer-mop:specializer-direct-methods class)))
             (on-precedences (obj class object)
               (mapcar (lambda (item)
                         (unless (eq item class)
                           (add-class obj item object)))
                       (closer-mop:class-precedence-list class)))
             (on-super (obj class object)
               (mapcar (lambda (item)
                         (unless (eq item class)
                           (add-class obj item object)))
                       (closer-mop:class-direct-superclasses class)))
             (on-sub (obj class object)
               (mapcar (lambda (item)
                         (unless (eq item class)
                           (add-class obj item object)))
                       (closer-mop:class-direct-subclasses class)))
             (on-slots (obj class object)
               (mapcar (lambda (slot)
                         (let ((sltt (create-clog-tree (tree-root obj)
                                                       :indent-level (1+ (indent-level obj))
                                                       :node-html "<span style='color:black'>&#9644;</a>"
                                                       :visible nil
                                                       :on-context-menu (lambda (obj)
                                                                          (on-new-sys-browser obj
                                                                                              :search (get-name (closer-mop:slot-definition-name slot))))
                                                       :content (format nil "<b>~A</b>~A"
                                                                        (get-name (closer-mop:slot-definition-name slot))
                                                                        (if (typep object class)
                                                                            (format nil " Object Value = ~A"
                                                                                    (escape-lisp (slot-value object (closer-mop:slot-definition-name slot))))
                                                                            "")))))
                           (when (typep object class)
                             (let* ((object (slot-value object (closer-mop:slot-definition-name slot)))
                                    (class (class-of object)))
                               (add-class sltt class object)))
                           (create-clog-tree-item (tree-root sltt)
                                                  :content (format nil "slot-definition-initargs = ~A"
                                                                   (escape-lisp (closer-mop:slot-definition-initargs slot))))
                           (create-clog-tree-item (tree-root sltt)
                                                  :content (format nil "slot-definition-initform = ~A"
                                                                   (escape-lisp   (closer-mop:slot-definition-initform slot))))
                           (create-clog-tree-item (tree-root sltt)
                                                  :content (format nil "slot-definition-initfunction = ~A"
                                                                   (escape-lisp (closer-mop:slot-definition-initfunction slot))))
                           (create-clog-tree-item (tree-root sltt)
                                                  :content (format nil "slot-definition-readers = ~A"
                                                                   (escape-lisp (closer-mop:slot-definition-readers slot))))
                           (create-clog-tree-item (tree-root sltt)
                                                  :content (format nil "slot-definition-writers = ~A"
                                                                   (escape-lisp (closer-mop:slot-definition-writers slot))))
                           (create-clog-tree-item (tree-root sltt)
                                                  :content (format nil "slot-type = ~A"
                                                                   (escape-lisp (closer-mop:slot-definition-type slot))))
                           (create-clog-tree-item (tree-root sltt)
                                                  :content (format nil "slot-definition-allocation = ~A"
                                                                   (escape-lisp (closer-mop:slot-definition-allocation slot))))))
                       (closer-mop:class-direct-slots class)))
             (on-calc-slots (obj class object)
               (dolist (dclass (closer-mop:class-precedence-list class))
                 (when (or (equalp (get-name (class-name dclass)) "common-lisp:standard-object")
                           (equalp (get-name (class-name dclass)) "common-lisp:structure-object"))
                   (return))
                 (create-div (tree-root obj) :content (format nil "&nbsp;&nbsp;<u>~A</u>" (get-name (class-name dclass)))
                             :class "w3-tiny")
                 (mapcar (lambda (slot)
                           (let ((sltt (create-clog-tree (tree-root obj)
                                                         :indent-level (1+ (indent-level obj))
                                                         :node-html "<span style='color:black'>&#9644;</a>"
                                                         :visible nil
                                                         :on-context-menu (lambda (obj)
                                                                            (on-new-sys-browser obj
                                                                                                :search (get-name (closer-mop:slot-definition-name slot))))
                                                         :content (format nil "<b>~A</b>~A"
                                                                          (get-name (closer-mop:slot-definition-name slot))
                                                                          (if (typep object class)
                                                                              (format nil " Object Value = ~A"
                                                                                      (escape-lisp (slot-value object (closer-mop:slot-definition-name slot))))
                                                                              "")))))
                             (when (typep object class)
                               (let* ((object (slot-value object (closer-mop:slot-definition-name slot)))
                                      (class (class-of object)))
                                 (add-class sltt class object)))
                             (create-clog-tree-item (tree-root sltt)
                                                    :content (format nil "slot-definition-initargs = ~A"
                                                                     (escape-lisp (closer-mop:slot-definition-initargs slot))))
                             (create-clog-tree-item (tree-root sltt)
                                                    :content (format nil "slot-definition-initform = ~A"
                                                                     (escape-lisp (closer-mop:slot-definition-initform slot))))
                             (create-clog-tree-item (tree-root sltt)
                                                    :content (format nil "slot-definition-initfunction = ~A"
                                                                     (escape-lisp (closer-mop:slot-definition-initfunction slot))))
                             (create-clog-tree-item (tree-root sltt)
                                                    :content (format nil "slot-definition-readers = ~A"
                                                                     (escape-lisp (closer-mop:slot-definition-readers slot))))
                             (create-clog-tree-item (tree-root sltt)
                                                    :content (format nil "slot-definition-writers = ~A"
                                                                     (escape-lisp (closer-mop:slot-definition-writers slot))))
                             (create-clog-tree-item (tree-root sltt)
                                                    :content (format nil "slot-type = ~A"
                                                                     (escape-lisp (closer-mop:slot-definition-type slot))))
                             (create-clog-tree-item (tree-root sltt)
                                                    :content (format nil "slot-definition-allocation = ~A"
                                                                     (escape-lisp (closer-mop:slot-definition-allocation slot))))))
                         (closer-mop:class-direct-slots dclass))))
             (on-change (object &key is-list)
               (setf (text tree) "")
               (browser-gc obj)
               (create-div tree :class "w3-tiny w3-center"
                           :content "left-click - drill down / right-click - system browse<br><br>")
               (if is-list
                   (add-list tree object)
                   (progn
                     (setf class (class-of object))
                     (add-class tree class object)))))
      (set-on-change root-obj (lambda (obj)
                                (declare (ignore obj))
                                (when (not (equal (text-value root-obj) ""))
                                  (let* ((*default-title-class*      *builder-title-class*)
                                         (*default-border-class*     *builder-border-class*)
                                         (*package* (find-package (string-upcase (text-value pac-line)))))
                                    (on-change (multiple-value-list (eval (read-from-string (text-value root-obj))))
                                               :is-list t)))))
      (when object
        (on-change object)))))