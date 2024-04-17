(in-package :clog-tools)

; Primary count zero is the default so no need to include those.
(defparameter +common-lisp-templates+
  '((assert :count 2 :sub (nil nil
                           (:style :list)
                           nil))
    (block :count 1)
    (case :count 1 :sub (nil nil (:count 0)))
    (catch :count 1)
    (ccase :count 1 :sub (nil nil (:count 0)))
    (cond :count 0 :sub (nil (:count 0)))
    (ctypecase :count 1 :sub (nil nil (:count 0)))
    (defclass :count 2 :sub (nil nil
                             (:style :list)
                             (:style :list :sub ((:count 0)))
                             (:count 0)))
    (defconstant :count 1)
    (defgeneric :count 2 :sub (nil nil
                               (:style :list)
                               nil))
    (define-compiler-macro :count 2 :sub (nil nil
                                          (:style :list)
                                          nil))
    (define-condition :count 2 :sub (nil nil
                                     (:style :list)
                                     (:style :list :sub ((:count 0)))
                                     (:count 0)))
    (define-method-combination :count 1)
    (define-modify-macro :count 2 :sub (nil nil
                                        (:style :list)
                                        nil))
    (define-setf-expander :count 2 :sub (nil nil
                                         (:style :list)
                                         nil))
    (define-symbol-macro :count 1)
    (defmacro :count 2 :sub (nil nil
                             (:style :list)
                             nil))
    (defmethod :count 2 :ignore (:before :after :around) :sub ((:style :list)
                                                               (:style :list)
                                                               nil))
    (defpackage :count 1)
    (defparameter :count 1)
    (defsetf :count 2) ; primary is actually 2-3
    (defstruct :count 1 :sub (nil
                              (:count 0)
                              nil))
    (deftype :count 2 :sub (nil nil
                            (:style :list)
                            nil))
    (defun :count 2 :sub (nil nil
                          (:style :list)
                          nil))
    (defvar :count 1)
    (destructuring-bind :count 2 :sub (nil nil
                                       (:style :list)
                                       nil))
    (do-all-symbols :style :tag :count 1 :sub (nil
                                               (:count 0)
                                               nil))
    (do :style :tag :count 2 :sub (nil
                                   (:style :list :sub ((:count 0)))
                                   (:count 0)
                                   nil))
    (do* :style :tag :count 2 :sub (nil
                                    (:style :list :sub ((:count 0)))
                                    (:count 0)
                                    nil))
    (do-external-symbols :style :tag :count 1 :sub (nil
                                                    (:count 0)
                                                    nil))
    (dolist :style :tag :count 1 :sub (nil
                                       (:count 0)
                                       nil))
    (do-symbols :style :tag :count 1 :sub (nil
                                           (:count 0)
                                           nil))
    (dotimes :style :tag :count 1 :sub (nil
                                        (:count 0)
                                        nil))
    (ecase :count 1 :sub (nil nil
                          (:count 0)))
    (etypecase :count 1 :sub (nil nil
                              (:count 0)))
    (eval-when :count 1)
    (flet :count 1 :sub (nil
                         (:style :list :sub ((:count 1)))
                         nil))
    (handler-bind :count 1 :sub (nil
                                 (:style :list :sub ((:count 0)))
                                 nil))
    (handler-case :count 1 :sub (nil nil
                                 (:count 1 :style :tag)))
    (if :count 3 :sub (nil
                       (:style :list :sub ((:count 0)))
                       nil))
    (:import-from :count 1)
    (labels :count 1 :sub (nil
                           (:style :list :sub ((:count 1)))
                           nil))
    (lambda :count 1 :sub (nil
                           (:style :list)
                           nil))
    (let :count 1 :sub (nil
                        (:style :list :sub ((:count 0)))
                        nil))
    (let* :count 1 :sub (nil
                         (:style :list :sub ((:count 0)))
                         nil))
    (loop :count 0)
    (macrolet :count 1 :sub (nil
                             (:style :list :sub ((:count 1)))
                             nil))
    (multiple-value-bind :count 2 :sub (nil
                                        (:style :list)
                                        nil nil))
    (prog :style :tag :count 1 :sub (nil
                                     (:style :list :sub ((:count 0)))
                                     nil))
    (prog* :style :tag :count 1 :sub (nil
                                      (:style :list :sub ((:count 0)))
                                      nil))
    (progv :count 2)
    (prog1 :style :tag :count 2 :sub (nil
                                      (:style :list :sub ((:count 0)))
                                      (:count 0)
                                      nil))
    (quote :count 0 :sub (nil
                          (:style :quote)))
    (:shadowing-import-from :count 1)
    (tagbody :style :tag :count 0)
    (typecase :count 1 :sub (nil nil
                             (:count 0)))
    (unless :count 1)
    (unwind-protect :count 1)
    (when :count 1)
    (with-accessors :count 2 :sub (nil
                                   (:style :list)
                                   nil))
    (with-compilation-unit :count 1 :sub (nil
                                          (:style :list)
                                          nil))
    (with-condition-restarts :count 2)
    (with-hash-table-iterator :count 1 :sub (nil
                                             (:count 0)
                                             nil))
    (with-input-from-string :count 1 :sub (nil
                                           (:count 0)
                                           nil))
    (with-open-file :count 1 :sub (nil
                                   (:count 0)
                                   nil))
    (with-open-stream :count 1 :sub (nil
                                     (:count 0)
                                     nil))
    (with-output-to-string :count 1 :sub (nil
                                          (:count 0)
                                          nil))
    (with-package-iterator :count 1 :sub (nil
                                          (:count 0)
                                          nil))
    (with-simple-restart :count 1 :sub (nil
                                        (:count 0)
                                        nil))
    (with-slots :count 2 :sub (nil
                               (:style :list)
                               nil))))

(defparameter +asdf-templates+
  '(("defsystem" :count 1)))

(defparameter +uiop-templates+
  '(("while-collecting" :count 1 :sub (nil
                                       (:style :list)
                                       nil))
    ("with-current-directory" :count 1 :sub (nil
                                             (:style :list)
                                             nil))
    ("with-deprecation" :count 1 :sub (nil
                                       (:style :list)
                                       nil))
    ("with-enough-pathname" :count 1 :sub (nil
                                           (:count 1)
                                           nil))
    ("with-fatal-condition-handler" :count 1 :sub (nil
                                                   (:style :list)
                                                   nil))
    ("with-input" :count 1 :sub (nil
                                 (:count 1)
                                 nil))
    ("with-muffled-compiler-conditions" :count 1 :sub (nil
                                                       (:style :list)
                                                       nil))
    ("with-muffled-conditions" :count 1 :sub (nil
                                              (:style :list)
                                              nil))
    ("with-muffled-loader-conditions" :count 1 :sub (nil
                                                     (:style :list)
                                                     nil))
    ("with-null-input" :count 1 :sub (nil
                                      (:count 1)
                                      nil))
    ("with-null-output" :count 1 :sub (nil
                                       (:count 1)
                                       nil))
    ("with-output" :count 1 :sub (nil
                                  (:count 1)
                                  nil))
    ("with-safe-io-syntax" :count 1 :sub (nil
                                          (:style :list)
                                          nil))
    ("with-saved-deferred-warnings" :count 1 :sub (nil
                                                   (:count 1)
                                                   nil))
    ("with-staging-pathname" :count 1 :sub (nil
                                            (:count 1)
                                            nil))
    ("with-temporary-file" :count 1 :sub (nil
                                          (:count 1)
                                          nil))
    ("with-upgradability" :count 1 :sub (nil
                                         (:style :list)
                                         nil))))

(defparameter +alexandria-templates+
  '(("cswitch" :count 1 :sub (nil nil
                              (:count 0)))
    ("destructuring-case" :count 1 :sub (nil nil
                                         (:count 0)))
    ("destructuring-ccase" :count 1 :sub (nil nil
                                          (:count 0)))
    ("destructuring-ecase" :count 1 :sub (nil nil
                                          (:count 0)))
    ("eswitch" :count 1 :sub (nil nil
                              (:count 0)))
    ("if-let" :count 1 :sub (nil
                             (:style :list :sub ((:count 0)))
                             nil))
    ("named-lambda" :count 2 :sub (nil nil
                                   (:style :list) nil))
    ("once-only" :count 1 :sub (nil
                                (:style :list) nil))
    ("switch" :count 1 :sub (nil nil
                             (:count 0)))
    ("when-let" :count 1 :sub (nil
                               (:style :list :sub ((:count 0)))
                               nil))
    ("when-let*" :count 1 :sub (nil
                                (:style :list :sub ((:count 0)))
                                nil))
    ("with-gensyms" :count 1 :sub (nil
                                   (:style :list)
                                   nil))
    ("with-unique-names" :count 1 :sub (nil
                                        (:style :list)))))
