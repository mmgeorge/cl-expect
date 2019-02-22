(defpackage :expect/macros
  (:use :cl)
  (:import-from :bordeaux-threads)
  (:import-from :expect/suite)
  (:import-from :expect/expect #:make-expect)
  (:import-from :expect/fixture)
  (:export #:deftest-of #:expect #:defixture #:with-cleanup #:capture-error #:*print-compile-message*)
  (:local-nicknames (:suite :expect/suite)
                    (:test :expect/test)
                    (:fixture :expect/fixture)))

(in-package :expect/macros)


(defvar *cl-expect-test* nil)
(defvar *in-fixture* nil)
(defvar *print-compile-message* t)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun expand-fixture-list (fixture-list body)
    (if fixture-list
      (destructuring-bind (var fixture) (car fixture-list)
        (let* ((fixture (eval fixture))
               (before (fixture:before fixture))
               (after (fixture:after fixture))
               (next (expand-fixture-list (cdr fixture-list) body))
               ;; Prevent accidental capture, esp. (deftest-of x ((var var) (var2 var2)) ...)
               (sym (gensym (symbol-name var)))
               (wrapped-next `(let ((,var ,sym)) ,next))
               (inner-form (if after
                               `(bb:finally ,wrapped-next (funcall #',after ,sym))
                               wrapped-next)))
          `(bb:attach ,before (lambda (,sym) ,inner-form))))
      `(progn ,@body))))


(defmacro deftest-of (function fixtures &body body)
  (let ((desc (car body))
        (test-body (cdr body)))
    (unless (typep desc 'string)
      (error "Malformed deftest-of form. Expected to find test description but found ~a" (type-of desc)))
  `(progn
     (let ((test (test:make-test ',function ,desc (package-name (symbol-package ',function)))))
       (setf (test:body test )
             (lambda ()
               ,(expand-fixture-list
                 fixtures
                 `((let* ((*cl-expect-test* test)
                          (bb:*promise-keep-specials*
                            (cons '*cl-expect-test* bb:*promise-keep-specials*)))
                   ,@test-body)))))
       (when *print-compile-message*
         (format t "Adding test definition for ~a~%" (test:name test)))
       (suite:register (suite:suite-of *package*) test))
       *cl-expect-test*)))


(defmacro expect ((predicate form expected))
  (let ((uneval `(,predicate ,form ,expected)))
    `(progn 
       (unless *cl-expect-test*
         (error "Expect must be called within a test!"))
       (test:add *cl-expect-test* (make-expect ,uneval ',predicate ,form ,expected)))))


(defmacro capture-error (error-type &body body)
  `(handler-case ,@body
     (,error-type (e) e)))


(defmacro defixture (name () &body body)
  `(let ((*in-fixture* t))
     ,(if (eq (caar body) 'with-cleanup)
          `(defparameter ,name ,@(macroexpand body))
          `(defparameter ,name  
             (make-instance 'fixture:fixture :before '(progn ,@body) :after nil)))))


(defmacro with-cleanup (before cleanup)
  `(progn
     (unless *in-fixture*
       (error "with-cleanup must be called within a defixture form!"))
     (make-instance 'fixture:fixture :before `(progn ,',before) :after ',cleanup)))
