(defpackage :expect/macros
  (:use :cl)
  (:import-from :expect/suite)
  (:import-from :expect/expect #:make-expect)
  (:export #:deftest-of #:expect)
  (:local-nicknames (:suite :expect/suite)
                    (:test :expect/test)))

(in-package :expect/macros)


(defvar *cl-expect-test* nil)


(defmacro deftest-of (function () &body body)
  (let ((desc (car body)))
    (unless (typep desc 'string)
      (error "Malformed deftest-of form. Expected to find test description but found ~a" (type-of desc)))
  `(progn
     (let ((test (test:make-test 
                  ',function
                  ,desc
                  (package-name (symbol-package ',function)))))
       (setf (test:body test )
             (lambda ()
               (let ((*cl-expect-test* test))
                 ,@(mapcar #'macroexpand (cdr body)))))
       (suite:register (suite:suite-of *package*) test))
       *cl-expect-test*)))


(defmacro expect ((predicate form expected))
  (let ((uneval `(,predicate ,form ,expected)))
    `(progn 
       (unless *cl-expect-test*
         (error "Expect must be called within a test!"))
       (test:add *cl-expect-test* (make-expect ',uneval ',predicate ,form ,expected)))))
