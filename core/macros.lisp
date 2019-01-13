(defpackage :expect/macros
  (:use :cl)
  (:import-from :bordeaux-threads)
  (:import-from :expect/suite)
  (:import-from :expect/expect #:make-expect)
  (:export #:deftest-of #:expect #:capture-error #:*print-compile-message*)
  (:local-nicknames (:suite :expect/suite)
                    (:test :expect/test)))

(in-package :expect/macros)


(defvar *cl-expect-test* nil)
(defvar *print-compile-message* t)

(defmacro deftest-of (function () &body body)
  (let ((desc (car body))
        (test-body (cdr body)))
    (unless (typep desc 'string)
      (error "Malformed deftest-of form. Expected to find test description but found ~a" (type-of desc)))
  `(progn
     (let ((test (test:make-test ',function ,desc (package-name (symbol-package ',function)))))
       (setf (test:body test )
             (lambda ()
               (let* ((*cl-expect-test* test)
                      ;; Add test to list of bordeaux threads default bindings which will cause
                      ;; it to get passed to newly created threads
                      (bt:*default-special-bindings*
                        (acons '*cl-expect-test* test bt:*default-special-bindings*)))
                 ,@test-body)))
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
