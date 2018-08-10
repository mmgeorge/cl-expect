(defpackage :expect/macros
  (:use :cl)
  (:import-from :expect/suite)
  (:import-from :expect/expect #:make-expect)
  (:local-nicknames (:suite :expect/suite)
                    (:test :expect/test))
  (:export #:deftest-of #:expect))

(in-package :expect/macros)


(defmacro deftest-of (function () &body body)
  `(progn
     (let ((cl-expect-test (test:make-test (symbol-name ',function))))
       (suite:register (suite:suite-of *package*) cl-expect-test)
       ,@body
       cl-expect-test)))


(defmacro expect ((predicate form expected))
  (let ((expect (make-expect predicate form expected)))
    `(progn 
       (unless cl-expect-test
         (error "Expect must be called within a test!"))
       (test:add cl-expect-test ,expect))))
