(defpackage :expect/example/example
  (:use :cl)
  (:import-from :expect/macros #:deftest-of #:expect)
  (:import-from :expect/suite)
  (:import-from :expect/report/report #:print-report)
  (:import-from :expect/runner #:run-tests)
  (:local-nicknames (:suite :expect/suite)))

(in-package :expect/example/example)


(defun my-nothing ()
  (+ 1 1))

(defun my-nothing-2 ()
  (+ 1 1))


(deftest-of my-nothing ()
  (expect (eql 1 1))
  (expect (eql 2 3))
  (expect (eql 3 4)))


(deftest-of my-nothing-2 ()
  (expect (eql 1 1))
  (expect (eql 2 3))
  (expect (eql 3 4)))

(deftest-of my-pass ()
  (expect (eql 3 3)))


(defun do-test ()
  (print-report (suite:run (suite:suite-of *package*)) 0))
