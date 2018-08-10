(defpackage :expect/example
  (:use :cl)
  (:import-from :expect/macros #:deftest-of #:expect)
  (:import-from :expect/suite)
  (:local-nicknames (:suite :expect/suite)))

(in-package :expect/example)


(defun my-nothing ()
  (+ 1 1))


(deftest-of my-nothing ()
  (expect (eql 1 1))
  (expect (eql 2 2)))


(defun do-test ()
  (suite:run (suite:suite-of *package*)))
