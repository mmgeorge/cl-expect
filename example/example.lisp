(defpackage :example
  (:use :cl)
  (:import-from :expect #:deftest-of #:expect #:run-tests))

(in-package :expect)


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


(run-tests)
