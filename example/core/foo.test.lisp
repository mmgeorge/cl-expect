(defpackage :example/foo.test 
  (:use :cl :expect :example/foo))

(in-package :example/foo.test)


(deftest-of add ()
  "Add two numbers"
  (expect (eq (add 1 1) 2)))


(deftest-of add ()
  "Add two numbers - bad test, should fail!"
  (expect (eq (add 0 1) 2)))


(deftest-of silly-incr ()
  "Call with bad args"
  (expect (typep (silly-incr 4 5) 'simple-error)))


(deftest-of silly-incr ()
  "Call with correct args"
  (expect (eq (silly-incr 4 1) 5)))


(deftest-of silly-incr ()
  "Call with bad args - bad test, should fail!"
  (expect (eq (silly-incr 4 5) 9)))