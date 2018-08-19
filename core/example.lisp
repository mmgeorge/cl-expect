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
  (expect (eql 2 3))
  (expect (eql 3 4)))


(defun my-other-nothing (a)
  (+ a 2))

(deftest-of my-other-nothing ()
  ;(expect (eql (my-other-nothing 2) 3))
  (expect (eql (my-other-nothing "2") 3)))


;(defun my-unexpected-nothing (a)
 ; (+ ))


(defun do-test ()
  (suite:run (suite:suite-of *package*)))
