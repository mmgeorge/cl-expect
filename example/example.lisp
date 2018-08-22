(defpackage :example
  (:use :cl)
  (:import-from :expect #:deftest-of #:expect #:run-tests))

(in-package :example)


(defun add (a b)
  (+ a b))

(deftest-of add ()
  (expect (eql (add 1 1) 2))
  (expect (eql (add 1 1) 3))
  (expect (eql (add "1" 2) 3)))


(defun sub (a b)
  (+ a b))

(deftest-of sub ()
  (expect (eql (sub 1 1) 0)))


(defun div (a b)
  (/ a b))

(deftest-of div ()
  (expect (eql (div 3 0) 1))
  (expect (eql (div 3 3) 1))
  (expect (typep (div 3 0) 'error))
  (expect (eq (div 3 "0") (make-condition 'type-error)))) ;; Does a class-of comparison


(defun deferred (a)
  (blackbird:with-promise (resolve reject)
    (resolve a)))

(deftest-of deferred ()
  (expect (eql (deferred 3) 3)))


(run-tests)
