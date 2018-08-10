(defpackage :expect/expect
  (:use :cl)
  (:export #:expect #:make-expect #:predicate #:form #:expected))

(in-package :expect/expect)

(defclass expect ()
  ((predicate :reader predicate :initarg :predicate)
   (form :reader form :initarg :form)
   (expected :reader expected :initarg :expected)))


(defun make-expect (predicate form expected)
  (make-instance 'expect :predicate predicate :form form :expected expected))


(defmethod make-load-form ((self expect) &optional env)
  (declare (ignore env))
  `(make-instance ',(class-of self)
                  :predicate ',(predicate self)
                  :form ',(form self)
                  :expected ',(expected self)))
